;; AtomicAuthenticity - Molecular-Level Supply Chain Verification Platform

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PRODUCT-NOT-FOUND (err u102))
(define-constant ERR-PRODUCT-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-VERIFICATION-NOT-READY (err u105))
(define-constant ERR-ALREADY-VALIDATED (err u106))
(define-constant ERR-PRODUCT-COMPLETED (err u107))
(define-constant ERR-INVALID-STATUS (err u108))

;; Data Variables
(define-data-var next-product-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% in basis points (250/10000)
(define-data-var total-platform-revenue uint u0)

;; Data Maps
(define-map products
  { product-id: uint }
  {
    manufacturer: principal,
    title: (string-utf8 200),
    description: (string-utf8 500),
    verification-cost: uint,
    current-funding: uint,
    supply-chain: (string-ascii 50),
    status: (string-ascii 20), ;; "verification", "active", "authenticated", "failed"
    created-at: uint,
    checkpoints-completed: uint,
    total-checkpoints: uint
  }
)

(define-map stakeholder-contributions
  { verifier: principal, product-id: uint }
  {
    amount: uint,
    contributed-at: uint
  }
)

(define-map stakeholder-reputation
  { stakeholder: principal }
  {
    score: uint,
    successful-verifications: uint,
    total-contributed: uint
  }
)

(define-map checkpoint-releases
  { product-id: uint, checkpoint: uint }
  {
    amount: uint,
    released-at: uint,
    is-released: bool
  }
)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER))

(define-private (is-product-manufacturer (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (is-eq tx-sender (get manufacturer product))
    false
  )
)

;; Helper Functions
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee)) u10000)
)

(define-private (update-stakeholder-reputation (stakeholder principal) (amount uint) (success bool))
  (let (
    (current-rep (default-to 
      { score: u100, successful-verifications: u0, total-contributed: u0 }
      (map-get? stakeholder-reputation { stakeholder: stakeholder })
    ))
  )
    (map-set stakeholder-reputation
      { stakeholder: stakeholder }
      {
        score: (if success 
          (+ (get score current-rep) u10) 
          (if (> (get score current-rep) u10) (- (get score current-rep) u10) u0)
        ),
        successful-verifications: (if success (+ (get successful-verifications current-rep) u1) (get successful-verifications current-rep)),
        total-contributed: (+ (get total-contributed current-rep) amount)
      }
    )
  )
)

;; Core Functions
(define-public (create-product 
  (title (string-utf8 200))
  (description (string-utf8 500))
  (verification-cost uint)
  (supply-chain (string-ascii 50))
  (total-checkpoints uint))
  (let ((product-id (var-get next-product-id)))
    (asserts! (> verification-cost u0) ERR-INVALID-AMOUNT)
    (asserts! (> total-checkpoints u0) ERR-INVALID-AMOUNT)
    (asserts! (< (len title) u201) ERR-INVALID-AMOUNT)
    (asserts! (< (len description) u501) ERR-INVALID-AMOUNT)
    (asserts! (< (len supply-chain) u51) ERR-INVALID-AMOUNT)
    
    (map-set products
      { product-id: product-id }
      {
        manufacturer: tx-sender,
        title: title,
        description: description,
        verification-cost: verification-cost,
        current-funding: u0,
        supply-chain: supply-chain,
        status: "verification",
        created-at: block-height,
        checkpoints-completed: u0,
        total-checkpoints: total-checkpoints
      }
    )
    
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

(define-public (contribute-to-verification (product-id uint) (amount uint))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
    (fee (calculate-platform-fee amount))
    (net-amount (- amount fee))
    (current-contribution (default-to 
      { amount: u0, contributed-at: u0 }
      (map-get? stakeholder-contributions { verifier: tx-sender, product-id: product-id })
    ))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status product) "verification") ERR-PRODUCT-COMPLETED)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update platform revenue
    (var-set total-platform-revenue (+ (var-get total-platform-revenue) fee))
    
    ;; Update product funding
    (map-set products
      { product-id: product-id }
      (merge product { current-funding: (+ (get current-funding product) net-amount) })
    )
    
    ;; Update stakeholder contribution
    (map-set stakeholder-contributions
      { verifier: tx-sender, product-id: product-id }
      {
        amount: (+ (get amount current-contribution) net-amount),
        contributed-at: block-height
      }
    )
    
    ;; Update stakeholder reputation
    (update-stakeholder-reputation tx-sender net-amount true)
    
    ;; Check if verification cost is reached
    (if (>= (+ (get current-funding product) net-amount) (get verification-cost product))
      (begin
        (map-set products
          { product-id: product-id }
          (merge product { 
            current-funding: (+ (get current-funding product) net-amount),
            status: "active" 
          })
        )
        (ok { product-activated: true, contribution-amount: net-amount })
      )
      (ok { product-activated: false, contribution-amount: net-amount })
    )
  )
)

(define-public (release-checkpoint-funding (product-id uint) (checkpoint uint))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
    (checkpoint-amount (/ (get current-funding product) (get total-checkpoints product)))
  )
    (asserts! (is-product-manufacturer product-id) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status product) "active") ERR-INVALID-STATUS)
    (asserts! (< checkpoint (get total-checkpoints product)) ERR-VERIFICATION-NOT-READY)
    (asserts! (is-eq checkpoint (get checkpoints-completed product)) ERR-VERIFICATION-NOT-READY)
    (asserts! 
      (is-none (map-get? checkpoint-releases { product-id: product-id, checkpoint: checkpoint }))
      ERR-ALREADY-VALIDATED
    )
    
    ;; Release checkpoint funding
    (try! (as-contract (stx-transfer? checkpoint-amount tx-sender (get manufacturer product))))
    
    ;; Record checkpoint release
    (map-set checkpoint-releases
      { product-id: product-id, checkpoint: checkpoint }
      {
        amount: checkpoint-amount,
        released-at: block-height,
        is-released: true
      }
    )
    
    ;; Update product checkpoints
    (let ((new-checkpoints-completed (+ (get checkpoints-completed product) u1)))
      (map-set products
        { product-id: product-id }
        (merge product { checkpoints-completed: new-checkpoints-completed })
      )
      
      ;; Check if product is authenticated
      (if (is-eq new-checkpoints-completed (get total-checkpoints product))
        (begin
          (map-set products
            { product-id: product-id }
            (merge product { 
              checkpoints-completed: new-checkpoints-completed,
              status: "authenticated" 
            })
          )
          (reward-verifiers product-id)
          (ok { checkpoint-released: checkpoint-amount, product-authenticated: true })
        )
        (ok { checkpoint-released: checkpoint-amount, product-authenticated: false })
      )
    )
  )
)

(define-private (reward-verifiers (product-id uint))
  (let ((product (unwrap! (map-get? products { product-id: product-id }) false)))
    ;; Simple reward mechanism - in a real implementation, you'd iterate through verifiers
    ;; For now, just update the manufacturer's reputation
    (update-stakeholder-reputation (get manufacturer product) (get current-funding product) true)
    true
  )
)

;; Read-only Functions
(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-stakeholder-contribution (verifier principal) (product-id uint))
  (map-get? stakeholder-contributions { verifier: verifier, product-id: product-id })
)

(define-read-only (get-stakeholder-reputation (stakeholder principal))
  (map-get? stakeholder-reputation { stakeholder: stakeholder })
)

(define-read-only (get-platform-stats)
  {
    total-products: (- (var-get next-product-id) u1),
    platform-fee: (var-get platform-fee),
    total-revenue: (var-get total-platform-revenue)
  }
)

(define-read-only (get-checkpoint-info (product-id uint) (checkpoint uint))
  (map-get? checkpoint-releases { product-id: product-id, checkpoint: checkpoint })
)

(define-read-only (calculate-product-progress (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (some {
      funding-progress: (if (> (get verification-cost product) u0)
        (/ (* (get current-funding product) u100) (get verification-cost product))
        u0
      ),
      checkpoint-progress: (if (> (get total-checkpoints product) u0)
        (/ (* (get checkpoints-completed product) u100) (get total-checkpoints product))
        u0
      )
    })
    none
  )
)

;; Admin Functions
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    (asserts! (<= new-fee u1000) ERR-INVALID-AMOUNT) ;; Max 10% fee
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-platform-revenue (amount uint))
  (begin
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    (asserts! (<= amount (var-get total-platform-revenue)) ERR-INSUFFICIENT-FUNDS)
    
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (var-set total-platform-revenue (- (var-get total-platform-revenue) amount))
    (ok amount)
  )
)

;; Emergency Functions
(define-public (emergency-pause-product (product-id uint))
  (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    
    (map-set products
      { product-id: product-id }
      (merge product { status: "failed" })
    )
    (ok true)
  )
)

;; Utility Functions
(define-private (verify-triple-validation (product-id uint))
  ;; Simplified validation - in production, this would check multiple validation sources
  true
)

(define-private (update-supply-chain-pool (supply-chain (string-ascii 50)) (amount uint))
  ;; Simplified supply chain pool update
  true
)