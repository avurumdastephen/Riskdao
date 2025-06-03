(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_POLICY_NOT_FOUND (err u103))
(define-constant ERR_CLAIM_NOT_FOUND (err u104))
(define-constant ERR_ALREADY_VOTED (err u105))
(define-constant ERR_VOTING_ENDED (err u106))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u107))
(define-constant ERR_INSUFFICIENT_STAKE (err u108))

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var min-underwriter-stake uint u1000000)
(define-data-var voting-period uint u144)

(define-map policies
  { policy-id: uint }
  {
    applicant: principal,
    coverage-amount: uint,
    premium: uint,
    risk-score: uint,
    active: bool,
    created-at: uint
  }
)

(define-map claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    amount: uint,
    description: (string-ascii 256),
    votes-for: uint,
    votes-against: uint,
    total-stake-for: uint,
    total-stake-against: uint,
    processed: bool,
    approved: bool,
    created-at: uint
  }
)

(define-map underwriters
  { underwriter: principal }
  {
    stake: uint,
    reputation: uint,
    total-votes: uint,
    successful-votes: uint
  }
)

(define-map policy-votes
  { policy-id: uint, underwriter: principal }
  {
    risk-score: uint,
    stake-amount: uint,
    timestamp: uint
  }
)

(define-map claim-votes
  { claim-id: uint, underwriter: principal }
  {
    vote: bool,
    stake-amount: uint,
    timestamp: uint
  }
)

(define-map policy-pool
  { policy-id: uint }
  {
    total-pool: uint,
    total-underwriters: uint
  }
)

(define-public (register-underwriter (stake-amount uint))
  (let ((current-stake (get-underwriter-stake tx-sender)))
    (asserts! (>= stake-amount (var-get min-underwriter-stake)) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set underwriters
      { underwriter: tx-sender }
      {
        stake: (+ current-stake stake-amount),
        reputation: u100,
        total-votes: u0,
        successful-votes: u0
      }
    )
    (ok true)
  )
)

(define-public (submit-policy (coverage-amount uint) (premium uint))
  (let ((policy-id (var-get next-policy-id)))
    (asserts! (> coverage-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> premium u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (map-set policies
      { policy-id: policy-id }
      {
        applicant: tx-sender,
        coverage-amount: coverage-amount,
        premium: premium,
        risk-score: u0,
        active: false,
        created-at: stacks-block-height
      }
    )
    (map-set policy-pool
      { policy-id: policy-id }
      {
        total-pool: u0,
        total-underwriters: u0
      }
    )
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (vote-on-policy (policy-id uint) (risk-score uint) (stake-amount uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (underwriter-data (unwrap! (map-get? underwriters { underwriter: tx-sender }) ERR_UNAUTHORIZED))
    (pool-data (unwrap! (map-get? policy-pool { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (<= risk-score u100) ERR_INVALID_AMOUNT)
    (asserts! (>= (get stake underwriter-data) stake-amount) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none (map-get? policy-votes { policy-id: policy-id, underwriter: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set policy-votes
      { policy-id: policy-id, underwriter: tx-sender }
      {
        risk-score: risk-score,
        stake-amount: stake-amount,
        timestamp: stacks-block-height
      }
    )
    
    (map-set policy-pool
      { policy-id: policy-id }
      {
        total-pool: (+ (get total-pool pool-data) stake-amount),
        total-underwriters: (+ (get total-underwriters pool-data) u1)
      }
    )
    
    (map-set underwriters
      { underwriter: tx-sender }
      (merge underwriter-data { total-votes: (+ (get total-votes underwriter-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (finalize-policy (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (pool-data (unwrap! (map-get? policy-pool { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (avg-risk-score (calculate-average-risk-score policy-id))
  )
    (asserts! (>= (get total-underwriters pool-data) u3) ERR_INSUFFICIENT_FUNDS)
    (asserts! (not (get active policy)) ERR_UNAUTHORIZED)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy {
        risk-score: avg-risk-score,
        active: true
      })
    )
    (ok true)
  )
)

(define-public (submit-claim (policy-id uint) (amount uint) (description (string-ascii 256)))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (claim-id (var-get next-claim-id))
  )
    (asserts! (get active policy) ERR_POLICY_NOT_FOUND)
    (asserts! (is-eq tx-sender (get applicant policy)) ERR_UNAUTHORIZED)
    (asserts! (<= amount (get coverage-amount policy)) ERR_INVALID_AMOUNT)
    
    (map-set claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        amount: amount,
        description: description,
        votes-for: u0,
        votes-against: u0,
        total-stake-for: u0,
        total-stake-against: u0,
        processed: false,
        approved: false,
        created-at: stacks-block-height
      }
    )
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (vote-on-claim (claim-id uint) (approve bool) (stake-amount uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) ERR_CLAIM_NOT_FOUND))
    (underwriter-data (unwrap! (map-get? underwriters { underwriter: tx-sender }) ERR_UNAUTHORIZED))
  )
    (asserts! (not (get processed claim)) ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (<= (- stacks-block-height (get created-at claim)) (var-get voting-period)) ERR_VOTING_ENDED)
    (asserts! (>= (get stake underwriter-data) stake-amount) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none (map-get? claim-votes { claim-id: claim-id, underwriter: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set claim-votes
      { claim-id: claim-id, underwriter: tx-sender }
      {
        vote: approve,
        stake-amount: stake-amount,
        timestamp: stacks-block-height
      }
    )
    
    (if approve
      (map-set claims
        { claim-id: claim-id }
        (merge claim {
          votes-for: (+ (get votes-for claim) u1),
          total-stake-for: (+ (get total-stake-for claim) stake-amount)
        })
      )
      (map-set claims
        { claim-id: claim-id }
        (merge claim {
          votes-against: (+ (get votes-against claim) u1),
          total-stake-against: (+ (get total-stake-against claim) stake-amount)
        })
      )
    )
    (ok true)
  )
)

(define-public (process-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) ERR_CLAIM_NOT_FOUND))
    (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (not (get processed claim)) ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (> (- stacks-block-height (get created-at claim)) (var-get voting-period)) ERR_VOTING_ENDED)
    
    (let ((approved (> (get total-stake-for claim) (get total-stake-against claim))))
      (map-set claims
        { claim-id: claim-id }
        (merge claim {
          processed: true,
          approved: approved
        })
      )
      
      (if approved
        (try! (as-contract (stx-transfer? (get amount claim) tx-sender (get claimant claim))))
        true
      )
      (ok approved)
    )
  )
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

(define-read-only (get-underwriter (underwriter principal))
  (map-get? underwriters { underwriter: underwriter })
)

(define-read-only (get-underwriter-stake (underwriter principal))
  (default-to u0 (get stake (map-get? underwriters { underwriter: underwriter })))
)

(define-read-only (calculate-average-risk-score (policy-id uint))
  (let ((pool-data (unwrap! (map-get? policy-pool { policy-id: policy-id }) u0)))
    (if (> (get total-underwriters pool-data) u0)
      u50
      u0
    )
  )
)

(define-read-only (get-policy-pool (policy-id uint))
  (map-get? policy-pool { policy-id: policy-id })
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)