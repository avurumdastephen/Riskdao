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
(define-constant ERR_NO_REWARDS_AVAILABLE (err u109))
(define-constant ERR_ADJUSTMENT_TOO_FREQUENT (err u110))
(define-constant ERR_INVALID_MULTIPLIER (err u111))

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var min-underwriter-stake uint u1000000)
(define-data-var voting-period uint u144)
(define-data-var base-premium-rate uint u1000)
(define-data-var premium-adjustment-period uint u1000)
(define-data-var last-premium-adjustment uint u0)
(define-data-var total-reward-pool uint u0)

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

(define-map premium-history
  { policy-id: uint }
  {
    original-premium: uint,
    adjusted-premium: uint,
    risk-multiplier: uint,
    last-adjustment: uint
  }
)

(define-map underwriter-performance
  { underwriter: principal }
  {
    accurate-votes: uint,
    total-evaluated-claims: uint,
    reward-balance: uint,
    last-reward-claim: uint,
    performance-score: uint
  }
)

(define-map reward-distributions
  { distribution-id: uint }
  {
    total-amount: uint,
    eligible-underwriters: uint,
    per-underwriter-reward: uint,
    distribution-block: uint
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
    (map-set underwriter-performance
      { underwriter: tx-sender }
      {
        accurate-votes: u0,
        total-evaluated-claims: u0,
        reward-balance: u0,
        last-reward-claim: u0,
        performance-score: u50
      }
    )
    (ok true)
  )
)

(define-public (submit-policy (coverage-amount uint) (premium uint))
  (let (
    (policy-id (var-get next-policy-id))
    (adjusted-premium (calculate-dynamic-premium coverage-amount premium))
  )
    (asserts! (> coverage-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> premium u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? adjusted-premium tx-sender (as-contract tx-sender)))
    (map-set policies
      { policy-id: policy-id }
      {
        applicant: tx-sender,
        coverage-amount: coverage-amount,
        premium: adjusted-premium,
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
    (map-set premium-history
      { policy-id: policy-id }
      {
        original-premium: premium,
        adjusted-premium: adjusted-premium,
        risk-multiplier: u100,
        last-adjustment: stacks-block-height
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
      
      (try! (update-underwriter-performance claim-id approved))
      
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

(define-read-only (calculate-dynamic-premium (coverage-amount uint) (base-premium uint))
  (let (
    (market-rate (var-get base-premium-rate))
    (coverage-multiplier (if (> coverage-amount u10000000) u120 u100))
    (dynamic-premium (/ (* base-premium coverage-multiplier) u100))
  )
    (if (> dynamic-premium base-premium)
      (+ base-premium (/ (- dynamic-premium base-premium) u2))
      base-premium
    )
  )
)

(define-public (adjust-premium-rates)
  (let (
    (current-block stacks-block-height)
    (last-adjustment (var-get last-premium-adjustment))
    (adjustment-period (var-get premium-adjustment-period))
  )
    (asserts! (> (- current-block last-adjustment) adjustment-period) ERR_ADJUSTMENT_TOO_FREQUENT)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (let ((new-rate (calculate-market-rate)))
      (var-set base-premium-rate new-rate)
      (var-set last-premium-adjustment current-block)
      (ok new-rate)
    )
  )
)

(define-public (distribute-rewards)
  (let (
    (total-rewards (var-get total-reward-pool))
    (eligible-count (count-eligible-underwriters))
  )
    (asserts! (> total-rewards u0) ERR_NO_REWARDS_AVAILABLE)
    (asserts! (> eligible-count u0) ERR_NO_REWARDS_AVAILABLE)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (let ((reward-per-underwriter (/ total-rewards eligible-count)))
      (try! (process-reward-distribution reward-per-underwriter))
      (var-set total-reward-pool u0)
      (ok reward-per-underwriter)
    )
  )
)

(define-public (claim-underwriter-rewards)
  (let (
    (performance-data (unwrap! (map-get? underwriter-performance { underwriter: tx-sender }) ERR_UNAUTHORIZED))
    (reward-amount (get reward-balance performance-data))
  )
    (asserts! (> reward-amount u0) ERR_NO_REWARDS_AVAILABLE)
    
    (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
    (map-set underwriter-performance
      { underwriter: tx-sender }
      (merge performance-data { 
        reward-balance: u0,
        last-reward-claim: stacks-block-height
      })
    )
    (ok reward-amount)
  )
)

(define-public (update-underwriter-performance (claim-id uint) (claim-approved bool))
  (let (
    (claim (unwrap! (map-get? claims { claim-id: claim-id }) ERR_CLAIM_NOT_FOUND))
    (voters (get-claim-voters claim-id))
  )
    (try! (process-voter-performance voters claim-approved))
    (ok true)
  )
)

(define-public (process-voter-performance (voters (list 50 principal)) (claim-approved bool))
  (fold process-single-voter-performance voters (ok claim-approved))
)

(define-public (process-single-voter-performance (voter principal) (result (response bool uint)))
  (let (
    (claim-approved (unwrap! result result))
    (performance-data (default-to 
      { 
        accurate-votes: u0,
        total-evaluated-claims: u0,
        reward-balance: u0,
        last-reward-claim: u0,
        performance-score: u50
      }
      (map-get? underwriter-performance { underwriter: voter })
    ))
  )
    (let (
      (voter-vote-data (get-voter-decision voter))
      (voter-correct (is-eq voter-vote-data claim-approved))
      (new-accurate-votes (if voter-correct 
        (+ (get accurate-votes performance-data) u1)
        (get accurate-votes performance-data)
      ))
      (new-total-claims (+ (get total-evaluated-claims performance-data) u1))
      (new-score (calculate-performance-score new-accurate-votes new-total-claims))
      (reward-earned (if voter-correct u1000 u0))
    )
      (map-set underwriter-performance
        { underwriter: voter }
        {
          accurate-votes: new-accurate-votes,
          total-evaluated-claims: new-total-claims,
          reward-balance: (+ (get reward-balance performance-data) reward-earned),
          last-reward-claim: (get last-reward-claim performance-data),
          performance-score: new-score
        }
      )
      (ok claim-approved)
    )
  )
)

(define-public (process-reward-distribution (reward-per-underwriter uint))
  (let ((all-underwriters (get-all-eligible-underwriters)))
    (fold add-reward-to-underwriter all-underwriters (ok reward-per-underwriter))
  )
)

(define-public (add-reward-to-underwriter (underwriter principal) (result (response uint uint)))
  (let (
    (reward-amount (unwrap! result result))
    (performance-data (default-to 
      { 
        accurate-votes: u0,
        total-evaluated-claims: u0,
        reward-balance: u0,
        last-reward-claim: u0,
        performance-score: u50
      }
      (map-get? underwriter-performance { underwriter: underwriter })
    ))
  )
    (if (>= (get performance-score performance-data) u60)
      (begin
        (map-set underwriter-performance
          { underwriter: underwriter }
          (merge performance-data { 
            reward-balance: (+ (get reward-balance performance-data) reward-amount)
          })
        )
        (ok reward-amount)
      )
      (ok reward-amount)
    )
  )
)

(define-read-only (calculate-market-rate)
  (let (
    (base-rate (var-get base-premium-rate))
    (market-adjustment (/ (get-contract-balance) u100000))
    (capped-adjustment (if (> market-adjustment u500) u500 market-adjustment))
  )
    (if (> capped-adjustment u0)
      (+ base-rate capped-adjustment)
      base-rate
    )
  )
)

(define-read-only (calculate-performance-score (accurate-votes uint) (total-claims uint))
  (if (> total-claims u0)
    (/ (* accurate-votes u100) total-claims)
    u50
  )
)

(define-read-only (count-eligible-underwriters)
  u10
)

(define-read-only (get-all-eligible-underwriters)
  (list tx-sender)
)

(define-read-only (get-claim-voters (claim-id uint))
  (list tx-sender)
)

(define-read-only (get-voter-decision (voter principal))
  true
)

(define-read-only (get-underwriter-performance (underwriter principal))
  (map-get? underwriter-performance { underwriter: underwriter })
)

(define-read-only (get-premium-history (policy-id uint))
  (map-get? premium-history { policy-id: policy-id })
)

(define-read-only (get-current-premium-rate)
  (var-get base-premium-rate)
)

(define-read-only (get-total-reward-pool)
  (var-get total-reward-pool)
)