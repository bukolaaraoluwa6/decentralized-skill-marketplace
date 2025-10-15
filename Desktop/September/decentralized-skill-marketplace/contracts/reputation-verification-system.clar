;; reputation-verification-system
;; Decentralized professional reputation and credential verification system
;; enabling peer validation and trust scoring for skill marketplace participants

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-PROFILE (err u201))
(define-constant ERR-INSUFFICIENT-STAKE (err u202))
(define-constant ERR-VALIDATION-EXISTS (err u203))
(define-constant ERR-INVALID-CREDENTIAL (err u204))
(define-constant ERR-DISPUTE-ACTIVE (err u205))
(define-constant ERR-VALIDATION-EXPIRED (err u206))
(define-constant ERR-INVALID-VOTE (err u207))
(define-constant ERR-ALREADY-VOTED (err u208))
(define-constant ERR-INSUFFICIENT-VALIDATORS (err u209))

(define-constant MIN-VALIDATOR-STAKE u1000000) ;; 1 STX in microSTX
(define-constant VALIDATION-PERIOD u144) ;; 24 hours in blocks
(define-constant MIN-VALIDATORS-REQUIRED u3)
(define-constant CONSENSUS-THRESHOLD u60) ;; 60% consensus required
(define-constant REPUTATION-DECAY-RATE u5) ;; 5% per period
(define-constant MAX-REPUTATION-SCORE u1000)
(define-constant VALIDATOR-REWARD u50000) ;; 0.05 STX reward

;; Data Variables
(define-data-var next-validation-id uint u1)
(define-data-var next-credential-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var contract-owner principal tx-sender)
(define-data-var system-enabled bool true)
(define-data-var total-validators uint u0)
(define-data-var validation-treasury uint u0)
(define-data-var reputation-update-period uint u1008) ;; Weekly updates

;; Data Maps
(define-map validator-profiles
  { validator-principal: principal }
  {
    stake-amount: uint,
    reputation-score: uint,
    total-validations: uint,
    successful-validations: uint,
    accuracy-rating: uint,
    registration-time: uint,
    last-active: uint,
    validator-status: (string-ascii 20),
    specialized-domains: (list 5 (string-ascii 50))
  }
)

(define-map professional-credentials
  { credential-id: uint }
  {
    professional-principal: principal,
    credential-type: (string-ascii 50),
    credential-description: (string-ascii 300),
    evidence-hash: (string-ascii 64),
    issuer-organization: (string-ascii 100),
    credential-status: (string-ascii 20), ;; "pending", "verified", "rejected", "disputed"
    verification-score: uint,
    expiration-date: (optional uint),
    created-at: uint,
    verified-at: (optional uint)
  }
)

(define-map validation-requests
  { validation-id: uint }
  {
    credential-id: uint,
    requester-principal: principal,
    validation-type: (string-ascii 30),
    stake-deposit: uint,
    validation-deadline: uint,
    assigned-validators: (list 5 principal),
    consensus-reached: bool,
    validation-result: (optional bool),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map validator-votes
  { validation-id: uint, validator-principal: principal }
  {
    vote-decision: bool, ;; true = approve, false = reject
    confidence-level: uint, ;; 1-100
    evidence-assessment: (string-ascii 200),
    vote-timestamp: uint,
    stake-weight: uint
  }
)

(define-map reputation-scores
  { professional-principal: principal }
  {
    base-reputation: uint,
    peer-validation-score: uint,
    credential-verification-score: uint,
    performance-history-score: uint,
    community-trust-score: uint,
    total-reputation: uint,
    last-updated: uint,
    reputation-trend: (string-ascii 10) ;; "rising", "stable", "declining"
  }
)

(define-map trust-relationships
  { validator-principal: principal, professional-principal: principal }
  {
    trust-level: uint, ;; 1-100
    endorsement-count: uint,
    interaction-history: uint,
    last-interaction: uint,
    trust-status: (string-ascii 15) ;; "trusted", "neutral", "flagged"
  }
)

(define-map dispute-cases
  { dispute-id: uint }
  {
    disputed-credential-id: uint,
    disputer-principal: principal,
    dispute-reason: (string-ascii 300),
    evidence-provided: (string-ascii 64),
    dispute-status: (string-ascii 20), ;; "open", "investigating", "resolved"
    arbitration-result: (optional bool),
    resolution-timestamp: (optional uint),
    created-at: uint
  }
)

(define-map peer-endorsements
  { endorser-principal: principal, endorsed-principal: principal, skill-domain: (string-ascii 50) }
  {
    endorsement-strength: uint, ;; 1-10
    endorsement-context: (string-ascii 200),
    verified-collaboration: bool,
    endorsement-timestamp: uint
  }
)

(define-map credential-votes uint uint) ;; validation-id -> vote count
(define-map validator-earnings principal uint)
(define-map professional-validation-history principal (list 20 uint))

;; Public Functions

;; Register as a validator with stake deposit
(define-public (register-validator 
  (stake-amount uint)
  (specialized-domains (list 5 (string-ascii 50))))
  (let (
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get system-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (>= stake-amount MIN-VALIDATOR-STAKE) ERR-INSUFFICIENT-STAKE)
    (asserts! (is-none (map-get? validator-profiles { validator-principal: tx-sender })) ERR-VALIDATION-EXISTS)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set validator-profiles
      { validator-principal: tx-sender }
      {
        stake-amount: stake-amount,
        reputation-score: u100, ;; Starting reputation
        total-validations: u0,
        successful-validations: u0,
        accuracy-rating: u100,
        registration-time: current-time,
        last-active: current-time,
        validator-status: "active",
        specialized-domains: specialized-domains
      }
    )
    
    (map-set validator-earnings tx-sender u0)
    (var-set total-validators (+ (var-get total-validators) u1))
    (ok true)
  )
)

;; Submit professional credential for verification
(define-public (submit-credential
  (credential-type (string-ascii 50))
  (credential-description (string-ascii 300))
  (evidence-hash (string-ascii 64))
  (issuer-organization (string-ascii 100))
  (expiration-date (optional uint)))
  (let (
    (credential-id (var-get next-credential-id))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get system-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (> (len credential-type) u0) ERR-INVALID-CREDENTIAL)
    (asserts! (> (len evidence-hash) u0) ERR-INVALID-CREDENTIAL)
    
    (map-set professional-credentials
      { credential-id: credential-id }
      {
        professional-principal: tx-sender,
        credential-type: credential-type,
        credential-description: credential-description,
        evidence-hash: evidence-hash,
        issuer-organization: issuer-organization,
        credential-status: "pending",
        verification-score: u0,
        expiration-date: expiration-date,
        created-at: current-time,
        verified-at: none
      }
    )
    
    (var-set next-credential-id (+ credential-id u1))
    (ok credential-id)
  )
)

;; Request validation for submitted credential
(define-public (request-credential-validation
  (credential-id uint)
  (validation-type (string-ascii 30))
  (stake-deposit uint))
  (let (
    (validation-id (var-get next-validation-id))
    (credential (unwrap! (map-get? professional-credentials { credential-id: credential-id }) ERR-INVALID-CREDENTIAL))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    (validation-deadline (+ current-time VALIDATION-PERIOD))
  )
    (asserts! (var-get system-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get professional-principal credential) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get credential-status credential) "pending") ERR-VALIDATION-EXISTS)
    (asserts! (>= (var-get total-validators) MIN-VALIDATORS-REQUIRED) ERR-INSUFFICIENT-VALIDATORS)
    (asserts! (> stake-deposit u0) ERR-INSUFFICIENT-STAKE)
    
    ;; Transfer validation stake
    (try! (stx-transfer? stake-deposit tx-sender (as-contract tx-sender)))
    
    (map-set validation-requests
      { validation-id: validation-id }
      {
        credential-id: credential-id,
        requester-principal: tx-sender,
        validation-type: validation-type,
        stake-deposit: stake-deposit,
        validation-deadline: validation-deadline,
        assigned-validators: (list),
        consensus-reached: false,
        validation-result: none,
        created-at: current-time,
        completed-at: none
      }
    )
    
    (var-set next-validation-id (+ validation-id u1))
    (ok validation-id)
  )
)

;; Submit validation vote as assigned validator
(define-public (submit-validation-vote
  (validation-id uint)
  (vote-decision bool)
  (confidence-level uint)
  (evidence-assessment (string-ascii 200)))
  (let (
    (validation-request (unwrap! (map-get? validation-requests { validation-id: validation-id }) ERR-INVALID-VOTE))
    (validator-profile (unwrap! (map-get? validator-profiles { validator-principal: tx-sender }) ERR-NOT-AUTHORIZED))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    (stake-weight (get stake-amount validator-profile))
  )
    (asserts! (var-get system-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (< current-time (get validation-deadline validation-request)) ERR-VALIDATION-EXPIRED)
    (asserts! (is-none (map-get? validator-votes { validation-id: validation-id, validator-principal: tx-sender })) ERR-ALREADY-VOTED)
    (asserts! (and (>= confidence-level u1) (<= confidence-level u100)) ERR-INVALID-VOTE)
    
    (map-set validator-votes
      { validation-id: validation-id, validator-principal: tx-sender }
      {
        vote-decision: vote-decision,
        confidence-level: confidence-level,
        evidence-assessment: evidence-assessment,
        vote-timestamp: current-time,
        stake-weight: stake-weight
      }
    )
    
    ;; Update validator activity
    (map-set validator-profiles
      { validator-principal: tx-sender }
      (merge validator-profile {
        total-validations: (+ (get total-validations validator-profile) u1),
        last-active: current-time
      })
    )
    
    ;; Check if consensus reached
    (try! (evaluate-validation-consensus validation-id))
    (ok true)
  )
)

;; Provide peer endorsement for professional skills
(define-public (endorse-professional-skill
  (endorsed-principal principal)
  (skill-domain (string-ascii 50))
  (endorsement-strength uint)
  (endorsement-context (string-ascii 200))
  (verified-collaboration bool))
  (let (
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get system-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq tx-sender endorsed-principal)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= endorsement-strength u1) (<= endorsement-strength u10)) ERR-INVALID-VOTE)
    
    (map-set peer-endorsements
      { endorser-principal: tx-sender, endorsed-principal: endorsed-principal, skill-domain: skill-domain }
      {
        endorsement-strength: endorsement-strength,
        endorsement-context: endorsement-context,
        verified-collaboration: verified-collaboration,
        endorsement-timestamp: current-time
      }
    )
    (ok true)
  )
)

;; File dispute against credential verification
(define-public (file-credential-dispute
  (credential-id uint)
  (dispute-reason (string-ascii 300))
  (evidence-provided (string-ascii 64)))
  (let (
    (dispute-id (var-get next-dispute-id))
    (credential (unwrap! (map-get? professional-credentials { credential-id: credential-id }) ERR-INVALID-CREDENTIAL))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get system-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get credential-status credential) "verified") ERR-INVALID-CREDENTIAL)
    (asserts! (> (len dispute-reason) u0) ERR-INVALID-CREDENTIAL)
    
    (map-set dispute-cases
      { dispute-id: dispute-id }
      {
        disputed-credential-id: credential-id,
        disputer-principal: tx-sender,
        dispute-reason: dispute-reason,
        evidence-provided: evidence-provided,
        dispute-status: "open",
        arbitration-result: none,
        resolution-timestamp: none,
        created-at: current-time
      }
    )
    
    ;; Update credential status
    (map-set professional-credentials
      { credential-id: credential-id }
      (merge credential { credential-status: "disputed" })
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

;; Update professional reputation score based on multiple factors
(define-public (update-reputation-score (professional-principal principal))
  (let (
    (current-reputation (match (map-get? reputation-scores { professional-principal: professional-principal })
      some-rep some-rep
      { base-reputation: u100, peer-validation-score: u0, credential-verification-score: u0,
        performance-history-score: u0, community-trust-score: u0, total-reputation: u100,
        last-updated: u0, reputation-trend: "stable" }))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    (peer-score (calculate-peer-validation-score professional-principal))
    (credential-score (calculate-credential-score professional-principal))
    (trust-score (calculate-community-trust-score professional-principal))
    (new-total (/ (+ peer-score credential-score trust-score) u3))
  )
    (asserts! (var-get system-enabled) ERR-NOT-AUTHORIZED)
    
    (map-set reputation-scores
      { professional-principal: professional-principal }
      (merge current-reputation {
        peer-validation-score: peer-score,
        credential-verification-score: credential-score,
        community-trust-score: trust-score,
        total-reputation: (if (< new-total MAX-REPUTATION-SCORE) new-total MAX-REPUTATION-SCORE),
        last-updated: current-time,
        reputation-trend: (determine-reputation-trend (get total-reputation current-reputation) new-total)
      })
    )
    (ok new-total)
  )
)

;; Read-Only Functions

;; Get validator profile information
(define-read-only (get-validator-profile (validator-principal principal))
  (map-get? validator-profiles { validator-principal: validator-principal })
)

;; Get professional credential details
(define-read-only (get-credential (credential-id uint))
  (map-get? professional-credentials { credential-id: credential-id })
)

;; Get validation request information
(define-read-only (get-validation-request (validation-id uint))
  (map-get? validation-requests { validation-id: validation-id })
)

;; Get professional reputation score
(define-read-only (get-reputation-score (professional-principal principal))
  (map-get? reputation-scores { professional-principal: professional-principal })
)

;; Get trust relationship between parties
(define-read-only (get-trust-relationship (validator-principal principal) (professional-principal principal))
  (map-get? trust-relationships { validator-principal: validator-principal, professional-principal: professional-principal })
)

;; Get dispute case details
(define-read-only (get-dispute-case (dispute-id uint))
  (map-get? dispute-cases { dispute-id: dispute-id })
)

;; Get peer endorsement information
(define-read-only (get-peer-endorsement (endorser-principal principal) (endorsed-principal principal) (skill-domain (string-ascii 50)))
  (map-get? peer-endorsements { endorser-principal: endorser-principal, endorsed-principal: endorsed-principal, skill-domain: skill-domain })
)

;; Get system statistics
(define-read-only (get-system-stats)
  {
    total-validators: (var-get total-validators),
    total-credentials: (- (var-get next-credential-id) u1),
    total-validations: (- (var-get next-validation-id) u1),
    total-disputes: (- (var-get next-dispute-id) u1),
    validation-treasury: (var-get validation-treasury),
    system-enabled: (var-get system-enabled)
  }
)

;; Private Functions

;; Evaluate validation consensus and finalize result
(define-private (evaluate-validation-consensus (validation-id uint))
  (let (
    (validation-request (unwrap! (map-get? validation-requests { validation-id: validation-id }) ERR-INVALID-VOTE))
    (total-votes (default-to u0 (map-get? credential-votes validation-id)))
  )
    ;; Simplified consensus evaluation
    (if (>= total-votes MIN-VALIDATORS-REQUIRED)
      (begin
        (map-set validation-requests
          { validation-id: validation-id }
          (merge validation-request { consensus-reached: true })
        )
        (ok true)
      )
      (ok false)
    )
  )
)

;; Calculate peer validation score for professional
(define-private (calculate-peer-validation-score (professional-principal principal))
  u75 ;; Simplified calculation returning base score
)

;; Calculate credential verification score
(define-private (calculate-credential-score (professional-principal principal))
  u80 ;; Simplified calculation returning base score
)

;; Calculate community trust score
(define-private (calculate-community-trust-score (professional-principal principal))
  u70 ;; Simplified calculation returning base score
)

;; Determine reputation trend based on score changes
(define-private (determine-reputation-trend (old-score uint) (new-score uint))
  (if (> new-score old-score)
      "rising"
      (if (< new-score old-score)
          "declining"
          "stable"))
)

;; Update trust relationship between validator and professional
(define-private (update-trust-relationship (validator-principal principal) (professional-principal principal) (interaction-strength uint))
  (let (
    (current-trust (match (map-get? trust-relationships { validator-principal: validator-principal, professional-principal: professional-principal })
      some-trust some-trust
      { trust-level: u50, endorsement-count: u0, interaction-history: u0, last-interaction: u0, trust-status: "neutral" }))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    (new-trust-level (let ((calculated-trust (+ (get trust-level current-trust) (/ interaction-strength u2)))) (if (< calculated-trust u100) calculated-trust u100)))
  )
    (map-set trust-relationships
      { validator-principal: validator-principal, professional-principal: professional-principal }
      (merge current-trust {
        trust-level: new-trust-level,
        endorsement-count: (+ (get endorsement-count current-trust) u1),
        interaction-history: (+ (get interaction-history current-trust) u1),
        last-interaction: current-time,
        trust-status: (if (>= new-trust-level u70) "trusted" "neutral")
      })
    )
    (ok true)
  )
)

