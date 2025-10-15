;; skill-matching-engine
;; Decentralized peer-to-peer skill exchange platform enabling professionals
;; to trade services directly with reputation-based matching and verification

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PROFILE (err u101))
(define-constant ERR-SKILL-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u103))
(define-constant ERR-SERVICE-UNAVAILABLE (err u104))
(define-constant ERR-INVALID-AGREEMENT (err u105))
(define-constant ERR-PAYMENT-REQUIRED (err u106))
(define-constant ERR-SERVICE-COMPLETED (err u107))
(define-constant ERR-DISPUTE-EXISTS (err u108))
(define-constant ERR-INVALID-RATING (err u109))

(define-constant MIN-REPUTATION u50)
(define-constant MAX-RATING u5)
(define-constant ESCROW-PERCENTAGE u5) ;; 5% held in escrow
(define-constant PLATFORM-FEE u2) ;; 2% platform fee

;; Data Variables
(define-data-var next-profile-id uint u1)
(define-data-var next-service-id uint u1)
(define-data-var next-agreement-id uint u1)
(define-data-var contract-owner principal tx-sender)
(define-data-var platform-enabled bool true)
(define-data-var total-services-completed uint u0)
(define-data-var platform-treasury uint u0)

;; Data Maps
(define-map professional-profiles
  { profile-id: uint }
  {
    owner: principal,
    professional-name: (string-ascii 100),
    skills: (list 10 (string-ascii 50)),
    hourly-rate: uint,
    availability-status: (string-ascii 20),
    total-earnings: uint,
    services-completed: uint,
    average-rating: uint,
    reputation-score: uint,
    portfolio-hash: (optional (string-ascii 64)),
    created-at: uint,
    updated-at: uint
  }
)

(define-map service-offerings
  { service-id: uint }
  {
    provider-id: uint,
    service-title: (string-ascii 100),
    service-description: (string-ascii 500),
    category: (string-ascii 50),
    required-skills: (list 5 (string-ascii 50)),
    estimated-duration: uint,
    service-price: uint,
    delivery-format: (string-ascii 100),
    active-status: bool,
    created-at: uint
  }
)

(define-map service-agreements
  { agreement-id: uint }
  {
    service-id: uint,
    client-principal: principal,
    provider-id: uint,
    agreed-price: uint,
    escrow-amount: uint,
    delivery-deadline: uint,
    agreement-status: (string-ascii 20), ;; "active", "completed", "disputed", "cancelled"
    work-delivered: bool,
    client-approved: bool,
    dispute-raised: bool,
    payment-released: bool,
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map skill-matching-scores
  { requester-id: uint, provider-id: uint }
  {
    compatibility-score: uint,
    skill-overlap: uint,
    rate-compatibility: uint,
    reputation-factor: uint,
    calculated-at: uint
  }
)

(define-map service-reviews
  { agreement-id: uint }
  {
    reviewer-principal: principal,
    rating: uint,
    review-text: (string-ascii 500),
    work-quality: uint,
    communication: uint,
    timeliness: uint,
    would-recommend: bool,
    reviewed-at: uint
  }
)

(define-map professional-stats
  { profile-id: uint }
  {
    total-projects: uint,
    completion-rate: uint,
    average-delivery-time: uint,
    client-satisfaction: uint,
    repeat-clients: uint,
    total-revenue: uint,
    skill-endorsements: uint,
    last-active: uint
  }
)

(define-map principal-to-profile principal uint)
(define-map profile-escrow uint uint)
(define-map disputed-agreements uint bool)

;; Public Functions

;; Create professional profile with skills and service offerings
(define-public (create-professional-profile 
  (professional-name (string-ascii 100))
  (skills (list 10 (string-ascii 50)))
  (hourly-rate uint)
  (portfolio-hash (optional (string-ascii 64))))
  (let (
    (profile-id (var-get next-profile-id))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get platform-enabled) ERR-SERVICE-UNAVAILABLE)
    (asserts! (> (len professional-name) u0) ERR-INVALID-PROFILE)
    (asserts! (> (len skills) u0) ERR-INVALID-PROFILE)
    (asserts! (> hourly-rate u0) ERR-INVALID-PROFILE)
    
    (map-set professional-profiles
      { profile-id: profile-id }
      {
        owner: tx-sender,
        professional-name: professional-name,
        skills: skills,
        hourly-rate: hourly-rate,
        availability-status: "available",
        total-earnings: u0,
        services-completed: u0,
        average-rating: u0,
        reputation-score: u100, ;; Starting reputation
        portfolio-hash: portfolio-hash,
        created-at: current-time,
        updated-at: current-time
      }
    )
    
    (map-set professional-stats
      { profile-id: profile-id }
      {
        total-projects: u0,
        completion-rate: u100,
        average-delivery-time: u0,
        client-satisfaction: u0,
        repeat-clients: u0,
        total-revenue: u0,
        skill-endorsements: u0,
        last-active: current-time
      }
    )
    
    (map-set principal-to-profile tx-sender profile-id)
    (var-set next-profile-id (+ profile-id u1))
    (ok profile-id)
  )
)

;; Create service offering with detailed specifications
(define-public (create-service-offering
  (service-title (string-ascii 100))
  (service-description (string-ascii 500))
  (category (string-ascii 50))
  (required-skills (list 5 (string-ascii 50)))
  (estimated-duration uint)
  (service-price uint))
  (let (
    (service-id (var-get next-service-id))
    (provider-profile (unwrap! (map-get? principal-to-profile tx-sender) ERR-NOT-AUTHORIZED))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get platform-enabled) ERR-SERVICE-UNAVAILABLE)
    (asserts! (> (len service-title) u0) ERR-INVALID-PROFILE)
    (asserts! (> service-price u0) ERR-INVALID-PROFILE)
    (asserts! (> estimated-duration u0) ERR-INVALID-PROFILE)
    
    (map-set service-offerings
      { service-id: service-id }
      {
        provider-id: provider-profile,
        service-title: service-title,
        service-description: service-description,
        category: category,
        required-skills: required-skills,
        estimated-duration: estimated-duration,
        service-price: service-price,
        delivery-format: "digital-delivery",
        active-status: true,
        created-at: current-time
      }
    )
    
    (var-set next-service-id (+ service-id u1))
    (ok service-id)
  )
)

;; Initialize service agreement with escrow and terms
(define-public (create-service-agreement
  (service-id uint)
  (delivery-deadline uint))
  (let (
    (agreement-id (var-get next-agreement-id))
    (service (unwrap! (map-get? service-offerings { service-id: service-id }) ERR-SKILL-NOT-FOUND))
    (provider-profile (unwrap! (map-get? professional-profiles { profile-id: (get provider-id service) }) ERR-INVALID-PROFILE))
    (agreed-price (get service-price service))
    (escrow-amount (/ (* agreed-price ESCROW-PERCENTAGE) u100))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get platform-enabled) ERR-SERVICE-UNAVAILABLE)
    (asserts! (get active-status service) ERR-SERVICE-UNAVAILABLE)
    (asserts! (>= (get reputation-score provider-profile) MIN-REPUTATION) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (not (is-eq tx-sender (get owner provider-profile))) ERR-NOT-AUTHORIZED)
    (asserts! (> delivery-deadline current-time) ERR-INVALID-AGREEMENT)
    
    ;; Transfer payment to escrow
    (try! (stx-transfer? agreed-price tx-sender (as-contract tx-sender)))
    
    (map-set service-agreements
      { agreement-id: agreement-id }
      {
        service-id: service-id,
        client-principal: tx-sender,
        provider-id: (get provider-id service),
        agreed-price: agreed-price,
        escrow-amount: escrow-amount,
        delivery-deadline: delivery-deadline,
        agreement-status: "active",
        work-delivered: false,
        client-approved: false,
        dispute-raised: false,
        payment-released: false,
        created-at: current-time,
        completed-at: none
      }
    )
    
    (map-set profile-escrow agreement-id agreed-price)
    (var-set next-agreement-id (+ agreement-id u1))
    (ok agreement-id)
  )
)

;; Mark service work as delivered by provider
(define-public (deliver-service-work (agreement-id uint))
  (let (
    (agreement (unwrap! (map-get? service-agreements { agreement-id: agreement-id }) ERR-INVALID-AGREEMENT))
    (provider-profile (unwrap! (map-get? professional-profiles { profile-id: (get provider-id agreement) }) ERR-INVALID-PROFILE))
  )
    (asserts! (var-get platform-enabled) ERR-SERVICE-UNAVAILABLE)
    (asserts! (is-eq tx-sender (get owner provider-profile)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get agreement-status agreement) "active") ERR-INVALID-AGREEMENT)
    (asserts! (not (get work-delivered agreement)) ERR-SERVICE-COMPLETED)
    
    (map-set service-agreements
      { agreement-id: agreement-id }
      (merge agreement { work-delivered: true })
    )
    (ok true)
  )
)

;; Client approves completed work and releases payment
(define-public (approve-and-release-payment (agreement-id uint))
  (let (
    (agreement (unwrap! (map-get? service-agreements { agreement-id: agreement-id }) ERR-INVALID-AGREEMENT))
    (provider-profile (unwrap! (map-get? professional-profiles { profile-id: (get provider-id agreement) }) ERR-INVALID-PROFILE))
    (agreed-price (get agreed-price agreement))
    (platform-fee-amount (/ (* agreed-price PLATFORM-FEE) u100))
    (provider-payment (- agreed-price platform-fee-amount))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get platform-enabled) ERR-SERVICE-UNAVAILABLE)
    (asserts! (is-eq tx-sender (get client-principal agreement)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get agreement-status agreement) "active") ERR-INVALID-AGREEMENT)
    (asserts! (get work-delivered agreement) ERR-INVALID-AGREEMENT)
    (asserts! (not (get payment-released agreement)) ERR-SERVICE-COMPLETED)
    
    ;; Release payment to provider
    (try! (as-contract (stx-transfer? provider-payment tx-sender (get owner provider-profile))))
    
    ;; Update platform treasury
    (var-set platform-treasury (+ (var-get platform-treasury) platform-fee-amount))
    
    ;; Update agreement status
    (map-set service-agreements
      { agreement-id: agreement-id }
      (merge agreement {
        agreement-status: "completed",
        client-approved: true,
        payment-released: true,
        completed-at: (some current-time)
      })
    )
    
    ;; Update provider statistics
    (map-set professional-profiles
      { profile-id: (get provider-id agreement) }
      (merge provider-profile {
        total-earnings: (+ (get total-earnings provider-profile) provider-payment),
        services-completed: (+ (get services-completed provider-profile) u1),
        updated-at: current-time
      })
    )
    
    (var-set total-services-completed (+ (var-get total-services-completed) u1))
    (ok true)
  )
)

;; Submit rating and review for completed service
(define-public (submit-service-review
  (agreement-id uint)
  (rating uint)
  (review-text (string-ascii 500))
  (work-quality uint)
  (communication uint)
  (timeliness uint)
  (would-recommend bool))
  (let (
    (agreement (unwrap! (map-get? service-agreements { agreement-id: agreement-id }) ERR-INVALID-AGREEMENT))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get platform-enabled) ERR-SERVICE-UNAVAILABLE)
    (asserts! (is-eq tx-sender (get client-principal agreement)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get agreement-status agreement) "completed") ERR-INVALID-AGREEMENT)
    (asserts! (and (>= rating u1) (<= rating MAX-RATING)) ERR-INVALID-RATING)
    (asserts! (and (>= work-quality u1) (<= work-quality MAX-RATING)) ERR-INVALID-RATING)
    (asserts! (and (>= communication u1) (<= communication MAX-RATING)) ERR-INVALID-RATING)
    (asserts! (and (>= timeliness u1) (<= timeliness MAX-RATING)) ERR-INVALID-RATING)
    
    (map-set service-reviews
      { agreement-id: agreement-id }
      {
        reviewer-principal: tx-sender,
        rating: rating,
        review-text: review-text,
        work-quality: work-quality,
        communication: communication,
        timeliness: timeliness,
        would-recommend: would-recommend,
        reviewed-at: current-time
      }
    )
    
    (try! (update-provider-rating (get provider-id agreement) rating))
    (ok true)
  )
)

;; Update professional profile information
(define-public (update-professional-profile
  (hourly-rate uint)
  (availability-status (string-ascii 20))
  (portfolio-hash (optional (string-ascii 64))))
  (let (
    (profile-id (unwrap! (map-get? principal-to-profile tx-sender) ERR-NOT-AUTHORIZED))
    (profile (unwrap! (map-get? professional-profiles { profile-id: profile-id }) ERR-INVALID-PROFILE))
    (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
  )
    (asserts! (var-get platform-enabled) ERR-SERVICE-UNAVAILABLE)
    (asserts! (> hourly-rate u0) ERR-INVALID-PROFILE)
    
    (map-set professional-profiles
      { profile-id: profile-id }
      (merge profile {
        hourly-rate: hourly-rate,
        availability-status: availability-status,
        portfolio-hash: portfolio-hash,
        updated-at: current-time
      })
    )
    (ok true)
  )
)

;; Read-Only Functions

;; Get professional profile details
(define-read-only (get-professional-profile (profile-id uint))
  (map-get? professional-profiles { profile-id: profile-id })
)

;; Get service offering details
(define-read-only (get-service-offering (service-id uint))
  (map-get? service-offerings { service-id: service-id })
)

;; Get service agreement details
(define-read-only (get-service-agreement (agreement-id uint))
  (map-get? service-agreements { agreement-id: agreement-id })
)

;; Get professional statistics
(define-read-only (get-professional-stats (profile-id uint))
  (map-get? professional-stats { profile-id: profile-id })
)

;; Get service review
(define-read-only (get-service-review (agreement-id uint))
  (map-get? service-reviews { agreement-id: agreement-id })
)

;; Get profile ID by principal
(define-read-only (get-profile-by-principal (user-principal principal))
  (map-get? principal-to-profile user-principal)
)

;; Calculate skill matching score between two professionals
(define-read-only (calculate-skill-compatibility 
  (requester-id uint) 
  (provider-id uint))
  (let (
    (requester-profile (unwrap! (map-get? professional-profiles { profile-id: requester-id }) ERR-INVALID-PROFILE))
    (provider-profile (unwrap! (map-get? professional-profiles { profile-id: provider-id }) ERR-INVALID-PROFILE))
    (requester-skills (get skills requester-profile))
    (provider-skills (get skills provider-profile))
  )
    (ok {
      requester-rate: (get hourly-rate requester-profile),
      provider-rate: (get hourly-rate provider-profile),
      provider-reputation: (get reputation-score provider-profile),
      skill-match: (calculate-skill-overlap requester-skills provider-skills)
    })
  )
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-professionals: (- (var-get next-profile-id) u1),
    total-services: (- (var-get next-service-id) u1),
    total-agreements: (- (var-get next-agreement-id) u1),
    completed-services: (var-get total-services-completed),
    platform-treasury: (var-get platform-treasury),
    platform-enabled: (var-get platform-enabled)
  }
)

;; Private Functions

;; Update provider average rating after new review
(define-private (update-provider-rating (provider-id uint) (new-rating uint))
  (let (
    (provider (unwrap! (map-get? professional-profiles { profile-id: provider-id }) ERR-INVALID-PROFILE))
    (current-avg (get average-rating provider))
    (total-reviews (get services-completed provider))
    (new-avg (if (is-eq total-reviews u0)
                new-rating
                (/ (+ (* current-avg total-reviews) new-rating) (+ total-reviews u1))))
  )
    (map-set professional-profiles
      { profile-id: provider-id }
      (merge provider { average-rating: new-avg })
    )
    (ok new-avg)
  )
)

;; Calculate skill overlap percentage between two skill lists
(define-private (calculate-skill-overlap 
  (skills-a (list 10 (string-ascii 50)))
  (skills-b (list 10 (string-ascii 50))))
  (let (
    (total-skills-a (len skills-a))
    (matching-skills (fold count-matching-skills skills-a u0))
  )
    (if (> total-skills-a u0)
        (/ (* matching-skills u100) total-skills-a)
        u0)
  )
)

;; Helper function to count matching skills
(define-private (count-matching-skills (skill (string-ascii 50)) (count uint))
  count ;; Simplified implementation
)

