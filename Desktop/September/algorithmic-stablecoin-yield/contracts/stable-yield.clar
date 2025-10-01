;; Algorithmic Stablecoin Yield Platform
;; Automated yield generation for stablecoins through algorithmic strategies

;; Define constants and error codes
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_PROTOCOL_NOT_FOUND (err u103))
(define-constant ERR_MIN_DEPOSIT_NOT_MET (err u104))
(define-constant ERR_WITHDRAWAL_LOCKED (err u105))
(define-constant ERR_PROTOCOL_DISABLED (err u106))
(define-constant ERR_REBALANCE_TOO_SOON (err u107))

;; Define data variables
(define-data-var contract-paused bool false)
(define-data-var total-funds-under-management uint u0)
(define-data-var performance-fee-rate uint u100) ;; 1% = 100 basis points
(define-data-var min-deposit-amount uint u1000000) ;; Minimum 1 STX equivalent
(define-data-var rebalance-threshold uint u500) ;; 5% yield difference = rebalance trigger
(define-data-var last-rebalance-block uint u0)
(define-data-var rebalance-cooldown uint u144) ;; ~24 hours in blocks

;; Define maps for protocol management
(define-map yield-protocols 
    {protocol-id: uint}
    {
        name: (string-ascii 50),
        enabled: bool,
        current-apy: uint,
        total-deposited: uint,
        risk-score: uint,
        last-updated: uint
    })

;; Define maps for user accounts
(define-map user-accounts
    {user: principal}
    {
        total-deposited: uint,
        total-earned: uint,
        last-deposit-block: uint,
        deposit-count: uint,
        auto-compound: bool,
        risk-preference: uint ;; 1=conservative, 2=moderate, 3=aggressive
    })

;; Define maps for yield strategies
(define-map yield-strategies
    {strategy-id: uint}
    {
        name: (string-ascii 50),
        enabled: bool,
        target-apy: uint,
        allocated-funds: uint,
        protocols: (list 10 uint),
        min-allocation: uint,
        max-allocation: uint
    })

;; Define maps for arbitrage opportunities
(define-map arbitrage-opportunities
    {opportunity-id: uint}
    {
        protocol-from: uint,
        protocol-to: uint,
        rate-difference: uint,
        potential-profit: uint,
        execution-cost: uint,
        expires-at: uint,
        executed: bool
    })

;; Define maps for compound tracking
(define-map compound-history
    {user: principal, compound-id: uint}
    {
        amount-compounded: uint,
        yield-earned: uint,
        compound-block: uint,
        new-apy: uint
    })

;; Administrative functions

;; Register a new yield protocol
(define-public (register-protocol 
    (protocol-id uint) 
    (name (string-ascii 50)) 
    (initial-apy uint) 
    (risk-score uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? yield-protocols {protocol-id: protocol-id})) (err u400))
        (map-set yield-protocols
            {protocol-id: protocol-id}
            {
                name: name,
                enabled: true,
                current-apy: initial-apy,
                total-deposited: u0,
                risk-score: risk-score,
                last-updated: block-height
            })
        (ok protocol-id)))

;; Update protocol APY (oracle integration)
(define-public (update-protocol-apy (protocol-id uint) (new-apy uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (match (map-get? yield-protocols {protocol-id: protocol-id})
            protocol
            (begin
                (map-set yield-protocols
                    {protocol-id: protocol-id}
                    (merge protocol {current-apy: new-apy, last-updated: block-height}))
                (ok new-apy))
            ERR_PROTOCOL_NOT_FOUND)))

;; Create yield strategy
(define-public (create-strategy
    (strategy-id uint)
    (name (string-ascii 50))
    (target-apy uint)
    (protocols (list 10 uint))
    (min-allocation uint)
    (max-allocation uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set yield-strategies
            {strategy-id: strategy-id}
            {
                name: name,
                enabled: true,
                target-apy: target-apy,
                allocated-funds: u0,
                protocols: protocols,
                min-allocation: min-allocation,
                max-allocation: max-allocation
            })
        (ok strategy-id)))

;; User interaction functions

;; Deposit stablecoins for yield generation
(define-public (deposit-for-yield (amount uint) (risk-preference uint))
    (let
        ((user-account (default-to 
            {total-deposited: u0, total-earned: u0, last-deposit-block: u0, deposit-count: u0, auto-compound: true, risk-preference: u1}
            (map-get? user-accounts {user: tx-sender}))))
        (begin
            (asserts! (not (var-get contract-paused)) (err u500))
            (asserts! (> amount u0) ERR_INVALID_AMOUNT)
            (asserts! (>= amount (var-get min-deposit-amount)) ERR_MIN_DEPOSIT_NOT_MET)
            (asserts! (<= risk-preference u3) (err u600))
            
            ;; Update user account
            (map-set user-accounts
                {user: tx-sender}
                {
                    total-deposited: (+ (get total-deposited user-account) amount),
                    total-earned: (get total-earned user-account),
                    last-deposit-block: block-height,
                    deposit-count: (+ (get deposit-count user-account) u1),
                    auto-compound: (get auto-compound user-account),
                    risk-preference: risk-preference
                })
            
            ;; Update total funds under management
            (var-set total-funds-under-management 
                (+ (var-get total-funds-under-management) amount))
            
            ;; Strategy deployment would be triggered here in production
            ;; (try! (deploy-to-optimal-strategy amount risk-preference))
            
            (ok amount))))

;; Compound earned yield
(define-public (compound-yield)
    (let
        ((user-account (unwrap! (map-get? user-accounts {user: tx-sender}) (err u404)))
         (earned-yield (calculate-user-yield tx-sender)))
        (begin
            (asserts! (> earned-yield u0) ERR_INVALID_AMOUNT)
            
            ;; Update user account with compounded yield
            (map-set user-accounts
                {user: tx-sender}
                (merge user-account 
                    {
                        total-deposited: (+ (get total-deposited user-account) earned-yield),
                        total-earned: (+ (get total-earned user-account) earned-yield)
                    }))
            
            ;; Record compound history
            (map-set compound-history
                {user: tx-sender, compound-id: (get deposit-count user-account)}
                {
                    amount-compounded: earned-yield,
                    yield-earned: earned-yield,
                    compound-block: block-height,
                    new-apy: (calculate-user-apy tx-sender)
                })
            
            (ok earned-yield))))

;; Execute arbitrage opportunity
(define-public (execute-arbitrage (opportunity-id uint))
    (let
        ((opportunity (unwrap! (map-get? arbitrage-opportunities {opportunity-id: opportunity-id}) (err u404))))
        (begin
            (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
            (asserts! (not (get executed opportunity)) (err u601))
            (asserts! (<= block-height (get expires-at opportunity)) (err u602))
            (asserts! (> (get potential-profit opportunity) (get execution-cost opportunity)) (err u603))
            
            ;; Mark opportunity as executed
            (map-set arbitrage-opportunities
                {opportunity-id: opportunity-id}
                (merge opportunity {executed: true}))
            
            ;; Execute the arbitrage logic would go here
            ;; This would involve flash loans, protocol interactions, etc.
            
            (ok (get potential-profit opportunity)))))

;; Automated rebalancing
(define-public (rebalance-portfolios)
    (let
        ((blocks-since-last (- block-height (var-get last-rebalance-block))))
        (begin
            (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
            (asserts! (>= blocks-since-last (var-get rebalance-cooldown)) ERR_REBALANCE_TOO_SOON)
            
            ;; Update last rebalance block
            (var-set last-rebalance-block block-height)
            
            ;; Rebalancing logic would analyze all protocols and strategies
            ;; Move funds from lower-yielding to higher-yielding opportunities
            
            (ok blocks-since-last))))

;; Withdraw funds with accrued yield
(define-public (withdraw-funds (amount uint))
    (let
        ((user-account (unwrap! (map-get? user-accounts {user: tx-sender}) (err u404)))
         (total-available (+ (get total-deposited user-account) (calculate-user-yield tx-sender))))
        (begin
            (asserts! (not (var-get contract-paused)) (err u500))
            (asserts! (> amount u0) ERR_INVALID_AMOUNT)
            (asserts! (<= amount total-available) ERR_INSUFFICIENT_FUNDS)
            
            ;; Calculate performance fees
            (let ((performance-fee (calculate-performance-fee tx-sender amount)))
                ;; Update user account
                (if (<= amount total-available)
                    (map-set user-accounts
                        {user: tx-sender}
                        (merge user-account 
                            {total-deposited: (- (get total-deposited user-account) amount)}))
                    false)
                
                ;; Update total funds under management
                (var-set total-funds-under-management 
                    (- (var-get total-funds-under-management) amount))
                
                (ok (- amount performance-fee))))))

;; Private helper functions

;; Deploy funds to optimal strategy based on risk preference
(define-private (deploy-to-optimal-strategy (amount uint) (risk-preference uint))
    (let
        ((optimal-strategy (find-optimal-strategy risk-preference)))
        (if (is-some optimal-strategy)
            (ok amount)
            (ok amount)))) ;; Default deployment strategy

;; Find optimal strategy for risk preference
(define-private (find-optimal-strategy (risk-preference uint))
    (some u1)) ;; Simplified - would implement complex strategy selection

;; Calculate user's accrued yield
(define-private (calculate-user-yield (user principal))
    (match (map-get? user-accounts {user: user})
        user-account
        ;; Simplified yield calculation - would implement complex APY calculations
        (/ (* (get total-deposited user-account) u500) u10000) ;; 5% simplified
        u0))

;; Calculate user's current APY
(define-private (calculate-user-apy (user principal))
    u500) ;; 5% simplified - would calculate based on current deployments

;; Calculate performance fees
(define-private (calculate-performance-fee (user principal) (withdrawal-amount uint))
    (let
        ((yield-earned (calculate-user-yield user)))
        (if (> yield-earned u0)
            (/ (* yield-earned (var-get performance-fee-rate)) u10000)
            u0)))

;; Read-only functions

;; Get protocol information
(define-read-only (get-protocol-info (protocol-id uint))
    (map-get? yield-protocols {protocol-id: protocol-id}))

;; Get user account details
(define-read-only (get-user-account (user principal))
    (map-get? user-accounts {user: user}))

;; Get user's total balance including yield
(define-read-only (get-user-total-balance (user principal))
    (match (map-get? user-accounts {user: user})
        user-account
        (+ (get total-deposited user-account) (calculate-user-yield user))
        u0))

;; Get strategy information
(define-read-only (get-strategy-info (strategy-id uint))
    (map-get? yield-strategies {strategy-id: strategy-id}))

;; Get arbitrage opportunity
(define-read-only (get-arbitrage-opportunity (opportunity-id uint))
    (map-get? arbitrage-opportunities {opportunity-id: opportunity-id}))

;; Get platform statistics
(define-read-only (get-platform-stats)
    {
        total-funds: (var-get total-funds-under-management),
        performance-fee: (var-get performance-fee-rate),
        min-deposit: (var-get min-deposit-amount),
        contract-paused: (var-get contract-paused),
        last-rebalance: (var-get last-rebalance-block)
    })

;; Get compound history for user
(define-read-only (get-user-compound-history (user principal) (compound-id uint))
    (map-get? compound-history {user: user, compound-id: compound-id}))

;; Calculate optimal allocation for user
(define-read-only (calculate-optimal-allocation (user principal))
    (match (map-get? user-accounts {user: user})
        user-account
        {
            conservative: (if (is-eq (get risk-preference user-account) u1) u70 u0),
            moderate: (if (is-eq (get risk-preference user-account) u2) u70 u0),
            aggressive: (if (is-eq (get risk-preference user-account) u3) u70 u0)
        }
        {conservative: u0, moderate: u0, aggressive: u0}))

;; Emergency functions

;; Emergency pause contract
(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused true)
        (ok true)))

;; Emergency unpause contract
(define-public (emergency-unpause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-paused false)
        (ok true)))