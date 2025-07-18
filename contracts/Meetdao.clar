(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-PROPOSAL-INACTIVE (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-VOTING-ENDED (err u104))
(define-constant ERR-INSUFFICIENT-VOTES (err u105))
(define-constant ERR-PROPOSAL-NOT-EXECUTABLE (err u106))
(define-constant ERR-INVALID-DURATION (err u107))
(define-constant ERR-PROPOSAL-ALREADY-FINALIZED (err u108))
(define-constant ERR-MEETING-NOT-FOUND (err u109))
(define-constant ERR-INVALID-MEETING-STATUS (err u110))
(define-constant ERR-BUDGET-NOT-FOUND (err u111))
(define-constant ERR-INSUFFICIENT-BUDGET (err u112))
(define-constant ERR-INVALID-AMOUNT (err u113))
(define-constant ERR-BUDGET-ALREADY-EXISTS (err u114))
(define-constant ERR-EXPENSE-NOT-FOUND (err u115))
(define-constant ERR-EXPENSE-ALREADY-APPROVED (err u116))
(define-constant ERR-INVALID-CATEGORY (err u117))

(define-constant PROPOSAL-STATUS-ACTIVE u1)
(define-constant PROPOSAL-STATUS-EXECUTED u2)
(define-constant PROPOSAL-STATUS-REJECTED u3)
(define-constant PROPOSAL-STATUS-EXPIRED u4)

(define-constant MEETING-STATUS-SCHEDULED u1)
(define-constant MEETING-STATUS-ACTIVE u2)
(define-constant MEETING-STATUS-COMPLETED u3)
(define-constant MEETING-STATUS-CANCELLED u4)

(define-constant BUDGET-STATUS-ACTIVE u1)
(define-constant BUDGET-STATUS-EXHAUSTED u2)
(define-constant BUDGET-STATUS-FROZEN u3)

(define-constant EXPENSE-STATUS-PENDING u1)
(define-constant EXPENSE-STATUS-APPROVED u2)
(define-constant EXPENSE-STATUS-REJECTED u3)

(define-constant MIN-VOTING-DURATION u144)
(define-constant MAX-VOTING-DURATION u4320)
(define-constant MIN-QUORUM-PERCENTAGE u10)

(define-data-var contract-owner principal tx-sender)
(define-data-var proposal-counter uint u0)
(define-data-var meeting-counter uint u0)
(define-data-var min-proposal-deposit uint u1000000)
(define-data-var default-voting-duration uint u1008)
(define-data-var budget-counter uint u0)
(define-data-var expense-counter uint u0)
(define-data-var total-treasury-balance uint u0)

(define-map proposals uint {
    id: uint,
    proposer: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    meeting-id: uint,
    start-block: uint,
    end-block: uint,
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint,
    status: uint,
    execution-delay: uint,
    deposit-amount: uint,
    created-at: uint
})

(define-map meetings uint {
    id: uint,
    organizer: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    scheduled-block: uint,
    duration-blocks: uint,
    status: uint,
    total-proposals: uint,
    attendee-count: uint,
    created-at: uint
})

(define-map votes { proposal-id: uint, voter: principal } {
    vote-type: uint,
    voting-power: uint,
    block-height: uint
})

(define-map proposal-deposits { proposal-id: uint } {
    depositor: principal,
    amount: uint,
    refunded: bool
})

(define-map meeting-attendees { meeting-id: uint, attendee: principal } {
    joined-at: uint,
    voting-power: uint
})

(define-map user-voting-power principal uint)

(define-map budgets uint {
    id: uint,
    category: (string-utf8 50),
    allocated-amount: uint,
    spent-amount: uint,
    remaining-amount: uint,
    created-by: principal,
    created-at: uint,
    status: uint,
    quarter: uint,
    description: (string-utf8 200)
})

(define-map expenses uint {
    id: uint,
    budget-id: uint,
    amount: uint,
    recipient: principal,
    description: (string-utf8 200),
    submitted-by: principal,
    submitted-at: uint,
    approved-by: (optional principal),
    approved-at: (optional uint),
    status: uint,
    category: (string-utf8 50)
})

(define-map budget-approvals { budget-id: uint, approver: principal } {
    approved: bool,
    voted-at: uint
})

(define-map expense-approvals { expense-id: uint, approver: principal } {
    approved: bool,
    voted-at: uint
})

(define-map treasury-contributions principal uint)

(define-public (create-meeting (title (string-utf8 100)) (description (string-utf8 500)) (scheduled-block uint) (duration-blocks uint))
    (let ((meeting-id (+ (var-get meeting-counter) u1))
          (current-block stacks-block-height))
        (asserts! (> scheduled-block current-block) ERR-INVALID-DURATION)
        (asserts! (and (>= duration-blocks u10) (<= duration-blocks u1440)) ERR-INVALID-DURATION)
        (map-set meetings meeting-id {
            id: meeting-id,
            organizer: tx-sender,
            title: title,
            description: description,
            scheduled-block: scheduled-block,
            duration-blocks: duration-blocks,
            status: MEETING-STATUS-SCHEDULED,
            total-proposals: u0,
            attendee-count: u0,
            created-at: current-block
        })
        (var-set meeting-counter meeting-id)
        (ok meeting-id)))

(define-public (create-proposal (title (string-utf8 100)) (description (string-utf8 500)) (meeting-id uint) (execution-delay uint))
    (let ((proposal-id (+ (var-get proposal-counter) u1))
          (current-block stacks-block-height)
          (voting-duration (var-get default-voting-duration))
          (deposit-amount (var-get min-proposal-deposit)))
        (asserts! (is-some (map-get? meetings meeting-id)) ERR-MEETING-NOT-FOUND)
        (asserts! (and (>= execution-delay u0) (<= execution-delay u4320)) ERR-INVALID-DURATION)
        (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
        (map-set proposals proposal-id {
            id: proposal-id,
            proposer: tx-sender,
            title: title,
            description: description,
            meeting-id: meeting-id,
            start-block: current-block,
            end-block: (+ current-block voting-duration),
            yes-votes: u0,
            no-votes: u0,
            abstain-votes: u0,
            status: PROPOSAL-STATUS-ACTIVE,
            execution-delay: execution-delay,
            deposit-amount: deposit-amount,
            created-at: current-block
        })
        (map-set proposal-deposits { proposal-id: proposal-id } {
            depositor: tx-sender,
            amount: deposit-amount,
            refunded: false
        })
        (var-set proposal-counter proposal-id)
        (match (map-get? meetings meeting-id)
            meeting (map-set meetings meeting-id (merge meeting { total-proposals: (+ (get total-proposals meeting) u1) }))
            false)
        (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote-type uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (current-block stacks-block-height)
          (voter-power (default-to u1 (map-get? user-voting-power tx-sender))))
        (asserts! (is-eq (get status proposal) PROPOSAL-STATUS-ACTIVE) ERR-PROPOSAL-INACTIVE)
        (asserts! (<= current-block (get end-block proposal)) ERR-VOTING-ENDED)
        (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
        (asserts! (and (>= vote-type u1) (<= vote-type u3)) ERR-NOT-AUTHORIZED)
        (map-set votes { proposal-id: proposal-id, voter: tx-sender } {
            vote-type: vote-type,
            voting-power: voter-power,
            block-height: current-block
        })
        (let ((updated-proposal 
               (if (is-eq vote-type u1)
                   (merge proposal { yes-votes: (+ (get yes-votes proposal) voter-power) })
                   (if (is-eq vote-type u2)
                       (merge proposal { no-votes: (+ (get no-votes proposal) voter-power) })
                       (merge proposal { abstain-votes: (+ (get abstain-votes proposal) voter-power) })))))
            (map-set proposals proposal-id updated-proposal)
            (ok true))))

(define-public (finalize-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (current-block stacks-block-height))
        (asserts! (is-eq (get status proposal) PROPOSAL-STATUS-ACTIVE) ERR-PROPOSAL-ALREADY-FINALIZED)
        (asserts! (> current-block (get end-block proposal)) ERR-VOTING-ENDED)
        (let ((total-votes (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal)))
              (yes-percentage (if (> total-votes u0) (* (/ (get yes-votes proposal) total-votes) u100) u0))
              (new-status (if (and (>= yes-percentage u51) (>= total-votes u3))
                             PROPOSAL-STATUS-EXECUTED
                             PROPOSAL-STATUS-REJECTED)))
            (map-set proposals proposal-id (merge proposal { status: new-status }))
            (if (is-eq new-status PROPOSAL-STATUS-EXECUTED)
                (begin
                    (try! (refund-proposal-deposit proposal-id))
                    (ok { status: "executed", yes-percentage: yes-percentage }))
                (ok { status: "rejected", yes-percentage: yes-percentage })))))

(define-public (join-meeting (meeting-id uint))
    (let ((meeting (unwrap! (map-get? meetings meeting-id) ERR-MEETING-NOT-FOUND))
          (current-block stacks-block-height)
          (voter-power (default-to u1 (map-get? user-voting-power tx-sender))))
        (asserts! (>= current-block (get scheduled-block meeting)) ERR-INVALID-MEETING-STATUS)
        (asserts! (<= current-block (+ (get scheduled-block meeting) (get duration-blocks meeting))) ERR-INVALID-MEETING-STATUS)
        (asserts! (is-none (map-get? meeting-attendees { meeting-id: meeting-id, attendee: tx-sender })) ERR-ALREADY-VOTED)
        (map-set meeting-attendees { meeting-id: meeting-id, attendee: tx-sender } {
            joined-at: current-block,
            voting-power: voter-power
        })
        (map-set meetings meeting-id (merge meeting { 
            attendee-count: (+ (get attendee-count meeting) u1),
            status: MEETING-STATUS-ACTIVE
        }))
        (ok true)))

(define-public (set-voting-power (user principal) (power uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= power u1) (<= power u100)) ERR-NOT-AUTHORIZED)
        (map-set user-voting-power user power)
        (ok true)))

(define-public (update-voting-duration (duration uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= duration MIN-VOTING-DURATION) (<= duration MAX-VOTING-DURATION)) ERR-INVALID-DURATION)
        (var-set default-voting-duration duration)
        (ok true)))

(define-public (update-proposal-deposit (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set min-proposal-deposit amount)
        (ok true)))

(define-public (cancel-meeting (meeting-id uint))
    (let ((meeting (unwrap! (map-get? meetings meeting-id) ERR-MEETING-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get organizer meeting)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status meeting) MEETING-STATUS-SCHEDULED) ERR-INVALID-MEETING-STATUS)
        (map-set meetings meeting-id (merge meeting { status: MEETING-STATUS-CANCELLED }))
        (ok true)))

(define-private (refund-proposal-deposit (proposal-id uint))
    (match (map-get? proposal-deposits { proposal-id: proposal-id })
        deposit (if (not (get refunded deposit))
                   (begin
                       (try! (as-contract (stx-transfer? (get amount deposit) tx-sender (get depositor deposit))))
                       (map-set proposal-deposits { proposal-id: proposal-id } (merge deposit { refunded: true }))
                       (ok true))
                   (ok true))
        (ok true)))

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

(define-read-only (get-meeting (meeting-id uint))
    (map-get? meetings meeting-id))

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter }))

(define-read-only (get-proposal-results (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (let ((total-votes (+ (+ (get yes-votes proposal) (get no-votes proposal)) (get abstain-votes proposal))))
                    (ok {
                        yes-votes: (get yes-votes proposal),
                        no-votes: (get no-votes proposal),
                        abstain-votes: (get abstain-votes proposal),
                        total-votes: total-votes,
                        yes-percentage: (if (> total-votes u0) (* (/ (get yes-votes proposal) total-votes) u100) u0),
                        status: (get status proposal)
                    }))
        ERR-PROPOSAL-NOT-FOUND))

(define-read-only (get-meeting-attendee (meeting-id uint) (attendee principal))
    (map-get? meeting-attendees { meeting-id: meeting-id, attendee: attendee }))

(define-read-only (get-user-voting-power (user principal))
    (default-to u1 (map-get? user-voting-power user)))

(define-read-only (get-contract-info)
    {
        owner: (var-get contract-owner),
        total-proposals: (var-get proposal-counter),
        total-meetings: (var-get meeting-counter),
        min-deposit: (var-get min-proposal-deposit),
        voting-duration: (var-get default-voting-duration)
    })

(define-read-only (is-proposal-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (and 
                    (is-eq (get status proposal) PROPOSAL-STATUS-ACTIVE)
                    (<= stacks-block-height (get end-block proposal)))
        false))

(define-read-only (can-user-vote (proposal-id uint) (user principal))
    (and 
        (is-proposal-active proposal-id)
        (is-none (map-get? votes { proposal-id: proposal-id, voter: user }))))

(define-read-only (get-proposal-deposit (proposal-id uint))
    (map-get? proposal-deposits { proposal-id: proposal-id }))

(define-public (contribute-to-treasury (amount uint))
    (let ((current-balance (var-get total-treasury-balance))
          (contributor-balance (default-to u0 (map-get? treasury-contributions tx-sender))))
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-treasury-balance (+ current-balance amount))
        (map-set treasury-contributions tx-sender (+ contributor-balance amount))
        (ok true)))

(define-public (create-budget (category (string-utf8 50)) (allocated-amount uint) (quarter uint) (description (string-utf8 200)))
    (let ((budget-id (+ (var-get budget-counter) u1))
          (current-block stacks-block-height)
          (treasury-balance (var-get total-treasury-balance)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> allocated-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= allocated-amount treasury-balance) ERR-INSUFFICIENT-BUDGET)
        (asserts! (and (>= quarter u1) (<= quarter u4)) ERR-INVALID-DURATION)
        (asserts! (>= (len category) u1) ERR-INVALID-CATEGORY)
        (map-set budgets budget-id {
            id: budget-id,
            category: category,
            allocated-amount: allocated-amount,
            spent-amount: u0,
            remaining-amount: allocated-amount,
            created-by: tx-sender,
            created-at: current-block,
            status: BUDGET-STATUS-ACTIVE,
            quarter: quarter,
            description: description
        })
        (var-set budget-counter budget-id)
        (ok budget-id)))

(define-public (submit-expense (budget-id uint) (amount uint) (recipient principal) (description (string-utf8 200)))
    (let ((expense-id (+ (var-get expense-counter) u1))
          (current-block stacks-block-height)
          (budget (unwrap! (map-get? budgets budget-id) ERR-BUDGET-NOT-FOUND)))
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (get status budget) BUDGET-STATUS-ACTIVE) ERR-BUDGET-NOT-FOUND)
        (asserts! (>= (get remaining-amount budget) amount) ERR-INSUFFICIENT-BUDGET)
        (asserts! (>= (len description) u1) ERR-INVALID-AMOUNT)
        (map-set expenses expense-id {
            id: expense-id,
            budget-id: budget-id,
            amount: amount,
            recipient: recipient,
            description: description,
            submitted-by: tx-sender,
            submitted-at: current-block,
            approved-by: none,
            approved-at: none,
            status: EXPENSE-STATUS-PENDING,
            category: (get category budget)
        })
        (var-set expense-counter expense-id)
        (ok expense-id)))

(define-public (approve-expense (expense-id uint))
    (let ((expense (unwrap! (map-get? expenses expense-id) ERR-EXPENSE-NOT-FOUND))
          (current-block stacks-block-height)
          (budget (unwrap! (map-get? budgets (get budget-id expense)) ERR-BUDGET-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status expense) EXPENSE-STATUS-PENDING) ERR-EXPENSE-ALREADY-APPROVED)
        (asserts! (>= (get remaining-amount budget) (get amount expense)) ERR-INSUFFICIENT-BUDGET)
        (try! (as-contract (stx-transfer? (get amount expense) tx-sender (get recipient expense))))
        (map-set expenses expense-id (merge expense {
            status: EXPENSE-STATUS-APPROVED,
            approved-by: (some tx-sender),
            approved-at: (some current-block)
        }))
        (let ((updated-budget (merge budget {
                spent-amount: (+ (get spent-amount budget) (get amount expense)),
                remaining-amount: (- (get remaining-amount budget) (get amount expense))
            })))
            (map-set budgets (get budget-id expense) updated-budget))
        (ok true)))

(define-public (reject-expense (expense-id uint))
    (let ((expense (unwrap! (map-get? expenses expense-id) ERR-EXPENSE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status expense) EXPENSE-STATUS-PENDING) ERR-EXPENSE-ALREADY-APPROVED)
        (map-set expenses expense-id (merge expense { status: EXPENSE-STATUS-REJECTED }))
        (ok true)))

(define-public (freeze-budget (budget-id uint))
    (let ((budget (unwrap! (map-get? budgets budget-id) ERR-BUDGET-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status budget) BUDGET-STATUS-ACTIVE) ERR-BUDGET-NOT-FOUND)
        (map-set budgets budget-id (merge budget { status: BUDGET-STATUS-FROZEN }))
        (ok true)))

(define-public (unfreeze-budget (budget-id uint))
    (let ((budget (unwrap! (map-get? budgets budget-id) ERR-BUDGET-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status budget) BUDGET-STATUS-FROZEN) ERR-BUDGET-NOT-FOUND)
        (map-set budgets budget-id (merge budget { status: BUDGET-STATUS-ACTIVE }))
        (ok true)))

(define-public (transfer-budget-funds (from-budget-id uint) (to-budget-id uint) (amount uint))
    (let ((from-budget (unwrap! (map-get? budgets from-budget-id) ERR-BUDGET-NOT-FOUND))
          (to-budget (unwrap! (map-get? budgets to-budget-id) ERR-BUDGET-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (get status from-budget) BUDGET-STATUS-ACTIVE) ERR-BUDGET-NOT-FOUND)
        (asserts! (is-eq (get status to-budget) BUDGET-STATUS-ACTIVE) ERR-BUDGET-NOT-FOUND)
        (asserts! (>= (get remaining-amount from-budget) amount) ERR-INSUFFICIENT-BUDGET)
        (map-set budgets from-budget-id (merge from-budget {
            remaining-amount: (- (get remaining-amount from-budget) amount),
            allocated-amount: (- (get allocated-amount from-budget) amount)
        }))
        (map-set budgets to-budget-id (merge to-budget {
            remaining-amount: (+ (get remaining-amount to-budget) amount),
            allocated-amount: (+ (get allocated-amount to-budget) amount)
        }))
        (ok true)))

(define-read-only (get-budget (budget-id uint))
    (map-get? budgets budget-id))

(define-read-only (get-expense (expense-id uint))
    (map-get? expenses expense-id))

(define-read-only (get-treasury-balance)
    (var-get total-treasury-balance))

(define-read-only (get-user-contribution (user principal))
    (default-to u0 (map-get? treasury-contributions user)))

(define-read-only (get-budget-summary (budget-id uint))
    (match (map-get? budgets budget-id)
        budget (ok {
            id: (get id budget),
            category: (get category budget),
            allocated-amount: (get allocated-amount budget),
            spent-amount: (get spent-amount budget),
            remaining-amount: (get remaining-amount budget),
            utilization-percentage: (if (> (get allocated-amount budget) u0) 
                                      (* (/ (get spent-amount budget) (get allocated-amount budget)) u100) 
                                      u0),
            status: (get status budget),
            quarter: (get quarter budget)
        })
        ERR-BUDGET-NOT-FOUND))

(define-read-only (get-budget-utilization (budget-id uint))
    (match (map-get? budgets budget-id)
        budget (let ((utilization-rate (if (> (get allocated-amount budget) u0) 
                                         (* (/ (get spent-amount budget) (get allocated-amount budget)) u100) 
                                         u0)))
                  (ok {
                      spent: (get spent-amount budget),
                      remaining: (get remaining-amount budget),
                      total: (get allocated-amount budget),
                      utilization-percentage: utilization-rate,
                      is-over-budget: (> (get spent-amount budget) (get allocated-amount budget))
                  }))
        ERR-BUDGET-NOT-FOUND))

(define-read-only (get-quarterly-budget-total (quarter uint))
    (ok {
        quarter: quarter,
        total-allocated: (fold calculate-quarterly-total (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0),
        total-spent: (fold calculate-quarterly-spent (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
    }))

(define-read-only (get-expense-by-category (category (string-utf8 50)))
    (ok { 
        category: category,
        total-submitted: (fold calculate-category-total (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0),
        approved-count: (fold calculate-category-approved (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
    }))

(define-read-only (get-treasury-stats)
    (ok {
        total-balance: (var-get total-treasury-balance),
        total-budgets: (var-get budget-counter),
        total-expenses: (var-get expense-counter),
        total-allocated: (fold calculate-total-allocated (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0),
        total-spent: (fold calculate-total-spent (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
    }))

(define-private (calculate-quarterly-total (budget-id uint) (acc uint))
    (match (map-get? budgets budget-id)
        budget (+ acc (get allocated-amount budget))
        acc))

(define-private (calculate-quarterly-spent (budget-id uint) (acc uint))
    (match (map-get? budgets budget-id)
        budget (+ acc (get spent-amount budget))
        acc))

(define-private (calculate-category-total (expense-id uint) (acc uint))
    (match (map-get? expenses expense-id)
        expense (+ acc (get amount expense))
        acc))

(define-private (calculate-category-approved (expense-id uint) (acc uint))
    (match (map-get? expenses expense-id)
        expense (if (is-eq (get status expense) EXPENSE-STATUS-APPROVED) (+ acc u1) acc)
        acc))

(define-private (calculate-total-allocated (budget-id uint) (acc uint))
    (match (map-get? budgets budget-id)
        budget (+ acc (get allocated-amount budget))
        acc))

(define-private (calculate-total-spent (budget-id uint) (acc uint))
    (match (map-get? budgets budget-id)
        budget (+ acc (get spent-amount budget))
        acc))
