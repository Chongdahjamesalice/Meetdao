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

(define-constant PROPOSAL-STATUS-ACTIVE u1)
(define-constant PROPOSAL-STATUS-EXECUTED u2)
(define-constant PROPOSAL-STATUS-REJECTED u3)
(define-constant PROPOSAL-STATUS-EXPIRED u4)

(define-constant MEETING-STATUS-SCHEDULED u1)
(define-constant MEETING-STATUS-ACTIVE u2)
(define-constant MEETING-STATUS-COMPLETED u3)
(define-constant MEETING-STATUS-CANCELLED u4)

(define-constant MIN-VOTING-DURATION u144)
(define-constant MAX-VOTING-DURATION u4320)
(define-constant MIN-QUORUM-PERCENTAGE u10)

(define-data-var contract-owner principal tx-sender)
(define-data-var proposal-counter uint u0)
(define-data-var meeting-counter uint u0)
(define-data-var min-proposal-deposit uint u1000000)
(define-data-var default-voting-duration uint u1008)

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
