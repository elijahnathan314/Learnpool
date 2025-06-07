(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-proposal-ended (err u106))
(define-constant err-proposal-active (err u107))
(define-constant err-already-voted (err u108))

(define-data-var next-proposal-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var min-proposal-amount uint u1000000)
(define-data-var voting-period uint u1440)

(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    status: (string-ascii 20),
    funded-amount: uint
  }
)

(define-map member-stakes
  { member: principal }
  { stake-amount: uint, join-block: uint }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

(define-map member-contributions
  { member: principal, proposal-id: uint }
  { amount: uint }
)

(define-public (join-dao (stake-amount uint))
  (let ((existing-stake (map-get? member-stakes { member: tx-sender })))
    (asserts! (> stake-amount u0) err-invalid-amount)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (var-set treasury-balance (+ (var-get treasury-balance) stake-amount))
    (map-set member-stakes
      { member: tx-sender }
      { 
        stake-amount: (+ (default-to u0 (get stake-amount existing-stake)) stake-amount),
        join-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint))
  (let ((proposal-id (var-get next-proposal-id))
        (member-stake (get-member-stake tx-sender)))
    (asserts! (> member-stake u0) err-unauthorized)
    (asserts! (>= funding-goal (var-get min-proposal-amount)) err-invalid-amount)
    (asserts! (<= funding-goal (var-get treasury-balance)) err-insufficient-funds)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        votes-for: u0,
        votes-against: u0,
        created-at: stacks-block-height,
        status: "active",
        funded-amount: u0
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
        (member-stake (get-member-stake tx-sender))
        (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })))
    (asserts! (> member-stake u0) err-unauthorized)
    (asserts! (is-eq (get status proposal) "active") err-proposal-ended)
    (asserts! (< stacks-block-height (+ (get created-at proposal) (var-get voting-period))) err-proposal-ended)
    (asserts! (is-none existing-vote) err-already-voted)
    (let ((voting-power (calculate-voting-power member-stake))
          (updated-proposal (if vote-for
            (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) })
            (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) }))))
      (map-set proposals { proposal-id: proposal-id } updated-proposal)
      (map-set proposal-votes
        { proposal-id: proposal-id, voter: tx-sender }
        { vote: vote-for, voting-power: voting-power }
      )
      (ok true)
    )
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found)))
    (asserts! (is-eq (get status proposal) "active") err-proposal-ended)
    (asserts! (>= stacks-block-height (+ (get created-at proposal) (var-get voting-period))) err-proposal-active)
    (let ((total-votes (+ (get votes-for proposal) (get votes-against proposal)))
          (approval-threshold (/ total-votes u2)))
      (if (> (get votes-for proposal) approval-threshold)
        (begin
          (try! (fund-proposal proposal-id))
          (map-set proposals 
            { proposal-id: proposal-id }
            (merge proposal { status: "approved", funded-amount: (get funding-goal proposal) })
          )
          (ok "approved")
        )
        (begin
          (map-set proposals 
            { proposal-id: proposal-id }
            (merge proposal { status: "rejected" })
          )
          (ok "rejected")
        )
      )
    )
  )
)

(define-public (contribute-to-proposal (proposal-id uint) (amount uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found)))
    (asserts! (is-eq (get status proposal) "approved") err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set member-contributions
      { member: tx-sender, proposal-id: proposal-id }
      { amount: amount }
    )
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-public (withdraw-funds (proposal-id uint) (amount uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get creator proposal)) err-unauthorized)
    (asserts! (is-eq (get status proposal) "approved") err-unauthorized)
    (asserts! (<= amount (get funded-amount proposal)) err-insufficient-funds)
    (asserts! (<= amount (var-get treasury-balance)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? amount tx-sender (get creator proposal))))
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-public (leave-dao)
  (let ((member-stake (get-member-stake tx-sender)))
    (asserts! (> member-stake u0) err-not-found)
    (asserts! (<= member-stake (var-get treasury-balance)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? member-stake tx-sender tx-sender)))
    (map-delete member-stakes { member: tx-sender })
    (var-set treasury-balance (- (var-get treasury-balance) member-stake))
    (ok member-stake)
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-stake (member principal))
  (default-to u0 (get stake-amount (map-get? member-stakes { member: member })))
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-voting-power (member principal))
  (let ((stake (get-member-stake member)))
    (calculate-voting-power stake)
  )
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-member-contribution (member principal) (proposal-id uint))
  (default-to u0 (get amount (map-get? member-contributions { member: member, proposal-id: proposal-id })))
)

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-min-proposal-amount)
  (var-get min-proposal-amount)
)

(define-private (calculate-voting-power (stake-amount uint))
  (if (> stake-amount u10000000)
    (+ u100 (/ stake-amount u100000))
    (+ u10 (/ stake-amount u10000))
  )
)

(define-private (fund-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found))
        (funding-amount (get funding-goal proposal)))
    (asserts! (<= funding-amount (var-get treasury-balance)) err-insufficient-funds)
    (var-set treasury-balance (- (var-get treasury-balance) funding-amount))
    (ok true)
  )
)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (update-min-proposal-amount (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-proposal-amount new-amount)
    (ok true)
  )
)