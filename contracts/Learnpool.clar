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
(define-constant err-milestone-not-found (err u109))
(define-constant err-milestone-already-claimed (err u110))
(define-constant err-insufficient-reputation (err u111))

(define-data-var next-proposal-id uint u1)
(define-data-var next-milestone-id uint u1)
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

(define-map member-reputation
  { member: principal }
  {
    total-reputation: uint,
    proposals-created: uint,
    successful-proposals: uint,
    votes-cast: uint,
    contributions-made: uint,
    milestones-achieved: uint,
    last-activity-block: uint,
    reputation-level: uint
  }
)

(define-map reputation-milestones
  { milestone-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    reputation-requirement: uint,
    reputation-reward: uint,
    category: (string-ascii 20),
    is-active: bool
  }
)

(define-map member-milestone-claims
  { member: principal, milestone-id: uint }
  { claimed-at: uint, reputation-earned: uint }
)

(define-map reputation-leaderboard
  { rank: uint }
  { member: principal, reputation-score: uint }
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
    (initialize-member-reputation tx-sender)
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
    (try! (update-member-reputation tx-sender "proposal-created" u0))
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
      (try! (update-member-reputation tx-sender "vote-cast" u0))
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
          (try! (update-member-reputation (get creator proposal) "proposal-approved" u0))
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
    (try! (update-member-reputation tx-sender "contribution-made" amount))
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

(define-read-only (get-member-reputation (member principal))
  (map-get? member-reputation { member: member })
)

(define-read-only (get-reputation-milestone (milestone-id uint))
  (map-get? reputation-milestones { milestone-id: milestone-id })
)

(define-read-only (get-member-milestone-claim (member principal) (milestone-id uint))
  (map-get? member-milestone-claims { member: member, milestone-id: milestone-id })
)

(define-read-only (get-leaderboard-position (rank uint))
  (map-get? reputation-leaderboard { rank: rank })
)

(define-read-only (get-member-reputation-level (member principal))
  (let ((rep-data (map-get? member-reputation { member: member })))
    (match rep-data
      member-rep (get reputation-level member-rep)
      u0
    )
  )
)

(define-read-only (get-enhanced-voting-power (member principal))
  (let ((base-power (get-voting-power member))
        (reputation-bonus (calculate-reputation-bonus member)))
    (+ base-power reputation-bonus)
  )
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

(define-private (calculate-reputation-bonus (member principal))
  (let ((rep-data (map-get? member-reputation { member: member })))
    (match rep-data
      member-rep (/ (get total-reputation member-rep) u100)
      u0
    )
  )
)

(define-private (initialize-member-reputation (member principal))
  (let ((existing-rep (map-get? member-reputation { member: member })))
    (if (is-none existing-rep)
      (map-set member-reputation
        { member: member }
        {
          total-reputation: u10,
          proposals-created: u0,
          successful-proposals: u0,
          votes-cast: u0,
          contributions-made: u0,
          milestones-achieved: u0,
          last-activity-block: stacks-block-height,
          reputation-level: u1
        }
      )
      true
    )
  )
)

(define-private (update-member-reputation (member principal) (activity-type (string-ascii 20)) (amount uint))
  (let ((rep-data (unwrap! (map-get? member-reputation { member: member }) (err u404))))
    (let ((new-reputation (calculate-reputation-gain activity-type amount))
          (updated-rep (merge rep-data {
            total-reputation: (+ (get total-reputation rep-data) new-reputation),
            proposals-created: (if (is-eq activity-type "proposal-created")
              (+ (get proposals-created rep-data) u1)
              (get proposals-created rep-data)
            ),
            successful-proposals: (if (is-eq activity-type "proposal-approved")
              (+ (get successful-proposals rep-data) u1)
              (get successful-proposals rep-data)
            ),
            votes-cast: (if (is-eq activity-type "vote-cast")
              (+ (get votes-cast rep-data) u1)
              (get votes-cast rep-data)
            ),
            contributions-made: (if (is-eq activity-type "contribution-made")
              (+ (get contributions-made rep-data) u1)
              (get contributions-made rep-data)
            ),
            last-activity-block: stacks-block-height,
            reputation-level: (calculate-reputation-level (+ (get total-reputation rep-data) new-reputation))
          })))
      (map-set member-reputation { member: member } updated-rep)
      (update-leaderboard member (get total-reputation updated-rep))
      (ok true)
    )
  )
)

(define-private (calculate-reputation-gain (activity-type (string-ascii 20)) (amount uint))
  (if (is-eq activity-type "proposal-created")
    u20
    (if (is-eq activity-type "proposal-approved")
      u50
      (if (is-eq activity-type "vote-cast")
        u5
        (if (is-eq activity-type "contribution-made")
          (/ amount u100000)
          u0
        )
      )
    )
  )
)

(define-private (calculate-reputation-level (total-reputation uint))
  (if (>= total-reputation u1000)
    u5
    (if (>= total-reputation u500)
      u4
      (if (>= total-reputation u200)
        u3
        (if (>= total-reputation u50)
          u2
          u1
        )
      )
    )
  )
)

(define-private (update-leaderboard (member principal) (reputation-score uint))
  (let ((current-rank (find-member-rank member)))
    (if (is-some current-rank)
      (begin
        (unwrap-panic (update-existing-rank member reputation-score (unwrap-panic current-rank)))
        true
      )
      (unwrap-panic (add-to-leaderboard member reputation-score))
    )
  )
)

(define-private (find-member-rank (member principal))
  (let ((rank-1 (map-get? reputation-leaderboard { rank: u1 }))
        (rank-2 (map-get? reputation-leaderboard { rank: u2 }))
        (rank-3 (map-get? reputation-leaderboard { rank: u3 }))
        (rank-4 (map-get? reputation-leaderboard { rank: u4 }))
        (rank-5 (map-get? reputation-leaderboard { rank: u5 })))
    (if (and (is-some rank-1) (is-eq member (get member (unwrap-panic rank-1))))
      (some u1)
      (if (and (is-some rank-2) (is-eq member (get member (unwrap-panic rank-2))))
        (some u2)
        (if (and (is-some rank-3) (is-eq member (get member (unwrap-panic rank-3))))
          (some u3)
          (if (and (is-some rank-4) (is-eq member (get member (unwrap-panic rank-4))))
            (some u4)
            (if (and (is-some rank-5) (is-eq member (get member (unwrap-panic rank-5))))
              (some u5)
              none
            )
          )
        )
      )
    )
  )
)

(define-private (update-existing-rank (member principal) (reputation-score uint) (current-rank uint))
  (begin
    (map-set reputation-leaderboard 
      { rank: current-rank }
      { member: member, reputation-score: reputation-score }
    )
    (ok true)
  )
)

(define-private (add-to-leaderboard (member principal) (reputation-score uint))
  (let ((rank-5 (map-get? reputation-leaderboard { rank: u5 })))
    (if (or (is-none rank-5) (> reputation-score (get reputation-score (unwrap-panic rank-5))))
      (begin
        (shift-leaderboard-down reputation-score)
        (map-set reputation-leaderboard 
          { rank: u1 }
          { member: member, reputation-score: reputation-score }
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-private (shift-leaderboard-down (new-score uint))
  (let ((rank-1 (map-get? reputation-leaderboard { rank: u1 }))
        (rank-2 (map-get? reputation-leaderboard { rank: u2 }))
        (rank-3 (map-get? reputation-leaderboard { rank: u3 }))
        (rank-4 (map-get? reputation-leaderboard { rank: u4 })))
    (begin
      (if (is-some rank-4)
        (map-set reputation-leaderboard { rank: u5 } (unwrap-panic rank-4))
        true
      )
      (if (is-some rank-3)
        (map-set reputation-leaderboard { rank: u4 } (unwrap-panic rank-3))
        true
      )
      (if (is-some rank-2)
        (map-set reputation-leaderboard { rank: u3 } (unwrap-panic rank-2))
        true
      )
      (if (is-some rank-1)
        (map-set reputation-leaderboard { rank: u2 } (unwrap-panic rank-1))
        true
      )
      true
    )
  )
)

(define-public (create-milestone (name (string-ascii 50)) (description (string-ascii 200)) (reputation-requirement uint) (reputation-reward uint) (category (string-ascii 20)))
  (let ((milestone-id (var-get next-milestone-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> reputation-requirement u0) err-invalid-amount)
    (asserts! (> reputation-reward u0) err-invalid-amount)
    (map-set reputation-milestones
      { milestone-id: milestone-id }
      {
        name: name,
        description: description,
        reputation-requirement: reputation-requirement,
        reputation-reward: reputation-reward,
        category: category,
        is-active: true
      }
    )
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (claim-milestone (milestone-id uint))
  (let ((milestone (unwrap! (map-get? reputation-milestones { milestone-id: milestone-id }) err-milestone-not-found))
        (member-rep (unwrap! (map-get? member-reputation { member: tx-sender }) err-not-found))
        (existing-claim (map-get? member-milestone-claims { member: tx-sender, milestone-id: milestone-id })))
    (asserts! (get is-active milestone) err-milestone-not-found)
    (asserts! (is-none existing-claim) err-milestone-already-claimed)
    (asserts! (>= (get total-reputation member-rep) (get reputation-requirement milestone)) err-insufficient-reputation)
    (let ((reputation-reward (get reputation-reward milestone))
          (updated-rep (merge member-rep {
            total-reputation: (+ (get total-reputation member-rep) reputation-reward),
            milestones-achieved: (+ (get milestones-achieved member-rep) u1),
            last-activity-block: stacks-block-height
          })))
      (map-set member-reputation { member: tx-sender } updated-rep)
      (map-set member-milestone-claims 
        { member: tx-sender, milestone-id: milestone-id }
        { claimed-at: stacks-block-height, reputation-earned: reputation-reward }
      )
      (update-leaderboard tx-sender (get total-reputation updated-rep))
      (ok true)
    )
  )
)

(define-public (deactivate-milestone (milestone-id uint))
  (let ((milestone (unwrap! (map-get? reputation-milestones { milestone-id: milestone-id }) err-milestone-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set reputation-milestones
      { milestone-id: milestone-id }
      (merge milestone { is-active: false })
    )
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