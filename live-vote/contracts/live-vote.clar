;; Infinity Voting - DAO Governance Contract

;; ============================================================
;; Constants
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED       (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND   (err u101))
(define-constant ERR-PROPOSAL-NOT-ACTIVE  (err u102))
(define-constant ERR-ALREADY-VOTED        (err u103))
(define-constant ERR-INVALID-AMOUNT       (err u104))
(define-constant ERR-TOKENS-LOCKED        (err u105))
(define-constant ERR-DISPUTE-NOT-FOUND    (err u106))
(define-constant ERR-ALREADY-JUROR        (err u107))
(define-constant ERR-NO-DELEGATION        (err u108))
(define-constant ERR-SELF-DELEGATION      (err u109))

;; Voting period in blocks (roughly 1 week at ~10 min/block)
(define-constant VOTING-PERIOD u1008)

;; Minimum stake required to submit a proposal
(define-constant MIN-PROPOSAL-STAKE u1000000) ;; in microSTX

;; Reputation decay rate (applied per epoch)
(define-constant REPUTATION-DECAY-RATE u95) ;; 95% retention per epoch

;; Maximum lock duration in blocks (~1 year)
(define-constant MAX-LOCK-DURATION u52560)

;; Multiplier precision (100 = 1x, 200 = 2x, etc.)
(define-constant MULTIPLIER-BASE u100)

;; ============================================================
;; Data Variables
;; ============================================================

(define-data-var proposal-count uint u0)
(define-data-var dispute-count   uint u0)

;; ============================================================
;; Data Maps
;; ============================================================

;; Staked token balances
(define-map staked-balances
  { staker: principal }
  { amount: uint })

;; Voting escrow: lock tokens for a duration to earn multiplier
(define-map voting-escrow
  { staker: principal }
  {
    locked-amount:   uint,
    lock-start:      uint,  ;; block height when lock started
    lock-duration:   uint,  ;; total lock duration in blocks
    unlock-at:       uint   ;; absolute block height for unlock
  })

;; Reputation scores (scaled by u1000 for fixed-point math)
(define-map reputation-scores
  { user: principal }
  { score: uint })

;; Delegation map: voter -> delegate
(define-map delegations
  { delegator: principal }
  {
    delegate:         principal,
    proposal-override: (optional uint) ;; override for a specific proposal
  })

;; Proposals
(define-map proposals
  { proposal-id: uint }
  {
    proposer:     principal,
    title:        (string-ascii 128),
    description:  (string-ascii 512),
    start-block:  uint,
    end-block:    uint,
    yes-votes:    uint,
    no-votes:     uint,
    status:       (string-ascii 16), ;; "active", "passed", "failed", "disputed"
    executed:     bool
  })

;; Vote records (prevents double voting)
(define-map vote-records
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint })

;; Disputes on proposals
(define-map disputes
  { dispute-id: uint }
  {
    proposal-id: uint,
    challenger:  principal,
    reason:      (string-ascii 256),
    resolved:    bool,
    outcome:     bool  ;; true = dispute upheld
  })

;; Jury membership per dispute
(define-map jury-members
  { dispute-id: uint, juror: principal }
  { voted: bool, vote: bool })

;; ============================================================
;; Private Helpers
;; ============================================================

;; Integer square root (Babylonian method, 5 iterations)
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (let
      (
        (x0 n)
        (x1 (/ (+ n u1) u2))
        (x2 (/ (+ (/ n x1) x1) u2))
        (x3 (/ (+ (/ n x2) x2) u2))
        (x4 (/ (+ (/ n x3) x3) u2))
        (x5 (/ (+ (/ n x4) x4) u2))
      )
      x5
    )
  )
)

;; Compute voting multiplier based on lock duration
;; Longer locks yield higher multipliers up to 4x
;; multiplier = 100 + (lock-duration / MAX-LOCK-DURATION) * 300
(define-private (get-lock-multiplier (lock-duration uint))
  (+ MULTIPLIER-BASE
     (/ (* lock-duration u300) MAX-LOCK-DURATION))
)

;; Get effective voting power for a principal
;; Combines: sqrt(staked) * lock-multiplier * reputation
(define-private (get-voting-power (voter principal))
  (let
    (
      (staked (default-to u0
                (get amount (map-get? staked-balances { staker: voter }))))
      (rep    (default-to u1000
                (get score (map-get? reputation-scores { user: voter }))))
      (escrow (map-get? voting-escrow { staker: voter }))
    )
    (let
      (
        (lock-mult
          (if (is-none escrow)
            MULTIPLIER-BASE
            (get-lock-multiplier
              (get lock-duration (unwrap-panic escrow)))))
        ;; Quadratic voting: power = sqrt(staked)
        (base-power (isqrt staked))
      )
      ;; Final power = base * lock-mult * reputation / (100 * 1000)
      (/ (* (* base-power lock-mult) rep) (* u100 u1000))
    )
  )
)

;; Resolve effective voter, accounting for delegation
(define-private (resolve-voter (original-voter principal) (proposal-id uint))
  (let
    (
      (delegation (map-get? delegations { delegator: original-voter }))
    )
    (if (is-none delegation)
      original-voter
      (let
        (
          (d (unwrap-panic delegation))
          (override (get proposal-override d))
        )
        ;; Check if delegator has an override for this specific proposal
        (if (and (is-some override)
                 (is-eq (unwrap-panic override) proposal-id))
          original-voter
          (get delegate d)
        )
      )
    )
  )
)

;; Update reputation after a vote (reward participation)
(define-private (reward-participation (user principal))
  (let
    (
      (current (default-to u1000
                  (get score (map-get? reputation-scores { user: user }))))
      ;; Add 10 reputation points (scaled by 1000)
      (updated (+ current u10000))
    )
    (map-set reputation-scores
      { user: user }
      { score: updated })
  )
)

;; Apply reputation decay (call periodically via epoch management)
(define-private (decay-reputation (user principal))
  (let
    (
      (current (default-to u1000
                  (get score (map-get? reputation-scores { user: user }))))
      (decayed (/ (* current REPUTATION-DECAY-RATE) u100))
    )
    (map-set reputation-scores
      { user: user }
      { score: decayed })
  )
)

;; ============================================================
;; Public Functions
;; ============================================================

;; --- Staking ---

;; Stake STX to participate in governance
(define-public (stake (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let
      (
        (current (default-to u0
                    (get amount (map-get? staked-balances { staker: tx-sender }))))
      )
      (map-set staked-balances
        { staker: tx-sender }
        { amount: (+ current amount) })
      (ok amount)
    )
  )
)

;; Unstake STX (only if not locked in escrow)
(define-public (unstake (amount uint))
  (let
    (
      (current (default-to u0
                  (get amount (map-get? staked-balances { staker: tx-sender }))))
      (escrow  (map-get? voting-escrow { staker: tx-sender }))
    )
    (asserts! (>= current amount) ERR-INVALID-AMOUNT)
    ;; Ensure tokens are not locked
    (asserts!
      (or (is-none escrow)
          (>= block-height (get unlock-at (unwrap-panic escrow))))
      ERR-TOKENS-LOCKED)
    (map-set staked-balances
      { staker: tx-sender }
      { amount: (- current amount) })
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (ok amount)
  )
)

;; --- Voting Escrow ---

;; Lock staked tokens for a duration to earn a voting multiplier
(define-public (lock-tokens (lock-duration uint))
  (let
    (
      (staked (default-to u0
                (get amount (map-get? staked-balances { staker: tx-sender }))))
    )
    (asserts! (> staked u0) ERR-INVALID-AMOUNT)
    (asserts! (<= lock-duration MAX-LOCK-DURATION) ERR-INVALID-AMOUNT)
    (map-set voting-escrow
      { staker: tx-sender }
      {
        locked-amount: staked,
        lock-start:    block-height,
        lock-duration: lock-duration,
        unlock-at:     (+ block-height lock-duration)
      })
    (ok true)
  )
)

;; --- Delegation ---

;; Delegate voting power to another principal
(define-public (delegate (to principal))
  (begin
    (asserts! (not (is-eq to tx-sender)) ERR-SELF-DELEGATION)
    (map-set delegations
      { delegator: tx-sender }
      { delegate: to, proposal-override: none })
    (ok true)
  )
)

;; Override delegation for a specific proposal (reclaim vote)
(define-public (override-delegation (proposal-id uint))
  (begin
    (asserts!
      (is-some (map-get? delegations { delegator: tx-sender }))
      ERR-NO-DELEGATION)
    (let
      (
        (d (unwrap-panic (map-get? delegations { delegator: tx-sender })))
      )
      (map-set delegations
        { delegator: tx-sender }
        (merge d { proposal-override: (some proposal-id) }))
      (ok true)
    )
  )
)

;; Revoke all delegation
(define-public (revoke-delegation)
  (begin
    (map-delete delegations { delegator: tx-sender })
    (ok true)
  )
)

;; --- Proposals ---

;; Submit a new governance proposal
(define-public (submit-proposal
    (title       (string-ascii 128))
    (description (string-ascii 512)))
  (let
    (
      (staked (default-to u0
                (get amount (map-get? staked-balances { staker: tx-sender }))))
      (id     (+ (var-get proposal-count) u1))
    )
    (asserts! (>= staked MIN-PROPOSAL-STAKE) ERR-INVALID-AMOUNT)
    (map-set proposals
      { proposal-id: id }
      {
        proposer:    tx-sender,
        title:       title,
        description: description,
        start-block: block-height,
        end-block:   (+ block-height VOTING-PERIOD),
        yes-votes:   u0,
        no-votes:    u0,
        status:      "active",
        executed:    false
      })
    (var-set proposal-count id)
    (ok id)
  )
)

;; Cast a vote on a proposal
;; Resolves delegation automatically
(define-public (vote (proposal-id uint) (vote-yes bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id })
                         ERR-PROPOSAL-NOT-FOUND))
      (effective-voter (resolve-voter tx-sender proposal-id))
    )
    ;; Proposal must be active
    (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-NOT-ACTIVE)
    (asserts! (< block-height (get end-block proposal))    ERR-PROPOSAL-NOT-ACTIVE)
    ;; Prevent double voting (checked on effective voter)
    (asserts!
      (is-none (map-get? vote-records
                  { proposal-id: proposal-id, voter: effective-voter }))
      ERR-ALREADY-VOTED)
    (let
      (
        (power (get-voting-power effective-voter))
      )
      ;; Record vote
      (map-set vote-records
        { proposal-id: proposal-id, voter: effective-voter }
        { vote: vote-yes, weight: power })
      ;; Update proposal tallies
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal
          {
            yes-votes: (if vote-yes
                          (+ (get yes-votes proposal) power)
                          (get yes-votes proposal)),
            no-votes:  (if vote-yes
                          (get no-votes proposal)
                          (+ (get no-votes proposal) power))
          }))
      ;; Reward participation reputation
      (reward-participation effective-voter)
      (ok power)
    )
  )
)

;; Finalize a proposal after its voting period ends
(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id })
                         ERR-PROPOSAL-NOT-FOUND))
    )
    (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-NOT-ACTIVE)
    (asserts! (>= block-height (get end-block proposal))  ERR-PROPOSAL-NOT-ACTIVE)
    (let
      (
        (passed (> (get yes-votes proposal) (get no-votes proposal)))
        (new-status (if passed "passed" "failed"))
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { status: new-status }))
      (ok passed)
    )
  )
)

;; --- Dispute Resolution ---

;; Raise a dispute on a passed/failed proposal
(define-public (raise-dispute
    (proposal-id uint)
    (reason      (string-ascii 256)))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id })
                         ERR-PROPOSAL-NOT-FOUND))
      (id       (+ (var-get dispute-count) u1))
    )
    ;; Only finalized proposals can be disputed
    (asserts!
      (or (is-eq (get status proposal) "passed")
          (is-eq (get status proposal) "failed"))
      ERR-PROPOSAL-NOT-ACTIVE)
    (map-set disputes
      { dispute-id: id }
      {
        proposal-id: proposal-id,
        challenger:  tx-sender,
        reason:      reason,
        resolved:    false,
        outcome:     false
      })
    ;; Mark proposal as disputed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { status: "disputed" }))
    (var-set dispute-count id)
    (ok id)
  )
)

;; Join a jury pool for a dispute
(define-public (join-jury (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id })
                        ERR-DISPUTE-NOT-FOUND))
    )
    (asserts! (not (get resolved dispute)) ERR-DISPUTE-NOT-FOUND)
    (asserts!
      (is-none (map-get? jury-members
                  { dispute-id: dispute-id, juror: tx-sender }))
      ERR-ALREADY-JUROR)
    (map-set jury-members
      { dispute-id: dispute-id, juror: tx-sender }
      { voted: false, vote: false })
    (ok true)
  )
)

;; Cast a jury vote on a dispute
(define-public (jury-vote (dispute-id uint) (uphold bool))
  (let
    (
      (juror-record (unwrap!
                      (map-get? jury-members
                        { dispute-id: dispute-id, juror: tx-sender })
                      ERR-NOT-AUTHORIZED))
    )
    (asserts! (not (get voted juror-record)) ERR-ALREADY-VOTED)
    (map-set jury-members
      { dispute-id: dispute-id, juror: tx-sender }
      { voted: true, vote: uphold })
    (ok true)
  )
)

;; Resolve a dispute (owner acts as final arbitrator in this version)
(define-public (resolve-dispute (dispute-id uint) (upheld bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let
      (
        (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id })
                          ERR-DISPUTE-NOT-FOUND))
        (proposal (unwrap! (map-get? proposals
                              { proposal-id: (get proposal-id dispute) })
                           ERR-PROPOSAL-NOT-FOUND))
      )
      (asserts! (not (get resolved dispute)) ERR-DISPUTE-NOT-FOUND)
      ;; Mark dispute resolved
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute { resolved: true, outcome: upheld }))
      ;; Restore proposal status based on outcome
      (map-set proposals
        { proposal-id: (get proposal-id dispute) }
        (merge proposal
          { status: (if upheld "failed" "passed") }))
      (ok upheld)
    )
  )
)

;; ============================================================
;; Read-Only Functions
;; ============================================================

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id }))

(define-read-only (get-vote-record (proposal-id uint) (voter principal))
  (map-get? vote-records { proposal-id: proposal-id, voter: voter }))

(define-read-only (get-staked-balance (staker principal))
  (default-to u0 (get amount (map-get? staked-balances { staker: staker }))))

(define-read-only (get-escrow-info (staker principal))
  (map-get? voting-escrow { staker: staker }))

(define-read-only (get-reputation (user principal))
  (default-to u1000 (get score (map-get? reputation-scores { user: user }))))

(define-read-only (get-delegation (delegator principal))
  (map-get? delegations { delegator: delegator }))

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id }))

(define-read-only (get-voting-power-for (voter principal))
  (get-voting-power voter))

(define-read-only (get-proposal-count)
  (var-get proposal-count))

(define-read-only (get-dispute-count)
  (var-get dispute-count))
