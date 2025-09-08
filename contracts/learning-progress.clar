;; Learning Progress Tracker
;; Individual skill progression, goals, and achievement tracking for Learnpool members

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-SKILLS u50)
(define-constant MAX-GOALS u20)
(define-constant SKILL-LEVEL-CAP u100)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED u600)
(define-constant ERR-SKILL-NOT-FOUND u601)
(define-constant ERR-GOAL-NOT-FOUND u602)
(define-constant ERR-INVALID-LEVEL u603)
(define-constant ERR-SKILL-EXISTS u604)
(define-constant ERR-GOAL-COMPLETED u605)
(define-constant ERR-MAX-SKILLS-REACHED u606)
(define-constant ERR-MAX-GOALS-REACHED u607)
(define-constant ERR-INVALID-TARGET u608)

;; Data variables
(define-data-var next-skill-id uint u1)
(define-data-var next-goal-id uint u1)
(define-data-var total-learners uint u0)

;; Member skill tracking
(define-map member-skills {member: principal, skill-name: (string-ascii 50)}
    {
        skill-level: uint,
        experience-points: uint,
        last-updated: uint,
        time-spent-learning: uint,
        mastery-achievements: uint,
        skill-category: (string-ascii 30)
    }
)

;; Learning goals and targets
(define-map learning-goals {member: principal, goal-id: uint}
    {
        goal-title: (string-ascii 100),
        target-skill: (string-ascii 50),
        target-level: uint,
        target-date: uint,
        created-at: uint,
        progress-percentage: uint,
        is-completed: bool,
        completion-date: (optional uint),
        reward-earned: uint
    }
)

;; Member progress profiles
(define-map member-progress-profiles principal
    {
        total-skills: uint,
        total-goals: uint,
        completed-goals: uint,
        total-experience: uint,
        learning-streak: uint,
        last-activity: uint,
        progress-level: uint,
        specialization: (string-ascii 50)
    }
)

;; Skill categories and definitions
(define-map skill-categories (string-ascii 30)
    {
        description: (string-ascii 200),
        base-experience-required: uint,
        level-multiplier: uint,
        category-bonus: uint,
        is-active: bool
    }
)

;; Learning sessions tracking
(define-map learning-sessions {member: principal, session-id: uint}
    {
        skill-focused: (string-ascii 50),
        session-duration: uint,
        experience-gained: uint,
        session-date: uint,
        session-type: (string-ascii 20),
        notes: (string-ascii 300)
    }
)

;; Achievement unlocks
(define-map skill-achievements {member: principal, achievement-type: (string-ascii 50)}
    {
        unlocked-at: uint,
        achievement-description: (string-ascii 200),
        reputation-bonus: uint,
        experience-bonus: uint
    }
)

;; Data tracking
(define-data-var next-session-id uint u1)

;; Public Functions

;; Initialize or update member progress profile
(define-public (initialize-progress-profile (specialization (string-ascii 50)))
    (let
        (
            (existing-profile (map-get? member-progress-profiles tx-sender))
        )
        (if (is-none existing-profile)
            (begin
                (map-set member-progress-profiles tx-sender
                    {
                        total-skills: u0,
                        total-goals: u0,
                        completed-goals: u0,
                        total-experience: u0,
                        learning-streak: u0,
                        last-activity: stacks-block-height,
                        progress-level: u1,
                        specialization: specialization
                    }
                )
                (var-set total-learners (+ (var-get total-learners) u1))
                (ok true)
            )
            (ok true)
        )
    )
)

;; Add or update a skill
(define-public (track-skill-progress 
    (skill-name (string-ascii 50)) 
    (experience-gained uint) 
    (time-spent uint)
    (skill-category (string-ascii 30)))
    (let
        (
            (current-skill (map-get? member-skills {member: tx-sender, skill-name: skill-name}))
            (member-profile (unwrap! (map-get? member-progress-profiles tx-sender) (err ERR-NOT-AUTHORIZED)))
        )
        (asserts! (< (get total-skills member-profile) MAX-SKILLS) (err ERR-MAX-SKILLS-REACHED))
        
        (match current-skill
            existing-skill
            ;; Update existing skill
            (let
                (
                    (new-exp (+ (get experience-points existing-skill) experience-gained))
                    (new-level (calculate-skill-level new-exp))
                    (new-time (+ (get time-spent-learning existing-skill) time-spent))
                )
                (asserts! (<= new-level SKILL-LEVEL-CAP) (err ERR-INVALID-LEVEL))
                
                (map-set member-skills {member: tx-sender, skill-name: skill-name}
                    (merge existing-skill
                        {
                            skill-level: new-level,
                            experience-points: new-exp,
                            last-updated: stacks-block-height,
                            time-spent-learning: new-time
                        }
                    )
                )
                
                ;; Update member profile
                (update-member-profile-stats tx-sender experience-gained)
                
                ;; Check for level-up achievements
                (check-skill-achievements tx-sender skill-name new-level)
                
                (ok new-level)
            )
            ;; Create new skill
            (begin
                (map-set member-skills {member: tx-sender, skill-name: skill-name}
                    {
                        skill-level: (calculate-skill-level experience-gained),
                        experience-points: experience-gained,
                        last-updated: stacks-block-height,
                        time-spent-learning: time-spent,
                        mastery-achievements: u0,
                        skill-category: skill-category
                    }
                )
                
                ;; Update member profile - increment skill count
                (map-set member-progress-profiles tx-sender
                    (merge member-profile
                        {
                            total-skills: (+ (get total-skills member-profile) u1),
                            total-experience: (+ (get total-experience member-profile) experience-gained),
                            last-activity: stacks-block-height
                        }
                    )
                )
                
                (ok (calculate-skill-level experience-gained))
            )
        )
    )
)

;; Create a learning goal
(define-public (create-learning-goal
    (goal-title (string-ascii 100))
    (target-skill (string-ascii 50))
    (target-level uint)
    (target-date uint))
    (let
        (
            (goal-id (var-get next-goal-id))
            (member-profile (unwrap! (map-get? member-progress-profiles tx-sender) (err ERR-NOT-AUTHORIZED)))
        )
        (asserts! (< (get total-goals member-profile) MAX-GOALS) (err ERR-MAX-GOALS-REACHED))
        (asserts! (and (> target-level u0) (<= target-level SKILL-LEVEL-CAP)) (err ERR-INVALID-TARGET))
        (asserts! (> target-date stacks-block-height) (err ERR-INVALID-TARGET))
        
        (map-set learning-goals {member: tx-sender, goal-id: goal-id}
            {
                goal-title: goal-title,
                target-skill: target-skill,
                target-level: target-level,
                target-date: target-date,
                created-at: stacks-block-height,
                progress-percentage: u0,
                is-completed: false,
                completion-date: none,
                reward-earned: u0
            }
        )
        
        ;; Update member profile goal count
        (map-set member-progress-profiles tx-sender
            (merge member-profile
                {
                    total-goals: (+ (get total-goals member-profile) u1),
                    last-activity: stacks-block-height
                }
            )
        )
        
        (var-set next-goal-id (+ goal-id u1))
        (ok goal-id)
    )
)

;; Update goal progress and check completion
(define-public (update-goal-progress (goal-id uint))
    (let
        (
            (goal (unwrap! (map-get? learning-goals {member: tx-sender, goal-id: goal-id}) (err ERR-GOAL-NOT-FOUND)))
            (current-skill (map-get? member-skills {member: tx-sender, skill-name: (get target-skill goal)}))
        )
        (asserts! (not (get is-completed goal)) (err ERR-GOAL-COMPLETED))
        
        (match current-skill
            skill-data
            (let
                (
                    (current-level (get skill-level skill-data))
                    (target-level (get target-level goal))
                    (progress-pct (min u100 (/ (* current-level u100) target-level)))
                    (is-goal-complete (>= current-level target-level))
                )
                (map-set learning-goals {member: tx-sender, goal-id: goal-id}
                    (merge goal
                        {
                            progress-percentage: progress-pct,
                            is-completed: is-goal-complete,
                            completion-date: (if is-goal-complete (some stacks-block-height) none),
                            reward-earned: (if is-goal-complete (calculate-goal-reward target-level) u0)
                        }
                    )
                )
                
                ;; Update member profile if goal completed
                (if is-goal-complete
                    (let
                        (
                            (member-profile (unwrap-panic (map-get? member-progress-profiles tx-sender)))
                        )
                        (map-set member-progress-profiles tx-sender
                            (merge member-profile
                                {
                                    completed-goals: (+ (get completed-goals member-profile) u1),
                                    total-experience: (+ (get total-experience member-profile) (calculate-goal-reward target-level))
                                }
                            )
                        )
                        (ok "goal-completed")
                    )
                    (ok "progress-updated")
                )
            )
            (ok "skill-not-started")
        )
    )
)

;; Record a learning session
(define-public (record-learning-session
    (skill-focused (string-ascii 50))
    (session-duration uint)
    (session-type (string-ascii 20))
    (notes (string-ascii 300)))
    (let
        (
            (session-id (var-get next-session-id))
            (experience-gained (calculate-session-experience session-duration session-type))
        )
        (map-set learning-sessions {member: tx-sender, session-id: session-id}
            {
                skill-focused: skill-focused,
                session-duration: session-duration,
                experience-gained: experience-gained,
                session-date: stacks-block-height,
                session-type: session-type,
                notes: notes
            }
        )
        
        ;; Auto-update skill progress
        (try! (track-skill-progress skill-focused experience-gained session-duration "general"))
        
        ;; Update learning streak
        (update-learning-streak tx-sender)
        
        (var-set next-session-id (+ session-id u1))
        (ok session-id)
    )
)

;; Read-only functions

(define-read-only (get-member-skill (member principal) (skill-name (string-ascii 50)))
    (map-get? member-skills {member: member, skill-name: skill-name})
)

(define-read-only (get-member-progress-profile (member principal))
    (map-get? member-progress-profiles member)
)

(define-read-only (get-learning-goal (member principal) (goal-id uint))
    (map-get? learning-goals {member: member, goal-id: goal-id})
)

(define-read-only (get-learning-session (member principal) (session-id uint))
    (map-get? learning-sessions {member: member, session-id: session-id})
)

(define-read-only (get-skill-achievement (member principal) (achievement-type (string-ascii 50)))
    (map-get? skill-achievements {member: member, achievement-type: achievement-type})
)

(define-read-only (get-system-stats)
    {
        total-learners: (var-get total-learners),
        total-sessions: (var-get next-session-id),
        total-goals: (var-get next-goal-id)
    }
)

;; Private functions

(define-private (calculate-skill-level (experience-points uint))
    (let
        (
            (level (/ experience-points u100)) ;; 100 XP per level
        )
        (if (<= level SKILL-LEVEL-CAP)
            level
            SKILL-LEVEL-CAP
        )
    )
)

(define-private (calculate-session-experience (duration uint) (session-type (string-ascii 20)))
    (let
        (
            (base-exp (/ duration u10)) ;; 1 XP per 10 units of duration
            (type-multiplier (if (is-eq session-type "intensive") u2 u1))
        )
        (* base-exp type-multiplier)
    )
)

(define-private (calculate-goal-reward (target-level uint))
    (* target-level u50) ;; 50 XP reward per level targeted
)

(define-private (update-member-profile-stats (member principal) (experience-gained uint))
    (let
        (
            (profile (unwrap-panic (map-get? member-progress-profiles member)))
        )
        (map-set member-progress-profiles member
            (merge profile
                {
                    total-experience: (+ (get total-experience profile) experience-gained),
                    last-activity: stacks-block-height,
                    progress-level: (calculate-progress-level (+ (get total-experience profile) experience-gained))
                }
            )
        )
    )
)

(define-private (calculate-progress-level (total-experience uint))
    (let
        (
            (level (/ total-experience u500)) ;; 500 XP per progress level
        )
        (+ level u1) ;; Start at level 1
    )
)

(define-private (check-skill-achievements (member principal) (skill-name (string-ascii 50)) (new-level uint))
    (if (is-eq new-level u10)
        (try! (unlock-achievement member "skill-novice" "Reached level 10 in a skill"))
        (if (is-eq new-level u25)
            (try! (unlock-achievement member "skill-intermediate" "Reached level 25 in a skill"))
            (if (is-eq new-level u50)
                (try! (unlock-achievement member "skill-advanced" "Reached level 50 in a skill"))
                (if (is-eq new-level u100)
                    (try! (unlock-achievement member "skill-master" "Reached level 100 in a skill"))
                    (ok true)
                )
            )
        )
    )
)

(define-private (unlock-achievement (member principal) (achievement-type (string-ascii 50)) (description (string-ascii 200)))
    (let
        (
            (existing-achievement (map-get? skill-achievements {member: member, achievement-type: achievement-type}))
        )
        (if (is-none existing-achievement)
            (begin
                (map-set skill-achievements {member: member, achievement-type: achievement-type}
                    {
                        unlocked-at: stacks-block-height,
                        achievement-description: description,
                        reputation-bonus: u100,
                        experience-bonus: u200
                    }
                )
                (ok true)
            )
            (ok false) ;; Already unlocked
        )
    )
)

(define-private (update-learning-streak (member principal))
    (let
        (
            (profile (unwrap-panic (map-get? member-progress-profiles member)))
            (last-activity (get last-activity profile))
            (current-streak (get learning-streak profile))
            ;; Simple streak logic: if last activity was within 2880 blocks (~20 days), increment
            (is-streak-continuing (< (- stacks-block-height last-activity) u2880))
        )
        (map-set member-progress-profiles member
            (merge profile
                {
                    learning-streak: (if is-streak-continuing (+ current-streak u1) u1),
                    last-activity: stacks-block-height
                }
            )
        )
    )
)

(define-private (min (a uint) (b uint))
    (if (< a b) a b)
)
