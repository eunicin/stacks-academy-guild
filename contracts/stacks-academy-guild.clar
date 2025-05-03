;; Stacks Academy Guild
;;
;; A comprehensive system for educational institutions to manage scholar registrations,
;; academic profiles, and information sharing permissions within the platform.

;; =============================================
;; Configuration & Constants
;; =============================================

;; System authority 
(define-constant SYSTEM-ADMIN tx-sender)

;; Error code definitions for system operations
(define-constant ERROR-ACCESS-DENIED (err u500))
(define-constant ERROR-RECORD-NONEXISTENT (err u501)) 
(define-constant ERROR-SCHOLAR-ALREADY-EXISTS (err u502))
(define-constant ERROR-VALIDATION-FAILED (err u503))
(define-constant ERROR-OPERATION-PROHIBITED (err u504))

;; =============================================
;; Storage Definitions
;; =============================================

;; Primary academic record storage system
(define-map academic-records
  { scholar-id: uint }
  {
    display-name: (string-ascii 50),
    blockchain-address: principal,
    enrollment-timestamp: uint,
    academic-summary: (string-ascii 160),
    academic-interests: (list 5 (string-ascii 30))
  }
)

;; Record access control system
(define-map record-access-permissions
  { scholar-id: uint, accessor-address: principal }
  { access-status: bool }
)

;; Scholar engagement history tracking
(define-map scholar-engagement-tracker
  { scholar-id: uint }
  {
    last-visit: uint,
    visit-count: uint,
    last-activity: (string-ascii 50)
  }
)

;; Counter for total registered scholars
(define-data-var registered-scholars-count uint u0)

;; =============================================
;; Utility Functions
;; =============================================

;; Validate if an interest tag is properly formatted
(define-private (is-interest-valid? (interest-tag (string-ascii 30)))
  (and
    (> (len interest-tag) u0)
    (< (len interest-tag) u31)
  )
)

;; Validate complete interests collection
(define-private (validate-interests-collection (interests-collection (list 5 (string-ascii 30))))
  (and
    (> (len interests-collection) u0)
    (<= (len interests-collection) u5)
    (is-eq (len (filter is-interest-valid? interests-collection)) (len interests-collection))
  )
)

;; Check if a scholar record exists in the system
(define-private (does-scholar-exist? (scholar-id uint))
  (is-some (map-get? academic-records { scholar-id: scholar-id }))
)

;; Verify scholar record ownership
(define-private (verify-scholar-ownership? (scholar-id uint) (address principal))
  (match (map-get? academic-records { scholar-id: scholar-id })
    scholar-data (is-eq (get blockchain-address scholar-data) address)
    false
  )
)

;; =============================================
;; System Administrative Functions
;; =============================================

;; Examine ownership status of a scholar's record
(define-public (validate-record-ownership (scholar-id uint) (claimed-owner principal))
  (let
    (
      (scholar-data (unwrap! (map-get? academic-records { scholar-id: scholar-id }) ERROR-RECORD-NONEXISTENT))
    )
    (ok (is-eq claimed-owner (get blockchain-address scholar-data)))
  )
)

;; Restrict access to scholar profiles based on permissions
(define-public (enforce-record-access-restrictions (scholar-id uint) (address principal))
  (let
    (
      (scholar-data (unwrap! (map-get? academic-records { scholar-id: scholar-id }) ERROR-RECORD-NONEXISTENT))
    )
    ;; Ensure requestor has appropriate access
    (asserts! (is-eq (get blockchain-address scholar-data) address) ERROR-OPERATION-PROHIBITED)
    (ok true)
  )
)

;; =============================================
;; Scholar Record Management
;; =============================================

;; Create new scholar enrollment
(define-public (create-scholar-enrollment
    (display-name (string-ascii 50))
    (academic-summary (string-ascii 160))
    (academic-interests (list 5 (string-ascii 30))))
  (let
    (
      (new-scholar-id (+ (var-get registered-scholars-count) u1))
    )
    ;; Input validation procedures
    (asserts! (and (> (len display-name) u0) (< (len display-name) u51)) ERROR-VALIDATION-FAILED)
    (asserts! (and (> (len academic-summary) u0) (< (len academic-summary) u161)) ERROR-VALIDATION-FAILED)
    (asserts! (validate-interests-collection academic-interests) ERROR-VALIDATION-FAILED)

    ;; Initialize scholar record
    (map-insert academic-records
      { scholar-id: new-scholar-id }
      {
        display-name: display-name,
        blockchain-address: tx-sender,
        enrollment-timestamp: block-height,
        academic-summary: academic-summary,
        academic-interests: academic-interests
      }
    )

    ;; Configure default access permissions
    (map-insert record-access-permissions
      { scholar-id: new-scholar-id, accessor-address: tx-sender }
      { access-status: true }
    )

    ;; Update total enrollment statistics
    (var-set registered-scholars-count new-scholar-id)
    (ok new-scholar-id)
  )
)

;; Add new scholar with complete profile
(define-public (register-academic-scholar
    (display-name (string-ascii 50))
    (academic-summary (string-ascii 160))
    (academic-interests (list 5 (string-ascii 30))))
  (let
    (
      (new-scholar-id (+ (var-get registered-scholars-count) u1))
    )
    ;; Input validation procedures
    (asserts! (and (> (len display-name) u0) (< (len display-name) u51)) ERROR-VALIDATION-FAILED)
    (asserts! (and (> (len academic-summary) u0) (< (len academic-summary) u161)) ERROR-VALIDATION-FAILED)
    (asserts! (validate-interests-collection academic-interests) ERROR-VALIDATION-FAILED)

    ;; Initialize scholar record
    (map-insert academic-records
      { scholar-id: new-scholar-id }
      {
        display-name: display-name,
        blockchain-address: tx-sender,
        enrollment-timestamp: block-height,
        academic-summary: academic-summary,
        academic-interests: academic-interests
      }
    )

    ;; Configure default access permissions
    (map-insert record-access-permissions
      { scholar-id: new-scholar-id, accessor-address: tx-sender }
      { access-status: true }
    )

    ;; Update total enrollment statistics
    (var-set registered-scholars-count new-scholar-id)
    (ok new-scholar-id)
  )
)

;; Update scholar's field of interest
(define-public (modify-scholar-interests (scholar-id uint) (revised-interests (list 5 (string-ascii 30))))
  (let
    (
      (scholar-data (unwrap! (map-get? academic-records { scholar-id: scholar-id }) ERROR-RECORD-NONEXISTENT))
    )
    ;; Validation procedures
    (asserts! (does-scholar-exist? scholar-id) ERROR-RECORD-NONEXISTENT)
    (asserts! (is-eq (get blockchain-address scholar-data) tx-sender) ERROR-OPERATION-PROHIBITED)
    (asserts! (validate-interests-collection revised-interests) ERROR-VALIDATION-FAILED)

    ;; Update interests field only
    (map-set academic-records
      { scholar-id: scholar-id }
      (merge scholar-data { academic-interests: revised-interests })
    )
    (ok true)
  )
)

;; Change scholar display name
(define-public (update-scholar-display-name (scholar-id uint) (new-display-name (string-ascii 50)))
  (let
    (
      (scholar-data (unwrap! (map-get? academic-records { scholar-id: scholar-id }) ERROR-RECORD-NONEXISTENT))
    )
    ;; Validation procedures
    (asserts! (does-scholar-exist? scholar-id) ERROR-RECORD-NONEXISTENT)
    (asserts! (is-eq (get blockchain-address scholar-data) tx-sender) ERROR-OPERATION-PROHIBITED)

    ;; Update display name field
    (map-set academic-records
      { scholar-id: scholar-id }
      (merge scholar-data { display-name: new-display-name })
    )
    (ok true)
  )
)

;; =============================================
;; Optimized Operations
;; =============================================

;; Streamlined interest update procedure
(define-public (efficient-interest-modification (scholar-id uint) (revised-interests (list 5 (string-ascii 30))))
  (begin
    (asserts! (does-scholar-exist? scholar-id) ERROR-RECORD-NONEXISTENT)
    (asserts! (validate-interests-collection revised-interests) ERROR-VALIDATION-FAILED)
    (map-set academic-records
      { scholar-id: scholar-id }
      (merge (unwrap! (map-get? academic-records { scholar-id: scholar-id }) ERROR-RECORD-NONEXISTENT) 
             { academic-interests: revised-interests })
    )
    (ok "Academic interests successfully updated")
  )
)

;; Comprehensive profile update with enhanced validation
(define-public (comprehensive-profile-update 
    (scholar-id uint) 
    (new-display-name (string-ascii 50)) 
    (new-summary (string-ascii 160)) 
    (new-interests (list 5 (string-ascii 30))))
  (let
    (
      (scholar-data (unwrap! (map-get? academic-records { scholar-id: scholar-id }) ERROR-RECORD-NONEXISTENT))
    )
    ;; Extended validation procedures
    (asserts! (does-scholar-exist? scholar-id) ERROR-RECORD-NONEXISTENT)
    (asserts! (is-eq (get blockchain-address scholar-data) tx-sender) ERROR-OPERATION-PROHIBITED)
    (asserts! (> (len new-display-name) u0) ERROR-VALIDATION-FAILED)
    (asserts! (< (len new-display-name) u51) ERROR-VALIDATION-FAILED)
    (asserts! (validate-interests-collection new-interests) ERROR-VALIDATION-FAILED)

    ;; Complete profile update
    (map-set academic-records
      { scholar-id: scholar-id }
      (merge scholar-data { 
        display-name: new-display-name, 
        academic-summary: new-summary, 
        academic-interests: new-interests 
      })
    )
    (ok true)
  )
)

;; =============================================
;; Engagement Tracking
;; =============================================

;; Record scholar system engagement
(define-public (log-scholar-system-interaction (scholar-id uint))
  (let
    (
      (current-engagement-data (default-to 
        { last-visit: u0, visit-count: u0, last-activity: "None" }
        (map-get? scholar-engagement-tracker { scholar-id: scholar-id })))
    )
    (asserts! (does-scholar-exist? scholar-id) ERROR-RECORD-NONEXISTENT)
    (map-set scholar-engagement-tracker
      { scholar-id: scholar-id }
      {
        last-visit: block-height,
        visit-count: (+ (get visit-count current-engagement-data) u1),
        last-activity: "system-access"
      }
    )
    (ok true)
  )
)

