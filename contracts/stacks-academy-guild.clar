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
