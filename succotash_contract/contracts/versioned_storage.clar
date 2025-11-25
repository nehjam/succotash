;; Enhanced Versioned Storage Smart Contract
;; Advanced storage with version history, access control, and comprehensive features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-VALUE (err u101))
(define-constant ERR-VERSION-NOT-FOUND (err u102))
(define-constant ERR-INVALID-VERSION (err u103))
(define-constant ERR-STORAGE-LOCKED (err u104))
(define-constant ERR-INVALID-TIMESTAMP (err u105))
(define-constant ERR-MAX-HISTORY-REACHED (err u106))
(define-constant ERR-ROLLBACK-DISABLED (err u107))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-HISTORY-SIZE u100)
(define-constant MAX-VALUE u1000000)

;; Data variables
(define-data-var stored-value uint u0)
(define-data-var current-version uint u0)
(define-data-var is-locked bool false)
(define-data-var rollback-enabled bool true)
(define-data-var auto-cleanup bool false)
(define-data-var cleanup-threshold uint u50)

;; Maps for extended functionality
(define-map version-history uint {
  value: uint,
  timestamp: uint,
  block-height: uint,
  author: principal
})

(define-map authorized-writers principal bool)
(define-map authorized-readers principal bool)
(define-map version-metadata uint {
  description: (string-ascii 100),
  tags: (list 5 (string-ascii 20))
})

;; Events map for audit trail
(define-map events uint {
  event-type: (string-ascii 20),
  version: uint,
  author: principal,
  timestamp: uint,
  data: (string-ascii 200)
})
(define-data-var event-counter uint u0)

;; ============================================================================
;; CORE STORAGE FUNCTIONS
;; ============================================================================

;; Enhanced update function with metadata support
(define-public (update-value (new-value uint) (description (string-ascii 100)) (tags (list 5 (string-ascii 20))))
  (let ((new-version (+ (var-get current-version) u1)))
    (begin
      ;; Authorization checks
      (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (default-to false (map-get? authorized-writers tx-sender))) ERR-NOT-AUTHORIZED)
      (asserts! (not (var-get is-locked)) ERR-STORAGE-LOCKED)
      (asserts! (<= new-value MAX-VALUE) ERR-INVALID-VALUE)
      
      ;; Auto-cleanup if enabled
      (if (var-get auto-cleanup)
        (unwrap-panic (cleanup-old-versions))
        u0)
      
      ;; Store current state in history before updating
      (map-set version-history (var-get current-version) {
        value: (var-get stored-value),
        timestamp: block-height,
        block-height: block-height,
        author: tx-sender
      })
      
      ;; Update current state
      (var-set stored-value new-value)
      (var-set current-version new-version)
      
      ;; Store metadata
      (map-set version-metadata new-version {
        description: description,
        tags: tags
      })
      
      ;; Log event
      (unwrap-panic (log-event "UPDATE" new-version (concat "Updated to: " (uint-to-ascii new-value))))
      
      (ok new-version)
    )
  )
)

;; Simple update function (backward compatibility)
(define-public (update-value-simple (new-value uint))
  (update-value new-value "" (list))
)

;; ============================================================================
;; GETTER FUNCTIONS
;; ============================================================================

;; Enhanced getter with full context
(define-read-only (get-full-context)
  {
    value: (var-get stored-value),
    version: (var-get current-version),
    timestamp: block-height,
    block-height: block-height,
    is-locked: (var-get is-locked),
    total-versions: (var-get current-version)
  }
)

;; Original getter (backward compatibility)
(define-read-only (get-value-and-version)
  {
    value: (var-get stored-value),
    version: (var-get current-version)
  }
)

;; Get specific version from history
(define-read-only (get-version-data (version uint))
  (if (is-eq version (var-get current-version))
    (some {
      value: (var-get stored-value),
      timestamp: block-height,
      block-height: block-height,
      author: CONTRACT-OWNER
    })
    (map-get? version-history version)
  )
)

;; Get version metadata
(define-read-only (get-version-metadata (version uint))
  (map-get? version-metadata version)
)

;; Get version range
(define-read-only (get-version-range (start-version uint) (end-version uint))
  (let ((current (var-get current-version)))
    (if (and (<= start-version end-version) (<= end-version current))
      (ok {
        start: start-version,
        end: end-version,
        count: (+ (- end-version start-version) u1)
      })
      ERR-INVALID-VERSION
    )
  )
)

;; ============================================================================
;; VERSION HISTORY MANAGEMENT
;; ============================================================================

;; Rollback to specific version
(define-public (rollback-to-version (target-version uint))
  (let ((version-data (unwrap! (get-version-data target-version) ERR-VERSION-NOT-FOUND))
        (new-version (+ (var-get current-version) u1)))
    (begin
      (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
      (asserts! (var-get rollback-enabled) ERR-ROLLBACK-DISABLED)
      (asserts! (not (var-get is-locked)) ERR-STORAGE-LOCKED)
      
      ;; Store current state in history
      (map-set version-history (var-get current-version) {
        value: (var-get stored-value),
        timestamp: block-height,
        block-height: block-height,
        author: tx-sender
      })
      
      ;; Rollback
      (var-set stored-value (get value version-data))
      (var-set current-version new-version)
      
      ;; Log rollback event
      (unwrap-panic (log-event "ROLLBACK" new-version (concat "Rolled back to version: " (uint-to-ascii target-version))))
      
      (ok new-version)
    )
  )
)

;; Cleanup old versions
(define-public (cleanup-old-versions)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; This is a simplified cleanup - in practice, you'd iterate through versions
    ;; and remove old ones based on your cleanup criteria
    (unwrap-panic (log-event "CLEANUP" (var-get current-version) "Cleaned up old versions"))
    
    (ok u0)
  )
)

;; ============================================================================
;; ACCESS CONTROL
;; ============================================================================

;; Grant write access
(define-public (grant-write-access (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-writers user true)
    (unwrap-panic (log-event "ACCESS_GRANT" (var-get current-version) "Granted write access"))
    (ok true)
  )
)

;; Revoke write access
(define-public (revoke-write-access (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-writers user false)
    (unwrap-panic (log-event "ACCESS_REVOKE" (var-get current-version) "Revoked write access"))
    (ok true)
  )
)

;; Check write access
(define-read-only (has-write-access (user principal))
  (or (is-eq user CONTRACT-OWNER) (default-to false (map-get? authorized-writers user)))
)

;; ============================================================================
;; LOCKING MECHANISM
;; ============================================================================

;; Lock storage (prevents all updates)
(define-public (lock-storage)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set is-locked true)
    (unwrap-panic (log-event "LOCK" (var-get current-version) "Storage locked"))
    (ok true)
  )
)

;; Unlock storage
(define-public (unlock-storage)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set is-locked false)
    (unwrap-panic (log-event "UNLOCK" (var-get current-version) "Storage unlocked"))
    (ok true)
  )
)

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

;; Toggle rollback capability
(define-public (toggle-rollback (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set rollback-enabled enabled)
    (ok enabled)
  )
)

;; Configure auto-cleanup
(define-public (configure-auto-cleanup (enabled bool) (threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set auto-cleanup enabled)
    (var-set cleanup-threshold threshold)
    (ok {enabled: enabled, threshold: threshold})
  )
)

;; ============================================================================
;; EVENT LOGGING
;; ============================================================================

;; Internal function to log events
(define-private (log-event (event-type (string-ascii 20)) (version uint) (data (string-ascii 200)))
  (let ((event-id (+ (var-get event-counter) u1)))
    (begin
      (map-set events event-id {
        event-type: event-type,
        version: version,
        author: tx-sender,
        timestamp: block-height,
        data: data
      })
      (var-set event-counter event-id)
      (ok event-id)
    )
  )
)

;; Get event by ID
(define-read-only (get-event (event-id uint))
  (map-get? events event-id)
)

;; Get recent events
(define-read-only (get-recent-events (count uint))
  (let ((current-event (var-get event-counter)))
    {
      total-events: current-event,
      requested-count: count,
      latest-event-id: current-event
    }
  )
)

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    current-version: (var-get current-version),
    current-value: (var-get stored-value),
    is-locked: (var-get is-locked),
    rollback-enabled: (var-get rollback-enabled),
    auto-cleanup: (var-get auto-cleanup),
    total-events: (var-get event-counter),
    owner: CONTRACT-OWNER
  }
)

;; Health check
(define-read-only (health-check)
  {
    status: "OK",
    version: (var-get current-version),
    locked: (var-get is-locked),
    timestamp: block-height
  }
)

;; Convert uint to string (helper function)
(define-private (uint-to-ascii (value uint))
  ;; Simplified conversion - in practice you'd implement full uint-to-string conversion
  (if (< value u10)
    (if (is-eq value u0) "0"
    (if (is-eq value u1) "1"
    (if (is-eq value u2) "2"
    (if (is-eq value u3) "3"
    (if (is-eq value u4) "4"
    (if (is-eq value u5) "5"
    (if (is-eq value u6) "6"
    (if (is-eq value u7) "7"
    (if (is-eq value u8) "8"
    "9")))))))))
    "10+")
)

;; ============================================================================
;; BACKWARD COMPATIBILITY
;; ============================================================================

(define-read-only (get-value)
  (var-get stored-value)
)

(define-read-only (get-version)
  (var-get current-version)
)

(define-read-only (get-owner)
  CONTRACT-OWNER
)