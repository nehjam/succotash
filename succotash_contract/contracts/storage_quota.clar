;; StorageQuota Smart Contract
;; Purpose: Enforce storage quotas per user with upgrade capabilities

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_QUOTA_EXCEEDED (err u101))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u102))
(define-constant ERR_USER_NOT_FOUND (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))

;; Default quota in bytes (100MB)
(define-constant DEFAULT_QUOTA u104857600)

;; Quota upgrade prices (in microSTX per GB)
(define-constant PRICE_PER_GB u1000000)

;; Data Variables
(define-data-var total-users uint u0)

;; Data Maps
;; User storage information
(define-map user-storage
    principal
    {
        quota-limit: uint,
        used-storage: uint,
        last-updated: uint
    }
)

;; Quota upgrade packages
(define-map quota-packages
    uint ;; package-id
    {
        additional-gb: uint,
        price: uint,
        active: bool
    }
)

;; Read-only functions

;; Get user storage info
(define-read-only (get-user-storage (user principal))
    (map-get? user-storage user)
)

;; Get user quota limit
(define-read-only (get-user-quota (user principal))
    (match (map-get? user-storage user)
        storage-info (get quota-limit storage-info)
        DEFAULT_QUOTA
    )
)

;; Get user used storage
(define-read-only (get-used-storage (user principal))
    (match (map-get? user-storage user)
        storage-info (get used-storage storage-info)
        u0
    )
)

;; Get available storage for user
(define-read-only (get-available-storage (user principal))
    (let ((quota (get-user-quota user))
          (used (get-used-storage user)))
        (if (>= used quota)
            u0
            (- quota used)
        )
    )
)

;; Check if upload is allowed
(define-read-only (can-upload (user principal) (file-size uint))
    (let ((available (get-available-storage user)))
        (>= available file-size)
    )
)

;; Get quota package info
(define-read-only (get-quota-package (package-id uint))
    (map-get? quota-packages package-id)
)

;; Get total users count
(define-read-only (get-total-users)
    (var-get total-users)
)

;; Public functions

;; Initialize user with default quota
(define-public (initialize-user)
    (let ((user tx-sender))
        (match (map-get? user-storage user)
            existing-storage (ok false) ;; User already exists
            (begin
                (map-set user-storage user {
                    quota-limit: DEFAULT_QUOTA,
                    used-storage: u0,
                    last-updated: block-height
                })
                (var-set total-users (+ (var-get total-users) u1))
                (ok true)
            )
        )
    )
)

;; Record file upload (increases used storage)
(define-public (record-upload (file-size uint))
    (let ((user tx-sender)
          (max-file-size u5368709120)) ;; 5GB max file size limit
        (asserts! (> file-size u0) ERR_INVALID_AMOUNT)
        (asserts! (<= file-size max-file-size) ERR_INVALID_AMOUNT)
        
        ;; Initialize user if not exists
        (match (map-get? user-storage user)
            storage-info
            ;; User exists, proceed with upload
            (let ((current-used (get used-storage storage-info))
                  (quota-limit (get quota-limit storage-info)))
                (asserts! (<= (+ current-used file-size) quota-limit) ERR_QUOTA_EXCEEDED)
                (asserts! (>= (+ current-used file-size) current-used) ERR_INVALID_AMOUNT) ;; Overflow check
                (map-set user-storage user {
                    quota-limit: quota-limit,
                    used-storage: (+ current-used file-size),
                    last-updated: block-height
                })
                (ok true)
            )
            ;; User doesn't exist, initialize with default quota and then upload
            (begin
                (asserts! (<= file-size DEFAULT_QUOTA) ERR_QUOTA_EXCEEDED)
                (map-set user-storage user {
                    quota-limit: DEFAULT_QUOTA,
                    used-storage: file-size,
                    last-updated: block-height
                })
                (var-set total-users (+ (var-get total-users) u1))
                (ok true)
            )
        )
    )
)

;; Record file deletion (decreases used storage)
(define-public (record-deletion (file-size uint))
    (let ((user tx-sender)
          (max-file-size u5368709120)) ;; 5GB max file size limit
        (asserts! (> file-size u0) ERR_INVALID_AMOUNT)
        (asserts! (<= file-size max-file-size) ERR_INVALID_AMOUNT)
        
        (match (map-get? user-storage user)
            storage-info
            (let ((current-used (get used-storage storage-info))
                  (new-used (if (>= current-used file-size)
                               (- current-used file-size)
                               u0)))
                (map-set user-storage user {
                    quota-limit: (get quota-limit storage-info),
                    used-storage: new-used,
                    last-updated: block-height
                })
                (ok true)
            )
            ERR_USER_NOT_FOUND
        )
    )
)

;; Upgrade quota via payment
(define-public (upgrade-quota (additional-gb uint))
    (let ((user tx-sender)
          (max-upgrade-gb u1000) ;; Max 1000GB upgrade at once
          (max-total-quota u1099511627776)) ;; Max 1TB total quota
        
        (asserts! (> additional-gb u0) ERR_INVALID_AMOUNT)
        (asserts! (<= additional-gb max-upgrade-gb) ERR_INVALID_AMOUNT)
        
        (let ((cost (* additional-gb PRICE_PER_GB))
              (additional-bytes (* additional-gb u1073741824))) ;; Convert GB to bytes
            
            ;; Check for overflow in cost calculation
            (asserts! (>= cost additional-gb) ERR_INVALID_AMOUNT)
            ;; Check for overflow in bytes calculation  
            (asserts! (>= additional-bytes additional-gb) ERR_INVALID_AMOUNT)
            
            ;; Transfer payment to contract
            (try! (stx-transfer? cost user (as-contract tx-sender)))
            
            ;; Initialize user if not exists, then upgrade
            (match (map-get? user-storage user)
                storage-info
                (let ((current-quota (get quota-limit storage-info))
                      (new-quota (+ current-quota additional-bytes)))
                    ;; Check for overflow and max quota limit
                    (asserts! (>= new-quota current-quota) ERR_INVALID_AMOUNT)
                    (asserts! (<= new-quota max-total-quota) ERR_INVALID_AMOUNT)
                    (map-set user-storage user {
                        quota-limit: new-quota,
                        used-storage: (get used-storage storage-info),
                        last-updated: block-height
                    })
                    (ok true)
                )
                ;; User doesn't exist, initialize with default quota plus upgrade
                (let ((new-quota (+ DEFAULT_QUOTA additional-bytes)))
                    (asserts! (>= new-quota DEFAULT_QUOTA) ERR_INVALID_AMOUNT)
                    (asserts! (<= new-quota max-total-quota) ERR_INVALID_AMOUNT)
                    (map-set user-storage user {
                        quota-limit: new-quota,
                        used-storage: u0,
                        last-updated: block-height
                    })
                    (var-set total-users (+ (var-get total-users) u1))
                    (ok true)
                )
            )
        )
    )
)

;; Admin functions

;; Set user quota (admin only)
(define-public (admin-set-user-quota (user principal) (new-quota uint))
    (let ((max-quota u1099511627776)) ;; Max 1TB quota
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-quota u0) ERR_INVALID_AMOUNT)
        (asserts! (<= new-quota max-quota) ERR_INVALID_AMOUNT)
        
        (match (map-get? user-storage user)
            storage-info
            (begin
                (map-set user-storage user {
                    quota-limit: new-quota,
                    used-storage: (get used-storage storage-info),
                    last-updated: block-height
                })
                (ok true)
            )
            ;; Create new user entry
            (begin
                (map-set user-storage user {
                    quota-limit: new-quota,
                    used-storage: u0,
                    last-updated: block-height
                })
                (var-set total-users (+ (var-get total-users) u1))
                (ok true)
            )
        )
    )
)

;; Update default quota price (admin only)
(define-public (admin-update-price-per-gb (new-price uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-price u0) ERR_INVALID_AMOUNT)
        ;; Note: This would require updating the constant in a new contract deployment
        ;; For now, we acknowledge the limitation of Clarity constants
        (ok true)
    )
)

;; Create quota package (admin only)
(define-public (admin-create-quota-package (package-id uint) (gb-amount uint) (price uint))
    (let ((max-gb u1000) ;; Max 1000GB per package
          (max-price u100000000)) ;; Max 100 STX price
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> gb-amount u0) ERR_INVALID_AMOUNT)
        (asserts! (> price u0) ERR_INVALID_AMOUNT)
        (asserts! (<= gb-amount max-gb) ERR_INVALID_AMOUNT)
        (asserts! (<= price max-price) ERR_INVALID_AMOUNT)
        
        (map-set quota-packages package-id {
            additional-gb: gb-amount,
            price: price,
            active: true
        })
        (ok true)
    )
)

;; Purchase quota package
(define-public (purchase-quota-package (package-id uint))
    (let ((user tx-sender))
        (match (map-get? quota-packages package-id)
            package-info
            (if (get active package-info)
                (let ((cost (get price package-info))
                      (gb-amount (get additional-gb package-info)))
                    ;; Transfer payment
                    (try! (stx-transfer? cost user (as-contract tx-sender)))
                    ;; Upgrade quota
                    (upgrade-quota gb-amount)
                )
                ERR_INVALID_AMOUNT ;; Package not active
            )
            ERR_INVALID_AMOUNT ;; Package not found
        )
    )
)

;; Withdraw contract balance (admin only)
(define-public (admin-withdraw (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (as-contract (stx-transfer? amount tx-sender recipient))
    )
)