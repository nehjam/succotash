;; Multi-User Wallet Smart Contract
;; Allows multiple users to deposit and withdraw their own funds

;; Data Variables
(define-map user-balances principal uint)

;; Error Constants
(define-constant ERR-INSUFFICIENT-BALANCE (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-TRANSFER-FAILED (err u102))

;; Read-only functions

;; Get balance for a specific user
(define-read-only (get-balance (user principal))
    (default-to u0 (map-get? user-balances user))
)

;; Get balance for the contract caller
(define-read-only (get-my-balance)
    (get-balance tx-sender)
)

;; Public functions

;; Deposit STX for a specific user
(define-public (deposit (user principal))
    (let (
        (amount (stx-get-balance tx-sender))
        (current-balance (get-balance user))
    )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (match (stx-transfer? amount tx-sender (as-contract tx-sender))
            success (begin
                (map-set user-balances user (+ current-balance amount))
                (ok amount)
            )
            error ERR-TRANSFER-FAILED
        )
    )
)

;; Deposit STX for the caller (simplified version)
(define-public (deposit-self (amount uint))
    (let (
        (current-balance (get-balance tx-sender))
    )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (match (stx-transfer? amount tx-sender (as-contract tx-sender))
            success (begin
                (map-set user-balances tx-sender (+ current-balance amount))
                (ok amount)
            )
            error ERR-TRANSFER-FAILED
        )
    )
)

;; Withdraw STX from caller's balance
(define-public (withdraw (amount uint))
    (let (
        (current-balance (get-balance tx-sender))
    )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
        (match (as-contract (stx-transfer? amount tx-sender tx-sender))
            success (begin
                (map-set user-balances tx-sender (- current-balance amount))
                (ok amount)
            )
            error ERR-TRANSFER-FAILED
        )
    )
)

;; Withdraw all funds from caller's balance
(define-public (withdraw-all)
    (let (
        (current-balance (get-balance tx-sender))
    )
        (asserts! (> current-balance u0) ERR-INSUFFICIENT-BALANCE)
        (match (as-contract (stx-transfer? current-balance tx-sender tx-sender))
            success (begin
                (map-delete user-balances tx-sender)
                (ok current-balance)
            )
            error ERR-TRANSFER-FAILED
        )
    )
)

;; Emergency function - get total contract balance (for debugging)
(define-read-only (get-contract-balance)
    (stx-get-balance (as-contract tx-sender))
)