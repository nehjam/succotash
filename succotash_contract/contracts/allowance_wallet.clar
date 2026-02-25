;; Allowance Wallet Smart Contract
;; Allows owner to set allowances for users who can withdraw up to their limit

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_ALLOWANCE (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))

;; Data Variables
(define-data-var contract-balance uint u0)

;; Data Maps
;; Maps user principal to their allowance amount
(define-map allowances principal uint)

;; Maps user principal to their spent amount
(define-map spent-amounts principal uint)

;; Private Functions

;; Get the current allowance for a user
(define-private (get-allowance (user principal))
  (default-to u0 (map-get? allowances user)))

;; Get the amount already spent by a user
(define-private (get-spent-amount (user principal))
  (default-to u0 (map-get? spent-amounts user)))

;; Calculate remaining allowance for a user
(define-private (get-remaining-allowance (user principal))
  (let ((allowance (get-allowance user))
        (spent (get-spent-amount user)))
    (if (>= spent allowance)
        u0
        (- allowance spent))))

;; Public Functions

;; Deposit STX into the contract (only owner)
(define-public (deposit (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok amount)))

;; Set allowance for a user (only owner)
(define-public (set-allowance (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set allowances user amount)
    (ok amount)))

;; Increase allowance for a user (only owner)
(define-public (increase-allowance (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (let ((current-allowance (get-allowance user)))
      (map-set allowances user (+ current-allowance amount))
      (ok (+ current-allowance amount)))))

;; Decrease allowance for a user (only owner)
(define-public (decrease-allowance (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (let ((current-allowance (get-allowance user)))
      (if (>= current-allowance amount)
          (begin
            (map-set allowances user (- current-allowance amount))
            (ok (- current-allowance amount)))
          (begin
            (map-delete allowances user)
            (ok u0))))))

;; Withdraw STX from allowance
(define-public (withdraw (amount uint))
  (let ((remaining-allowance (get-remaining-allowance tx-sender))
        (current-balance (var-get contract-balance))
        (current-spent (get-spent-amount tx-sender)))
    (begin
      (asserts! (> amount u0) ERR_INVALID_AMOUNT)
      (asserts! (<= amount remaining-allowance) ERR_INSUFFICIENT_ALLOWANCE)
      (asserts! (<= amount current-balance) ERR_INSUFFICIENT_BALANCE)
      (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
      (var-set contract-balance (- current-balance amount))
      (map-set spent-amounts tx-sender (+ current-spent amount))
      (ok amount))))

;; Reset spent amount for a user (only owner)
(define-public (reset-spent-amount (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete spent-amounts user)
    (ok true)))

;; Remove allowance for a user (only owner)
(define-public (remove-allowance (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete allowances user)
    (map-delete spent-amounts user)
    (ok true)))

;; Emergency withdraw all funds (only owner)
(define-public (emergency-withdraw)
  (let ((current-balance (var-get contract-balance)))
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
      (asserts! (> current-balance u0) ERR_INSUFFICIENT_BALANCE)
      (try! (as-contract (stx-transfer? current-balance tx-sender CONTRACT_OWNER)))
      (var-set contract-balance u0)
      (ok current-balance))))

;; Read-only Functions

;; Get contract balance
(define-read-only (get-contract-balance)
  (var-get contract-balance))

;; Get user's allowance
(define-read-only (get-user-allowance (user principal))
  (get-allowance user))

;; Get user's spent amount
(define-read-only (get-user-spent (user principal))
  (get-spent-amount user))

;; Get user's remaining allowance
(define-read-only (get-user-remaining-allowance (user principal))
  (get-remaining-allowance user))

;; Check if user has allowance
(define-read-only (has-allowance (user principal))
  (> (get-allowance user) u0))

;; Get contract owner
(define-read-only (get-contract-owner)
  CONTRACT_OWNER)