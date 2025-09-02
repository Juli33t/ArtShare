;; ArtShare: Digital Art Tokenization and Royalty Platform
;; This contract manages art piece tokenization, collector ownership, and artist royalty distribution

;; Error codes
(define-constant err-unauthorized (err u1))
(define-constant err-artwork-exists (err u2))
(define-constant err-artwork-not-found (err u3))
(define-constant err-insufficient-funds (err u4))
(define-constant err-tokens-exhausted (err u5))
(define-constant err-transfer-failed (err u6))
(define-constant err-invalid-amount (err u7))
(define-constant err-invalid-string (err u8))

;; Data structures
(define-map artworks
  { artwork-id: uint }
  {
    title: (string-ascii 100),
    artist: (string-ascii 100),
    total-tokens: uint,
    remaining-tokens: uint,
    token-price: uint,
    total-royalties: uint,
    creator: principal
  }
)

(define-map collector-tokens
  { artwork-id: uint, collector: principal }
  { tokens: uint }
)

(define-map royalty-payouts
  { payout-id: uint }
  {
    artwork-id: uint,
    amount: uint,
    payout-date: uint,
    processed: bool
  }
)

;; Variables
(define-data-var next-artwork-id uint u1)
(define-data-var next-payout-id uint u1)
(define-data-var platform-commission-percent uint u3) ;; 3% platform commission
(define-data-var contract-admin principal tx-sender)

;; Read-only functions
(define-read-only (get-artwork (artwork-id uint))
  (map-get? artworks { artwork-id: artwork-id })
)

(define-read-only (get-collector-tokens (artwork-id uint) (collector principal))
  (default-to { tokens: u0 }
    (map-get? collector-tokens { artwork-id: artwork-id, collector: collector })
  )
)

(define-read-only (get-payout (payout-id uint))
  (map-get? royalty-payouts { payout-id: payout-id })
)

(define-read-only (calculate-token-value (artwork-id uint) (token-count uint))
  (let (
    (artwork (unwrap-panic (get-artwork artwork-id)))
    (token-price (get token-price artwork))
  )
    (* token-price token-count)
  )
)

;; Helper functions
(define-private (is-valid-string (value (string-ascii 100)))
  (> (len value) u0)
)

;; Public functions
(define-public (mint-artwork (title (string-ascii 100)) (artist (string-ascii 100)) (total-tokens uint) (token-price uint))
  (let (
    (artwork-id (var-get next-artwork-id))
    (caller tx-sender)
    (valid-title (is-valid-string title))
    (valid-artist (is-valid-string artist))
  )
    ;; Check that title is valid
    (asserts! valid-title err-invalid-string)
    
    ;; Check that artist is valid
    (asserts! valid-artist err-invalid-string)
    
    ;; Check that total tokens is greater than zero
    (asserts! (> total-tokens u0) err-invalid-amount)
    
    ;; Check that token price is greater than zero
    (asserts! (> token-price u0) err-invalid-amount)
    
    ;; Add the artwork to the map
    (map-set artworks
      { artwork-id: artwork-id }
      {
        title: title,
        artist: artist,
        total-tokens: total-tokens,
        remaining-tokens: total-tokens,
        token-price: token-price,
        total-royalties: u0,
        creator: caller
      }
    )
    
    ;; Increment the artwork ID counter
    (var-set next-artwork-id (+ artwork-id u1))
    
    ;; Return the new artwork ID
    (ok artwork-id)
  )
)

(define-public (purchase-tokens (artwork-id uint) (token-count uint))
  (let (
    (artwork (unwrap-panic (get-artwork artwork-id)))
    (remaining-tokens (get remaining-tokens artwork))
    (token-price (get token-price artwork))
    (artwork-creator (get creator artwork))
    (total-cost (* token-price token-count))
    (caller tx-sender)
    (current-tokens (get tokens (get-collector-tokens artwork-id caller)))
  )
    ;; Check that token count is greater than zero
    (asserts! (> token-count u0) err-invalid-amount)
    
    ;; Check that there are enough tokens available
    (asserts! (>= remaining-tokens token-count) err-tokens-exhausted)
    
    ;; Transfer the STX from the collector to the artwork creator
    (asserts! (>= (stx-get-balance caller) total-cost) err-insufficient-funds)
    (try! (stx-transfer? total-cost caller artwork-creator))
    
    ;; Update the artwork's remaining tokens
    (map-set artworks
      { artwork-id: artwork-id }
      (merge artwork { remaining-tokens: (- remaining-tokens token-count) })
    )
    
    ;; Update the collector's tokens
    (map-set collector-tokens
      { artwork-id: artwork-id, collector: caller }
      { tokens: (+ current-tokens token-count) }
    )
    
    (ok true)
  )
)

(define-public (deposit-royalties (artwork-id uint) (amount uint))
  (let (
    (artwork (unwrap-panic (get-artwork artwork-id)))
    (caller tx-sender)
    (artwork-creator (get creator artwork))
    (current-royalties (get total-royalties artwork))
    (validated-amount amount)
  )
    ;; Check that amount is greater than zero
    (asserts! (> validated-amount u0) err-invalid-amount)
    
    ;; Only the artwork creator can deposit royalties
    (asserts! (is-eq caller artwork-creator) err-unauthorized)
    
    ;; Transfer the STX from the caller to the contract
    (try! (stx-transfer? validated-amount caller (as-contract tx-sender)))
    
    ;; Update the artwork's total royalties
    (map-set artworks
      { artwork-id: artwork-id }
      (merge artwork { total-royalties: (+ current-royalties validated-amount) })
    )
    
    ;; Create a new payout
    (let (
      (payout-id (var-get next-payout-id))
    )
      (map-set royalty-payouts
        { payout-id: payout-id }
        {
          artwork-id: artwork-id,
          amount: validated-amount,
          payout-date: u0,
          processed: false
        }
      )
      
      ;; Increment the payout ID counter
      (var-set next-payout-id (+ payout-id u1))
      
      (ok payout-id)
    )
  )
)

(define-public (process-royalty-payout (payout-id uint))
  (let (
    (payout (unwrap-panic (get-payout payout-id)))
    (artwork-id (get artwork-id payout))
    (amount (get amount payout))
    (processed (get processed payout))
    (artwork (unwrap-panic (get-artwork artwork-id)))
    (total-tokens (get total-tokens artwork))
    (platform-commission (/ (* amount (var-get platform-commission-percent)) u100))
    (distributable-amount (- amount platform-commission))
  )
    ;; Check that the payout hasn't already been processed
    (asserts! (not processed) err-unauthorized)
    
    ;; Mark the payout as processed
    (map-set royalty-payouts
      { payout-id: payout-id }
      (merge payout { processed: true })
    )
    
    (ok true)
  )
)

(define-public (claim-royalty-share (artwork-id uint) (payout-id uint))
  (let (
    (payout (unwrap-panic (get-payout payout-id)))
    (payout-artwork-id (get artwork-id payout))
    (amount (get amount payout))
    (processed (get processed payout))
    (artwork (unwrap-panic (get-artwork artwork-id)))
    (total-tokens (get total-tokens artwork))
    (caller tx-sender)
    (collector-tokens (get tokens (get-collector-tokens artwork-id caller)))
    (platform-commission (/ (* amount (var-get platform-commission-percent)) u100))
    (distributable-amount (- amount platform-commission))
    (collector-share (/ (* distributable-amount collector-tokens) total-tokens))
  )
    ;; Check that the artwork IDs match
    (asserts! (is-eq artwork-id payout-artwork-id) err-artwork-not-found)
    
    ;; Check that the collector has tokens
    (asserts! (> collector-tokens u0) err-unauthorized)
    
    ;; Check that the payout is processed
    (asserts! processed err-unauthorized)
    
    ;; Transfer the collector's share of the royalties
    (try! (as-contract (stx-transfer? collector-share tx-sender caller)))
    
    (ok collector-share)
  )
)

(define-public (set-platform-commission (new-commission-percent uint))
  (begin
    ;; Only the contract admin can set the platform commission
    (asserts! (is-eq tx-sender (var-get contract-admin)) err-unauthorized)
    
    ;; Commission cannot be more than 15%
    (asserts! (<= new-commission-percent u15) err-invalid-amount)
    
    (var-set platform-commission-percent new-commission-percent)
    (ok true)
  )
)
