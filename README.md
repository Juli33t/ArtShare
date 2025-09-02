# ArtShare - Digital Art Tokenization Platform

ArtShare is a revolutionary blockchain-based platform that enables artists to tokenize their digital artworks and share royalties with collectors. Built on the Stacks blockchain using Clarity smart contracts.

## Features

- **Art Tokenization**: Artists can mint their digital artworks as tokenized assets
- **Fractional Ownership**: Collectors can purchase tokens representing fractional ownership
- **Royalty Distribution**: Automated royalty sharing based on token ownership
- **Transparent Transactions**: All transactions recorded on the blockchain
- **Creator Economy**: Direct support for digital artists through decentralized ownership

## Smart Contract Functions

### Core Functions
- `mint-artwork`: Create a new tokenized artwork
- `purchase-tokens`: Buy fractional ownership tokens
- `deposit-royalties`: Artists deposit royalties for distribution
- `claim-royalty-share`: Collectors claim their share of royalties

### Read-Only Functions
- `get-artwork`: Retrieve artwork information
- `get-collector-tokens`: Check token ownership
- `calculate-token-value`: Calculate token values

## Getting Started

1. Deploy the contract using Clarinet
2. Artists can mint their artworks using `mint-artwork`
3. Collectors purchase tokens with `purchase-tokens`
4. Artists deposit royalties which are distributed to token holders

## Platform Commission

The platform charges a 3% commission on royalty distributions to maintain the ecosystem.

## License

MIT License - Supporting the creator economy through blockchain innovation.
\`\`\`

```clarity file="greenvest-platform.clar"
;; GreenVest: Renewable Energy Project Investment Platform
;; This contract manages green energy project funding, investor participation, and carbon credit distribution

;; Error codes
(define-constant err-unauthorized (err u1))
(define-constant err-project-exists (err u2))
(define-constant err-project-not-found (err u3))
(define-constant err-insufficient-funds (err u4))
(define-constant err-funding-complete (err u5))
(define-constant err-transfer-failed (err u6))
(define-constant err-invalid-amount (err u7))
(define-constant err-invalid-string (err u8))

;; Data structures
(define-map energy-projects
  { project-id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    total-units: uint,
    available-units: uint,
    unit-cost: uint,
    total-credits: uint,
    developer: principal
  }
)

(define-map investor-units
  { project-id: uint, investor: principal }
  { units: uint }
)

(define-map credit-distributions
  { distribution-id: uint }
  {
    project-id: uint,
    amount: uint,
    distribution-timestamp: uint,
    finalized: bool
  }
)

;; Variables
(define-data-var next-project-id uint u1)
(define-data-var next-distribution-id uint u1)
(define-data-var platform-fee-rate uint u2) ;; 2% platform fee
(define-data-var contract-manager principal tx-sender)

;; Read-only functions
(define-read-only (get-energy-project (project-id uint))
  (map-get? energy-projects { project-id: project-id })
)

(define-read-only (get-investor-units (project-id uint) (investor principal))
  (default-to { units: u0 }
    (map-get? investor-units { project-id: project-id, investor: investor })
  )
)

(define-read-only (get-credit-distribution (distribution-id uint))
  (map-get? credit-distributions { distribution-id: distribution-id })
)

(define-read-only (calculate-investment-value (project-id uint) (unit-count uint))
  (let (
    (project (unwrap-panic (get-energy-project project-id)))
    (unit-cost (get unit-cost project))
  )
    (* unit-cost unit-count)
  )
)

;; Helper functions
(define-private (is-valid-string (value (string-ascii 100)))
  (> (len value) u0)
)

;; Public functions
(define-public (launch-project (name (string-ascii 100)) (location (string-ascii 100)) (total-units uint) (unit-cost uint))
  (let (
    (project-id (var-get next-project-id))
    (caller tx-sender)
    (valid-name (is-valid-string name))
    (valid-location (is-valid-string location))
  )
    ;; Check that name is valid
    (asserts! valid-name err-invalid-string)
    
    ;; Check that location is valid
    (asserts! valid-location err-invalid-string)
    
    ;; Check that total units is greater than zero
    (asserts! (> total-units u0) err-invalid-amount)
    
    ;; Check that unit cost is greater than zero
    (asserts! (> unit-cost u0) err-invalid-amount)
    
    ;; Add the project to the map
    (map-set energy-projects
      { project-id: project-id }
      {
        name: name,
        location: location,
        total-units: total-units,
        available-units: total-units,
        unit-cost: unit-cost,
        total-credits: u0,
        developer: caller
      }
    )
    
    ;; Increment the project ID counter
    (var-set next-project-id (+ project-id u1))
    
    ;; Return the new project ID
    (ok project-id)
  )
)

(define-public (invest-in-project (project-id uint) (unit-count uint))
  (let (
    (project (unwrap-panic (get-energy-project project-id)))
    (available-units (get available-units project))
    (unit-cost (get unit-cost project))
    (project-developer (get developer project))
    (total-investment (* unit-cost unit-count))
    (caller tx-sender)
    (current-units (get units (get-investor-units project-id caller)))
  )
    ;; Check that unit count is greater than zero
    (asserts! (> unit-count u0) err-invalid-amount)
    
    ;; Check that there are enough units available
    (asserts! (>= available-units unit-count) err-funding-complete)
    
    ;; Transfer the STX from the investor to the project developer
    (asserts! (>= (stx-get-balance caller) total-investment) err-insufficient-funds)
    (try! (stx-transfer? total-investment caller project-developer))
    
    ;; Update the project's available units
    (map-set energy-projects
      { project-id: project-id }
      (merge project { available-units: (- available-units unit-count) })
    )
    
    ;; Update the investor's units
    (map-set investor-units
      { project-id: project-id, investor: caller }
      { units: (+ current-units unit-count) }
    )
    
    (ok true)
  )
)

(define-public (generate-credits (project-id uint) (amount uint))
  (let (
    (project (unwrap-panic (get-energy-project project-id)))
    (caller tx-sender)
    (project-developer (get developer project))
    (current-credits (get total-credits project))
    (validated-amount amount)
  )
    ;; Check that amount is greater than zero
    (asserts! (> validated-amount u0) err-invalid-amount)
    
    ;; Only the project developer can generate credits
    (asserts! (is-eq caller project-developer) err-unauthorized)
    
    ;; Transfer the STX from the caller to the contract
    (try! (stx-transfer? validated-amount caller (as-contract tx-sender)))
    
    ;; Update the project's total credits
    (map-set energy-projects
      { project-id: project-id }
      (merge project { total-credits: (+ current-credits validated-amount) })
    )
    
    ;; Create a new distribution
    (let (
      (distribution-id (var-get next-distribution-id))
    )
      (map-set credit-distributions
        { distribution-id: distribution-id }
        {
          project-id: project-id,
          amount: validated-amount,
          distribution-timestamp: u0,
          finalized: false
        }
      )
      
      ;; Increment the distribution ID counter
      (var-set next-distribution-id (+ distribution-id u1))
      
      (ok distribution-id)
    )
  )
)

(define-public (finalize-credit-distribution (distribution-id uint))
  (let (
    (distribution (unwrap-panic (get-credit-distribution distribution-id)))
    (project-id (get project-id distribution))
    (amount (get amount distribution))
    (finalized (get finalized distribution))
    (project (unwrap-panic (get-energy-project project-id)))
    (total-units (get total-units project))
    (platform-fee (/ (* amount (var-get platform-fee-rate)) u100))
    (distributable-amount (- amount platform-fee))
  )
    ;; Check that the distribution hasn't already been finalized
    (asserts! (not finalized) err-unauthorized)
    
    ;; Mark the distribution as finalized
    (map-set credit-distributions
      { distribution-id: distribution-id }
      (merge distribution { finalized: true })
    )
    
    (ok true)
  )
)

(define-public (claim-carbon-credits (project-id uint) (distribution-id uint))
  (let (
    (distribution (unwrap-panic (get-credit-distribution distribution-id)))
    (distribution-project-id (get project-id distribution))
    (amount (get amount distribution))
    (finalized (get finalized distribution))
    (project (unwrap-panic (get-energy-project project-id)))
    (total-units (get total-units project))
    (caller tx-sender)
    (investor-units (get units (get-investor-units project-id caller)))
    (platform-fee (/ (* amount (var-get platform-fee-rate)) u100))
    (distributable-amount (- amount platform-fee))
    (investor-credits (/ (* distributable-amount investor-units) total-units))
  )
    ;; Check that the project IDs match
    (asserts! (is-eq project-id distribution-project-id) err-project-not-found)
    
    ;; Check that the investor has units
    (asserts! (> investor-units u0) err-unauthorized)
    
    ;; Check that the distribution is finalized
    (asserts! finalized err-unauthorized)
    
    ;; Transfer the investor's share of the credits
    (try! (as-contract (stx-transfer? investor-credits tx-sender caller)))
    
    (ok investor-credits)
  )
)

(define-public (update-platform-fee (new-fee-rate uint))
  (begin
    ;; Only the contract manager can update the platform fee
    (asserts! (is-eq tx-sender (var-get contract-manager)) err-unauthorized)
    
    ;; Fee cannot be more than 8%
    (asserts! (&lt;= new-fee-rate u8) err-invalid-amount)
    
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)
