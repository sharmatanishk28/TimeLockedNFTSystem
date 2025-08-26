;; TimeLocked NFT System Contract
;; NFTs that reveal content or unlock features after specific time periods or conditions are met

;; Define the NFT
(define-non-fungible-token timelocked-nft uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-nft-not-found (err u102))
(define-constant err-time-locked (err u103))
(define-constant err-invalid-unlock-time (err u104))

;; Data variables
(define-data-var nft-counter uint u0)

;; NFT metadata and time-lock information
(define-map nft-metadata
  uint
  {
    owner: principal,
    locked-content: (string-ascii 256),
    unlock-time: uint,
    is-unlocked: bool,
    mint-time: uint
  })

;; Public metadata (visible before unlock)
(define-map nft-public-data
  uint
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    preview-image: (string-ascii 256)
  })

;; Function 1: Mint TimeLocked NFT
;; Mints a new NFT with locked content that will be revealed after a specific time
(define-public (mint-timelocked-nft 
  (recipient principal)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (preview-image (string-ascii 256))
  (locked-content (string-ascii 256))
  (unlock-delay-blocks uint))
  (let 
    (
      (nft-id (+ (var-get nft-counter) u1))
      (current-block-height stacks-block-height)
      (unlock-time (+ current-block-height unlock-delay-blocks))
    )
    (begin
      ;; Validate inputs
      (asserts! (> unlock-delay-blocks u0) err-invalid-unlock-time)
      
      ;; Mint the NFT
      (try! (nft-mint? timelocked-nft nft-id recipient))
      
      ;; Store NFT metadata with time-lock information
      (map-set nft-metadata nft-id
        {
          owner: recipient,
          locked-content: locked-content,
          unlock-time: unlock-time,
          is-unlocked: false,
          mint-time: current-block-height
        })
      
      ;; Store public data (always visible)
      (map-set nft-public-data nft-id
        {
          name: name,
          description: description,
          preview-image: preview-image
        })
      
      ;; Increment counter
      (var-set nft-counter nft-id)
      
      ;; Return success with NFT ID and unlock time
      (ok {nft-id: nft-id, unlock-time: unlock-time}))))

;; Function 2: Unlock NFT Content
;; Reveals the locked content if the time condition is met
(define-public (unlock-nft-content (nft-id uint))
  (let 
    (
      (nft-data (unwrap! (map-get? nft-metadata nft-id) err-nft-not-found))
      (nft-owner (get owner nft-data))
      (unlock-time (get unlock-time nft-data))
      (is-unlocked (get is-unlocked nft-data))
      (locked-content (get locked-content nft-data))
    )
    (begin
      ;; Check if caller is the NFT owner
      (asserts! (is-eq tx-sender nft-owner) err-not-authorized)
      
      ;; Check if enough time has passed
      (asserts! (>= stacks-block-height unlock-time) err-time-locked)
      
      ;; Update the unlock status
      (map-set nft-metadata nft-id
        (merge nft-data {is-unlocked: true}))
      
      ;; Return the unlocked content
      (ok {
        nft-id: nft-id,
        unlocked-content: locked-content,
        unlock-time: unlock-time,
        current-block: stacks-block-height
      }))))

;; Read-only functions for querying NFT information

;; Get public NFT data (always accessible)
(define-read-only (get-nft-public-info (nft-id uint))
  (ok (map-get? nft-public-data nft-id)))

;; Get NFT lock status and timing info
(define-read-only (get-nft-lock-info (nft-id uint))
  (match (map-get? nft-metadata nft-id)
    nft-data (ok {
      unlock-time: (get unlock-time nft-data),
      is-unlocked: (get is-unlocked nft-data),
      mint-time: (get mint-time nft-data),
      blocks-remaining: (if (>= stacks-block-height (get unlock-time nft-data))
                         u0
                         (- (get unlock-time nft-data) stacks-block-height))
    })
    err-nft-not-found))

;; Get unlocked content (only if unlocked and caller is owner)
(define-read-only (get-unlocked-content (nft-id uint))
  (match (map-get? nft-metadata nft-id)
    nft-data (if (and (is-eq tx-sender (get owner nft-data)) (get is-unlocked nft-data))
               (ok (some (get locked-content nft-data)))
               (ok none))
    err-nft-not-found))

;; Get total NFT count
(define-read-only (get-total-nfts)
  (ok (var-get nft-counter)))
  stacks-block-height
