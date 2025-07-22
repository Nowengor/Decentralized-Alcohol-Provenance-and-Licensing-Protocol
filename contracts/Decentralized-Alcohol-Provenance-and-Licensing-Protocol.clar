(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-age (err u104))

(define-non-fungible-token alcohol-token uint)

(define-map bottle-details
    uint 
    {
        producer: principal,
        batch-id: (string-ascii 32),
        product-type: (string-ascii 32),
        production-date: uint,
        ipfs-metadata: (string-ascii 64),
        current-owner: principal,
        is-verified: bool
    }
)

(define-map producer-licenses
    principal
    {
        license-id: (string-ascii 32),
        expiry: uint,
        jurisdictions: (list 10 (string-ascii 32)),
        status: bool
    }
)

(define-map distributor-licenses
    principal
    {
        license-id: (string-ascii 32),
        regions: (list 10 (string-ascii 32)),
        expiry: uint,
        status: bool
    }
)

(define-data-var token-id-nonce uint u0)

(define-public (register-producer 
    (license-id (string-ascii 32))
    (jurisdictions (list 10 (string-ascii 32)))
    (expiry uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set producer-licenses tx-sender
            {
                license-id: license-id,
                expiry: expiry,
                jurisdictions: jurisdictions,
                status: true
            }
        ))
    )
)

(define-public (mint-bottle 
    (batch-id (string-ascii 32))
    (product-type (string-ascii 32))
    (ipfs-metadata (string-ascii 64)))
    (let
        ((producer-license (unwrap! (map-get? producer-licenses tx-sender) err-unauthorized))
         (token-id (var-get token-id-nonce)))
        (asserts! (get status producer-license) err-unauthorized)
        (asserts! (< burn-block-height (get expiry producer-license)) err-unauthorized)
        (try! (nft-mint? alcohol-token token-id tx-sender))
        (map-set bottle-details token-id
            {
                producer: tx-sender,
                batch-id: batch-id,
                product-type: product-type,
                production-date: burn-block-height,
                ipfs-metadata: ipfs-metadata,
                current-owner: tx-sender,
                is-verified: true
            }
        )
        (var-set token-id-nonce (+ token-id u1))
        (ok token-id)
    )
)

(define-public (transfer-bottle 
    (token-id uint)
    (recipient principal))
    (let
        ((bottle (unwrap! (map-get? bottle-details token-id) err-not-found)))
        (asserts! (is-eq (get current-owner bottle) tx-sender) err-unauthorized)
        (try! (nft-transfer? alcohol-token token-id tx-sender recipient))
        (ok (map-set bottle-details token-id
            (merge bottle { current-owner: recipient })))
    )
)

(define-read-only (get-bottle-details (token-id uint))
    (ok (unwrap! (map-get? bottle-details token-id) err-not-found))
)

(define-read-only (get-producer-license (producer principal))
    (ok (unwrap! (map-get? producer-licenses producer) err-not-found))
)

(define-public (update-metadata
    (token-id uint)
    (new-metadata (string-ascii 64)))
    (let
        ((bottle (unwrap! (map-get? bottle-details token-id) err-not-found)))
        (asserts! (is-eq (get producer bottle) tx-sender) err-unauthorized)
        (ok (map-set bottle-details token-id
            (merge bottle { ipfs-metadata: new-metadata })))
    )
)

(define-public (verify-bottle
    (token-id uint))
    (let
        ((bottle (unwrap! (map-get? bottle-details token-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set bottle-details token-id
            (merge bottle { is-verified: true })))
    )
)