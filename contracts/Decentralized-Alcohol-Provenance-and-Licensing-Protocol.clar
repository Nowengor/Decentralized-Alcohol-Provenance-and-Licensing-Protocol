(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-age (err u104))
(define-constant err-age-not-verified (err u105))
(define-constant err-customer-not-found (err u106))
(define-constant err-batch-recalled (err u107))

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

(define-map customer-age-verification
    principal
    {
        birth-year: uint,
        verification-date: uint,
        is-verified: bool,
        verifier: principal
    }
)

(define-map sales-records
    uint
    {
        token-id: uint,
        customer: principal,
        distributor: principal,
        sale-date: uint,
        age-verified: bool
    }
)

(define-map batch-recalls
    (string-ascii 32)
    {
        recalled: bool,
        recall-date: uint,
        reason: (string-ascii 128)
    }
)

(define-data-var token-id-nonce uint u0)
(define-data-var sale-id-nonce uint u0)

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

(define-public (register-distributor
    (license-id (string-ascii 32))
    (regions (list 10 (string-ascii 32)))
    (expiry uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set distributor-licenses tx-sender
            {
                license-id: license-id,
                regions: regions,
                expiry: expiry,
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
        ((bottle (unwrap! (map-get? bottle-details token-id) err-not-found))
         (batch-recall (map-get? batch-recalls (get batch-id bottle))))
        (asserts! (is-eq (get current-owner bottle) tx-sender) err-unauthorized)
        (asserts! (is-none batch-recall) err-batch-recalled)
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

(define-public (verify-customer-age
    (customer principal)
    (birth-year uint))
    (let
        ((current-block burn-block-height)
         (minimum-age u21)
         (current-year u2024)
         (calculated-age (- current-year birth-year)))
        (asserts! (>= calculated-age minimum-age) err-invalid-age)
        (ok (map-set customer-age-verification customer
            {
                birth-year: birth-year,
                verification-date: current-block,
                is-verified: true,
                verifier: tx-sender
            }
        ))
    )
)

(define-public (record-alcohol-sale
    (token-id uint)
    (customer principal))
    (let
        ((bottle (unwrap! (map-get? bottle-details token-id) err-not-found))
         (customer-verification (unwrap! (map-get? customer-age-verification customer) err-customer-not-found))
         (batch-recall (map-get? batch-recalls (get batch-id bottle)))
         (sale-id (var-get sale-id-nonce)))
        (asserts! (is-eq (get current-owner bottle) tx-sender) err-unauthorized)
        (asserts! (get is-verified customer-verification) err-age-not-verified)
        (asserts! (is-none batch-recall) err-batch-recalled)
        (map-set sales-records sale-id
            {
                token-id: token-id,
                customer: customer,
                distributor: tx-sender,
                sale-date: burn-block-height,
                age-verified: true
            }
        )
        (var-set sale-id-nonce (+ sale-id u1))
        (ok sale-id)
    )
)

(define-read-only (get-customer-verification (customer principal))
    (ok (unwrap! (map-get? customer-age-verification customer) err-customer-not-found))
)

(define-read-only (get-sale-record (sale-id uint))
    (ok (unwrap! (map-get? sales-records sale-id) err-not-found))
)

(define-read-only (check-customer-age-eligibility (customer principal))
    (match (map-get? customer-age-verification customer)
        verification (ok (and
            (get is-verified verification)
            (>= (- u2024 (get birth-year verification)) u21)))
        err-customer-not-found
    )
)

(define-public (initiate-batch-recall
    (batch-id (string-ascii 32))
    (reason (string-ascii 128)))
    (let
        ((producer-license (unwrap! (map-get? producer-licenses tx-sender) err-unauthorized)))
        (asserts! (get status producer-license) err-unauthorized)
        (asserts! (< burn-block-height (get expiry producer-license)) err-unauthorized)
        (ok (map-set batch-recalls batch-id
            {
                recalled: true,
                recall-date: burn-block-height,
                reason: reason
            }
        ))
    )
)

(define-read-only (get-batch-recall-status (batch-id (string-ascii 32)))
    (ok (unwrap! (map-get? batch-recalls batch-id) err-not-found))
)
