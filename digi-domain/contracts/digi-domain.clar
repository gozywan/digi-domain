;; DigiDomain - Decentralized Domain Name System for Web3
;; A hierarchical namespace system with cross-chain domain portability

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-name (err u104))
(define-constant err-insufficient-stake (err u105))
(define-constant err-domain-expired (err u106))

;; Minimum stake required for domain registration (in microSTX)
(define-constant min-stake-amount u1000000)

;; Domain expiry period (in blocks, ~1 year assuming 10 min blocks)
(define-constant domain-expiry-period u52560)

;; Data Variables
(define-data-var total-domains uint u0)

;; Data Maps

;; Primary domain registry
(define-map domains
    { name: (string-ascii 64) }
    {
        owner: principal,
        registered-at: uint,
        expires-at: uint,
        staked-amount: uint,
        usage-count: uint,
        metadata-uri: (optional (string-utf8 256))
    }
)

;; Multi-chain resolution mapping
(define-map chain-resolutions
    { domain-name: (string-ascii 64), chain-identifier: (string-ascii 32) }
    { 
        resolved-address: (string-ascii 128),
        contract-address: (optional (string-ascii 128)),
        updated-at: uint
    }
)

;; Subdomain registry for hierarchical namespace
(define-map subdomains
    { parent-domain: (string-ascii 64), subdomain: (string-ascii 64) }
    {
        owner: principal,
        resolver: (string-ascii 128),
        created-at: uint
    }
)

;; Domain usage tracking for Proof of Utility
(define-map domain-activity
    { name: (string-ascii 64) }
    {
        last-accessed: uint,
        total-resolutions: uint,
        active-chains: (list 10 (string-ascii 32))
    }
)

;; Read-only functions

(define-read-only (get-domain-info (name (string-ascii 64)))
    (map-get? domains { name: name })
)

(define-read-only (get-chain-resolution (domain-name (string-ascii 64)) (chain-identifier (string-ascii 32)))
    (map-get? chain-resolutions { domain-name: domain-name, chain-identifier: chain-identifier })
)

(define-read-only (get-subdomain-info (parent-domain (string-ascii 64)) (subdomain (string-ascii 64)))
    (map-get? subdomains { parent-domain: parent-domain, subdomain: subdomain })
)

(define-read-only (get-domain-activity (name (string-ascii 64)))
    (map-get? domain-activity { name: name })
)

(define-read-only (is-domain-active (name (string-ascii 64)))
    (match (map-get? domains { name: name })
        domain-data (< block-height (get expires-at domain-data))
        false
    )
)

(define-read-only (get-total-domains)
    (ok (var-get total-domains))
)

;; Private functions

(define-private (is-valid-domain-name (name (string-ascii 64)))
    (and
        (> (len name) u0)
        (<= (len name) u64)
    )
)

;; Public functions

;; Register a new domain
(define-public (register-domain (name (string-ascii 64)) (metadata-uri (optional (string-utf8 256))))
    (let
        (
            (existing-domain (map-get? domains { name: name }))
            (stake-amount min-stake-amount)
        )
        (asserts! (is-valid-domain-name name) err-invalid-name)
        (asserts! (is-none existing-domain) err-already-exists)
        
        ;; Transfer stake from user
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        ;; Register domain
        (map-set domains
            { name: name }
            {
                owner: tx-sender,
                registered-at: block-height,
                expires-at: (+ block-height domain-expiry-period),
                staked-amount: stake-amount,
                usage-count: u0,
                metadata-uri: metadata-uri
            }
        )
        
        ;; Initialize activity tracking
        (map-set domain-activity
            { name: name }
            {
                last-accessed: block-height,
                total-resolutions: u0,
                active-chains: (list)
            }
        )
        
        ;; Increment total domains
        (var-set total-domains (+ (var-get total-domains) u1))
        
        (ok true)
    )
)

;; Set multi-chain resolution
(define-public (set-chain-resolution 
    (domain-name (string-ascii 64)) 
    (chain-identifier (string-ascii 32))
    (resolved-address (string-ascii 128))
    (contract-address (optional (string-ascii 128))))
    (let
        (
            (domain-data (unwrap! (map-get? domains { name: domain-name }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        (asserts! (< block-height (get expires-at domain-data)) err-domain-expired)
        
        ;; Set chain resolution
        (map-set chain-resolutions
            { domain-name: domain-name, chain-identifier: chain-identifier }
            {
                resolved-address: resolved-address,
                contract-address: contract-address,
                updated-at: block-height
            }
        )
        
        ;; Update usage count
        (map-set domains
            { name: domain-name }
            (merge domain-data { usage-count: (+ (get usage-count domain-data) u1) })
        )
        
        (ok true)
    )
)

;; Register a subdomain
(define-public (register-subdomain 
    (parent-domain (string-ascii 64))
    (subdomain (string-ascii 64))
    (resolver (string-ascii 128)))
    (let
        (
            (parent-data (unwrap! (map-get? domains { name: parent-domain }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner parent-data)) err-unauthorized)
        (asserts! (is-valid-domain-name subdomain) err-invalid-name)
        (asserts! (< block-height (get expires-at parent-data)) err-domain-expired)
        
        (map-set subdomains
            { parent-domain: parent-domain, subdomain: subdomain }
            {
                owner: tx-sender,
                resolver: resolver,
                created-at: block-height
            }
        )
        
        (ok true)
    )
)

;; Renew domain registration
(define-public (renew-domain (name (string-ascii 64)))
    (let
        (
            (domain-data (unwrap! (map-get? domains { name: name }) err-not-found))
            (additional-stake min-stake-amount)
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        
        ;; Transfer additional stake
        (try! (stx-transfer? additional-stake tx-sender (as-contract tx-sender)))
        
        ;; Extend expiration
        (map-set domains
            { name: name }
            (merge domain-data 
                {
                    expires-at: (+ (get expires-at domain-data) domain-expiry-period),
                    staked-amount: (+ (get staked-amount domain-data) additional-stake)
                }
            )
        )
        
        (ok true)
    )
)

;; Transfer domain ownership
(define-public (transfer-domain (name (string-ascii 64)) (new-owner principal))
    (let
        (
            (domain-data (unwrap! (map-get? domains { name: name }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        (asserts! (< block-height (get expires-at domain-data)) err-domain-expired)
        
        (map-set domains
            { name: name }
            (merge domain-data { owner: new-owner })
        )
        
        (ok true)
    )
)

;; Update domain metadata
(define-public (update-metadata (name (string-ascii 64)) (metadata-uri (string-utf8 256)))
    (let
        (
            (domain-data (unwrap! (map-get? domains { name: name }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner domain-data)) err-unauthorized)
        (asserts! (< block-height (get expires-at domain-data)) err-domain-expired)
        
        (map-set domains
            { name: name }
            (merge domain-data { metadata-uri: (some metadata-uri) })
        )
        
        (ok true)
    )
)

;; Record domain resolution (for Proof of Utility tracking)
(define-public (record-resolution (name (string-ascii 64)) (chain-identifier (string-ascii 32)))
    (let
        (
            (domain-data (unwrap! (map-get? domains { name: name }) err-not-found))
            (activity-data (default-to 
                { last-accessed: u0, total-resolutions: u0, active-chains: (list) }
                (map-get? domain-activity { name: name })
            ))
        )
        (asserts! (< block-height (get expires-at domain-data)) err-domain-expired)
        
        (map-set domain-activity
            { name: name }
            {
                last-accessed: block-height,
                total-resolutions: (+ (get total-resolutions activity-data) u1),
                active-chains: (get active-chains activity-data)
            }
        )
        
        (ok true)
    )
)