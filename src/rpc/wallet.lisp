;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-rpc)


;;; Specs in https://getmonero.org/resources/developer-guides/wallet-rpc.html


;;; HTTP JSON RPCs

(defjsonrpc add-address-book ("add_address_book" address &key payment-id description)
  "Add an entry to the address book."
  (append (list (cons "address" address))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when description
            (list (cons "description" description)))))

(defjsonrpc create-account ("create_account" &key label)
  "Create a new account."
  (when label
    (list (cons "label" label))))

(defjsonrpc create-address ("create_address" account-index &key label)
  "Create a new address for an account."
  (append (list (cons "account_index" account-index))
          (when label
            (list (cons "label" label)))))

(defjsonrpc create-wallet ("create_wallet" filename password language)
  "Create a new wallet."
  (list (cons "filename" filename)
        (cons "password" password)
        (cons "language" language)))

(defjsonrpc delete-address-book ("delete_address_book" index)
  "Delete an entry from the address book."
  (list (cons "index" index)))

(defjsonrpc export-key-images ("export_key_images")
  "Export a signed set of key images.")

(defjsonrpc get-account-tags ("get_account_tags")
  "Get a list of user-defined account tags.")

(defjsonrpc get-accounts ("get_accounts" &key tag)
  "Get all accounts for a wallet."
  (when tag
    (list (cons "tag" tag))))

(defjsonrpc get-address ("get_address" account-index &key address-indexes)
  "Return the wallet's addresses for an account."
  (append (list (cons "account_index" account-index))
          (when address-indexes
            (list (cons "address_index" address-indexes)))))

(defjsonrpc get-address-book ("get_address_book" entries)
  "Retrieves entries from the address book."
  (list (cons "entries" entries)))

(defjsonrpc get-balance ("get_balance" account-index)
  "Return the wallet's balance."
  (list (cons "account_index" account-index)))

(defjsonrpc get-bulk-payments ("get_bulk_payments" payment-ids min-block-height)
  "Get a list of incoming payments."
  (list (cons "payment_ids" payment-ids)
        (cons "min_block_height" min-block-height)))

(defjsonrpc get-height ("get_height")
  "Returns the wallet's current block height.")

(defjsonrpc get-languages ("get_languages")
  "Get a list of available languages for your wallet's seed.")

(defjsonrpc get-payments ("get_payments" payment-id)
  "Get a list of incoming payments."
  (list (cons "payment_id" payment-id)))

(defjsonrpc get-transfer-by-txid ("get_transfer_by_txid" transaction-id &key account-index)
  "Show information about a transfer."
  (append (list (cons "txid" transaction-id))
          (when account-index
            (list (cons "account_index" account-index)))))

(defjsonrpc get-transfers ("get_transfers" &key incoming outgoing pending failed pool filter-by-height min-height max-height account-index subaddress-indices)
  "Returns a list of transfers."
  (append (when incoming
            (list (cons "in" t)))
          (when outgoing
            (list (cons "out" t)))
          (when pending
            (list (cons "pending" t)))
          (when failed
            (list (cons "failed" t)))
          (when pool
            (list (cons "pool" t)))
          (when filter-by-height
            (list (cons "filter_by_height" t)))
          (when min-height
            (list (cons "min_height" min-height)))
          (when max-height
            (list (cons "max_height" max-height)))
          (when account-index
            (list (cons "account_index" account-index)))
          (when subaddress-indices
            (list (cons "subaddr_indices" subaddress-indices)))))

(defjsonrpc get-tx-notes ("get_tx_notes" transaction-ids)
  "Get string notes for transactions."
  (list (cons "txids" transaction-ids)))

(defjsonrpc import-key-images ("import_key_images" signed-key-images)
  "Import signed key images list and verify their spent status."
  (list (cons "signed_key_images" signed-key-images)))

(defjsonrpc incoming-transfers ("incoming_transfers" transfer-type &key account-index subaddress-indices verbose)
  "Return a list of incoming transfers to the wallet."
  (append (list (cons "transfer_type" transfer-type))
          (when account-index
            (list (cons "account_index" account-index)))
          (when subaddress-indices
            (list (cons "subaddr_indices" subaddress-indices)))
          (when verbose
            (list (cons "verbose" t)))))

(defjsonrpc label-account ("label_account" account-index label)
  "Label an account."
  (list (cons "account_index" account-index)
        (cons "label" label)))

(defjsonrpc label-address ("label_address" account-index address-index label)
  "Label an address."
  (list (cons "index" (list (cons "major" account-index)
                            (cons "minor" address-index)))
        (cons "label" label)))

(defjsonrpc make-integrated-address ("make_integrated_address" payment-id)
  "Make an integrated address from the wallet address and a payment id."
  (list (cons "payment_id" payment-id)))

#|
    make_uri
    open_wallet
    parse_uri
    query_key
    rescan_blockchain
    rescan_spent
    set_account_tag_description
    set_tx_notes
    sign
    split_integrated_address
    start_mining
    stop_mining
    stop_wallet
    store
    sweep_all
    sweep_dust
    tag_accounts
    transfer
    transfer_split
    untag_accounts
    verify
|#
