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

(defjsonrpc get-address ("get_address" &key account-index address-indexes)
  "Return the wallet's addresses for an account."
  (append (when account-index
            (list (cons "account_index" account-index)))
          (when address-indexes
            (list (cons "address_index" address-indexes)))))

(defjsonrpc get-address-book ("get_address_book" &key entries)
  "Retrieves entries from the address book."
  (when entries
    (list (cons "entries" (coerce entries 'vector)))))

(defjsonrpc get-balance ("get_balance" &key account-index)
  "Return the wallet's balance."
  (when account-index
    (list (cons "account_index" account-index))))

(defjsonrpc get-bulk-payments ("get_bulk_payments" payment-ids min-block-height)
  "Get a list of incoming payments."
  (list (cons "payment_ids" (coerce payment-ids 'vector))
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
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))))

(defjsonrpc get-tx-notes ("get_tx_notes" transaction-ids)
  "Get string notes for transactions."
  (list (cons "txids" (coerce transaction-ids 'vector))))

(defjsonrpc import-key-images ("import_key_images" signed-key-images)
  "Import signed key images list and verify their spent status."
  (list (cons "signed_key_images" (coerce signed-key-images 'vector))))

(defjsonrpc incoming-transfers ("incoming_transfers" transfer-type &key account-index subaddress-indices verbose)
  "Return a list of incoming transfers to the wallet."
  (append (list (cons "transfer_type" transfer-type))
          (when account-index
            (list (cons "account_index" account-index)))
          (when subaddress-indices
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))
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

(defjsonrpc make-uri ("make_uri" address &key amount payment-id recipient-name description)
  "Create a payment URI."
  (append (list (cons "address" address))
          (when amount
            (list (cons "amount" amount)))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when recipient-name
            (list (cons "recipient_name" recipient-name)))
          (when description
            (list (cons "tx_description" description)))))

(defjsonrpc open-wallet ("open_wallet" filename password)
  "Open a wallet."
  (list (cons "filename" filename)
        (cons "password" password)))

(defjsonrpc parse-uri ("parse_uri" uri)
  "Parse a payment URI to get payment information."
  (list (cons "uri" uri)))

(defjsonrpc query-key ("query_key" key-type)
  "Return the spend or view private key."
  (list (cons "key_type" key-type)))

(defjsonrpc rescan-blockchain ("rescan_blockchain")
  "Rescan blockchain from scratch.")

(defjsonrpc rescan-spent ("rescan_spent")
  "Rescan the blockchain for spent outputs.")

(defjsonrpc set-account-tag-description ("set_account_tag_description" tag description)
  "Set description for an account tag."
  (list (cons "tag" tag)
        (cons "description" description)))

(defjsonrpc set-tx-notes ("set_tx_notes" transaction-ids notes)
  "Set arbitrary string notes for transactions."
  (list (cons "txids" (coerce transaction-ids 'vector))
        (cons "notes" (coerce notes 'vector))))

(defjsonrpc sign ("sign" string)
  "Sign a string."
  (list (cons "data" string)))

(defjsonrpc split-integrated-address ("split_integrated_address" integrated-address)
  "Retrieve the standard address and payment id corresponding to an integrated
address."
  (list (cons "integrated_address" integrated-address)))

(defjsonrpc start-mining ("start_mining" thread-count background-mining ignore-battery)
  "Start mining in the Monero daemon."
  (list (cons "threads_count" thread-count)
        (cons "do_background_mining" background-mining)
        (cons "ignore_battery" ignore-battery)))

(defjsonrpc stop-mining ("stop_mining")
  "Stop mining in the Monero daemon.")

(defjsonrpc stop-wallet ("stop_wallet")
  "Stop the wallet, storing the current state.")

(defjsonrpc store ("store")
  "Save the current state.")

(defjsonrpc sweep-all ("sweep_all" address &key account-index subaddress-indices priority ring-size unlock-time payment-id get-transaction-keys below-amount do-not-relay get-transaction-hex get-transaction-metadata)
  "Send all unlocked balance to an address."
  (append (list (cons "address" address))
          (when account-index
            (cons "account_index" account-index))
          (when subaddress-indices
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))
          (when priority
            (list (cons "priority" priority)))
          (when ring-size
            (list (cons "ring_size" ring-size)))
          (when unlock-time
            (list (cons "unlock_time" unlock-time)))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when get-transaction-keys
            (list (cons "get_tx_keys" get-transaction-keys)))
          (when below-amount
            (list (cons "below_amount" below-amount)))
          (when do-not-relay
            (list (cons "do_not_relay" do-not-relay)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" get-transaction-hex)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" get-transaction-metadata)))))

(defjsonrpc sweep-dust ("sweep_dust")
  "Send all dust outputs back to the wallet.")

(defjsonrpc tag-accounts ("tag_accounts" tag accounts)
  "Apply a filtering tag to a list of accounts."
  (list (cons "tag" tag)
        (cons "accounts" (coerce accounts 'vector))))

(defjsonrpc transfer ("transfer" destinations &key account-index subaddress-indices priority ring-size unlock-time payment-id get-transaction-key do-not-relay get-transaction-hex get-transaction-metadata)
  "Send monero to a number of recipients."
  (append (list (cons "destinations" (coerce destinations 'vector)))
          (when account-index
            (cons "account_index" account-index))
          (when subaddress-indices
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))
          (when priority
            (list (cons "priority" priority)))
          (when ring-size
            (list (cons "ring_size" ring-size)))
          (when unlock-time
            (list (cons "unlock_time" unlock-time)))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when get-transaction-key
            (list (cons "get_tx_key" get-transaction-key)))
          (when do-not-relay
            (list (cons "do_not_relay" do-not-relay)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" get-transaction-hex)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" get-transaction-metadata)))))

(defjsonrpc transfer-split ("transfer_split" destinations &key account-index subaddress-indices priority ring-size unlock-time payment-id get-transaction-keys do-not-relay get-transaction-hex get-transaction-metadata)
  "Like transfer, but can split into several transactions if necessary."
  (append (list (cons "destinations" (coerce destinations 'vector)))
          (when account-index
            (cons "account_index" account-index))
          (when subaddress-indices
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))
          (when priority
            (list (cons "priority" priority)))
          (when ring-size
            (list (cons "ring_size" ring-size)))
          (when unlock-time
            (list (cons "unlock_time" unlock-time)))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when get-transaction-keys
            (list (cons "get_tx_keys" get-transaction-keys)))
          (when do-not-relay
            (list (cons "do_not_relay" do-not-relay)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" get-transaction-hex)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" get-transaction-metadata)))))

(defjsonrpc untag-accounts ("untag_accounts" accounts)
  "Remove filtering tag from a list of accounts."
  (list (cons "accounts" (coerce accounts 'vector))))

(defjsonrpc verify ("verify" string address signature)
  "Verify a signature on a string."
  (list (cons "string" string)
        (cons "address" address)
        (cons "signature" signature)))
