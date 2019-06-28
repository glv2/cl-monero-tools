;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-wallet-rpc)


;;; Specs in https://getmonero.org/resources/developer-guides/wallet-rpc.html


;;; HTTP JSON RPCs

(defjsonrpc add-address-book ("add_address_book" address &key payment-id description)
  "Add an entry to the address book."
  (append (list (cons "address" address))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when description
            (list (cons "description" description)))))

(defjsonrpc change-wallet-password ("change_wallet_password" &key old-password new-password)
  "Change a wallet password."
  (append (when old-password
            (list (cons "old_password" old-password)))
          (when new-password
            (list (cons "new_password" new-password)))))

(defjsonrpc check-reserve-proof ("check_reserve_proof" address signature &key message)
  "Proves a wallet as a disposable reserve using a signature."
  (append (list (cons "address" address)
                (cons "signature" signature))
          (when message
            (list (cons "message" message)))))

(defjsonrpc check-spend-proof ("check_spend_proof" transaction-id signature &key message)
  "Prove a spend using a signature."
  (append (list (cons "txid" transaction-id)
                (cons "signature" signature))
          (when message
            (list (cons "message" message)))))

(defjsonrpc check-tx-key ("check_tx_key" transaction-id transaction-secret-key address)
  "Check a transaction in the blockchain with its secret key."
  (list (cons "txid" transaction-id)
        (cons "tx_key" transaction-secret-key)
        (cons "address" address)))

(defjsonrpc check-tx-proof ("check_tx_proof" transaction-id address signature &key message)
  "Prove a transaction by checking its signature."
  (append (list (cons "txid" transaction-id)
                (cons "address" address)
                (cons "signature" signature))
          (when message
            (list (cons "message" message)))))

(defjsonrpc close-wallet ("close_wallet")
  "Close the current wallet.")

(defjsonrpc create-account ("create_account" &key label)
  "Create a new account."
  (when label
    (list (cons "label" label))))

(defjsonrpc create-address ("create_address" account-index &key label)
  "Create a new address for an account."
  (append (list (cons "account_index" account-index))
          (when label
            (list (cons "label" label)))))

(defjsonrpc create-wallet ("create_wallet" filename language &key password)
  "Create a new wallet."
  (append (list (cons "filename" filename)
                (cons "language" language))
          (when password
            (list (cons "password" password)))))

(defjsonrpc delete-address-book ("delete_address_book" index)
  "Delete an entry from the address book."
  (list (cons "index" index)))

(defjsonrpc describe-transfer ("describe_transfer" &key unsigned-transaction-set multisig-transaction-set)
  "Describe the transfers encoded in a string in hex format."
  (append (when unsigned-transaction-set
            (list (cons "unsigned_txset" unsigned-transaction-set)))
          (when multisig-transaction-set
            (list (cons "multisig_txset" multisig-transaction-set)))))

(defjsonrpc export-key-images ("export_key_images" &key all)
  "Export a signed set of key images."
  (when all
    (list (cons "all" t))))

(defjsonrpc export-multisig-info ("export_multisig_info")
  "Export multisig info for other participants.")

(defjsonrpc export-outputs ("export_outputs" &key all)
  "Export all outputs in hex format."
  (when all
    (list (cons "all" t))))

(defjsonrpc finalize-multisig ("finalize_multisig" multisig-info password)
  "Turn this wallet into a multisig wallet, extra step for M/N wallets."
  (list (cons "multisig_info" (coerce multisig-info 'vector))
        (cons "password" password)))

(defjsonrpc get-account-tags ("get_account_tags")
  "Get a list of user-defined account tags.")

(defjsonrpc get-accounts ("get_accounts" &key tag)
  "Get all accounts for a wallet."
  (when tag
    (list (cons "tag" tag))))

(defjsonrpc get-address ("get_address" &key account-index address-indices)
  "Return the wallet's addresses for an account."
  (append (when account-index
            (list (cons "account_index" account-index)))
          (when address-indices
            (list (cons "address_index" (coerce address-indices 'vector))))))

(defjsonrpc get-address-book ("get_address_book" &key entries)
  "Retrieves entries from the address book."
  (when entries
    (list (cons "entries" (coerce entries 'vector)))))

(defjsonrpc get-address-index ("get_address_index" address)
  "Get account and address indexes from a specific (sub)address."
  (list (cons "address" address)))

(defjsonrpc get-attribute ("get_attribute" key)
  "Get attribute value by name."
  (list (cons "key" key)))

(defjsonrpc get-balance ("get_balance" &key account-index address-indices all-accounts)
  "Return the wallet's balance."
  (append (when account-index
            (list (cons "account_index" account-index)))
          (when address-indices
            (list (cons "address_indices" (coerce address-indices 'vector))))
          (when all-accounts
            (list (cons "all_accounts" t)))))

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

(defjsonrpc get-reserve-proof ("get_reserve_proof" all &key account-index amount message)
  "Generate a signature to prove of an available amount in a wallet."
  (append (list (cons "all" all))
          (when account-index
            (list (cons "account_index" account-index)))
          (when amount
            (list (cons "amount" amount)))
          (when account-index
            (list (cons "message" message)))))

(defjsonrpc get-spend-proof ("get_spend_proof" transaction-id &key message)
  "Generate a signature to prove a spend."
  (append (list (cons "txid" transaction-id))
          (when message
            (list (cons "message" message)))))

(defjsonrpc get-transfer-by-txid ("get_transfer_by_txid" transaction-id &key account-index)
  "Show information about a transfer."
  (append (list (cons "txid" transaction-id))
          (when account-index
            (list (cons "account_index" account-index)))))

(defjsonrpc get-transfers ("get_transfers" &key incoming outgoing pending failed pool filter-by-height min-height max-height account-index subaddress-indices all-accounts)
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
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))
          (when all-accounts
            (list (cons "all_accounts" t)))))

(defjsonrpc get-tx-key ("get_tx_key" transaction-id)
  "Get transaction secret key from transaction id."
  (list (cons "txid" transaction-id)))

(defjsonrpc get-tx-notes ("get_tx_notes" transaction-ids)
  "Get string notes for transactions."
  (list (cons "txids" (coerce transaction-ids 'vector))))

(defjsonrpc get-tx-proof ("get_tx_proof" transaction-id address &key message)
  "Get transaction signature to prove it."
  (append (list (cons "txid" transaction-id)
                (cons "address" address))
          (when message
            (list (cons "message" message)))))

(defjsonrpc get-version ("get_version")
  "Get RPC version, where the major version number is the first 16 bits and the
  minor version number is the last 16 bits.")

(defjsonrpc import-key-images ("import_key_images" signed-key-images &key offset)
  "Import signed key images list and verify their spent status."
  (append (list (cons "signed_key_images" (coerce signed-key-images 'vector)))
          (when offset
            (list (cons "offset" offset)))))

(defjsonrpc import-multisig-info ("import_multisig_info" multisig-info)
  "Import multisig info from other participants."
  (list (cons "info" multisig-info)))

(defjsonrpc import-outputs ("import_outputs" outputs-data-hex)
  "Import outputs in hex format."
  (list (cons "outputs_data_hex" outputs-data-hex)))

(defjsonrpc incoming-transfers ("incoming_transfers" transfer-type &key account-index subaddress-indices)
  "Return a list of incoming transfers to the wallet."
  (append (list (cons "transfer_type" transfer-type))
          (when account-index
            (list (cons "account_index" account-index)))
          (when subaddress-indices
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))))

(defjsonrpc is-multisig ("is_multisig")
  "Check if a wallet is a multisig one.")

(defjsonrpc label-account ("label_account" account-index label)
  "Label an account."
  (list (cons "account_index" account-index)
        (cons "label" label)))

(defjsonrpc label-address ("label_address" account-index address-index label)
  "Label an address."
  (list (cons "index" (list (cons "major" account-index)
                            (cons "minor" address-index)))
        (cons "label" label)))

(defjsonrpc make-integrated-address ("make_integrated_address" &key standard-address payment-id)
  "Make an integrated address from the wallet address and a payment id."
  (append (when standard-address
            (list (cons "standard_address" standard-address)))
          (when payment-id
            (list (cons "payment_id" payment-id)))))

(defjsonrpc make-multisig ("make_multisig" multisig-info threshold password)
  "Make a wallet multisig by importing peers multisig string."
  (list (cons "multisig_info" multisig-info)
        (cons "threshold" threshold)
        (cons "password" password)))

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

(defjsonrpc open-wallet ("open_wallet" filename &key password)
  "Open a wallet."
  (append (list (cons "filename" filename))
          (when password
            (list (cons "password" password)))))

(defjsonrpc parse-uri ("parse_uri" uri)
  "Parse a payment URI to get payment information."
  (list (cons "uri" uri)))

(defjsonrpc prepare-multisig ("prepare_multisig")
  "Prepare a wallet for multisig by generating a multisig string to share with
  peers.")

(defjsonrpc query-key ("query_key" key-type)
  "Return the spend or view private key."
  (list (cons "key_type" key-type)))

(defjsonrpc refresh ("refresh" &key start-height)
  "Refresh a wallet after openning."
  (when start-height
    (list (cons "start_height" start-height))))

(defjsonrpc relay-tx ("relay_tx" transaction-data-hex)
  "Relay a transaction previously created with DO-NOT-RELAY."
  (list (cons "hex" transaction-data-hex)))

(defjsonrpc rescan-blockchain ("rescan_blockchain" &key hard)
  "Rescan blockchain from scratch."
  (when hard
    (list (cons "hard" t))))

(defjsonrpc rescan-spent ("rescan_spent")
  "Rescan the blockchain for spent outputs.")

(defjsonrpc set-account-tag-description ("set_account_tag_description" tag description)
  "Set description for an account tag."
  (list (cons "tag" tag)
        (cons "description" description)))

(defjsonrpc set-attribute ("set_attribute" key value)
  "Set arbitrary attribute."
  (list (cons "key" key)
        (cons "value" value)))

(defjsonrpc set-tx-notes ("set_tx_notes" transaction-ids notes)
 "Set arbitrary string notes for transactions."
  (list (cons "txids" (coerce transaction-ids 'vector))
        (cons "notes" (coerce notes 'vector))))

(defjsonrpc sign ("sign" string)
  "Sign a string."
  (list (cons "data" string)))

(defjsonrpc sign-multisig ("sign_multisig" transaction-data-hex)
  "Sign a transaction in multisig."
  (list (cons "tx_data_hex" transaction-data-hex)))

(defjsonrpc sign-transfer ("sign_transfer" unsigned-transaction-set &key export-raw get-transaction-keys)
  "Sign a transaction created on a read-only wallet (in cold-signing process)."
  (append (list (cons "unsigned_txset" unsigned-transaction-set))
          (when export-raw
            (list (cons "export_raw" t)))
          (when get-transaction-keys
            (list (cons "get_tx_keys" t)))))

(defjsonrpc split-integrated-address ("split_integrated_address" integrated-address)
  "Retrieve the standard address and payment id corresponding to an integrated
address."
  (list (cons "integrated_address" integrated-address)))

(defjsonrpc start-mining ("start_mining" thread-count background-mining ignore-battery)
  "Start mining in the Monero daemon."
  (list (cons "threads_count" thread-count)
        (cons "do_background_mining" (when background-mining t))
        (cons "ignore_battery" (when ignore-battery t))))

(defjsonrpc stop-mining ("stop_mining")
  "Stop mining in the Monero daemon.")

(defjsonrpc stop-wallet ("stop_wallet")
  "Stop the wallet, storing the current state.")

(defjsonrpc store ("store")
  "Save the current state.")

(defjsonrpc submit-multisig ("submit_multisig" transaction-data-hex)
  "Submit a signed multisig transaction."
  (list (cons "tx_data_hex" transaction-data-hex)))

(defjsonrpc submit-transfer ("submit_transfer" transaction-data-hex)
  "Submit a previously signed transaction on a read-only wallet (in cold-signing
process)."
  (list (cons "tx_data_hex" transaction-data-hex)))

(defjsonrpc sweep-all ("sweep_all" address &key account-index subaddress-indices priority ring-size outputs unlock-time payment-id get-transaction-keys below-amount do-not-relay get-transaction-hex get-transaction-metadata)
  "Send all unlocked balance to an address."
  (append (list (cons "address" address))
          (when account-index
            (list (cons "account_index" account-index)))
          (when subaddress-indices
            (list (cons "subaddr_indices" (coerce subaddress-indices 'vector))))
          (when priority
            (list (cons "priority" priority)))
          (when ring-size
            (list (cons "ring_size" ring-size)))
          (when outputs
            (list (cons "outputs" outputs)))
          (when unlock-time
            (list (cons "unlock_time" unlock-time)))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when get-transaction-keys
            (list (cons "get_tx_keys" t)))
          (when below-amount
            (list (cons "below_amount" below-amount)))
          (when do-not-relay
            (list (cons "do_not_relay" t)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" t)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" t)))))

(defjsonrpc sweep-dust ("sweep_dust" &key get-transaction-keys do-not-relay get-transaction-hex get-transaction-metadata)
  "Send all dust outputs back to the wallet."
  (append (when get-transaction-keys
            (list (cons "get_tx_keys" t)))
          (when do-not-relay
            (list (cons "do_not_relay" t)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" t)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" t)))))

(defjsonrpc sweep-single ("sweep_single" address &key priority ring-size outputs unlock-time payment-id get-transaction-key key-image do-not-relay get-transaction-hex get-transaction-metadata)
  "Send all of a specific unlocked output to an address."
  (append (list (cons "address" address))
          (when priority
            (list (cons "priority" priority)))
          (when ring-size
            (list (cons "ring_size" ring-size)))
          (when outputs
            (list (cons "outputs" outputs)))
          (when unlock-time
            (list (cons "unlock_time" unlock-time)))
          (when payment-id
            (list (cons "payment_id" payment-id)))
          (when get-transaction-key
            (list (cons "get_tx_key" t)))
          (when key-image
            (list (cons "key_image" key-image)))
          (when do-not-relay
            (list (cons "do_not_relay" t)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" t)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" t)))))

(defjsonrpc tag-accounts ("tag_accounts" tag accounts)
  "Apply a filtering tag to a list of accounts."
  (list (cons "tag" tag)
        (cons "accounts" (coerce accounts 'vector))))

(defjsonrpc transfer ("transfer" destinations &key account-index subaddress-indices priority ring-size unlock-time payment-id get-transaction-key do-not-relay get-transaction-hex get-transaction-metadata)
  "Send monero to a number of recipients."
  (append (list (cons "destinations" (coerce destinations 'vector)))
          (when account-index
            (list (cons "account_index" account-index)))
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
            (list (cons "get_tx_key" t)))
          (when do-not-relay
            (list (cons "do_not_relay" t)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" t)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" t)))))

(defjsonrpc transfer-split ("transfer_split" destinations &key account-index subaddress-indices priority ring-size unlock-time payment-id get-transaction-keys do-not-relay get-transaction-hex get-transaction-metadata)
  "Like transfer, but can split into several transactions if necessary."
  (append (list (cons "destinations" (coerce destinations 'vector)))
          (when account-index
            (list (cons "account_index" account-index)))
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
            (list (cons "get_tx_keys" t)))
          (when do-not-relay
            (list (cons "do_not_relay" t)))
          (when get-transaction-hex
            (list (cons "get_tx_hex" t)))
          (when get-transaction-metadata
            (list (cons "get_tx_metadata" t)))))

(defjsonrpc untag-accounts ("untag_accounts" accounts)
  "Remove filtering tag from a list of accounts."
  (list (cons "accounts" (coerce accounts 'vector))))

(defjsonrpc verify ("verify" string address signature)
  "Verify a signature on a string."
  (list (cons "data" string)
        (cons "address" address)
        (cons "signature" signature)))
