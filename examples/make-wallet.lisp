;;; Generate a new wallet and print its address and seed.

(asdf:load-system "monero-tools")

(let* ((secret-key (monero-tools:generate-secret-key))
       (seed (monero-tools:secret-key->mnemonic-seed secret-key :english))
       (address (monero-tools:secret-spend-key->address secret-key)))
  (format t "Wallet address: ~a~%Seed: ~a~%" address seed))
