;;; ynab-payee.el --- YNAB Payee -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;;; Commentary:
;;
;;; Code:

(require 'pcache)

(require 'ynab-api)

(cl-defstruct ynab-payee
  id                  ;;string
  name                ;;string
  transfer-account-id ;;string
  deleted             ;;boolean
  )

(defun ynab--parse-payees (payees)
  "Parse PAYEES from the YNAB API."
  (cl-loop for payee across (plist-get (plist-get payees :data) :payees) collect
           (make-ynab-payee
            :id (plist-get payee :id)
            :name (plist-get payee :name)
            :transfer-account-id (plist-get payee :transfer_account_id)
            :deleted (plist-get payee :deleted))))

(defun ynab-payee-list (&optional skip-cache)
  "Return the list of payees for the chosen budget.

Payees are returned from the cache unless SKIP-CACHE is non-nil,
in which case they are fetched from the server."
  (if (or ynab--skip-cache skip-cache)
      (ynab-payee--fetch)
    (if (pcache-has ynab--cache 'payees)
        (pcache-get ynab--cache 'payees)
      (ynab-payee--fetch))))

(provide 'ynab-payee)
;;; ynab-payee.el ends here
