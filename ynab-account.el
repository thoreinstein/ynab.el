;;; ynab-account.el --- YNAB Account -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;;; Commentary:
;;
;;; Code:

(require 'pcache)

(require 'ynab-api)

(cl-defstruct ynab-account
  id
  name
  type
  on-budget
  closed
  note
  balance
  cleared-balance
  uncleared-balance
  transfer-payee-id
  deleted)

(defun ynab--parse-accounts (accounts)
  "Parse ACCOUNTS from the YNAB API."
  (cl-loop for account across (plist-get (plist-get accounts :data) :accounts) collect
           (make-ynab-account
            :id (plist-get account :id)
            :name (plist-get account :name)
            :type (plist-get account :type)
            :on-budget (plist-get account :on_budget)
            :closed (plist-get account :closed)
            :note (plist-get account :note)
            :balance (plist-get account :balance)
            :cleared-balance (plist-get account :cleared_balance)
            :uncleared-balance (plist-get account :uncleared_balance)
            :transfer-payee-id (plist-get account :transfer_payee_id)
            :deleted (plist-get account :deleted))))

(defun ynab-account-list (&optional skip-cache)
  "Return the list of accounts for the chosen budget.

Accounts are returned from the cache unless SKIP-CACHE is passed
in which case they are fetched directly from the server."
  (if (or ynab-skip-cache skip-cache)
      (ynab-account--fetch)
    (if (pcache-has ynab--cache 'accounts)
        (pcache-get ynab--cache 'accounts)
      (ynab-account--fetch))))

(defun ynab-account-names-for-ido ()
  "Get a list of accounts for `ido' completion."
  (let ((accounts (ynab-account-list)))
    (mapcar #'ynab-account-name accounts)))

(defun ynab-account-find-by-name (account-name)
  "Return the account struct for ACCOUNT-NAME."
  (let ((accounts (ynab-account-list)))
    (cl-find-if (lambda (a) (string-equal (ynab-account-name a) account-name)) accounts)))

(defun ynab-account--fetch ()
  "Fetch the list of YNAB accounts from the server."
  (let* ((path (format "budgets/%s/accounts" (ynab-budget-id ynab--chosen-budget)))
         (accounts (ynab--parse-accounts (ynab-api--make-request path))))
    (unless ynab-skip-cache
      (pcache-put ynab--cache 'accounts accounts)
      accounts)))

(provide 'ynab-account)
;;; ynab-account.el ends here
