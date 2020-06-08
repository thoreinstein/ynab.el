;;; ynab-transaction.el --- YANB Transaction Class -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;;; Commentary:
;;
;;; Code:

(require 'pcache)

(require 'ynab-api)
(require 'ynab-budget)
(require 'ynab-util)

(defvar ynab--transactions-date-since nil
  "The date which transactions will be fetched from.")

(defconst ynab--transaction-cache (pcache-repository "ynab-transactions"))

(cl-defstruct ynab-transaction
  "A YNAB transaction."
  id ;;string
  date ;;string
  amount ;;integer
  memo ;;string
  cleared ;;string
  approved ;;boolean
  flag-color ;;string
  account-id ;;string
  payee-id ;;string
  category-id ;;string
  transfer-account-id ;;string
  transfer-transaction-id ;;string
  matched-transaction-id ;;string
  import-id ;;string
  deleted ;;boolean
  account-name ;;string
  payee-name ;;string
  category-name ;;string
  ;; TODO Split transaction support
  ;;subtransactions []transaction
  )

(defun ynab--parse-transactions (transactions)
  "Parse TRANSACTIONS from the YNAB API."
  (cl-loop for transaction across (plist-get (plist-get transactions :data) :transactions) collect
           (make-ynab-transaction
            :id (plist-get transaction :id)
            :date (plist-get transaction :date)
            :amount (plist-get transaction :amount)
            :memo (plist-get transaction :memo)
            :cleared (plist-get transaction :cleared)
            :approved (plist-get transaction :approved)
            :account-id (plist-get transaction :account_id)
            :payee-id (plist-get transaction :payee_id)
            :category-id (plist-get transaction :category_id)
            :transfer-account-id (plist-get transaction :transfer_account_id)
            :transfer-transaction-id (plist-get transaction :transfer_transaction_id)
            :matched-transaction-id (plist-get transaction :matched_transaction_id)
            :import-id (plist-get transaction :import_id)
            :deleted (plist-get transaction :deleted)
            :account-name (plist-get transaction :account_name)
            :payee-name (plist-get transaction :payee_name)
            :category-name (plist-get transaction :category_name)

            )))

(defun ynab-transaction-list (&optional skip-cache)
  "Return the list of transactions for chosen budget.

If SKIP-CACHE is non nil, results will be fetched from the server
instead of the cache and any existing cache invalidated."
  (if (or ynab-skip-cache skip-cache)
      (progn
        (pcache-invalidate ynab--transaction-cache (ynab-budget-id ynab--chosen-budget))
        (pcache-purge-invalid ynab--transaction-cache)
        (ynab-transaction--fetch))
    (if (pcache-has ynab--transaction-cache (ynab-budget-id ynab--chosen-budget))
        (let* ((transactions (pcache-get ynab--transaction-cache (ynab-budget-id ynab--chosen-budget)))
              (oldest (first transactions)))
          (if (ts< (ts-parse ynab--transactions-date-since) (ts-parse (ynab-transaction-date oldest)))
              (ynab-transaction--fetch)
            (seq-filter (lambda (transaction)
                          (ts>= (ts-parse (ynab-transaction-date transaction)) (ts-parse ynab--transactions-date-since)))
                        transactions))))))

;; (ynab-transaction-list t)

(defun ynab-transaction-list-for-view ()
  "Format the list of transactions for view."
  (mapcar (lambda (transaction)
            (let* ((amount (ynab-transaction-amount transaction))
                   (cleared (ynab-transaction-cleared transaction))
                   (negative-amount (<= (if amount amount 0) 0)))
              (list (ynab-transaction-id transaction)
                    (vector (ynab-transaction-date transaction)
                            (ynab-transaction-payee-name transaction)
                            (ynab-transaction-category-name transaction)
                            (if negative-amount (ynab--format-amount amount) "")
                            (if (not negative-amount) (ynab--format-amount amount) "")
                            (if cleared cleared "")))))
          (ynab-transaction-list)))

(defun ynab-transaction--fetch ()
  "Fetch the list of transactions for the chosen budget."
  (let* ((path (format "budgets/%s/transactions?since_date=%s" (ynab-budget-id ynab--chosen-budget) ynab--transactions-date-since))
         (transactions (ynab--parse-transactions (ynab-api--make-request path))))
    (unless ynab-skip-cache
      (pcache-put ynab--transaction-cache (ynab-budget-id ynab--chosen-budget) transactions))
    transactions))

(provide 'ynab-transaction)

;;; ynab-transaction.el ends here
