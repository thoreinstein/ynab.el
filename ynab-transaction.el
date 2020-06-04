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

(defvar ynab--transactions-date-since nil
  "The date which transactions will be fetched from.")

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
  ;;subtransactions []transaction
  )

(defun ynab--parse-transactions (transactions)
  "Parse TRANSACTIONS from the YNAB API."
    (cl-loop for transaction across (plist-get (plist-get transactions :data) :transactions) collect
                  (make-ynab-transaction
                   :id (plist-get transaction :id)
                   :date (plist-get transaction :date)
                   :payee-name (plist-get transaction :payee_name)
                   :category-name (plist-get transaction :category_name)
                   :amount (plist-get transaction :amount)
                   :cleared (plist-get transaction :cleared))))

(defun ynab-transaction-list (&optional skip-cache)
  "Return the list of transactions for chosen budget.

If SKIP-CACHE is non nil, results will be fetched from the server
instead of the cache and any existing cache invalidated."
  (if (or ynab-skip-cache skip-cache)
      (progn
        (pcache-invalidate ynab--cache (ynab-budget-id ynab--chosen-budget))
        (pcache-purge-invalid ynab--cache)
        (ynab-transaction--fetch))
    (if (pcache-has ynab--cache (ynab-budget-id ynab--chosen-budget))
        (pcache-get ynab--cache (ynab-budget-id ynab--chosen-budget))
      (ynab-budget--fetch))))

(defun ynab-transaction-list-for-view ()
  "Format the list of transactions for view."
 (mapcar (lambda (transaction)
                  ;; (pp transaction)
                  (list (ynab-transaction-id transaction)
                        (vector (ynab-transaction-date transaction)
                                (ynab-transaction-payee-name transaction)
                                (ynab-transaction-category-name transaction)
                                (format "$%.2f" (/ (ynab-transaction-amount transaction) 1000.00))
                                (ynab-transaction-cleared transaction)
                                )
                        )
                  )
                (ynab-transaction-list)))

(defun ynab-transaction--fetch ()
  "Fetch the list of transactions for the chosen budget."
  (let* ((path (format "budgets/%s/transactions?since_date=%s" (ynab-budget-id ynab--chosen-budget) ynab--transactions-date-since))
         (transactions (ynab--parse-transactions (ynab-api--make-request path))))
    (unless ynab-skip-cache
      (pcache-put ynab--cache (ynab-budget-id ynab--chosen-budget) transactions))
    transactions))

;;; DEBUG
;; (ynab-transaction--fetch)
;; (pcache-has ynab--cache (ynab-budget-id ynab--chosen-budget))
;; (ynab-transaction-list t)
;; (ynab-transaction-list-for-view)

(provide 'ynab-transaction)

;;; ynab-transaction.el ends here
