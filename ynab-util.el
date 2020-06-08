;;; ynab-util.el --- YNAB Utility Methods -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;;; Commentary:
;;
;;; Code:


(defun ynab-transaction--fetch ()
  "Fetch the list of transactions for the chosen budget."
  (let* ((path (format "budgets/%s/transactions?since_date=%s" (ynab-budget-id ynab--chosen-budget) ynab--transactions-date-since))
         (transactions (ynab--parse-transactions (ynab-api--make-request path))))
    (unless ynab-skip-cache
      (pcache-put ynab--cache (ynab-budget-id ynab--chosen-budget) transactions))
    transactions))

(defun ynab--format-amount (amount)
  "Format the AMOUNT of a YNAB transaction.

Amounts from the API are in `miliunits' and need to be formatted to
read as dollars and cents.

TODO This will need to take into account the currency unit provided by
the budget."
  (format "$%.2f" (/ amount 1000.00)))

(provide 'ynab-util)
;;; ynab-util.el ends here
