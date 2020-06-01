;;; ynab.el --- Major mode for YNAB (you need a budget) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;; Author: Jim Anders <https://github.com/janders223>
;; Maintainer: Jim Anders <jimanders223@gmail.com>
;; Created: May 20, 2020
;; Modified: May 20, 2020
;; Version: 0.0.2
;; Keywords: YNAB budget convenience
;; Homepage: https://github.com/janders223/ynab.el
;; Package-Requires: ((emacs "26.3") (cl-lib "0.5") (ts "0.2"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Major mode for working with your YNAB (you need a budget) data
;;
;;; Code:

(require 'json)
(require 'ts)

(defgroup ynab nil
  "Use YNAB from the comfort of Emacs."
  :group 'convenience)

(defcustom ynab-personal-token ""
  "Your personal access token for YNAB."
  :group 'ynab
  :type 'string)

(defcustom ynab-default-budget "last-used"
  "The default budget to pull up.

This is set to `last-used' as per the documentation and will be the last used budget in an 'approved' app."
  :group 'ynab
  :type 'string)

(defconst ynab--api-url "https://api.youneedabudget.com/v1/")

(cl-defstruct (ynab-transaction (:constructor ynab-transaction--create))
  "A YNAB transaction."
  id date payee category amount cleared)

(defun ynab--fetch-transactions-for-budget (budget &optional date)
  "Fetch the list of transactions for the specified BUDGET and optional DATE."
  (let ((url-request-extra-headers (list (cons "Authorization" (format "Bearer %s" ynab-personal-token))))
        (date-since (if date
                        date
                        (ts-format "%Y-%m-%d" (ts-dec 'day 30 (ts-now)))))
        (json-object-type 'plist))
    (message "Fetching transactions from YNAB")
  (with-current-buffer
   (url-retrieve-synchronously
    (format "%s/budgets/%s/transactions?since_date=%s" ynab--api-url budget date-since))
   (let ((result (json-read-object)))
     (message (format "Fetched %d transactions from YNAB" (length (plist-get (plist-get result :data) :transactions))))
     (cl-loop for transaction across (plist-get (plist-get result :data) :transactions) collect
                  (ynab-transaction--create
                   :id (plist-get transaction :id)
                   :date (plist-get transaction :date)
                   :payee (plist-get transaction :payee_name)
                   :category (plist-get transaction :category_name)
                   :amount (plist-get transaction :amount)
                   :cleared (plist-get transaction :cleared)))))))

(defvar ynab-transactions-mode-map nil "Keymap for `ynab-transactions-mode'.")

(progn
  (setq ynab-transactions-mode-map (make-sparse-keymap))
  (define-key ynab-transactions-mode-map (kbd "C-c C-d") 'ynab-set-transaction-since-date))

(define-derived-mode ynab-transactions-mode tabulated-list-mode "YNAB Transactions"
  "Major mode for interacting with YNAB transactions."
  :group 'ynab
  ;; (use-local-map ynab-transactions-mode-map)
  (setq tabulated-list-format
        [("Date" 15 t) ("Payee" 40 nil) ("Category" 40 nil) ("Amount" 15 nil) ("Cleared" 10 nil)]
        tabulated-list-sort-key (cons "Date" t)))

(defun ynab--refresh-transaction-list (&optional date)
  "Refresh the transactions, optionally setting the since DATE."
  (setq tabulated-list-entries
        (mapcar (lambda (transaction)
                  (list (ynab-transaction-id transaction)
                        (vector (ynab-transaction-date transaction)
                                (ynab-transaction-payee transaction)
                                (ynab-transaction-category transaction)
                                (format "$%.2f" (/ (ynab-transaction-amount transaction) 1000.00))
                                (ynab-transaction-cleared transaction))))
                (if date
                    (ynab--fetch-transactions-for-budget ynab-default-budget date)
                    (ynab--fetch-transactions-for-budget ynab-default-budget)))))

(defun ynab-set-transaction-since-date (date)
  "Set the DATE from which to pull transactions.

When you first load ynab this is defaulted to 30 days ago.
The date you choose will fetch transactions recorded _ON_ or _AFTER_ the chosen date."
  (interactive "sEnter the desired date: ")
  (ynab--refresh-transaction-list date)
  (tabulated-list-print))

;;;###autoload
(defun ynab ()
  "Enter ynab."
  (interactive)
  (let ((buffer (get-buffer-create "*YNAB Transactions*")))
    (with-current-buffer buffer
      (ynab-transactions-mode)
      (ynab--refresh-transaction-list)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buffer)
    nil))

(provide 'ynab)
;;; ynab.el ends here
