;;; ynab.el --- YNAB... in Emacs! -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;; Author: Jim Anders <http://github.com/janders223>
;; Maintainer: Jim Anders <jimanders223@gmail.com>
;; Created: May 20, 2020
;; Modified: May 20, 2020
;; Version: 0.0.1
;; Keywords: YNAB
;; Homepage: https://github.com/janders223/ynab.el
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
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
;; Major mode for working with your YNAB data
;;
;;; Code:

(require 'json)

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

(defun ynab--fetch-transactions-for-budget (budget)
  "Fetch the list of transactions for the specified BUDGET."
  (let ((url-request-extra-headers (list (cons "Authorization" (format "Bearer %s" ynab-personal-token))))
        (calc-date-format "XYYYY-MM-DD")
        (date-since (substring (calc-eval "(now() - 30") 1 -1))
        (json-object-type 'plist))
  (with-current-buffer
   (url-retrieve-synchronously
    (format "%s/budgets/%s/transactions?since_date=%s" ynab--api-url budget date-since))
   (let ((result (json-read-object)))
     (cl-loop for transaction across (plist-get (plist-get result :data) :transactions) collect
                  (ynab-transaction--create
                   :id (plist-get transaction :id)
                   :date (plist-get transaction :date)
                   :payee (plist-get transaction :payee_name)
                   :category (plist-get transaction :category_name)
                   :amount (plist-get transaction :amount)
                   :cleared (plist-get transaction :cleared)))))))

(define-derived-mode ynab-transactions-mode tabulated-list-mode "YNAB Transactions"
  "Major mode for interacting with YNAB transactions."
  :group 'ynab
  (setq tabulated-list-format
        [("Date" 15 t) ("Payee" 40 nil) ("Category" 40 nil) ("Amount" 15 nil) ("Cleared" 10 nil)]
        tabulated-list-sort-key (cons "Date" t)))

(defun ynab--refresh-transaction-list ()
  "Refresh the transactions."
  (setq tabulated-list-entries
        (mapcar (lambda (transaction)
                  (list (ynab-transaction-id transaction)
                        (vector (ynab-transaction-date transaction)
                                (ynab-transaction-payee transaction)
                                (ynab-transaction-category transaction)
                                (format "$%.2f" (/ (ynab-transaction-amount transaction) 1000.00))
                                (ynab-transaction-cleared transaction))))
                (ynab--fetch-transactions-for-budget ynab--default-budget))))

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
