;;; ynab-budget.el --- YNAB Budget -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;; Author: Jim Anders <http://github/kon8522>
;; Maintainer: Jim Anders <jimanders223@gmail.com>
;; Created: June 02, 2020
;; Modified: June 02, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/kon8522/ynab-budget
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  YNAB Budget
;;
;;; Code:

(require 'eieio)
(require 'pcache)

(require 'ynab-api)

(defclass ynab-budget ()
  ((id :initarg :id
       :initform ""
       :type string
       :documentation "The budget id from YNAB.")
   (name :initarg :name
         :initform ""
         :type string
         :documentation "The budgeet name from YNAB."))
  "YNAB Budget information.")

(cl-defmethod ynab-budget-id ((budget ynab-budget))
  "Get the `:id' for BUDGET."
  (oref budget :id))

(cl-defmethod ynab-budget-name ((budget ynab-budget))
  "Get the `:name' for BUDGET."
  (oref budget :name))

(defun ynab-budget-list ()
  "Return the list of budgets, either from the cache or the server."
  (if ynab-skip-cache
      (ynab-budget--fetch)
    (if (pcache-has ynab--cache 'budgets)
        (pcache-get ynab--cache 'budgets)
      (ynab-budget--fetch))))

(defun ynab-budget-names-for-ido ()
  "Get a list of budget names for `ido' completion."
  (let ((budgets (ynab-budget-list)))
    (mapcar  'ynab-budget-name budgets)))

(defun ynab-budget--find-by-name (budget-name)
  "Return the `:id' for budget where `:name' id BUDGET-NAME."
  (let ((budgets (ynab-budget-list)))
    (object-assoc budget-name :name budgets)))

(defun ynab-budget--fetch ()
  "Fetch the list of YNAB budgets from the server."
  (let* ((path "budgets")
         (result (ynab-api--make-request path))
         (budgets (cl-loop for budget across (plist-get (plist-get result :data) :budgets) collect
                           (ynab-budget
                            :id (plist-get budget :id)
                            :name (plist-get budget :name)))))
    (unless ynab-skip-cache
      (pcache-put ynab--cache 'budgets budgets))
    budgets))

(ynab-budget-list)

(provide 'ynab-budget)

;;; ynab-budget.el ends here
