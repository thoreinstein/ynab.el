;;; ynab-budget.el --- YNAB Budget -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;;; Commentary:
;;
;;; Code:

(require 'pcache)

(require 'ynab-api)

(cl-defstruct ynab-budget id name)

(defun ynab-budget-list (&optional skip-cache)
  "Return the list of budgets, either from the cache or the server if SKIP-CACHE is not nil."
  (if (or ynab-skip-cache skip-cache)
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
    (cl-find-if (lambda (b) (string-equal (ynab-budget-name b) budget-name)) budgets)))

(defun ynab-budget--fetch ()
  "Fetch the list of YNAB budgets from the server."
  (let* ((path "budgets")
         (result (ynab-api--make-request path))
         (budgets (cl-loop for budget across (plist-get (plist-get result :data) :budgets) collect
                           (make-ynab-budget
                            :id (plist-get budget :id)
                            :name (plist-get budget :name)))))
    (unless ynab-skip-cache
      (pcache-put ynab--cache 'budgets budgets))
    budgets))

(provide 'ynab-budget)

;;; ynab-budget.el ends here
