;;; ynab-api.el --- Handlers for the YNAB API -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)

(require 'ynab-cache)

(defcustom ynab-personal-token ""
  "Your personal access token for YNAB."
  :group 'ynab
  :type 'string)

(defconst ynab--api-url "https://api.youneedabudget.com/v1/")

;;; data structures

(cl-defstruct ynab-transaction
  "A YNAB transaction."
  id date payee category amount cleared)

;; (cl-defstruct ynab-budget
;;   "A YNAB Budget."
;;   id name)

;; (cl-defstruct ynab-payee
;;   "A YNAB Payee."
;;   id name deleted)

;; (cl-defstruct ynab-category-group
;;   "Top Level YNAB Category Group."
;;   id name hidden deleted)

;; (cl-defstruct ynab-category
;;   "A YNAB Budget Category."
;;   id
;;   category_group_id
;;   name
;;   hidden
;;   original_category_group_id
;;   note
;;   budgeted
;;   activity
;;   balance
;;   goal_type
;;   goal_creation_month
;;   goal_target
;;   goal_target_month
;;   goal_percentage_complete
;;   deleted)

;;; data parsers

(defun ynab--parse-transactions (transactions)
  "Parse TRANSACTIONS from the YNAB API."
    (cl-loop for transaction across (plist-get (plist-get transactions :data) :transactions) collect
                  (make-ynab-transaction
                   :id (plist-get transaction :id)
                   :date (plist-get transaction :date)
                   :payee (plist-get transaction :payee_name)
                   :category (plist-get transaction :category_name)
                   :amount (plist-get transaction :amount)
                   :cleared (plist-get transaction :cleared))))

;; (defun ynab--parse-payees (payees)
;;   "Parse PAYEES from the YNAB API."
;;  (cl-loop for payee across (plist-get (plist-get payees :data) :payees) collect
;;                  (make-ynab-payee
;;                   :id (plist-get payee :id)

;;                   :name (plist-get payee :name)
;;                   :deleted (plist-get payee :deleted))))

;; (defun ynab--parse-categories (categories)
;;   "Parse CATEGORIES from the YNAB API."
;; (cl-loop for category across categories collect
;;                           (make-ynab-category
;;                            :id (plist-get category :id)
;;                            :category_group_id (plist-get category :category_group_id)
;;                            :name (plist-get category :name)
;;                            :hidden (plist-get category :hidden)
;;                            :original_category_group_id (plist-get category :original_category_group_id)
;;                            :note (plist-get category :note)
;;                            :budgeted (plist-get category :budgeted)
;;                            :activity (plist-get category :activity)
;;                            :balance (plist-get category :balance)
;;                            :goal_type (plist-get category :goal_type)
;;                            :goal_creation_month (plist-get category :goal_creation_month)
;;                            :goal_target (plist-get category :goal_target)
;;                            :goal_target_month (plist-get category :goal_target_month)
;;                            :goal_percentage_complete (plist-get category :goal_percentage_complete)
;;                            :deleted (plist-get category :deleted))))

;; (defun ynab--parse-category-groups (category-groups)
;;   "Parse CATEGORY-GROUPS from the YNAB API."
;;   (flatten (cl-loop for category-group across (plist-get (plist-get category-groups :data) :category_groups) do
;;            (make-ynab-category-group
;;             :id (plist-get category-group :id)
;;             :name (plist-get category-group :name)
;;             :hidden (plist-get category-group :hidden)
;;             :deleted (plist-get category-group :deleted))
;;            collect (ynab--parse-categories (plist-get category-group :categories)))))

;;; data fetchers

(defun ynab--fetch-transactions-for-budget (budget &optional date)
  "Fetch the list of transactions for the specified BUDGET and optional DATE."
  (let* ((date-since (if date
                        date
                        (ts-format "%Y-%m-%d" (ts-dec 'day 30 (ts-now)))))
         (path (format "budgets/%s/transactions?since_date=%s" (ynab-budget-id budget) date-since))
         (result (ynab--parse-transactions (ynab-api--make-request path))))
    ;;; TODO Shove result into a caching mechanism to avoid making repeated API calls
    result))


;; (defun ynab--fetch-payee-list-for-budget (budget)
;;   "Fetch and parse the payee list for the BUDGET."
;;   (let* ((path (format "budgets/%s/payees" (ynab-budget-id budget)))
;;          (result (ynab--parse-payees (ynab-api--make-request path))))
;;     ;;; TODO Shove result into a caching mechanism to avoid making repeated API calls
;;     result))

;; (defun ynab--fetch-category-list-for-budget (budget)
;;   "Fetch and parse the categorylist for BUDGET."
;;   (let* ((path (format "budgets/%s/categories" (ynab-budget-id budget)))
;;         (result (ynab--parse-category-groups (ynab-api--make-request path))))
;;     ;;; TODO Shove result into a caching mechanism to avoid making repeated API calls
;;     result))

;; (ynab-budget-id ynab--chosen-budget)
;; (ynab--fetch-payee-list-for-budget ynab--chosen-budget)

;;; internals

(defun ynab-api--make-request (path)
  "Make the api request to PATH and upack the response."
  (let ((url-request-extra-headers (list (cons "Authorization" (format "Bearer %s" ynab-personal-token))))
        (json-object-type 'plist))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "%s/%s" ynab--api-url path))
      (json-read-object))))

(defun flatten (list)
  "Flatten LIST sublists into a single list."
  (mapcan (lambda (x) (if (listp x) x nil)) list))

(provide 'ynab-api)

;;; ynab-api.el ends here
