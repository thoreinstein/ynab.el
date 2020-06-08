;;; ynab-api.el --- Handlers for the YNAB API -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defcustom ynab-personal-token ""
  "Your personal access token for YNAB."
  :group 'ynab
  :type 'string)

(defconst ynab--api-url "https://api.youneedabudget.com/v1/")

(defun ynab-api--make-request (path)
  "Make the api request to PATH and upack the response."
  (let ((url-request-extra-headers (list (cons "Authorization" (format "Bearer %s" ynab-personal-token))))
        (json-object-type 'plist))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "%s/%s" ynab--api-url path))
      (json-read-object))))

(provide 'ynab-api)

;;; ynab-api.el ends here
