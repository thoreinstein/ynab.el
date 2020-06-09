;;; ynab-log.el --- YNAB logging -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)

(defface ynab-log-date-face
  '((t :inherit font-lock-type-face))
  "Face for showing the date in the ynab log buffer."
  :group 'ynab)

(defface ynab-log-error-level-face
  '((t :foreground "red"))
  "Face for showing the `error' log level in the ynab log buffer."
  :group 'ynab)

(defface ynab-log-warn-level-face
  '((t :foreground "goldenrod"))
  "Face for showing the `warn' log level in the ynab log buffer."
  :group 'ynab)

(defface ynab-log-info-level-face
  '((t :foreground "deep sky blue"))
  "Face for showing the `info' log level in the ynab log buffer."
  :group 'ynab)

(defface ynab-log-debug-level-face
  '((t :foreground "magenta2"))
  "Face for showing the `debug' log level in the ynab log buffer."
  :group 'ynab)

(defvar ynab-log-buffer-name "*ynab-log*"
  "Name of buffer used for logging Ynab events.")

(defvar ynab-log-level 'info
  "Lowest type of messages to be logged.")

(defun ynab-log-buffer ()
  "Return the buffer for `ynab-log', creating it as needed."
  (let ((buffer (get-buffer ynab-log-buffer-name)))
    (if buffer
        buffer
      (with-current-buffer (generate-new-buffer ynab-log-buffer-name)
        (special-mode)
        (current-buffer)))))

(defun ynab-log--level-number (level)
  "Return a relative level number for LEVEL."
  (cl-case level
    (debug -10)
    (info 0)
    (warn 10)
    (error 20)
    (otherwise -10)))

(defun ynab-log (level fmt &rest objects)
  "Write log message FMT at LEVEL to YNAB's log buffer.

LEVEL should be a symbol: debug, info, warn, error.
FMT must be a string suitable for `format' given OBJECTS as arguments."
  (let ((log-buffer (ynab-log-buffer))
        (log-level-face (cl-case level
                          (debug 'ynab-log-debug-level-face)
                          (info 'ynab-log-info-level-face)
                          (warn 'ynab-log-warn-level-face)
                          (error 'ynab-log-error-level-face)))
        (inhibit-read-only t))
    (when (>= (ynab-log--level-number level)
              (ynab-log--level-number ynab-log-level))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert
         (format
          (concat "[" (propertize "%s" 'face 'ynab-log-date-face) "] "
                  "[" (propertize "%s" 'face log-level-face) "]: %s\n")
          (format-time-string "%Y-%m-%d %H:%M:%S")
          level
          (apply #'format fmt objects)))))))

(provide 'ynab-log)

;;; ynab-log.el ends here
