;;; ynab-cache.el --- Cache things in ynab.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jim Anders
;;
;; Author: Jim Anders <http://github/kon8522>
;; Maintainer: Jim Anders <jimanders223@gmail.com>
;; Created: June 02, 2020
;; Modified: June 02, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/kon8522/ynab-cache
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Cache things in ynab.el
;;
;;; Code:

(require 'pcache)

(defun ynab--cache-budgets (budgets)
  "Add BUDGETS to the cache."
  (let ((repo (pcache-repository "ynab-budgets")))
    (cl-loop for budget in budgets do
             (pcache-put repo (ynab-budget-name budget) budget))))

(provide 'ynab-cache)
;;; ynab-cache.el ends here
