;;; mulex.el --- Mule Extension  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/mulex
;; Version: 0.1.3
;; Keywords: mule, multilingual
;; Package-Requires: ((emacs "30.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package provides an extensiont to mule (multilingual environment).
;;
;; Within the package, each language is referred to by an IETF language tag,
;; e.g., 'en', 'en-GB', and 'ja'.
;;
;;; Code:

(defvar mulex-languages
  '(("chinese" . zh)
    ("english" . en)
    ("french" . fr)
    ("german" . de)
    ("japanese" . ja)
    ("korean" . ko))
  "Mapping language string to IETF language tag.")

(defun mulex-im-lang ()
  "Infer language from the current input method."
  (if current-input-method
      (catch 'break
        (dolist (la mulex-languages)
          (when (string-match-p (concat "\\`" (car la)) current-input-method)
            (throw 'break (cdr la))))
        nil)))

(defvar mulex-tr nil
  "Text translation table.
The alist provides a mapping for text and its translation. For example,

  (\"foo\" . (('ja . \"ほげ\")))

provides a translation of 'foo' to 'ほげ' when the language is Japanese.")

(defvar mulex-tr-months
  '((en . ("January" "Feburury" "March" "April" "May" "June"
           "July" "August" "September" "October" "November" "December"))))

(defun mulex-s (text &optional tr lang)
  "Render TEXT in the currently chosen language.
If TR is given, it overrides the translation defined `mulex-tr'.

If LANG is not given, the function will use `mulex-im-lang' for auto-discovery."
  (let ((lang (or lang (mulex-im-lang))))
    (alist-get lang (or tr (alist-get text mulex-tr nil nil #'equal)) text)))

(defun mulex-date-decode (s-date)
  "Decode date string S-DATE into numeric components.
Use `decoded-time-' functions to get individual components."
  (parse-time-string s-date))

(defun mulex-date-format (year month day &optional lang)
  "Format YEAR, MONTH, and DAY in LANG.
This function returns nil if given date cannot be formatted."
  (pcase (or lang (mulex-im-lang))
    ('ja
     (concat (and year (format "%d年" year))
             (and month (format "%d月" month))
             (and day (format "%d日" day))))
    (_
     (cond
      ((and year month day)
       (format "%s %d, %d" (nth (1- month) (alist-get 'en mulex-tr-months))
               day year))
      ((and year month)
       (format "%s %d" (nth (1- month) (alist-get 'en mulex-tr-months)) year))
      (year (format "%d" year))
      (t nil)))))

(defmacro mulex-case (&rest body)
  `(pcase (mulex-im-lang)
     ,@body))

(provide 'mulex)
;;; mulex.el ends here
