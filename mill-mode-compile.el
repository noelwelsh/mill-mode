;;; mill-mode-compile.el --- Compilation like commands in Mill
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Support for compilation like commands in Mill. A compilation like command is
;;; a command where the output should be parsed for file position inforation,
;;; and this information should be made available to `next-error` and related
;;; functions. For example, compilation errors include code positions that
;;; `next-error` will navigate through, as do test errors.
;;;
;;; Code:

(require 'compile)
(require 'mill-mode-base)

(defconst mill-compilation-regexp
  '(;; Sbt 1.0.x
    ("^\\[error][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):" 1 2 3 2 1)
    ;; Sbt 0.13.x
    ("^\\[error][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):" 1 2 nil 2 1)
    ;; https://github.com/Duhemm/sbt-errors-summary
    ("^\\[error][[:space:]]\\[E[[:digit:]]+][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):$" 1 2 3 2 1)
    ("^\\[warn][[:space:]]+\\[E[[:digit:]]+][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):$" 1 2 3 1 1)
    ("^\\[warn][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):" 1 2 nil 1 1)
    ("^\\[info][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):" 1 2 nil 0 1)
    ;; failing scalatests
    ("^\\[info][[:space:]]+\\(.*\\) (\\([^:[:space:]]+\\):\\([[:digit:]]+\\))" 2 3 nil 2 1)
    ("^\\[warn][[:space:]][[:space:]]\\[[[:digit:]]+][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):" 1 2 3 1 1)
    )
  "Regexps for finding error locations in compile like output.")

(define-compilation-mode mill-compilation-mode "mill-mode"
  "Compilation for mode for Mill compile like commands."
  (set (make-local-variable 'compilation-error-regexp-alist)
       mill-compilation-regexp))

(defun mill-mode-run-compile-like (&rest commands)
  "Run Mill COMMANDS, a list of string, in a buffer that will link to error messages."
  (let ((command (string-join (cons mill-program-name commands) " ")))
    (with-current-buffer (mill-get-buffer)
      (erase-buffer)
      (cd (mill-find-root))
      (compilation-start command 'mill-compilation-mode (lambda (m) buffer-name)))))

(provide 'mill-mode-compile)
;;; mill-mode-compile.el ends here
