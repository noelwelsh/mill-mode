;;; mill-mode.el --- Interactive support for Mill projects
;; -*- lexical-binding- t; -*-

;;; Commentary:
;;;
;;; Major mode for working with Mill. Loosely based on sbt-mode.
;;;
;;; Code:

(require 'compile)
(require 'mill-mode-base)
(require 'mill-mode-compile)


(defconst mill-top-level-commands
  '("all" "resolve" "inspect" "show" "shutdown" "path" "plan" "version" "visualize" "visualizePlan" "clean")
  "List of mill commands that can be run without specifying a project.")

(defvar-local mill-current-module nil
  "The current Mill module to use when issuing user commands.
  Nil if no project has been selected yet, a String otherwise.")


;;; Interactive functions

(defun mill-compile-current-module ()
  "Compile the current module, displaying the output in the Mill buffer."
  (interactive)
  (let ((module (mill-get-or-choose-current-module)))
    (mill-mode-run-compile-like (format "%s.compile" module))))


;;; Internals

(defun mill-modules ()
  "Return as a list of strings the Mill modules in the currenct project."
  (set-difference (mill-run-mill-to-list "resolve" "_") mill-top-level-commands :test #'equal))

(defun mill-set-current-module ()
  "Ask the user to choose the current module from those available, and record that decision in `mill-current-module'. Return the user's a choice, a string."
  (let ((module (completing-read "Choose a module: " (mill-modules))))
    (setq-local mill-current-module module)
    module))

(defun mill-get-or-choose-current-module ()
  "Get the current value of `mill-current-module`, unless it is nil in which case prompt the user to choose a module."
  (or mill-current-module (mill-set-current-module)))


(provide 'mill-mode)
;;; mill-mode.el ends here
