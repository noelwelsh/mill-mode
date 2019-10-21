;;; mill-mode.el --- Interactive support for Mill projects
;; -*- lexical-binding- t; -*-

;;; Commentary:
;;;
;;; Major mode for working with Mill. Loosely based on sbt-mode.
;;;
;;; Code:

(require 'compile)
(require 'shell)

(defcustom mill-program-name "mill"
  "Name of the executable to run Mill."
  :type 'string
  :group 'mill)

(defcustom mill-buffer-name-base "*mill*"
  "Buffer name for Mill"
  :type 'string
  :group 'mill)

(defconst mill-top-level-commands
  '("all" "resolve" "inspect" "show" "shutdown" "path" "plan" "version" "visualize" "visualizePlan" "clean")
  "List of mill commands that can be run without specifying a project.")

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
    ))

(defvar-local mill-current-module nil
  "The current Mill module to use when issuing user commands.
  Nil if no project has been selected yet, a String otherwise.")


;;; Interactive functions

(defun mill-compile-current-module ()
  "Compile the current module, displaying the output in the Mill buffer."
  (interactive)
  (let ((module (mill-get-or-choose-current-module))
        (dir (mill-find-root)))
    (compilation-start (format "cd %s; %s %s.compile" dir mill-program-name module) 'mill-compilation-mode)))


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

(defun mill-run-mill (&rest commands)
  "Run Mill with the given strings COMMANDS, displaying output in a buffer called *mill*projectdir."
  (let* ((project-root (mill-find-root))
         (buffer-name (mill-buffer-name)))
    (with-current-buffer buffer-name
      (erase-buffer)
      (cd project-root)
      (insert mill-program-name " " (string-join commands " ") "\n")
      (let ((process (apply 'start-process "mill" buffer-name mill-program-name commands)))
        (display-buffer (current-buffer))
        (require 'shell)
        (shell-mode)
        (set-process-filter process 'comint-output-filter)))))

(defun mill-run-mill-to-list (&rest commands)
  "Run Mill with the given strings COMMANDS, collecting the output to a list of string. The --disable-ticker option is always included, so ticker output will not be in the result."
  (let* ((project-root (mill-find-root)))
    (add-to-list 'commands "--disable-ticker")
    (with-temp-buffer
      (cd project-root)
      (apply 'process-lines mill-program-name commands))))

(defun mill-find-root ()
  "Find the root directory of the Mill project.

Starting from the current `default-directory`, find a parent
directory that is a Mill root. A Mill root directory is a
directory containing a build.sc.

Returns the directory or nil if not found."
  (locate-dominating-file default-directory "build.sc"))

(defun mill-buffer-name ()
  "Return the buffer name, a string, for running Mill."
  (format "%s<%s>"
          mill-buffer-name-base
          (abbreviate-file-name (mill-find-root))))

(define-compilation-mode mill-compilation-mode "Compile current module using mill"
  (set (make-local-variable 'compilation-error-regexp-alist)
       mill-compilation-regexp))



(provide 'mill-mode)
;;; mill-mode.el ends here
