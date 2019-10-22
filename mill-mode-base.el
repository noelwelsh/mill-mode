;;; mill-mode-base.el --- Basic utilities for working with Mill
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Code:

(require 'shell)

(defcustom mill-program-name "mill"
  "Name of the executable to run Mill."
  :type 'string
  :group 'mill)

(defcustom mill-buffer-name-base "*mill*"
  "Buffer name for Mill"
  :type 'string
  :group 'mill)

(defun mill-run-mill (&rest commands)
  "Run Mill with the given strings COMMANDS, displaying output in a buffer called *mill*projectdir."
  (let* ((project-root (mill-find-root)))
    (with-current-buffer (mill-get-buffer)
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

(defun mill-get-buffer ()
  "Return the Mill buffer, creating it if needed."
  (get-buffer-create (mill-buffer-name)))

(provide 'mill-mode-base)
;;; mill-mode-base.el ends here
