;;; ert-runner-mode.el --- ert-runner emacs integration

;; Copyright © 2014 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/ert.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (pkg-info "0.4"))
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  ert-runner emacs integration

;;; Code:

(require 'compile)
(require 'ansi-color)

(defvar ert-runner-mode-command "cask exec ert-runner"
  "")

(defvar ert-runner-mode--project-root-indicators
  '("Cask")
  "list of file-/directory-names which indicate a root of a emacs lisp project")

(defun ert-runner-mode-project-root ()
  (let ((file (file-name-as-directory (expand-file-name default-directory))))
    (ert-runner-mode--project-root-identifier file ert-runner-mode--project-root-indicators)))

(defun ert-runner-mode--project-root-identifier (file indicators)
  (let ((root-dir (if indicators (locate-dominating-file file (car indicators)) nil)))
    (cond (root-dir (directory-file-name (expand-file-name root-dir)))
          (indicators (ert-runner-mode--project-root-identifier file (cdr indicators)))
          (t nil))))

(defun ert-runner-mode--establish-root-directory ()
  "Set the default-directory to the emacs lisp package project root."
  (let ((project-root (ert-runner-mode-project-root)))
    (if (not project-root)
        (error "Couldn't find any project root")
      (setq default-directory project-root))))

(defvar ert-runner-mode--buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'ert-runner-mode--buffer-name)

(defvar ert-runner-mode--error-link-options
  '(ert-runner-mode "\\([-A-Za-z0-9./_]+\\):\\([0-9]+\\)\\(: warning\\)?" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")

(defun ert-runner-mode--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(define-compilation-mode ert-runner-buffer-mode "ert-runner"
  "ert-runner compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^Finished in .*$" . font-lock-string-face)
                              ("^ert-runner.*$" . font-lock-string-face)))
    ;; Set any bound buffer name buffer-locally
    (setq ert-runner-mode--buffer-name ert-runner-mode--buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'ert-runner-mode--kill-any-orphan-proc)))

(defvar ert-runner-mode--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defun ert-runner-mode--handle-compilation-once ()
  (remove-hook 'compilation-filter-hook 'ert-runner-mode--handle-compilation-once t)
  (delete-matching-lines "\\(-*- mode:\\|^$\\|karma run\\|Loading config\\|--no-single-run\\|Karma finished\\|Karma started\\|karma-compilation;\\)"
                         (point-min) (point)))

(defun ert-runner-mode--handle-compilation ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun ert-runner-mode-compilation-run (cmdlist buffer-name)
  "Run CMDLIST in `buffer-name'.
Returns the compilation buffer.
Argument BUFFER-NAME for the compilation."
  (save-some-buffers (not compilation-ask-about-save) ert-runner-mode--save-buffers-predicate)
  (let* ((ert-runner-mode--buffer-name buffer-name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (compilation-start (mapconcat 'concat cmdlist " ")
                           'ert-runner-buffer-mode
                           (lambda (b) ert-runner-mode--buffer-name))
      (setq-local compilation-error-regexp-alist-alist
                  (cons ert-runner-mode--error-link-options compilation-error-regexp-alist-alist))
      (setq-local compilation-error-regexp-alist (cons 'ert-runner-mode compilation-error-regexp-alist))
      (add-hook 'compilation-filter-hook 'ert-runner-mode--handle-compilation nil t)
      (add-hook 'compilation-filter-hook 'ert-runner-mode--handle-compilation-once nil t))))

(defun ert-runner-mode--flatten (alist)
  (cond ((null alist) nil)
        ((atom alist) (list alist))
        (t (append (ert-runner-mode--flatten (car alist))
                   (ert-runner-mode--flatten (cdr alist))))))


(defun ert-runner-mode--build-runner-cmdlist (command)
  "Build the commands list for the runner."
  (remove "" (ert-runner-mode--flatten
              (list (if (stringp command)
                        (split-string command)
                      command)))))

(defun ert-runner-mode-run ()
  (interactive)
  (ert-runner-mode-execute (list "")
                 "*ert-runner*"))

(defun ert-runner-mode-execute (cmdlist buffer-name)
  "Run a karma command."
  (let ((old-directory default-directory))
    (ert-runner-mode--establish-root-directory)
    (cd default-directory)
    (ert-runner-mode-compilation-run (ert-runner-mode--build-runner-cmdlist (list ert-runner-mode-command cmdlist))
                           buffer-name)
    (cd old-directory)))


;;;###autoload
(defun ert-runner-mode-version (&optional show-version)
  "Get the ert-runner-mode version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'ert-runner-mode)))
    (when show-version
      (message "ert-runner-mode version: %s" version))
    version))

(provide 'ert-runner-mode)

;;; ert-runner-mode.el ends here
