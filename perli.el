;;; perli.el --- Perli(Multi-platform Perl REPL) from Emacs

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'comint)
(require 'cperl-mode)

(defgroup perli nil
  "Run a perli process in a buffer"
  :group 'perl)

(defcustom inferior-perli-mode-hook nil
  "Hook for customizing inferior-perli mode"
  :type 'hook
  :group 'perli)

;;(defvar inferior-perli-mode-map
;;  (let ((map (make-sparse-keymap)))
;;    (define-key map (kbd "C-c C-r") 'perli-send-region)
;;    (define-key map (kbd "C-c C-z") 'switch-to-perli)
;;    map))

(define-derived-mode inferior-perli-mode comint-mode "Inferior perli"
  "Major mode for interacting with an inferior perli process"
  (setq comint-prompt-regexp "^[0-9]+> *")
  (setq mode-line-process '(":%s"))
  (setq comint-input-filter 'perli--input-filter))

(defcustom inferior-perli-filter-regexp "\\`\\s-+"
  "Regular expression of input filter"
  :type 'regexp
  :group 'perli)

(defvar perli--command "perli")
(defvar perli--buffer nil)
(defvar perli--program-name "perli")

(defun perli--input-filter (str)
  (not (string-match inferior-perli-filter-regexp str)))

(defvar perli--buffer nil)

(defun perli-proc ()
  (unless (and perli--buffer
               (get-buffer perli--buffer)
               (comint-check-proc perli--buffer))
    (perli-interactively-start-process))
  (or (perli--get-process)
      (error "No current process. See variable `perli--buffer'")))

(defun perli--get-process ()
  (let ((buf (if (eq major-mode 'inferior-perli-mode)
                 (current-buffer)
               perli--buffer)))
    (get-buffer-process buf)))

(defun perli-interactively-start-process (&optional _cmd)
  (save-window-excursion
    (run-perli (read-string "Run perli: " perli--program-name))))

;;;###autoload
(defun run-perli (cmd)
  (interactive
   (list (if current-prefix-arg
             (read-string "Run Perli: " perli--command)
           perli--command)))
  (when (not (comint-check-proc "*perli*"))
    (let ((cmdlist (split-string-and-unquote cmd)))
      (set-buffer (apply 'make-comint "perli" (car cmdlist) nil
                         (cdr cmdlist)))
      (inferior-perli-mode)))
  (setq perli--program-name cmd)
  (setq perli--buffer "*perli*")
  (pop-to-buffer-same-window "*perli*"))

(defun switch-to-perli (eob-p)
  (interactive "P")
  (let ((win (get-buffer-window perli--buffer)))
    (if win
        (select-window win)
      (switch-to-buffer (get-buffer-create perli--buffer))))
  (unless (and perli--buffer
               (get-buffer perli--buffer)
               (process-live-p (get-buffer-process perli--buffer)))
    (perli-interactively-start-process))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defsubst perli--remove-newlines (str)
  (replace-regexp-in-string "\r?\n?$" " " str))

(defun perli-send-region (start end)
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (comint-send-string (perli-proc) (perli--remove-newlines str))
    (comint-send-string (perli-proc) "\n")))

(defun perli-send-region-and-go (start end)
  (interactive "r")
  (perli-send-region start end)
  (switch-to-perli t))

(defun perli-other-window ()
  "Run perli in other window, reusing existing buffer/window if available."
  (interactive)
  (let* ((buf (get-buffer "*perli*"))
         (proc (and buf (get-buffer-process buf)))
         (alive (and proc (process-live-p proc)))
         (win (and buf (get-buffer-window buf))))
    ;; Navigate to the buffer/window
    (cond
     (win              (select-window win))
     (t                (switch-to-buffer-other-window (get-buffer-create "*perli*"))))
    ;; Start perli if not running
    (unless alive
      (run-perli "perli"))))

(defun perli-send-buffer ()
  "Send the entire buffer to the perli process."
  (interactive)
  (perli-send-region (point-min) (point-max)))

(defun perli-send-line ()
  "Send the current line to the perli process."
  (interactive)
  (perli-send-region (line-beginning-position) (line-end-position)))

(defun perli-send-defun ()
  "Send the defun at point to the perli process."
  (interactive)
  (save-excursion
    (let ((end (progn (end-of-defun) (point)))
          (start (progn (beginning-of-defun) (point))))
      (perli-send-region start end))))

;; Expression navigation adapted from perl-repl.el
;; by Francisco Jurado (https://github.com/quicoju/perl-repl-el)

(defun perli--point-at-expr-boundary-p ()
  (memq 59 (cperl-after-expr-p)))

(defun perli--end-of-expression-p ()
  (and (perli--point-at-expr-boundary-p)
       (eq (char-before) ?\;)))

(defun perli--backward-to-start-of-expr ()
  (condition-case nil
      (progn
        (while (or (not (perli--point-at-expr-boundary-p))
                   (nth 3 (syntax-ppss)))
          (backward-char)
          (skip-chars-backward "$@%#")))
    (error nil)))

(defun perli--forward-to-end-of-expr ()
  (condition-case nil
      (progn
        (while (or (not (perli--end-of-expression-p))
                   (nth 3 (syntax-ppss)))
          (forward-char)
          (skip-chars-forward "$@%#")))
    (error nil)))

(defun perli-send-expression ()
  "Send the current expression to the perli process."
  (interactive)
  (save-excursion
    (perli--forward-to-end-of-expr)
    (if (> (current-column) 1)
        (backward-char))
    (let ((end (point)))
      (perli--backward-to-start-of-expr)
      (perli-send-region (point) end))))

(provide 'perli)

;;; perli.el ends here
