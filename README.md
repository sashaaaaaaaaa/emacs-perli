# perli.el
**perli.el** is Emacs front end of [Perli](https://github.com/mklement0/perli).


## screenshot

![git-messenger.el](image/reply1.png)


## Basic Usage

### `run-perli`

Run perli REPL.

### `perli-other-window`

Run perli REPL in other window.

### `switch-to-perli`

Switch to the perli buffer. Reuses an existing perli window 
if visible, otherwise switches in current window.

### `perli-send-region`

Send region to perli REPL process.

### `perli-send-line`

Send line to perli REPL process.

### `perli-send-buffer`

Send buffer to perli REPL process.

### `perli-send-defun`

Send defun at point to perli REPL process.

### `perli-send-expression`

Send expression to perli REPL process.



## Sample Configuration

```lisp
(require 'perli)

(require 'cperl-mode)
(define-key cperl-mode-map (kbd "C-c C-r") 'perli-send-region)
(define-key cperl-mode-map (kbd "C-c C-z") 'switch-to-perli)
```
