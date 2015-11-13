;;; buffer-flip.el --- Use key-chord to cycle through buffers like Alt-Tab in Windows
;; Copyright (C) 2015 Russell Black

;; Author: Russell Black (killdash9@github)
;; Keywords: convenience
;; URL: https://github.com/killdash9/buffer-flip.el
;; Created: 10th November 2015
;; Version: 20151110
;; Package-Requires: ((key-chord "20150808"))

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
;; Lets you flip through buffers like Alt-Tab in Windows.

;;; Code:
(require 'key-chord)

(defvar buffer-flip-mode-map '(keymap)
  "The mode map for `buffer-flip-mode'.")

;;;###autoload
(define-minor-mode buffer-flip-mode
  "A global minor mode that lets you flip through buffers like
Alt-Tab in Windows, keeping the most recently used buffers on
the top of the stack.  Depends on `key-chord-mode'.

By default, the key chord to begin flipping through buffers is
\"u8\".  You can customize these keys with the variable
`buffer-flip-keys'.

\"u\" and \"8\" are roughly analogous to Alt and Tab,
respectively.  To begin cycling through the buffers, press u and
8 at the same time or in rapid succession, `key-chord' style.
This will flip to the second buffer in the stack returned by
`buffer-list'.  Repeatedly pressing \"8\" will continue to cycle
through the buffers.  Pressing * (shift-8) will cycle in the
opposite direction.  Just begin working in a buffer to stop
cycling.  C-g cancels cycling and restores the buffer you were in
before cycling, analagous to Esc when cycling in Windows."
  :global t :keymap buffer-flip-mode-map
  (when buffer-flip-mode
    (unless key-chord-mode
      (when (yes-or-no-p "key-chord-mode must be enabled for buffer-flip-mode to work.  Enable key-chord-mode?")
        (key-chord-mode 1)))))

(defun buffer-flip-set-keys (symbol value)
  "Set keys used by buffer-flip.
Called from variable customization.  SYMBOL is
`buffer-flip-keys', and the keys in VALUE are used to register
key bindings in `buffer-flip-mode-map'."
  (when (not (and (= 3 (length value)) (or (stringp value))))
    (user-error "buffer-flip-keys must be a three character string"))
  (set-default symbol value)
  ;; empty the mode map to clear out previous bindings
  (setcdr buffer-flip-mode-map nil)
  (key-chord-define buffer-flip-mode-map (substring value 0 2) 'buffer-flip))

(defcustom buffer-flip-keys "u8*"
  "Keys for flipping through buffers.
The first two characters form the key-chord that begins buffer
cycling.  The second character pressed on its own continues
cycling in the forward direction.  The third character cycles in
the backward direction.  This would typically be the shifted
version of the second character.  These may not be modifier keys,
and because of a restriction in key-chord, these must be
characters between 32 and 126.  Choose a key combination not
likely to be used in succession in normal editing."
  :set 'buffer-flip-set-keys :type '(string) :group 'buffer-flip)

(defun buffer-flip ()
  "Begins the process of flipping through buffers.
See `buffer-flip-mode' for more information."
  (interactive)
  (lexical-let*
      ((buffer-list (buffer-list))
       (index 0)                  ; The current index into buffer-list
       (cycle ; a function which will be invoked by the transient map below
        (lambda () (interactive)
          ;; switch to the next non-minibuffer buffer that is not in
          ;; another window
          (loop with buf
                do (setq index
                         (mod (+ index (if (cl-find (elt buffer-flip-keys 2)
                                                    (this-command-keys)) -1 1))
                              (length buffer-list)))
                do (setq buf (nth index buffer-list))
                while (or (get-buffer-window buf) (minibufferp buf))
                finally (switch-to-buffer buf t)))))
    (funcall cycle) ; first buffer flip
    (set-transient-map                 ; read flip key or exit cycling
     `(keymap (,(elt buffer-flip-keys 1) . ,cycle)  ; cycle forward
              (,(elt buffer-flip-keys 2) . ,cycle)) ; cycle backward
     t (lambda () (switch-to-buffer (if (cl-find 7 (this-command-keys)) 
                                   (car buffer-list) ; C-g restores original buffer
                                        ; Any other key places current buffer
                                        ; on top of stack
                                 (current-buffer))))))) 

(provide 'buffer-flip)
;;; buffer-flip.el ends here
