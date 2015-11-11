;;; buffer-flip.el -- Cycle through buffers like ALT-TAB in Windows.
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
;; Lets you flip through buffers like ALT-TAB in Windows.

;;; Code:
(require 'key-chord)

(defvar buffer-flip-mode-map '(keymap)
  "The mode map for `buffer-flip-mode'.")

(define-minor-mode buffer-flip-mode
  "A global minor mode that lets you flip through buffers like
ALT-TAB in Windows, keeping the most recently used buffers on
the top of the stack.  Depends on `key-chord'.

By default, the key chord to begin flipping through buffers is
\"u8\".  You can customize these keys with the variable
`buffer-flip-keys'.

\"u\" and \"8\" are roughly analogous to ALT and tab,
respectively.  To begin cycling through the buffers, press u and
8 at the same time or in rapid succession, `key-chord' style.
This will flip to the second buffer in the stack returned by
`buffer-list'.  Repeatedly pressing \"8\" will continue to cycle
through the buffers.  Pressing * (shift-8) will cycle in the
opposite direction.  C-g will bring you back to buffer you were
in before cycling, analagous to ESC when cycling in Windows.
Once you press any other key, cycling exits.  So, pressing \"u8\"
repeatedly will toggle between the top two buffers.  Pressing
\"u88\" will go to the third buffer from the top of the stack."
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
    (user-error "This must be a three character string"))
  (set-default symbol value)
  ;; empty the mode map to clear out previous bindings
  (setcdr buffer-flip-mode-map nil)
  (key-chord-define buffer-flip-mode-map (substring value 0 2) 'buffer-flip))

(defcustom buffer-flip-keys "u8*"
  "Keys for flipping through buffers.
The first character functions as the ALT in ALT-TAB, and the
second character functions as the TAB.  The third character
functions as SHIFT-TAB.  This would typically be the shifted
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
       (index 0)
       (cycle
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
    (funcall cycle)
    (set-transient-map
     `(keymap (,(elt buffer-flip-keys 1) . ,cycle)  ; cycle forward
              (,(elt buffer-flip-keys 2) . ,cycle)) ; cycle backward
     t (lambda () (switch-to-buffer (if (cl-find 7 (this-command-keys)) ; C-g resets
                                   (car buffer-list) (current-buffer)))))))

(provide 'buffer-flip)
;;; buffer-flip.el ends here