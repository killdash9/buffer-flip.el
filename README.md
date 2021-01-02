buffer-flip.el
=================

This package streamlines the operation of switching between recent
buffers, with an emphasis on minimizing keystrokes.  Inspired by the
Alt-Tab convention in Windows, it keeps the most recently used buffers
on the top of the stack.

Installation
------------

buffer-flip is available on Melpa.

[![MELPA](https://melpa.org/packages/buffer-flip-badge.svg)](https://melpa.org/#/buffer-flip)

### Installing with package-install

    M-x package-install RET buffer-flip RET

Then add the following to your config, adapting to your preferences.

```lisp
;; key to begin cycling buffers.  Global key.
(global-set-key (kbd "M-<tab>") 'buffer-flip)
    
;; transient keymap used once cycling starts
(setq buffer-flip-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-<tab>")   'buffer-flip-forward) 
        (define-key map (kbd "M-S-<tab>") 'buffer-flip-backward)
        (define-key map (kbd "M-ESC")     'buffer-flip-abort)
        map))

;; buffers matching these patterns will be skipped
(setq buffer-flip-skip-patterns 
      '("^\\*helm\\b"
        "^\\*swiper\\*$"))
```

### Installing with [use-package](https://github.com/jwiegley/use-package)

```lisp
(use-package buffer-flip
  :ensure t
  :bind  (("M-<tab>" . buffer-flip)
          :map buffer-flip-map
          ( "M-<tab>" .   buffer-flip-forward) 
          ( "M-S-<tab>" . buffer-flip-backward) 
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))
```

Usage example
-------------

The following assumes the above key bindings. 

To begin cycling through the buffers, press <kbd>Alt-Tab</kbd>.  This
begins the flipping process by switching to the most recently used
buffer.  At this point the transient buffer-flip-map is active.
Pressing <kbd>Alt-Tab</kbd> will continue to cycle through the buffer
stack, more recent buffers first.  Pressing <kbd>Alt-Shift-Tab</kbd>
will cycle in the opposite direction.  Just begin working in the
currently-displayed buffer to stop cycling.  Doing so places that
buffer on top of the stack.  <kbd>Alt-Esc</kbd> cancels cycling and
switches to the buffer you started in.

| Pressing                                                                    | Does this                                                                         |
|-----------------------------------------------------------------------------|-----------------------------------------------------------------------------------|
| <kbd>Alt-Tab</kbd>                                                          | Flips to second most recent buffer                                                |
| <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd>                                       | Flip to third most recent buffer                                                  |
| <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd> <kbd>Alt-Esc</kbd> | Start flipping through buffers and then cancel, returning to the original buffer. |
| <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd> <kbd>Alt-Shift-Tab</kbd>              | Flips forward through the two most recent buffers, then flips backward one buffer.|


Another good key binding
------------------------

Personally I use the following key bindings which rely on key-chord.
I like it because I can reach the keys easily in home position.

```lisp
(use-package buffer-flip
  :ensure t
  :chords (("u8" . buffer-flip))
  :bind  (:map buffer-flip-map
               ( "8" .   buffer-flip-forward) 
               ( "*" .   buffer-flip-backward) 
               ( "C-g" . buffer-flip-abort)))
```

With these bindings,

| Pressing                                               | Does this                                                                         |
|--------------------------------------------------------|-----------------------------------------------------------------------------------|
| <kbd>u8</kbd>                                          | Alternates between the two most recent buffers                                    |
| <kbd>u8</kbd> <kbd>8</kbd>                             | Flip to third most recent buffer                                                  |
| <kbd>u8</kbd> <kbd>8</kbd> <kbd>8</kbd> <kbd>C-g</kbd> | Start flipping through buffers and then cancel, returning to the original buffer. |
| <kbd>u8</kbd> <kbd>8</kbd> <kbd>*</kbd>                | Flips forward through the two most recent buffers, then flips backward one buffer.|

This is incidentally the default key binding for previous versions of
this package.  The current version has no default key binding.

There is a functional difference between this binding and the Alt-Tab
version above, which is that pressing the <kbd>u8</kbd> chord
repeatedly will alternate between the two most recent buffers because
it restarts the cycling process each time by invoking the
`buffer-flip` command.  In contrast, pressing <kbd>Alt-Tab</kbd>
repeatedly cycles through deeper and deeper buffers on the stack.
This is because <kbd>Alt-Tab</kbd> is bound to both `buffer-flip` and
`buffer-flip-forward`.  The first press puts you into cycling mode,
and subsequent presses cycle forward.


buffer-flip-other-window 
------------------------

A common operation in my work flow is to switch to the other window
buffer before flipping buffers.  That is, I want to keep my current
buffer on top, but I want to get to some buried buffer in the other
window.  `buffer-flip-other-window` does just that.  If there is only
one window, the function will split the window automatically.  It's a
little like `pop-to-buffer` for buffer-flipping.  It can be bound to
its own keystroke, or can be invoked by calling `buffer-flip` with a
prefix arg.

The (Non) UI
-------------

Or, "Why don't you have a screenshot?"  This package streamlines the
operation of switching to recent buffers, a common operation in my
workflow.  Many buffer management systems display a list of buffer
names for you to select from.  Extra ui elements like that often come
at the cost of additional keystrokes.  This package doesn't have any
ui elements, it simply changes the current buffer as you cycle.  Once
you are looking at the buffer you want, just start editing and the
cycling automatically exits.  Pressing the key bound to
`buffer-flip-abort` during cycling will take you back to where you
started.

This package is not efficient for switching to a deeply-buried buffer.
There are
[other](http://tuhdo.github.io/helm-intro.html#ID-0386c827-7f5d-4056-bf4d-8d0fc01fc1ab)
[tools](http://www.gnu.org/software/emacs/manual/html_mono/ido.html)
for that.

Motivation
-----------

The [Alt-Tab](https://en.wikipedia.org/wiki/Alt-Tab) convention for
switching windows was one thing Microsoft got right.  Because it keeps
the most recently-used things on the top of the stack, it is often
very fast to switch to the thing you want.  There are [similar Emacs
packages](http://www.emacswiki.org/emacs/ControlTABbufferCycling) out
there, but many are too heavyweight for my needs, or are not
stack-based.

A note on the buffer stack
--------------------------

There is no additional buffer stack maintained by this package.  Emacs
already keeps its buffers in a stack, and this package leverages that
fact.  You can see the Emacs buffer stack by running `M-x
list-buffers` or by evaluating `(buffer-list)`.