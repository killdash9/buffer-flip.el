buffer-flip.el
=================

Provides a global minor mode that lets you flip through buffers like
Alt-Tab in Windows, keeping the most recently used buffers on the top
of the stack.  Depends on
[`key-chord`](https://melpa.org/#/key-chord).

Motivation
-----------

The [Alt-Tab](https://en.wikipedia.org/wiki/Alt-Tab) convention for
switching windows was one thing Microsoft got right.  Because it keeps
the most recently-used things on the top of the stack, it is very
often very fast to switch to the thing you want.  There are
[similar Emacs packages](http://www.emacswiki.org/emacs/ControlTABbufferCycling)
out there, but many are too heavyweight for my taste, involve new UI
elements, or are not stack-based.  My implementation is very simple
and lightweight -- less than 40 lines of actual code.

The (Non) UI
-------------

Or, "Why don't you have a screenshot?"  This package streamlines the
operation of switching between the most recent buffers, a common
operation in my workflow.  Many buffer management systems display a
list of buffer names for you to select from.  Extra ui elements like
that often come at the cost of additional keystrokes.  There is no
such buffer list here.  It simply switches the current buffer as you
cycle.  Once you are looking at the buffer you want, just start
working and the cycling automatically exits.  Pressing C-g during
cycling will take you back to the pre-cycling buffer.

This package is not efficient for switching to a deeply-buried buffer.
There are
[other](http://tuhdo.github.io/helm-intro.html#ID-0386c827-7f5d-4056-bf4d-8d0fc01fc1ab)
[tools](http://www.gnu.org/software/emacs/manual/html_mono/ido.html)
for that.

Key Bindings
-------------

By default, the key chord to begin flipping through buffers is `u8`.
You can customize these keys with the variable `buffer-flip-keys`.

`u` and `8` are roughly analogous to `Alt` and `Tab`, respectively.
To begin cycling through the buffers, press `u` and `8` at the same
time or in rapid succession, `key-chord` style.  This will flip to the
second buffer in the stack returned by `(buffer-list)`.  Repeatedly
pressing `8` will continue to cycle through the buffers.  Pressing `*`
(`shift-8`) will cycle in the opposite direction.  Just begin working
in a buffer to stop cycling.  `C-g` cancels cycling and restores the
buffer you were in before cycling began, analagous to `Esc` when
cycling in Windows.
