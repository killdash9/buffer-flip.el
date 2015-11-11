buffer-flip.el
=================

Provides a global minor mode that lets you flip through buffers like
Alt-Tab in Windows, keeping the most recently used buffers on the top
of the stack.  Depends on
[`key-chord`](https://melpa.org/#/key-chord).

Motivation
----------

The Alt-Tab convention for switching windows was one thing Microsoft
got right.  Because it keeps the most recently-used things on the top
of the stack, it is very often very fast to switch to the thing you
want.  There are
[similar Emacs packages](http://www.emacswiki.org/emacs/ControlTABbufferCycling)
out there, but many are too heavyweight for my taste, involve new UI
elements, or are not stack-based.  My implementation is very simple
and lightweight -- less than 40 lines of actual code.

How to use it
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
