buffer-flip.el
=================

Provides a global minor mode that lets you flip through buffers like
ALT-TAB in Windows, keeping the most recently used buffers on the top
of the stack.  Depends on
[`key-chord`](https://melpa.org/#/key-chord).

Why?
----

The ALT-TAB convention for switching windows was one thing Microsoft
got right.  Because it keeps the most recently-used things on the top
of the stack, it is very often very fast to switch to the thing you
want.  There are similar Emacs packages out there, but nothing quite
did what I want, so I wrote this.  It's simple and less than 40 lines
of actual code.  Most of the file is documentation.

How to use it
-------------

By default, the key chord to begin flipping through buffers is `u8`.
You can customize these keys with the variable `buffer-flip-keys`.

`u` and `8` are roughly analogous to `ALT` and `tab`, respectively.
To begin cycling through the buffers, press `u` and `8` at the same
time or in rapid succession, `key-chord` style.  This will flip to the
second buffer in the stack returned by `(buffer-list)`.  Repeatedly
pressing `8` will continue to cycle through the buffers.  Pressing `*`
(`shift-8`) will cycle in the opposite direction.  `C-g` will bring
you back to buffer you were in before cycling, analagous to `ESC` when
cycling in Windows.  Once you press any other key, cycling exits.  So,
pressing `u8` repeatedly will toggle between the top two buffers.
Pressing `u88` will go to the third buffer from the top of the stack.

