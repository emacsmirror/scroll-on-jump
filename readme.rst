
##############
Scroll on Jump
##############

This package allows you to control the scrolling on any operation that jumps to a new location.

- Re-centers vertical scrolling.
- Optionally animate the scroll using a fixed time limit.

Since this might not always be desired, this can be applied using advice or individual key bindings,
if you prefer to limit the changes to certain shortcuts.

Available via `melpa <https://melpa.org/#/scroll-on-jump>`__.

`See demo video <https://youtu.be/7krbhASqwLY>`__.


Motivation
==========

Actions that move the point may move the cursor to the edge of the screen,
while having a large scroll-margin can help, this isn't always convenient
as it can be useful to move the cursor to the screen edge.


Features
========

Animated Scrolling
   You can optionally jump to the new location in multiple steps (lines/pixels)
   this can help to give context when moving around the document.
Responsive Animation
   To avoid animation display slowing you down - any input will cancel the animation
   so multiple key strokes won't lag behind display updates.
Un-intrusive
   This only exposes functionality you can make use of,
   without interfering with anything you don't want it to.


Usage
=====

The following functions are exposed.

``scroll-on-jump``
   Macro that behaves like ``progn``,
   any code that runs in the body of this macro will perform scrolling when moving the point.
``scroll-on-jump-interactive``
   Use this to wrap interactive function (intended to be used in key bindings).
``scroll-on-jump-advice-add``
   Adds advice so scrolling is performed whenever the function is called.
``scroll-on-jump-advice-remove``
   Remove the advice added to the function.

Commands that work well include:

- Jump to search result, paragraph, function ... etc.
- Undo/redo.
- Go to declaration.


Wrapping Commands That Scroll
-----------------------------

If a command it's self sets a new scroll location,
these can be wrapped using ``scroll-on-jump-with-scroll-`` prefix,
so ``scroll-on-jump-with-scroll-interactive``, ``scroll-on-jump-with-scroll-advice-add`` .. etc.

In this case the newly set scroll location will be used when displaying the animation.


Key Binding Example
-------------------

Take this example of pressing ``Ctrl-Z`` for undo.

Using ``scroll-on-jump-interactive`` makes this use ``scrol-on-jump`` features.

Before:

.. code-block:: elisp

   (global-set-key (kbd "<C-z>") 'undo)

After:

.. code-block:: elisp

   (global-set-key (kbd "<C-z>") (scroll-on-jump-interactive 'undo))


Advice Example
--------------

Use this with care, since it advises all uses of the function.

.. code-block:: elisp

   (scroll-on-jump-advice-add forward-paragraph)
   (scroll-on-jump-advice-add backward-paragraph)


Macro Example
-------------

When already using a ``lambda`` in a key-binding, it's simplest to wrap this in a macro.


Before:

.. code-block:: elisp

   (global-set-key (kbd "<f12>")
     (lambda () (interactive) (my-function)))

After:

.. code-block:: elisp

   (global-set-key (kbd "<f12>")
     (lambda () (interactive) (scroll-on-jump (my-function))))


Complete Example
----------------

Here is a more complete example for evil-mode users.

.. code-block:: elisp

   (with-eval-after-load 'evil
     (scroll-on-jump-advice-add evil-undo)
     (scroll-on-jump-advice-add evil-redo)
     (scroll-on-jump-advice-add evil-jump-item)
     (scroll-on-jump-advice-add evil-jump-forward)
     (scroll-on-jump-advice-add evil-jump-backward)
     (scroll-on-jump-advice-add evil-ex-search-next)
     (scroll-on-jump-advice-add evil-ex-search-previous)
     (scroll-on-jump-advice-add evil-forward-paragraph)
     (scroll-on-jump-advice-add evil-backward-paragraph)
     (scroll-on-jump-advice-add evil-goto-mark)

     ;; Actions that themselves scroll.
     (scroll-on-jump-with-scroll-advice-add evil-goto-line)
     (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
     (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
     (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
     (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
     (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

   (with-eval-after-load 'goto-chg
     (scroll-on-jump-advice-add goto-last-change)
     (scroll-on-jump-advice-add goto-last-change-reverse))

   (global-set-key (kbd "<C-M-next>") (scroll-on-jump-interactive 'diff-hl-next-hunk))
   (global-set-key (kbd "<C-M-prior>") (scroll-on-jump-interactive 'diff-hl-previous-hunk))


Customization
=============

While the defaults seem to work well, these values can be customized.

``scroll-on-jump-duration``: ``0.4``
   The duration (in seconds) for jumping to take, set to ``0.0`` to jump immediately.
``scroll-on-jump-smooth``: ``t``
   When not nil, use smooth scrolling (by pixels), otherwise scroll by lines.
``scroll-on-jump-curve``: ``'smooth``
   Apply a curve to the scroll speed, starting and ending slow.

   :'smooth: Ease in/out.
   :'smooth-in: Ease in (end slow).
   :'smooth-out: Ease in (start slow).
   :'linear: Linear motion (no easing).
``scroll-on-jump-curve-power``: ``3.0``
   The strength of the curve (when non-linear).

   Values between 2 and 8 work well.

   - Below 2.0 approaches a linear curve.
   - Above 8.0 can make the motion overly abrupt.
``scroll-on-jump-mode-line-format``: ``nil``
   When non-nil, use this value for the ``mode-line-format`` while scrolling.
   This can be used to temporarily override the mode-line while scrolling.
   It can also help to avoid overly complex mode-lines from slowing down scrolling.


Installation
============

Until this is available on melpa, straight can be used to install this package.

.. code-block:: elisp

   (use-package scroll-on-jump
     :config
     (setq scroll-on-jump-duration 0.6))


Limitations
===========

- Any commands that themselves scroll to a new location *and* modify the buffer will not work as expected
  (they may scroll too far for example).
