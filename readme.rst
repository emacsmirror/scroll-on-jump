
##############
Scroll on Jump
##############

This package allows you to control the scrolling on any operation that jumps to a new location.

- Re-centers vertical scrolling.
- Optionally animate the scroll using a fixed time limit.

Since this might not always be desired, this can be applied using advice or individual key bindings,
if you prefer to limit the changes to certain shortcuts.

.. Available via `melpa <https://melpa.org>`__.

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
     (scroll-on-jump-advice-add evil-backward-paragraph))

   (with-eval-after-load 'goto-chg
     (scroll-on-jump-advice-add goto-last-change)
     (scroll-on-jump-advice-add goto-last-change-reverse))

   (global-set-key (kbd "<C-M-next>") (scroll-on-jump-interactive 'diff-hl-next-hunk))
   (global-set-key (kbd "<C-M-prior>") (scroll-on-jump-interactive 'diff-hl-previous-hunk)))


Customization
=============

While the defaults seem to work well, these values can be customized.

``scroll-on-jump-duration``: 0.4
   The duration for jumping to take, set to ``0.0`` to jump immediately.
``scroll-on-jump-smooth``: t
   When not nil, use smooth scrolling (by pixels).
``scroll-on-jump-use-curve``
   Apply a curve to the scroll speed, starting and ending slow.


Installation
============

Until this is available on melpa, straight can be used to install this package.

.. code-block:: elisp

   (use-package scroll-on-jump
     :config
     (setq scroll-on-jump-duration 0.6)

     :straight
     (scroll-on-jump
       :type git
       :host gitlab
       :repo "ideasman42/emacs-scroll-on-jump"))


Limitations
===========

- Any commands that themselves scroll to a new location will not work as expected
  (they may scroll too far for example).
