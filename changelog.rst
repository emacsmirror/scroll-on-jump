
##########
Change Log
##########

  - Add ``scroll-on-jump-curve`` & ``scroll-on-jump-curve-power``
    to support different kinds of curves & control their strength.

- Version 0.2 (2023-01-22)

  - Add ``scroll-on-jump-mode-line-format`` to support overriding the mode-line while scrolling.
  - Workaround bug in evil visual line mode.
  - Fix error counting lines jumping to a point outside buffer range.
  - Fix error detecting scroll changes.
  - Fix smooth (pixel) scrolling not being used in most cases.
  - Fix error passing out-of-range point to count-screen-lines.
  - Fix visual glitch when used with next/previous line.
  - Fix #1: Support wrapping functions that themselves scroll.
  - Fix error where the pixel scroll was left a non-zero value.
  - Fix recursive calls to animate scrolling.
  - Fix the case when the windows buffer is not the current-buffer.
  - Fix scrolling down when the point is at the beginning of the buffer.

- Version 0.1 (2020-12-07)

  Initial release.
