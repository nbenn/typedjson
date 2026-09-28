## R CMD check results

0 errors | 0 warnings | 1 note

* Days since last update: 6

## Update

This version clears the WARNING that the gcc-ASAN additional check reports for 0.1.0 at install, a `-Wstringop-overflow` raised by GCC 16 under link-time optimization. That is also why it follows 0.1.0 within a week.

## Bundled code

The package bundles the yyjson C library, version 0.12.0, as `src/yyjson.c` and `src/yyjson.h`.

Its author is credited in `Authors@R` as a contributor and copyright holder, the MIT license text is reproduced in `inst/YYJSON-LICENSE`, and `LICENSE.note` records the arrangement together with the upstream URL.
