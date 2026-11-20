## R CMD check results

0 errors | 0 warnings | 0 notes

## Update

This version fixes the test ERRORs that the r-devel-linux-x86_64-debian-gcc, r-devel-linux-x86_64-fedora-clang and r-devel-linux-x86_64-fedora-gcc flavours report for 0.1.1.
CRAN asked for them to be corrected before 2026-12-11.
The failing tests round-trip S7 objects, which 0.1.1 cannot do under S7 1.0.0.

## Bundled code

The package bundles the yyjson C library, version 0.12.0, as `src/yyjson.c` and `src/yyjson.h`.

Its author is credited in `Authors@R` as a contributor and copyright holder, the MIT license text is reproduced in `inst/YYJSON-LICENSE`, and `LICENSE.note` records the arrangement together with the upstream URL.
