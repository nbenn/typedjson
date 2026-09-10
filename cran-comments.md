## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Possibly misspelled words

The incoming checks flag `diffable`, `greppable` and `lossy` in the Description. All three are spelled as intended.

The first two say that a document this package writes can be read by `diff` and searched by `grep`, which is the property the package exists to provide. The third is the ordinary term for a conversion that does not preserve what it was given.

## Bundled code

The package bundles the yyjson C library, version 0.12.0, as `src/yyjson.c` and `src/yyjson.h`.

Its author is credited in `Authors@R` as a contributor and copyright holder, the MIT license text is reproduced in `inst/YYJSON-LICENSE`, and `LICENSE.note` records the arrangement together with the upstream URL.
