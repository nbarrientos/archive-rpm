[![Tests](https://github.com/nbarrientos/archive-rpm/actions/workflows/test.yml/badge.svg)](https://github.com/nbarrientos/archive-rpm/blob/master/test/archive-rpm-tests.el)
[![Melpa](https://melpa.org/packages/archive-rpm-badge.svg)](https://melpa.org/#/archive-rpm)

# archive-rpm

## Description

Browse RPM and CPIO archives in Emacs with archive-mode

Have you ever tried opening a tarball or a zip file in Emacs?  You
will have noticed that it shows you a list of all the files in the
archive, and you can open the individual files.  Then perhaps you
tried opening an RPM file, hoping to see something similar, only to be
faced with a wall of binary data.

This pair of Emacs Lisp modules attempts to remediate that, giving you
the same experience with RPM files as with tarballs and other archive
files.

(Why two modules?  As it turns out, RPM files consist of a bunch of
metadata prepended to a compressed CPIO archive, so it makes sense to
implement handling of CPIO archives, and then do RPMs on top of that.)

## History

In March 2022, [MELPA
switched](https://github.com/melpa/melpa/pull/7945) to this repository
as source for this package due to the [original
one](https://github.com/legoscia/archive-rpm) not being maintained.

## Installation

To install these modules, type `M-x package-install-file`, and select
the _directory_ containing `archive-rpm.el` and `archive-cpio.el`
(don't select one of the modules themselves!).  After that, any RPM
files you open should display as some metadata plus a file listing.

This package is also available from MELPA so if you have that
repository configured `(package-install "archive-rpm")` should do the
trick.

## Tests

To run the test suite make sure that `rpmbuild` is installed on the
system and run `make test`.
