# autonix-deps

`autonix-deps` is part of a system for automatically generating Nix
expressions for large collections of
software. [Nix](http://nixos.org/nix/) is a purely functional
package manager available for Linux and other Unix-like platforms.

The `autonix` system constructs Nix expressions in three stages. The
first stage is a script to download and hash package sources to
construct a *manifest*. Second, the package sources are examined to
detect *dependencies*. Dependencies can be detected inside and outside
the package collection. Finally, the manifest and dependencies are
combined to produce the actual Nix *expressions*, which give
instructions to build each package.

Dependency detection is necessarily specific to each package
collection, but the process is always the same. Given a list of
package sources archives (the *manifest*), the dependency detector
examines the files in each archive (their names and contents) to guess
dependencies. The output of this process is a mapping from package
names to dependency names.

Between package collections, only the mechanism of guessing
dependencies changes. `autonix-deps` is a Haskell library designed as
an abstraction of the common pattern. It also provides some of the
mechanisms to detect dependencies, when these mechanisms are
independent of the package collection. For example, a mechanism is
provided to detect dependencies in projects using the CMake build
system; even though this is a detail, it is one shared between many
different projects. The actual dependency detection is done by a
collection-specific executable, which provides a few specific
analyzers and calls one of the high-level routines from this
library. For example,
[`autonix-deps-kf5`](https://github.com/ttuegel/autonix-deps-kf5)
detects dependencies for KDE Frameworks 5 and related software.

## Futher Reading
* [`Autonix.Analyze`](https://github.com/ttuegel/autonix-deps/blob/master/src/Autonix/Analyze.hs)
  provides the high-level interface for depedency detectors.
* [`Autonix.CMake`](https://github.com/ttuegel/autonix-deps/blob/master/src/Autonix/CMake.hs)
  is an example of a simple dependency analyzer that looks for
  dependencies when the CMake build system is used. CMake is the only
  build system specifically supported right now, but this module
  serves as a concise example of how one could add support for other
  build systems.
