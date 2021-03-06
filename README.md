# provisionr

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/mrc-ide/provisionr.svg?branch=master)](https://travis-ci.org/mrc-ide/provisionr)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/5c1iugo4h24v7o6b/branch/master?svg=true)](https://ci.appveyor.com/project/richfitz/provisionr-7pe94/branch/master)
[![codecov.io](https://codecov.io/github/mrc-ide/provisionr/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/provisionr?branch=master)

This package aims to fill a hole in provisioning a set of packages.  There are some trouble spots that it aims to fill

* `install.packages()` with a list of packages does not throw an error if package installation fails, which makes using it in scripts difficult
* Resolving dependencies across packages that are not on CRAN is difficult.  `Remotes:` helps, but only if people have been dilegent
* Provisioning a set of packages for a different architecture than the one you are operating on is difficult.

What this package aims to do is let users write

```r
provisionr::provision_library(packages, path, platform, version)
```

and have a library built at `path` containing `packages` (and all their dependencies) for `platform` (windows, macosx, linux) and a particular R `version`.  Declaring non-CRAN package sources is done with an argument `src`, which centralises collection of github, local, and other package sources.

## Use cases

* Install one or more packages into a library, skipping packages that are already installed
* Create a complete self-contained library for a set of packages, isolated from the rest of the system
* As above, but for a different architecture than your current system (e.g., create a windows library when on linux or v.v.)

At the same time, do all this while allowing installation of github and local package sources (including detecting and acting on upgrades).

The package differs from [packrat](https://github.com/rstudio/packrat) in that it is not trying to create a strictly versioned library (though that may included in a future release).

## Installation

```r
# install.packages("drat")
drat:::add("mrc-ide")
install.packages("provisionr")
```
