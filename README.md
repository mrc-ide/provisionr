# provisionr

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

This package aims to fill a hole in provisioning a set of packages.  There are some trouble spots that it aims to fill

* `install.packages()` with a list of packages does not throw an error if package installation fails, which makes using it in scripts difficult
* Resolving dependencies across packages that are not on CRAN is difficult.  `Remotes:` helps, but only if people have been dilegent
* Provisioning a set of packages for a different architechture than the one you are operating on is difficult.

So the issues to address are:

* Build a local drat with a bunch of github (and other) sources.  Keep this up to date efficiently by comparing against the sha of the appropriate branch.  This gives us a set of source packages we can interrogate quickly (and do things like compare with the sysreqs database later on).  When doing this, it may make sense to collect things from Remotes: sources _within_ these packages.

## Use cases

* Install one or more packages into a library, skipping packages that are already installed
* Create a complete self-contained library for a set of packages, isolated from the rest of the system
* As above, but for a different architecture than your current system (e.g., create a windows library when on linux or v.v.)

At the same time, do all this while allowing installation of github and local package sources (including detecting and acting on upgrades).

The package differs from [packrat](https://github.com/rstudio/packrat) in that it is not trying to create a strictly versioned library (though that may included in a future release).
