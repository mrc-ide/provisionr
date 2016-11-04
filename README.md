# provision

This package aims to fill a hole in provisioning a set of packages.  There are some trouble spots that it aims to fill

* `install.packages()` with a list of packages does not throw an error if package installation fails, which makes using it in scripts difficult
* Resolving dependencies across packages that are not on CRAN is difficult.  `Remotes:` helps, but only if people have been dilegent
* Provisioning a set of packages for a different architechture than the one you are operating on is difficult.

So the issues to address are:

* Build a local drat with a bunch of github (and other) sources.  Keep this up to date efficiently by comparing against the sha of the appropriate branch.  This gives us a set of source packages we can interrogate quickly (and do things like compare with the sysreqs database later on).  When doing this, it may make sense to collect things from Remotes: sources _within_ these packages.
