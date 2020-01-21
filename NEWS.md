# provisionr 0.1.14

* Fix parsing of R packages that contain many references to a minimum R version.

# provisionr 0.1.13

* Shell quote all arguments before building packages

# provisionr 0.1.12

* Detect and use GITHUB_PAT for downloading private repositories

# provisionr 0.1.8

* Fix bug in progress bar reporting
* Add new package dependency provisioning (`provision_dependencies`)

# provisionr 0.1.6

* Fix odd bug during installation of packages that are not on CRAN but suggested by CRAN (as for glmmADMB via HeritSeq).

# provisionr 0.1.5

* Missing binary package indexes (e.g., most drat repositories) no longer causes an error ([#17](https://github.com/mrc-ide/provisionr/issues/17))
* Fix bug where `cran_download` would select packages from future versions ([#18](https://github.com/mrc-ide/provisionr/issues/18) and [nomad#8](https://github.com/reconhub/nomad/issues/8))
