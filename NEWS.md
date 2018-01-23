# provisionr 0.1.6

* Fix odd bug during installation of packages that are not on CRAN but suggested by CRAN (as for glmmADMB via HeritSeq).

# provisionr 0.1.5

* Missing binary package indexes (e.g., most drat repositories) no longer causes an error ([#17](https://github.com/mrc-ide/provisionr/issues/17))
* Fix bug where `cran_download` would select packages from future versions ([#18](https://github.com/mrc-ide/provisionr/issues/18) and [nomad#8](https://github.com/reconhub/nomad/issues/8))
