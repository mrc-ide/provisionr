plan_installation <- function(packages, db, lib, installed_action,
                              local_drat = NULL) {
  if (installed_action == "skip") {
    skip <- .packages(TRUE, lib)
  } else {
    skip <- NULL
  }
  base <- base_packages()
  builtin <- builtin_packages()
  requested <- setdiff(packages, base_packages())
  available <- union(.packages(TRUE, lib), builtin)

  ## TODO: drop the setdiff here, which also drops the lib argument?
  msg <- setdiff(packages, c(db$all[, "Package"], c(skip, base)))
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for: %s",
                 paste(msg, collapse = ", ")))
  }

  packages <- setdiff(recursive_deps(packages, db$all), base)
  msg <- setdiff(packages, db$all[, "Package"])
  if (length(msg) > 0L) {
    stop(sprintf("Can't find installation candidate for dependencies: %s",
                 paste(msg, collapse = ", ")))
  }

  if (installed_action == "skip") {
    packages <- setdiff(packages, available)
  }

  binary <- packages %in% rownames(db$bin)

  if (installed_action == "upgrade" || installed_action == "upgrade_all") {
    if (installed_action == "upgrade") {
      check <- union(setdiff(packages, available), requested)
    } else {
      check <- packages
    }
    packages <- check_version(check, lib, db, local_drat)
  }

  ## Check the versions here to prefer source packages where they
  ## are newer
  nv <- function(x) {
    x[is.na(x)] <- "0.0.0"
    numeric_version(x)
  }
  v_bin <- nv(db$bin[match(packages, rownames(db$bin)), "Version"])
  v_src <- nv(db$src[match(packages, rownames(db$src)), "Version"])
  avoid_bin <- intersect(packages[v_src > v_bin], rownames(db$bin))

  if (length(avoid_bin) > 0L) {
    is_from_cran <- function(p, cran) {
      if (length(cran) == 0L) {
        FALSE
      } else {
        ## We could either use
        ##   any(x & y)
        ## or
        ##   any(x) && any(y)
        ## But the latter performs better when there are two repos
        ## claiming to be CRAN
        any(string_starts_with(db$bin[p, "Repository"], cran)) &&
          any(string_starts_with(db$src[p, "Repository"], cran))
      }
    }
    cran_update <- vlapply(avoid_bin, is_from_cran, db$repos[db$is_cran])
    if (any(cran_update)) {
      msg <- sprintf(
        "Preferring old CRAN binary for %s",
        paste(sprintf('"%s"', avoid_bin[cran_update]), collapse = ", "))
      provisionr_log("note", msg)
      avoid_bin <- avoid_bin[!cran_update]
    }
    if (length(avoid_bin) > 0L) {
      db$bin <- db$bin[-match(avoid_bin, rownames(db$bin)), ]
    }
  }

  compile <- rep_len(FALSE, length(packages))
  binary <- packages %in% rownames(db$bin)

  if (any(!binary)) {
    j <- match(packages[!binary], db$src[, "Package"])
    compile[!binary] <- db$src[j, "NeedsCompilation"] == "yes"
  }

  ret <- list(packages = packages,
              binary = binary,
              compile = compile,
              db = db)
  class(ret) <- "provisionr_plan"
  ret
}

check_installed_packages <- function(packages, lib, cols = NULL) {
  if (is.null(cols)) {
    cols <- c("Depends", "Imports")
  }

  ## packages we do not *need* to see in the library
  builtin <- builtin_packages()
  ## packages that we can see in the library
  installed <- union(.packages(TRUE, lib), builtin)
  ## packages known to be missing
  missing <- setdiff(packages, installed)
  check <- setdiff(packages, missing)
  checked <- installed

  while (length(check) > 0L) {
    p <- check[[1L]]
    desc <- find_description(p, lib)
    if (is.na(desc)) {
      missing <- c(missing, p)
      extra <- character(0)
    } else {
      deps <- parse_deps(na.omit(read.dcf(desc, cols)[1L, ]))
      ## add them to our package list:
      extra <- setdiff(deps, checked)
    }
    checked <- c(checked, p)
    check <- c(check[-1L], extra)
  }

  missing
}

find_description <- function(p, lib) {
  full <- file.path(lib, p, "DESCRIPTION")
  found <- file.exists(full)
  if (any(found)) {
    full[which(found)[[1L]]]
  } else {
    NA_character_
  }
}

## TODO: we need to drop recommended from here, unless I deal with the
## issue seen with Hmisc vs survival not working with R 3.3.2 unless
## survival (a recommended package) is manually updated.  So going
## through versions might fix things.
base_packages <- function() {
  rownames(installed.packages(priority = "base"))
}

builtin_packages <- function() {
  rownames(installed.packages(priority = c("base", "recommended")))
}

recursive_deps <- function(x, db, suggests = FALSE) {
  done <- character()
  base <- base_packages()
  cols <- c("Depends", "Imports", "LinkingTo",
            if (suggests) "Suggests")

  while (length(x) > 0L) {
    done <- c(done, x)
    deps <- parse_deps(na.omit(c(db[match(x, db[, "Package"]), cols])))
    x <- setdiff(deps, c(x, base))
  }

  sort(unique(done))
}

parse_deps <- function(x) {
  ## TODO: This does not support returning version numbers (so
  ## depending on particular versions of packages is not going to work
  ## here).
  ##
  ## Somewhere I had the version parsing thing; I will need that back
  ## soon.  For now this just strips version information entirely.
  ## This could be something good to push into remotes, perhaps?
  val <- unlist(strsplit(x, ","), use.names=FALSE)
  val <- gsub("(\\s|\\().*", "", trimws(val))
  val[val != "R"]
}

parse_deps_version <- function(x) {
  xx <- strsplit(trimws(sub("\n", " ", x, fixed = TRUE)),
                 "\\s*,\\s*", perl = TRUE)
  re <- "^([^\\s(]+)\\s*(.*)"
  xx[vlapply(xx, identical, NA_character_)] <- list(character(0))

  i <- lengths(xx, FALSE)
  j <- factor(rep(seq_along(i), i), seq_along(i))

  s <- unlist(xx, use.names = FALSE)
  pkg <- sub(re, "\\1", s, perl = TRUE)
  ver <- sub(re, "\\2", s, perl = TRUE)
  ver[!nzchar(ver)] <- NA_character_
  re_ver <- "^\\(([^\\s]+)\\s+([^)]+)\\)"
  op <- sub(re_ver, "\\1", ver, perl = TRUE)
  ver <- sub(re_ver, "\\2", ver, perl = TRUE)

  dat <- cbind(name = pkg, operator = op, version = ver)
  ret <- lapply(split(dat, j), matrix, ncol = 3, dimnames = dimnames(dat))
  if (!is.null(names(x))) {
    names(ret) <- names(x)
  }
  ret
}

drat_ensure_PACKAGES <- function(path) {
  path_PACKAGES <- file.path(path, "PACKAGES")
  if (!file.exists(path_PACKAGES)) {
    dir.create(path, FALSE, TRUE)
    writeLines(character(0), path_PACKAGES)
  }
}

check_version <- function(packages, lib, db, local_drat) {
  current <- packages %in% .packages(TRUE, lib)
  if (any(current)) {
    check <- packages[current]

    desc <- setNames(file.path(find.package(check, lib), "DESCRIPTION"), check)
    v_current <- numeric_version(vcapply(desc, read.dcf, "Version"))
    binary <- check %in% rownames(db$bin)
    v_db <- setNames(character(length(check)), check)
    v_db[binary] <- db$bin[check[binary], "Version"]
    v_db[!binary] <- db$src[check[!binary], "Version"]
    v_db <- numeric_version(v_db)
    current[current] <- v_current >= v_db

    if (any(current) && !is.null(local_drat)) {
      ## Extra check for non-version-number-indicated change in
      ## packages that are in the package_source-controlled repo.
      ## This is not that straightforward to do though as I don't
      ## always have access to the same fields in installed packages
      ## and in the storr database. The best I have is for "Packaged"
      ## field.
      st <- drat_storr(local_drat)
      dat_drat <- st$mget(st$list())
      names(dat_drat) <- vcapply(dat_drat, "[[", "Package")

      for (p in intersect(names(dat_drat), packages[current])) {
        t_drat <- sub(";.*", "", dat_drat[[p]]$Packaged)
        t_lib <- sub(";.*", "", drop(read.dcf(desc[[p]], "Packaged")))
        if (isTRUE(as.POSIXct(t_drat) > as.POSIXct(t_lib))) {
          provisionr_log("force",
                         sprintf("upgrade '%s' as detected newer source", p))
          current[packages == p] <- FALSE
        }
      }
    }
  }
  packages[!current]
}

plan_force_binary <- function(packages, plan, local_drat) {
  i <- which(rownames(plan$db$bin) %in% packages)
  drop <- i[plan$db$bin[i, "Repository"] != file_url(local_drat)]
  if (length(drop) > 0L) {
    ## Need to update the plan, too, here.
    packages_bin <- intersect(rownames(plan$db$bin)[i], plan$packages)
    j <- match(packages_bin, plan$packages)
    plan$binary[j] <- FALSE
    plan$compile[j] <- plan$db$src[packages_bin, "NeedsCompilation"] == "yes"
    plan$db$bin <- plan$db$bin[-i, , drop = FALSE]
  }

  plan
}

##' @export
print.provisionr_plan <- function(x, ...) {
  cat("<provisionr_plan>\n")
  pkgs <- strwrap(paste(x$packages, collapse = ", "),
                  initial = " - packages: ", exdent = 13)
  cat(paste0(pkgs, "\n", collapse = ""))
  invisible(x)
}
