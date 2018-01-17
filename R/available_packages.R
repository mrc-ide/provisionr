## This somewhat sanitises available.packages.  The idea is to behave
## like available.packages but to allow filtering against
available_packages <- function(urls, r_version = NULL, os_type = NULL,
                               subarch = NULL, drop_duplicates = TRUE,
                               missing_index_is_error = TRUE, progress = NULL) {
  if (is.null(r_version)) {
    use_rds <- getRversion() >= numeric_version("3.4.0")
  } else {
    r_version <- check_r_version(r_version)
    use_rds <- r_version >= numeric_version("3.4.0")
  }
  fields <- colnames(utils::available.packages(NULL))
  dat <- lapply(urls, read_available_packages,
                use_rds, missing_index_is_error, progress)
  ret <- do.call("rbind", lapply(dat, clean_matrix, fields))
  rownames(ret) <- unname(ret[, "Package"])
  filter_available_packages(ret, r_version, os_type, subarch, drop_duplicates)
}

## Unlike the version in 'available.packages' this is not (yet)
## extensible, but that's probably tweakable really.
filter_available_packages <- function(db, r_version = NULL,
                                      os_type = NULL,
                                      subarch = NULL,
                                      drop_duplicates = TRUE) {
  db <- filter_available_packages_r_version(db, r_version)
  db <- filter_available_packages_os_type(db, os_type)
  db <- filter_available_packages_subarch(db, subarch)
  db <- filter_available_packages_drop_duplicates(db, drop_duplicates)
  db
}

filter_available_packages_r_version <- function(db, r_version) {
  if (!is.null(r_version) && nrow(db) > 0L) {
    ## This takes ~0.1s so that's a bit slower than ideal
    v <- parse_deps_version(db[, "Depends"])
    f <- function(x) {
      i <- x[, "name"] == "R"
      if (any(i)) {
        x[i, 2:3]
      } else {
        c(operator = ">=", version = "0.0.0")
      }
    }
    y <- vapply(v, f, character(2))
    ok <- logical(length(v))
    for (op in unique(y["operator", ])) {
      i <- y["operator", ] == op
      ok[i] <- get(op)(r_version, numeric_version(y["version", i]))
    }
    db <- db[ok, , drop = FALSE]
  }
  db
}

filter_available_packages_os_type <- function(db, os_type) {
  if (!is.null(os_type)) {
    os_type <- match_value(os_type, c("unix", "windows"))
    type <- db[, "OS_type"]
    db <- db[is.na(type) | type == os_type, , drop = FALSE]
  }
  db
}

filter_available_packages_subarch <- function(db, subarch) {
  if (!is.null(subarch)) {
    ## This does not really fuss me for now
    stop("WRITEME")
  }
  db
}

filter_available_packages_drop_duplicates <- function(db, drop_duplicates) {
  if (isTRUE(drop_duplicates)) {
    nm <- db[, "Package"]
    dups <- unique(nm[duplicated(nm)])
    if (length(dups) > 0L) {
      stale <- function(p) {
        i <- which(nm == p)
        v <- package_version(db[i, "Version"])
        j <- max(v) == v
        unname(i[!j | duplicated(j)])
      }
      drop <- unlist(lapply(dups, stale))
      db <- db[-drop, , drop = FALSE]
    }
  }
  db
}

cache <- new.env(parent = emptyenv())
read_available_packages <- function(url, use_rds, missing_index_is_error,
                                    progress) {
  if (is_file_url(url)) {
    index <- file.path(file_unurl(url), "PACKAGES")
    if (file.exists(index)) {
      d <- read.dcf(index)
    } else if (missing_index_is_error) {
      stop("No package index at ", index)
    } else {
      provisionr_log("no index", index)
      return(matrix(character(0)))
    }
  } else if (exists(url, cache)) {
    d <- cache[[url]]
  } else {
    nm <- paste0("PACKAGES", c(if (use_rds) ".rds", ".gz", ""))
    for (x in nm) {
      path <- tryCatch(download_file1(file.path(url, x), tempdir(),
                                      dest_file = basename(tempfile()),
                                      progress = progress, report = FALSE),
                       error = identity)
      if (!inherits(path, "error")) {
        if (x == "PACKAGES.rds") {
          d <- readRDS(path)
        } else {
          d <- read.dcf(path)
        }
        cache[[url]] <- d
        unlink(path)
        break
      }
    }
    if (inherits(path, "error")) {
      if (missing_index_is_error) {
        stop(path)
      } else {
        provisionr_log("no index", url)
        return(matrix(character(0)))
      }
    }
  }
  if (nrow(d) > 0L) {
    rownames(d) <- d[, "Package"]
  }
  if (!("Repository" %in% colnames(d)) && nrow(d) > 0) {
    d <- cbind(d, Repository = url)
  }
  if ("Path" %in% colnames(d)) {
    i <- !is.na(d[, "Path"])
    d[i, "Repository"] <- paste0(d[i, "Repository"], "/", d[i, "Path"])
  }
  d
}
