##' Download multiple files.
##' @title Download multiple files
##' @param urls A character vector of urls
##' @param dest_dir A single existing directory to download files into
##' @param ... Currently ignored
##'
##' @param labels A character vector of labels to use to describe the
##'   files being downloaded when printing (defaults to
##'   \code{basename(urls)})
##'
##' @param overwrite Overwrite files that exist already?  If
##'   \code{FALSE} (the default) then existing files are skipped.
##'
##' @param count Logical, indicating if a count of progress across the
##'   urls should be included.
##'
##' @param dest_file If the files should be renamed as they are
##'   downloaded, include a vector of filenames here the same length
##'   as \code{urls}.  Directory components will be created, within
##'   \code{dest_dir}.
##'
##' @param copy_file_urls Logical, indicating if \code{file:///} urls
##'   should be copied into \code{dest_dir}
##'
##' @param progress Print a progress bar?
##'
##' @param report Print a summary?
##'
##' @return A character vector, the same length as \code{urls}, with
##'   the destination file paths (even if no downloading was done).  A
##'   failure to download a file (e.g., a 403 forbidden, 404 not
##'   found, or general network error) will result in an R error.
##'
##' @export
download_files <- function(urls, dest_dir, ..., labels = NULL,
                           overwrite = FALSE, count = TRUE,
                           dest_file = NULL, copy_file_urls = TRUE,
                           progress = NULL, report = TRUE) {
  if (!is_directory(dest_dir)) {
    stop("dest_dir must be a directory")
  }
  if (is.null(labels)) {
    labels <- substr(basename(urls), 1, ceiling(getOption("width", 80) / 4))
  } else {
    if (length(labels) != length(urls)) {
      stop("length(labels) must be the same as length(urls)")
    }
  }
  if (is.null(dest_file)) {
    dest_file <- basename(urls)
  } else {
    if (length(dest_file) != length(urls)) {
      stop("length(dest_file) must be the same as length(urls)")
    }
  }

  filename <- file.path(dest_dir, dest_file)
  dest_file <- format(dest_file)
  progress <- download_progress(progress)

  for (i in seq_along(urls)) {
    f <- filename[[i]]
    if (file.exists(f) && !overwrite) {
      next
    }

    u <- urls[[i]]
    p <- progress_multi(i, labels, count, progress)
    dir.create(dirname(f), FALSE, TRUE)
    if (is_file_url(u)) {
      u <- file_unurl(u)
      if (copy_file_urls) {
        file.copy(u, filename[[i]], overwrite = overwrite)
      } else {
        filename[[i]] <- u
        next
      }
    } else {
      h <- curl::new_handle(noprogress = !progress,
                            progressfunction = p$callback)
      f_dl <- paste0(f, "_dl")
      if (file.exists(f_dl)) {
        stop("Remove stale download file: ", f_dl)
      }
      res <- withCallingHandlers(curl::curl_fetch_disk(u, f_dl, h),
                                 error = function(e) file.remove(f_dl))
      if (res$status_code > 300) {
        file.remove(f_dl)
        stop(download_error(res))
      }
      file.rename(f_dl, f)
    }
    s <- format(structure(file.size(f), class = "object_size"),
                units = "auto")
    if (report) {
      message(sprintf("%s => %s (%s)", p$prefix, dest_file[[i]], s))
    }
  }

  filename
}

download_file1 <- function(url, dest_dir, ..., label = NULL,
                           overwrite = FALSE,
                           dest_file = NULL, copy_file_url = TRUE,
                           progress = NULL, report = TRUE) {
  download_files(url, dest_dir, labels = label,
                 overwrite = overwrite, dest_file = dest_file,
                 copy_file_urls = copy_file_url,
                 progress = progress, report = report,
                 count = FALSE)
}

download_error <- function(r) {
  msg <- sprintf("Downloading '%s' failed with code %d", r$url, r$status_code)
  structure(list(message = msg, r = r, call = NULL),
            class = c("download_error", "error", "condition"))
}

download_progress <- function(progress) {
  progress %||% getOption("provisionr.download.progress", TRUE)
}

progress_multi <- function(i, labels, count, progress) {
  label <- format(labels[[i]], width = max(nchar(labels)), justify = "right")
  if (count) {
    is <- format(i, width = nchar(length(labels)))
    prefix <- sprintf("[%s/%s] %s", is, length(labels), label)
  } else {
    prefix <- label
  }
  bar <- NULL
  type <- "down"
  seen <- 0

  if (progress) {
    callback <- function(down, up) {
      if (type == "down") {
        total <- down[[1L]]
        now <- down[[2L]]
      } else {
        total <- up[[1L]]
        now <- up[[2L]]
      }

      if (total == 0 && now == 0) {
        bar <<- NULL
        seen <<- 0
        return(TRUE)
      }

      if (is.null(bar)) {
        if (total == 0) {
          fmt <- paste0(prefix, " [ :bytes in :elapsed ]")
          total <- 1e8 # arbitrarily big
        } else {
          fmt <- paste0(prefix, " [:percent :bar]")
        }
        bar <<- progress::progress_bar$new(fmt, total, clear = TRUE,
                                           show_after = 0)
      }
      if (total == 0) {
        bar$tick(now)
      } else {
        bar$tick(now - seen)
        seen <<- now
      }

      TRUE
    }
  } else {
    callback <- function(down, up) {
      TRUE
    }
  }

  list(callback = callback,
       prefix = prefix)
}
