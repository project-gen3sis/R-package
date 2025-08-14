#' Filesystem adapters
#'
#' Thin wrappers around base filesystem calls to enable dependency injection in tests
#' and to centralize I/O behavior (logging, retries, future switches to fs, etc.).
#'
#' @param path Character: a file or directory path.
#' @param ... Passed to [base::dir.create()].
#'
#' @return
#' - `dir_exists()`: logical scalar indicating existence.
#' - `dir_make()`: logical scalar from [base::dir.create()] (invisibly).
#'
#' @keywords internal
#' @md
#' @noRd
file.exists <- function(path) base::file.exists(path)

dir.create <- function(path, ...) base::dir.create(path, ...)