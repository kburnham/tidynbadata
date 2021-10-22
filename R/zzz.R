#' Tools for computing advanced NBA statistics
#'
#' @section Package Options:
#'
#' tidybnadata used the following options to configure behavior:
#'
#' \itemize{
#'   \item `tidynbadata.archive_path`: location of archive directory
#'   \item `tidynbadata.current_season`: default season for msfsportsfeeds data
#'   \item `tidynbadata.msf_version_id`: default version for msfsportsfeeds
#'
#'}
#' @docType package
#' @name tidynbadata
"_PACKAGE"

tidynbadata_default_options <- list(
  tidynbadata.archive_path = "~/tidynbadata_archive",
  tidynbadata.current_season = "2021-2022-regular",
  tidynbadata.msf_version_id = "2.0"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(tidynbadata_default_options) %in% names(op))
  if (any(toset)) options(tidynbadata_default_options[toset])

  invisible()
}
