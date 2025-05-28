#' Downloads R
#'
#' Downloads R in \code{app_dir}. If it has already been downloaded, \code{get_R} will use that file. If the download fails it will stop.
#'
#' If \code{\link{create_app}(include_R = TRUE)}, then \code{get_R}.
#'
#' @inheritParams create_app
#'
#' @return
#' \code{sprintf('R-\%s-win.exe', R_version)} in \code{app_dir}.
#'
#' @inherit setup_section seealso
#' @author Jonathan M. Hill
#' @export
get_R <- function(
  app_dir = getwd(),
  R_version = paste0(">=", R.version$major, ".", R.version$minor)
) {
  if (!base::dir.exists(app_dir)) {
    base::stop(glue::glue("{app_dir} does not exist."), call. = FALSE)
  }

  R_version <- sanitize_R_version(R_version, clean = TRUE)

  # Get latest R version from current release page
  latest_R_version <- base::readLines(
    "https://cran.rstudio.com/bin/windows/base/",
    warn = FALSE
  ) |>
    stringr::str_extract("R-\\d+\\.\\d+\\.\\d+-win\\.exe") |>
    stats::na.omit() |>
    base::unique() |>
    stringr::str_extract("\\d+\\.\\d+\\.\\d+")

  # Get list of old R versions
  old_R_versions <- base::readLines(
    "https://cran.rstudio.com/bin/windows/base/old/",
    warn = FALSE
  ) |>
    stringr::str_extract("\\d+\\.\\d+\\.\\d+") |>
    stats::na.omit() |>
    base::unique()

  # Determine base URL for R installer
  if (latest_R_version == R_version) {
    base_url <- glue::glue(
      "https://cran.r-project.org/bin/windows/base/R-{R_version}-win.exe"
    )
  } else {
    base_url <- glue::glue(
      "https://cran.r-project.org/bin/windows/base/old/{R_version}/R-{R_version}-win.exe"
    )
  }

  filename <- base::file.path(app_dir, glue::glue("R-{R_version}-win.exe"))

  if (base::file.exists(filename)) {
    base::cat("Using the copy of R already included:\n", filename, "\n")
  } else {
    base::cat(glue::glue("Downloading R-{R_version} ...\n"))

    if (!R_version %in% base::c(latest_R_version, old_R_versions)) {
      base::stop(
        glue::glue("That version of R ({R_version}) is not listed on CRAN."),
        call. = FALSE
      )
    }

    base::tryCatch(
      curl::curl_download(base_url, filename),
      error = function(e) {
        base::cat(glue::glue(
          "
{base_url} is not a valid URL.

This is likely to have happened because there was a change in the URL.

This might have already been fixed in the latest version of RInno. Install it with remotes::install_github('ficonsulting/RInno').

If this doesn't help please submit an issue: {utils::packageDescription('RInno', fields = 'BugReports')}

- Thanks!\n"
        ))
      }
    )

    if (!base::file.exists(filename)) {
      base::stop(glue::glue("{filename} failed to download."), call. = FALSE)
    }
  }
}
