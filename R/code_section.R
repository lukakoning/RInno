#' Pascal script to check registry for R
#'
#' Modern Delphi-like Pascal adds a lot of customization possibilities to the
#' installer. For examples, please visit \href{http://www.jrsoftware.org/ishelp/topic_scriptintro.htm}{Pascal Scripting Introduction}.
#'
#' This script checks the registry for R, so that R will only be installed if necessary.
#'
#' @inheritParams setup_section
#' @inheritParams create_app
#'
#' @examples \dontrun{
#' readLines(system.file('installation/code.iss', package = 'RInno'))
#' }
#'
#' @inherit setup_section return seealso
#'
#' @author Jonathan M. Hill
#' @export
code_section <- function(
  iss,
  R_version = paste0(">=", base::R.version$major, ".", base::R.version$minor)
) {
  if (base::length(R_version) == 0) {
    R_version <- paste0(">=", base::R.version$major, ".", base::R.version$minor)
  }

  R_version <- sanitize_R_version(R_version)

  # Get latest R versions from CRAN using xml2/rvest
  # Step 1: Scrape latest version(s) from main directory
  latest_versions <- xml2::read_html(
    "https://cran.rstudio.com/bin/windows/base/"
  ) |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("^R-\\d+\\.\\d+\\.\\d+-win\\.exe$") |>
    stringr::str_extract("\\d+\\.\\d+\\.\\d+")

  # Step 2: Scrape all old versions from the /old/ directory
  old_base_url <- "https://cran.rstudio.com/bin/windows/base/old/"
  old_page <- xml2::read_html(old_base_url)

  # Get list of version directories
  old_dirs <- old_page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("^\\d+\\.\\d+\\.\\d+/?$") |>
    stringr::str_extract("^\\d+\\.\\d+\\.\\d+")

  # Scrape each old version subdirectory for matching `.exe` files
  get_old_version_exe <- function(version) {
    version_url <- paste0(old_base_url, version, "/")
    tryCatch(
      {
        xml2::read_html(version_url) |>
          rvest::html_nodes("a") |>
          rvest::html_attr("href") |>
          stringr::str_subset("^R-\\d+\\.\\d+\\.\\d+-win\\.exe$") |>
          stringr::str_extract("\\d+\\.\\d+\\.\\d+")
      },
      error = function(e) {
        NULL
      }
    )
  }

  # Apply function to each old version folder
  old_versions_full <- base::unlist(
    base::lapply(old_dirs, get_old_version_exe),
    use.names = FALSE
  )

  # Combine and deduplicate all R versions
  R_versions <- base::unique(base::c(latest_versions, old_versions_full))
  R_versions <- base::sort(R_versions, decreasing = TRUE)

  # Extract inequality and numeric version
  inequality <- base::substr(
    R_version,
    1,
    base::attr(base::regexpr("[<>=]+", R_version), "match.length")
  )
  version_target <- base::gsub("[<>=]", "", R_version)

  # Evaluate which versions match the spec
  version_specs <- base::paste0(
    "base::numeric_version('",
    R_versions,
    "')",
    inequality,
    "base::numeric_version('",
    version_target,
    "')"
  )

  if (!version_target %in% R_versions && base::interactive()) {
    base::stop(
      glue::glue(
        "R version - {version_target} - was not found on CRAN. Please use `R_version` to specify one that is, or file an issue:\n\nhttps://github.com/ficonsulting/RInno/issues"
      ),
      call. = FALSE
    )
  }

  results <- base::unlist(base::lapply(
    version_specs,
    function(x) base::eval(base::parse(text = x))
  ))
  acceptable_R_versions <- base::paste0(
    glue::glue("RVersions.Add('{R_versions[results]}');"),
    collapse = "\n  "
  )

  # Read base Pascal template
  code_file <- base::paste0(
    base::readLines(base::system.file(
      "installation/code.iss",
      package = "RInno"
    )),
    collapse = "\n"
  )

  # Return full Pascal code block
  glue::glue(
    '
{iss}
{code_file}

// Initialize the values of supported versions
RVersions := TStringList.Create; // Make a new TStringList object reference
// Add strings to the StringList object
  {acceptable_R_versions}

end;

// Procedure called by InnoSetup when it is closing
procedure DeinitializeSetup();
begin
  RVersions.Free;
end;
'
  )
}
