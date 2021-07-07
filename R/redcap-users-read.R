#' @title Export the users of a REDCap project
#'
#' @description Export the users (as a data dictionary) of a REDCap project
#' as a [base::data.frame()].
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.  Required.
#' @param token The user-specific string that serves as the password for a
#' project.  Required.
#' @param verbose A boolean value indicating if `message`s should be printed
#' to the R console during the operation.  The verbose output might contain
#' sensitive information (*e.g.* PHI), so turn this off if the output might
#' be visible somewhere public. Optional.
#' @param config_options  A list of options to pass to `POST` method in the
#' `httr` package.  See the details in [redcap_read_oneshot()]. Optional.
#'
#' @return Currently, a list is returned with the following elements:
#'
#' * `data`: An R [base::data.frame()] of the desired records and columns.
#' * `success`: A boolean value indicating if the operation was apparently
#' successful.
#' * `status_codes`: A collection of
#' [http status codes](http://en.wikipedia.org/wiki/List_of_HTTP_status_codes),
#' separated by semicolons.  There is one code for each batch attempted.
#' * `outcome_messages`: A collection of human readable strings indicating the
#' operations' semicolons.  There is one code for each batch attempted.  In an
#' unsuccessful operation, it should contain diagnostic information.
#' * `elapsed_seconds`: The duration of the function.
#'
#' @author Will Beasley
#'
#' @references The official documentation can be found on the 'API Help Page'
#' and 'API Examples' pages on the REDCap wiki (*i.e.*,
#' https://community.projectredcap.org/articles/456/api-documentation.html and
#' https://community.projectredcap.org/articles/462/api-examples.html).
#' If you do not have an account for the wiki, please ask your campus REDCap
#' administrator to send you the static material.
#'
#' @examples
#' \dontrun{
#' uri   <- "https://bbmc.ouhsc.edu/redcap/api/"
#' token <- "9A81268476645C4E5F03428B8AC3AA7B"
#' REDCapR::redcap_users_read(redcap_uri=uri, token=token)
#' }

#' @export
redcap_users_read <- function(
  redcap_uri,
  token,
  verbose           = TRUE,
  config_options    = NULL
) {

  checkmate::assert_character(redcap_uri  , any.missing=FALSE, len=1, pattern="^.{1,}$")
  checkmate::assert_character(token       , any.missing=FALSE, len=1, pattern="^.{1,}$")

  token               <- sanitize_token(token)
  verbose             <- verbose_prepare(verbose)

  post_body <- list(
    token    = token,
    content  = "user",
    format   = "csv"
  )

  # This is the important line that communicates with the REDCap server.
  kernel <- kernel_api(redcap_uri, post_body, config_options)

  if (kernel$success) {

    col_types <-
      readr::cols(
        username = readr::col_character(),
        email = readr::col_character(),
        firstname = readr::col_character(),
        lastname = readr::col_character(),
        expiration = readr::col_character(),
        data_access_group = readr::col_character(),
        design = readr::col_character(),
        user_rights = readr::col_character(),
        data_access_groups = readr::col_character(),
        data_export = readr::col_character(),
        reports = readr::col_character(),
        stats_and_charts = readr::col_character(),
        manage_survey_participants = readr::col_character(),
        calendar = readr::col_character(),
        data_import_tool = readr::col_character(),
        data_comparison_tool = readr::col_character(),
        logging = readr::col_character(),
        file_repository = readr::col_character(),
        data_quality_create = readr::col_character(),
        data_quality_execute = readr::col_character(),
        api_export = readr::col_character(),
        api_import = readr::col_character(),
        mobile_app = readr::col_character(),
        mobile_app_download_data = readr::col_character(),
        record_create = readr::col_character(),
        record_rename = readr::col_character(),
        record_delete = readr::col_character(),
        lock_records_customization = readr::col_character(),
        lock_records = readr::col_character(),
        lock_records_all_forms = readr::col_character(),
        forms = readr::col_character(),
        .default    = readr::col_character()
      )

    try(
      # Convert the raw text to a dataset.
      ds <-
        readr::read_csv(
          kernel$raw_text,
          col_types = col_types
        ),
      # Don't print the warning in the try block.  Print it below,
      #   where it's under the control of the caller.
      silent = TRUE
    )

    if (exists("ds") & inherits(ds, "data.frame")) {
      outcome_message <- sprintf(
        "The data dictionary describing %s users was read from REDCap in %0.1f seconds.  The http status code was %i.",
        format(nrow(ds), big.mark = ",", scientific = FALSE, trim = TRUE),
        kernel$elapsed_seconds,
        kernel$status_code
      )

      # If an operation is successful, the `raw_text` is no longer returned
      #   to save RAM.  The content is not really necessary with httr's status
      #   message exposed.
      kernel$raw_text   <- ""
    } else { # nocov start
      # Override the 'success' determination from the http status code
      #   and return an empty data.frame.
      kernel$success    <- FALSE
      ds                <- data.frame()
      outcome_message   <- sprintf(
        "The REDCap users export failed.  The http status code was %i.  The 'raw_text' returned was '%s'.",
        kernel$status_code,
        kernel$raw_text
      )
    }       # nocov end
  } else {
    ds                  <- data.frame() #Return an empty data.frame
    outcome_message     <- sprintf(
      "The REDCapR users export operation was not successful.  The error message was:\n%s",
      kernel$raw_text
    )
  }

  if (verbose)
    message(outcome_message)

  list(
    data               = ds,
    success            = kernel$success,
    status_code        = kernel$status_code,
    outcome_message    = outcome_message,
    elapsed_seconds    = kernel$elapsed_seconds,
    raw_text           = kernel$raw_text
  )
}
