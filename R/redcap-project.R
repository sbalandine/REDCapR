#' @name redcap_project
#' @export redcap_project
#' @importFrom methods new
#'
#' @title A Reference Class to make later calls to REDCap more convenient
#'
#' @description This `Reference Class` represents a REDCap project.
#' Once some values are set that are specific to a REDCap project
#' (such as the URI and token), later calls are less verbose
#' (such as reading and writing data).
#'
#' @field redcap_uri The URI (uniform resource identifier) of the
#' REDCap project.  Required.
#' @field token token The user-specific string that serves as the
#' password for a project.  Required.
#'
#' @examples
#' uri     <- "https://bbmc.ouhsc.edu/redcap/api/"
#' token   <- "D70F9ACD1EDD6F151C6EA78683944E98"
#' \dontrun{
#' project <- REDCapR::redcap_project$new(redcap_uri=uri, token=token)
#' ds_all  <- project$read()
#'
#' # Demonstrate how repeated calls are more concise when the token and
#' #   url aren't always passed.
#' ds_skinny <- project$read(fields=c("record_id", "sex", "height"))$data
#'
#' ids_of_males    <- ds_skinny$record_id[ds_skinny$sex==1]
#' ids_of_shorties <- ds_skinny$record_id[ds_skinny$height < 40]
#'
#' ds_males        <- project$read(records=ids_of_males, batch_size=2)$data
#' ds_shorties     <- project$read(records=ids_of_shorties)$data
#'
#' #Switch the Genders
#' sex_original   <- ds_skinny$sex
#' ds_skinny$sex  <- (1 - ds_skinny$sex)
#' project$write(ds_skinny)
#'
#' #Switch the Genders back
#' ds_skinny$sex <- sex_original
#' project$write(ds_skinny)
#' }
#' 
RedcapProject <-
  R6::R6Class(
    classname = 'RedcapProject',

    private = list(

      .redcap_uri = NULL,
      .redcap_token= NULL

    ),

    public = list(

      initialize = function(
        redcap_uri, 
        redcap_token = NA_character_
      ) {
        private$.redcap_uri = redcap_uri
        private$.redcap_token= redcap_token

        checkmate::assert_character(private$.redcap_uri  , any.missing = FALSE, len = 1, pattern = "^.{1,}$")
        checkmate::assert_character(private$.redcap_token, any.missing = TRUE , len = 1, pattern = "^.{1,}$")
      },

      createProject = function(
        redcap_super_token = NULL,
        title,
        notes = '',
        purpose = 0,
        purpose_other = '',
        is_longitudinal = FALSE,
        is_survey = FALSE,
        is_autonumbering = FALSE,
        verbose = TRUE,
        config_options  = NULL
      ) {

        csv_elements <- NULL #This prevents the R CHECK NOTE: 'No visible binding for global variable Note in R CMD check';  Also see  if( getRversion() >= "2.15.1" )    utils::globalVariables(names=c("csv_elements")) #http://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check; http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

        checkmate::assert_character(redcap_super_token, any.missing = FALSE, len = 1, pattern = "^.{1,}$")

        token   <- sanitize_token(redcap_super_token, super = TRUE)
        verbose <- verbose_prepare(verbose)

        data <- 
          data.frame(
            project_title=title,
            purpose=purpose,
            purpose_other=purpose_other,
            project_notes=notes,
            is_longitudinal = as.integer(is_longitudinal), 
            surveys_enabled = as.integer(is_survey), 
            record_autonumbering_enabled= as.integer(is_autonumbering)
          )

        con     <-  base::textConnection(
          object  = "csv_elements",
          open    = "w",
          local   = TRUE
        )
        utils::write.csv(data, con, row.names = FALSE, na = "")
        close(con)

        csv     <- paste(csv_elements, collapse = "\n")
        rm(csv_elements, con)

        post_body <- list(
          token         = token,
          content       = "project",
          format        = "csv",
          data          = csv,
          returnFormat  = "csv"
        )

        # This is the important line that communicates with the REDCap server.
        kernel <- kernel_api(private$.redcap_uri, post_body, config_options)

        if (kernel$success) {

          outcome_message       <- sprintf(
            "Project created in %0.1f seconds.",
            kernel$elapsed_seconds
          )

          # If an operation is successful, the `raw_text` is no longer returned to save RAM.  The content is not really necessary with httr's status message exposed.
          kernel$raw_text <- ""

        } else { # If the returned content wasn't recognized as a valid integer, then

          outcome_message        <- sprintf(
            "The REDCapR project creation operation was not successful.  The error message was:\n%s",
            kernel$raw_text
          )

        }

        if (verbose)
          message(outcome_message)

        list(
          success                   = kernel$success,
          status_code               = kernel$status_code,
          outcome_message           = outcome_message,
          elapsed_seconds           = kernel$elapsed_seconds,
          raw_text                  = kernel$raw_text
        )
      }
    ),

    active = list(
    )
  )

redcap_project <- setRefClass(
  Class = "redcap_project",

  fields = list(
    redcap_uri  = "character",
    token       = "character"
  ),

  methods = list(

    read = function(
      batch_size                  = 100L,
      interbatch_delay            = 0,
      records                     = NULL,
      records_collapsed           = "",
      fields                      = NULL,
      fields_collapsed            = "",
      forms                       = NULL,
      forms_collapsed             = "",
      events                      = NULL,
      events_collapsed            = "",
      raw_or_label                = "raw",
      raw_or_label_headers        = "raw",
      export_checkbox_label       = FALSE,
      # placeholder returnFormat
      export_survey_fields        = FALSE,
      export_data_access_groups   = FALSE,
      filter_logic                  = "",

      guess_type                    = TRUE,
      guess_max                     = 1000L,
      verbose                       = TRUE,
      config_options                = NULL
    ) {

      "Exports records from a REDCap project."

      return(REDCapR::redcap_read(
        batch_size                    = batch_size,
        interbatch_delay              = interbatch_delay,

        redcap_uri                    = redcap_uri,
        token                         = token,
        records                       = records,
        records_collapsed             = records_collapsed,
        fields                        = fields,
        fields_collapsed              = fields_collapsed,
        forms                         = forms,
        forms_collapsed               = forms_collapsed,
        events                        = events,
        events_collapsed              = events_collapsed,
        raw_or_label                  = raw_or_label,
        raw_or_label_headers          = raw_or_label_headers,
        export_checkbox_label         = export_checkbox_label,
        # placeholder returnFormat
        export_survey_fields          = export_survey_fields,
        export_data_access_groups     = export_data_access_groups,
        filter_logic                  = filter_logic,

        guess_type                    = guess_type,
        # placeholder guess_max
        verbose                       = verbose,
        config_options                = config_options
      ))
    },

    write = function(
      ds_to_write,
      batch_size            = 100L,
      interbatch_delay      = 0,
      continue_on_error     = FALSE,
      verbose               = TRUE,
      config_options        = NULL
    ) {

      "Imports records to a REDCap project."

      return(REDCapR::redcap_write(
        ds_to_write             = ds_to_write,
        batch_size              = batch_size,
        interbatch_delay        = interbatch_delay,
        continue_on_error       = continue_on_error,
        redcap_uri              = redcap_uri,
        token                   = token,
        config_options          = config_options
      ))
    }
  ) #End methods list
) #End class

# http://adv-r.had.co.nz/OO-essentials.html # nolint
# http://stackoverflow.com/questions/21875596/mapping-a-c-sharp-class-definition-to-an-r-reference-class-definition # nolint
