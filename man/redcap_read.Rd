% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap-read.R
\name{redcap_read}
\alias{redcap_read}
\title{Read records from a REDCap project in subsets, and stacks them
together before returning a dataset}
\usage{
redcap_read(
  batch_size = 100L,
  interbatch_delay = 0.5,
  continue_on_error = FALSE,
  redcap_uri,
  token,
  records = NULL,
  records_collapsed = "",
  fields = NULL,
  fields_collapsed = "",
  forms = NULL,
  forms_collapsed = "",
  events = NULL,
  events_collapsed = "",
  raw_or_label = "raw",
  raw_or_label_headers = "raw",
  export_checkbox_label = FALSE,
  export_survey_fields = FALSE,
  export_data_access_groups = FALSE,
  filter_logic = "",
  datetime_range_begin = as.POSIXct(NA),
  datetime_range_end = as.POSIXct(NA),
  col_types = NULL,
  guess_type = TRUE,
  guess_max = NULL,
  verbose = TRUE,
  config_options = NULL,
  id_position = 1L
)
}
\arguments{
\item{batch_size}{The maximum number of subject records a single batch
should contain.  The default is 100.}

\item{interbatch_delay}{The number of seconds the function will wait
before requesting a new subset from REDCap. The default is 0.5 seconds.}

\item{continue_on_error}{If an error occurs while reading, should records
in subsequent batches be attempted.  The default is \code{FALSE}, which prevents
subsequent batches from running.  Required.}

\item{redcap_uri}{The URI (uniform resource identifier) of the REDCap
project.  Required.}

\item{token}{The user-specific string that serves as the password for a
project.  Required.}

\item{records}{An array, where each element corresponds to the ID of a
desired record.  Optional.}

\item{records_collapsed}{A single string, where the desired ID values are
separated by commas.  Optional.}

\item{fields}{An array, where each element corresponds to a desired project
field.  Optional.}

\item{fields_collapsed}{A single string, where the desired field names are
separated by commas.  Optional.}

\item{forms}{An array, where each element corresponds to a desired project
form.  Optional.}

\item{forms_collapsed}{A single string, where the desired form names are
separated by commas.  Optional.}

\item{events}{An array, where each element corresponds to a desired project
event.  Optional.}

\item{events_collapsed}{A single string, where the desired event names are
separated by commas.  Optional.}

\item{raw_or_label}{A string (either \code{'raw'} or \code{'label'} that specifies
whether to export the raw coded values or the labels for the options of
multiple choice fields.  Default is \code{'raw'}.}

\item{raw_or_label_headers}{A string (either \code{'raw'} or \code{'label'} that
specifies for the CSV headers whether to export the variable/field names
(raw) or the field labels (label).  Default is \code{'raw'}.}

\item{export_checkbox_label}{specifies the format of checkbox field values
specifically when exporting the data as labels.  If \code{raw_or_label} is
\code{'label'} and \code{export_checkbox_label} is TRUE, the values will be the text
displayed to the users.  Otherwise, the values will be 0/1.}

\item{export_survey_fields}{A boolean that specifies whether to export the
survey identifier field (e.g., 'redcap_survey_identifier') or survey
timestamp fields (e.g., instrument+'_timestamp') .}

\item{export_data_access_groups}{A boolean value that specifies whether or
not to export the \code{redcap_data_access_group} field when data access groups
are utilized in the project. Default is \code{FALSE}. See the details below.}

\item{filter_logic}{String of logic text (e.g., \verb{[gender] = 'male'}) for
filtering the data to be returned by this API method, in which the API
will only return the records (or record-events, if a longitudinal project)
where the logic evaluates as TRUE.   An blank/empty string returns all records.}

\item{datetime_range_begin}{To return only records that have been created or
modified \emph{after} a given datetime, provide a
\href{https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html}{POSIXct}
value.
If not specified, REDCap will assume no begin time.}

\item{datetime_range_end}{To return only records that have been created or
modified \emph{before} a given datetime, provide a
\href{https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html}{POSIXct}
value.
If not specified, REDCap will assume no end time.}

\item{col_types}{A \code{\link[readr:cols]{readr::cols()}} object passed internally to
\code{\link[readr:read_delim]{readr::read_csv()}}.  Optional.}

\item{guess_type}{A boolean value indicating if all columns should be
returned as character.  If true, \code{\link[readr:read_delim]{readr::read_csv()}} guesses the intended
data type for each column.}

\item{guess_max}{Deprecated.}

\item{verbose}{A boolean value indicating if \code{message}s should be printed
to the R console during the operation.  The verbose output might contain
sensitive information (\emph{e.g.} PHI), so turn this off if the output might
be visible somewhere public. Optional.}

\item{config_options}{A list of options to pass to \code{POST} method in the
\code{httr} package.  See the details in \code{redcap_read_oneshot()} Optional.}

\item{id_position}{The column position of the variable that unique
identifies the subject (typically \code{record_id}).
This defaults to the first variable in the dataset.}
}
\value{
Currently, a list is returned with the following elements:
\itemize{
\item \code{data}: An R \code{\link[base:data.frame]{base::data.frame()}} of the desired records and columns.
\item \code{success}: A boolean value indicating if the operation was apparently
successful.
\item \code{status_codes}: A collection of
\href{http://en.wikipedia.org/wiki/List_of_HTTP_status_codes}{http status codes},
separated by semicolons.  There is one code for each batch attempted.
\item \code{outcome_messages}: A collection of human readable strings indicating the
operations' semicolons.  There is one code for each batch attempted.  In an
unsuccessful operation, it should contain diagnostic information.
\item \code{records_collapsed}: The desired records IDs, collapsed into a single
string, separated by commas.
\item \code{fields_collapsed}: The desired field names, collapsed into a single
string, separated by commas.
\item \code{filter_logic}: The filter statement passed as an argument.
\item \code{elapsed_seconds}: The duration of the function.
}
}
\description{
From an external perspective, this function is similar to
\code{\link[=redcap_read_oneshot]{redcap_read_oneshot()}}.  The internals differ in that \code{redcap_read}
retrieves subsets of the data, and then combines them before returning
(among other objects) a single \code{\link[base:data.frame]{base::data.frame()}}.  This function can
be more appropriate than \code{\link[=redcap_read_oneshot]{redcap_read_oneshot()}} when returning large
datasets that could tie up the server.
}
\details{
Specifically, it internally uses multiple calls to \code{\link[=redcap_read_oneshot]{redcap_read_oneshot()}}
to select and return data.  Initially, only the primary key is queried
through the REDCap API.  The long list is then subsetted into batches,
whose sizes are determined by the \code{batch_size} parameter.  REDCap is then
queried for all variables of the subset's subjects.  This is repeated for
each subset, before returning a unified \code{\link[base:data.frame]{base::data.frame()}}.

The function allows a delay between calls, which allows the server to
attend to other users' requests (such as the users entering data in a
browser).  In other words, a delay between batches does not bog down
the webserver when exporting/importing a large dataset.

A second benefit is less RAM is required on the webserver.  Because
each batch is smaller than the entire dataset, the webserver
tackles more manageably sized objects in memory.  Consider batching
if you encounter the error\preformatted{ERROR: REDCap ran out of server memory. The request cannot be processed.
Please try importing/exporting a smaller amount of data.
}

For \code{\link[=redcap_read]{redcap_read()}} to function properly, the user must have Export
permissions for the 'Full Data Set'.  Users with only 'De-Identified'
export privileges can still use \code{redcap_read_oneshot}.  To grant the
appropriate permissions:
\itemize{
\item go to 'User Rights' in the REDCap project site,
\item select the desired user, and then select 'Edit User Privileges',
\item in the 'Data Exports' radio buttons, select 'Full Data Set'.
}
}
\examples{
\dontrun{
uri     <- "https://bbmc.ouhsc.edu/redcap/api/"
token   <- "9A81268476645C4E5F03428B8AC3AA7B"
REDCapR::redcap_read(batch_size=2, redcap_uri=uri, token=token)$data

# Specify the column types.
col_types <- readr::cols(
  record_id  = readr::col_integer(),
  race___1   = readr::col_logical(),
  race___2   = readr::col_logical(),
  race___3   = readr::col_logical(),
  race___4   = readr::col_logical(),
  race___5   = readr::col_logical(),
  race___6   = readr::col_logical()
)
REDCapR::redcap_read(
  redcap_uri = uri,
  token      = token,
  col_types  = col_types,
  batch_size = 2
)$data

}
}
\references{
The official documentation can be found on the 'API Help Page'
and 'API Examples' pages on the REDCap wiki (\emph{i.e.},
https://community.projectredcap.org/articles/456/api-documentation.html
and
https://community.projectredcap.org/articles/462/api-examples.html).
If you do not have an account for the wiki, please ask your campus REDCap
administrator to send you the static material.
}
\author{
Will Beasley
}
