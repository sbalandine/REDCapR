#' @title Validate and sanitize the user's REDCap token
#'
#' @description Verifies the token is nonmissing and conforms to the legal
#' pattern of a 32-character hexadecimal value.
#' Trailing line endings are removed.
#'
#' @param token The REDCap token. Required.
#'
#' @return The token, without a terminal newline character.
#'
#' @note Contact your institution's REDCap administrator for more information
#' about your project-specific token.
#'
#' @author Hao Zhu, Benjamin Nutter, Will Beasley
#'
#' @examples
#' secret_token_1 <- "12345678901234567890123456ABCDEF"
#' secret_token_2 <- "12345678901234567890123456ABCDEF\n"
#' REDCapR::sanitize_token(secret_token_1)
#' REDCapR::sanitize_token(secret_token_2)

#' @export
sanitize_token <- function(token, super = FALSE) {

  if(super) 
    token_nchar <- 64 else 
    token_nchar <- 32

  pattern <- sprintf("^([0-9A-F]{%s})(?:\\n)?$", token_nchar)

  if (is.na(token)) {
    stop(
      sprintf("The token is `NA`, not a valid %s-character hexademical value.", token_nchar)
    )
  } else if (nchar(token) == 0L) {
    stop(
      "The token is an empty string, ",
      sprintf("not a valid %s-character hexademical value.", token_nchar)
    )
  } else if (!grepl(pattern, token, perl = TRUE)) {
    stop(
      sprintf("The token is not a valid %s-character hexademical value.", token_nchar)
    )
  }

  sub(pattern, "\\1", token, perl = TRUE)
}
