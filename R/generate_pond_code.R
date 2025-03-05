
#' Generate codes from Pond ID names
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples

generate_pond_code <- function(name) {

  # Clean up the name by removing non-alphanumeric characters (except spaces)
  clean_name <- stringr::str_replace_all(name, "[^A-Za-z0-9]", "")

  # Take the first 3 characters of the cleaned name (letters only)
  abbreviation <- stringr::str_sub(clean_name, 1, 3)

  # Extract numeric part (if any)
  numeric_suffix <- stringr::str_extract(name, "\\d+")

  # If there's a numeric suffix, append it to the abbreviation (to make sure it's unique)
  if (!is.na(numeric_suffix)) {

    abbreviation <- stringr::str_c(abbreviation, stringr::str_sub(numeric_suffix, 1, 1))

  }

  # If the abbreviation is still less than 4 characters,
  # append a single letter from the cleaned name (or nothing if already at 4 characters)
  if (stringr::str_length(abbreviation) < 4 && is.na(numeric_suffix)) {

    abbreviation <- stringr::str_c(abbreviation, stringr::str_sub(clean_name, 4, 4))

  }

  # Ensure abbreviation length is exactly 4 characters by truncating if necessary
  abbreviation <- stringr::str_sub(abbreviation, 1, 4)

  # Capitalize the abbreviation
  abbreviation <- stringr::str_to_upper(abbreviation)

  return(abbreviation)

}
