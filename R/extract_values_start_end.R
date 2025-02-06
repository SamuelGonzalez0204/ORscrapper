#' Extract variable value from text lines
#'
#' This function searches for a specific pattern in text lines and extracts the corresponding value.
#'
#' @param lines Character vector. The lines of text to search within.
#' @param search_text Character. The regular expression pattern to match.
#' @return The extracted value as a character, or "Null" if not found.
#' @export
extract_variable <- function(lines, search_text) {
  matched_indices <- grep(search_text, lines)
  if (length(matched_indices) != 0) {
    matched_line <- lines[matched_indices[1]]
    if (search_text == ".*% células tumorales:\\s") {
      matched_line <- sub("\\s*/.*", "", matched_line)
    }
    value <- sub(search_text, "", matched_line)
    return(trimws(value))
  } else {
    return("Null")
  }
}

#' Extract values from start or end patterns
#'
#' This function appends extracted variable values based on start or end markers to a list.
#'
#' @param list_input List. The list to append extracted values to.
#' @param lines Character vector. The text lines to search within.
#' @param pattern Character. The pattern to search for.
#' @return An updated list with appended values.
#' @export
extract_values_start_end <- function(list_input, lines, pattern) {
  return(c(list_input, extract_variable(lines, pattern)))
}
