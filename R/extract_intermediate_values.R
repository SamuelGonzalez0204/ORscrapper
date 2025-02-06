#' Search for a specific value in text lines
#'
#' This function searches for a specific text pattern in a set of lines and extracts values that follow the pattern.
#'
#' @param search_text Character. The pattern to search for in the text lines.
#' @param lines Character vector. The lines of text to search within.
#' @return A character vector with extracted values matching the search criteria.
#' @export
search_value <- function(search_text, lines) {
  found_values <- character()
  matching_indices <- which(grepl(search_text, lines))
  search_array <- strsplit(search_text, " ")[[1]]

  for (i in matching_indices) {
    line_array <- strsplit(lines[i], " ")[[1]]
    for (j in seq_len(length(line_array) - length(search_array))) {
      segment <- line_array[j:(j + length(search_array) - 1)]
      if (identical(segment, search_array)) {
        next_index <- j + length(search_array)
        result <- character()
        while (!is.na(line_array[next_index]) && line_array[next_index] != "") {
          result <- c(result, line_array[next_index])
          next_index <- next_index + 1
        }
        result <- paste(result, collapse = " ")
        found_values <- c(found_values, result)
      }
    }
  }
  return(found_values)
}

#' Extract intermediate values from text lines
#'
#' This function retrieves unique matches for a search pattern within text lines.
#'
#' @param list_input List. The list to append extracted values to.
#' @param lines Character vector. The text lines to search within.
#' @param search_text Character. The pattern to search for.
#' @return An updated list with appended values.
#' @export
extract_intermediate_values <- function(list_input, lines, search_text) {
  return(unlist(c(list_input, unique(search_value(search_text, lines)))))
}
