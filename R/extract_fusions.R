#' Extract fusion variants from text
#'
#' This function identifies and extracts fusion variants from text lines based on specific patterns.
#'
#' @param lines Character vector. Lines of text to search for fusion variants.
#' @param mutations Character vector. List of mutations to look for.
#' @return A list of fusion variants identified in the text.
#' @export
extract_fusions <- function(lines, mutations) {
  variants <- character()
  for (line in lines) {
    for (mutation in mutations) {
      gene_pattern <- paste0(mutation, "\\.[A-Za-z0-9]+\\.[A-Za-z0-9]+")
      if (grepl(gene_pattern, line)) {
        for (word in strsplit(line, " ")[[1]]) {
          if (grepl(gene_pattern, word)) {
            variants <- c(variants, word)
          }
        }
      }
    }
  }
  return(variants)
}
