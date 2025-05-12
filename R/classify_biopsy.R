#' Determine the type of biopsy from identifiers
#'
#' This function analyzes biopsy identifiers and categorizes them into specific
#' types based on a defined rule.
#'
#' @param biopsy_numbers Character vector. Identifiers of biopsies to classify.
#' @return A character vector representing the type of Sample type:
#'          1, biopsy
#'          2, aspiration
#'          3, cytology
#'
#' @examples
#' InputPath <- system.file("extdata", package = "ORscrapper")
#' files <- read_pdf_files(InputPath)
#' lines <- read_pdf_content(files[1])  # Example with the first file
#'
#' NB_values <- c()
#' NB_values <- extract_intermediate_values(NB_values, lines, "biopsia:")
#'
#' biopsies_identifiers <- classify_biopsy(NB_values)
#'
#' @export
classify_biopsy <- function(biopsy_numbers) {
  biopsy_lists <- lapply(biopsy_numbers, function(x) list(x))
  biopsy_type_codes <- sapply(biopsy_lists, function(x) substr(x, 5, 5))
  biopsy_types <- ifelse(biopsy_type_codes == 'B', '1',
                         ifelse(biopsy_type_codes == 'P', '2', '3'))
  return(biopsy_types)
}
