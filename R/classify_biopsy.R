#' Determine the type of biopsy from identifiers
#'
#' This function analyzes biopsy identifiers and categorizes them into specific types based on a defined rule.
#'
#' @param biopsy_numbers Character vector. Identifiers of biopsies to classify.
#' @return A character vector representing the type of biopsy: '1', '2', or '3'.
#' @export
classify_biopsy <- function(biopsy_numbers) {
  biopsy_lists <- lapply(biopsy_numbers, function(x) list(x))
  biopsy_type_codes <- sapply(biopsy_lists, function(x) substr(x, 5, 5))
  biopsy_types <- ifelse(biopsy_type_codes == "B", '1',
                         ifelse(biopsy_type_codes == "P", '2', '3'))
  return(biopsy_types)
}
