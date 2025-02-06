#' Search for pathogenicity information in NCBI ClinVar
#'
#' This function queries the NCBI ClinVar database for germline classifications based on gene and codification data.
#'
#' @param pathogenicity Ordered list. Existing pathogenicity data.
#' @param genes_mutated Ordered list. Existing mutated gene data.
#' @param total_codifications Ordered list. Existing mutated gen codification data.
#' @return An updated list of pathogenicity classifications based on NCBI ClinVar search results.
#'
#' @import rentrez
#' @export
search_ncbi_clinvar <- function(pathogenicity, genes_mutated, total_codifications) {
  searched_pathogenicity <- list()
  for (list_index in seq_along(pathogenicity)) {
    new_pathogenicity <- c()
    for (element_index in seq_along(pathogenicity[[list_index]])) {
      gene_query <- paste(genes_mutated[[list_index]][[element_index]], "[gene]", total_codifications[[list_index]][[element_index]])
      res <- entrez_search(db = "clinvar", term = gene_query)
      if (length(res$ids) != 0) {
        summary <- entrez_summary(db = "clinvar", id = res$ids[1])
        result <- extract_from_esummary(summary, "germline_classification")
        new_pathogenicity <- c(new_pathogenicity, result$description)
      } else {
        new_pathogenicity <- c(new_pathogenicity, "No results")
      }
    }
    searched_pathogenicity <- c(searched_pathogenicity, list(new_pathogenicity))
  }
  return(searched_pathogenicity)
}
