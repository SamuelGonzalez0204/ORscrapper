#' Search for pathogenicity information in NCBI ClinVar
#'
#' This function queries the NCBI ClinVar database for germline classifications based on gene and codification data.
#'
#' @param pathogenicity Ordered list. Existing pathogenicity data.
#' @param genes_mutated Ordered list. Existing mutated gene data.
#' @param total_codifications Ordered list. Existing mutated gen codification data.
#' @return An updated list of pathogenicity classifications based on NCBI ClinVar search results.
#'
#' @examples
#' InputPath <- system.file("extdata", package = "ORscrapper")
#' files <- read_pdf_files(InputPath)
#' lines <- read_pdf_content(files[1])  # Example with the first file
#'
#' genes_file <- system.file("extdata/Genes.xlsx", package = "ORscrapper")
#' genes <- readxl::read_excel(genes_file)
#' mutations <- unique(genes$GEN)
#'
#' TableValues <- extract_values_from_tables(lines, mutations)
#' mutateGenes <- TableValues[[1]]
#' pathogenity <- TableValues[[2]]
#' codifications <- TableValues[[4]]
#'
#' search_pathogenity <- search_ncbi_clinvar(pathogenity, mutateGenes, codifications)
#'
#' @import rentrez
#' @export
search_ncbi_clinvar <- function(pathogenicity, genes_mutated, total_codifications) {
  searched_pathogenicity <- list()
  for (list_index in seq_along(pathogenicity)) {
    new_pathogenicity <- c()
    for (element_index in seq_along(pathogenicity[[list_index]])) {
      gene_query <- paste(genes_mutated[[list_index]][[element_index]], "[gene]", total_codifications[[list_index]][[element_index]])
      res <- rentrez::entrez_search(db = "clinvar", term = gene_query)
      if (length(res$ids) != 0) {
        summary <- rentrez::entrez_summary(db = "clinvar", id = res$ids[1])
        result <- rentrez::extract_from_esummary(summary, "germline_classification")
        new_pathogenicity <- c(new_pathogenicity, result$description)
      } else {
        new_pathogenicity <- c(new_pathogenicity, "No results")
      }
    }
    searched_pathogenicity <- c(searched_pathogenicity, list(new_pathogenicity))
  }
  return(searched_pathogenicity)
}

