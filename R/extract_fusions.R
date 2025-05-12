#' Extract fusion variants from text
#'
#' This function identifies and extracts fusion variants from text lines based on specific patterns.
#'
#' @param lines Character vector. Lines of text to search for fusion variants.
#' @param mutations Character vector. List of mutations to look for.
#' @return A list of fusion variants identified in the text.
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
#' fusions <- extract_fusions(lines, mutations)
#'
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
  if (length(variants)!=0){
    return(variants)
  }
  else{
    return("No fusions")
  }
}
