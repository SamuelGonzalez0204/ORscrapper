#' Filter for pathogenic results only
#'
#' This function filters a list of pathogenicity classifications, retaining only those marked as "Pathogenic".
#'
#' @param pathogenic_list List. A list of pathogenicity classifications.
#' @param related_list List. A list of corresponding data to filter alongside pathogenicity.
#' @return A list containing only the elements of the related list corresponding to "Pathogenic" classifications.
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
#' frequencies <- TableValues[[3]]
#' changes <- TableValues[[5]]
#'
#' pathogenic_mutations <- filter_pathogenic_only(pathogenity, mutateGenes)
#' pathogenic_changes <- filter_pathogenic_only(pathogenity, changes)
#' pathogenic_frequencies <- filter_pathogenic_only(pathogenity, frequencies)
#'
#' @export
filter_pathogenic_only <- function(pathogenic_list, related_list) {
  total_size <- length(pathogenic_list)
  positions <- which(unlist(pathogenic_list) == "Pathogenic")
  filtered_list <- rep(NA, length(unlist(pathogenic_list)))
  filtered_list[positions] <- unlist(related_list)[positions]
  grouped <- gl(total_size, ceiling(length(filtered_list) / total_size))
  return(split(filtered_list, grouped))
}
