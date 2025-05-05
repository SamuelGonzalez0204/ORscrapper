#' Read all PDF files in a directory
#'
#' This function scans a specified directory and retrieves all files with a .pdf extension.
#'
#' @param path Character. Path to the directory to scan for PDF files.
#' @return A character vector with the full paths of the PDF files.
#'
#' @examples
#' InputPath <- system.file("extdata", package = "ORscrapper")
#' files <- read_pdf_files(InputPath)
#'
#' @export
read_pdf_files <- function(path) {
  pdf_files <- list.files(path = path, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  return(pdf_files)
}
