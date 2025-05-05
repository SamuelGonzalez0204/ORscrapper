#' Extract numeric identifiers from file names
#'
#' This function retrieves chip values from file names matching a specific pattern.
#'
#' @param files Character vector. File names to process.
#' @return A character vector of chip identifiers extracted from the file names.
#'
#' @examples
#' InputPath <- system.file("extdata", package = "ORscrapper")
#' files <- read_pdf_files(InputPath)
#'
#' chips <- extract_chip_id(files)
#'
#' @export
extract_chip_id <- function(files) {
  return(gsub(".*?([0-9]+\\.[0-9]+).*", "\\1", files[grep("\\.pdf$", files)]))
}
