

#' Read content from a PDF file
#'
#' This function extracts the text content from a PDF file and splits it into individual lines.
#'
#' @param file_path Character. The path to the PDF file.
#' @return A character vector, where each element is a line from the PDF content.
#'
#' @import pdftools
#' @export
read_pdf_content <- function(file_path) {
  doc <- pdf_text(file_path)
  text <- paste(doc, collapse = "\n")
  return(strsplit(text, "\n")[[1]])
}
