#' Extract a subset of text based on start and end patterns
#'
#' This function extracts lines from a text based on specified start and end markers.
#'
#' @param start_text Character. The text marker indicating the beginning of the subset.
#' @param start_text2 Character. An optional secondary start marker.
#' @param lines_total Character vector. The full set of text lines.
#' @param text_limit Character vector. The text marker indicating the end of the subset.
#' @param text_limit2 Character vector. An optional secondary end marker.
#' @return A character vector containing the extracted lines.
narrow_text <- function(start_text, start_text2 = "   Variaciones del número de copias", lines_total, text_limit, text_limit2 = "Comentarios adicionales sobre las variantes") {
  extracted_lines <- character()
  start_indices <- grep(start_text, lines_total)
  start_indices2 <- grep(start_text2, lines_total)
  limit_index <- grep(text_limit, lines_total)[1]
  limit_index2 <- grep(text_limit2, lines_total)

  if (length(start_indices2) != 0 || length(start_indices) > 1) {
    if (length(start_indices2) != 0 && length(start_indices) != 0) {
      extracted_lines <- lines_total[(start_indices + 1):(start_indices2 - 1)]
    } else if (length(limit_index2) != 0 && length(start_indices) != 0) {
      extracted_lines <- lines_total[(start_indices + 1):(limit_index2[1] - 1)]
    } else if (length(start_indices2) != 0) {
      extracted_lines <- lines_total[(start_indices2 + 1):(limit_index - 1)]
    } else {
      extracted_lines <- lines_total[(start_indices + 1):(limit_index - 1)]
    }
  } else if (length(start_indices2) == 0 && length(start_indices) == 0) {
    extracted_lines <- c(extracted_lines, "No biomarkers")
  } else {
    extracted_lines <- lines_total[(start_indices + 1):(limit_index - 1)]
  }
  return(extracted_lines)
}


#' Extract values from tables within text
#'
#' This function analyzes a subset of text lines, extracting information such as mutations, pathogenicity, frequencies, codifications and changes.
#' @param lines Character vector. Lines of text to process.
#' @param mutations Character vector. List of known mutation identifiers.
#' @param genes_mutated Ordered list to store extracted gene data.
#' @param pathogenicity Ordered list to store extracted pathogenicity information.
#' @param frequencies Ordered list to store extracted frequency data.
#' @param codifications Ordered list to store extracted codification data.
#' @param changes Ordered list to store extracted changes data.
#' @param values Aggregated list of extracted information.
#' @param start Starting marker for the relevant table section.
#' @param start2 Secondary starting marker for the table section, in case the table is divided in two pages.
#' @param end text marker indicating the end of the subset.
#' @param end2 secondary end marker.
#' @return A list containing extracted data: genes, pathogenicity, frequencies, codifications and changes.
#'
#' @import stringr
#' @export
extract_values_from_tables <- function(lines, mutations,
                                       genes_mutated = list(), pathogenicity = list(), frequencies = list(),
                                       codifications = list(), changes = list(), values = list(),
                                       start = "Variantes de secuencia de ADN",
                                       start2 = "   Variaciones del número de copias",
                                       end = "Genes analizados",
                                       end2 ="Comentarios adicionales sobre las variantes") {
  lines <- narrow_text(start, start2, lines, end, end2)
  positions <- mut_pats <- freq_list <- pdf_mutations <- patho_list <- cod_list <- change_list <- c()
  split_lines <- strsplit(lines, "\\s+")

  frequency_pattern <- "\\d{2}\\.\\d{2}\\%"
  codification_pattern <- "c\\.[0-9]+[A-Za-z>_]+"

  pos <- 0
  for (line in split_lines) {
    pos <- pos + 1
    if (line[1] %in% mutations) {
      positions <- c(positions, pos)
      pdf_mutations <- c(pdf_mutations, line[1])
      for (item in strsplit(line, " ")) {
        freq_match <- str_match(item, frequency_pattern)
        cod_match <- str_match(item, codification_pattern)
        change_match <- str_match(item, "p\\.\\(.*?\\)")

        if (!is.na(freq_match)) {
          freq_list <- c(freq_list, freq_match[1])
        } else if (!is.na(cod_match)) {
          cod_list <- c(cod_list, cod_match[1])
        }
        if (!is.na(change_match)) {
          change_list <- c(change_list, change_match[1])
        }
      }
    }
  }

  if (length(positions) != 0) {
    for (p in seq_along(positions)) {
      if (p == length(positions)) {
        if (any(grepl("pathogenicity|Pathogenic", lines[positions[p]:length(lines)]))) {
          patho_list <- c(patho_list, "Pathogenic")
        } else {
          patho_list <- c(patho_list, "No results")
        }
      } else if (any(grepl("pathogenicity|Pathogenic", lines[positions[p]:(positions[p + 1] - 1)]))) {
        patho_list <- c(patho_list, "Pathogenic")
      } else {
        patho_list <- c(patho_list, "No results")
      }
    }
  }

  genes_mutated <- c(genes_mutated, list(pdf_mutations))
  pathogenicity <- c(pathogenicity, list(patho_list))
  frequencies <- c(frequencies, list(freq_list))
  codifications <- c(codifications, list(cod_list))
  changes <- append(changes, list(unlist(change_list)))
  values <- c(genes_mutated, pathogenicity, frequencies, codifications, changes)
  return(values)
}
