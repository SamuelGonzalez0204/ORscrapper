library(roxygen2, devtools)

#' @import pdftools
#' @import stringr
#' @import dplyr
#' @import rentrez


#' Read all PDF files in a directory
#'
#' This function scans a specified directory and retrieves all files with a .pdf extension.
#'
#' @param path Character. Path to the directory to scan for PDF files.
#' @return A character vector with the full paths of the PDF files.
#' @export
read_pdf_files <- function(path) {
  pdf_files <- list.files(path = path, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  return(pdf_files)
}


#' Read content from a PDF file
#'
#' This function extracts the text content from a PDF file and splits it into individual lines.
#'
#' @param file_path Character. The path to the PDF file.
#' @return A character vector, where each element is a line from the PDF content.
#' @export
read_pdf_content <- function(file_path) {
  doc <- pdf_text(file_path)
  text <- paste(doc, collapse = "\n")
  return(strsplit(text, "\n")[[1]])
}

#' Search for a specific value in text lines
#'
#' This function searches for a specific text pattern in a set of lines and extracts values that follow the pattern.
#'
#' @param search_text Character. The pattern to search for in the text lines.
#' @param lines Character vector. The lines of text to search within.
#' @return A character vector with extracted values matching the search criteria.
#' @export
search_value <- function(search_text, lines) {
  found_values <- character()
  matching_indices <- which(grepl(search_text, lines))
  search_array <- strsplit(search_text, " ")[[1]]

  for (i in matching_indices) {
    line_array <- strsplit(lines[i], " ")[[1]]
    for (j in seq_len(length(line_array) - length(search_array))) {
      segment <- line_array[j:(j + length(search_array) - 1)]
      if (identical(segment, search_array)) {
        next_index <- j + length(search_array)
        result <- character()
        while (!is.na(line_array[next_index]) && line_array[next_index] != "") {
          result <- c(result, line_array[next_index])
          next_index <- next_index + 1
        }
        result <- paste(result, collapse = " ")
        found_values <- c(found_values, result)
      }
    }
  }
  return(found_values)
}

#' Extract variable value from text lines
#'
#' This function searches for a specific pattern in text lines and extracts the corresponding value.
#'
#' @param lines Character vector. The lines of text to search within.
#' @param search_text Character. The regular expression pattern to match.
#' @return The extracted value as a character, or "Null" if not found.
#' @export
extract_variable <- function(lines, search_text) {
  matched_indices <- grep(search_text, lines)
  if (length(matched_indices) != 0) {
    matched_line <- lines[matched_indices[1]]
    if (search_text == ".*% células tumorales:\\s") {
      matched_line <- sub("\\s*/.*", "", matched_line)
    }
    value <- sub(search_text, "", matched_line)
    return(trimws(value))
  } else {
    return("Null")
  }
}

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
#' @export
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

#' Extract values from start or end patterns
#'
#' This function appends extracted variable values based on start or end markers to a list.
#'
#' @param list_input List. The list to append extracted values to.
#' @param lines Character vector. The text lines to search within.
#' @param pattern Character. The pattern to search for.
#' @return An updated list with appended values.
#' @export
extract_values_start_end <- function(list_input, lines, pattern) {
  return(c(list_input, extract_variable(lines, pattern)))
}

#' Extract intermediate values from text lines
#'
#' This function retrieves unique matches for a search pattern within text lines.
#'
#' @param list_input List. The list to append extracted values to.
#' @param lines Character vector. The text lines to search within.
#' @param search_text Character. The pattern to search for.
#' @return An updated list with appended values.
#' @export
extract_intermediate_values <- function(list_input, lines, search_text) {
  return(unlist(c(list_input, unique(search_value(search_text, lines)))))
}

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

#' Extract numeric identifiers from file names
#'
#' This function retrieves chip values from file names matching a specific pattern.
#'
#' @param files Character vector. File names to process.
#' @return A character vector of chip identifiers extracted from the file names.
#' @export
extract_chip_id <- function(files) {
  return(gsub(".*?([0-9]+\\.[0-9]+).*", "\\1", files[grep("\\.pdf$", files)]))
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

#' Search for pathogenicity information in NCBI ClinVar
#'
#' This function queries the NCBI ClinVar database for germline classifications based on gene and codification data.
#'
#' @param pathogenicity Ordered list. Existing pathogenicity data.
#' @param genes_mutated Ordered list. Existing mutated gene data.
#' @param total_codifications Ordered list. Existing mutated gen codification data.
#' @return An updated list of pathogenicity classifications based on NCBI ClinVar search results.
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

#' Extract fusion variants from text
#'
#' This function identifies and extracts fusion variants from text lines based on specific patterns.
#'
#' @param lines Character vector. Lines of text to search for fusion variants.
#' @param mutations Character vector. List of mutations to look for.
#' @return A list of fusion variants identified in the text.
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
  return(variants)
}

#' Filter for pathogenic results only
#'
#' This function filters a list of pathogenicity classifications, retaining only those marked as "Pathogenic".
#'
#' @param pathogenic_list List. A list of pathogenicity classifications.
#' @param related_list List. A list of corresponding data to filter alongside pathogenicity.
#' @return A list containing only the elements of the related list corresponding to "Pathogenic" classifications.
#' @export
filter_pathogenic_only <- function(pathogenic_list, related_list) {
  total_size <- length(pathogenic_list)
  positions <- which(unlist(pathogenic_list) == "Pathogenic")
  filtered_list <- rep(NA, length(unlist(pathogenic_list)))
  filtered_list[positions] <- unlist(related_list)[positions]
  grouped <- gl(total_size, ceiling(length(filtered_list) / total_size))
  return(split(filtered_list, grouped))
}
