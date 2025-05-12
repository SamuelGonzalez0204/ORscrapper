library(testthat)

create_mock_lines <- function(content_type = "single_mutation", mutations = c("KRAS", "EGFR"), start = "Start Pattern", end = "End Pattern", start2 = NULL, end2 = NULL) {
  base_lines <- character()
  if (!is.null(start)) base_lines <- c(base_lines, start)
  if (!is.null(start2)) base_lines <- c(base_lines, start2)

  if (content_type == "single_mutation") {
    base_lines <- c(base_lines,
                    "KRAS       p.(G12C)         c.34G>T          2       22.51%       Pathogenic info",
                    "Some other text",
                    "More pathogenicity details",
                    "Another unrelated line")
  } else if (content_type == "multiple_mutations") {
    base_lines <- c(base_lines,
                    "KRAS       p.(G12C)         c.34G>T          2       22.51%       Pathogenic details for KRAS",
                    "Intermediate text line",
                    "EGFR       p.(L858R)        c.2573T>C        5       10.00% ",
                    "End of EGFR section")
  } else if (content_type == "missing_patterns") {
    base_lines <- c(base_lines,
                    "TP53 gene_only",
                    "BRCA1 p.(Tyr978fs) 5.55%",
                    "PTEN c.123A>G",
                    "Another line")
  } else if (content_type == "empty_section") {
    base_lines <- c(base_lines,
                    "Some text",
                    "Even more text")
  }

  if (!is.null(end2)) base_lines <- c(base_lines, end2)
  if (!is.null(end)) base_lines <- c(base_lines, end)

  return(base_lines)
}


test_that("extract_values_from_tables handles standard case (single mutation)", {
  # This tests the core logic with one mutation and standard patterns
  lines <- create_mock_lines("single_mutation", start = "Start Pattern", end = "End Pattern")
  mutations <- c("KRAS", "EGFR")

  result <- extract_values_from_tables(lines, mutations, start = "Start Pattern", end = "End Pattern")

  expect_true(is.list(result))
  expect_length(result, 5)
  expect_equal(result[[1]], "KRAS")
  expect_equal(result[[2]], "Pathogenic")
  expect_equal(result[[3]], "22.51%")
  expect_equal(result[[4]], "c.34G>T")
  expect_equal(result[[5]], "p.(G12C)")
})

test_that("extract_values_from_tables handles multiple mutations", {
  # This tests the loop over 'positions' and the pathogenicity check for each mutation
  lines <- create_mock_lines("multiple_mutations", start = "Start Pattern", end = "End Pattern")
  mutations <- c("KRAS", "EGFR")

  result <- extract_values_from_tables(lines, mutations, start = "Start Pattern", end = "End Pattern")

  expect_true(is.list(result))
  expect_length(result, 5)
  expect_equal(result[[1]], c("KRAS", "EGFR"))
  expect_equal(result[[2]], c("Pathogenic", "No results"))
  expect_equal(result[[3]], c("22.51%", "10.00%"))
  expect_equal(result[[4]], c("c.34G>T", "c.2573T>C"))
  expect_equal(result[[5]], c("p.(G12C)", "p.(L858R)"))
})


test_that("extract_values_from_tables handles missing patterns for a mutation", {
  # This tests the 'if (n_mut != length(...))' branches
  lines <- create_mock_lines("missing_patterns", start = "Start Pattern", end = "End Pattern")
  mutations <- c("TP53", "BRCA1", "PTEN")

  result <- extract_values_from_tables(lines, mutations, start = "Start Pattern", end = "End Pattern")

  expect_true(is.list(result))
  expect_length(result, 5)
  expect_equal(result[[1]], c("TP53", "BRCA1", "PTEN"))
  expect_equal(result[[2]], c("No results", "No results", "No results"))
  expect_equal(result[[3]], c("None", "5.55%", "None"))
  expect_equal(result[[4]], c("None", "None", "c.123A>G"))
  expect_equal(result[[5]], c("None", "p.(Tyr978fs)", "None"))
})


test_that("extract_values_from_tables handles edge cases for narrowing text (narrow_text function branches)", {
  mutations <- c("GENE1", "GENE2")

  # Test case 1: Start found, End found (standard - covered by others but good to be explicit)
  lines1 <- c("START", "line 1", "line 2", "END")
  result1 <- extract_values_from_tables(lines1, mutations, start = "START", end = "END")
  expect_equal(result1, list(NULL, NULL, NULL, NULL, NULL))

  # Test case 3: Start found, End2 found, End found (tests 'length(limit_index2) != 0 && length(start_indices) != 0')
  lines2 <- c("START", "line 1", "line 2", "END2", "line 3", "END")
  result2 <- extract_values_from_tables(lines2, mutations, start = "START", end2 = "END2", end = "END")
  expect_equal(result2, list(NULL, NULL, NULL, NULL, NULL))


  # Test case 4: Only Start2 found, End found (tests 'length(start_indices2) != 0')
  lines3 <- c("Intro", "START2", "line A", "line B", "END")
  result3 <- extract_values_from_tables(lines3, mutations, start = "NON_EXISTENT_START", start2 = "START2", end = "END")
  expect_equal(result3, list(NULL, NULL, NULL, NULL, NULL))


  # Test case 5: Neither Start nor Start2 found, End found (tests 'length(start_indices2) == 0 && length(start_indices) == 0')
  lines4 <- c("No start pattern here", "some line", "another line", "END")
  result4 <- extract_values_from_tables(lines4, mutations, start = "NON_EXISTENT_START", start2 = "NON_EXISTENT_START2", end = "END")
  expect_equal(result4, list(NULL, NULL, NULL, NULL, NULL))


  # Test case 6: Start found, but End pattern is MISSING (tests 'length(limit_index) != 0 && !is.na(limit_index)' being FALSE)
  lines5 <- c("START", "line 1", "line 2", "No END pattern here")
  result5 <- extract_values_from_tables(lines5, mutations, start = "START", end = "NON_EXISTENT_END")
  expect_equal(result5, list(NULL, NULL, NULL, NULL, NULL))

})

# Test case from original test - keeping it as it tests no mutations found
test_that("extract_values_from_tables handles no mutations found", {
  lines <- c("Some text", "Another line", "No mutations here")
  mutations <- c("BRCA1", "TP53") # Genes not present in lines

  lines_section <- create_mock_lines("empty_section", start = "Section Start", end = "Section End")

  result_no_mutations <- extract_values_from_tables(lines_section, mutations, start = "Section Start", end = "Section End")
  expect_true(is.list(result_no_mutations))
  expect_length(result_no_mutations, 5)
  expect_equal(result_no_mutations, list(NULL, NULL, NULL, NULL, NULL))
})

# Test case from original test - keeping it
test_that("extract_values_from_tables handles empty input lines", {
  empty_lines <- character()
  mutations <- c("BRCA1")

  result_empty <- extract_values_from_tables(empty_lines, mutations, start = "Start Pattern", end = "End Pattern")

  expect_true(is.list(result_empty))
  expect_length(result_empty, 5)
  expect_equal(result_empty, list(NULL, NULL, NULL, NULL, NULL))
})
