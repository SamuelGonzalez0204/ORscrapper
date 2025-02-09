test_that("Testing the extract_values_from_tables function", {

  # Step 1: Define a mock set of text lines that would represent the table in the PDF
  lines <- c("Initial pattern",
             "KRAS        p.(G12C)               c.34G>T                 2          22.51%        1999 chr12:25398285         Conflicting     NM_033360.4           missense",
             "                                                                                                                interpretations",
             "                                                                                                                of",
             "                                                                                                                pathogenicity",
             "Final pattern")
  mutations <- c("KRAS", "SMO")

  # Step 2: Call extract_values_from_tables to extract mutation data
  result <- extract_values_from_tables(lines, mutations, start = "Initial pattern", end = "Final pattern")

  # Step 3: Check that the function correctly extracted the mutation data
  expect_true(length(result) > 0)
  expect_equal(result[[1]], c("KRAS"))
  expect_equal(result[[2]], c("Pathogenic"))
  expect_equal(result[[3]], c("22.51%"))
  expect_equal(result[[4]], c("c.34G>T"))
  expect_equal(result[[5]], c("p.(G12C)"))

  # Step 4: Test if the function handles missing data correctly (e.g., no mutations found)
  result_no_mutations <- extract_values_from_tables(lines, mutations = c("BRCA"))
  expect_equal(result_no_mutations, list(NULL, NULL, NULL, NULL, NULL))

  # Step 5: Test handling of empty lines (no mutations to process)
  empty_lines <- c("No mutations here", "Nothing to extract")
  result_empty <- extract_values_from_tables(empty_lines, mutations)
  expect_equal(result_empty, list(NULL, NULL, NULL, NULL, NULL))

  # Step 6: Test handling of a case with an invalid start pattern in extract_values_from_tables
  lines_invalid_start <- c("Invalid start", "More text", "Gene3 mutation3 p.(Gly>Ser) 15.23%", "Genes analizados")
  result_invalid_start <- extract_values_from_tables(lines_invalid_start, mutations)
  expect_equal(result_invalid_start, list(NULL, NULL, NULL, NULL, NULL))
})
