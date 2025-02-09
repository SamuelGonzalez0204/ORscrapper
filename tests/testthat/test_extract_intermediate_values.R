test_that("Testing the extract_intermediate_values function", {

  # Step 1: Create a sample text vector
  text_lines <- c(
    "Datos paciente: NHC: 0000   Nº Biopsy: MA23B0001-A1   Date: 16-feb-2024"
  )

  # Step 2: Initialize an empty list
  extracted_values <- list()

  # Step 3: Extract and append values to the list
  extracted_values <- extract_intermediate_values(extracted_values, text_lines, "Nº Biopsy:")

  # Step 4: Verify the list contains unique extracted values
  expect_equal(extracted_values, c("MA23B0001-A1"))  # Expected output: MA23B0001-A1

  # Step 5: Try extracting a non-existing pattern
  extracted_values <- extract_intermediate_values(extracted_values, text_lines, "Nonexistent Pattern:")
  expect_equal(extracted_values, c("MA23B0001-A1"))  # Should remain unchanged
})
