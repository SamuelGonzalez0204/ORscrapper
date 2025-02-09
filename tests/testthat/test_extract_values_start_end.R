test_that("Testing the extract_values_start_end function", {

  # Step 1: Create a sample text vector
  text_lines <- c(
    "Sample ID: 78910",
    "Gene: EGFR",
    "% células tumorales: 80"
  )

  # Step 2: Initialize an empty list
  extracted_values <- list()

  # Step 3: Extract a value and append to the list
  extracted_values <- extract_values_start_end(extracted_values, text_lines, "Sample ID:\\s*")
  expect_equal(extracted_values[[1]], "78910")  # Expected output: "78910"

  # Step 4: Extract another value and append to the list
  extracted_values <- extract_values_start_end(extracted_values, text_lines, "Gene:\\s*")
  expect_equal(extracted_values[[2]], "EGFR")  # Expected output: "EGFR"

  # Step 5: Extract the tumor cell percentage and append
  extracted_values <- extract_values_start_end(extracted_values, text_lines, ".*% células tumorales:\\s")
  expect_equal(extracted_values[[3]], "80")  # Expected output: "80"

  # Step 6: Try extracting a non-existing value
  extracted_values <- extract_values_start_end(extracted_values, text_lines, "Nonexistent Field:\\s*")
  expect_equal(extracted_values[[4]], "Null")  # Expected output: "Null"
})
