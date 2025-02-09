test_that("Testing the extract_variable function", {

  # Step 1: Create a sample text vector
  text_lines <- c(
    "Patient ID: 12345",
    "Tumor Type: Lung Cancer",
    "Mutations detected: BRAF V600E",
    "% células tumorales: 60"
  )

  # Step 2: Test extraction with a normal pattern
  result1 <- extract_variable(text_lines, "Patient ID:\\s*")
  expect_equal(result1, "12345")  # Expected output: "12345"

  # Step 3: Test extraction with a different pattern
  result2 <- extract_variable(text_lines, "Tumor Type:\\s*")
  expect_equal(result2, "Lung Cancer")  # Expected output: "Lung Cancer"

  # Step 4: Test extraction of tumor cell percentage (special case)
  result3 <- extract_variable(text_lines, ".*% células tumorales:\\s")
  expect_equal(result3, "60")  # Expected output: "60"

  # Step 5: Test when pattern is not found
  result4 <- extract_variable(text_lines, "Nonexistent Field:\\s*")
  expect_equal(result4, "Null")  # Expected output: "Null"
})
