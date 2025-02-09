test_that("Testing the search_value function", {

  # Step 1: Create a sample text vector
  text_lines <- c(
    "Datos paciente: NHC: 0000   Nº Biopsy: MA23B0001-A1   Date: 16-feb-2024"
  )

  # Step 2: Test extraction for a initial pattern
  result1 <- search_value("NHC:", text_lines)
  expect_equal(result1, "0000")  # Expected output: "0000"

  # Step 3: Test extraction for a intermediate pattern
  result2 <- search_value("Nº Biopsy:", text_lines)
  expect_equal(result2, "MA23B0001-A1")  # Expected output: MA23B0001-A1

  # Step 4: Test extraction for a pattern that does not exist
  result3 <- search_value("Nonexistent Field:", text_lines)
  expect_equal(result3, character(0))  # Expected output: Empty vector

  # Step 5: Test extraction for a final pattern
  result4 <- search_value("Date:", text_lines)
  expect_equal(result4, "16-feb-2024")  # Expected output: "16-feb-2024"
})
