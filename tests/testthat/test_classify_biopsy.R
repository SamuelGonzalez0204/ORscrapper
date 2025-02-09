test_that("Testing the classify_biopsy function", {

  # Step 1: Create a vector of biopsy identifiers for testing
  biopsy_numbers <- c("MA23B0001-A1", "BU24P0013-A1", "BA23C0009-A1", "SA13B0018-C1", "BU44P0001-A3")

  # Step 2: Call the classify_biopsy function
  result <- classify_biopsy(biopsy_numbers)

  # Step 3: Check that the function classifies as expected
  expect_equal(result, c("1", "2", "3", "1", "2"))  # Expected output: '1', '2', '3', '1', '2'

  # Step 4: Test with a single biopsy identifier
  result_single <- classify_biopsy("BU23B0001-A1")
  expect_equal(result_single, c("1"))  # Expected output: "1" for type 'B'

  # Step 5: Test with an empty vector of biopsy identifiers
  result_empty <- classify_biopsy(character(0))
  expect_equal(result_empty, logical(0))  # Expected output: Empty character vector

  # Step 6: Test with invalid biopsy identifiers
  result_invalid <- classify_biopsy(c("invalid1", "invalid2"))
  expect_equal(result_invalid, c("3", "3"))  # Expected output: "3" for invalid identifiers
})
