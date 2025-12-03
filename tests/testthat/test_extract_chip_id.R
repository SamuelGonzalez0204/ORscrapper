test_that("Testing the extract_chip_id function", {

  # Step 1: Using the system.file() function to get the path of the test PDF file stored in the package's extdata folder
  file_path <- system.file("extdata", "100.1-example.pdf", package = "ORscrapper")

  # Step 2: Call the function being tested and store the result
  result <- extract_chip_id(file_path)

  # Step 3: Check if the result matches the expected value
  expect_equal(result, "100.1")
})
