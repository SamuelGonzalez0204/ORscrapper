test_that("Testing the read_pdf_content function", {

  # Step 1: Define the test PDF file path (you can use an example PDF in the extdata folder)
  file_path <- system.file("extdata", "135.6ANONIMO.pdf", package = "ORscrapper")

  # Step 2: Call the function to extract content from the PDF
  result <- read_pdf_content(file_path)

  # Step 3: Check if the result is a character vector
  expect_type(result, "character")  # Ensure the result is a character vector

  # Step 4: Check if the result contains at least one line of text
  expect_gt(length(result), 0)  # Ensure there is at least one line of text

  # Step 5: Check if the result contains specific expected text
  expect_true(any(grepl("Variantes de secuencia de ADN", result)))
})
