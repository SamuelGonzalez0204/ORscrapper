test_that("Testing the read_pdf_files function", {

  # Step 1: Set up a temporary directory for the test
  temp_dir <- tempdir()  # Create a temporary directory

  # Step 2: Create mock PDF files in the temporary directory
  # Use file.create to create an empty file with a .pdf extension
  pdf_file_1 <- file.path(temp_dir, "test_file1.pdf")
  pdf_file_2 <- file.path(temp_dir, "test_file2.pdf")
  file.create(pdf_file_1)
  file.create(pdf_file_2)

  # Step 3: Call the function being tested
  # Use read_pdf_files to list the PDF files in the temporary directory
  result <- read_pdf_files(temp_dir)

  # Step 4: Check if the result matches the expected output
  # Expect that the result is a character vector with the paths to the created PDF files
  expect_equal(result, c(pdf_file_1, pdf_file_2))

  # Step 5: Clean up the created files
  unlink(pdf_file_1)
  unlink(pdf_file_2)
})
