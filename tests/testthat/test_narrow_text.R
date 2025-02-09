test_that("Testing the narrow_text function", {

  # Step 1: Define input lines and expected outputs
  lines <- c("Header1",
             "Start pattern",
             "Extract this text",
             "Middle text",
             "End pattern",
             "Footer")

  # Step 2: Call the narrow_text function to extract lines between markers
  result <- narrow_text("Start pattern", "   Variaciones del número de copias", lines, "End pattern", "Comentarios adicionales sobre las variantes")

  # Step 3: Check that the result matches the expected subset of text
  expect_equal(result, c("Extract this text", "Middle text"))

  # Step 4: Test when no match is found for start text
  result_empty_start <- narrow_text("Nonexistent start pattern", "   Variaciones del número de copias", lines, "End pattern", "Comentarios adicionales sobre las variantes")
  expect_equal(result_empty_start, c("No biomarkers"))

  # Step 5: Test when no match is found for the end pattern
  result_empty_end <- narrow_text("Start pattern", "   Variaciones del número de copias", lines, "Nonexistent end pattern", "Comentarios adicionales sobre las variantes")
  expect_equal(result_empty_end, character())
})
