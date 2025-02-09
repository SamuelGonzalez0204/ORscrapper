# Step 1: Test if the function correctly identifies fusion variants in text
test_that("extract_fusions correctly extracts fusion variants", {

  # Create sample input data
  lines <- c(
    " Fusiones de genes (ARN)",
    "Genes ID de la variante Locus Numero de lecturas",
    " CCDC6-RET CCDC6-RET.C1R12.COSF1271 chr10:61665880 - chr10:43612032 3166"
  )

  mutations <- c("BCR", "RET", "EML4", "CCDC6")

  # Execute the function
  result <- extract_fusions(lines, mutations)

  # Expected extracted variants
  expected_variants <- c("CCDC6-RET.C1R12.COSF1271")

  # Check if the extracted variants match the expected ones
  expect_equal(result, expected_variants)
})

# Step 2: Test if the function handles cases with no fusion variants
test_that("extract_fusions returns an empty list when no fusion variants are found", {

  # Create input data with no fusion variants
  lines <- c(
    "This sample contains no known fusion variants.",
    "Just some random genetic markers.",
    "ALK is present but not in a fusion format."
  )

  mutations <- c("BCR", "EML4", "ALK")

  # Execute the function
  result <- extract_fusions(lines, mutations)

  # Check if the function returns an empty character vector
  expect_equal(result, character(0))
})
