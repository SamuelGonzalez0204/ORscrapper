# Step 1: Test if the function correctly filters "Pathogenic" values
test_that("filter_pathogenic_only retains only 'Pathogenic' elements", {

  # Create sample input data
  pathogenic_list <- list(
    c("Pathogenic", "Benign", "Uncertain"),
    c("Likely Pathogenic", "Pathogenic", "Benign"),
    c("Pathogenic", "Pathogenic", "Uncertain")
  )

  related_list <- list(
    c("Gene1", "Gene2", "Gene3"),
    c("Gene4", "Gene5", "Gene6"),
    c("Gene7", "Gene8", "Gene9")
  )

  # Execute the function
  result <- filter_pathogenic_only(pathogenic_list, related_list)

  # Expected filtered output
  expected_result <- list(
    "1"= c("Gene1", NA, NA),
    "2"= c(NA, "Gene5", NA),
    "3"= c("Gene7", "Gene8", NA)
  )

  # Check if the filtered output matches the expected result
  expect_equal(result, expected_result)
})

# Step 2: Test if the function handles cases where no "Pathogenic" values are present
test_that("filter_pathogenic_only returns NA for lists without 'Pathogenic'", {

  # Create input data with no "Pathogenic" values
  pathogenic_list <- list(
    c("Benign", "Uncertain", "Likely Benign"),
    c("Uncertain", "Benign", "Likely Benign")
  )

  related_list <- list(
    c("GeneA", "GeneB", "GeneC"),
    c("GeneD", "GeneE", "GeneF")
  )

  # Execute the function
  result <- filter_pathogenic_only(pathogenic_list, related_list)

  # Expected output should contain only NAs
  expected_result <- list(
    "1" = c(NA_character_, NA_character_, NA_character_),
    "2" = c(NA_character_, NA_character_, NA_character_)
  )

  # Check if the function correctly handles cases with no "Pathogenic" values
  expect_equal(result, expected_result)
})
