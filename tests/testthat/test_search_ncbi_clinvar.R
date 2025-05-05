library(testthat)
library(mockery)

if (requireNamespace("mockery", quietly = TRUE)) {
# Step 1: Test if the function correctly retrieves pathogenicity information from NCBI ClinVar
test_that("search_ncbi_clinvar correctly retrieves pathogenicity information", {

  # Create sample input data
  pathogenicity <- list(list("Unknown", "Likely pathogenic"))
  genes_mutated <- list(list("BRCA1", "TP53"))
  total_codifications <- list(list("c.1234A>T", "c.5678G>C"))

  # Mock entrez_search to simulate a successful search in ClinVar
  mock_entrez_search <- mockery:::mock(list(ids = c("12345")), cycle = TRUE)
  stub(search_ncbi_clinvar, "entrez_search", mock_entrez_search)

  # Mock entrez_summary to return fake ClinVar data
  mock_entrez_summary <- function(db, id) {
    list(germline_classification = "Pathogenic")
  }
  stub(search_ncbi_clinvar, "entrez_summary", mock_entrez_summary)

  # Mock extract_from_esummary to simulate extraction of germline classification
  mock_extract_from_esummary <- function(summary, field) {
    list(description = "Pathogenic")
  }
  stub(search_ncbi_clinvar, "extract_from_esummary", mock_extract_from_esummary)

  # Execute the function with the mocked responses
  result <- search_ncbi_clinvar(pathogenicity, genes_mutated, total_codifications)

  # Check if the function correctly retrieves "Pathogenic" classification
  expect_equal(result, list(c("Pathogenic", "Pathogenic")))
})

# Step 2: Test if the function handles missing ClinVar results correctly
test_that("search_ncbi_clinvar handles missing ClinVar results correctly", {

  # Create sample input data
  pathogenicity <- list(list("Unknown"))
  genes_mutated <- list(list("BRCA2"))
  total_codifications <- list(list("c.8765T>A"))

  # Mock entrez_search to return no results
  mock_entrez_search_empty <- mock(list(ids = character(0)), cycle = TRUE)
  stub(search_ncbi_clinvar, "entrez_search", mock_entrez_search_empty)

  # Execute the function with the mocked responses
  result <- search_ncbi_clinvar(pathogenicity, genes_mutated, total_codifications)

  # Check if the function correctly returns "No results"
  expect_equal(result, list(c("No results")))
})
}
