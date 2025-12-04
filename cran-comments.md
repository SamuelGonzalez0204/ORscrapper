## Test environments
* local Windows 11 install, R 4.5.2
* win-builder (devel and release)
* R-hub (Fedora Linux, Ubuntu Linux, macOS)

## R CMD check results
0 errors | 0 warnings | 1 note

* Note: "New submission" -> This is a first submission.
* Note: "Possibly misspelled words" -> 'ClinVar', 'Oncomine', and 'ORscrapper' are correct domain-specific terms/names.

## Reverse dependencies
This is a new submission, so there are no reverse dependencies.

## Comments for CRAN
This package queries the NCBI ClinVar API via 'rentrez'.
- All tests involving the API are mocked using 'mockery' and do not access the network.
- All examples in .Rd files that access the API are wrapped in \dontrun{}.
- The vignette uses pre-calculated cached data and performs no live API calls during compilation.

SystemRequirements 'poppler-cpp' is declared for the 'pdftools' dependency.
