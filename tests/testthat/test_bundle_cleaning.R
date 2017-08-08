library(clahrcnwlhf)
context("Bundle data cleaning")

create_bundle_completeness_test_dataset <- function() {
  bundle_completeness_test_rows <- c(1,2,3,10,13)
  bundle_completeness_test_data <- clahrcnwlhf::bundle_data_clean[bundle_completeness_test_rows, ]
  save(bundle_completeness_test_data, file="tests/testthat/datafortesting/bundle_completeness_test_data.Rda")
}


test_that("bundle_element_data_completeness returns correct completeness analysis",{

  load("datafortesting/bundle_completeness_test_data.Rda")

  # Specify correct results here
  correct_result <- c(4,5,8,8,0)

  # Run the function
  bundle_completeness <- bundle_element_data_completeness(
    bundles = bundle_completeness_test_data)

  # Test that the results are as expected
  expect_match(colnames(bundle_completeness ), "number.nas", all = FALSE)
  expect_equal(bundle_completeness$number.nas, correct_result, all = TRUE)

})
