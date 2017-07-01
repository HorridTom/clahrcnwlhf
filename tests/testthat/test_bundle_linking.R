library(clahrcnwlhf)
context("Bundle linking")

create_bundle_link_test_dataset <- function() {

  bundle_link_cols <- c("Bundle.Number","PseudoID","Admission.Datetime")
  bundle_link_test_rows <- c(1,2,3,31,5)
  bundle_link_test_data <- clahrcnwlhf::bundle_data_clean[bundle_link_test_rows, which(colnames(clahrcnwlhf::bundle_data_clean) %in% bundle_link_cols)]
  save(bundle_link_test_data, file="tests/testthat/datafortesting/bundle_link_test_data.Rda")
}


test_that("linked.spell column is created by link_bundles",{

  load("datafortesting/bundle_link_test_data.Rda")
  linked_bundles <- link_bundles(bundles = bundle_link_test_data, episodes = clahrcnwl::emergency_adms)
  expect_match(colnames(linked_bundles), "linked.spell", all = FALSE)

})
