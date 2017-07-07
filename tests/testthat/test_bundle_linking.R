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

  # Best linking algorithm idea at present:
  # 1. Any bundle with bundle.in.spell == TRUE should be linked to its prev.spell (320)
  # 2. Of the rest (449), any bundle with lag.to.next.adm <= 3 days (361) should be linked to its next.spell
  # 3. This leaves:
  #   a) 11 bundles not in spell with lag.to.next.adm > 3 days
  #   b) 77 bundles not in spell with no subsequent admission. The distribution of lag.from.prev.adm
  #       for this subset shows that they are not close in time to the previous admission.
  # These 88 bundles under (3) require further investigation.

})
