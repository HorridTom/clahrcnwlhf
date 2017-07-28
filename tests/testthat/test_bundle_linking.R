library(clahrcnwlhf)
context("Bundle linking")

create_bundle_link_test_dataset <- function() {

  bundle_link_cols <- c("Bundle.Number","PseudoID","Admission.Datetime")
  bundle_link_test_rows <- c(1,2,3,31,5,141,439, 152, 192, 196)
  bundle_link_test_data <- clahrcnwlhf::bundle_data_clean[bundle_link_test_rows, which(colnames(clahrcnwlhf::bundle_data_clean) %in% bundle_link_cols)]
  save(bundle_link_test_data, file="tests/testthat/datafortesting/bundle_link_test_data.Rda")
}


create_nicor_link_test_dataset <- function() {

  nicor_link_cols <- c("PseudoID","Date.of.Visit")
  nicor_link_test_rows <- c(1,1147,72,912)
  nicor_link_test_data <- clahrcnwlhf::nicor_data_clean[nicor_link_test_rows, which(colnames(clahrcnwlhf::nicor_data_clean) %in% nicor_link_cols)]
  save(nicor_link_test_data, file="tests/testthat/datafortesting/nicor_link_test_data.Rda")
}


test_that("linked.spell column is created by link_bundles",{

  load("datafortesting/bundle_link_test_data.Rda")

  # Best linking algorithm idea at present:
  # 1. Any bundle with bundle.in.spell == TRUE should be linked to its prev.spell (320)
  # 2. Of the rest (449), any bundle with lag.to.next.adm <= 3 days (361) should be linked to its next.spell
  # 3. This leaves:
  #   a) 11 bundles not in spell with lag.to.next.adm > 3 days
  #   b) 77 bundles not in spell with no subsequent admission. The distribution of lag.from.prev.adm
  #       for this subset shows that they are not close in time to the previous admission.
  #
  #       NB a subset of 25 these have *no* admissions in the data warehouse for the patient id
  #       on the bundle.
  #
  #   These 88 bundles under (3) require further investigation.

  # Specify correct results
  correct_results <- bundle_link_test_data
  correct_results$linked.spell <- NA

  correct_results[correct_results$PseudoID == 1217659, "linked.spell"] <- 179895
  correct_results[correct_results$PseudoID == 1062983, "linked.spell"] <- 51934
  correct_results[correct_results$PseudoID == 1053505, "linked.spell"] <- NA
  correct_results[correct_results$PseudoID == 1163163, "linked.spell"] <- NA
  correct_results[correct_results$PseudoID == 1257929, "linked.spell"] <- NA

  # Run the linking function
  linked_bundles <- link_bundles(bundles = bundle_link_test_data, episodes = clahrcnwlhf::emergency_adms)

  # Test that the linked.spell column exists in the output
  expect_match(colnames(linked_bundles), "linked.spell", all = FALSE)

  # Test that the values returned in this column is correct
  expect_equal(linked_bundles[linked_bundles$PseudoID == 1217659, "linked.spell"], correct_results[correct_results$PseudoID == 1217659, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$PseudoID == 1062983, "linked.spell"], correct_results[correct_results$PseudoID == 1062983, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$PseudoID == 1053505, "linked.spell"], correct_results[correct_results$PseudoID == 1053505, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$PseudoID == 1163163, "linked.spell"], correct_results[correct_results$PseudoID == 1163163, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$PseudoID == 1257929, "linked.spell"], correct_results[correct_results$PseudoID == 1257929, "linked.spell"])


})


test_that("linked.spell column is created by link_nicor",{

  load("datafortesting/nicor_link_test_data.Rda")

  # Specify correct results
  correct_results <- nicor_link_test_data
  correct_results$linked.spell <- NA

  correct_results[correct_results$PseudoID == 1088990, "linked.spell"] <- 73029
  correct_results[correct_results$PseudoID == 1280004, "linked.spell"] <- NA
  correct_results[correct_results$PseudoID == 1203126, "linked.spell"] <- 168186
  correct_results[correct_results$PseudoID == 2000010, "linked.spell"] <- NA

  # Run the linking function
  linked_nicor <- link_nicor(nicor = nicor_link_test_data,
                             episodes = clahrcnwlhf::emergency_adms)

  # Test that the linked.spell column exists in the output
  expect_match(colnames(linked_nicor), "linked.spell", all = FALSE)

  # Test that the values returned in this column is correct
  expect_equal(linked_nicor[linked_nicor$PseudoID == 1088990, "linked.spell"], correct_results[correct_results$PseudoID == 1088990, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$PseudoID == 1280004, "linked.spell"], correct_results[correct_results$PseudoID == 1280004, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$PseudoID == 1203126, "linked.spell"], correct_results[correct_results$PseudoID == 1203126, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$PseudoID == 2000010, "linked.spell"], correct_results[correct_results$PseudoID == 2000010, "linked.spell"])


})
