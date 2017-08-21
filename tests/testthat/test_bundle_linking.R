library(clahrcnwlhf)
context("Bundle linking")


create_bundle_link_test_dataset <- function() {

  bundle_link_cols <- c("Bundle.Number","PseudoID","Admission.Datetime")
  bundle_link_test_bundle_numbers <- c(1,2,3,31,5,141,439, 152, 192, 196)
  bundle_link_test_data <- clahrcnwlhf::bundle_data_clean[which(clahrcnwlhf::bundle_data_clean$Bundle.Number %in% bundle_link_test_bundle_numbers),
                                                          which(colnames(clahrcnwlhf::bundle_data_clean) %in% bundle_link_cols)]
  save(bundle_link_test_data, file="tests/testthat/datafortesting/bundle_link_test_data.Rda")
}


create_bundle_link_dupe_removal_test_dataset <- function() {

  bundle_dupe_link_cols <- c("Bundle.Number","PseudoID","Admission.Datetime",
                             "number.nas")
  bundle_dupe_link_bundle_numbers <- c(192, 196, 193, 198, 591, 698, 207, 213,
                                       234, 771)

  bundle_link_dupe_removal_test_data <-
    clahrcnwlhf::bundle_data_clean[which(clahrcnwlhf::bundle_data_clean$Bundle.Number %in% bundle_dupe_link_bundle_numbers),
                                   which(colnames(clahrcnwlhf::bundle_data_clean) %in% bundle_dupe_link_cols)]
  save(bundle_link_dupe_removal_test_data,
       file="tests/testthat/datafortesting/bundle_link_dupe_removal_test_data.Rda")

}


create_nicor_link_test_dataset <- function() {

  nicor_link_cols <- c("PseudoID","Date.of.Visit", "Hospital", "nicor.entry.id")
  nicor_link_test_entries <- c(1,1147,72,912,150,604,241,280,1162)
  nicor_link_test_data <- clahrcnwlhf::nicor_data_clean[which(clahrcnwlhf::nicor_data_clean$nicor.entry.id %in% nicor_link_test_entries),
                                                        which(colnames(clahrcnwlhf::nicor_data_clean) %in% nicor_link_cols)]
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

  correct_results[correct_results$Bundle.Number == 1, "linked.spell"] <- 179895
  correct_results[correct_results$Bundle.Number == 31, "linked.spell"] <- 51934
  correct_results[correct_results$Bundle.Number == 141, "linked.spell"] <- NA
  correct_results[correct_results$Bundle.Number == 439, "linked.spell"] <- NA
  correct_results[correct_results$Bundle.Number == 152, "linked.spell"] <- NA

  # Run the linking function
  linked_bundles <- link_bundles(bundles = bundle_link_test_data, episodes = clahrcnwlhf::emergency_adms)

  # Test that the linked.spell column exists in the output
  expect_match(colnames(linked_bundles), "linked.spell", all = FALSE)

  # Test that the values returned in this column is correct
  expect_equal(linked_bundles[linked_bundles$Bundle.Number == 1, "linked.spell"], correct_results[correct_results$Bundle.Number == 1, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$Bundle.Number == 31, "linked.spell"], correct_results[correct_results$Bundle.Number == 31, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$Bundle.Number == 141, "linked.spell"], correct_results[correct_results$Bundle.Number == 141, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$Bundle.Number == 439, "linked.spell"], correct_results[correct_results$Bundle.Number == 439, "linked.spell"])
  expect_equal(linked_bundles[linked_bundles$Bundle.Number == 152, "linked.spell"], correct_results[correct_results$Bundle.Number == 152, "linked.spell"])


})


test_that("linked.spell column is created by link_nicor",{

  load("datafortesting/nicor_link_test_data.Rda")

  # Specify correct results
  correct_results <- nicor_link_test_data
  correct_results$linked.spell <- NA

  correct_results[correct_results$nicor.entry.id == 1, "linked.spell"] <- 73029
  correct_results[correct_results$nicor.entry.id == 1147, "linked.spell"] <- NA
  correct_results[correct_results$nicor.entry.id == 72, "linked.spell"] <- 168186
  correct_results[correct_results$nicor.entry.id == 912, "linked.spell"] <- NA
  correct_results[correct_results$nicor.entry.id == 150, "linked.spell"] <- 60960
  correct_results[correct_results$nicor.entry.id == 604, "linked.spell"] <- NA
  correct_results[correct_results$nicor.entry.id == 241, "linked.spell"] <- 71503
  correct_results[correct_results$nicor.entry.id == 280, "linked.spell"] <- 71506
  correct_results[correct_results$nicor.entry.id == 1162, "linked.spell"] <- 71503

  # Run the linking function
  linked_nicor <- link_nicor(nicor = nicor_link_test_data,
                             episodes = clahrcnwlhf::emergency_adms)

  # Test that the linked.spell column exists in the output
  expect_match(colnames(linked_nicor), "linked.spell", all = FALSE)

  # Test that the values returned in this column is correct
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 1, "linked.spell"], correct_results[correct_results$nicor.entry.id == 1, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 1149, "linked.spell"], correct_results[correct_results$nicor.entry.id == 1149, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 72, "linked.spell"], correct_results[correct_results$nicor.entry.id == 72, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 914, "linked.spell"], correct_results[correct_results$nicor.entry.id == 914, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 150, "linked.spell"], correct_results[correct_results$nicor.entry.id == 150, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 604, "linked.spell"], correct_results[correct_results$nicor.entry.id == 604, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 241, "linked.spell"], correct_results[correct_results$nicor.entry.id == 241, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 280, "linked.spell"], correct_results[correct_results$nicor.entry.id == 280, "linked.spell"])
  expect_equal(linked_nicor[linked_nicor$nicor.entry.id == 1162, "linked.spell"], correct_results[correct_results$nicor.entry.id == 1162, "linked.spell"])


})


test_that("Duplicate links are removed correctly", {

  load("datafortesting/bundle_link_dupe_removal_test_data.Rda")
  linked_bundles <- link_bundles(bundles = bundle_link_dupe_removal_test_data,
                                 episodes = clahrcnwlhf::emergency_adms)

  # Specify correct results
  correct_results <- bundle_link_dupe_removal_test_data[
    which(bundle_link_dupe_removal_test_data$Bundle.Number %in% c(196, 198, 591, 207, 771)),]
  correct_results$linked.spell <- NA

  correct_results[correct_results$Bundle.Number == 196, "linked.spell"] <- 8223
  correct_results[correct_results$Bundle.Number == 198, "linked.spell"] <- 13473
  correct_results[correct_results$Bundle.Number == 591, "linked.spell"] <- 51984
  correct_results[correct_results$Bundle.Number == 207, "linked.spell"] <- 147843

  # Run the duplicate link removal function
  deduped_linked_bundles <- remove_duplicate_bundle_links(linked_bundles = linked_bundles)

  # Test that the correct bundles are linked to each spell
  expect_equal(deduped_linked_bundles[deduped_linked_bundles$Bundle.Number == 196, "linked.spell"],
               correct_results[correct_results$Bundle.Number == 196, "linked.spell"])
  expect_equal(deduped_linked_bundles[deduped_linked_bundles$Bundle.Number == 198, "linked.spell"],
               correct_results[correct_results$Bundle.Number == 198, "linked.spell"])
  expect_equal(deduped_linked_bundles[deduped_linked_bundles$Bundle.Number == 591, "linked.spell"],
               correct_results[correct_results$Bundle.Number == 591, "linked.spell"])
  expect_equal(deduped_linked_bundles[deduped_linked_bundles$Bundle.Number == 207, "linked.spell"],
               correct_results[correct_results$Bundle.Number == 207, "linked.spell"])
  expect_equal(deduped_linked_bundles[deduped_linked_bundles$Bundle.Number == 771, "linked.spell"],
               correct_results[correct_results$Bundle.Number == 771, "linked.spell"])

  #Test that each spell only appears once in the deduped data
  expect_equal(length(deduped_linked_bundles[which(!is.na(deduped_linked_bundles$linked.spell)),"linked.spell"]),
               length(unique(deduped_linked_bundles[which(!is.na(deduped_linked_bundles$linked.spell)),"linked.spell"])))

})
