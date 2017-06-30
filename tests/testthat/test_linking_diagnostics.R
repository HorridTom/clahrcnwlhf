library(clahrcnwlhf)
context("Linking diagnostics")

test_that("prev.spell and next.spell columns are correctly created by nearest_spells",{

  load("datafortesting/bundle_link_test_data.Rda")

  # Set up correct results - note this based on the test file created by create_bundle_link_test_dataset
  # in test_bundle_linking.R
  correct_results <- bundle_link_test_data
  correct_results$prev.spell <- NA
  correct_results$next.spell <- NA
  correct_results[correct_results$PseudoID == 1217659, "prev.spell"] <- 179895
  correct_results[correct_results$PseudoID == 1132437, "prev.spell"] <- 108960
  correct_results[correct_results$PseudoID == 1217659, "next.spell"] <- 179896
  correct_results[correct_results$PseudoID == 1132437, "next.spell"] <- 108961

  bundles_with_nearest_spells <- nearest_spells(bundles = bundle_link_test_data, episodes = clahrcnwlhf::emergency_adms)

  # Check the output columns exist
  expect_match(colnames(bundles_with_nearest_spells), "prev.spell", all = FALSE)
  expect_match(colnames(bundles_with_nearest_spells), "next.spell", all = FALSE)

  # Check the values returned in these columns are correct
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "prev.spell"], correct_results[correct_results$PseudoID == 1217659, "prev.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1132437, "prev.spell"], correct_results[correct_results$PseudoID == 1132437, "prev.spell"])

  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "next.spell"], correct_results[correct_results$PseudoID == 1217659, "next.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1132437, "next.spell"], correct_results[correct_results$PseudoID == 1132437, "next.spell"])

})
