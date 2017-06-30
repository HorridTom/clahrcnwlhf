library(clahrcnwlhf)
context("Linking diagnostics")

test_that("prev.spell column is correctly created by prev_spell",{

  load("datafortesting/bundle_link_test_data.Rda")

  # Set up correct results - note this based on the test file created by create_bundle_link_test_dataset
  # in test_bundle_linking.R
  correct_results <- bundle_link_test_data
  correct_results$prev.spell <- NA
  correct_results[correct_results$PseudoID == 1217659, "prev.spell"] <- 179895
  correct_results[correct_results$PseudoID == 1132437, "prev.spell"] <- 108960

  bundles_with_prev_spell <- prev_spell(bundles = bundle_link_test_data, episodes = clahrcnwl::emergency_adms)

  expect_match(colnames(bundles_with_prev_spell), "prev.spell", all = FALSE)

  expect_equal(bundles_with_prev_spell[bundles_with_prev_spell$PseudoID == 1217659, "prev.spell"], correct_results[correct_results$PseudoID == 1217659, "prev.spell"])
  expect_equal(bundles_with_prev_spell[bundles_with_prev_spell$PseudoID == 1132437, "prev.spell"], correct_results[correct_results$PseudoID == 1132437, "prev.spell"])

})
