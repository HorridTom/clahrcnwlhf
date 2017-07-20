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
  correct_results[correct_results$PseudoID == 1241298, "prev.spell"] <- 199432
  correct_results[correct_results$PseudoID == 1062983, "prev.spell"] <- NA

  correct_results[correct_results$PseudoID == 1217659, "next.spell"] <- 179896
  correct_results[correct_results$PseudoID == 1132437, "next.spell"] <- 108961
  correct_results[correct_results$PseudoID == 1241298, "next.spell"] <- NA
  correct_results[correct_results$PseudoID == 1062983, "next.spell"] <- 51934

  bundles_with_nearest_spells <- nearest_spells(bundles = bundle_link_test_data, episodes = clahrcnwlhf::emergency_adms)

  # Check the output columns exist
  expect_match(colnames(bundles_with_nearest_spells), "prev.spell", all = FALSE)
  expect_match(colnames(bundles_with_nearest_spells), "next.spell", all = FALSE)

  # Check the values returned in these columns are correct
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "prev.spell"], correct_results[correct_results$PseudoID == 1217659, "prev.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1132437, "prev.spell"], correct_results[correct_results$PseudoID == 1132437, "prev.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1241298, "prev.spell"], correct_results[correct_results$PseudoID == 1241298, "prev.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1062983, "prev.spell"], correct_results[correct_results$PseudoID == 1062983, "prev.spell"])

  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "next.spell"], correct_results[correct_results$PseudoID == 1217659, "next.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1132437, "next.spell"], correct_results[correct_results$PseudoID == 1132437, "next.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1241298, "next.spell"], correct_results[correct_results$PseudoID == 1241298, "next.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1062983, "next.spell"], correct_results[correct_results$PseudoID == 1062983, "next.spell"])

})


test_that("bundle.in.spell column is correctly created by bundle_in_spell",{

  load("datafortesting/bundle_link_test_data.Rda")

  # Set up correct results - note this based on the test file created by create_bundle_link_test_dataset
  # in test_bundle_linking.R
  correct_results <- bundle_link_test_data
  correct_results$bundle.in.spell <- NA
  correct_results[correct_results$PseudoID == 1217659, "bundle.in.spell"] <- TRUE
  correct_results[correct_results$PseudoID == 1132437, "bundle.in.spell"] <- TRUE
  correct_results[correct_results$PseudoID == 1241298, "bundle.in.spell"] <- TRUE
  correct_results[correct_results$PseudoID == 1062983, "bundle.in.spell"] <- NA
  correct_results[correct_results$PseudoID == 1076292, "bundle.in.spell"] <- FALSE

  # Run the bundle_in_spell function
  bundles_with_in_spell <- bundle_in_spell(bundle_link_test_data)

  # Check the output column exists
  expect_match(colnames(bundles_with_in_spell), "bundle.in.spell", all = FALSE)

  # Check the values returned in these columns are correct
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$PseudoID == 1217659, "bundle.in.spell"], correct_results[correct_results$PseudoID == 1217659, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$PseudoID == 1132437, "bundle.in.spell"], correct_results[correct_results$PseudoID == 1132437, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$PseudoID == 1241298, "bundle.in.spell"], correct_results[correct_results$PseudoID == 1241298, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$PseudoID == 1062983, "bundle.in.spell"], correct_results[correct_results$PseudoID == 1062983, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$PseudoID == 1076292, "bundle.in.spell"], correct_results[correct_results$PseudoID == 1076292, "bundle.in.spell"])

})


test_that("nearest_spells function returns lag columns",{

  load("datafortesting/bundle_link_test_data.Rda")

  # Specify correct results
  correct_results <- bundle_link_test_data
  correct_results$lag.from.prev.adm <- NA
  correct_results$lag.to.next.adm <- NA

  correct_results[correct_results$PseudoID == 1217659, "lag.from.prev.adm"] <- as.difftime(2, units = "days")
  correct_results[correct_results$PseudoID == 1062983, "lag.from.prev.adm"] <- NA
  correct_results$lag.from.prev.adm <- as.difftime(correct_results$lag.from.prev.adm, units = "days")

  correct_results[correct_results$PseudoID == 1217659, "lag.to.next.adm"] <- as.difftime(31.52431, units = "days")
  correct_results[correct_results$PseudoID == 1241298, "lag.to.next.adm"] <- NA
  correct_results[correct_results$PseudoID == 1076292, "lag.to.next.adm"] <- as.difftime(45/(60*24), units = "days")
  correct_results$lag.to.next.adm <- as.difftime(correct_results$lag.to.next.adm, units = "days")

  # Run the function
  bundles_with_nearest_spells <- nearest_spells(bundles = bundle_link_test_data, episodes = clahrcnwlhf::emergency_adms)


  # Check result has a column called "lag.from.prev.adm" with type "difftime"
  expect_match(colnames(bundles_with_nearest_spells), "lag.from.prev.adm", all = FALSE)
  expect_match(colnames(bundles_with_nearest_spells), "lag.to.next.adm", all = FALSE)
  expect_match(class(bundles_with_nearest_spells$lag.from.prev.adm),"difftime")
  expect_match(class(bundles_with_nearest_spells$lag.to.next.adm),"difftime")

  # TODO: Write expectations on results here
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "lag.from.prev.adm"], correct_results[correct_results$PseudoID == 1217659, "lag.from.prev.adm"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1062983, "lag.from.prev.adm"], correct_results[correct_results$PseudoID == 1062983, "lag.from.prev.adm"])

  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "lag.to.next.adm"], correct_results[correct_results$PseudoID == 1217659, "lag.to.next.adm"], tolerance = 1e-4)
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1241298, "lag.to.next.adm"], correct_results[correct_results$PseudoID == 1241298, "lag.to.next.adm"], tolerance = 1e-4)
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1076292, "lag.to.next.adm"], correct_results[correct_results$PseudoID == 1076292, "lag.to.next.adm"], tolerance = 1e-4,
               info = paste(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1076292, "lag.to.next.adm"],correct_results[correct_results$PseudoID == 1076292, "lag.to.next.adm"], sep = "---"))


})


test_that("nearest_spells pulls in admission dates for prev.spell and next.spell correctly",{

  load("datafortesting/bundle_link_test_data.Rda")

  # Specify correct results
  correct_results <- bundle_link_test_data
  #correct_results$prev.adm.dt <- strptime(NA, format = "%Y-%m-%d %H:%M:%S")
  #correct_results$next.adm.dt <- strptime(NA, format = "%Y-%m-%d %H:%M:%S")

  correct_results[correct_results$PseudoID == 1217659, "prev.adm.dt"] <- as.POSIXct(strptime("2015-07-25 14:45:00", format = "%Y-%m-%d %H:%M:%S"))
  correct_results[correct_results$PseudoID == 1062983, "prev.adm.dt"] <- as.POSIXct(strptime(NA, format = "%Y-%m-%d %H:%M:%S"))

  correct_results[correct_results$PseudoID == 1217659, "next.adm.dt"] <- as.POSIXct(strptime("2015-08-28 03:20:00", format = "%Y-%m-%d %H:%M:%S"))
  correct_results[correct_results$PseudoID == 1241298, "next.adm.dt"] <- as.POSIXct(strptime(NA, format = "%Y-%m-%d %H:%M:%S"))

  correct_results$prev.adm.dt <- as.POSIXct(correct_results$prev.adm.dt, origin="1970-01-01")
  correct_results$next.adm.dt <- as.POSIXct(correct_results$next.adm.dt, origin="1970-01-01")

  # Run the function
  bundles_with_nearest_spells <- nearest_spells(bundles = bundle_link_test_data, episodes = clahrcnwlhf::emergency_adms)

  # Check result has columns called "prev.adm.dt" and "next.adm.dt"
  expect_match(colnames(bundles_with_nearest_spells), "prev.adm.dt", all = FALSE)
  expect_match(colnames(bundles_with_nearest_spells), "next.adm.dt", all = FALSE)

  # Check results are correct
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "prev.adm.dt"], correct_results[correct_results$PseudoID == 1217659, "prev.adm.dt"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1062983, "prev.adm.dt"], correct_results[correct_results$PseudoID == 1062983, "prev.adm.dt"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1217659, "next.adm.dt"], correct_results[correct_results$PseudoID == 1217659, "next.adm.dt"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$PseudoID == 1241298, "next.adm.dt"], correct_results[correct_results$PseudoID == 1241298, "next.adm.dt"])

})
