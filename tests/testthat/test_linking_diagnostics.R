library(clahrcnwlhf)
context("Linking diagnostics")

test_that("prev.spell and next.spell columns are correctly created by nearest_spells",{

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/episode_link_test_data.Rda")

  # Set up correct results - note this based on the test file created by create_bundle_link_test_dataset
  # in test_bundle_linking.R
  correct_results <- bundle_link_test_data
  correct_results$prev.spell <- NA
  correct_results$next.spell <- NA

  correct_results[correct_results$Bundle.Number == 1, "prev.spell"] <- 179895
  correct_results[correct_results$Bundle.Number == 2, "prev.spell"] <- 108960
  correct_results[correct_results$Bundle.Number == 3, "prev.spell"] <- 199432
  correct_results[correct_results$Bundle.Number == 31, "prev.spell"] <- NA

  correct_results[correct_results$Bundle.Number == 1, "next.spell"] <- 179896
  correct_results[correct_results$Bundle.Number == 2, "next.spell"] <- 108961
  correct_results[correct_results$Bundle.Number == 3, "next.spell"] <- NA
  correct_results[correct_results$Bundle.Number == 31, "next.spell"] <- 51934

  bundles_with_nearest_spells <- nearest_spells(bundles = bundle_link_test_data, episodes = episode_link_test_data)

  # Check the output columns exist
  expect_match(colnames(bundles_with_nearest_spells), "prev.spell", all = FALSE)
  expect_match(colnames(bundles_with_nearest_spells), "next.spell", all = FALSE)

  # Check the values returned in these columns are correct
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 1, "prev.spell"], correct_results[correct_results$Bundle.Number == 1, "prev.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 2, "prev.spell"], correct_results[correct_results$Bundle.Number == 2, "prev.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 3, "prev.spell"], correct_results[correct_results$Bundle.Number == 3, "prev.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 31, "prev.spell"], correct_results[correct_results$Bundle.Number == 31, "prev.spell"])

  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 1, "next.spell"], correct_results[correct_results$Bundle.Number == 1, "next.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 2, "next.spell"], correct_results[correct_results$Bundle.Number == 2, "next.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 3, "next.spell"], correct_results[correct_results$Bundle.Number == 3, "next.spell"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 31, "next.spell"], correct_results[correct_results$Bundle.Number == 31, "next.spell"])

})


test_that("bundle.in.spell column is correctly created by bundle_in_spell",{

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/episode_link_test_data.Rda")

  # Set up correct results - note this based on the test file created by create_bundle_link_test_dataset
  # in test_bundle_linking.R
  correct_results <- bundle_link_test_data
  correct_results$bundle.in.spell <- NA
  correct_results[correct_results$Bundle.Number == 1, "bundle.in.spell"] <- TRUE
  correct_results[correct_results$Bundle.Number == 2, "bundle.in.spell"] <- TRUE
  correct_results[correct_results$Bundle.Number == 3, "bundle.in.spell"] <- TRUE
  correct_results[correct_results$Bundle.Number == 31, "bundle.in.spell"] <- NA
  correct_results[correct_results$Bundle.Number == 5, "bundle.in.spell"] <- FALSE

  # Run the bundle_in_spell function
  bundles_with_in_spell <- bundle_in_spell(bundle_link_test_data, episodes = episode_link_test_data)

  # Check the output column exists
  expect_match(colnames(bundles_with_in_spell), "bundle.in.spell", all = FALSE)

  # Check the values returned in these columns are correct
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$Bundle.Number == 1, "bundle.in.spell"], correct_results[correct_results$Bundle.Number == 1, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$Bundle.Number == 2, "bundle.in.spell"], correct_results[correct_results$Bundle.Number == 2, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$Bundle.Number == 3, "bundle.in.spell"], correct_results[correct_results$Bundle.Number == 3, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$Bundle.Number == 31, "bundle.in.spell"], correct_results[correct_results$Bundle.Number == 31, "bundle.in.spell"])
  expect_equal(bundles_with_in_spell[bundles_with_in_spell$Bundle.Number == 5, "bundle.in.spell"], correct_results[correct_results$Bundle.Number == 5, "bundle.in.spell"])

})


test_that("nearest_spells function returns lag columns",{

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/episode_link_test_data.Rda")

  # Specify correct results
  correct_results <- bundle_link_test_data
  correct_results$lag.from.prev.adm <- NA
  correct_results$lag.to.next.adm <- NA

  correct_results[correct_results$Bundle.Number == 1, "lag.from.prev.adm"] <- as.difftime(2, units = "days")
  correct_results[correct_results$Bundle.Number == 31, "lag.from.prev.adm"] <- NA
  correct_results$lag.from.prev.adm <- as.difftime(correct_results$lag.from.prev.adm, units = "days")

  correct_results[correct_results$Bundle.Number == 1, "lag.to.next.adm"] <- as.difftime(31.52431, units = "days")
  correct_results[correct_results$Bundle.Number == 3, "lag.to.next.adm"] <- NA
  correct_results[correct_results$Bundle.Number == 5, "lag.to.next.adm"] <- as.difftime(45/(60*24), units = "days")
  correct_results$lag.to.next.adm <- as.difftime(correct_results$lag.to.next.adm, units = "days")

  # Run the function
  bundles_with_nearest_spells <- nearest_spells(bundles = bundle_link_test_data, episodes = episode_link_test_data)


  # Check result has a column called "lag.from.prev.adm" with type "difftime"
  expect_match(colnames(bundles_with_nearest_spells), "lag.from.prev.adm", all = FALSE)
  expect_match(colnames(bundles_with_nearest_spells), "lag.to.next.adm", all = FALSE)
  expect_match(class(bundles_with_nearest_spells$lag.from.prev.adm),"difftime")
  expect_match(class(bundles_with_nearest_spells$lag.to.next.adm),"difftime")

  # TODO: Write expectations on results here
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 1, "lag.from.prev.adm"], correct_results[correct_results$Bundle.Number == 1, "lag.from.prev.adm"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 31, "lag.from.prev.adm"], correct_results[correct_results$Bundle.Number == 31, "lag.from.prev.adm"])

  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 1, "lag.to.next.adm"], correct_results[correct_results$Bundle.Number == 1, "lag.to.next.adm"], tolerance = 1e-4)
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 3, "lag.to.next.adm"], correct_results[correct_results$Bundle.Number == 3, "lag.to.next.adm"], tolerance = 1e-4)
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 5, "lag.to.next.adm"], correct_results[correct_results$Bundle.Number == 5, "lag.to.next.adm"], tolerance = 1e-4,
               info = paste(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 5, "lag.to.next.adm"],correct_results[correct_results$Bundle.Number == 5, "lag.to.next.adm"], sep = "---"))


})


test_that("nearest_spells pulls in admission dates for prev.spell and next.spell correctly",{

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/episode_link_test_data.Rda")

  # Specify correct results
  correct_results <- bundle_link_test_data
  #correct_results$prev.adm.dt <- strptime(NA, format = "%Y-%m-%d %H:%M:%S")
  #correct_results$next.adm.dt <- strptime(NA, format = "%Y-%m-%d %H:%M:%S")

  correct_results[correct_results$Bundle.Number == 1, "prev.adm.dt"] <- as.POSIXct(strptime("2015-07-25 14:45:00", format = "%Y-%m-%d %H:%M:%S"))
  correct_results[correct_results$Bundle.Number == 31, "prev.adm.dt"] <- as.POSIXct(strptime(NA, format = "%Y-%m-%d %H:%M:%S"))

  correct_results[correct_results$Bundle.Number == 1, "next.adm.dt"] <- as.POSIXct(strptime("2015-08-28 03:20:00", format = "%Y-%m-%d %H:%M:%S"))
  correct_results[correct_results$Bundle.Number == 3, "next.adm.dt"] <- as.POSIXct(strptime(NA, format = "%Y-%m-%d %H:%M:%S"))

  correct_results$prev.adm.dt <- as.POSIXct(correct_results$prev.adm.dt, origin="1970-01-01")
  correct_results$next.adm.dt <- as.POSIXct(correct_results$next.adm.dt, origin="1970-01-01")

  # Run the function
  bundles_with_nearest_spells <- nearest_spells(bundles = bundle_link_test_data, episodes = episode_link_test_data)

  # Check result has columns called "prev.adm.dt" and "next.adm.dt"
  expect_match(colnames(bundles_with_nearest_spells), "prev.adm.dt", all = FALSE)
  expect_match(colnames(bundles_with_nearest_spells), "next.adm.dt", all = FALSE)

  # Check results are correct
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 1, "prev.adm.dt"], correct_results[correct_results$Bundle.Number == 1, "prev.adm.dt"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 31, "prev.adm.dt"], correct_results[correct_results$Bundle.Number == 31, "prev.adm.dt"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 1, "next.adm.dt"], correct_results[correct_results$Bundle.Number == 1, "next.adm.dt"])
  expect_equal(bundles_with_nearest_spells[bundles_with_nearest_spells$Bundle.Number == 3, "next.adm.dt"], correct_results[correct_results$Bundle.Number == 3, "next.adm.dt"])

})


test_that("plot_lag_dist successfully generates a plot with various arguments", {

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/episode_link_test_data.Rda")

  bundles_with_in_spell <- bundle_in_spell(bundle_link_test_data, episodes = episode_link_test_data)

  p1 <- plot_lag_dist(bundles = bundle_link_test_data,
                      bis = bundles_with_in_spell, prev = TRUE,
                      cumulative = FALSE, facet = FALSE)

  p2 <- plot_lag_dist(bundles = bundle_link_test_data,
                      prev = TRUE,
                      cumulative = FALSE, facet = FALSE, max_lag = 100)

  p3 <- plot_lag_dist(bundles = bundle_link_test_data,
                      prev = FALSE,
                      cumulative = TRUE, facet = TRUE)

  expect_is(p1$layers[[1]], "ggproto")
  expect_identical(sapply(p1$layers, function(x) class(x$geom)[1]),"GeomBar")
  expect_is(p2$layers[[1]], "ggproto")
  expect_identical(sapply(p2$layers, function(x) class(x$geom)[1]),"GeomBar")
  expect_is(p3$layers[[1]], "ggproto")
  expect_identical(sapply(p3$layers, function(x) class(x$geom)[1]),"GeomStep")

})


test_that("plot_linking_venn generates a venn diagram showing linkage results", {

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/nicor_link_test_data.Rda")

  bundle_venn_test_bundle_numbers <- c(1,2,3,31,5,141,439, 152)
  nicor_venn_test_entries <- c(1,1149,72,914,150,604)

  linked_bundles <- link_bundles(bundles = bundle_link_test_data, episodes = episode_link_test_data)
  linked_nicor <- link_nicor(nicor = nicor_link_test_data, episodes = episode_link_test_data)

  venn.d <- plot_linking_venn(episodes = episode_link_test_data,
                              linked_bundles = linked_bundles[which(linked_bundles$Bundle.Number %in% bundle_venn_test_bundle_numbers),],
                              linked_nicor = linked_nicor[which(linked_nicor$nicor.entry.id %in% nicor_venn_test_entries),],
                              plot_vd = FALSE)

  expect_is(venn.d[[1]], "polygon")
  expect_is(venn.d[[length(venn.d)]],"text")

  expect_equal(venn.d[[7]]$label, "3")
  #expect_equal(venn.d[[8]]$label, "263245")
  expect_equal(venn.d[[9]]$label, "5")

})


test_that("duplicated_links returns a dataframe identifying spells linked to multiple bundles",{

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/episode_link_test_data.Rda")

  linked_bundles <- link_bundles(bundles = bundle_link_test_data, episodes = episode_link_test_data)

  duplicated_bundle_spells <- duplicated_links(linked_dataset = linked_bundles)

  expect_equal(nrow(duplicated_bundle_spells), 2)
  expect_equal(duplicated_bundle_spells[1,"linked.spell"],8223)
  expect_equal(duplicated_bundle_spells[2,"linked.spell"],8223)

})


test_that("dupe_link_details returns range of dates on spells from duplicated_links",{

  load("datafortesting/bundle_link_test_data.Rda")
  load("datafortesting/episode_link_test_data.Rda")

  linked_bundles <- link_bundles(bundles = bundle_link_test_data, episodes = episode_link_test_data)

  duplicated_bundle_spells <- duplicated_links(linked_dataset = linked_bundles)

  #Correct results
  max_date <- as.POSIXct(strptime("2016-01-09 19:30:00", format = "%Y-%m-%d %H:%M:%S"))
  min_date <- as.POSIXct(strptime("2016-01-09 15:24:00", format = "%Y-%m-%d %H:%M:%S"))

  #Run dupe_link_details
  dupe_diags <- dupe_link_details(dupe_bundles = duplicated_bundle_spells)

  #Test the results
  expect_equal(nrow(dupe_diags), 1)
  expect_equal(dupe_diags[1,"num.buns"],2)
  expect_equal(dupe_diags[1,"min.bun.adm"], min_date)
  expect_equal(dupe_diags[1,"max.bun.adm"], max_date)
  expect_equal(dupe_diags[1,"diff.bun.adm"], difftime(max_date, min_date, units = "days"))
  expect_equal(dupe_diags[1,"all.in.spell"], FALSE)
  expect_equal(dupe_diags[1,"any.in.spell"], FALSE)

})
