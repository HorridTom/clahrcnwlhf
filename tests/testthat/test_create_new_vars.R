library(clahrcnwlhf)
context("Create new variables ready for ITS regression analysis")

# Test data datafortesting/create_new_vars_test_data.Rda created as follows:
# Random sample of 70 PseudoIDs from emergency_adms, union
# Random sample of 30 PseudoIDs from emergency_adms with (>=1 episode with) Heart.Failure.Episode == TRUE
# Two PseudoIDs known to have multiple nicor records linked to one spell

test_that("Number of spells is preserved",{

  load("datafortesting/create_new_vars_test_data.Rda")
  ward_hf_types <- read.csv(file = "../../data-raw/wardhftypes.csv", stringsAsFactors = FALSE)
  ward_sites <- read.csv(file = "../../data-raw/wardsites.csv", stringsAsFactors = FALSE)
  imd_lookup <- read.csv(file = "../../data-raw/imd-indices.csv", stringsAsFactors = FALSE)

  # Number of spells prior to running create_new_vars
  n_spells_before <- nrow(create_new_vars_test_data %>% dplyr::filter(new_spell == TRUE))

  # Number of spells after running create_new_vars
  n_spells_after <- nrow(create_new_vars(df = create_new_vars_test_data,
                                         from.vignette = TRUE,
                                         ward_hf_types = ward_hf_types,
                                         ward_sites = ward_sites,
                                         imd_lookup = imd_lookup))

  # Test they are the same
  expect_equal(n_spells_after, n_spells_before)

})

test_that("Time to next spell is non-negative",{

  load("datafortesting/create_new_vars_test_data.Rda")
  ward_hf_types <- read.csv(file = "../../data-raw/wardhftypes.csv", stringsAsFactors = FALSE)
  ward_sites <- read.csv(file = "../../data-raw/wardsites.csv", stringsAsFactors = FALSE)
  imd_lookup <- read.csv(file = "../../data-raw/imd-indices.csv", stringsAsFactors = FALSE)

  with_new_vars <- create_new_vars(df = create_new_vars_test_data,
                  from.vignette = TRUE,
                  ward_hf_types = ward_hf_types,
                  ward_sites = ward_sites,
                  imd_lookup = imd_lookup)

  times_to_next_spell <- with_new_vars[!is.na(with_new_vars$time.to.next.spell),"time.to.next.spell"]

  expect_true(as.integer(times_to_next_spell) >= 0)

})



