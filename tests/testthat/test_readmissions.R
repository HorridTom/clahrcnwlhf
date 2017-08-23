library(clahrcnwlhf)
context("Readmission status calculation")

create_readmission_test_dataset <- function() {

  readm_cols <- c("PseudoID", "AdmissionDate", "CSPAdmissionTime",
                  "DischargeDate", "CSPDischargeTime", "Heart.Failure.Episode",
                  "new.pat", "new_spell", "spell_number")
  pt_numbers <- c(1000003, 1000013, 1000014, 1006081, 1011392, 1016198)
  readm_test_data <- clahrcnwlhf::emergency_adms[which(clahrcnwlhf::emergency_adms$PseudoID %in% pt_numbers &
                                                         clahrcnwlhf::emergency_adms$new_spell == TRUE),
                                                          which(colnames(clahrcnwlhf::emergency_adms) %in% readm_cols)]

  readm_test_data$CSPAdmissionTime <- as.POSIXct(readm_test_data$CSPAdmissionTime)
  readm_test_data$CSPDischargeTime <- as.POSIXct(readm_test_data$CSPDischargeTime)

  save(readm_test_data, file="tests/testthat/datafortesting/readm_test_data.Rda")
}


test_that("Readmission columns are correctly created",{

  load("datafortesting/readm_test_data.Rda")

  # Correct results
  correct_results_raw <- read.csv(file = "datafortesting/readm_test_correct.csv", stringsAsFactors = FALSE)
  correct_results <- dplyr::left_join(readm_test_data, correct_results_raw[,c("spell_number","time.to.next.spell","time.to.next.hf.spell", "all.cause.readmission.cat", "hf.readmission.cat")], by = c("spell_number"))
  correct_results$time.to.next.spell <- as.difftime(correct_results$time.to.next.spell, units = "days")
  correct_results$time.to.next.hf.spell <- as.difftime(correct_results$time.to.next.hf.spell, units = "days")
  correct_results$all.cause.readmission.cat <- factor(correct_results$all.cause.readmission.cat,
                                                      levels=c("7","30","90"), exclude = NULL)
  correct_results$hf.readmission.cat <- factor(correct_results$hf.readmission.cat,
                                                      levels=c("7","30","90"), exclude = NULL)

  # Run function
  readms <- clahrcnwlhf::create_readms(readm_test_data)

  # Test that the time.to.next.spell and time.to.next.hf.spell columns exist
  expect_match(colnames(readms), "time.to.next.spell", all = FALSE)
  expect_match(colnames(readms), "time.to.next.hf.spell", all = FALSE)
  expect_match(colnames(readms), "all.cause.readmission.cat", all = FALSE)
  expect_match(colnames(readms), "hf.readmission.cat", all = FALSE)


  # Test the results are correct
  expect_equivalent(readms[,c("spell_number","time.to.next.spell")],
               correct_results[,c("spell_number","time.to.next.spell")])
  expect_equivalent(readms[,c("spell_number","time.to.next.hf.spell")],
               correct_results[,c("spell_number","time.to.next.hf.spell")])
  expect_equivalent(readms[,c("spell_number","all.cause.readmission.cat")],
                    correct_results[,c("spell_number","all.cause.readmission.cat")])
  expect_equivalent(readms[,c("spell_number","hf.readmission.cat")],
                    correct_results[,c("spell_number","hf.readmission.cat")])


})
