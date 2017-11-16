library(clahrcnwlhf)
context("De-duplication of IDs")

test_that("Duplicate IDs are correctly re-assigned ",{
  load("datafortesting/dupe_ID_test_data.Rda")
  load("datafortesting/correct_dupe_ID_test_data.Rda")

  result <- fix_dupe_IDs(dupe_ID_test_data, dupe_ID_test_data, dupe_ID_test_data)[[1]]

  expect_identical(result, correct_dupe_ID_test_data)

})


test_that("replace_ids correctly replaces PseudoIDs",{

  load("datafortesting/dupe_ID_test_data.Rda")

  group_of_ids <- c(1043767, 1213264)

  result <- replace_ids(dupe_ID_test_data, group_of_ids)
  records <- dupe_ID_test_data[which(dupe_ID_test_data$PseudoID %in% group_of_ids),]
  records_result <- result[which(result$PseudoID == group_of_ids[1]),]

  expect_match(as.character(result$PseudoID), as.character(group_of_ids[1]), all = FALSE, info = 'replacement id is present')
  expect_true(all(!(result$PseudoID == group_of_ids[2])), info = 'replaced id is not present')
  expect_identical(records[,!(colnames(records) %in% c('PseudoID'))],records_result[,!(colnames(records_result) %in% c('PseudoID'))], info = 'records identical')


})
