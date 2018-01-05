# Heart Failure project analysis November 2016
# Step 0 - Recreate Raw Data - Version 0.1
# This script loads the individual component files of the raw dataset
# and stitches them together, saving the result as an .RData file

#' load_data_files
#'
#' load_data_files loads in a set of Excel files as dataframes
#'
#' @param fl list of paths of the files to be loaded
#'
#' @return A list of dataframes, one for each of the file paths in fl.
#' @export
load_data_files <- function(fl) {

  files <- lapply(fl, function(x) gdata::read.xls(x, stringsAsFactors = FALSE))
  files

}

#' merge_data_files
#'
#' Takes a list of dataframes with identical column names and types, and returns a
#' merged dataframe with that same column structure and all the data from the
#' original listed dataframes.
#'
#' @param frame_list list of dataframes to be merged into one
#'
#' @return A dataframe containing all the data from the original listed dataframes
#' @export
merge_data_files <- function(frame_list) {

  # Merge the dataframes passed into one
  do.call("rbind", frame_list)

}

#' load_bundle_data
#'
#' @param fn the file name of the csv file containing the bundle data
#'
#' @return no return value
#' @export
#'
load_bundle_data <- function(fn = "Heart_Failure_Admission_Care_Bundles_Raw.csv") {
  bundle_data <- read.csv(file = paste("data-raw/",fn,sep = ""), stringsAsFactors = FALSE)
  devtools::use_data(bundle_data)
}

#' load_NICOR_data
#'
#' @param fn the file name of the csv file containing the NICOR data
#'
#' @return no return value
#' @export
#'
load_NICOR_data <- function(fn = "NICOR_NPH_CMH_PSEUDO.csv") {
  nicor_data <- read.csv(file = paste("data-raw/",fn,sep = ""), stringsAsFactors = FALSE)
  devtools::use_data(nicor_data)
}

# Load in separate files from NWLH data warehouse and join together.
fileNames <- Sys.glob("data-raw/split*.xlsx")
data_frames <- load_data_files(fileNames)
admission_data <- merge_data_files(data_frames)

# Save the result
devtools::use_data(admission_data)



# To resolve the pseudo-duplication issue in time I had to manually splice the
# data - this sequence contains this, notably involving admission_data_merged
# TODO: code this up properly!

# library(tidyverse)
# adc_new_emsp_nph <- adc_new %>% filter(AdmissionType=='Emergency', EpisodeNumber==1, StartWardSite =='NPH')
# adc_new$CSPAdmissionTime <- as.POSIXct(adc_new$CSPAdmissionTime)
# adc_new$CSPDischargeTime <- as.POSIXct(adc_new$CSPDischargeTime)
# adc_new$EpisodeStartTime <- as.POSIXct(adc_new$EpisodeStartTime)
# adc_new$EpisodeEndTime <- as.POSIXct(adc_new$EpisodeEndTime)
# adc_new_emsp_nph <- adc_new %>% filter(AdmissionType=='Emergency', EpisodeNumber==1, StartWardSite =='NPH')
# n_disch_new <- clahrcnwlhf::disch_time_table(adc_new_emsp_nph)
# n_disch_new
# devtools::load_all(".")
# n_disch_old <- clahrcnwlhf::disch_time_table(clahrcnwlhf::emergency_adms)
# n_disch_old
# n_disch_old <- clahrcnwlhf::disch_time_table(clahrcnwlhf::emergency_adms[which(clahrcnwlhf::emergency_adms$StartWardSite == 'NPH'),])
# n_disch_old
# n_disch_old <- clahrcnwlhf::disch_time_table(clahrcnwlhf::emergency_adms[which(clahrcnwlhf::emergency_adms$StartWardSite == 'NPH' && clahrcnwlhf::emergency_adms$EpisodeNumber == 1),])
# n_disch_old
# load("C:/Users/tw299/Rprojects/clahrcnwlhf/data/emergency_adms.rda")
# View(emergency_adms)
# n_disch_old <- clahrcnwlhf::disch_time_table(clahrcnwlhf::emergency_adms[which(clahrcnwlhf::emergency_adms$StartWardSite == 'NPH' & clahrcnwlhf::emergency_adms$EpisodeNumber == 1),])
# n_disch_old
# admission_data$extract <- 1
# admission_data_new$extract <- 2
# admission_data_merged <- merge_data_files(list(admission_data,admission_data_new))
# admission_data_old <- admission_data
# rm(admission_data)
# admission_data <-admission_data_merged
# rm(admission_data_clean)
# rm(emergency_adms)
# rm(data_frames)
# admission_data <- admission_data[which(!(admission_data$extract == 1 & admission_data$DischargeDate>= as.Date('2016-08-01'))),]
# admission_data <- admission_data[which(!(admission_data$extract == 2 & admission_data$DischargeDate <= as.Date('2016-07-31'))),]
# save(admission_data, file='admission_data_pseudo_uncorr.rda')
# rm(bundle_link_test_data)
# rm(episode_link_test_data)
# rm(nicor_link_test_data)
# ps_mapping <- readxl::read_excel('data-raw/Pseudo_to_Pseudo_20171102_v_1_1.xlsx')
# View(ps_mapping)
# table(ps_mapping$WasMistake)
# admission_data_2 <- dplyr::left_join(admission_data, dplyr::select(ps_mapping, PseudoID, NewPseudo))
# admission_data_2$NewPseudo2 <- NA
# #admission_data_2[which(is.na(admission_data_2$NewPseudo)),"NewPseudo2"] <- admission_data_2[which(is.na(admission_data_2$PseudoID)),"NewPseudo2"]
# admission_data_2[which(is.na(admission_data_2$NewPseudo)),"NewPseudo2"] <- admission_data_2[which(is.na(admission_data_2$NewPseudo)),"PseudoID"]
# admission_data_2[which(!is.na(admission_data_2$NewPseudo)),"NewPseudo2"] <- admission_data_2[which(!is.na(admission_data_2$NewPseudo)),"NewPseudo"]
# table(is.na(admission_data_2$NewPseudo2))
# admission_data_2$OldPseudoID <- admission_data_2$PseudoID
# admission_data_2$PseudoID <- admission_data_2$NewPseudo2
# admission_data_2$NewPseudo <- NULL
# rm(admission_data)
# admission_data <- admission_data_2
# rm(admission_data_2)
# save(admission_data, file = 'data/admission_data.rda')
# devtools::load_all(".")
# devtools::load_all(".")
# load("C:/Users/tw299/Rprojects/clahrcnwlhf/data/admission_data.rda")
# clahrcnwlhf::clean_and_save()
# load("C:/Users/tw299/Rprojects/clahrcnwlhf/data/admission_data_clean.rda")
# n_disch_merged <- clahrcnwlhf::disch_time_table(admission_data_clean)
# n_disch_merged
# clahrcnwlhf::clean_and_save(restrict_disch_date = FALSE)
# library(clahrcnwlhf)
# devtools::load_all(".")
# load("C:/Users/tw299/Rprojects/clahrcnwlhf/data/admission_data_clean.rda")
# n_disch_merged <- clahrcnwlhf::disch_time_table(admission_data_clean)
# n_disch_merged
# emsp_nph <- admission_data_clean[which(admission_data_clean$AdmissionType == 'Emergency' & admission_data_clean$EpisodeNumber == 1, admission_data_clean$StartWardSite == 'NPH'),]
# n_disch_merged <- clahrcnwlhf::disch_time_table(emsp_nph)
# n_disch_merged
# emsp_nph <- admission_data_clean[which(admission_data_clean$AdmissionType == 'Emergency' & admission_data_clean$EpisodeNumber == 1 & admission_data_clean$StartWardSite == 'NPH'),]
# n_disch_merged <- clahrcnwlhf::disch_time_table(emsp_nph)
# n_disch_merged
# matplot(n_disch_merged, type = c("b"), pch=20, col = 1, xaxt="n", ylim=c(0,6500),
# main="Total emergency discharges from Northwick Park Hospital by month")
# axis(side=1,at=1:length(n_disch_merged),labels=names(n_disch_merged), las = 2, cex.axis =0.6)
# clahrcnwlhf::make_emergency_adms_dataset()
# load("C:/Users/tw299/Rprojects/clahrcnwlhf/data/emergency_adms.rda")
# emsp_nph <- emergency_adms[which(emergency_adms$AdmissionType == 'Emergency' & emergency_adms$EpisodeNumber == 1 & emergency_adms$StartWardSite == 'NPH'),]
# n_disch_merged <- clahrcnwlhf::disch_time_table(emsp_nph)
# n_disch_merged

