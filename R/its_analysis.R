
#' its_aggregate_wide
#'
#' @param df emergency admissions episode data
#'
#' @return aggregated data for ITS analysis
#' @export
#'
its_aggregate_wide <- function(df = clahrcnwlhf::emergency_adms,
                               from.vignette = FALSE, ward_hf_types = NULL,
                               ward_sites = NULL, imd_lookup = NULL) {

  #add the additional variables needed
  emspells <- clahrcnwlhf::create_new_vars(df = df, from.vignette,
                                           ward_hf_types, ward_sites,
                                           imd_lookup)
  emspells <- clahrcnwlhf::make_flag_variables(df = emspells)
  #emspells$CSPAdmissionTime <- as.POSIXct(emspells$CSPAdmissionTime)
  #emspells$CSPDischargeTime <- as.POSIXct(emspells$CSPDischargeTime)
  #emspells$EpisodeEndTime <- NULL
  #emspells$EpisodeStartTime <- NULL

  cat_agg <- emspells %>%
    group_by(DischargeMonth, WardSite, Ward_hf_type) %>%
    summarize(count = NROW(PseudoID), bundle = max(bundle),
              heartfailure = sum(Heart.Failure.Episode),
              hf_anycode = sum(HF.any.code),
              female = sum(female),
              hf_bundle = sum(bundle),
              white = sum(white),
              black = sum(black),
              asian = sum(asian),
              otherethnicity = sum(other.ethnicity),
              younger60 = sum(younger.60),
              deaths = sum(died),
              readmission_7dd = sum(readm.7),
              readmission_30dd = sum(readm.30),
              readmission_90dd = sum(readm.90),
              hfreadmission_7days = sum(readm.hf.7),
              hfreadmission_30days = sum(readm.hf.30),
              hfreadmission_90days = sum(readm.hf.90),
              with_imd = sum(imd.available))

  cont_agg <- emspells %>%
    group_by(DischargeMonth, WardSite, Ward_hf_type) %>%
    summarize(mean_age = mean(Age.est, na.rm = TRUE), mean_los = mean(los, na.rm = TRUE), mean_imd = mean(IMD, na.rm = TRUE), bed_days = sum(los, na.rm = TRUE))

  its_w <- inner_join(cat_agg, cont_agg, by = c("DischargeMonth", "WardSite", "Ward_hf_type"))

  its_w <- add_proportion_columns(its_w)
  its_w
}



#' its_aggregate
#'
#' @param df emergency admissions episode data
#'
#' @return aggregated data for ITS analysis
#' @export
#'
its_aggregate <- function(df = clahrcnwlhf::emergency_adms) {

  emspells <- clahrcnwlhf::create_new_vars(df = df)
  #emspells$CSPAdmissionTime <- as.POSIXct(emspells$CSPAdmissionTime)
  #emspells$CSPDischargeTime <- as.POSIXct(emspells$CSPDischargeTime)
  #emspells$EpisodeEndTime <- NULL
  #emspells$EpisodeStartTime <- NULL

  cat_agg <- emspells %>%
    group_by(DischargeMonth, Heart.Failure.Episode, HF.any.code, Sex, WardSite, Ward_hf_type, bundle, nicor, EthnicGroupComp, AgeBand_B, died, all.cause.readmission.cat, hf.readmission.cat) %>%
    summarize(count = NROW(PseudoID))

  cont_agg <- emspells %>%
    group_by(DischargeMonth, Heart.Failure.Episode, HF.any.code, Sex, WardSite, Ward_hf_type, bundle, nicor, EthnicGroupComp, AgeBand_B, died, all.cause.readmission.cat, hf.readmission.cat) %>%
    summarize(avAge = mean(Age.est, na.rm = TRUE), avLOS = mean(los, na.rm = TRUE), avIMD = mean(IMD, na.rm = TRUE), bed.days = sum(los, na.rm = TRUE))

  inner_join(cat_agg, cont_agg, by = c("DischargeMonth", "Heart.Failure.Episode", "HF.any.code","Sex", "WardSite", "Ward_hf_type", "bundle", "nicor", "EthnicGroupComp", "AgeBand_B", "died", "all.cause.readmission.cat", "hf.readmission.cat"))

}


#' create_new_vars
#'
#' @param df emergency admissions dataframe
#'
#' @return df with additional variables for ITS added
#' @export
#'
create_new_vars <- function(df = clahrcnwlhf::emergency_adms, from.vignette = FALSE, ward_hf_types = NULL, ward_sites = NULL, imd_lookup = NULL) {

  # Convert date-times to POSIX ct to please tidyverse
  df$CSPAdmissionTime <- as.POSIXct(df$CSPAdmissionTime)
  df$CSPDischargeTime <- as.POSIXct(df$CSPDischargeTime)
  df$EpisodeEndTime <- as.POSIXct(df$EpisodeEndTime)
  df$EpisodeStartTime <- as.POSIXct(df$EpisodeStartTime)

  #Load ward classification tables
  if(!from.vignette) {
    ward_hf_types <- read.csv(file = "data-raw/wardhftypes.csv", stringsAsFactors = FALSE)
    ward_sites <- read.csv(file = "data-raw/wardsites.csv", stringsAsFactors = FALSE)
  }

  # Map discharge ward to ward site and hf_type
  df <- dplyr::left_join(df, ward_hf_types, by = c("CSPLastWard"="Ward"))
  df <- dplyr::left_join(df, ward_sites, by = c("CSPLastWard"="Ward"))

  # Remove duplicate links from nicor data
  # Note crude method of selecting between duplicate links - ok for now
  linked_nicor <- clahrcnwlhf::bundle_element_data_completeness(bundles = clahrcnwlhf::linked_nicor_data)
  linked_nicor <- clahrcnwlhf::remove_duplicate_bundle_links(linked_bundles = linked_nicor, date_col = "Date.of.Visit")

  # Add link to linked bundle and nicor record where these exist, and flag variable
  df <- dplyr::left_join(df, clahrcnwlhf::linked_bundle_data[,c("Bundle.Number","linked.spell")], by = c("spell_number"="linked.spell"))
  df <- dplyr::left_join(df, linked_nicor[,c("nicor.entry.id","linked.spell")], by = c("spell_number"="linked.spell"))
  df$bundle <- !is.na(df$Bundle.Number)
  df$nicor <- !is.na(df$nicor.entry.id)

  # Add alternative codings of ethnicity
  df <- clahrcnwlhf::recode_ethnicity(df)

  # Add IMD
  if(!from.vignette){imd_lookup <- read.csv(file = "data-raw/imd-indices.csv", stringsAsFactors = FALSE)}
  df <- dplyr::left_join(df, imd_lookup[,c("LSOA_Code","IMD")], by = c("LSOA"="LSOA_Code"))

  # Add estimated age
  df$Age.est <- sapply(strsplit(as.character(df$AgeBand) , "to") , function(i) mean(as.numeric(i))) + 0.5

  # Collapse age bands
  df$AgeBand_B <- forcats::fct_collapse(df$AgeBand,
                               young = c("0","1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64"),
                               old = c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 to 99", "100 to 114")
  )

  # In-hospital Mortality
  df$died <- df$DischargeMethodCode == 4

  # Add discharge Month
  df$AdmissionMonth <- format(df$AdmissionDate, format = '%Y-%m')
  df$DischargeMonth <- format(df$DischargeDate, format = '%Y-%m')

  # Restrict to only first episode of each spell
  df <- df[which(df$new_spell == TRUE),]

  # Remove duplicate spells
  df <- dplyr::distinct(df, PseudoID, CSPDischargeTime, .keep_all = TRUE)

  # Add readmission columns
  df <- create_readms(df)

  df

}


#' create_readms
#'
#' @param df dataframe of spell data
#'
#' @return df with readmission variable columns added
#' @export
#'
create_readms <- function(df) {

  # Create time to next spell column
  df <- lag.column(df, "AdmissionDate", nm="next.adm.dt", backlag = TRUE, sc="new.pat")
  df$time.to.next.spell <- difftime(df$next.adm.dt, df$DischargeDate, units = "days")

  # Create time to next HF spell column (only for HF spells)
  # TODO: Note this repeats code from new.pat function, refactor!
  hf <- df[which(df$Heart.Failure.Episode == TRUE),]

  hf <- consecutive.diff(hf, "PseudoID", "PseudoID","hf.new.pat.diff")
  hf$hf.new.pat <- sapply(hf$hf.new.pat.diff, function(x) {
    if(is.na(x)||!(x==0)){return(TRUE)}
    else {return(FALSE)}})

  hf <- lag.column(hf, "AdmissionDate", nm="next.hf.adm.dt", backlag = TRUE, sc="hf.new.pat")
  hf$time.to.next.hf.spell <- difftime(hf$next.hf.adm.dt, hf$DischargeDate, units = "days")

  df <- dplyr::left_join(df, hf[,c("spell_number","time.to.next.hf.spell")], by = "spell_number")

  # Create the categorical variables
  df$all.cause.readmission.cat <- cut(as.numeric(df$time.to.next.spell), breaks = c(0,7,30,90), labels = c("7","30","90"), right = FALSE)
  df$hf.readmission.cat <- cut(as.numeric(df$time.to.next.hf.spell), breaks = c(0,7,30,90), labels = c("7","30","90"), right = FALSE)

  df
}


#' make_flag_variables
#'
#' @param df emergency admissions dataframe, with additional variables added by create_new_vars
#'
#' @return df with additional flag variables needed for the ITS analysis
#' @export
#'
make_flag_variables <- function(df) {
  df$female <- (as.character(df$Sex) == 'F') %in% TRUE
  df$white <- (as.character(df$EthnicGroupComp) == 'W') %in% TRUE
  df$black <- (as.character(df$EthnicGroupComp) == 'U') %in% TRUE
  df$asian <- (as.character(df$EthnicGroupComp) == 'Y') %in% TRUE
  df$other.ethnicity <- (as.character(df$EthnicGroupComp) == 'Z' | as.character(df$EthnicGroupComp) == 'X')  %in% TRUE
  df$younger.60 <- (as.character(df$AgeBand_B) == 'young') %in% TRUE
  df$readm.7 <- (as.character(df$all.cause.readmission.cat) == '7') %in% TRUE
  df$readm.30 <- (as.character(df$all.cause.readmission.cat) == '30') %in% TRUE
  df$readm.90 <- (as.character(df$all.cause.readmission.cat) == '90') %in% TRUE
  df$readm.hf.7 <- (as.character(df$hf.readmission.cat) == '7') %in% TRUE
  df$readm.hf.30 <- (as.character(df$hf.readmission.cat) == '30') %in% TRUE
  df$readm.hf.90 <- (as.character(df$hf.readmission.cat) == '90') %in% TRUE
  df$imd.available <- !is.na(df$IMD)

  df
}


#' add_proportion_columns
#'
#' @param agg_df the wide-aggregated data for its analysis
#'
#' @return agg_df with variables expressed as proportions of count
#' @export
#'
add_proportion_columns <- function(agg_df) {

  denom <- 'count'
  denom2 <- 'heartfailure'
  nums <- c("heartfailure",
            "hf_anycode",
            "female",
            "hf_bundle",
            "white",
            "black",
            "asian",
            "otherethnicity",
            "younger60",
            "deaths",
            "readmission_7dd",
            "readmission_30dd",
            "readmission_90dd",
            "with_imd")
  nums2 <- c("hfreadmission_7days",
             "hfreadmission_30days",
             "hfreadmission_90days")
  for (x in nums) {
    agg_df <- column_as_proportion(agg_df, numerator_col = x, denominator_col = denom,
                                   prop_col_name = paste('percentage_',x,sep=''))
  }
  for (x in nums2) {
    agg_df <- column_as_proportion(agg_df, numerator_col = x, denominator_col = denom2,
                                   prop_col_name = paste('percentage_',x,sep=''))
  }

  agg_df

}


#' column_as_proportion
#'
#' @param df input dataframe
#' @param numerator_col column of df to form numerator of new proportion column
#' @param denominator_col column of df to form denominator of new proportion column
#' @param prop_col_name name for new proportion column
#'
#' @return df with new proportion column added
#' @export
#'
column_as_proportion <- function(df, numerator_col, denominator_col, prop_col_name) {
  df[,prop_col_name] <- df[,numerator_col] / df[,denominator_col]
  df
}


#' drop_unnecessary_cols
#'
#' @param df
#'
#' @return copy of df but with only those columns needed for ITS analysis
#' @export
#'
drop_unnecessary_cols <- function(df,
                                  drop.cols = c('AdmissionDate', 'DischargeDate', 'PrimaryDiagnosis',
                 'SecondaryDiagnosis1','SecondaryDiagnosis2',
                 'SecondaryDiagnosis3',
                 'SecondaryDiagnosis4',
                 'SecondaryDiagnosis5',
                 'SecondaryDiagnosis6',
                 'SecondaryDiagnosis7',
                 'SecondaryDiagnosis8',
                 'SecondaryDiagnosis9',
                 'EthnicGroup',
                 'EpisodeNumber',
                 'EpisodeStartDate',
                 'EpisodeStartTime',
                 'EpisodeEndDate',
                 'EpisodeEndTime',
                 'extract',
                 'NewPseudo2',
                 'OldPseudoID',
                 'new.pat',
                 'new_spell',
                 'episode_order')) {
  df %>% select(-one_of(drop.cols))
}


#' save_its_raw_data_csv
#'
#' Saves a copy of the raw data for ITS analysis to the data-out folder.
#' This dataset is the output from create_new_vars(), filtered to
#' discharges prior to August 2017, with only columns required for
#' the ITS analysis
#' @return returns the raw data for ITS analysis invisibly
#' @export
#'
#'
save_its_raw_data_csv <- function() {
  emergency_spells <- clahrcnwlhf::create_new_vars()
  emergency_spells_disch_pre_aug17 <- emergency_spells %>% filter(CSPDischargeTime <= as.Date("2017-07-31"))
  emergency_spells_reduced <- clahrcnwlhf::drop_unnecessary_cols(emergency_spells_disch_pre_aug17)

  # TODO: For use as part of the package this should be replaced with the package version
  current_git_commit <- stringr::str_sub(system("git rev-parse HEAD", intern=TRUE), 1, 8)
  time_stamp <- gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse="-"))
  # TODO: For use as part of the package this should be refactored to save outside package
  out_path <- paste("data-out/emergency_spells_reduced", current_git_commit, time_stamp, ".csv", sep = "_")
  readr::write_csv(emergency_spells_reduced, path = out_path)
}
