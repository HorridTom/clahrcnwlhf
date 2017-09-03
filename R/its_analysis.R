
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

  # Add link to linked bundle and nicor record where these exist, and flag variable
  df <- dplyr::left_join(df, clahrcnwlhf::linked_bundle_data[,c("Bundle.Number","linked.spell")], by = c("spell_number"="linked.spell"))
  df <- dplyr::left_join(df, clahrcnwlhf::linked_nicor_data[,c("nicor.entry.id","linked.spell")], by = c("spell_number"="linked.spell"))
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
