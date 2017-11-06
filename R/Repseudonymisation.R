#' Repseuso
#'
#' @return
#' @export
#'
repseudo <- function() {
  ps_mapping <- readxl::read_excel('data-raw/Pseudo_to_Pseudo_20171102_v_1_1.xlsx')

  View(ps_mapping)
  table(ps_mapping$WasMistake)

  admission_data_2 <- dplyr::left_join(admission_data, dplyr::select(ps_mapping, PseudoID, NewPseudo))
  admission_data_2$NewPseudo2 <- NA
  #admission_data_2[which(is.na(admission_data_2$NewPseudo)),"NewPseudo2"] <- admission_data_2[which(is.na(admission_data_2$PseudoID)),"NewPseudo2"]
  admission_data_2[which(is.na(admission_data_2$NewPseudo)),"NewPseudo2"] <- admission_data_2[which(is.na(admission_data_2$NewPseudo)),"PseudoID"]
  admission_data_2[which(!is.na(admission_data_2$NewPseudo)),"NewPseudo2"] <- admission_data_2[which(!is.na(admission_data_2$NewPseudo)),"NewPseudo"]
  table(is.na(admission_data_2$NewPseudo2))
  admission_data_2$OldPseudoID <- admission_data_2$PseudoID
  admission_data_2$PseudoID <- admission_data_2$NewPseudo2
  admission_data_2$NewPseudo <- NULL
  rm(admission_data)
  admission_data <- admission_data_2
  rm(admission_data_2)
  save(admission_data, file = 'data/admission_data.rda')
  devtools::load_all(".")
}
