
#' clean_bundle_data
#'
#' @param df the raw bundle data
#'
#' @return the cleaned bundle data
#' @export
#'
clean_bundle_data <- function(df) {
  # Remove leading and trailing whitespace from any character fields
  char_cols <- sapply(df, is.character)
  df[,char_cols] <- lapply(df[,char_cols], function(x){
    trimws(x, which="both")})

  # Convert any empty values to NA
  df <- as.data.frame(lapply(df, FUN = function(x) {sapply(x, function(x) gsub("^$", NA, x))}), stringsAsFactors = FALSE)

  # Convert the following columns to factors:
  f_cols_list <- c("Ward","BNP.Level.Measured","Specialist.Referral.Made")
  factor_columns <- colnames(df) %in% f_cols_list
  df <- make_factors(df, cols = factor_columns)

  # Convert the following columns to date format:
  # TODO: The convert_to_date function currently does not work for dates
  # of format dd/mm/yyyy
  d_cols_list <- c("Admission.Date.Bundle")
  date_columns <- colnames(df) %in% d_cols_list
  df[,date_columns] <- as.Date(df[,date_columns], format = "%d/%m/%Y")
  df$Admission.Datetime <- convert_to_datetime(df$Admission.Date.Bundle, df$Admission.Time.Bundle, dt_form = "%Y-%m-%d %H:%M")

  df
}

#' bundle_clean_and_save
#'
#' @return
#' @export
#'
bundle_clean_and_save <- function() {
  bundle_data_clean <- clean_bundle_data(clahrcnwlhf::bundle_data)
  devtools::use_data(bundle_data_clean, overwrite = TRUE)

}
