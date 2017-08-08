
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
  d_cols_list <- c("Admission.Date.Bundle", "BNP.Date", "Date.referred", "Date.completed")
  date_columns <- colnames(df) %in% d_cols_list
  df[,date_columns] <- lapply(df[,date_columns], as.Date, format = "%d/%m/%Y")

  #TODO: Sort out Date.of.last.echo column - approximate dates

  #TODO: Update the helper functions convert_to_date and convert_to_datetime to deal with:
  # i) NAs in columns
  # ii) different date formats - some functionality already but not sufficient?
  # iii) WRITE TESTS FOR THIS!

  df$Admission.Datetime <- convert_to_datetime(df$Admission.Date.Bundle, df$Admission.Time.Bundle, dt_form = "%Y-%m-%d %H:%M")

  #Add column summarizing completeness of data for each bundle
  df <- bundle_element_data_completeness(bundles = df)

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


#' bundle_element_data_completeness
#'
#' @param bundles the bundle data to be assessed for completeness
#'
#' @return bundles with additional column showing number of NAs
#' @export
#'
bundle_element_data_completeness <- function(bundles = clahrcnwlhf::bundle_data_clean) {

  bundles$number.nas <- rowSums(is.na(bundles))
  bundles

}
