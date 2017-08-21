
#' clean_nicor_data
#'
#' @param df the raw nicor data
#'
#' @return the cleaned nicor data
#' @export
#'
clean_nicor_data <- function(df) {
  # Add entry ID column
  df$nicor.entry.id <- 1:nrow(df)

  # Remove leading and trailing whitespace from any character fields
  char_cols <- sapply(df, is.character)
  df[,char_cols] <- lapply(df[,char_cols], function(x){
    trimws(x, which="both")})

  # Convert any empty values to NA
  df <- as.data.frame(lapply(df, FUN = function(x) {sapply(x, function(x) gsub("^$", NA, x))}), stringsAsFactors = FALSE)

  # Use first three chars of site name only
  df$Hospital <- substr(df$Hospital,1,3)

  # Convert the following columns to factors:
  # NB there are many more columns that should be converted, if they are to be used
  # for now, only listing the first few + any likely to be needed (YAGNI)
  f_cols_list <- c("Hospital","Patient.sex","Main.Place.of.Care","Specialist.input",
                   "Breathlessness", "Peripheral.Oedema","Death.in.hospital","Confirmed.diagnosis.HF")
  factor_columns <- colnames(df) %in% f_cols_list
  df <- make_factors(df, cols = factor_columns)

  # Convert the following columns to date format:
  d_cols_list <- c("Date.of.Visit", "Date.of.discharge", "Date.of.review.appointment", "Date.Created")
  date_columns <- colnames(df) %in% d_cols_list
  df[,date_columns] <- lapply(df[,date_columns], as.Date, format = "%d/%m/%Y")

  #TODO: Update the helper functions convert_to_date and convert_to_datetime to deal with:
  # i) NAs in columns
  # ii) different date formats - some functionality already but not sufficient?
  # iii) WRITE TESTS FOR THIS!

  #Remove duplicates
  dupe_entries <- duplicated(df[,!(colnames(df) %in% c("Date.Created","Readmission.Dataset"))])
  df <- df[!dupe_entries,]

  df
}

#' nicor_clean_and_save
#'
#' @return
#' @export
#'
nicor_clean_and_save <- function() {
  nicor_data_clean <- clean_nicor_data(clahrcnwlhf::nicor_data)
  devtools::use_data(nicor_data_clean, overwrite = TRUE)

}
