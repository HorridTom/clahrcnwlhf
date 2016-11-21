#' clean_data
#'
#' This function cleans the raw data by performing the following steps:
#' \enumerate{
#'   \item Remove leading and trailing whitespace from any character fields
#'   \item Replace any instances of "NULL" or "#N/A" in character fields with NA
#'   \item Convert the following columns to factors: AgeBand, EthnicGroup, Sex,
#'   AdmissionMethodCode, AdmissionType, CSPLastWard}
#'   \item Convert the following columns from character to Date format:
#'   the following columns of the raw data:
#'   AdmissionDate, DischargeDate, EpisodeStartDate, EpisodeEndDate
#'
#'
#' @param df the data frame containing the episode data to be cleaned
#'
#' @return A data frame containing the cleaned data
#' @export
#'
clean_data <-function(df) {

  # Remove leading and trailing whitespace from any character fields
  char_cols <- sapply(df, is.character)
  df[,char_cols] <- lapply(df[,char_cols], function(x){
                                              trimws(x, which="both")})

  # Replace all instances of "NULL" and "#N/A" in character fields with NAs
  dfc <- df[,char_cols]
  dfc[dfc=="NULL"] <- NA
  dfc[dfc=="#N/A"] <- NA
  df[,char_cols] <- dfc

  # Convert the following columns to factors:
  f_cols_list <- c("AgeBand","EthnicGroup","Sex","AdmissionMethodCode",
                   "AdmissionType","CSPLastWard")
  factor_columns <- colnames(df) %in% f_cols_list
  df <- make_factors(df, cols = factor_columns)

  # Convert the following columns to date format:
  d_cols_list <- c("AdmissionDate","DischargeDate","EpisodeStartDate",
                   "EpisodeEndDate")
  date_columns <- colnames(df) %in% d_cols_list
  df[,date_columns] <- lapply(df[,date_columns], convert_to_date)

  df

}

#' make_factors
#'
#' Converts the specified columns of a dataframe to factors
#'
#' @param df the dataframe whose columns are to be converted
#' @param cols a logical vector with one element for each column
#'
#' @return A dataframe identical to df except that any columns whose
#' corresponding entry in cols is \code{TRUE} is converted to a factor
#' @export
#'
make_factors <- function(df, cols) {
  df[,cols] <- lapply(df[,cols], as.factor)
  df
}

#' na_count
#'
#' Display the number of NAs per column
#'
#' @param df the dataframe in question
#'
#' @return A dataframe showing the number of NA values in each column of df
#' @export
#'
na_count <- function(df) {
  data.frame(sapply(df, function(y) sum(length(which(is.na(y))))))
}

#' convert_to_date
#'
#' Convert a character vector containing dates as substrings into a vector of the
#' corresopding dates
#'
#' @param v character vector containing dates as substrings
#' @param match_reg regular expression specifying the format of the dates
#' contained within the substrings. Default is ^[0-9]{4}-[0-9]{2}-[0-9]{2}.
#' @param date_form the date format to be used for conversion.
#'
#' @return A date vector the same length as v
#' @export
#'
#'
convert_to_date <- function(v, match_reg = "^[0-9]{4}-[0-9]{2}-[0-9]{2}",
                            date_form = "%Y-%m-%d") {
# TODO: modify so that this function takes a date format as parameter, and
#       converts it to a regex internally.
  pos <- regexpr(match_reg, v, perl=TRUE)
  dt <- regmatches(v, pos)
  as.Date(dt, date_form)
}
