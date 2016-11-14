#' clean_data
#'
#' This function cleans the raw data by performing the following steps:
#' \enumerate{
#'   \item Remove leading and trailing whitespace from any character fields
#'   \item Replace any instances of "NULL" or "#N/A" in character fields with NA
#'   \item Convert the following columns to factors: AgeBand, EthnicGroup, Sex,
#'   AdmissionMethodCode, AdmissionType, CSPLastWard}
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
