
#' disch_time_table
#'
#' Produces a table of number of rows of a dataframe df per time period of the
#' column DischargeDate
#'
#' @param df dataframe in question
#' @param split_by the time period to split by. Defaults to months.
#'
#' @return a vector of row counts for each time period
#' @export
#'
disch_time_table <- function(df, split_by = '%Y-%m') {
  df$splitby <- factor(format(df$DischargeDate, split_by))
  df_splt <- split.data.frame(df, df$splitby)

  m <- sapply(df_splt, nrow)

  m

}

#' make_diag_flag
#'
#' @param dataframe
#' @param colname
#' @param code_regex
#' @param new_colname
#'
#' @return dataframe with diagnosis flag column added
#' @export
#'
make_diag_flag <- function(dataframe, colname = "PrimaryDiagnosis", code_regex,
                           new_colname = "diag.flag") {

  #This function needs to be modified to accept a vector of codes
  #Could convert to regex within the function?
  #i. This is designated by any of the following ICD-10 codes: I11.0 Hypertensive
  #heart disease with (congestive) heart failure; I25.5 Ischaemic cardiomyopathy;
  #I42.0 Dilated cardiomyopathy; I42.9 Cardiomyopathy, unspecified; I50.0 Congestive
  #heart failure; I50.1 Left ventricular failure; I50.9 Heart failure, unspecified.


  dataframe[,new_colname] <- grepl(code_regex, dataframe[,colname], perl=TRUE)
  dataframe

}
