#' clean_data
#'
#' This function cleans the raw data by performing the following steps:
#' \enumerate{
#'   \item Remove leading and trailing whitespace from any character fields
#'   \item Replace any instances of NULL or #N/A in character fields with NA
#'   \item Convert the following columns to factors: AgeBand, EthnicGroup, Sex,
#'   AdmissionMethodCode, AdmissionType, CSPLastWard
#'   \item Convert the following columns from character to Date format:
#'   the following columns of the raw data:
#'   AdmissionDate, DischargeDate, EpisodeStartDate, EpisodeEndDate}
#'
#'
#' @param df the data frame containing the episode data to be cleaned
#'
#' @return A data frame containing the cleaned data
#' @export
#'
clean_data <-function(df, restrict_disch_date = TRUE) {

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

  # Convert the following columns to datetime format:
  dt_cols_list <- list()
  dt_cols_list[[1]] <- c("AdmissionDate","CSPAdmissionTime")
  dt_cols_list[[2]] <- c("DischargeDate","CSPDischargeTime")
  dt_cols_list[[3]] <- c("EpisodeStartDate","EpisodeStartTime")
  dt_cols_list[[4]] <- c("EpisodeEndDate","EpisodeEndTime")
  t_cols_list <- sapply(dt_cols_list, function(x) {x[2]})
  time_columns <- colnames(df) %in% t_cols_list

  # First pad all the time columns with zeros
  df[,time_columns] <- lapply(dt_cols_list, function(x){
    stringr::str_pad(df[[x[2]]], 4, pad = "0")
  })

  # Convert to datetimes
  df[,time_columns] <- lapply(dt_cols_list, function(x){
    convert_to_datetime(dv = df[[x[1]]], tv = df[[x[2]]])
  })

  if (restrict_disch_date) {
    df <- clahrcnwlhf::subset_by_date(df,
                                      start_date=as.Date("2012-01-01"),
                                      end_date=as.Date("2016-09-30"))
  }

  df <- clahrcnwlhf::remove_dupes(df)

  df

}

#' clean_and_save
#'
#' Runs clean_data on admission_data and saves the result as
#' admission_data_clean, overwriting any previous version.
#'
#'
#' @return NULL
#' @export
#'
#'
clean_and_save <- function() {
  admission_data_clean <- clean_data(clahrcnwlhf::admission_data,
                                      restrict_disch_date = TRUE)
  devtools::use_data(admission_data_clean, overwrite = TRUE)
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

#' convert_to_datetime
#'
#' @param dv a vector of dates or characters representing dates
#' @param tv a vector of times or characters representing times
#' @param dt_form the concatenated format of dv and tv
#'
#' @return A POSIXlt vector combining dv and tv.
#' @export
#'
#'
convert_to_datetime <- function(dv, tv, dt_form = "%Y-%m-%d %H%M") {
  dt_char <- paste(dv, tv)
  strptime(dt_char, dt_form)
}

#' subset_by_date
#'
#' @param df the dataframe to be subsetted
#' @param col_name the date column to be subsetted on
#' @param start_date the start date for the subsetting
#' @param end_date the end date for the subsetting
#'
#' @return a row subset of df, including all rows where col_name is between
#' start_date and end_date (inclusive).
#' @export
#'
#'
subset_by_date <- function(df, col_name = "DischargeDate", start_date, end_date) {
  df[which(df[,col_name] >= start_date
                             & df[,col_name] <= end_date),]
}


#' missing_data_table
#'
#' @param df the admissions data frame to be analysed for missing data
#'
#' @return a matrix showing missing data for each field by year
#' @export
#'
#'
missing_data_table <- function(df, split_by = '%Y', result = 'both') {
  df$splitby <- factor(format(df$DischargeDate, split_by))
  adm_data_y <- split.data.frame(df, df$splitby)
  m <- do.call(rbind, sapply(adm_data_y, clahrcnwlhf::na_count))
  n <- sapply(adm_data_y, nrow)
  rownames(m) <- gsub("\\..*$", "", rownames(m))
  colnames(m) <- colnames(df)
  p <- m/n
  m_table <- do.call(rbind,list(m,colSums(m)))
  rownames(m_table)[length(rownames(m_table))] <- "Total"
  n[length(n)+1] <- sum(n)
  names(n)[length(n)] <- "Total"
  p_table <- m_table/n
  md_table<-matrix(paste(m_table," (",round(100*p_table,1),"%)",sep=""),
                   nrow=nrow(m_table), dimnames=dimnames(m_table))
  switch(result, both = md_table, count_no_total = m, count = m_table,
         percent_no_total = p, percent = p_table)
}

#' remove_dupes
#'
#' @param df an episode dataframe to be de-duped
#'
#' @return the de-duped df
#' @export
#'
remove_dupes <- function(df, dupe_cols = c("PseudoID","AdmissionDate",
                                           "CSPAdmissionTime","DischargeDate",
                                           "CSPDischargeTime",
                                           "PrimaryDiagnosis",
                                           "SecondaryDiagnosis1",
                                           "SecondaryDiagnosis2",
                                           "EpisodeNumber")) {
  df <- df[!duplicated(df[dupe_cols]),]
  df
}
