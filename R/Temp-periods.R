#' make_period_col
#'
#' @param df data frame to which we want to add a period column
#' @param colname column in df containing all the dates on which the period established
#' @param split_dates vector of strings representing the date cut-offs of the periods.
#' Note the first entry must a date before the earliest date in colname
#' and the last date must be a date after the final date in colname
#' @param period_labels labels for periods split by split_dates
#' @param new_colname name of new column created containing the period into which the date falls
#'
#' @return df with an extra column of periods
#' @export
#'
make_period_col <- function(df, colname, split_dates, period_labels, new_colname) {
  bins<- as.Date(split_dates)
  df[,new_colname] <- cut(df[,colname], breaks = bins, labels = period_labels)
  df
}
