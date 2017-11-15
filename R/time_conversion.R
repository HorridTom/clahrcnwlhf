#' convert_time_cols
#'
#' @param df dataframe
#' @param lt_to_ct specify which way to convert
#'
#' @return df with times converted
#'
#' @export
convert_time_cols <- function(df, lt_to_ct = TRUE) {

  if (lt_to_ct) {
    convert_cols <- sapply(emergency_adms, is.POSIXlt)
    df[,convert_cols] <- as.data.frame(lapply(df[,convert_cols], as.POSIXct))
  } else {
    convert_cols <- sapply(emergency_adms, is.POSIXct)
    df[,convert_cols] <- as.data.frame(lapply(df[,convert_cols], as.POSIXlt))
  }

  df

}


#' check_idempotency_of_convert_time_cols
#'
#' @param df dataframe
#' @param lt_to_lt specify starting point of conversion
#'
#' @return logical indicating whether idempotent
#' @export
#'
check_idempotency_of_convert_time_cols <- function(df, lt_to_lt = TRUE) {

  if(lt_to_lt) {
    df1 <- convert_time_cols(df, lt_to_ct = TRUE)
    df2 <- convert_time_cols(df, lt_to_ct = FALSE)
    result <- all.equal(df2, df1)
  } else {
    df1 <- convert_time_cols(df, lt_to_ct = FALSE)
    df2 <- convert_time_cols(df, lt_to_ct = TRUE)
    result <- all.equal(df2, df1)
  }

  result
}


is.POSIXct <- function(x) inherits(x, "POSIXct")
is.POSIXlt <- function(x) inherits(x, "POSIXlt")
is.POSIXt <- function(x) inherits(x, "POSIXt")
is.Date <- function(x) inherits(x, "Date")
