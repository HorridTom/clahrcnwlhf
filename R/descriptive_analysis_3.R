
#' plot_age_dist
#'
#' @param df data frame containing the data to be plotted
#'
#' @return plot of age distribution. Flipped because of the age ranges
#' @export
#'
  plot_age_dist <- function(df) {

    #dataframe restricted to only rows where 'new.spell' = TRUE
    df <- df[which(df$new_spell == TRUE),]

    #age band in ascending order
    df$AgeBand <- factor(df$AgeBand,
                               levels = c('0', '1 to 4', '5 to 9', '10 to 14', '15 to 19', '20 to 24', '25 to 29', '30 to 34', '35 to 39', '40 to 44', '45 to 49', '50 to 54', '55 to 59', '60 to 64', '65 to 69', '70 to 74', '75 to 79', '80 to 84', '85 to 89', '90 to 94', '95 to 99', '100 to 104', '105 to 109', '106 to 109', '107 to 109', '108 to 109', '109 to 109'))

    #plot split into periods, overall is also an option with a few edits. facet-wrap = produces the two different periods, coord_flip = flips the graphs
    ggplot(df, aes(x=AgeBand))+geom_bar(width= 0.7)+facet_wrap( ~ period.date)+ggtitle("Distribution of age band across period A and B")+coord_flip()+ylab("Frequency")+xlab("Age Band")

}


#' Table of age distribution (categorical data: frequencies/crosstabs)
#'
#' @param df
#'
#' @return a (two-way) summary table of age distribution containing frequncies for each period and total sums for both periods
#' @export
#'
  plot_dist_table <- function(df, age_col = 'df$AgeBand', stratify = FALSE, strat_col){
    # df = dataframe
    #age_col = column containing age band variable
    #stratify = indicate whether table table is split into periods or overall
    #stract_col name of column to split age band by

    #age band distribution for both periods + sum of both
    ageband_perioddate <- addmargins(table(df$AgeBand, df$period.date),2)

    ageband_perioddate #this is what I run in the command line

  }


