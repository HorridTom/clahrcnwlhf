
#' plot_age_dist
#'
#'  @param df data frame containing the data to be plotted
#'
#'  @return plot of age distribution. Flipped because of the age ranges
#'  @export
#'
    plot_age_dist <- function(df) {

      #dataframe restricted to only rows where 'new.spell' = TRUE
    df <- restrict_to_spells(df)

      #plot split into periods, overall is also an option with a few edits. facet-wrap = produces the two different periods, coord_flip = flips the graphs
   ggplot2::ggplot(df, ggplot2::aes(x=AgeBand))+ggplot2::geom_bar(width= 0.7)+ggplot2::facet_wrap( ~ period.date)+ggplot2::ggtitle("Distribution of age band across period A and B")+ggplot2::coord_flip()+ggplot2::ylab("Frequency")+ggplot2::xlab("Age Band")

}


#' plot_dist_table
#'
#'  @param df dataframe
#'  @param age_col column containing age band variable
#'  @param stratify indicate whether table table is split into periods or overall
#'  @param strat_col name of column to split age band by
#'
#'  @return a (two-way) summary table of age distribution containing frequncies for each period and total sums for both periods
#'  @export
#'
    plot_dist_table <- function(df, age_col = 'AgeBand', stratify = FALSE, strat_col){

      #dataframe restricted to only rows where 'new.spell' = TRUE
    df <- restrict_to_spells (df)

      #age band distribution for both periods + sum of both
    ageband_perioddate <- addmargins(table(df$AgeBand, df$period.date),2)

    ageband_perioddate #this is what I run in the command line

  }


