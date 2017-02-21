#' plot_age_dist
#'
#' @param df data frame containing the data to be plotted
#'
#' @return plot of age distribution
#' @export
#'
plot_age_dist <- function(df) {

  plot(df$AgeBand)

}

#ACTUAL ANALYSIS (Trying out commands) - STARTING WITH PLOTS. The function plot is limiting so I am now using barplot as this is more flexible and enables me to do alot more.

# 1. Plot Ageband of HF where primary diagnosis is HF, I use teh function below
plot(EA$AgeBand[EA$Heart.Failure.Episode=='TRUE'], ylab = 'Frequency', main = 'Barplot of Emergency_Adms for all patients with Heart failure as the primary Diagnosis')

# 2. To then switch to barplot, first I start with a table so:
AgeBand <- table (EA$AgeBand)

#then to view it simply type 'AgeBand'. Plot barplot of AgeBand, sorting it by 'order'
barplot(AgeBand[order(AgeBand, decreasing = TRUE)])

#customise the plot. plot horrizontol with decreasing (teh decreasing refers to the bars not the actual data)
barplot(AgeBand[order(AgeBand, decreasing = TRUE)], horiz = TRUE, las = 1)

#plot boxplot of Episode Number by Sex (you do this by the over '~' command)
boxplot(EA$EpisodeNumber~EA$Sex, ylab = 'Episode Number', main = 'Boxplot distribution of Episode Number by Sex')
