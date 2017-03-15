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

# 1. Basic descriptive of the data (I.e. statistics and frequencies)
summary(EA)

# 1. Plot Ageband of HF where primary diagnosis is HF, I use the function below
plot(EA$AgeBand[EA$Heart.Failure.Episode=='TRUE'], ylab = 'Frequency', main = 'Barplot of Emergency_Adms for all patients with Heart failure as the primary Diagnosis')

# 2. To then switch to barplot, first I start with a table so:
AgeBand <- table (EA$AgeBand)

#then to view it simply type 'AgeBand'. Plot barplot of AgeBand, sorting it by 'order'
barplot(AgeBand[order(AgeBand, decreasing = TRUE)])

#customise the plot. plot horrizontol with decreasing (the decreasing refers to the bars not the actual data)
barplot(AgeBand[order(AgeBand, decreasing = TRUE)], horiz = TRUE, las = 1)

#plot boxplot of Episode Number by Sex (you do this by the over '~' command)
boxplot(EA$EpisodeNumber~EA$Sex, ylab = 'Episode Number', main = 'Boxplot distribution of Episode Number by Sex')

#categorical data: frequencies/crosstabs
table(EA$AgeBand) # for only one categorical variable
AgeBandSex <- table(EA$AgeBand, EA$Sex) # is for two-way tables
AgeBandSex # generate and visulaise table
addmargins(AgeBandSex) # Adds row/col margins

#ISSUES TO BE DISCUSSED WITH TOM - reorder a string. Reduce variable to be able to doa fisher's test

#Test-analysis to see if I can hypotehsis test categorical variable.
#if I take the example of heartfailure episode vs spell. This is a goood example as I have a two variable which is the only way I can conduct a fisher. Both are T, F so;
table(EA$Heart.Failure.Episode) #then
table(EA$new_spell) #and finally combine both
HearFailure_Spell <- table(EA$Heart.Failure.Episode,EA$new_spell) # to then do a Pearson Chi-square's simply run;
chisq.test(HearFailure_Spell)

#PLOTS - STACKED
#aSK TOM <- sTILL STRUGGLING WITH ORDERING AGEBAND

#DEFINING A VARIABLE
#e.g. lets deifine teh variable EA$Sex
EA$Sex <- factor(EA$Sex, levels = c("F", "M", "U", "NA's"), labels = c("female", "male", "unknown", "missing"))

# if we were then to plot this as a boxplot of e.g spell number by Sex , that would be
boxplot(EA$spell_number~EA$Sex, ylab = 'Spell_number', main = 'Boxplot distribution of Spell Number by Sex')

# 1. Plot Ageband of New Patient where New Patient is TRUE, I use the function below
plot(EA$AgeBand[EA$new.pat=='TRUE'], ylab = 'Frequency', main = 'Barplot of EA for all new patients where New Spell is TRUE')

#Subsetting
new_patinet <- subset(EA, new.pat == TRUE) #OR
new_patient_trial <- EA[EA$new.pat == TRUE, ] #return all columns

#Within the new created subset called new_patient, I created a dataframe of only new.patients and then to plot ageband by those with true HF episode code
plot(new_patinet$AgeBand[new_patinet$Heart.Failure.Episode=='TRUE'], ylab = 'Frequency', main = 'Barplot of Emergency_Adms for all patients with Heart failure as the primary Diagnosis')

#BASIC DESCRIPTIVE - eye-balling the variables
describe() #generates n, frequency and proportion . For a more detailed version,
stat.desc() #generates
