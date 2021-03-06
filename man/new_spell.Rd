% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_analysis.R
\name{new_spell}
\alias{new_spell}
\title{new_spell}
\usage{
new_spell(df, id = "PseudoID", adt = "CSPAdmissionTime", npat = "new.pat",
  epno = "EpisodeNumber")
}
\arguments{
\item{df}{an episode dataframe}

\item{id}{the column name of the id field}

\item{adt}{the column name of the admission datetime field}

\item{npat}{the column name of the new patient flag field}

\item{epno}{the episode number in the spell}
}
\value{
a dataframe which is a copy of df with a new column
new_spell which is TRUE if the row represents an
episode that is the first for that patient and that admission datetime, and
FALSE if the row above represents an episode for
the same patient and same admission datetime.
}
\description{
new_spell
}

