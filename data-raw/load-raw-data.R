# Heart Failure project analysis November 2016
# Step 0 - Recreate Raw Data - Version 0.1
# This script loads the individual component files of the raw dataset
# and stitches them together, saving the result as an .RData file

#' load_data_files
#'
#' load_data_files loads in a set of Excel files as dataframes
#'
#' @param fl list of paths of the files to be loaded
#'
#' @return A list of dataframes, one for each of the file paths in fl.
#' @export
load_data_files <- function(fl) {

  files <- lapply(fl, function(x) gdata::read.xls(x, stringsAsFactors = FALSE))
  files

}

#' merge_data_files
#'
#' Takes a list of dataframes with identical column names and types, and returns a
#' merged dataframe with that same column structure and all the data from the
#' original listed dataframes.
#'
#' @param frame_list list of dataframes to be merged into one
#'
#' @return A dataframe containing all the data from the original listed dataframes
#' @export
merge_data_files <- function(frame_list) {

  # Merge the dataframes passed into one
  do.call("rbind", frame_list)

}

# Load in separate files from NWLH data warehouse and join together.
fileNames <- Sys.glob("data-raw/split*.xlsx")
data_frames <- load_data_files(fileNames)
admission_data <- merge_data_files(data_frames)

# Save the result
devtools::use_data(admission_data)
