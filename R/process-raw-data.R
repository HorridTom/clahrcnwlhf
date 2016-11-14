# Heart Failure project analysis November 2016
# Step 0 - Recreate Raw Data - Version 0.1
# This script loads the individual component files of the raw dataset
# and stitches them together, saving the result as an .RData file

#R:\CLAHRC 2 PROJECT\Planning for CLAHRC 2\Cross cutting themes\PHII\Data\NWLH\Heart_Failure
setwd("R:/CLAHRC 2 PROJECT/Planning for CLAHRC 2/Cross cutting themes/PHII/Data/NWLH/Heart_Failure")
getwd()

#The gdata package enables R to read in Excel files. Note this requires a
#Perl installation - e.g. from http://www.activestate.com/activeperl
#require(gdata)

#The timezone should be Europe/London
Sys.timezone()

load_data_files <- function(path = getwd(), mask) {
  #Load all files in 'path' with filename matching regex 'mask'

  fl <- list.files(path = path, pattern = mask)
  files <- lapply(fl, function(x) read.xls(x))
  files

}

merge_data_files <- function(frame_list) {

  #Merge the dataframes passed into one
  do.call("rbind", frame_list)

}

#Stage 1: load in separate files from Stephen and join together.
data_frames <- load_data_files(mask = "^split.*xls.*$")
admission_data <- merge_data_files(data_frames)

#Save the result
save(admission_data,file="admission_data_RAW.RData")
#The csv is big, so don't save as csv at this point
#write.csv(admission_data, file="admission_data_RAW.csv")

#Use the following to load the result of stage 1
#load(file="admission_data_RAW.RData")
