

#Import intakes

library(readxl)
library(dplyr)

#Set directory file
dir <- "/Users/GuillermoMartinez/GreenFeed_UW/Methane/LFF682/FeedEfficiency_Data/Intake_DM/Intakes_covariate"

##Initialize an empty data frame to store the merged data
df <- data.frame()

##List files in the directory
list <- list.files(path = dir, recursive = T)
list

##Create a list of dates
dates <- seq(as.Date("2023-07-06"), as.Date("2023-07-28"), by = "day")

### Initialize count number of lines
count_row = 0

### Read files in a loop
p=1
for (file in list.files(path = dir, recursive = T)) {
  
  cat("File: ",file,"\n")
  
  tmp <- read_xls(file.path(dir, file), sheet = "Intake by cow by feed", skip = 3)
  tmp <- tmp[-nrow(tmp), ]
  
  tmp$date <- dates[p]
  
  tmp <- tmp[,!names(tmp) %in% c("Comments")]
  tmp_row <- nrow(tmp)
  
  cat("No. of rows: ",tmp_row,"\n")
  
  count_row = count_row + tmp_row
  
  cat("No. of total rows: ", count_row, "\n")
  
  if (nrow(df) == 0){
    df = tmp
  
  } else {
    df = bind_rows(df, tmp)
  }
  
  p = p + 1
}

Intakes <- df[,c(4,1,3)]

TrialID <- "LFF682"
Intakes <- data.frame(TrialID, Intakes)
names(Intakes) <- c("TrialID", "Date", "Visible_ID", "FedKg")

write.table(Intakes, file = "~/Downloads/Feed_intakes_covLFF682.txt", row.names = F)


