##### Old code to format milk weights from AFI ##################################

library(readr)
library(reshape2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(lubridate)


rm(list = ls()) # initialization

Trial_ID <- "RL01" ## Here you can change the Trial_ID !!!


## Step 1: Merge individual files from each cow

# Set directory file
dir <- paste0("~/GreenFeed_UW/Methane/Studies/", Trial_ID, "/Milk_weights/MWfiles/files")

## Initialize an empty data frame to store the merged data
data <- data.frame()

## List files in the directory
list <- list.files(path = dir, recursive = T, full.names = T)
list

# Read files in a loop
for (file in list) {
  # Extract animal ID from file name
  ID <- sub("^([0-9]+) - .*", "\\1", basename(file))

  # Read the file
  temp <- readr::read_table(file,
    col_types = cols(
      Days = col_skip(), Milk = col_number(),
      Cond. = col_skip(), AMT = col_skip(),
      X6 = col_skip()
    )
  )

  # Add animal ID column
  temp$Visible_ID <- ID
  temp <- temp[, c("Visible_ID", names(temp)[-which(names(temp) == "Visible_ID")])]


  # Combine dataframes
  if (nrow(temp) == 0) {
    data <- temp
  } else {
    data <- dplyr::bind_rows(data, temp)
  }
}

# Remove temporary variable
rm(temp)

# Save milk weights file in Excel or without format
write.table(data, file = paste0("~/Downloads/", Trial_ID, "_MilkWeights", Sys.Date()))


## Step 2: Merge MW files generated in the step 1 with those from previous weeks

# Before that remove all files in MWfiles!!!

Trial_ID <- "RL01" ## Here you can change the Trial_ID !!!

# Set directory file
dir <- paste0("~/GreenFeed_UW/Methane/Studies/", Trial_ID, "/Milk_weights/MWfiles/")

## Initialize an empty data frame to store the merged data
data <- data.frame()

## List files in the directory
list <- list.files(path = dir, recursive = T, full.names = T)
list

### Read files in a loop
for (file in list) {
  temp <- read.csv(file, sep = "")

  if (nrow(temp) == 0) {
    data <- temp
  } else {
    data <- bind_rows(temp, data)
  }
}
rm(temp)

data <- data %>%
  dplyr::arrange(Visible_ID, Date) %>% # Sort the data by Visible_ID and Date
  dplyr::group_by(Visible_ID, Date) %>% # Group by Visible_ID and Date
  dplyr::slice_tail(n = 1) %>% # Keep the last occurrence within each group
  dplyr::ungroup()

# Save milk weights file in Excel or without format
write.table(data, file = paste0("~/Downloads/UW_", Trial_ID, "_MilkWeights", Sys.Date()), row.names = F, quote = F)




#################################################################################
