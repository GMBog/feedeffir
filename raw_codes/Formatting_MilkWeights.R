# Script to transform milk_weights.csv from DairyComp
# Written by Guillermo Martinez Boggio

library(readr)
library(reshape2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(lubridate)


#### MILK WEIGHTS ###############################################################

rm(list = ls()) # initialization

Trial_ID <- "FP700" ## Here you can change the Trial_ID !!!
file_name <- "FP700_MilkWeights_56to512.csv" ## Here you can change the file name !!!


# 1. Open the Excel or csv file
file_path <- paste0("~/GreenFeed_UW/Methane/", Trial_ID, "/Milk_weights/", file_name)

if (tolower(tools::file_ext(file_path)) == "csv") {
  MilkWeights <- read_csv(file_path)
} else if (tolower(tools::file_ext(file_path)) %in% c("xls", "xlsx")) {
  MilkWeights <- read_excel(file_path)
} else {
  stop("Unsupported file format.")
}

## Remove the last one/two rows that are NA and Total=N
MilkWeights <- MilkWeights[1:(nrow(MilkWeights) - 1), ]
nrow(MilkWeights)

# 2. Transpose the content from columns to rows using the cowID as reference column
data <- reshape2::melt(MilkWeights, id.vars = "ID")

# 3. Transform Y# as Date, AM_PM in numbers (PM=1 and AM=2), and add the trialID
date_sequence <- seq(Sys.Date() - 1, by = "-1 day", length.out = 7)

data <- data %>%
  separate(variable, into = c("Y", "AM_PM"), sep = "(?<=Y[1-7])(?=AM|PM)", remove = FALSE) %>%
  dplyr::mutate(
    Date = date_sequence[as.numeric(gsub("Y", "", Y))],
    Trial_ID = Trial_ID
  ) %>%
  # MilkNum = ifelse(AM_PM == "AM", 2, 1)) %>% #If you want to have numbers instead AM_PM remove '#' at the begging
  dplyr::select(Trial_ID, Date, AM_PM, ID, value)

# 4. Add the names for the columns

## Here you can choose the names that you prefer, but do not change the order of them
names(data) <- c("Trial_ID", "Date", "MilkNum", "Visible_ID", "MilkLbs")

# 5. Check cows with low MW and replace the Lbs for a (.)


# 6. Save milk weights file in Excel or without format
file_path <- paste0("~/Downloads/", Trial_ID, "_MilkWeights", month(max(data$Date)), day(max(data$Date)), "to", month(min(data$Date)), day(min(data$Date)))
write.table(data, file = file_path, row.names = F)




# Extra step to COMPILE all milk weight files

Trial_ID <- "FP700" ## Here you can change the Trial_ID !!!

# Set directory file
dir <- paste0("~/GreenFeed_UW/Methane/", Trial_ID, "/Milk_weights/MWfiles/")

## Initialize an empty data frame to store the merged data
data <- data.frame()

## List files in the directory
list <- list.files(path = dir, recursive = T, full.names = T)
list

### Read files in a loop
for (file in list) {
  temp <- read.csv(file, sep = "") # Give the option to open excel files!!!

  if (nrow(temp) == 0) {
    data <- temp
  } else {
    data <- bind_rows(data, temp)
  }
}
rm(temp)

# Remove duplicates dates and ids based on the way we pull down data from DairyComp
data <- data %>%
  dplyr::distinct()


# Remove outliers for milk weights per cow
## The formula use to define outliers is: I=[q0.25−1.5*IQR;q0.75+1.5*IQR]

data$MilkLbs[data$MilkLbs == 0] <- NA

data <- data %>%
  dplyr::group_by(Visible_ID) %>%
  dplyr::mutate(MilkLbs1 = ifelse(MilkLbs %in% boxplot.stats(MilkLbs)$out, NA, MilkLbs)) %>%
  dplyr::ungroup()


file_path <- paste0("~/Downloads/UW_", Trial_ID, "_MilkWeights", Sys.Date() - 1, ".xlsx")
write.xlsx(data, file_path)




#################################################################################


#### MILK COMPOSITION ###########################################################


# Extra step to join milk composition files

Trial_ID <- "RL01" ## Here you can change the Trial_ID !!!

# Set directory file
dir <- paste0("/Users/GuillermoMartinez/GreenFeed_UW/Methane/", Trial_ID, "/Milk_composition/MCOMPfiles/")

## Initialize an empty data frame to store the merged data
data <- data.frame()

## List files in the directory
list <- list.files(path = dir, recursive = T, full.names = T)
list

### Read files in a loop
for (file in list) {
  temp <- read_excel(file, col_types = c("numeric", "date", "text", rep("numeric", 7))) # Give the option to open excel files!!!

  if (nrow(temp) == 0) {
    data <- temp
  } else {
    data <- bind_rows(data, temp)
  }
}
rm(temp)

names(data) <- c("Sample", "Date", "Visible_ID", "MilkNum", "FatPct", "PrtPct", "LacPct", "SnF", "Cells", "Urea")

data$Date <- as.Date(data$Date)

file_path <- paste0("~/Downloads/UW_", Trial_ID, "_MilkComposition2024-05-09.xlsx")
write.xlsx(data[, 1:10], file_path)

#################################################################################
