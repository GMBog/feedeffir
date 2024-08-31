## Script to create the Wxcel with INFO for milk labels
## Written by Victoria WU

library(readxl)
library(dplyr)
library(openxlsx)

rm(list = ls()) # initialization

### IF YOU SET YOUR DIR HERE! THEN YOU DON'T NEED TO PUT ALL YPUR PATH WHEN OPEN FILE LIKE IN "file"
setwd("/Users/GuillermoMartinez/GreenFeed_UW/Methane/FP700")


## NOW IMAGINE THAT WE ONLY HAVE THE LIST OF COWS (COW ID)
## BUT WE NOW THAT THE STUDY WILL LAST 8 WEEKS AND WE NEED TUES, WED, THUR SAMPLING EVERY WEEK

## CREATE THE FILE THAT YOU ARE USING LATER



# import every sheet from the excel file
file <- "FP700 MilkComp ID.xlsx"
sheet_names <- excel_sheets(file)

weeks_data <- list()

for (sheet in sheet_names) {
  sheet_data <- read_excel(file, sheet = sheet, col_types = c("numeric", "date", "text", "text"))
  weeks_data[[sheet]] <- sheet_data
}

# set the function for modifying the sheet
modify <- function(df) {
  df %>%
    rename(NUM = Sample, ID = `Cow ID`, DATE = Date, TIME = `AM/PM`) %>%
    mutate(
      DATE = as.Date(DATE, format = "%Y-%m-%d"),
      DATE = format(DATE, "%m/%d/%Y")
    ) %>%
    mutate(STUDY = "FP700") %>%
    select(DATE, ID, TIME, STUDY, NUM, everything())
}

weeks_data <- lapply(weeks_data, modify)

# save the data into an excel
# Create a new workbook
wb <- createWorkbook()

# Loop through each element in the weeks_data list
for (week_name in names(weeks_data)) {
  addWorksheet(wb, week_name)
  writeData(wb, sheet = week_name, weeks_data[[week_name]])
}

# Save the workbook to a file
saveWorkbook(wb, "Milk_labels_FP700.xlsx", overwrite = TRUE)




## OLD SCRIPT ####################################################################

cow_ID <- read_excel("GreenFeed_UW/Methane/FP690/FP690_ID.xlsx",
  sheet = "Final List", col_types = c("numeric", "text", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric")
)

create_dates <- function(start_date, end_date) {
  # Create a sequence of dates
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  # Identify the middle date
  middle_index <- length(dates) %/% 2 + 1
  middle_date <- dates[middle_index]

  # Repeat the middle date
  dates <- c(dates, rep(middle_date, 1))

  # Sort the dates
  dates <- sort(dates)

  return(dates)
}

i <- 7
start_date <- c("2023-09-26", "2023-10-03", "2023-10-10", "2023-10-17", "2023-10-24", "2023-10-31", "2023-11-07")
end_date <- c("2023-09-28", "2023-10-05", "2023-10-12", "2023-10-19", "2023-10-26", "2023-11-02", "2023-11-09")
dates <- create_dates(start_date[i], end_date[i])
times <- rep(c("PM", "AM", "PM", "AM"), length(cow_ID$ID))

data <- data.frame(
  Date = dates,
  Cow_ID = rep(cow_ID$ID, each = 4),
  AM_PM = times
)

order_indices <- order(data$Date, data$AM_PM, data$Cow_ID)
data <- data[order_indices, ]

write.csv(data, file = "~/Downloads/data.csv", row.names = F)

#################################################################################
