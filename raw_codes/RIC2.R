## SCRIPT TO PROCESSING INTAKE FILES
## Written by Guillermo Martinez Boggio

# INPUT: VR file with intakes
# OUTPUT: Excel file with raw and processed data

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)
library(openxlsx)
library(writexl)

rm(list = ls()) # initialization

TrialID <- "HMW706" # Change here for the current study!!!


# Open the intake file including the path where the file is saved
file_path <- paste0("~/Downloads/", TrialID, "/VR240626.DAT")
VR <- read_csv(file_path, col_names = FALSE, col_types = cols(X1 = col_character())) %>%
  dplyr::select(c(-12:-16)) %>%
  dplyr::mutate(X11 = X10)

## Extract Date from the filename
Date <- as.Date(str_match(file_path, "VR(\\d{6})\\.DAT$")[2], format = "%y%m%d")

# Bins -> Include here the sequence of bin number needed, e.g., if you need intakes from bin 12 to 20, just change "Bins <- seq(12,20)".
Bins <- seq(1, 32)
VR <- VR[VR$X3 %in% Bins, ]


# Step 1: List the high negative values in the file

high_negative_intakes <- VR[VR$X10 <= -5, ]


# Step 2: Remove negative intakes

cows_negative_intakes <- VR[VR$X10 < 0, ] # Cows to remove from the file
names(cows_negative_intakes) <- c("Transponder", "Cow", "Bin", "Time_Start", "Time_End", "Sec", "Kg_Start", "Kg_End", "Feed", "As_Fed", "As_Fed_Corr")

VR <- VR[VR$X10 >= 0, ] # Retain in our data set "VR" only intakes greater or equal than 0


# Step 3: Order data set VR based on col "animalID" and "Start_time"
VR <- VR[order(VR$X2, VR$X4), ]


# Step 4: Save the raw data in a file called "Raw_data"

Raw_data <- VR %>%
  dplyr::mutate(Date = Date, Type = "FEED") %>%
  dplyr::relocate(Date, Type, .before = X1)
names(Raw_data) <- c("Date", "Type", "Transponder", "Cow", "Bin", "Time_Start", "Time_End", "Sec", "Kg_Start", "Kg_End", "Feed", "As_Fed", "As_Fed_Corr")


# Step 5: Identify and save all duplicates records in a file called "Duplicates"

## The principle in this step is checking the difference in seconds between the start and end time for the actual row with the previous one for the same animal and different bin
## Duplicate is considered when a animal is in two bins at the same time!!!

Duplicates <- VR %>%
  dplyr::filter((lead(X2) == X2 & lead(X3) != X3 & as.numeric(lead(X4) - X5, units = "secs") < 0) |
    (X2 == lag(X2) & X3 != lag(X3) & as.numeric(X4 - lag(X5), units = "secs") < 0))
names(Duplicates) <- c("Transponder", "Cow", "Bin", "Time_Start", "Time_End", "Sec", "Kg_Start", "Kg_End", "Feed", "As_Fed", "As_Fed_Corr")


# Step 6: Get the maximum and minimum start time

## Change format time of columns Start_time (X4) and End_time (X5)
VR$X4 <- format(VR$X4, "%H:%M:%S")
VR$X5 <- format(VR$X5, "%H:%M:%S")

## Get the latest and earliest times and save it in two files
Latest_times <- data.frame(Date, Total = max(VR$X4))
Earliest_times <- data.frame(Date, Total = min(VR$X5))


# Step 7: Create a file with the final data

Final_data <- VR %>%
  dplyr::group_by(Visible_ID = X2) %>%
  dplyr::summarise(Trial_ID = first(TrialID), Date = first(Date), Feed = first(X9), FedKg = sum(X10)) %>%
  dplyr::select(Trial_ID, Date, Visible_ID, Feed, FedKg) %>%
  dplyr::mutate(Visible_ID = as.numeric(Visible_ID), Obs = NA)



# Step 8: Get the top10 and bottom10 cows with Fed in Kg

Top10 <- head(Final_data[order(Final_data$FedKg, decreasing = T), ], 10)
Bottom10 <- head(Final_data[order(Final_data$FedKg), ], 10)


# Step 9: Plot intakes by cow by Diet

ggplot(Final_data, aes(x = as.character(Visible_ID), y = FedKg, fill = Feed)) +
  geom_col(width = 0.3) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.05, size = 8, family = "Times New Roman"),
    axis.text.y = element_text(hjust = 1.05, size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman", face = "bold"),
    axis.title.y = element_text(size = 12, family = "Times New Roman", face = "bold"),
    legend.position = "right",
  ) +
  labs(title = "", x = "Visible ID", y = "Fed (Kg)")


# Step 10: IF the study has different Diets, in this step you can get the FedKg per Diet per cow

## A. Get the average per Diet
Diets <- VR %>%
  dplyr::group_by(Diet = X9) %>%
  dplyr::summarise(FedKg = round(sum(X10), 0))


## B. Get the % of each Diet that each cow have been eaten
Diets_per_cow <- VR %>%
  dplyr::group_by(Visible_ID = X2) %>%
  dplyr::mutate(TotalDiet = sum(X10)) %>%
  dplyr::group_by(Visible_ID, Diet = X9) %>%
  dplyr::summarise(Percentage = round(sum(X10) / first(TotalDiet) * 100, 1)) %>%
  tidyr::pivot_wider(names_from = Diet, values_from = Percentage) %>%
  mutate_at(vars(-Visible_ID), ~ ifelse(. == 0, NA, .))


# Step 11: Compiling daily intakes

# Set directory file (where you save the Excel files)
dir <- paste0("~/Downloads/", TrialID, "/Intake_files/")

# Include here the name of your compiled file
file_name <- paste0("UW_", TrialID, "_CompiledIntakes.xlsx")

list <- list.files(path = dir, recursive = T, full.names = T)
list

# Merge the compiled data with daily data
if (file_name %in% basename(list)) {
  # If the file exists, read the Excel file
  temp <- read_excel(paste0(dir, file_name))
} else {
  # If the file does not exist, create an empty data frame
  temp <- data.frame()
}

data <- bind_rows(temp, Final_data)

# Remove overlapping records
data <- data %>%
  dplyr::distinct()

write_xlsx(data, path = file_name)



# Step 12: Create a Excel file with all the tables create in this script

excel_file <- "Downloads/your_file.xlsx" # Specify the file path for your Excel file

# Create an Excel workbook
write_xlsx(
  list(
    Raw_Data = Raw_data,
    Latest_times = Latest_times,
    Earliest_times = Earliest_times,
    Duplicates = Duplicates,
    Animals_removed = cows_negative_intakes,
    Top10 = Top10,
    Bottom10 = Bottom10,
    Multiple_Diets = Diets,
    Diets_Fedcows = Diets_per_cow,
    Final_Data = Final_data
  ),
  path = excel_file
)
