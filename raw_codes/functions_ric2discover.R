################################################################################
## RIC2Discover Visit Report (VR) Data Processing Functions
## Authored by Dr. Ryan Selz Pralle, University of Wisconsin - Platteville
## Version 1.2, 07/17/2024

# This code is based on a MS Excel Macro authored by Dr. Lou Armentano,
# professor emeritus at UW - Madison. It was later adapted to R by Dr. Guillermo
# Martinez Boggio, during his Post Doctorate with Dr. Francisco Penagaricano at
# UW - Madison. The present code is a significant upgrade that allows file
# importing, filtering, and writing in batches. Work is ongoing to implement
# functions for routine quality control on a daily or weekly basis. In the
# interim, we recommend Dr. Boggio's public repository:
# https://github.com/GMBog/GreenFeed-and-FeedIntakes


##########
# PACKAGES: Required

require(dplyr)
require(tidyverse)


##########
# FUNCTION: Read a batch of VR files

read.VR <- function(x = Files) {
  # This function is intended to run as a subroutine of other functions.
  # Object 'x' is a list of VR file names in a directory. Functions vary in their
  # construction of this list, so that they are more efficient.

  # Dependent packages
  require(dplyr)
  require(tidyverse)

  # Basic function to read VR file (CSV) named 'x' and preprocess
  df <- read_csv(x, col_names = FALSE, col_types = cols(X1 = col_character())) %>% dplyr::select(c(-11:-16))
  colnames(df) <- c("RFID", "CowID", "BinID", "Time_Start", "Time_End", "Sec", "Kg_Start", "Kg_End", "Feed", "Intake_kg_asfed")
  return(df)
}


##########
# FUNCTION: Merge and filter a batch of VR files

RIC2 <- function(Dir_VR = NA, Date_Start = NA, Date_End = Sys.Date(), Cows = NA,
                 Bins = NA, Min_Intake = 0, Min_Length = 0, Dir_Write = NA,
                 writeFiles = F) {
  # This is the primary function for importing, filtering, and merging batches of
  # VR data. It is not intended for regular monitoring of cows or equipment
  # functionality.

  # It is recommended that you do not execute this function while the folder
  # containing VR files is the working directory. That helps avoid accidental
  # writing of CSV output files to the VR folder.

  #' Dir_VR' = The directory containing VR files.
  #' Date_Start' = First file date to include in merged data sets. Default includes all.
  #' Date_End' = Last files date to include in merged data sets. Default includes all.
  #' Cows' = List of cow IDs to include in merged data sets. Default includes all.
  #' Bins' = List of bin IDs to include in merged data sets. Default includes all.
  #' Min_Intake' = Minimum visit intake (kg) to be included in the filtered and daily summary data sets.
  #' Min_Length' = Minimum visit length (sec) to be included in the filtered and daily summary data sets.
  #' Dir_Write' = The directory to write output CSV files.
  #' writeFiles' = Boolean to determine whether output CSV should be written.

  # Dependent packages
  require(dplyr)
  require(tidyverse)

  # Change to VR file directory
  WD <- getwd()
  if (!is.na(Dir_VR)) {
    setwd(dir = Dir_VR)
  }

  # Identify VR files in the directory
  # Any non-VR files in the directory will make this fail
  Files <- list.files(".", full.names = T, recursive = T)
  Dates <- as.Date(str_match(Files, "VR(\\d{6})\\.DAT$")[, 2], format = "%y%m%d")

  # Filter 'Files' object by dates, only need Date_Start
  # This is more efficient than pulling all data in and then filtering
  if (!is.na(Date_Start)) {
    Date_Start <- as.Date(Date_Start)
    Date_End <- as.Date(Date_End)
    Files <- Files[which(Dates >= Date_Start & Dates <= Date_End)]
  }

  # Read VR files in directory
  dat_raw <- map_df(Files, ~ read.VR(.x), .id = "VR_order")
  dat_raw$Julian_Date <- as.Date(NA)
  for (i in 1:length(Files)) {
    dat_raw[which(dat_raw$VR_order == i), "Julian_Date"] <- as.Date(Dates[i])
  }
  dat_raw <- dat_raw[, -1] %>% select(Julian_Date, everything())

  # Change back to original directory
  setwd(dir = WD)

  # Order data by Julian Date, CowID, and Time_Start
  dat_raw <- dat_raw[order(dat_raw$Julian_Date, dat_raw$CowID, dat_raw$Time_Start), ]

  # Initialize data for filtering
  dat <- dat_raw

  # Filter by cow list
  if (!is.na(Cows[[1]])) {
    dat <- dat[dat$CowID %in% Cows, ]
  }

  # Filter by bin list
  if (!is.na(Bins[[1]])) {
    dat <- dat[dat$BinID %in% Bins, ]
  }

  # Filter by minimum feed intake, requires a value, default is 0
  dat <- dat[dat$Intake_kg_asfed >= Min_Intake, ]

  # Filter by minimum visit length, requires a value, default is 0
  dat <- dat[dat$Intake_kg_asfed >= Min_Length, ]

  # Identify duplicates, animals visiting two bins at once
  duplicates <- dat_raw %>% dplyr::filter((lead(BinID) == BinID &
    lead(Time_Start) != Time_Start &
    as.numeric(lead(Time_End) - Sec, units = "secs") < 0) |
    (BinID == lag(BinID) &
      Time_Start != lag(Time_Start) &
      as.numeric(Time_End - lag(Sec), units = "secs") < 0))

  # Summarize data into daily values
  dat_daily <- dat %>%
    group_by(Julian_Date, CowID, Feed) %>%
    summarize(Intake_kg_asfed = sum(Intake_kg_asfed), visit_count = sum(!is.na(Sec)), visit_time_sec = sum(Sec))
  dat_filtered <- dat

  # Write output data frames as CSV files
  if (!is.na(Dir_Write)) {
    setwd(dir = Dir_Write)
  }
  if (writeFiles == T) {
    write.csv(dat_daily, file = "ric2_daily.csv", row.names = F, quote = F)
    write.csv(dat_filtered, file = "ric2_visits_filtered.csv", row.names = F, quote = F)
    write.csv(dat_raw, file = "ric2_visits_raw.csv", row.names = F, quote = F)
  }
  setwd(dir = WD)

  # Return final object, a list of data frames
  #' RIC2_daily' = summary of VR data by day after applying filters
  #' VR_filtered' = VR data after applying filters
  #' VR_raw' = VR data prior to filters, except Time_Start and Time_End
  x <- list("RIC2_daily" = dat_daily, "VR_filtered" = dat_filtered, "VR_raw" = dat_raw)
  return(x)
}


##########
# FUNCTION: Generate quality control data sets for cows and bins on a day

RIC2.qc.day <- function(Dir_VR = NA, Day = NA, Cows = NA, Bins = NA,
                        Min_Intake = 0, Min_Length = 0, cowLow = 30,
                        negIntThr = -1, visitShort = 10, visitLong = 3600,
                        Dir_Write = NA, writeFiles = F) {
  # This function producse data sets that can be used for within day quality
  # control analyses. It is intended for regular monitoring of cows and
  # equipment functionality.

  # It is recommended that you do not execute this function while the folder
  # containing VR is the working directory. That helps avoid accidental writing
  # of CSV output files to the VR folder.

  #' Dir_VR' = The directory containing the VR file.
  #' Day' = Date VR file of interest was generated, defaults to the most recent date in folder.
  #' Cows' = List of cow IDs to include in day summary. Default includes all.
  #' Bins' = List of bin IDs to include in day summary. Default includes all.
  #' Min_Intake' = Minimum visit intake (kg) to be included in day summary.
  #' Min_Length' = Minimum visit length (sec) to be included in day summary.
  #' cowLow' = Daily intake (kg) alarm for a cow.
  #' negIntThr' = Intake alarm for a visit, usually an exceptionally negative value.
  #' visitShort' = Visit length (sec) alarm for an abnormally short visit.
  #' visitLong' = Visit length (sec) alarm for an abnormally long visit.
  #' Dir_Write' = The directory to write output CSV files.
  #' writeFiles' = Boolean to determine whether output CSV should be written.

  # Dependent packages
  require(dplyr)
  require(tidyverse)

  # Change to VR file directory
  WD <- getwd()
  if (!is.na(Dir_VR)) {
    setwd(dir = Dir_VR)
  }

  # Identify VR files in the directory
  # Any non-VR files in the directory will make this fail
  Files <- list.files(".", full.names = T, recursive = T)
  Dates <- as.Date(str_match(Files, "VR(\\d{6})\\.DAT$")[, 2], format = "%y%m%d")

  # Select VR file, defaults to most recent file
  if (!is.na(Day)) {
    Day <- as.Date(Day)
    Files <- Files[which(Dates == Day)]
  } else {
    Day <- Dates[order(Dates, decreasing = T)][[1]]
    Files <- Files[which(Dates == Day)]
  }

  # Read VR file from directory
  dat_raw <- read.VR(x = Files)
  dat_raw$Julian_Date <- Day
  dat_raw <- dat_raw %>% select(Julian_Date, everything())

  # Change back to original directory
  setwd(dir = WD)

  # Order data by CowID, and Time_Start
  dat_raw <- dat_raw[order(dat_raw$CowID, dat_raw$Time_Start), ]

  # Generate alarm data sets
  negIntakes <- dat_raw[which(dat_raw$Intake_kg_asfed < 0), ]
  negIntakes_thresh <- dat_raw[which(dat_raw$Intake_kg_asfed < negIntThr), ]
  shortVisits <- dat_raw[which(dat_raw$Sec < visitShort), ]
  longVisits <- dat_raw[which(dat_raw$Sec > visitLong), ]
  duplicates <- dat_raw %>% dplyr::filter((lead(BinID) == BinID & lead(Time_Start) != Time_Start & as.numeric(lead(Time_End) - Sec, units = "secs") < 0) | (BinID == lag(BinID) & Time_Start != lag(Time_Start) & as.numeric(Time_End - lag(Sec), units = "secs") < 0))

  # Initialize data for filtering
  dat <- dat_raw

  # Filter by CowID list
  if (!is.na(Cows[[1]])) {
    dat <- dat[dat$CowID %in% Cows, ]
  }

  # Filter by BinID list
  if (!is.na(Bins[[1]])) {
    dat <- dat[dat$BinID %in% Bins, ]
  }

  # Filter by minimum feed intake, requires a value, default is 0
  dat <- dat[dat$Intake_kg_asfed >= Min_Intake, ]

  # Filter by minimum visit length, requires a value, default is 0
  dat <- dat[dat$Intake_kg_asfed >= Min_Length, ]

  # Summarize filtered data within day
  dat_day_cow <- dat %>%
    group_by(CowID, Feed) %>%
    summarize(Intake_kg_asfed = sum(Intake_kg_asfed), visit_count = sum(!is.na(Sec)), visit_time_sec = sum(Sec))
  dat_day_bin <- dat %>%
    group_by(BinID, Feed) %>%
    summarize(Intake_kg_asfed = sum(Intake_kg_asfed), visit_count = sum(!is.na(Sec)), visit_time_sec = sum(Sec))
  dat_filtered <- dat

  # Write output data frames as CSV files
  if (!is.na(Dir_Write)) {
    setwd(dir = Dir_Write)
  }
  if (writeFiles == T) {
    Day2 <- gsub("-", "", Day)
    write.csv(dat_day_cow, file = paste0("ric2_qc_day_cow_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(dat_day_bin, file = paste0("ric2_qc_day_bin_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(dat_filtered, file = paste0("ric2_qc_day_filtered_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(dat_raw, file = paste0("ric2_visits_raw_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(negIntakes, file = paste0("negIntakes_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(negIntakes_thresh, file = paste0("negIntakes_thresh_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(shortVisits, file = paste0("shortVisits_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(longVisits, file = paste0("longVisits_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(duplicates, file = paste0("duplicates_", Day2, ".csv"), row.names = F, quote = F)
  }
  setwd(dir = WD)

  # Return final object, a list of data frames
  #' RIC2_day_cow' = summary of VR data over the day by cow after applying filters
  #' RIC2_day_bin' = summary of VR data over the day by bin after applying filters
  #' VR_filtered' = VR data after applying filters
  #' VR_raw' = VR data prior to filters
  #' negIntakes' = visits with negative feed intakes
  #' negIntakes_thresh' = visits with negative feed intakes below threshold
  #' shortVisits' = visits with a length below threshold
  #' longVisits' = visits with a lengath above threshold
  #' duplicates' = duplicate visits, a cow(s) in two bins at once
  x <- list(
    "RIC2_day_cow" = dat_day_cow, "RIC2_day_bin" = dat_day_bin,
    "VR_filtered" = dat_filtered, "VR_raw" = dat_raw,
    "negIntakes" = negIntakes, "negIntakes_thresh" = negIntakes_thresh,
    "shortVisits" = shortVisits, "longVisits" = longVisits,
    "duplicates" = duplicates
  )
  return(x)
}


##########
# FUNCTION: Generate quality control data sets for cows and bins over a week

RIC2.qc.wk <- function(Dir_VR = NA, Day = NA, Cows = NA, Bins = NA,
                       Min_Intake = 0, Min_Length = 0, cowLow = 30,
                       negIntThr = -1, visitShort = 10, visitLong = 3600,
                       Dir_Write = NA, writeFiles = F) {
  # This function produces data sets that can be used for a within week quality
  # control analyses. It is intended for regular monitoring of cows and
  # equipment functionality.

  # It is recommended that you do not execute this function while the folder
  # containing VR is the working directory. That helps avoid accidental writing
  # of CSV output files to the VR folder.

  #' Dir_VR' = The directory containing VR files.
  #' Day' = Reference date for importing VR files, we import the previous 7 day (not files) and defaults to the most recent date in folder.
  #' Cows' = List of cows to include in day summary. Default includes all.
  #' Bins' = List of bins to include in day summary. Default includes all.
  #' Min_Intake' = Minimum visit intake (kg) to be included in day summary.
  #' Min_Length' = Minimum visit length (sec) to be included in day summary.
  #' cowLow' = Daily intake (kg) alarm for a cow.
  #' negIntThr' = Intake alarm for a visit.
  #' visitShort' = Visit length (sec) alarm for a short visit.
  #' visitLong' = Visit length (sec) alarm for a long visit.
  #' Dir_Write' = The directory to write output CSV files.
  #' writeFiles' = Boolean to determine whether output CSV should be written.

  # Dependent packages
  require(dplyr)
  require(tidyverse)

  # Change to VR file directory
  WD <- getwd()
  if (!is.na(Dir_VR)) {
    setwd(dir = Dir_VR)
  }

  # Identify VR files in the directory
  # Any non-VR files in the directory will make this fail
  Files <- list.files(".", full.names = T, recursive = T)
  Dates <- as.Date(str_match(Files, "VR(\\d{6})\\.DAT$")[, 2], format = "%y%m%d")

  # Select VR files, defaults to most recent file for backdating
  if (!is.na(Day)) {
    Day <- as.Date(Day)
    BackDate <- Day - 7
    Files <- Files[which(Dates <= Day & Dates >= BackDate)]
    Dates <- Dates[which(Dates <= Day & Dates >= BackDate)]
  } else {
    Day <- Dates[order(Dates, decreasing = T)][[1]]
    BackDate <- Day - 7
    Files <- Files[which(Dates <= Day & Dates >= BackDate)]
    Dates <- Dates[which(Dates <= Day & Dates >= BackDate)]
  }

  # Read VR files from directory
  dat_raw <- map_df(Files, ~ read.VR(.x), .id = "VR_order")
  dat_raw$Julian_Date <- as.Date(NA)
  for (i in 1:length(Files)) {
    dat_raw[which(dat_raw$VR_order == i), "Julian_Date"] <- as.Date(Dates[i])
  }
  dat_raw <- dat_raw[, -1] %>% select(Julian_Date, everything())

  # Change back to original directory
  setwd(dir = WD)

  # Order data by Julian_Date, CowID, and Time_Start
  dat_raw <- dat_raw[order(dat_raw$Julian_Date, dat_raw$CowID, dat_raw$Time_Start), ]

  # Generate alarm data sets
  negIntakes <- dat_raw[which(dat_raw$Intake_kg_asfed < 0), ]
  negIntakes_thresh <- dat_raw[which(dat_raw$Intake_kg_asfed < negIntThr), ]
  shortVisits <- dat_raw[which(dat_raw$Sec < visitShort), ]
  longVisits <- dat_raw[which(dat_raw$Sec > visitLong), ]
  duplicates <- dat_raw %>% dplyr::filter((lead(BinID) == BinID & lead(Time_Start) != Time_Start & as.numeric(lead(Time_End) - Sec, units = "secs") < 0) | (BinID == lag(BinID) & Time_Start != lag(Time_Start) & as.numeric(Time_End - lag(Sec), units = "secs") < 0))

  # Initialize data for filtering
  dat <- dat_raw

  # Filter by Cows
  if (!is.na(Cows[[1]])) {
    dat <- dat[dat$CowID %in% Cows, ]
  }

  # Filter by Bins
  if (!is.na(Bins[[1]])) {
    dat <- dat[dat$BinID %in% Bins, ]
  }

  # Filter by minimum feed intake, requires a value
  dat <- dat[dat$Intake_kg_asfed >= Min_Intake, ]

  # Filter by minimum visit length, requires a value
  dat <- dat[dat$Intake_kg_asfed >= Min_Length, ]

  # Summarize filtered data
  dat_daily_cow <- dat %>%
    group_by(Julian_Date, CowID, Feed) %>%
    summarize(Intake_kg_asfed = sum(Intake_kg_asfed), visit_count = sum(!is.na(Sec)), visit_time_sec = sum(Sec))
  dat_daily_bin <- dat %>%
    group_by(Julian_Date, BinID, Feed) %>%
    summarize(Intake_kg_asfed = sum(Intake_kg_asfed), visit_count = sum(!is.na(Sec)), visit_time_sec = sum(Sec))
  dat_filtered <- dat

  # Write output data frames as CSV files
  if (!is.na(Dir_Write)) {
    setwd(dir = Dir_Write)
  }
  if (writeFiles == T) {
    Day2 <- gsub("-", "", Day)
    write.csv(dat_daily_cow, file = paste0("ric2_qc_wk_cow", Day2, ".csv"), row.names = F, quote = F)
    write.csv(dat_daily_bin, file = paste0("ric2_qc_wk_bin", Day2, ".csv"), row.names = F, quote = F)
    write.csv(dat_filtered, file = paste0("ric2_qc_wk_filtered_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(dat_raw, file = paste0("ric2_visits_raw_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(negIntakes, file = paste0("negIntakes_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(negIntakes_thresh, file = paste0("negIntakes_thresh_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(shortVisits, file = paste0("shortVisits_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(longVisits, file = paste0("longVisits_", Day2, ".csv"), row.names = F, quote = F)
    write.csv(duplicates, file = paste0("duplicates_", Day2, ".csv"), row.names = F, quote = F)
  }
  setwd(dir = WD)

  # Return final object, a list of data frames
  #' RIC2_daily_wk_cow' = summary of VR data over the week by cow after applying filters
  #' RIC2_daily_wk_bin' = summary of VR data over the week by bin after applying filters
  #' VR_filtered' = VR data after applying filters
  #' VR_raw' = VR data prior to filters
  #' negIntakes' = visits with negative feed intakes
  #' negIntakes_thresh' = visits with negative feed intakes below threshold
  #' shortVisits' = visits with a length below threshold
  #' longVisits' = visits with a length above threshold
  #' duplicates' = duplicate visits, a cow(s) in two bins at once
  x <- list(
    "RIC2_daily_wk_cow" = dat_daily_cow,
    "RIC2_daily_wk_bin" = dat_daily_bin,
    "VR_filtered_wk" = dat_filtered,
    "VR_raw_wk" = dat_raw, "negIntakes" = negIntakes,
    "negIntakes_thresh_wk" = negIntakes_thresh,
    "shortVisits_wk" = shortVisits, "longVisits_wk" = longVisits,
    "duplicates_wk" = duplicates
  )
  return(x)
}


################################################################################
################################################################################
