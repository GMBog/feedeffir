#' @name process_bw
#' @title Process Body Weights
#'
#' @description Process body weights files, and compute metabolic body weights and
#'     delta body weights using a linear model prediction for the entire period.
#'
#' @param file a character string representing the path to the Excel file containing body weights. The file should have columns:
#'     'Visible_ID' (cow identifier), 'Date', 'BW_lbs' (body weight in pounds).
#' @param rfid_tbl a data frame with rfid and experiment information
#' @param start_date a character string representing the start date for the analysis (format 'mm/dd/yyyy')
#' @param end_date a character string representing the end date for the analysis (format 'mm/dd/yyyy')
#'
#' @return A list containing two data frames:
#'     \item{metabolic_bw}{Data frame containing metabolic body weights summary.}
#'     \item{delta_bw}{Data frame containing delta body weights.}
#'
#' @examples
#' @export process_bw
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import grDevices
#' @import purrr
#' @import readxl
utils::globalVariables(c("week", "BW_lbs", "BW_kg", "mBW", "Average_mBW"))

process_bw <- function(file, start_date, end_date) {
  Body_weights <- readxl::read_excel(file, sheet = "BW_BCS", col_types = c("text", "date", "numeric", "numeric"))

  ## Check for outliers, if so put NA
  data <- Body_weights %>%
    dplyr::mutate(week = floor(as.numeric(difftime(as.Date(Date), start_date, units = "weeks"))) + 1) %>%
    dplyr::group_by(Visible_ID, week) %>%
    dplyr::mutate(
      BW_lbs = ifelse(BW_lbs %in% grDevices::boxplot.stats(BW_lbs)$out, NA, BW_lbs),
      BW_kg = ifelse(BW_kg %in% grDevices::boxplot.stats(BW_kg)$out, NA, BW_kg)
    )

  ## Transform body weights in kg and then calculate metabolic BW (mBW) with BW in kg
  data$mBW <- (data$BW_lbs / 2.205)^0.75

  head(data)
  table(data$Date, data$week)

  ## Calculate the average and SD of the mBW per cow
  metabolic_BW <- data %>%
    dplyr::group_by(Visible_ID, week) %>%
    dplyr::summarise(
      n = n(),
      Average_mBW = mean(mBW, na.rm = T),
      SD_mBW = stats::sd(mBW, na.rm = T),
      CV_mBW = (stats::sd(mBW, na.rm = T) / mean(mBW, na.rm = T))
    ) %>%
    dplyr::group_by(Visible_ID) %>%
    dplyr::summarise(
      mBW = mean(Average_mBW, na.rm = T),
      SD_mBW = stats::sd(Average_mBW, na.rm = T)
    )


  names(metabolic_BW) <- c("Visible_ID", "mBW", "SD_mBW")


  # Create a table with all dates in the study
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  # Exapand the grid to get all possible combinations of ID and dates
  data <- expand.grid(Visible_ID = unique(Body_weights$Visible_ID), Date = dates)

  # Match ID and Date to assign values
  data$Value <- Body_weights$BW_kg[match(
    paste(data$Visible_ID, as.character(data$Date)),
    paste(Body_weights$Visible_ID, Body_weights$Date)
  )]

  # Do the liner regression to obtain the predicted BW for each day
  data <- data[order(data$Visible_ID, data$Date), ]
  BWhat <- data %>%
    split(f = data$Visible_ID) %>%
    purrr::map(~ {
      lm_model <- lm(Value ~ Date, data = .x)
      predict(lm_model, newdata = .x)
    }) %>%
    purrr::map_df(~ as.data.frame(.x), .id = "CowID")

  names(BWhat)[2] <- "BWhat"
  data$BWhat <- BWhat$BWhat

  # Optionally plot for a specific Visible_ID (random)
  random_id <- sample(unique(data$Visible_ID), 1)
  plot(
    y = data$BWhat[data$Visible_ID == random_id],
    x = data$Date[data$Visible_ID == random_id],
    main = paste("BWhat for Visible_ID:", random_id),
    xlab = "Date",
    ylab = "Predicted BW"
  )

  ## Calculate weekly deltaBW for each cow
  delta_BW <- data %>%
    dplyr::mutate(week = floor(as.numeric(difftime(as.Date(Date), start_date, units = "weeks"))) + 1) %>%
    dplyr::group_by(Visible_ID, week) %>%
    dplyr::summarize(BWhat = last(BWhat) - first(BWhat)) %>%
    dplyr::group_by(Visible_ID) %>%
    dplyr::summarise(BWhat = mean(BWhat))

  # Rename columns
  names(delta_BW) <- c("Visible_ID", "deltaBW")

  # Return a list with two data frames: metabolic_bw and delta_bw
  return(list(metabolic_bw = metabolic_BW, delta_bw = delta_BW))
}
