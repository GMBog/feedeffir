#' @name calculate_milke
#' @title Calculate Milk Energy
#'
#' @description
#' This function processes milk weights and composition data to calculate individual milk energy.
#' The calculation uses coefficients for fat, protein, and lactose to estimate milk energy.
#'
#' @param file1 a character string representing the path to the Excel file containing milk weights
#' @param file2 a character string representing the path to the Excel file containing milk composition
#' @param start_date a character string representing the start date for the analysis (format 'mm/dd/yyyy')
#' @param coeff1 a numeric value representing the coefficient for fat (default is 9.29)
#' @param coeff2 a numeric value representing the coefficient for protein (default is 5.85)
#' @param coeff3 a numeric value representing the coefficient for lactose (default is 3.95)
#'
#' @return A data frame with daily milk energy
#'
#' @examples
#' @export calculate_milke
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import readxl
utils::globalVariables(c("MilkNum", "MilkLbs", "week", "FatLbs", "ProtLbs", "LactLbs", "FatKg", "ProtKg", "LactKg", "milkE"))

calculate_milke <- function(file1, file2, start_date,
                            coeff1 = 9.29, coeff2 = 5.85, coeff3 = 3.95) {
  MilkWeights <- readxl::read_excel(file1, col_types = c("text", "text", "text", "text", "numeric"))

  MilkWeights <- MilkWeights %>%
    dplyr::select(Visible_ID = Visible_ID, Date = Date, MilkNum, MilkLbs) %>%
    dplyr::mutate(
      Date = as.Date(Date),
      MilkLbs = ifelse(MilkLbs == 0, NA, MilkLbs),
      MilkKg = round(MilkLbs / 2.205, 2)
    )

  MilkComposition <- read_excel(file2, col_types = c("text", "date", "text", "text", rep("numeric", 6)))

  ## Join milk weights and composition
  Milk_energy <- MilkWeights %>%
    dplyr::inner_join(MilkComposition[, 2:10], by = c("MilkNum", "Date", "Visible_ID"))

  # Transform components in % to kg and lbs
  Milk_energy$FatLbs <- Milk_energy$MilkLbs * (Milk_energy$FatPct / 100)
  Milk_energy$ProtLbs <- Milk_energy$MilkLbs * (Milk_energy$PrtPct / 100)
  Milk_energy$LactLbs <- Milk_energy$MilkLbs * (Milk_energy$LacPct / 100)

  Milk_energy$FatKg <- Milk_energy$MilkKg * (Milk_energy$FatPct / 100)
  Milk_energy$ProtKg <- Milk_energy$MilkKg * (Milk_energy$PrtPct / 100)
  Milk_energy$LactKg <- Milk_energy$MilkKg * (Milk_energy$LacPct / 100)

  ### Remove data before Experimental start date
  Milk_energy <- Milk_energy %>%
    dplyr::filter(Date >= start_date)

  # Calculate daily milk components per cow
  Daily_milkE <- Milk_energy %>%
    dplyr::mutate(week = floor(as.numeric(difftime(as.Date(Date), start_date), units = "weeks")) + 1) %>%
    dplyr::group_by(Visible_ID, week, MilkNum) %>%
    # Calculate components in kg
    dplyr::summarise(
      MilkLbs = sum(MilkLbs),
      FatLbs = sum(FatLbs),
      ProtLbs = sum(ProtLbs),
      LactLbs = sum(LactLbs),
      FatKg = sum(FatKg),
      ProtKg = sum(ProtKg),
      LactKg = sum(LactKg)
    ) %>%
    # Calculate milk energy based on the following formula:
    dplyr::mutate(milkE = (coeff1 * FatKg) + (coeff2 * ProtKg) + (coeff3 * LactKg)) %>%
    # Do the average of milk energy per week
    dplyr::group_by(Visible_ID, week) %>%
    dplyr::summarise(
      n = n(),
      MilkLbs = mean(MilkLbs),
      FatLbs = mean(FatLbs),
      ProtLbs = mean(ProtLbs),
      LactLbs = mean(LactLbs),
      milkE = mean(milkE)
    )


  return(Daily_milkE)
}
