#' @name process_intakes
#' @title Process Feed and Pellet Intakes
#'
#' @description Process feed intakes and pellet inatkes. Creates a dataset
#'     with daily dry matter inatke for each animal across the study.
#'     The DMI includes individual feed and pellet intakes from RIC and GreenFeed system.
#'
#' @param file1 a character string representing the path to the Excel file containing feed intake data. The file should have columns:
#'     'Visible_ID' (cow identifier), 'Date' (date of intake), 'FedKg' (amount of feed in kilograms)
#' @param file2 a character string representing the path to the CSV file containing pellet intake data. The file should have columns:
#'     'FarmName' (cow identifier), 'Date' (date of intake), 'PIntake_kg' (amount of pellets in kilograms)
#' @param rfid_tbl a data frame with rfid and experiment information
#' @param start_date a date object representing the start date for the analysis (format 'mm/dd/yyyy')
#' @param end_date a date object representing the end date for the analysis (format 'mm/dd/yyyy')
#' @param iDM a numeric vector representing the dry matter percentage for each week of the study. This should be as long as the number of weeks in the study.
#' @param pDM a numeric value representing the dry matter percentage of the pellet intake (default is 0.95)
#'
#' @return A data frame with the following columns:
#'     \item{Visible_ID}{Cow identifier.}
#'     \item{week}{Week number relative to the start date.}
#'     \item{n}{Number of intake records in that week.}
#'     \item{DMI}{Average dry matter intake (DMI) for the week.}
#'
#' @examples
#' @export process_intakes
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import readr
#' @import readxl
utils::globalVariables(c("DM", "PIntake_kg", "iDMI", "pDMI", "DMI_total"))

process_intakes <- function(file1, file2, rfid_tbl, start_date, end_date, iDM, pDM = 0.95) {
  Feed_Intakes <- readxl::read_excel(file1, col_types = c("text", "date", "text", "numeric"))
  Pellet_Intakes <- readr::read_csv(file2, col_types = cols(FarmName = col_character(), RFID = col_character()))

  # Add treatment levels present in RFID file
  Feed_Intakes <- Feed_Intakes %>%
    dplyr::inner_join(rfid_tbl, by = c("Visible_ID" = "FarmName")) %>%
    # Filter out cows with records before the start_date
    dplyr::filter(Date >= start_date) %>%
    dplyr::distinct()

  # Calculate DMI for feed intakes
  Feed_Intakes <- Feed_Intakes %>%
    dplyr::mutate(
      week = floor(as.numeric(difftime(as.Date(Date), start_date), units = "weeks")) + 1,
      DM = ifelse(week <= length(iDM), iDM[week], NA_real_),
      iDMI = ifelse(FedKg > 0, FedKg * DM, 0)
    ) # Avoid negative intakes

  head(Feed_Intakes)
  table(Feed_Intakes$Date, Feed_Intakes$week)

  # Calculate DMI for pellet intakes
  Pellet_Intakes <- Pellet_Intakes %>%
    dplyr::mutate(
      DM = ifelse(!is.na(PIntake_kg), pDM, 0),
      pDMI = PIntake_kg * DM
    )

  head(Pellet_Intakes)

  # Sum feed and pellet intakes
  Daily_intakes <- Feed_Intakes %>%
    dplyr::inner_join(Pellet_Intakes, by = c("Visible_ID" = "FarmName", "Date")) %>%
    dplyr::mutate(
      DMI_total = iDMI + pDMI,
      DMI_total = ifelse(DMI_total == 0, NA, DMI_total)
    )

  # Generate weekly intakes for each cow
  Dry_matter_intakes <- Daily_intakes %>%
    dplyr::mutate(week = floor(as.numeric(difftime(as.Date(Date), start_date), units = "weeks")) + 1) %>%
    dplyr::group_by(Visible_ID, week) %>%
    dplyr::summarise(
      n = n(),
      Avg_DMI = ifelse(all(is.na(DMI_total)), 0, mean(DMI_total, na.rm = TRUE))
    ) %>%
    dplyr::mutate(across(everything(), ~ replace(., is.nan(.), NA)))

  names(Dry_matter_intakes) <- c("Visible_ID", "week", "n", "DMI")

  return(Dry_matter_intakes)
}
