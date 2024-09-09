#' @name process_DC_milkw
#' @title Process and Format Dairy Comp Milk Weights
#'
#' @description Compiles and formats milk weight files from Dairy Comp system.
#'
#' @param exp a character string representing the name of the experiment
#' @param file_path a character string representing the file path to Excel or CSV file downloaded from DC
#' @param late a numeric value representing the number of days late. By default 1
#'
#' @return A table with the milk weights from Dairy Comp system
#' @examples
#' @export process_DC_milkw
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import lubridate
#' @import readr
#' @import readxl
utils::globalVariables(c("AM_PM", "ID", "Trial_ID", "value", "variable", "Y"))

# Function to format milk weights from Dairy Comp system
process_DC_milkw <- function(exp = NA, file_path, late = 1){

  # 1. Open the Excel or csv file
  if (tolower(tools::file_ext(file_path)) == "csv") {
    MilkWeights <- readr::read_csv(file_path)
    MilkWeights <- MilkWeights[1:(nrow(MilkWeights) - 1), ]
  } else if (tolower(tools::file_ext(file_path)) %in% c("xls", "xlsx")) {
    MilkWeights <- readxl::read_excel(file_path)
    MilkWeights <- MilkWeights[1:(nrow(MilkWeights) - 2), ]
  } else {
    stop("Unsupported file format.")
  }

  message("Number of cows in file: ", nrow(MilkWeights))

  # 2. Transpose the content from columns to rows using the cowID as reference column
  data <- reshape2::melt(MilkWeights, id.vars = "ID")

  # 3. Transform Y# as Date, AM_PM in numbers (PM=1 and AM=2), and add the trialID
  date_sequence <- seq(Sys.Date() - late, by = "-1 day", length.out = 7)
  message("Time range: ", print(date_sequence))

  data <- data %>%
    separate(variable, into = c("Y", "AM_PM"), sep = "(?<=Y[1-7])(?=AM|PM)", remove = FALSE) %>%
    dplyr::mutate(
      Date = date_sequence[as.numeric(gsub("Y", "", Y))],
      Trial_ID = exp
    ) %>%
    dplyr::select(Trial_ID, Date, AM_PM, ID, value)


  # 4. Add the names for the columns
  names(data) <- c("Trial_ID", "Date", "MilkNum", "Visible_ID", "MilkLbs")

  # 5. Save milk weights file in Excel or without format
  file_path <- paste0("~/Downloads/", exp, "_MilkWeights", lubridate::month(max(data$Date)), lubridate::day(max(data$Date)), "to", lubridate::month(min(data$Date)), lubridate::day(min(data$Date)))
  write.table(data, file = file_path, row.names = F)

}
