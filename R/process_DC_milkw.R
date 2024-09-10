#' @name process_DC_milkw
#' @title Process and Format Dairy Comp Milk Weights
#'
#' @description Compiles and formats milk weight files from Dairy Comp system.
#'     It supports CSV and Excel files, transposes the data, and calculates the appropriate date range for each row.
#'     Allows filtering of outliers based on milk weight values and saves the formatted milk weights to a file.
#'
#' @param exp a character string representing the name of the experiment
#' @param file_path a character string representing the file path to Excel or CSV file downloaded from DC
#' @param output_dir a character string representing the directory path where the output files should be saved
#' @param late a numeric value representing the number of days late. By default 1
#' @param rm.out A logical flag (default FALSE). If TRUE, outliers based on the standard deviation of milk weights are removed.
#'
#' @return Dataset and a table with milk weights from Dairy Comp system
#' @examples
#' # How to process the milk weights from DC?
#' # First pull down the milk weights from DC, and save as an Excel file
#'
#' file_path <- system.file("extdata", "MilkWeight_DCfile.xls", package = "feedeffir")
#'
#' # Next use the following function:
#' data <- process_DC_milkw(exp = "Test",
#'                          file_path = file_path,
#'                          output_dir = getwd(),
#'                          rm.out = FALSE)
#'
#' on.exit(unlink(output_dir, recursive = TRUE))
#'
#' @export process_DC_milkw
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import lubridate
#' @import readr
#' @import readxl
utils::globalVariables(c("AM_PM", "ID", "Trial_ID", "value", "variable", "Y"))

# Function to format milk weights from Dairy Comp system
process_DC_milkw <- function(exp = NA, file_path, output_dir, late = 1, rm.out = FALSE) {
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
  message("Time range:\n", paste(date_sequence, collapse = "\n"))

  data <- data %>%
    separate(variable, into = c("Y", "AM_PM"), sep = "(?<=Y[1-7])(?=AM|PM)", remove = FALSE) %>%
    dplyr::mutate(
      Date = date_sequence[as.numeric(gsub("Y", "", Y))],
      Trial_ID = exp
    ) %>%
    dplyr::select(Trial_ID, Date, AM_PM, ID, value)


  # 4. Add the names for the columns
  names(data) <- c("Trial_ID", "Date", "MilkNum", "Visible_ID", "MilkLbs")

  # 5. Remove outliers based on standard deviation
  if (rm.out) {
    mean_value <- mean(data$MilkLbs, na.rm = TRUE)
    sd_value <- stats::sd(data$MilkLbs, na.rm = TRUE)

    initial_count <- nrow(data)  # Number of rows before filtering

    data <- data %>%
      dplyr::filter(MilkLbs >= mean_value - 3 * sd_value & MilkLbs <= mean_value + 3 * sd_value)

    final_count <- nrow(data)  # Number of rows after filtering

    message("Number of outlier records removed: ", initial_count - final_count)
  }

  ## Print summary of MilkLbs
  print(summary(data$MilkLbs))

  # 6. Save milk weights file in Excel or without format
  file_path <- paste0(output_dir, exp, "_MilkWeights",
                      lubridate::month(max(data$Date)),
                      lubridate::day(max(data$Date)), "to",
                      lubridate::month(min(data$Date)),
                      lubridate::day(min(data$Date)))
  write.table(data, file = file_path, row.names = F)

  return(data)
}
