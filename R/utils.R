#' @name process_files
#' @title Process Files
#'
#' @description Process CSV, Excel, or no extension files with specific format
#'    for milk composition and body weights files
#'
#' @param dir a character string representing the directory containing the file paths of the CSV, Excel, or no extension files to be processed
#' @param opt an integer representing the option to process Excel files from Rosy Lane (opt = 2)
#'
#' @return A data frame containing the combined data from all processed files.
#' @examples
#' @export process_files
#' @import dplyr
#' @import readr
#' @import readxl
#' @import reshape2
#' @import utils
process_files <- function(dir = NA, opt = 1) {
  # List all files in the directory
  file_list <- list.files(dir, full.names = TRUE)
  print(file_list)

  data <- data.frame() # Initialize an empty data frame

  for (file in file_list) {
    # Check the file extension
    ext <- tools::file_ext(file)

    if (ext == "csv") {
      # Process CSV files
      temp <- utils::read.csv(file, sep = ",")
    } else if (ext == "xlsx" || ext == "xls") {
      if (opt == 2) {
        # Process Excel files (specific format for milk composition and body weight files)
        temp <- readxl::read_excel(file, col_types = c("text", "text", "numeric"))
      } else {
        # Process Excel files (default format)
        temp <- readxl::read_excel(file, col_types = c("numeric", "date", "numeric", "text", rep("numeric", 6)))
      }
    } else if (ext == "") {
      # Process files with no extension (assume space-separated)
      temp <- utils::read.csv(file, sep = " ", header = TRUE)
    } else {
      warning(paste("Unsupported file type:", ext))
      next
    }

    # Combine data
    if (nrow(data) == 0) {
      data <- temp
    } else {
      data <- dplyr::bind_rows(data, temp)
    }
  }

  return(data)
}

#' @name transform_cols_in_rows
#' @title Transform Columns in Rows
#'
#' @description Transpose a table using animal ID as reference.
#'     It is useful when working with raw body weights.
#'
#' @param file a character string representing an Excel to transpose
#'
#' @return A file with transposed data to compile
#' @examples
#' @export
#'
#' @import readr
#' @import readxl
#' @import reshape2
transform_cols_in_rows <- function(file) {
  # Open file
  data <- readxl::read_excel(file, col_types = c("text", rep("numeric", 3)))

  # Transform col in rows using animal ID as reference
  data <- reshape2::melt(data, id.vars = "Visible_ID")

  # Add format to the col with Dates
  data$variable <- as.Date(data$variable, format = "%m/%d/%Y")

  # Add names to the cols
  names(data) <- c("Visible_ID", "Date", "BW_lbs")

  # Save file
  write.table(data, file = paste0("~/Downloads/XXX_BWfile"), row.names = FALSE)
}
