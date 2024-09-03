#' @title process_files
#' @name process_files
#' @description Process csv, excel, or no extension files with specific format
#'    for milk composition and body weights files
#'
#' @param Dir A directory containing the file paths of the CSV, Excel, or no extension files to be processed.
#' @param opt Option to process Excel files from Rosy Lane (opt = 2)
#'
#' @return A data frame containing the combined data from all processed files.
#' @export process_files
#'
#' @examples
#' @import readxl
#' @import utils
#' @import dplyr
#' @import reshape2
#' @import readr

process_files <- function(Dir = NA, opt = 1) {
  # List all files in the directory
  file_list <- list.files(Dir, full.names = TRUE)
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
        temp <- readxl::read_excel(file, col_types = c("numeric", "date", "text", rep("numeric", 7)))
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


#' @title process_AFI_files
#' @name process_AFI_files
#' @description Process files from AFI system with milk weights
#'
#' @param Dir Directory with AFI files
#'
#' @return A table with all animals with milk weights
#' @export
#'
#' @examples
#' @import dplyr
#' @import readr

process_AFI_files <- function(Dir) {
  # List AFI files in the directory
  file_list <- list.files(path = Dir, recursive = FALSE, full.names = TRUE)
  print(file_list)

  data <- data.frame() # Initialize an empty data frame

  for (file in file_list) {
    # Extract animal ID from file name
    ID <- sub("^([0-9]+) - .*", "\\1", basename(file))

    # Read the file
    temp <- readr::read_table(file, col_types = readr::cols(
      Days = readr::col_skip(), Milk = readr::col_number(),
      Cond. = readr::col_skip(), AMT = readr::col_skip(),
      X6 = readr::col_skip()
    ))

    # Add animal ID column
    temp$Visible_ID <- ID
    temp <- temp[, c("Visible_ID", names(temp)[-which(names(temp) == "Visible_ID")])]

    # Combine dataframes
    if (nrow(temp) == 0) {
      data <- temp
    } else {
      data <- dplyr::bind_rows(data, temp)
    }
  }

  return(data)
}



#' @title transform_cols_in_rows
#' @name transform_cols_in_rows
#' @description Transpose a table using animal ID as reference
#'
#' @param file Excel file to transpose
#'
#' @return A file with transposed data to compile
#' @export
#'
#' @examples
#' @import readxl
#' @import reshape2
#' @import readr

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
