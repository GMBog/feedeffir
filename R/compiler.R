#' @name compiler
#' @title Compile Files
#'
#' @description Compiles and formats different types of files.
#'
#' @param exp a character string representing the name of the experiment.
#' @param dir a character string representing the directory with files.
#' @param save_dir a character string representing the output directory.
#' @param type a character string indicating the type of data to process ('bw', 'mw', 'mcomp', 'vr').
#' @param compfile a character string indicating the name of the compiled file to use for merging (for 'vr' type).
#' @param vrdata a data frame with the process vr file
#'
#' @return A data frame with the compiled data.
#'
#' @examples
#' @export compiler
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import readr
#' @import readxl
#' @import openxlsx
#' @import utils
compiler <- function(exp = NA, dir, save_dir, type, compfile = NULL, vrdata = NULL) {
  # Ensure the type parameter is lowercase
  type <- tolower(type)

  # List all files in the directory
  file_list <- list.files(dir, full.names = TRUE)
  print(file_list)

  data <- data.frame() # Initialize an empty data frame

  if (type == "bw") {
    # Process body weights files
    for (file in file_list) {
      ext <- tools::file_ext(file) # Get the file extension

      if (ext == "xlsx") {
        temp <- readxl::read_excel(file, col_types = c("text", "text", "numeric"))
      } else if (ext == "csv") {
        temp <- readr::read_csv(file)
      } else {
        stop("Unsupported file format: ", ext)
      }

      # Append data to the main data frame
      data <- dplyr::bind_rows(data, temp)
    }
    # Save the compiled data
    openxlsx::write.xlsx(data, paste0(save_dir, "/UW_", exp, "_BodyWeights_", Sys.Date(), ".xlsx"))

    return(data)
  } else if (type == "mw") {
    # Process milk weights files
    for (file in file_list) {
      temp <- utils::read.csv(file, sep = "")

      # Append data to the main data frame
      data <- dplyr::bind_rows(data, temp)
    }

    # Clean and remove duplicates
    data <- data %>%
      dplyr::distinct() %>%
      dplyr::mutate(MilkLbs = ifelse(MilkLbs == 0, ".", MilkLbs))

    # Save the compiled data
    openxlsx::write.xlsx(data, paste0(save_dir, "/UW_", exp, "_MilkWeights_", Sys.Date(), ".xlsx"))

    return(data)
  } else if (type == "mcomp") {
    # Process milk composition files
    for (file in file_list) {
      temp <- readxl::read_excel(file, col_types = c("numeric", "date", "numeric", "text", rep("numeric", 6)))

      # Append data to the main data frame
      data <- dplyr::bind_rows(data, temp)
    }

    # Rename columns
    colnames(data) <- c(
      "Sample",
      "Date",
      "Visible_ID",
      "MilkNum",
      "FatPct",
      "PrtPct",
      "LacPct",
      "SnF",
      "Urea",
      "Cells"
    )

    # Save the compiled data
    openxlsx::write.xlsx(data, paste0(save_dir, "/UW_", exp, "_MilkComposition_", Sys.Date(), ".xlsx"))

    return(data)
  } else if (type == "vr") {
    # Merge the compiled data with daily data
    if (compfile %in% basename(list)) {
      # If the file exists, read the Excel file
      temp <- readxl::read_excel(paste0(dir, compfile))
      message("The compiled file exist!")
    } else {
      # If the file does not exist, create an empty data frame
      temp <- data.frame()
      message("The compiled files doesn't exist. A compiled file was created!")
    }

    data <- dplyr::bind_rows(temp, vrdata)

    # Remove overlapping records
    data <- data %>%
      dplyr::distinct()

    # Save the compiled file
    writexl::write_xlsx(data, path = paste0(dir, compfile))

    return(data)
  }
}
