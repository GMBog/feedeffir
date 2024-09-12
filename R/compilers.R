#' @name compile_milkcomp_files
#' @title Compile Milk Composition Files
#'
#' @description `compile_milkcomp_files()` compiles and formats milk composition files
#'
#' @param exp a character string representing the name of the experiment
#' @param dir a character string representing the directory with milk composition files in Excel format
#' @param output_dir a character string representing the output directory
#'
#' @return An Excel file with the compiled milk composition data
#'
#' @examples
#' @export compile_milkcomp_files
#'
#' @import openxlsx
compile_milkcomp_files <- function(exp = NA, dir, output_dir) {
  # Process files using the function to read csv files
  data <- process_files(dir)

  # Include names to the compiled milk composition file
  names(data) <- c(
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

  # Save file
  openxlsx::write.xlsx(data,
                       paste0(output_dir, "/UW_", exp, "_MilkComposition", Sys.Date(), ".xlsx"))
}


#' @name compile_milkw_files
#' @title Compile Milk Weights Files
#'
#' @description `compile_milkw_files()` compiles and formats milk weight files
#'
#' @param exp a character string representing the name of the experiment
#' @param dir a character string representing the directory with milk composition files in Excel format
#' @param output_dir a character string representing the output directory
#'
#' @return An Excel file with the compiled milk weights data
#'
#' @examples
#' @export compile_milkw_files
#'
#' @import openxlsx
compile_milkw_files <- function(exp = NA, dir, output_dir) {
  ## Initialize an empty data frame to store the merged data
  data <- data.frame()

  ## List files in the directory
  list <- list.files(path = dir, recursive = T, full.names = T)
  print(list)

  ### Read files in a loop
  for (file in list) {
    temp <- read.csv(file, sep = "") # Give the option to open excel files!!!

    if (nrow(temp) == 0) {
      data <- temp
    } else {
      data <- bind_rows(data, temp)
    }
  }
  rm(temp)

  # Remove duplicates dates and ids based on the way we pull down data from DairyComp
  data <- data %>%
    dplyr::distinct()

  data$MilkLbs[data$MilkLbs == 0] <- "."

  # Save file
  openxlsx::write.xlsx(data,
                       paste0(output_dir, "/UW_", exp, "_MilkWeights", Sys.Date(), ".xlsx"))
}



#' @name compile_bw_files
#' @title Compile Body Weights Files
#'
#' @description Compiles and formats body weights files.
#'
#' @param exp a character string representing the name of the experiment
#' @param dir a character string representing the directory with the body weight files
#' @param opt an integer representing the option to process BW files. If Rosy Lane data (opt = 2)
#'
#' @return An Excel file with body weights
#'
#' @examples
#' @export
#'
#' @import openxlsx
compile_bw_files <- function(exp = NA, dir, opt = 1) {
  # Process files using the function for read.csv files
  data <- process_files(dir, opt)

  # Save file
  openxlsx::write.xlsx(data, paste0("~/Downloads/UW_", exp, "_BodyWeights", Sys.Date(), ".xlsx"))
}


#' @name compile_VRfiles
#' @title Compile VR Files with Intakes
#'
#' @description Compiles VR files with intakes from each day.
#'
#' @param dir a character string representing the directory with compiled file
#' @param compfile a character string representing the name of the file with compiled data
#' @param data the data frame returned by `process_VRfiles()`
#'
#' @return An Excel file with compiled VR files
#'
#' @examples
#' @export compile_VRfiles
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import readxl
#' @import writexl
compile_VRfiles <- function(dir, compfile, data) {
  list <- list.files(path = dir, recursive = T, full.names = T)
  list

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

  data <- dplyr::bind_rows(temp, data)

  # Remove overlapping records
  data <- data %>%
    dplyr::distinct()

  writexl::write_xlsx(data, path = paste0(dir, compfile))

  return(data)
}
