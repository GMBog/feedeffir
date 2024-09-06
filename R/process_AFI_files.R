#' @name process_AFI_milkw
#' @title Process and Format AFI Milk Weights
#'
#' @description Compiles and formats milk weight files from AFI system.
#'     First, it compiles cow files, then compile using the ID the milk weights with the previous ones.
#'
#' @param exp a character string representing the name of the experiment
#' @param dir1 a character string representing the directory with files from AFI system (.txt)
#' @param dir2 a character string representing the directory with CSV files
#'
#' @return A table with the compiled milk weights from AFI system
#' @examples
#' @export process_AFI_milkw
#'
#' @import readr
#' @import dplyr
#' @importFrom dplyr %>%
#' @import readr
utils::globalVariables(c("Visible_ID", "Date"))

# Function to format milk weights from AFI system
process_AFI_milkw <- function(exp = NA, dir1, dir2) {
  ## Step 1: Merge individual AFI files from each cow

  process_AFI_files <- function(dir) {
    # List AFI files in the directory
    file_list <- list.files(path = dir, recursive = FALSE, full.names = TRUE)
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

  # Process AFI files
  data1 <- process_AFI_files(dir1)

  # Save milk weights file in Excel or without format
  write.table(data1,
    file = paste0(dir2, exp, "_MilkWeights", Sys.Date()),
    sep = " ", row.names = TRUE, col.names = TRUE
  )


  ## Step 2: Merge MW files generated in the step 1 with those from previous weeks

  # Process files using the function for read.csv files
  data2 <- process_files(dir2)

  # Remove duplicates
  data2 <- data2 %>%
    dplyr::arrange(Visible_ID, Date) %>% # Sort the data by Visible_ID and Date
    dplyr::group_by(Visible_ID, Date) %>% # Group by Visible_ID and Date
    dplyr::slice_tail(n = 1) %>% # Keep the last occurrence within each group
    dplyr::ungroup()

  # Save milk weights file in Excel or without format
  write.table(data2, file = paste0("~/Downloads/UW_", exp, "_MilkWeights", Sys.Date()), row.names = F, quote = F)
}
