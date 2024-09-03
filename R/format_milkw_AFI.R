#' @name format_milkw_AFI
#' @title format_milkw_AFI
#'
#' @description `format_milkw_AFI()` compiles and formats milk weights files
#'     from AFI computer at the farm. First, it will compile cow files and
#'     then compile using the animal ID the milk weights with the previous ones.
#'
#' @param Trial Name of the trial
#' @param Dir1 Directory with files from AFI system (.txt)
#' @param Dir2 Directory with csv files
#'
#' @return A table with the compiled milk weights from AFI system
#' @details
#' This function uses two helper functions:
#' - `process_AFI_files`: Processes AFI files to extract and format milk weights.
#'
#' @examples
#' @export format_milkw_AFI
#'
#' @import readr
#' @import dplyr
#' @importFrom dplyr %>%
#' @import readr
utils::globalVariables(c("Visible_ID", "Date"))

# Function to format milk weights from AFI system
format_milkw_AFI <- function(Trial = NA, Dir1 = NA, Dir2 = NA) {
  ## Step 1: Merge individual AFI files from each cow

  # Process AFI files
  data1 <- process_AFI_files(Dir1)

  # Save milk weights file in Excel or without format
  write.table(data1,
    file = paste0(Dir2, Trial, "_MilkWeights", Sys.Date()),
    sep = " ", row.names = TRUE, col.names = TRUE
  )


  ## Step 2: Merge MW files generated in the step 1 with those from previous weeks

  # Process files using the function for read.csv files
  data2 <- process_files(Dir2)

  # Remove duplicates
  data2 <- data2 %>%
    dplyr::arrange(Visible_ID, Date) %>% # Sort the data by Visible_ID and Date
    dplyr::group_by(Visible_ID, Date) %>% # Group by Visible_ID and Date
    dplyr::slice_tail(n = 1) %>% # Keep the last occurrence within each group
    dplyr::ungroup()

  # Save milk weights file in Excel or without format
  write.table(data2, file = paste0("~/Downloads/UW_", Trial, "_MilkWeights", Sys.Date()), row.names = F, quote = F)
}
