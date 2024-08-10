#' @title format_milkw_AFI
#' @name format_milkw_AFI
#' @description Format milk weights from AFI.
#'
#' @param Trial Name of the trial
#' @param Dir1 Directory with files from AFI system (.txt)
#' @param Dir2 Directory with csv files
#'
#' @return A table with the compiled milk weights from AFI system
#'
#' @examples
#'
#' @export format_milkw_AFI
#'
#' @import readr
#' @import dplyr
#' @import readr


# Function to process AFI files
process_AFI_files <- function(file_list) {
  data <- data.frame()  # Initialize an empty data frame

  for (file in file_list) {

    # Extract animal ID from file name
    ID <- sub('^([0-9]+) - .*', '\\1', basename(file))

    # Read the file
    temp <- readr::read_table(file, col_types = cols(Days = col_skip(), Milk = col_number(),
                                                     Cond. = col_skip(), AMT = col_skip(),
                                                     X6 = col_skip()))

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

# Function to process csv files
process_csv_files <- function(file_list) {
  data <- data.frame() # Initialize an empty data frame

  for (file in file_list) {

    temp <- utils::read.csv(file, sep = "")

    if(nrow(temp) == 0){
      data = temp
    }else{
      data = dplyr::bind_rows(temp, data)
    }
  }

  return(data)
}


# Main function to format milk weights from AFI system
format_milkw_AFI <- function(Trial = NA, Dir1 = NA, Dir2 = NA){

  ## Step 1: Merge individual AFI files from each cow

  # List AFI files in the directory
  list1 <- list.files(path = Dir1, recursive = TRUE, full.names = TRUE)
  list1

  # Process AFI files
  data1 <- process_AFI_files(list1)

  # Save milk weights file in Excel or without format
  write.table(data, file = paste0("~/Downloads/",Trial,"_MilkWeights",Sys.Date()))


  ## Step 2: Merge MW files generated in the step 1 with those from previous weeks

  ## Remove 'file' folder in Dir1 if it exists
  file_path <- file.path(Dir1, "file")
  if (dir.exists(file_path)) {
    unlink(file_path, recursive = TRUE)
  }


  # List csv files in the directory
  list2 <- list.files(path = Dir2, recursive = TRUE, full.names = TRUE)
  list2

  # Process files using the function for read.csv files
  data2 <- process_csv_files(list2)

  # Remove duplicates
  data2 <- data2 %>% dplyr::distinct(Visible_ID, Date, .keep_all = TRUE)

  # Save milk weights file in Excel or without format
  write.table(data2, file = paste0("~/Downloads/UW_",Trial,"_MilkWeights",Sys.Date()), row.names = F, quote = F)


}

