#' @title
#' @name
#' @description Format body weights files.
#'
#' @param Trial Name of the trial
#' @param file File that contains the body weights
#'
#' @return A table with the body weights (AnimalID, Date, and Weights)
#'
#' @examples
#'
#' @export
#'
#' @import readr
#' @import dplyr
#' @import readr
#' @import reshape2



transform_cols_in_rows <- function(Trial = NA, file) {

  # Open file
  data <- readxl::read_excel(file, col_types = c("text", rep("numeric",3)))

  # Transform col in rows using animal ID as reference
  data <- reshape2::melt(data, id.vars = "Visible_ID")

  # Add format to the col with Dates
  data$variable <- as.Date(data$variable, format = "%m/%d/%Y")

  # Add names to the cols
  names(data) <- c("Visible_ID", "Date", "BW_lbs")

  # Save file
  write.table(data, file = paste0("~/Downloads/",Trial,"_BW"), row.names = FALSE)

}

process_excel_files <- function(file_list){
  data <- data.frame() # Initialize an empty data frame

  for (file in file_list) {

    temp <- readxl::read_excel(file, col_types = c("numeric", "date", "text", rep("numeric", 7)))

    if(nrow(temp) == 0){
      data = temp
    }else{
      data = dplyr::bind_rows(temp, data)
    }
  }

  return(data)

}

compile_excel_files <- function(Trial = NA, Dir = NA) {

  # List files in the directory
  list <- list.files(path = Dir, recursive = TRUE, full.names = TRUE)
  list

  # Process files using the function for read.csv files
  data <- process_excel_files(list)

  # Save file
  file_path <- paste0("~/Downloads/UW_",Trial,"_BodyWeights",Sys.Date(),".xlsx")
  write.xlsx(data, file_path)

}

