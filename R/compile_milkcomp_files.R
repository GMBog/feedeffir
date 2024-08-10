#' @title compile_milkcomp_files
#' @name compile_milkcomp_files
#' @description Compile milk composition files
#'
#' @param Trial Name of the trial
#' @param Dir Directory with milk composition files in Excel format
#'
#' @return A table with the compiled milk composition files
#'
#' @examples
#'
#' @export compile_milkcomp_files
#'
#' @import readxl
#' @import dplyr
#' @import readr

process_milkcomp_files <- function(file_list) {
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

compile_milkcomp_files <- function(Trial = NA, Dir = NA) {

  # List milk composition files in the directory
  list <- list.files(path = Dir, recursive = TRUE, full.names = TRUE)
  list

  # Process files using the function for read.csv files
  data <- process_milkcomp_files(list)

  # Include names to the compiled milk composition file
  names(data) <- c("Sample", "Date", "Visible_ID", "MilkNum", "FatPct", "PrtPct", "LacPct", "SnF", "Urea", "Cells")

  # Save file
  file_path <- paste0("~/Downloads/UW_",Trial, "_MilkComposition", Sys.Date(), ".xlsx")
  write.xlsx(data, file_path)

}

