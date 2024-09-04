#' @name compile_milkcomp_files
#' @title Compile Milk Composition Files
#'
#' @description `compile_milkcomp_files()` compiles and formats milk composition files
#'
#' @param Trial Name of the trial
#' @param Dir Directory with milk composition files in Excel format
#'
#' @return A table with the compiled milk composition files
#'
#' @examples
#' @export compile_milkcomp_files
#'
#' @import openxlsx
compile_milkcomp_files <- function(Trial = NA, Dir = NA) {
  # List milk composition files in the directory

  # Process files using the function to read csv files
  data <- process_files(Dir)

  # Include names to the compiled milk composition file
  names(data) <- c("Sample", "Date", "Visible_ID", "MilkNum", "FatPct", "PrtPct", "LacPct", "SnF", "Urea", "Cells")

  # Save file
  openxlsx::write.xlsx(data, paste0("~/Downloads/UW_", Trial, "_MilkComposition", Sys.Date(), ".xlsx"))
}

