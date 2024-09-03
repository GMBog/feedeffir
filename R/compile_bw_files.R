#' @name compile_bw_files
#' @title Compile Body Weights Files
#'
#' @description `compile_bw_files()` compiles and format body weights files.
#'
#' @param Trial Name of the trial
#' @param Dir Directory where they are the body weight files
#' @param opt Option to process BW files. If Rosy Lane data (opt = 2)
#'
#' @return A table with body weights (AnimalID, Date, and Weights)
#'
#' @examples
#' @export
#'
#' @import openxlsx
compile_bw_files <- function(Trial = NA, Dir = NA, opt = 1) {

  # Process files using the function for read.csv files
  data <- process_files(Dir, opt)

  # Save file
  openxlsx::write.xlsx(data, paste0("~/Downloads/UW_", Trial, "_BodyWeights", Sys.Date(), ".xlsx"))
}
