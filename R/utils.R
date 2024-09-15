#' @name transform_cols_in_rows
#' @title Transform Columns in Rows
#'
#' @description Transpose a table using animal ID as reference.
#'     It is useful when working with raw body weights.
#'
#' @param file a character string representing an Excel to transpose
#'
#' @return A file with transposed data to compile
#' @examples
#' @export
#'
#' @import readr
#' @import readxl
#' @import reshape2
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
