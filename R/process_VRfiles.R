#' @name process_VRfiles
#' @title Process VR files from RIC system
#'
#' @description Processes VR files from RIC system with feed intakes and
#'    save the results into an Excel file.
#'
#' @param exp A character string specifying the experiment ID to use in file paths and processing.
#' @param VRfile VRfile with intakes
#' @param bins Vector with the number of bins to be used. For example, 'seq(1,32)' or 'c(1,2,3,4,5,6, ..., 32)'
#' @param output_dir The directory path where the output files should be saved.
#'
#' @return A plot with the Fed in Kg per animal and an Excel file with the intakes.
#'
#' @export process_VRfiles
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import ggplot2
#' @import readr
#' @import readxl
#' @import stringr
#' @import tidyr
#' @import writexl
utils::globalVariables(c("TrialID", "X1", "X2", "X3", "X4", "X5", "X9", "X10", "Type", "Feed", "FedKg", "Date",
                         "TotalDiet", "Diet", "Percentage"))

process_VRfiles <- function(exp = NA, VRfile, bins = seq(1, 32), output_dir) {
  # Read the intake file
  VR <- readr::read_csv(VRfile, col_names = FALSE, col_types = cols(X1 = col_character())) %>%
    dplyr::select(-12:-16) %>%
    dplyr::mutate(X11 = X10)

  # Extract Date from the filename
  Date <- as.Date(stringr::str_match(VRfile, "VR(\\d{6})\\.DAT$")[2], format = "%y%m%d")

  # Filter bins
  VR <- VR[VR$X3 %in% bins, ]

  # Step 1: List high negative values
  high_negative_intakes <- VR[VR$X10 <= -5, ]

  # Step 2: Remove negative intakes
  cows_negative_intakes <- VR[VR$X10 < 0, ]
  names(cows_negative_intakes) <- c(
    "Transponder",
    "Cow",
    "Bin",
    "Time_Start",
    "Time_End",
    "Sec",
    "Kg_Start",
    "Kg_End",
    "Feed",
    "As_Fed",
    "As_Fed_Corr"
  )
  VR <- VR[VR$X10 >= 0, ]

  # Step 3: Order data
  VR <- VR[order(VR$X2, VR$X4), ]

  # Step 4: Save raw data
  Raw_data <- VR %>%
    dplyr::mutate(Date = Date, Type = "FEED") %>%
    dplyr::relocate(Date, Type, .before = X1)
  names(Raw_data) <- c(
    "Date",
    "Type",
    "Transponder",
    "Cow",
    "Bin",
    "Time_Start",
    "Time_End",
    "Sec",
    "Kg_Start",
    "Kg_End",
    "Feed",
    "As_Fed",
    "As_Fed_Corr"
  )

  # Step 5: Identify and save duplicates
  Duplicates <- VR %>%
    dplyr::filter((lead(X2) == X2 & lead(X3) != X3 & as.numeric(lead(X4) - X5, units = "secs") < 0) |
      (X2 == lag(X2) & X3 != lag(X3) & as.numeric(X4 - lag(X5), units = "secs") < 0))
  names(Duplicates) <- c(
    "Transponder",
    "Cow",
    "Bin",
    "Time_Start",
    "Time_End",
    "Sec",
    "Kg_Start",
    "Kg_End",
    "Feed",
    "As_Fed",
    "As_Fed_Corr"
  )

  # Step 6: Get the maximum and minimum start time
  VR$X4 <- format(VR$X4, "%H:%M:%S")
  VR$X5 <- format(VR$X5, "%H:%M:%S")
  Latest_times <- data.frame(Date, Total = max(VR$X4))
  Earliest_times <- data.frame(Date, Total = min(VR$X5))

  # Step 7: Create final data
  Final_data <- VR %>%
    dplyr::group_by(Visible_ID = X2) %>%
    dplyr::summarise(
      TrialID = first(exp),
      Date = first(Date),
      Feed = first(X9),
      FedKg = sum(X10)
    ) %>%
    dplyr::select(TrialID, Date, Visible_ID, Feed, FedKg) %>%
    dplyr::mutate(
      Visible_ID = as.numeric(Visible_ID),
      Obs = NA
    )

  # Step 8: Get top 10 and bottom 10 cows
  Top10 <- head(Final_data[order(Final_data$FedKg, decreasing = TRUE), ], 10)
  Bottom10 <- head(Final_data[order(Final_data$FedKg), ], 10)

  # Step 9: Plot intakes by cow by diet
  plot <- ggplot2::ggplot(Final_data, ggplot2::aes(x = as.character(Visible_ID), y = FedKg, fill = Feed)) +
    ggplot2::geom_col(width = 0.3) +
    ggplot2::theme_grey() +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1.05, size = 8, family = "Times New Roman"),
        axis.text.y = ggplot2::element_text(hjust = 1.05, size = 8, family = "Times New Roman"),
        axis.title.x = ggplot2::element_text(size = 12, family = "Times New Roman", face = "bold"),
        axis.title.y = ggplot2::element_text(size = 12, family = "Times New Roman", face = "bold"),
        legend.position = "right"
      ) +
    ggplot2::labs(title = "", x = "Visible ID", y = "Fed (Kg)")

  print(plot)

  # Step 10: Handle different diets
  Diets <- VR %>%
    dplyr::group_by(Diet = X9) %>%
    dplyr::summarise(FedKg = round(sum(X10), 0))

  Diets_per_cow <- VR %>%
    dplyr::group_by(Visible_ID = X2) %>%
    dplyr::mutate(TotalDiet = sum(X10)) %>%
    dplyr::group_by(Visible_ID, Diet = X9) %>%
    dplyr::summarise(Percentage = round(sum(X10) / first(TotalDiet) * 100, 1)) %>%
    tidyr::pivot_wider(names_from = Diet, values_from = Percentage) %>%
    mutate_at(vars(-Visible_ID), ~ ifelse(. == 0, NA, .))

  # Step 11: Create an Excel file with all tables
  writexl::write_xlsx(
    list(
      Raw_Data = Raw_data,
      Latest_times = Latest_times,
      Earliest_times = Earliest_times,
      Duplicates = Duplicates,
      Animals_removed = cows_negative_intakes,
      Top10 = Top10,
      Bottom10 = Bottom10,
      Multiple_Diets = Diets,
      Diets_Fedcows = Diets_per_cow,
      Final_Data = Final_data
    ),
    path = paste0(output_dir, "/Intakes_", Date, ".xlsx")
  )

  message("The VR file was processed and the result saved at ", output_dir)

  return(Final_data)
}
