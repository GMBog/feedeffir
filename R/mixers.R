#' @name mixers
#' @title Check Diet Intakes from RIC system
#'
#' @description Checking diets from VR files from RIC system with feed intakes
#'
#' @param start_date A character string specifying the start date of the trial
#' @param VRfolder A character string with the path to the folder with VRfiles
#' @param groups A character string with the path to the file with groups IDs
#' @param bins Vector with the number of bins to be used. For example, 'seq(1,32)' or 'c(1,2,3,4,5,6, ..., 32)'
#'
#' @return A list of data sets with the intakes per diet per day, per week and per trial
#'
#' @export mixers
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @import readr
#' @import readxl
#' @import stringr
#' @import tidyr

mixers <- function(start_date, VRfolder, groups, bins = seq(1,32)) {

  # Read groups file
  read_groups <- function(path) {
    ext <- tools::file_ext(path)
    if (ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(path, col_types = rep("text", 3))
    } else {
      df <- readr::read_csv(path, col_types = readr::cols(
        FarmName = readr::col_character(),
        RFID = readr::col_character(),
        .default = readr::col_character()
      ))
    }
    names(df)[1:3] <- c("Cow", "Transponder", "TRT")
    df
  }
  Groups <- read_groups(groups)

  # Read VR files
  read_vr <- function(dir) {
    files <- list.files(dir, pattern = "^VR\\d{6}\\.DAT$", full.names = TRUE)
    all_data <- list()

    for (file in files) {
      # Extract date from filename
      date_str <- stringr::str_match(basename(file), "^VR(\\d{6})\\.DAT$")[,2]
      file_date <- as.Date(date_str, format = "%y%m%d")

      # Read file
      dat <- readr::read_csv(file, col_names = FALSE, col_types = cols(X1 = col_character()))

      # Remove columns 12 to 16 (if they exist)
      if (ncol(dat) >= 16) {
        dat <- dplyr::select(dat, -c(12:16))
      }

      # Move X10 to X11 (if both exist)
      if ("X10" %in% colnames(dat)) {
        dat$X11 <- dat$X10
      }

      # Add date column (from filename)
      dat$Date <- file_date
      all_data[[length(all_data) + 1]] <- dat
    }

    # Combine all data
    if (length(all_data) == 0) {
      return(NULL)
    }
    out <- dplyr::bind_rows(all_data)

    # Rename columns
    new_names <- c("Transponder", "Cow", "Bin", "Time_Start", "Time_End", "Sec", "Kg_Start", "Kg_End",
                   "Feed", "As_Fed", "As_Fed_Corr", "Date")
    colnames(out) <- new_names[1:ncol(out)]

    return(out)
  }
  VR <- read_vr(VRfolder)

  if (is.null(VR)) {
    stop("No VR data found in the specified directory.")
  }

  # Filter out records
  VR <- VR %>%
    dplyr::filter(Bin %in% bins,
                  as.numeric(As_Fed) >= 0) %>%
    dplyr::arrange(Cow, Date, Time_Start)

  # Identify and remove duplicates
  duplicates <- VR %>%
    dplyr::filter((lead(Cow) == Cow & lead(Date) == Date & lead(Bin) != Bin & as.numeric(lead(Time_Start) - Time_End, units = "secs") < 0) |
                    (Cow == lag(Cow) & Date == lag(Date) & Bin != lag(Bin) & as.numeric(Time_Start - lag(Time_End), units = "secs") < 0))

  VR <- dplyr::anti_join(VR, duplicates)

  # Join raw and groups data
  VR <- Groups %>%
    dplyr::inner_join(VR %>%
                        dplyr::mutate(Cow = as.character(Cow)), by = c("Cow", "Transponder")) %>%
    dplyr::filter(Date >= as.Date(start_date)) %>%
    dplyr::mutate(Week = floor(as.numeric(difftime(as.Date(Date), as.Date(start_date), units = "weeks"))) + 1)

  # Compute daily data
  daily_data <- VR %>%
    dplyr::group_by(Cow, Date, Feed, Week) %>%
    dplyr::summarise(
      IntakeKg = sum(as.numeric(As_Fed), na.rm = TRUE),
      TRT = unique(TRT),
      .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = Feed,
      values_from = IntakeKg,
      names_prefix = "IntakeKg_",
      values_fill = 0) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      TotalKg = sum(c_across(starts_with("IntakeKg_")), na.rm = TRUE),
      across(starts_with("IntakeKg_"),
             ~ round((.x / TotalKg) * 100, 0), .names = "Pct_{.col}")) %>%
    dplyr::group_by(Cow, Date) %>%
    dplyr::mutate(n = round(sum(c_across(starts_with("Pct_")) > 0) / 2, 0)) %>%
    dplyr::ungroup()

  # Compute weekly data
  weekly_data <- daily_data %>%
    dplyr::group_by(Cow, Week, TRT) %>%
    dplyr::summarise(
      n = sum(n, na.rm = TRUE),
      across(starts_with("IntakeKg_"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      TotalKg = sum(c_across(starts_with("IntakeKg_")), na.rm = TRUE),
      across(starts_with("IntakeKg_"),
             ~ round((.x / TotalKg) * 100, 0), .names = "Pct_{.col}")) %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(~ gsub("Pct_IntakeKg_", "Pct_", .x), starts_with("Pct_"))

  # Compute experimental data
  trial_data <- weekly_data %>%
    dplyr::group_by(Cow, TRT) %>%
    dplyr::summarise(
      n = sum(n, na.rm = TRUE),
      across(starts_with("IntakeKg_"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      TotalKg = sum(c_across(starts_with("IntakeKg_")), na.rm = TRUE),
      across(starts_with("IntakeKg_"),
             ~ round((.x / TotalKg) * 100, 0), .names = "Pct_{.col}")) %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(~ gsub("Pct_IntakeKg_", "Pct_", .x), starts_with("Pct_"))

  # Get a summary
  summary <- trial_data %>%
    tidyr::pivot_longer(
      cols = starts_with("IntakeKg_"),
      names_to = "Feed",
      values_to = "Intake") %>%
    dplyr::mutate(Feed = gsub("IntakeKg_", "", Feed)) %>%
    dplyr::group_by(Cow, TRT) %>%
    dplyr::summarise(
      n = first(n),
      IntakeKg_Assigned = sum(if_else(Feed == TRT, Intake, 0), na.rm = TRUE),
      IntakeKg_Other = sum(if_else(Feed != TRT, Intake, 0), na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::mutate(
      TotalKg = IntakeKg_Assigned + IntakeKg_Other,
      Pct_Assigned = round((IntakeKg_Assigned / TotalKg) * 100, 1),
      Pct_Other = round((IntakeKg_Other / TotalKg) * 100, 1))

  return(list(
    daily_data = daily_data,
    weekly_data = weekly_data,
    trial_data = trial_data,
    summary = summary
  ))
}

