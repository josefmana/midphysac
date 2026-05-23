# Set-up targets for data import:
targets_data <- list(
  targets::tar_target(
    datafile,
    command = here::here("data-raw", "COSACTIW_NANOK_KOKOSA.xlsx"),
    format = "file"
  ),
  targets::tar_target(
    actfile,
    command = here::here("data-raw", "PA_combined_output.xlsx"),
    format = "file"
  ),
  targets::tar_target(
    activity_data,
    command = readxl::read_xlsx(actfile, "Summary")
  ),
  targets::tar_target(
    data,
    cue = targets::tar_cue("always"),
    command = import_data(
      file = datafile,
      sheet = "cosactiw+nanok+kokosa",
      acts = activity_data,
      norms = memory_norms,
      thresholds = memory_thresholds,
      thres_type = "mean"
    ) |>
      dplyr::filter(mPA %in% c("COSACTIW", "NANOK")) |>
      dplyr::mutate(mPA = factor(mPA, levels = c("COSACTIW", "NANOK")))
  ),
  #tarchetypes::tar_map(
  #  values = tibble::tibble(
  #    branch = c("cosactiw_nanok", "cosactiw_kokosa"),
  #    groups = list(c("COSACTIW", "NANOK"), c("COSACTIW", "KOKOSA"))
  #  ),
  #  names = tidyselect::all_of("branch"),
  #  targets::tar_target(
  #    data,
  #    command = data_raw |>
  #      dplyr::filter(mPA %in% groups) |>
  #      dplyr::mutate(mPA = factor(mPA, levels = groups))
  #  )
  #),
  NULL
)
