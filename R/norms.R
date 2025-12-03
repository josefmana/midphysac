#' Compute scaled scores
#'
#' Using Table 7 from the normative study by Frydrychova et al.
#' ([2018](https://psycnet.apa.org/record/2018-63633-003)), this
#' function maps a single raw RAVLT delayed recall score to a
#' scaled score (M = 10, SD = 3).
#'
#' @param raw Numeric. Raw RAVLT delayed recall score.
#'
#' @returns Numeric vector with scaled scores.
#'
#' @export
ravlt_ss <- function(raw) {
  dplyr::case_when(
    raw == 0 ~ 3,
    raw == 1 ~ 4,
    raw == 2 ~ 5,
    raw == 3 ~ 6,
    raw == 4 ~ 7,
    raw == 5 ~ 8,
    raw == 6 ~ 9,
    raw %in% c(7,8) ~ 10,
    raw == 9 ~ 11,
    raw == 10 ~ 12,
    raw == 11 ~ 13,
    raw == 12 ~ 14,
    raw == 13 ~ 15,
    raw %in% c(14,15) ~ 16
  )
}


#' Find memory thresholds for SuperAging
#'
#' Using normative values of RAVLT and PVLT raw normative
#' data.
#'
#' @param pvlt_data_file Character. Path do PVLT normative
#'   data.
#' @param pvlt_id_file Character. Path do PVLT included IDs.
#' @param nanok_file Character. Path do NANOK normative
#'   data.
#' @param ravlt_file Character. Path do RAVLT normative
#'   data (subset of NANOK).
#' @param output Character. Return thresholds for SuperAging
#'   (`"thresholds"`) or normative values (`"norms"`).
#'
#' @returns RAVLT and PVLT thresholds for SuperAging or
#'   normative tables of RAVLT and PVLT.
#'
#' @export
extract_thresholds <- function(
    pvlt_data_file,
    pvlt_id_file,
    nanok_file,
    ravlt_file,
    output = "thresholds"
    ) {
  # Demography data:
  demo <- readxl::read_xls(path = nanok_file, sheet = "NANOK_demografie") |>
    dplyr::mutate(edu = dplyr::case_when(
      `1_EDU_TYPE_2` == 0 ~ "lower",
      `1_EDU_TYPE_2` == 1 ~ "higher"
    )) |>
    dplyr::filter(`1_GENDER` == 2) |> # women only
    dplyr::select(ID, edu)
  # RAVLT data:
  ravlt <- readr::read_csv(file = ravlt_file, col_types = readr::cols()) |>
    dplyr::filter(gender == "zena") |> # keep women only
    dplyr::mutate(
      ID = id,
      age = vek,
      age_cat = vek_kategorie,
      edu = dplyr::case_when(vzdelani_2 == "nizsi" ~ "lower", vzdelani_2 == "vyssi" ~ "higher"),
      raw = RAVLT_t7,
      scaled = ravlt_ss(RAVLT_t7)
    ) |>
    dplyr::select(ID, age, age_cat, edu, raw, scaled)
  # PVLT data:
  pvlt <- readr::read_csv(pvlt_data_file, col_types = readr::cols()) |>
    dplyr::filter((!(Jméno %in% readr::read_csv(pvlt_id_file, col_names = FALSE)$X1))) |>
    dplyr::mutate(
      ID = as.numeric(Jméno), # for compatibility with NANOK, participant 1226p and 1247p were dropped as a result
      age = Věk,
      raw = T9
    ) |>
    dplyr::select(ID, age, raw) |>
    dplyr::left_join(demo, dplyr::join_by(ID)) |>
    dplyr::filter(complete.cases(edu)) |>
    dplyr::mutate(age_cat = dplyr::case_when(
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      age >= 70 & age < 75 ~ "70-74",
      age >= 75 & age < 80 ~ "75-79",
      age >= 80 & age < 85 ~ "80-84",
      age >= 85 ~ "85+"
    )) |>
    dplyr::select(ID, age, age_cat, edu, raw)
  # Define a function for computing M, S, N and df:
  M <- function(x) {
    mean(x, na.rm = TRUE)
  }
  S <- function(x) {
    sd(x, na.rm = TRUE)
  }
  N <- function(x) {
    length(na.omit(x))
  }
  # Compute threshold values, i.e., expected value for a women
  # in 60-64 years age category and their 95% CIs
  thresholds <- purrr::map_dfr(c("ravlt", "pvlt"), function(task) {
    sapply(c("M", "S", "N"), function(fun) {
      sapply(c("lower", "higher"), function(l) {
        do.call(fun, list(subset(get(task), age_cat == "60-64" & edu == l)$raw))
      })
    }) |>
      as.data.frame() |>
      tibble::rownames_to_column("edu") |>
      dplyr::mutate(
        df = N - 1,
        low_CI = M + ( S / sqrt(N) ) * qt(.025, df),
        high_CI = M - ( S / sqrt(N) ) * qt(.025, df),
        thresh_mean = ceiling(M),
        thresh_low = ceiling(low_CI),
        thresh_high = ceiling(high_CI),
        task = task
      )
  }) |>
    dplyr::relocate(task, .before = 1)
  # Normative tables for RAVLT and PVLT:
  norms <- array(
    data = NA, # to be added via loops
    dim = c(task = 2, stat = 3, educ = 2, age = length(unique(ravlt$age_cat))),
    dimnames = list(
      task = c("ravlt", "pvlt"),
      stat = c("M", "S", "N"),
      educ = c("lower", "higher"),
      age = unique(ravlt$age_cat)
    )
  )
  # looping action
  for (t in dimnames(norms)$task) {
    for (s in dimnames(norms)$stat) {
      for (e in dimnames(norms)$educ) {
        for (a in dimnames(norms)$age) {
          norms[t, s, e, a] <- do.call(s, list(subset(get(t), age_cat == a & edu == e)$raw))
        }
      }
    }
  }
  # Return what was asked for:
  get(output)
}
