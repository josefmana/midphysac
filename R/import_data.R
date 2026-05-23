#' Import data for analysis
#'
#' Reads, pre-processes, and cleans data for analysis.
#'
#' @param file Character. Path to raw data XLSX file.
#' @param sheet Character or numeric. Denotes sheet where the
#'   data resides inside of `file`.
#' @param norms Tibble or data.frame. Normative values for scoring
#'   memory tasks. Computed by \code{extract_thresholds}.
#' @param thresholds Tibble or data.frame. Thresholds based on normative
#'   values of  memory tasks. Computed by \code{extract_thresholds}.
#' @param thres_type Character.
#'
#' @returns A tibble with relevant columns.
#'
#' @export
import_data <- function(
    file,
    sheet,
    norms,
    thresholds,
    thres_type = "mean"
) {
  readxl::read_xlsx(file, sheet = sheet, na = "NA") |>
    dplyr::select(
      1, Study, Age, `Education-2-cat`, `PA...4`,
      `Regular-PA`, MMSE, GDS15, GAI, FAQ, `Total-mental-activities`, Health, # outcomes
      `Memory test`, RAVLT_delayed_recall, TMT_B_time, Spon_sem, VF_animals, # SuperAging variables raw scores
      `SA-TMT-B-new`, `SA-BNT-new`, `SA-VF`, # SuperAging indicators
      Z_RAVLT_PVLT_delayed_recall, Z_TMT_B_uds, Z_BNT_new, Z_VF_uds, # SuperAging variables z scores
      Type_of_prevailing_occupation_during_life, Marital_status,
      `Do_you_smoke?`, Hypertension, Diabetes # health-related covariates
    ) |>
    dplyr::mutate(
      mPA = factor(
        dplyr::case_when(
          Study == "COSACTIW" ~ 1,
          Study == "KOKOSA"   ~ 2,
          Study == "NANOK"    ~ 3
        ),
        levels = 1:3,
        labels = c("COSACTIW", "KOKOSA", "NANOK")
      ),
      Cosactiw = factor(
        dplyr::if_else(Study == "COSACTIW", 1, 0)
      ),
      Education = factor(
        `Education-2-cat`,
        levels = 1:2,
        labels = c("lower", "higher"),
        ordered = TRUE
      ),
      Age_bin = dplyr::case_when(
        Age >= 60 & Age < 65 ~ "60-64",
        Age >= 65 & Age < 70 ~ "65-69",
        Age >= 70 & Age < 75 ~ "70-74",
        Age >= 75 & Age < 80 ~ "75-79",
        Age >= 80 & Age < 85 ~ "80-84",
        Age >= 85 ~ "85+"
      ),
      Z_RAVLT_PVLT_delayed_recall = unlist(
        # re-calculate z-score for memory tasks based on table norms
        # from data provided to us by original authors
        sapply(seq_len(dplyr::n()), function(i) {
          test <- dplyr::case_when(
            `Memory test`[i] == "PVLT"  ~ "pvlt",
            `Memory test`[i] == "RAVLT" ~ "ravlt"
          )
          score <- as.integer(RAVLT_delayed_recall[i])
          M  <- norms[test, "M", Education[i], Age_bin[i]]
          SD <- norms[test, "S", Education[i], Age_bin[i]]
          (score - M) / SD
        }),
        use.names = FALSE
      ),
      cutoff = unlist(
        sapply(seq_len(dplyr::n()), function(i) {
          # helper column showing thresholds for cognitive SA
          with(thresholds, {
            t <- dplyr::case_when(
              `Memory test`[i] == "PVLT"  ~ "pvlt",
              `Memory test`[i] == "RAVLT" ~ "ravlt"
            )
            e <- as.character(Education[i])
            get(paste0("thresh_", thres_type))[task == t & edu == e]
          })
        }),
        use.names = FALSE
      ),
      Delayed_recall_SA = dplyr::if_else(
        # need to be at least at the level of threshold in delayed
        # memory task to qualify for SA
        RAVLT_delayed_recall >= cutoff, 1, 0
      ),
      # Re-calculate SA indexes according to published project-specific rules
      # Georgi et al. (2024), <https://doi.org/10.29364/epsy.493>, Table 1
      # Georgi et al. (2026), <https://doi.org/10.1016/j.actpsy.2026.106478>, Data analysis
      `SA-TMT-B-final` = dplyr::case_when(
        Education == "lower"  & TMT_B_time >  249 ~ 0,
        Education == "lower"  & TMT_B_time <= 249 ~ 1,
        Education == "higher" & TMT_B_time >  223 ~ 0,
        Education == "higher" & TMT_B_time <= 223 ~ 1,
        .default = NA_real_
      ),
      `SA-BNT-final` = dplyr::case_when(
        Education == "lower"  & Spon_sem <  19 ~ 0,
        Education == "lower"  & Spon_sem >= 19 ~ 1,
        Education == "higher" & Spon_sem <  23 ~ 0,
        Education == "higher" & Spon_sem >= 23 ~ 1,
        .default = NA_real_
      ),
      `SA-VF-final` = dplyr::case_when(
        Education == "lower"  & VF_animals <  12 ~ 0,
        Education == "lower"  & VF_animals >= 12 ~ 1,
        Education == "higher" & VF_animals <  13 ~ 0,
        Education == "higher" & VF_animals >= 13 ~ 1,
        .default = NA_real_
      ),
      `SA-memory-final` = dplyr::case_when(
        Education == "lower"  & `Memory test` == "PVLT"  & RAVLT_delayed_recall <  10 ~ 0,
        Education == "lower"  & `Memory test` == "PVLT"  & RAVLT_delayed_recall >= 10 ~ 1,
        Education == "higher" & `Memory test` == "PVLT"  & RAVLT_delayed_recall <  11 ~ 0,
        Education == "higher" & `Memory test` == "PVLT"  & RAVLT_delayed_recall >= 11 ~ 1,
        Education == "lower"  & `Memory test` == "RAVLT" & RAVLT_delayed_recall <   8 ~ 0,
        Education == "lower"  & `Memory test` == "RAVLT" & RAVLT_delayed_recall >=  8 ~ 1,
        Education == "higher" & `Memory test` == "RAVLT" & RAVLT_delayed_recall <  11 ~ 0,
        Education == "higher" & `Memory test` == "RAVLT" & RAVLT_delayed_recall >= 11 ~ 1,
        .default = NA_real_
      ),
      # Needs at least the cut-off in a memory task
      # and at least average performance in all remaining tasks
      # to be labelled SA = 1
      SA = factor(dplyr::if_else(
        (`SA-TMT-B-new` + `SA-BNT-new` + `SA-VF` + Delayed_recall_SA) == 4,
        true = 1,
        false = 0
      )),
      SA_comp = factor(dplyr::if_else(
        (`SA-TMT-B-final` + `SA-BNT-final` + `SA-VF-final` + `SA-memory-final`) == 4,
        true = 1,
        false = 0
      )),
      Z_SA = rowMeans(
        dplyr::across(tidyselect::starts_with("Z_")),
        na.rm = TRUE
      ),
      cPA = factor(dplyr::if_else(
        `Regular-PA` == 1, 1, 0
      )),
      Profession = factor(dplyr::case_when(
        Type_of_prevailing_occupation_during_life == 1 ~ "manual",
        Type_of_prevailing_occupation_during_life == 2 ~ "mostly manual",
        Type_of_prevailing_occupation_during_life == 3 ~ "mostly mental",
        Type_of_prevailing_occupation_during_life == 4 ~ "mental"
      )),
      Status = factor(dplyr::case_when(
        Marital_status == 1 ~ "Non-married", # "Single",
        Marital_status == 2 ~ "Married/partnership",
        Marital_status == 3 ~ "Non-married", # "Widowed",
        Marital_status == 4 ~ "Non-married"  #"Divorced"
      )),
      Depr = factor(dplyr::if_else(
        GDS15 > 5, 1, 0
      )),
      Anx = factor(dplyr::if_else(
        GAI > 10, 1, 0
      )),
      dplyr::across(
        .cols = tidyselect::all_of(c("Do_you_smoke?", "Hypertension", "Diabetes")),
        .fns = \(x) factor(2 - x)
      )
    ) |>
    # Re-naming variables:
    dplyr::rename(
      "ID" = "ID...1",
      "A2PA" = "PA...4",
      "Total_MA" = "Total-mental-activities",
      "Delayed_recall_raw" = "RAVLT_delayed_recall",
      "Delayed_recall_z" = "Z_RAVLT_PVLT_delayed_recall",
      "Delayed_reacall_SA_comp" = "SA-memory-final",
      "TMT_B_raw" = "TMT_B_time",
      "TMT_B_z" = "Z_TMT_B_uds",
      "TMT_B_SA" = "SA-TMT-B-new",
      "TMT_B_SA_comp" = "SA-TMT-B-final",
      "BNT_30_raw" = "Spon_sem",
      "BNT_30_z" = "Z_BNT_new",
      "BNT_30_SA" = "SA-BNT-new",
      "BNT_30_SA_comp" = "SA-BNT-final",
      "VF_Animals_raw" = "VF_animals",
      "VF_Animals_z" = "Z_VF_uds",
      "VF_Animals_SA" = "SA-VF",
      "VF_Animals_SA_comp" = "SA-VF-final",
      "Smoking" = "Do_you_smoke?"
    ) |>
    dplyr::select(
      ID, A2PA, mPA, Cosactiw, Age, Age_bin, Education,
      SA, cPA, Z_SA, MMSE, GDS15, GAI, FAQ, Depr, Anx,
      Total_MA, Health, Profession, Status,
      cutoff,
      tidyselect::ends_with("_raw"),
      tidyselect::ends_with("_z"),
      tidyselect::ends_with("_SA"),
      tidyselect::ends_with("_comp"),
      Smoking, Hypertension, Diabetes
    )
}

