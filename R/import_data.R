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
#' @returns A tibble with following columns:
#'
#' @export
import_data <- function(
    file,
    sheet,
    norms,
    thresholds,
    thres_type = "mean") {
  # Read the file:
  readxl::read_xlsx(file, sheet = sheet, na = "NA") |>
    # Keep variables of interest:
    dplyr::select(
      1, Study, Age, `Education-2-cat`, Type_of_prevailing_occupation_during_life, Marital_status, # predictors
      `Regular-PA`, MMSE, GDS15, GAI, FAQ, `Total-mental-activities`, Health, # outcomes
      RAVLT_delayed_recall, TMT_B_time, Spon_sem, VF_animals, # SuperAging variables raw scores
      `SA-TMT-B-new`, `SA-BNT-new`, `SA-VF`, # SuperAging indicators
      Z_RAVLT_PVLT_delayed_recall, Z_TMT_B_uds, Z_BNT_new, Z_VF_uds, # SuperAging variables z scores
      `Do_you_smoke?`, Hypertension, Diabetes # health-related covariates
    ) |>
    dplyr::mutate(
      mPA = factor(
        Study,
        labels = c("COSACTIW","NANOK")
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
          score <- as.integer(RAVLT_delayed_recall[i])
          M <- norms[ifelse(Study[i] == "NANOK", "pvlt", "ravlt"), "M", Education[i], Age_bin[i]]
          SD <- norms[ifelse(Study[i] == "NANOK", "pvlt", "ravlt"), "S", Education[i], Age_bin[i]]
          (score - M) / SD
        }),
        use.names = FALSE
      ),
      cutoff = unlist(
        sapply(seq_len(dplyr::n()), function(i) {
          # helper column showing thresholds for cognitive SA
          with(thresholds, {
            t <- dplyr::case_when(Study[i] == "NANOK" ~ "pvlt", Study[i] == "COSACTIW" ~ "ravlt")
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
      SA = factor(dplyr::if_else(
        # needs at least the cut-off in a memory task
        # and at least average performance in all remaining tasks
        # to be labelled SA = 1
        (`SA-TMT-B-new` + `SA-BNT-new` + `SA-VF` + Delayed_recall_SA) == 4,
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
      "Total_MA" = "Total-mental-activities",
      "Delayed_recall_raw" = "RAVLT_delayed_recall",
      "Delayed_recall_z" = "Z_RAVLT_PVLT_delayed_recall",
      "TMT_B_raw" = "TMT_B_time",
      "TMT_B_z" = "Z_TMT_B_uds",
      "TMT_B_SA" = "SA-TMT-B-new",
      "BNT_30_raw" = "Spon_sem",
      "BNT_30_z" = "Z_BNT_new",
      "BNT_30_SA" = "SA-BNT-new",
      "VF_Animals_raw" = "VF_animals",
      "VF_Animals_z" = "Z_VF_uds",
      "VF_Animals_SA" = "SA-VF",
      "Smoking" = "Do_you_smoke?"
    ) |>
    dplyr::select(
      ID, mPA, Cosactiw, Age, Age_bin, Education,
      SA, cPA, Z_SA, MMSE, GDS15, GAI, FAQ, Depr, Anx,
      Total_MA, Health, Profession, Status,
      cutoff, tidyselect::ends_with("_raw"), tidyselect::ends_with("_z"), tidyselect::ends_with("_SA"),
      Smoking, Hypertension, Diabetes
    )
}

