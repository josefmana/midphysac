# ==============================================================
#  PA Pipeline — COSACTIW + KOKOSA
#  Outputs: PA_combined_output.xlsx (4 sheets)
#
#  This script was generated with the assistance of Claude.ai
#  (Anthropic, claude.ai) based on the PA coding rules derived
#  from the COSACTIW dataset and applied to the KOKOSA (COBRA B)
#  dataset. The coding rules, activity mappings and exclusion
#  lists were extracted iteratively from the hand-coded data in
#  dialogue with Claude Sonnet (claude.ai, May 2026).
#
#  Rules summary:
#    WHO-PA = 1 if sum of Hours_per_week for moderate/intense
#             activities >= 2.5 h/week (all activity types)
#    Regular-PA-without walking, chores = 1 if sum of
#             Hours_per_week for non-excluded activities > 0
#             (excluded: walking, fast_walking, dog_walking,
#              household_chores, cottage_work, care_for_sister,
#              care_for_husband, mushroom_picking, shopping)
#    Regular-PA-without walking, chores, gardening = same but
#             additionally excluding gardening
#  Flags are placed on the first activity row of each person;
#  all subsequent rows for that person receive NA.
# ==============================================================

# ── Packages (install once if needed) ─────────────────────────
# install.packages(c("readxl", "dplyr", "writexl"))

library(readxl)
library(dplyr)
library(writexl)

# ── 0. File paths ─────────────────────────────────────────────
cosactiw_path <- here::here("data-raw", "COSACTIW_NANOK_KOKOSA.xlsx") # original COSACTIW workbook
kokosa_path   <- here::here("data-raw", "KOKOSA_dbf_final.xlsx")      # KOKOSA (COBRA B) workbook
cobra_sheet   <- "6COBRA-B-PA"
output_path   <- here::here("data-raw", "PA_combined_output.xlsx")

# ── 1. Exclusion lists ────────────────────────────────────────
walk_excl <- c("walking", "fast_walking", "dog_walking")
# Note: hiking and nordic_walking are NOT excluded (sport activities)

chores_excl <- c("household_chores", "cottage_work",
                 "care_for_sister",  "care_for_husband",
                 "mushroom_picking",  "shopping")

# ── 2. Activity mapping (KOKOSA raw → COBRA standard) ─────────
activity_map <- c(
  # walking
  "walking"                        = "walking",
  "Walking"                        = "walking",
  "walks"                          = "walking",
  "walks with husband"             = "walking",
  "light walk"                     = "walking",
  "light walking"                  = "walking",
  "light_walking"                  = "walking",
  "slow walking"                   = "walking",
  "city_walks"                     = "walking",
  "walking/trips"                  = "walking",
  "Walking on stairs - 10 floors"  = "walking",
  # fast_walking
  "brisk_walk"                     = "fast_walking",
  "brisk walking"                  = "fast_walking",
  "brisk_walks"                    = "fast_walking",
  "fast walking"                   = "fast_walking",
  "Fast walking"                   = "fast_walking",
  "faster walking"                 = "fast_walking",
  "cross-country _walking"         = "fast_walking",
  # hiking
  "hiking"                         = "hiking",
  "trekking"                       = "hiking",
  # dog_walking
  "dog_walking"                    = "dog_walking",
  "dog walking"                    = "dog_walking",
  "walking_the_dog"                = "dog_walking",
  # mushroom_picking
  "walks_(mushroom_picking)"       = "mushroom_picking",
  # gardening
  "gardening"                      = "gardening",
  "Gardening"                      = "gardening",
  "gardenning"                     = "gardening",
  "light_gardening"                = "gardening",
  "light gardening"                = "gardening",
  "intensive_gardening"            = "gardening",
  "growing"                        = "gardening",
  "watering plants"                = "gardening",
  # household_chores
  "household_chores"               = "household_chores",
  "Household_chores"               = "household_chores",
  "household chores"               = "household_chores",
  "household_ chores"              = "household_chores",
  "Husehold_chores"                = "household_chores",
  "housekeeping"                   = "household_chores",
  "housework"                      = "household_chores",
  "house keeping"                  = "household_chores",
  "house choires"                  = "household_chores",
  "light housework"                = "household_chores",
  "cleaning"                       = "household_chores",
  "cooking"                        = "household_chores",
  "cooking "                       = "household_chores",
  "household/garden chores"        = "household_chores",
  "household_chores_and_gardening" = "household_chores",
  # cottage_work
  "house decorating"               = "cottage_work",
  "recontruction"                  = "cottage_work",
  "painting"                       = "cottage_work",
  # exercise
  "exercise"                       = "exercise",
  "exercise "                      = "exercise",
  "excercise"                      = "exercise",
  "Excersice"                      = "exercise",
  "light exercise"                 = "exercise",
  "light_excercise"                = "exercise",
  # health_exercise
  "health exercises"               = "health_exercise",
  # rehabilitation
  "rehabilitation_exercises"       = "rehabilitation_exercises",
  # cycling / rotoped
  "cycling"                        = "bike",
  "cycling "                       = "bike",
  "excercise_bike"                 = "rotoped",
  "treadmill"                      = "rotoped",
  # swimming
  "swimming"                       = "swimming",
  "light swimming"                 = "swimming",
  # sports
  "volleyball"                     = "volleyball",
  "ping pong"                      = "pingpong",
  "pingpong"                       = "pingpong",
  # mind-body
  "yoga"                           = "yoga",
  "joga"                           = "yoga",
  "stretching"                     = "stretching",
  "dance"                          = "dancing",
  # animal care / other
  "animal_care"                    = "feeding_animals",
  "pet care"                       = "feeding_animals",
  "shopping"                       = "shopping",
  "babysitting"                    = "babysitting"
)

# ── 3. Intensity mapping (KOKOSA raw → standard) ──────────────
intensity_map <- c(
  "moderate"  = "moderate", "Moderate"  = "moderate",
  "light"     = "light",    "Light"     = "light",
  "intenzive" = "intense",  "hard"      = "intense",
  "intense"   = "intense"
)

# ── 4. PA calculation function ────────────────────────────────
compute_pa <- function(df) {
  per_person <- df %>%
    group_by(ID) %>%
    summarise(
      mod_hrs  = sum(Hours_per_week[Intensity %in% c("moderate", "intense")],
                     na.rm = TRUE),
      hrs_nwc  = sum(Hours_per_week[!Activity %in% c(walk_excl, chores_excl)],
                     na.rm = TRUE),
      hrs_nwcg = sum(Hours_per_week[!Activity %in% c(walk_excl, chores_excl, "gardening")],
                     na.rm = TRUE),
      has_hrs  = any(!is.na(Hours_per_week)),
      .groups  = "drop"
    ) %>%
    mutate(
      `WHO-PA` = case_when(
        !has_hrs       ~ NA_real_,
        mod_hrs >= 2.5 ~ 1,
        TRUE           ~ 0
      ),
      `Regular-PA-without walking, chores` = case_when(
        !has_hrs    ~ NA_real_,
        hrs_nwc > 0 ~ 1,
        TRUE        ~ 0
      ),
      `Regular-PA-without walking, chores, gardening` = case_when(
        !has_hrs     ~ NA_real_,
        hrs_nwcg > 0 ~ 1,
        TRUE         ~ 0
      )
    )

  df %>%
    group_by(ID) %>%
    mutate(row_in_group = row_number()) %>%
    ungroup() %>%
    left_join(
      per_person %>%
        select(ID, `WHO-PA`,
               `Regular-PA-without walking, chores`,
               `Regular-PA-without walking, chores, gardening`),
      by = "ID"
    ) %>%
    mutate(
      `WHO-PA` = if_else(row_in_group == 1, `WHO-PA`, NA_real_),
      `Regular-PA-without walking, chores` =
        if_else(row_in_group == 1,
                `Regular-PA-without walking, chores`, NA_real_),
      `Regular-PA-without walking, chores, gardening` =
        if_else(row_in_group == 1,
                `Regular-PA-without walking, chores, gardening`, NA_real_)
    ) %>%
    select(-row_in_group)
}

# ── 5. Load & prepare COSACTIW ────────────────────────────────
cosactiw_raw <- read_excel(cosactiw_path, cobra_sheet, na = "NA")

cosactiw_clean <- cosactiw_raw %>%
  mutate(
    # ID as zero-padded character (preserve leading zeros)
    ID        = formatC(as.integer(ID), width = 4, flag = "0"),
    Activity  = trimws(Activity),
    Intensity = tolower(trimws(Intensity))
  ) %>%
  select(-`WHO-PA`,
         -`Regular-PA-without walking, chores`,
         -`Regular-PA-without walking, chores, gardening`)

cosactiw_out <- compute_pa(cosactiw_clean)

# ── 6. Load & prepare KOKOSA ──────────────────────────────────
kokosa_raw <- read_excel(kokosa_path, cobra_sheet, na = "NA")

kokosa_out <- kokosa_raw %>%
  mutate(
    # ID as character (already 6 digits; preserve as-is)
    ID             = as.character(ID),
    Intensity      = intensity_map[trimws(as.character(Intensity))],
    Activity       = activity_map[trimws(as.character(Activity))],
    Hours_per_week = suppressWarnings(
      as.numeric(gsub("\\+", "", trimws(as.character(Hours_per_week))))
    ),
    Times_per_week = suppressWarnings(as.numeric(Times_per_week))
  ) %>%
  compute_pa()

# ── 7. Identify COSACTIW discrepancies ────────────────────────
pa_cols <- c("WHO-PA",
             "Regular-PA-without walking, chores",
             "Regular-PA-without walking, chores, gardening")

orig_flags <- cosactiw_raw %>%
  filter(!is.na(`WHO-PA`)) %>%
  mutate(ID = formatC(as.integer(ID), width = 4, flag = "0")) %>%
  select(ID, all_of(pa_cols)) %>%
  rename_with(~ paste0(., "_original"), all_of(pa_cols))

calc_flags <- cosactiw_out %>%
  filter(!is.na(`WHO-PA`)) %>%
  select(ID, all_of(pa_cols)) %>%
  rename_with(~ paste0(., "_calculated"), all_of(pa_cols))

comparison <- left_join(orig_flags, calc_flags, by = "ID")

discrepant_ids <- comparison %>%
  filter(
    `WHO-PA_original` != `WHO-PA_calculated` |
    `Regular-PA-without walking, chores_original` !=
      `Regular-PA-without walking, chores_calculated` |
    `Regular-PA-without walking, chores, gardening_original` !=
      `Regular-PA-without walking, chores, gardening_calculated`
  ) %>%
  pull(ID)

cat("Discrepant COSACTIW IDs:", paste(discrepant_ids, collapse = ", "), "\n")

# Sheet 4: discrepant participants — all rows + original flags for comparison
sheet4 <- cosactiw_out %>%
  filter(ID %in% discrepant_ids) %>%
  left_join(
    cosactiw_raw %>%
      filter(formatC(as.integer(ID), width = 6, flag = "0") %in% discrepant_ids) %>%
      mutate(ID = formatC(as.integer(ID), width = 6, flag = "0")) %>%
      select(ID, all_of(pa_cols)) %>%
      rename(
        WHO_PA_original     = `WHO-PA`,
        RegPA_nwc_original  = `Regular-PA-without walking, chores`,
        RegPA_nwcg_original = `Regular-PA-without walking, chores, gardening`
      ),
    by = "ID"
  )

# ── 8. Sheet 1: combined summary ──────────────────────────────
sheet1 <- bind_rows(
  cosactiw_out %>%
    filter(!is.na(`WHO-PA`)) %>%
    select(ID, all_of(pa_cols)) %>%
    mutate(study_id = "COSACTIW", .after = ID),
  kokosa_out %>%
    filter(!is.na(`WHO-PA`)) %>%
    select(ID, all_of(pa_cols)) %>%
    mutate(study_id = "KOKOSA", .after = ID)
)

# ── 9. Save ───────────────────────────────────────────────────
write_xlsx(
  list(
    "Summary"                = sheet1,
    "COSACTIW_full"          = cosactiw_out,
    "KOKOSA_full"            = kokosa_out,
    "COSACTIW_discrepancies" = sheet4
  ),
  path = output_path
)

cat("Saved:", output_path, "\n")
cat("Sheet1 rows:", nrow(sheet1),
    " | Sheet2:", nrow(cosactiw_out),
    " | Sheet3:", nrow(kokosa_out),
    " | Sheet4:", nrow(sheet4), "\n")
cat("ID type in COSACTIW output:", class(cosactiw_out$ID), "\n")
cat("ID type in KOKOSA output:  ", class(kokosa_out$ID),   "\n")
cat("Sample COSACTIW IDs:", head(unique(cosactiw_out$ID), 5), "\n")
cat("Sample KOKOSA IDs:  ", head(unique(kokosa_out$ID),   5), "\n")
