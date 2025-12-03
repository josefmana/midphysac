#' Perform propensity scores matching
#'
#' Performs propensity scores matching for specified estimand
#' using specified method and formula, including balance checks.
#' For now, estimand it fixed to "ATC."
#'
#' @param d0 Tibble or data frame. Raw data.
#' @param sets Tibble or data frame. Adjustment sets, ideally
#'   derived from a causal model such as DAG.
#' @param dist Character. Method of matching, pushed to
#'   `MatchIt::matchit()` via the `distance` argument.
#'   Defaults to `"glm"`.
#' @param link Character. Link for matching, pushed to
#'   `MatchIt::matchit()` via the `link` argument.
#'   Defaults to `"logit"`.
#'
#' @returns A list with four components:
#' \describe{
#'   \item{\code{data}}{List. Data with propensity scores
#'   weights added as a new column.}
#'   \item{\code{method}}{Character. Indicates method used
#'   for matching.}
#'   \item{\code{tabs}}{List. Tables with numerical balance
#'   checks.}
#'   \item{\code{plots}}{List. Figures with graphical balance
#'   checks.}
#' }
#'
#' @export
perform_matching <- function(
    d0,
    sets,
    dist = "glm",
    link = "logit"
    ) {
  # extract predictor/matching pairs
  X <- tibble::tibble(
    formula = unique(sets$matching),
    estimand = "ATC"
  )
  # dirty trick to add health relate covatiates ----
  X <- X |>
    dplyr::full_join(
      X |> dplyr::mutate(formula = glue::glue(
        "{formula} + Hypertension + Diabetes"
      )),
      by = dplyr::join_by(formula, estimand)
    )
  # Prepare data for health covariates analysis:
  d1 <- d0 |>
    dplyr::select(ID, mPA, Cosactiw, Age, Education, cPA, Hypertension, Diabetes) |>
    tidyr::drop_na()
  # Propensity scores matching:
  labs <- rlang::set_names(x = seq_len(nrow(X)), nm = X$formula)
  fit0 <- lapply(labs, function(i) {
    with(X, {
      if (stringr::str_detect(formula[i], "Hypertension")) {
        df <- d1
      } else {
        df <- d0
      }
      MatchIt::matchit(
        formula = as.formula(formula[i]),
        data = df,
        method = "full",
        distance = dist,
        link = link,
        estimand = estimand[i]
      )
    })
  })
  # Outcomes:
  figs <- lapply(labs, function(i) {
    nms <- rlang::set_names(
      x = strsplit(as.character(fit0[[i]]$formula)[[3]], split = " + ", fixed = TRUE)[[1]]
    )
    lapply(nms, function(x) {
      cobalt::bal.plot(
        fit0[[i]],
        var.name = x,
        which = "both",
        colors = c("navyblue","orange"),
        sample.names = c("Non-weighted", "Propensity score-weighted")
      ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = .5, face = "bold")
        )
    })
  })
  tables <- lapply(labs, \(i) summary(fit0[[i]]))
  data <- lapply(labs, \(i) MatchIt::match.data(fit0[[i]]))
  method <- glue::glue("{dist} ({link} link)")
  list(
    data = data,
    method = method,
    tabs = tables,
    plots = figs
  )
}
