#' Draw a directed acyclic graphs
#'
#' Takes in sets of covariates derived by \code{adjustment_table}
#' and changes them to equations that can be used as \code{formula}.
#'
#' @param plot Logical. Should ggplot be returned (`TRUE`, default) or
#'   tidy_daggity object (`FALSE`).
#'
#' @returns Directed acyclic graph representing causal assumptions
#'   for the analysis.
#'
#' @export
draw_dag <- function(plot = TRUE) {
  # Node labels and coordinates:
  nms <- data.frame(
    name  = c("S", "Age", "Education", "mPA", "cPA", "Cognition", "Affect"),
    label = c("S", "Age", "Educ.", "m-PA", "c-PA", "Cogn.", "Affect"),
    x = c(2, 3, 2, 1, 1, 3, 2),
    y = c(0, 1, 1, 1, 3, 3, 3)
  ) |>
    dplyr::mutate(
      colour = dplyr::if_else(name %in% paste0("S", 1:3), "black", "white")
    )
  # Prepare the DAG:
  dag <- ggdag::dagify(
    Affect ~ mPA + cPA + Age + Education,
    Cognition ~ mPA + cPA + Education + Affect + Age,
    cPA ~ mPA + Age + Education,
    mPA ~ Education,
    S ~ mPA + Education + Age,
    latent = "S",
    coords = nms[, c("name","x","y")]
  ) |>
    ggdag::tidy_dagitty() |>
    dplyr::arrange(name) |>
    dplyr::mutate(
      selection = dplyr::if_else(name == "S", "1", "0"),
      curve = dplyr::if_else(
        is.na(direction), NA, dplyr::if_else(name == "cPA" & to == "Cognition", 0.60, 0)
      )
    )
  # Basic DAG plot:
  plt <- dag |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      shape = selection,
      colour = selection
    ) +
    ggdag::geom_dag_point(
      size = 20,
      fill = "white",
      stroke = 1
    ) +
    ggdag::geom_dag_edges_arc(
      curvature = na.omit(dag$data$curve),
      arrow = grid::arrow(length = grid::unit(11, "pt"), type = "open")
    ) +
    ggplot2::scale_shape_manual(values = c(`1` = 22, `0` = 21)) +
    ggplot2::scale_colour_manual(values = c("white", "black")) +
    ggdag::geom_dag_text(
      label = dplyr::arrange(nms, name)$label,
      color = "black",
      size = 5.33
    ) +
    ggdag::theme_dag() +
    ggplot2::theme(legend.position = "none")
  # Return it:
  if (plot) {
    plt
  } else {
    dag
  }
}
