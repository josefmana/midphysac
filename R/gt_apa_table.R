#' Generate an APA-style table
#'
#' Converts a tibble or data frame into an
#' APA-formatted table using the gt package.
#' The resulting object inherits all the features
#' and customizations provided by \code{gt}.
#'
#' @param x A tibble or data frame to be formatted as
#'   an APA-style table.
#' @param grp An optional column name (as a string or
#'   symbol) to use for grouping rows via \code{gt::gt()}'s \code{groupname_col}
#'   parameter. Defaults to \code{NULL}.
#' @param nms An optional column name (as a string or
#'   symbol) to use for row names via \code{gt::gt()}'s \code{rowname_col}
#'   parameter. Defaults to \code{NULL}.
#' @param tit A character string specifying the table title.
#'   Will be converted to HTML format. Defaults to \code{""}.
#'
#' @returns A \code{gt} table object formatted according to APA style.
#'
#' @export
gt_apa_table <- function(x, grp = NULL, nms = NULL, tit = "") {
  x |> gt::gt(groupname_col = grp, rowname_col = nms) |>
    gt::tab_options(
      table.border.top.color = "white",
      heading.title.font.size = gt::px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(100),
      table.background.color = "white"
    ) |>
    gt::cols_align(align = "center") |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align = "center"
        ),
        gt::cell_fill(color = "white", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = tidyselect::everything(),
        rows = tidyselect::everything()
      )
    ) |>
    gt::tab_header( # title setup
      title = gt::html(tit)
    ) |>
    gt::opt_align_table_header(align = "left")
}
