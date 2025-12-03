# Set-up targets for results extraction:
targets_results <- list(
  targets::tar_target(
    results,
    command = stat_test(
      fits = models$fits,
      specs = specs,
      sets = adjustment_sets,
      CIs = TRUE
    )
  ),
  targets::tar_target(
    results_plot,
    command = plot_results(
      data,
      stats = results,
      specs = specs,
      type = "1",
      save = TRUE
    )
  ),
  targets::tar_target(
    cognition_plot,
    command = plot_results(
      data,
      stats = results,
      specs = specs,
      type = "2",
      save = TRUE
    )
  ),
  targets::tar_target(
    results_tables,
    command = table_results(results)
  )
)
