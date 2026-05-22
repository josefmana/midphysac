# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set()

# Load all in-house functions:
tar_source()

# List the targets:
list(
  targets_assumptions,
  targets_norms,
  targets_data,
  targets_regressions,
  targets_results
)
