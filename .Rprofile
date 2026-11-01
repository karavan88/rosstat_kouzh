# ==============================================================================
# R PROFILE - RUSSIAN STATISTICS PROJECT
# ==============================================================================
# Activates renv and loads the project configuration on R startup.
# ==============================================================================

# The renv sandbox lock lives in the user cache. Disable sandboxing here so
# startup does not hang when that cache is unavailable (for example in a
# restricted terminal environment).
options(renv.config.sandbox.enabled = FALSE)

source("renv/activate.R")

if (file.exists("project_config.R")) {
  source("project_config.R")
}

options(
  repos = c(CRAN = "https://cloud.r-project.org/"),
  warn = 1,
  scipen = 999,
  stringsAsFactors = FALSE,
  width = 120
)
