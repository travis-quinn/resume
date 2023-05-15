library(magrittr)
functions_to_source <- list.files("R", full.names = TRUE)
for (f in functions_to_source) source(f)

DATA <- qs::qread("cached_data.qs")
skills_data <- clean_skills(DATA$skills)

cache_exists <- file.exists("cached_data.qs")
vars_exists <- file.exists("_variables.yml")
heatmap_exists <- file.exists("images/heatmap.svg")
necessary_files_exist <- all(cache_exists,
                             vars_exists,
                             heatmap_exists)