
write_to_vars <- function(DATA) {

  nested_list <- list(
    contact = clean_contact(DATA$contact),
    summary = clean_summary(DATA$summary),
    work = clean_work(DATA$work),
    work_other = clean_work_other(DATA$work_other),
    education = clean_education(DATA$education),
    interests = clean_interests(DATA$interests)
  )

  yaml::write_yaml(nested_list, "_variables.yml")

}



clean_summary <- function(df) {

  rlang::set_names(df$text,
                   nm = df$id) %>%
    as.list()

}



clean_contact <- function(df) {

  rlang::set_names(df$text,
                   nm = df$id) %>%
    as.list()

}



clean_work <- function(df) {

  work_cols <- names(df) %>%
    purrr::keep(~ stringr::str_detect(.x, "^work"))

  purrr::set_names(work_cols) %>%
    purrr::map(~ {
      df_work <- df %>%
        dplyr::select(id, all_of(.x)) %>%
        dplyr::filter(!is.na(.data[[.x]]))
      rlang::set_names(df_work[[.x]],
                       nm = df_work[["id"]]) %>%
        as.list()
    })

}



clean_work_other <- function(df) {

  ids <- c(paste0("title", seq.int(nrow(df))),
           paste0("company", seq.int(nrow(df))))

  rlang::set_names(c(df$title, df$company),
                   nm = ids) %>%
    as.list()

}



clean_education <- function(df) {

  rlang::set_names(df$text,
                   nm = df$id) %>%
    as.list()

}



# to be passed on to heatmap function
clean_hard_skills <- function(df) {

  # skip first row
  reshaped <- df[-1, ] %>%
    # convert to integer class
    dplyr::mutate(dplyr::across(-hard_skill,
                                ~ as.character(.x) %>%
                                  dplyr::na_if("NA") %>%
                                  as.integer())) %>%
    dplyr::rename(Software = hard_skill) %>%
    # reshape
    tidyr::pivot_longer(cols = -Software,
                        names_to = "Skill",
                        values_to = "Level",
                        values_transform = list(Level = as.integer))

  # create ordering for heatmap
  software_levels <- df[-1, ]$hard_skill
  skill_levels <- names(df)[-1]
  exp_level_levels <- c(5, 4, 3, 2, 1, 0, NA)

  # coerce all to factors
  reshaped %>%
    dplyr::mutate(Software = factor(Software,
                                    levels = rev(software_levels),
                                    ordered = TRUE),
                  Skill = factor(Skill,
                                 levels = skill_levels,
                                 ordered = TRUE),
                  Level = factor(Level,
                                 levels = exp_level_levels,
                                 ordered = TRUE))

}



clean_interests <- function(df) {

  rlang::set_names(df$text,
                   nm = df$id) %>%
    as.list()

}

clean_soft_skills <- function(df) {

  # skip first row
  reshaped <- df[-1, ] %>%
    # convert to integer class
    dplyr::mutate(dplyr::across(-soft_skill,
                                ~ as.character(.x) %>%
                                  dplyr::na_if("NA") %>%
                                  as.integer())) %>%
    dplyr::rename(Attribute = soft_skill) %>%
    # reshape
    tidyr::pivot_longer(cols = -Attribute,
                        names_to = "Skill",
                        values_to = "Level",
                        values_transform = list(Level = as.integer))

  # create ordering for heatmap
  attribute_levels <- df[-1, ]$soft_skill
  skill_levels <- names(df)[-1]
  exp_level_levels <- c(5, 4, 3, 2, 1, 0, NA)

  # coerce all to factors
  reshaped %>%
    dplyr::mutate(Attribute = factor(Attribute,
                                    levels = rev(attribute_levels),
                                    ordered = TRUE),
                  Skill = factor(Skill,
                                 levels = skill_levels,
                                 ordered = TRUE),
                  Level = factor(Level,
                                 levels = exp_level_levels,
                                 ordered = TRUE))

}



clean_interests <- function(df) {

  rlang::set_names(df$text,
                   nm = df$id) %>%
    as.list()

}




