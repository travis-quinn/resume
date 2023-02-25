
auth_google_sheets <- function() {

  # email and cache sourced from project-level .Rprofile
  googlesheets4::gs4_auth(
    email = gargle::gargle_oauth_email(),
    cache = gargle::gargle_oauth_cache()
  )

}



get_sheet <- function(wb_id, sheet_name) {

  auth_google_sheets()

  googlesheets4::read_sheet(wb_id, sheet = sheet_name)

}



get_data <- function(wb_id) {

  sheets <- googlesheets4::sheet_names(wb_id)

  rlang::set_names(sheets) %>%
    purrr::map(~ get_sheet(wb_id, .x))

}
