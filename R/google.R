
auth_google_sheets <- function() {

  # email and cache sourced from project-level .Rprofile
  googlesheets4::gs4_auth(
    email = gargle::gargle_oauth_email(),
    cache = gargle::gargle_oauth_cache()
  )

}

get_sheet <- function(sheet_name) {

  auth_google_sheets()

  wb_id <- "1DrQduf2C0muSsZFNJI5y9Ljtr3qKYNacC9YRkI8NiQs"

  googlesheets4::read_sheet(wb_id, sheet = sheet_name)

}

get_sheet("skills")
