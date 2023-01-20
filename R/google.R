
auth_google_sheets <- function() {

  # email and cache sourced from project-level .Rprofile
  googlesheets4::gs4_auth(
    email = gargle::gargle_oauth_email(),
    cache = gargle::gargle_oauth_cache()
  )

}
