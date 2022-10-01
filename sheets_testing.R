#testing integration with Google Sheets based on this tutorial
#https://www.jdtrat.com/blog/connect-shiny-google/

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = FALSE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "/.secrets"
)

googledrive::drive_auth()

googlesheets4::gs4_auth()

googlesheets4::gs4_create(name = "your-sheet-name", 
                          # Create a sheet called main for all data to 
                          # go to the same place
                          sheets = "main")
