library(googlesheets)

Grab_data <- function(variable){
  
  gs_mock_runway_title <- gs_title("Mock Runway Table")
  gs_MRT <- gs_read(gs_mock_runway_title, ws-1)
}


