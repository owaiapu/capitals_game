#capital guessing game

library(shiny)
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
library(sf)
library(sp)
library(dplyr)
library(rgdal)
library(DT)
library(googledrive)
library(googlesheets4)
library(shinydashboard)

source("events.R")

#shapefile and data from here: https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0/data?geometry=168.214%2C-42.875%2C-17.763%2C60.022

#set options to auto-authenticate to google sheets
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = "owtmcmillan.apps@gmail.com",
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)
drive_auth(cache = ".secrets", email = "owtmcmillan.apps@gmail.com")

#source capitals
capitals_df <- read.csv("capitallist.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
#notable wrongs - mostly triggers for cities that people think are capitals but aren't
notable_wrongs <- read.csv("notablewrongs.csv", fileEncoding = "UTF-8-BOM")
#acceptables - converts "acceptable" guesses to correct. Mostly cities with accents or special characters
acceptables <- read.csv("acceptables.csv", fileEncoding = "latin1")

#source leaderboard from google sheets
leaderboard_id <- drive_get("000_leaderboard")$id
leaderboard <- read_sheet(ss = leaderboard_id, sheet = "main")

#readOGR doesn't work on shinyapps io because it requires a local path
# my_spdf <- readOGR( 
#   dsn= paste0(getwd(), "/countries2") , 
#   layer="World_Countries__Generalized_",
#   verbose=FALSE,
#   encoding = "UTF-8-BOM",
#   stringsAsFactors = FALSE
# ) 

my_spdf <- st_read("countries2/World_Countries__Generalized_.shp")

my_spdf <- my_spdf[my_spdf$FID %in% capitals_df$FID,]

my_spdf_init <- my_spdf

guesses = c()

pal <- colorBin(c("#FFDDDD", "#1A2219", "#B7A300"), domain = c(0,2))

#intialise values
current_lat <- -90

#countries out will become a 1 when we've passed their latitude
countries_out <- rep(0, times = 195)
#cities, countries and latitudes are invisible until you pass their latitude
capital_names_out <- rep("???", times = 195)
country_names_out <- rep("???", times = 195)
latitudes_out <- rep("???", times = 195)

#this code make it press the 'submit' button when you hit enter
jscode <- '
$(function() {
var $els = $("[data-proxy-click]");
$.each(
$els,
function(idx, el) {
var $el = $(el);
var $proxy = $("#" + $el.data("proxyClick"));
$el.keydown(function (e) {
if (e.keyCode == 13) {
$proxy.click();
}
});
}
);
});
'
#this code is meant to auto-redirect to cursor to the input box after you hit enter but it doesn't work
jscode2 <- 'Shiny.addCustomMessageHandler("refocus",
                                  function(NULL) {
                                    document.getElementById("guess").focus();
                                  });'

ui <- fluidPage(
  includeCSS("funfact_style.css"),
  tags$head(tags$script(HTML(jscode))),
  tags$head(tags$script(
    'Shiny.addCustomMessageHandler("refocus",
                                  function(NULL) {
                                    document.getElementById("guess").focus();
                                  });'
  )),
  `data-proxy-click` = "submit_lat",
  navbarPage("Capitals Game",
             tabPanel("How to Play",
                      column(5,
                             h2("How to play"),
                      p("The aim of the game is to score as many points as possible, by naming as many capital cities as you can."),
                      p("However, you must move up the globe - ", span("once a capital is named, all other capitals to its south are eliminated.", style = "font-weight:bold")),
                      p("The table on the right shows the countries and capitals which are eliminated (gold means that you got it!).
                        The text boxes at the bottom-center show you how many countries are remaining, and how many you missed with your last guess.
                        There are no penalties for incorrect guesses."),
                      p("That's it. Good luck!"),
                      h2("The Undo Button"),
                      p("You have ", span("one", style = "font-weight:bold"), "opportunity to undo a guess. If you enter a guess but are unhappy with how many countries were eliminated, you can undo it.
                        However, the undo must be done within", span("fifteen seconds", style = "font-weight:bold"), "of making your guess!"),
                      p("You cannot undo your first answer."),
                      h2("What even is a capital city or country?"),
                      p("It's complicated. I just found a map file online and am not smart enough to change it. There's no penalties for incorrect guesses, so just have a go and read about the various geopolitical disputes afterwards.
                         You may even discover some fun facts along the way...")
                      ),
                      column(7,
                             tags$img(src = "example_game.png", width = "90%"))
             ),
             tabPanel("Play",
                      column(9, leafletOutput("map", height = "60vh"),
                             column(4,
                                    tags$style(HTML('#submit_lat {margin-top: 30px}')),
                                    splitLayout(cellWidths = c("60%", "40%"),
                                                uiOutput("text_input"),
                                                actionButton("submit_lat", "Submit")
                                    ),
                                    uiOutput("undo_button"),
                                    hr(),
                                    h4(textOutput("result_text"))
                             ),
                             column(4, 
                                    h1(htmlOutput("score_text")),
                                    h5(textOutput("countriesmissed_text"),
                                       hr(),
                                       h4(textOutput("countries_rem_text")),
                                       h4(textOutput("countries_missed_text"))
                                    )
                             ),
                             column(4,
                                    div(htmlOutput("fun_fact_textbox"), id = "funfact_header_css"),
                                    div(htmlOutput("game_end_prompt"))
                             )
                      ),
                      column(3, dataTableOutput("countriesout_table"))
             ),
             tabPanel("Leaderboard",
                      column(4, h2("Leaderboard"),
                             p("You cannot view previous games until yours is complete."), dataTableOutput("leaderboard")),
                      column(8, leafletOutput("leaderboard_game", height = "500px"),
                             tableOutput("leaderboard_game_table"))
             )
  )
)

server = function(input, output, session) {
  
  rv <- reactiveValues()
  #initialise reactive values
  rv$countries_out <- rep(0, times = 195)
  rv$capital_names_out <- rep("???", times = 195)
  rv$country_names_out <- rep("???", times = 195)
  rv$latitudes_out <- rep("???", times = 195)
  rv$longitudes_out <- rep("???", times = 195)
  rv$countriesout_new <- c()
  rv$prev_lat <- -90
  rv$current_lat <- -90
  rv$result_text <- "Let's Play!"
  rv$score <- 0
  rv$countries_missed <- 0
  rv$countries_rem <- 195
  rv$countriesout_df <- data.frame("Country" = c(), "Capital" = c(), "Latitude" = c(), "Longitude" = c())
  rv$correct_countries <- c()
  rv$all_guesses <- c()
  rv$miss_list <- c()
  rv$game_summary <- data.frame("Guess No." = c(), "Guess" = c(), "Countries Missed" = c())
  rv$undo_used <- FALSE
  rv$events <- c("", FALSE)
  rv$timer <- 15
  rv$active <- FALSE
  
  #convert leaderboard to reactive so that it updates when you add your score
  rv$leaderboard <- leaderboard
  
  #countdown timer for the 'undo'. observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(rv$active)
      {
        rv$timer <- rv$timer-1
        if(rv$timer<1)
        {
          rv$active <- FALSE
        }
      }
    })
  })
  
  observeEvent(input$submit_lat, {
    session$sendCustomMessage(type="refocus",message=list(NULL))
    lastchar = substr(input$guess, nchar(input$guess), nchar(input$guess))
    temp_guess <- ifelse(lastchar == " ",
                         substr(input$guess, 1, nchar(input$guess) -1),
                         input$guess) #phones tend to add a space at the end -> remove if present
    #check if guess is in 'acceptables' list and covert to correct answer if it is
    if(tolower(temp_guess) %in% tolower(acceptables$capital_input)){
      current_guess <- acceptables[tolower(acceptables$capital_input) == tolower(temp_guess),]$capital_corrected}else{
        current_guess <- temp_guess}
    
    #check if any special events triggered
    rv$events <- events_func(current_guess, rv$countries_rem)
    
    if(tolower(current_guess) %in% tolower(notable_wrongs$City)){ #trigger event if its a notable wrong
      rv$result_text <- paste(current_guess, " is not the capital. Try again.")}else{
        if(!(tolower(current_guess) %in% tolower(capitals_df$Capital))){ #otherwise just say it's wrong
          rv$result_text <- paste(current_guess, " is an incorrect guess. Try again.")}else{
            if(tolower(current_guess) %in% tolower(capitals_df$Capital) & 
               capitals_df$Latitude[tolower(capitals_df$Capital) == tolower(current_guess)] <= rv$current_lat){
              rv$result_text <- paste(current_guess, " is already out. Try again.")}else{
                if(tolower(current_guess) %in% tolower(capitals_df$Capital)){
                  #a correct guess! 
                  rv$result_text <- paste(current_guess, " is a correct guess!")
                  rv$correct_countries <- c(rv$correct_countries, capitals_df$Capital[tolower(capitals_df$Capital) == tolower(current_guess)])
                  rv$all_guesses <- c(rv$all_guesses, capitals_df$Capital[tolower(capitals_df$Capital) == tolower(current_guess)])
                  prev_rem <- rv$countries_rem
                  rv$current_lat <- capitals_df$Latitude[tolower(capitals_df$Capital) == tolower(current_guess)]
                  rv$score <- rv$score + 1
                  
                  rv$countries_out[capitals_df$Latitude <= rv$current_lat] <- 1
                  rv$countries_out[capitals_df$Capital %in% rv$correct_countries] <- 2
                  rv$country_names_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$COUNTRY[capitals_df$Latitude <= rv$current_lat]
                  rv$latitudes_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$Latitude[capitals_df$Latitude <= rv$current_lat]
                  rv$longitudes_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$Longitude[capitals_df$Latitude <= rv$current_lat]
                  rv$capital_names_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$Capital[capitals_df$Latitude <= rv$current_lat]
                  rv$countries_rem <- 195 - sum(rv$countries_out > 0)
                  rv$countries_missed <- prev_rem - rv$countries_rem - 1
                  rv$miss_list <- c(rv$miss_list, rv$countries_missed)
                  rv$countriesout_new <- capitals_df[capitals_df$Latitude <= rv$current_lat & capitals_df$Latitude > rv$prev_lat,]$COUNTRY
                  
                  rv$game_summary <- data.frame("Guess No." = seq(1, length(rv$all_guesses)),
                                                "Guess" = rv$all_guesses,
                                                "Countries Missed" = rv$miss_list)
                  
                  my_spdf$countries <- rv$country_names_out
                  my_spdf$latitudes<- rv$latitudes_out
                  my_spdf$capitals <- rv$capital_names_out
                  my_spdf$countries_out <- rv$countries_out
                  #my_spdf$countries_out_new <- rv$countries_out_new
                  
                  rv$countriesout_df <- data.frame("Country" = rv$country_names_out[rv$country_names_out != "???"],
                                                   "Capital" = rv$capital_names_out[rv$country_names_out != "???"],
                                                   "Latitude" = as.numeric(rv$latitudes_out[rv$country_names_out != "???"]),
                                                   "Longitude" = as.numeric(rv$longitudes_out[rv$country_names_out != "???"]))
                  rv$newlyout_df <- rv$countriesout_df[rv$countriesout_df$Country %in% rv$countriesout_new,]
                  rv$countriesout_df <- rv$countriesout_df[order(rv$countriesout_df$Latitude, decreasing = TRUE),]
                  
                  labels <- lapply(seq(nrow(my_spdf)), function(i) {
                    paste0( "<b>", my_spdf$countries[i], '</br>', 
                            "Capital: </b>", my_spdf$capitals[i], '</br>', 
                            "<b>Latitude: </b>", my_spdf$latitudes[i]) 
                  })
                  
                  leafletProxy("map") %>%
                    #removeShape(rv$countriesout_new) %>%
                    #clearMarkers() %>% %>%
                    
                    addPolygons(data = my_spdf,
                                weight = 1,
                                dashArray = "",
                                color = "black",
                                fillOpacity = 0.4,
                                layerId = my_spdf$COUNTRY,
                                fillColor = ~pal(countries_out),
                                highlight = highlightOptions(
                                  weight = 3,
                                  color = "#123",
                                  dashArray = "",
                                  fillOpacity = 0.8,
                                  bringToFront = FALSE),
                                label = lapply(labels, htmltools::HTML),
                                options = pathOptions(pane = "polygons")
                    ) %>%
                    addCircleMarkers(rv$newlyout_df$Longitude, rv$newlyout_df$Latitude, label = rv$newlyout_df$Capital,
                                     radius = 4, stroke = FALSE, fillOpacity = 0.9, color = "#363636",
                                     options = pathOptions(pane = "circles"))
                  
                  output$countriesout_table <- renderDataTable({
                    datatable(rv$countriesout_df[c("Country", "Capital", "Latitude")], options = list(pageLength = 50)) %>%
                      formatStyle("Capital", target = "row", backgroundColor = styleEqual(rv$correct_countries, rep("#f2f28f", times = length(rv$correct_countries))))
                  })
                  
                  output$game_summary_table <- renderTable({
                    rv$game_summary
                  })
                  
                  rv$prev_lat <- rv$current_lat
                  rv$active <- TRUE
                  rv$timer <- 15
                  
                }}
          }
      }
    #refocus the cursor but this doesn't work
    session$sendCustomMessage(type="refocus",message=list(NULL))
  }
  )
  
  observeEvent(input$undo, {
    session$sendCustomMessage(type="refocus",message=list(NULL))
    if(rv$active & !rv$undo_used & rv$score > 1){
      revert_to_input <- rv$correct_countries[length(rv$correct_countries)-1]
      rv$correct_countries <- rv$correct_countries[1:length(rv$correct_countries)-1]
      rv$all_guesses <- c(rv$all_guesses, "Undo")
      rv$miss_list <- c(rv$miss_list, -1* tail(rv$miss_list, n=1))
      rv$score <- rv$score -1 
      rv$result_text <- "Your undo has been used."
      rv$undo_used <- TRUE
      
      rv$game_summary <- data.frame("Guess No." = seq(1, length(rv$all_guesses)),
                                    "Guess" = rv$all_guesses,
                                    "Countries Missed" = rv$miss_list)
      
      rv$current_lat <- capitals_df$Latitude[tolower(capitals_df$Capital) == tolower(revert_to_input)]
      rv$prev_lat <- rv$current_lat
      
      rv$countries_out <- rep(0, times = 195)
      rv$capital_names_out <- rep("???", times = 195)
      rv$country_names_out <- rep("???", times = 195)
      rv$latitudes_out <- rep("???", times = 195)
      rv$longitudes_out <- rep("???", times = 195)
      
      rv$countries_out[capitals_df$Latitude <= rv$current_lat] <- 1
      rv$countries_out[capitals_df$Capital %in% rv$correct_countries] <- 2
      rv$country_names_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$COUNTRY[capitals_df$Latitude <= rv$current_lat]
      rv$latitudes_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$Latitude[capitals_df$Latitude <= rv$current_lat]
      rv$longitudes_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$Longitude[capitals_df$Latitude <= rv$current_lat]
      rv$capital_names_out[capitals_df$Latitude <= rv$current_lat] <- capitals_df$Capital[capitals_df$Latitude <= rv$current_lat]
      rv$countries_rem <- 195 - sum(rv$countries_out > 0)
      
      my_spdf$countries <- rv$country_names_out
      my_spdf$latitudes<- rv$latitudes_out
      my_spdf$capitals <- rv$capital_names_out
      my_spdf$countries_out <- rv$countries_out
      
      rv$countriesout_df <- data.frame("Country" = rv$country_names_out[rv$country_names_out != "???"],
                                       "Capital" = rv$capital_names_out[rv$country_names_out != "???"],
                                       "Latitude" = as.numeric(rv$latitudes_out[rv$country_names_out != "???"]),
                                       "Longitude" = as.numeric(rv$longitudes_out[rv$country_names_out != "???"]))
      rv$countriesout_df <- rv$countriesout_df[order(rv$countriesout_df$Latitude, decreasing = TRUE),]
      
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(data = my_spdf,
                    weight = 1,
                    dashArray = "",
                    color = "black",
                    fillOpacity = 0.4,
                    fillColor = ~pal(countries_out),
                    layerId = my_spdf$COUNTRY,
                    highlight = highlightOptions(
                      weight = 3,
                      color = "#123",
                      dashArray = "",
                      fillOpacity = 0.8,
                      bringToFront = FALSE),
                    label = ~paste0(countries, ", Capital: ", capitals, ", Latitude: ", latitudes),
                    options = pathOptions(pane = "polygons")
        ) %>%
        addCircleMarkers(rv$countriesout_df$Longitude, rv$countriesout_df$Latitude, label = rv$countriesout_df$Capital,
                         radius = 4, stroke = FALSE, fillOpacity = 0.7, color = "#363636",
                         options = pathOptions(pane = "circles"))
      
      output$countriesout_table <- renderDataTable({
        datatable(rv$countriesout_df[c("Country", "Capital", "Latitude")], options = list(pageLength = 50)) %>%
          formatStyle("Capital", target = "row", backgroundColor = styleEqual(rv$correct_countries, rep("#f2f28f", times = length(rv$correct_countries))))
      })
      
      output$game_summary_table <- renderTable({
        rv$game_summary
      })
      
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  output$countries_rem_text <- renderText(paste("Countries remaining: ", rv$countries_rem))
  output$countries_missed_text <- renderText(paste("Total countries missed: ", 195 - rv$countries_rem - rv$score))
  output$result_text <- renderText(paste(rv$result_text))
  output$score_text <- renderUI({
    return(div("Score: ", span(rv$score, style = "font-weight:bold; color:#4CC417")))
  })
  
  output$guess_text <- renderText(paste("Current latitude is ", rv$current_lat))
  output$countriesmissed_text <- renderText(paste("Previous guess missed: ", rv$countries_missed, " countries."))
  
  
  output$map <- renderLeaflet({
    m <- leaflet(my_spdf) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addMapPane("polygons", zIndex = 400) %>%
      addMapPane("circles", zIndex = 1000) %>%
      addPolygons(data = my_spdf,
                  weight = 1,
                  dashArray = "",
                  color = "black",
                  fillOpacity = 0.2,
                  fillColor = ~pal(countries_out),
                  layerId = my_spdf$COUNTRY,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#123",
                    dashArray = "",
                    fillOpacity = 0.5,
                    bringToFront = FALSE)
                  #label = ~paste0(country_known, ", Capital: ", capital_known, ", Latitude: ", latitude_known)
      )
  })
  
  
  output$text_input <- renderUI({
    times <- input$submit_lat
    textInput("guess", "Guess a capital:")
  })
  
  output$undo_button <- renderUI({
    if(!(rv$undo_used)){
      actionButton("undo", paste("Undo (", rv$timer, ")"), sep ="")}else{
        paste("Undo has been used.")
      }
  })
  
  output$fun_fact_textbox <- renderUI({
    if(rv$countries_rem == 0){
      HTML(paste(
        div("Game Over :) ", style = "font-size: 30px"),
        div("How did you go? Check out the leaderboard on the next page. ")
      ))
    }
    else if(rv$events[2]){
      HTML(paste(
        div("Fun Fact!", style = "font-size: 30px"),
        div(rv$events[1])
      ))
    } else {
      HTML(paste(
        div("")
      ))
    }
  })
  
  observeEvent(req(rv$countries_rem == 0), {
    output$game_end_prompt <- renderUI({
      HTML(paste(
        div("You can now view previous games from the leaderboard."),
        div("Would you like to enter your score on the leaderboard?"),
        textInput("leaderboard_name", "Name: "),
        actionButton("submit_name", "Submit")
      ))
    })
  })
  
  observeEvent(input$submit_name, {
    output$game_end_prompt <- renderUI({
      HTML(paste(
        div("Done :) How did you place?!")
      ))
    })
  })
  
  observeEvent(input$submit_name, {
    timecode <- paste(Sys.time())
    timecode <- gsub(':', '', timecode)
    timecode <- gsub(' ', '_', timecode)
    
    leaderboard_entry <- data.frame(
      "Score" = rv$score,
      "Date" = timecode,
      "Name" = input$leaderboard_name
    )
    #update leaderboard file
    rv$leaderboard <- rbind(rv$leaderboard, leaderboard_entry)
    rv$leaderboard <- rev(rv$leaderboard[order(rv$leaderboard$Score, decreasing = TRUE),])
    sheet_write(data = rv$leaderboard, ss = leaderboard_id, sheet = "main")
    #write.csv(rv$leaderboard, "leaderboard/000_leaderboard.csv", row.names = FALSE)
    
    #save reference file
    filepath_tosave <- paste(timecode, "_", input$leaderboard_name, sep = "")
    gs4_create(name = filepath_tosave, sheets = "main")
    referencefile_id <- drive_get(filepath_tosave)$id
    sheet_write(data = rv$game_summary, ss = referencefile_id, sheet = "main")
    #write.csv(rv$game_summary, filepath_tosave, row.names = FALSE)
    
    #re-output the leaderboard
    df$data <- data.frame(
      cbind(rv$leaderboard, 
            Actions = shinyInput(actionButton, length(rv$leaderboard$Score), 'button_', label = "View Game", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
      )
    )
    
    output$leaderboard <- renderDataTable(
      df$data, server = FALSE, escape = FALSE, selection = 'none'
    )
  }
  )
  
  rv$game_showing <- NA
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  df <- reactiveValues(data = data.frame(
    cbind(leaderboard, 
          Actions = shinyInput(actionButton, length(leaderboard$Score), 'button_', label = "View Game", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
    )
  ))
  
  output$leaderboard <- renderDataTable(
    df$data, server = FALSE, escape = FALSE, selection = 'none'
  )
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    rv$game_showing <<- paste(df$data[selectedRow,]$Date, "_", df$data[selectedRow,]$Name, sep = "")
  })
  
  output$leaderboard_game <- renderLeaflet({
    if(!is.na(rv$game_showing) && rv$game_showing != "" && rv$countries_rem == 0){
      #game_to_show <- read.csv(rv$game_showing, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
      game_to_show <- read_sheet(ss = drive_get(rv$game_showing)$id, sheet = "main")
      render_full_game(game_to_show, capitals_df, my_spdf_init)
    }
  })
  
  output$leaderboard_game_table <- renderTable({
    if(!is.na(rv$game_showing) && rv$game_showing != "" && rv$countries_rem == 0){
      read_sheet(ss = drive_get(rv$game_showing)$id, sheet = "main")
    }
  })
  
  
  render_full_game <- function(game_summary, capital_list, spdf){
    undo_location <- match("Undo", game_summary$Guess)
    if(!is.na(undo_location)){
      #remove the Undo and prior guess, if there is one
      guess_list <- game_summary[-c(undo_location, undo_location-1),]
    } else {
      guess_list <- game_summary
    }
    
    spdf$countries <- capitals_df$COUNTRY
    spdf$latitudes<- capitals_df$Latitude
    spdf$capitals <- capitals_df$Capital
    spdf$countries_out <- rep(1, times = 195)
    spdf$countries_out[capitals_df$Capital %in% game_summary$Guess] <- 2
    
    labels <- lapply(seq(nrow(spdf)), function(i) {
      paste0( "<b>", spdf$countries[i], '</br>', 
              "Capital: </b>", spdf$capitals[i], '</br>', 
              "<b>Latitude: </b>", spdf$latitudes[i]) 
    })
    
    m <- leaflet(spdf) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addMapPane("polygons", zIndex = 400) %>%
      addMapPane("circles", zIndex = 1000) %>%
      addPolygons(data = spdf,
                  weight = 1,
                  dashArray = "",
                  color = "black",
                  fillOpacity = 0.4,
                  fillColor = ~pal(countries_out),
                  layerId = spdf$COUNTRY,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#123",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = FALSE),
                  label = lapply(labels, htmltools::HTML),
                  options = pathOptions(pane = "polygons") 
      ) %>%
      addCircleMarkers(capital_list$Longitude, capital_list$Latitude, label = capital_list$Capital,
                       radius = 4, stroke = FALSE, fillOpacity = 0.9, color = "#363636",
                       options = pathOptions(pane = "circles"))
    m
  }
  
}

shinyApp(ui = ui, server = server)