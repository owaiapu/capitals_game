library(shiny)
library(shinydashboard)

# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(
    title = "page_title"
  ),
  dashboardSidebar(
  ),
  dashboardBody(tags$head(tags$script(
    'Shiny.addCustomMessageHandler("refocus",
                                  function(NULL) {
                                    document.getElementById("text_input1").focus();
                                  });'
  )),
  tabBox(title = "app_title",
         id = "id", width = "800px",
         tabPanel("tab1",
                  fluidRow(
                    column(width = 12,
                           box(
                             status = "info", solidHeader = TRUE,
                             collapsible = FALSE,
                             width = 12, 
                             title = "textInput", 
                             tags$textarea(id = "text_input1", rows =  2, cols = 50, "", autofocus = "autofocus"), 
                             fluidRow(
                               column(width = 12, 
                                      actionButton("actionbutton1", label = textOutput("actionbutton1_label"))
                               )))))),
         tabPanel("tab2"
         ))))

# Define server logic 

server <- function(input, output, session) {
  data1 <- eventReactive(input$actionbutton1, {
    
    substr(input$text_input1, 1, 1)
    
  })
  
  output$actionbutton1_label <- renderPrint(cat(noquote({substr(input$text_input1, 1, 1)})))
  
  observeEvent(input$actionbutton1,{
    
    isolate({
      if (data1() %in% c("a", "e", "i", "o", "u")) {
        updateTextInput(session, "text_input1",
                        value = paste(input$text_input1, "starts with a vowel", "", sep = " "))
      }
      else {
        updateTextInput(session, "text_input1",
                        value = paste(input$text_input1, "starts with a consonant", "", sep = " "))
      }
      
      session$sendCustomMessage(type="refocus",message=list(NULL))
      
    })
    
  })}
# Run the application 
shinyApp(ui = ui, server = server)