library(shiny)


ui <- fluidPage(

  titlePanel("Amino Master"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "mode",
        label = "Choose the game mode",
        choices = c("...","Structure", "One-letter code", "Three-letter code"),
        selected = "...",
      ),
      actionButton("start", "Start game")
      
    ),
     mainPanel(
       uiOutput("instructions"),
       uiOutput("game"),
       
     )
  )
)
