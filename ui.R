library(shiny)


ui <- fluidPage(

  titlePanel("Amino Master"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "mode",
        label = "Choose a game mode",
        choices = c("mode","Structure", "One-letter code", "Three-letter code"),
        selected = "mode",
      ),
      actionButton("start", "Start game")
    ),
     mainPanel(
       uiOutput("game"),
       uiOutput("question"),
       tableOutput("results")
     )
  )
)
