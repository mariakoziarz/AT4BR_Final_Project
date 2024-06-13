library(shiny)

#user interface
ui <- fluidPage(
  
  titlePanel("Amino Master"),
  
  sidebarLayout(
    sidebarPanel(
      #initializing input panel
      selectInput(
        "mode",
        label = "Choose the game mode",
        choices = c("...","Structure", "One-letter code", "Three-letter code"),
        selected = "...",
      ),
      actionButton("start", "Start game")
      
    ),
    #what is happening on main panel
     mainPanel(
       uiOutput("instructions"),
       uiOutput("game"),
       textOutput("score")
       
     )
  )
)
