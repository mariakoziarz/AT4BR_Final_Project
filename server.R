library(shiny)


aminoacids <- data.frame(
  name = c("Alanine", "Cysteine", "Aspartic acid", "Glutamic acid", "Phenylalanine", 
            "Glycine", "Histidine", "Isoleucine", "Lysine", "Leucine",
            "Methionine", "Asparagine", "Proline", "Glutamine", "Arginine",
            "Serine", "Threonine", "Valine", "Tryptophan", "Tyrosine"),
  oneletter_code = c("A", "C", "D", "E", "F", 
                    "G", "H", "I", "K", "L", 
                    "M", "N", "P", "Q", "R", 
                    "S", "T", "V", "W", "Y"),
  threeletter_code = c("Ala", "Cys", "Asp", "Glu", "Phe", 
                   "Gly", "His", "Ile", "Lys", "Leu", 
                   "Met", "Asn", "Pro", "Gln", "Arg", 
                   "Ser", "Thr", "Val", "Trp", "Tyr")
)

server <- function(input, output, session) {
 values <- reactiveValues(
    started = FALSE,
    current_question = 1,
    total_questions = 10,
    correct_answers = 0,
    no_repeats = c(),
    answers = data.frame(question = character(10), answer = character(10), right_answer = character(10))
    

 )
 output$instructions <- renderUI({
   if(!values$started){
     h4("Hello")
   }
 })
 
 observeEvent(input$start,{
   values$started <- TRUE
   values$current_question <- 1
   values$correct_answers <- 0
   values$ answers <- data.frame(question = character(10), answer = character(10), right_answer = character(10))
   output$game <- renderUI({
     fluidPage(
       uiOutput("question"),
       textInput("answer", "Your answer: "),
       actionButton("submit", "Submit")
     )
     
   })
})
 
 output$question <- renderUI({
   random_number <- runif(1, 1, 20)
   if(random_number %in% values$no_repeats){
     while (random_number %in% values$no_repeats) {
       random_number <- runif(1, 1, 20)
     }
     append(values$no_repeats, random_number)
   }
   if(input$mode == "Structure"){
     
     renderImage({
       filename <- normalizePath(file.path('./amino_acids/', 
                                           paste(aminoacids$threeletter_code[random_number], '.png', sep = '')))
     })
   } else if(input$mode == "One-letter code"){
     h3(aminoacids$oneletter_code[random_number])
   }else{
     h3(aminoacids$threeletter_code[random_number])
   }
 }
 )
 
}


