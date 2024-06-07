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
                   "Ser", "Thr", "Val", "Trp", "Tyr"),
  stringsAsFactors = FALSE
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
   if(values$started){
     random_number <- runif(1, 1, 20)
     
     if(random_number %in% values$no_repeats){
       while (random_number %in% values$no_repeats) {
         random_number <- runif(1, 1, 20)
       }
       append(values$no_repeats, random_number)
     }
     
     if(input$mode == "Structure"){
       filename <- normalizePath(file.path('./amino_acids/', 
                                           paste(aminoacids$threeletter_code[random_number], '.png', sep = '')))
       
       output$image <- renderImage({
         list(src = filename, height = "400px", width = "500px")
       }, deleteFile = FALSE)
       
       
     } else if(input$mode == "One-letter code"){
       h3(aminoacids$oneletter_code[random_number])
     }else{
       h3(aminoacids$threeletter_code[random_number])
     }
   }
   
 }
 )
 observeEvent(input$submit, {
   current_question <- values$current_question
   correct_answers <- aminoacids$name[values$no_repeats[current_question]]
   input_answer <- input$answer
   
   if(input$mode == "Structure"){
     values$answers$question[values$current_question] <-("Structure of " + aminoacids$threeletter_code[random_number])
   } else if (input$mode == "One-letter code") {
     values$answers$question[values$current_question] <- aminoacids$oneletter_code[random_number]
   }else {
     values$answers$question[values$current_question] <- aminoacids$threeletter_code[random_number]
   }
   values$answers$answer[values$current_question] <- input_answer
   values$answers$right_answer[values$current_question] <- correct_answers
   
   if(tolower(input_answer) == tolower(input_answer)){
     values$correct_answers <- values$correct_answers + 1
   }
   
   if(values$current_question < values$total_questions){
     values$current_question <- values$current_question + 1
     updateTextInput(session, "answer", value = "")
   } else {
     output$game <- renderUI({
       h3("Results:")
     })
    output$results <- renderTable({
      cbind(values$answers, 
            if (tolower(values$answers$answer) == tolower(values$answers$right_answer)){
              Results = "Right"
            }else{
              Results = "Wrong"
            })
    })
   }
 })
 
}


