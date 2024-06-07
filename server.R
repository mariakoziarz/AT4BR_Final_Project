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
    random_number = NULL,
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
   values$no_repeats <- c()
   values$ answers <- data.frame(question = character(10), answer = character(10), right_answer = character(10))
   
   output$game <- renderUI({
     fluidPage(
       uiOutput("question"),
       textInput("answer", "Your answer: "),
       actionButton("submit", "Submit")
     )
     
   })
   isolate({
     updateQuestion()
   })
})
 
 updateQuestion <- function() {
   values$random_number <- sample(1:20, 1)
   
   while (values$random_number %in% values$no_repeats) {
     values$random_number <- sample(1:20, 1)
   }
   
   values$no_repeats <- append(values$no_repeats, values$random_number)
   
   output$question <- renderUI({
     if(input$mode == "Structure"){
       filename <- normalizePath(file.path('./amino_acids/', 
                                           paste(aminoacids$threeletter_code[values$random_number], '.png', sep = '')))
       
       output$image <- renderImage({
         list(src = filename, height = "400px", width = "500px")
       }, deleteFile = FALSE)
       
       imageOutput("image")
     } else if(input$mode == "One-letter code"){
       h3(aminoacids$oneletter_code[values$random_number])
     } else {
       h3(aminoacids$threeletter_code[values$random_number])
     }
   })
 }
 
 observeEvent(input$submit, {
   
   correct_answer <- aminoacids$name[values$no_repeats[values$current_question]]
   input_answer <- input$answer
   
   if(input$mode == "Structure"){
     values$answers$question[values$current_question] <-paste("Structure of", aminoacids$threeletter_code[values$random_number])
   } else if (input$mode == "One-letter code") {
     values$answers$question[values$current_question] <- aminoacids$oneletter_code[values$random_number]
   }else {
     values$answers$question[values$current_question] <- aminoacids$threeletter_code[values$random_number]
   }
   values$answers$answer[values$current_question] <- input_answer
   values$answers$right_answer[values$current_question] <- correct_answer

   if(tolower(input_answer) == tolower(correct_answer)){
     values$correct_answers <- values$correct_answers + 1
   }
   
   if(values$current_question < values$total_questions){
     
     values$current_question <- values$current_question + 1
     
     
     updateTextInput(session, "answer", value = "")
     isolate({
       updateQuestion()
     })
   } else {
     output$game <- renderUI({
       tagList(h3("Results:"),
       tableOutput("results")
       )
     })
     
     output$results <- renderTable({
       req(values$current_question == values$total_questions)
       results <- ifelse(tolower(values$answers$answer) == tolower(values$answers$right_answer), "Right", "Wrong")
       cbind(values$answers, Results = results)
     })
   }
 })
 
}


