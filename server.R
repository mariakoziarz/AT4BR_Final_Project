library(shiny)

#list of amino acids
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

#defining the server logic
server <- function(input, output, session) {
  
 #initialization of object with reactive values
 values <- reactiveValues(
    started = FALSE, 
    current_question = 1,
    total_questions = 10,
    correct_answers = 0,
    no_repeats = c(), #vector to store used random numbers
    random_number = NULL,
    answers = data.frame(question = character(10), answer = character(10), right_answer = character(10))
    #default values before starting the game
 )
 #instructions displayed after running thr game
 output$instructions <- renderText({
   if(!values$started){
     paste("<b>Welcome, young scientist!</b>", "<br>",
           "Now that you have entered the world of science, it is high time for you to acquire a skill that every scientist should have - the recognition of amino acids.", 
           "<br>", "This game will help you in this seemingly difficult task.", 
           "<br>",  "Select the game mode and then press the <B>‘Start game'</B> button.", "<br>", 
           "You will see a structure/one-letter code/three-letter code, depending on the option selected.", "<br>", 
           " Enter the name of the corresponding amino acid in the box.", "<br>", 
           "<b>Have fun!</b>")
   }
 })
 #action caused by clicking "Start game" button
 observeEvent(input$start,{
   values$started <- TRUE
   values$current_question <- 1
   values$correct_answers <- 0
   values$no_repeats <- c()
   values$ answers <- data.frame(question = character(10), answer = character(10), right_answer = character(10))
   
   #default structure of output elements after starting the game
   output$game <- renderUI({
     fluidPage(
       renderText({
         paste(values$current_question, "/", values$total_questions)
       }),
       uiOutput("question"),
       textInput("answer", "Your answer: "),
       actionButton("submit", "Submit")
       
     )
     
   })
   #updating page after submit
   isolate({
     updateQuestion()
   })
})
 #function to update page 
 updateQuestion <- function() {
   values$random_number <- sample(1:20, 1) #drawing number of amino acid 
   
   while (values$random_number %in% values$no_repeats) { #loop to avoid doubled questions
     values$random_number <- sample(1:20, 1)
   }
   
   values$no_repeats <- append(values$no_repeats, values$random_number)
   
   output$question <- renderUI({ #values apearing on the screen depending on choosen option
     if(input$mode == "Structure"){
       #based on structure with random amino acids
       filename <- normalizePath(file.path('./amino_acids/', 
                                           paste(aminoacids$threeletter_code[values$random_number], '.png', sep = '')))
       
       output$image <- renderImage({
         list(src = filename, height = "400px", width = "500px")
       }, deleteFile = FALSE)
       
       imageOutput("image")
     } else if(input$mode == "One-letter code"){
       h3(aminoacids$oneletter_code[values$random_number]) #displaying three-letter code
     } else if (input$mode == "Three-letter code"){
       h3(aminoacids$threeletter_code[values$random_number]) #displaying one-letter code
     }
   })
   
 }
 
 #actions taken after submitting the answer
 observeEvent(input$submit, {
   
   correct_answer <- aminoacids$name[values$no_repeats[values$current_question]] 
   input_answer <- input$answer
   
   #filling the dataframe table with correct answers, input answer and validation
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
   
   #checking whether the game is finishes
   if(values$current_question < values$total_questions){#if it isn't go to the next
     
     values$current_question <- values$current_question + 1
     
     
     updateTextInput(session, "answer", value = "")
     isolate({
       updateQuestion()
     })
   } else { #if is display the results
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
     output$score <- renderText({
       paste("Score: ", values$correct_answers)
     })
   }
 })
 
}


