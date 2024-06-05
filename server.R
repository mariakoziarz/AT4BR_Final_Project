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
    current_question = 1,
    total_questions = 10,
    correct_answers = 0,
    answers = data.frame(question = character(10), answer = character(10), right_answer = character(10)),

 )
 
}


