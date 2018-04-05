
y <- c("shiny", "shinyjs", "V8", "taRifx", "shinythemes")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")
}
library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
}

# Define the fields we want to save from the form
fields <- c("f_name","l_name", "id", "condition", "LL_q1", "LL_q2", "LL_q3", "LL_q4", "LL_q5")


jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

saveData <- function(data) 
  {
  data <- t(data)
  data <- data.frame(cbind("date" = as.character(Sys.Date()),
                           "time" = substr(Sys.time(), 12, 19), data))
  # convert factor to character
  data <- japply(data, which(sapply(data, class)=="factor"), as.character)
  # data <- data.frame(cbind(data, "LL_score"= sum(data[1,7:11])))
  # Create a unique filename
  fileName <- here::here("studySummary", "llQuestData", "lake_louise_scores.csv")
  # Check if data file exists
  if(file.exists(fileName))
    {
    oldData <- read.csv(fileName)
    data <- rbind(data, oldData)
    }
  # Write the file to the local system
  write.csv(x = data,
    file = fileName, 
    row.names = FALSE, quote = TRUE)
  }
  
# Shiny app with 3 fields that the user can submit data for


  ui <- fluidPage(theme = shinytheme("slate"),
    titlePanel("The Lake Louise AMS scoring system 2017"),
    div(id = "form",
    hr(),
    fluidRow(
      column(3, 
        textInput("f_name", "First Name", "")),
      column(3,
        textInput("l_name", "Last Name", ""))),
    fluidRow(
      column(3, 
        textInput("id", "Subject ID", "")),
      column(3,
        selectizeInput("condition", "Condition", choices = list("Treatment A" = "A",
                                                                "Treatment B" = "B")))),
    hr(),
    fluidRow(
      column(3, 
    checkboxGroupInput("LL_q1", label = h3("Headache:"),
                       width = '100%',
                       choices = list("0 None at all" = 0, 
                                      "1 A mild headache" = 1,
                                      "2 Moderate headache" = 2,
                                      "3 Severe headache, incapacitating" = 3))),
    column(3,
    
    
    checkboxGroupInput("LL_q2", label = h3("Gastrointestinal symptoms:"), 
                       choices = list("0 Good appetite" = 0, 
                                      "1 Poor appetite or nausea" = 1,
                                      "2 Moderate nausea or vomiting" = 2,
                                      "3 Severe nausea and vomiting, incapacitating" = 3)))),
    fluidRow(
      column(3,
    
    checkboxGroupInput("LL_q3", label = h3("Fatigue and/or weakness:"), 
                       choices = list("0 Not tired or weak" = 0, 
                                      "1 Mild fatigue/weakness" = 1,
                                      "2 Moderate fatigue/weakness" = 2,
                                      "3 Severe fatigue/weakness, incapacitating" = 3))),
    column(3,
    checkboxGroupInput("LL_q4", label = h3("Dizziness/lightheadedness:"), 
                       choices = list("0 No dizziness/lightheadedness" = 0, 
                                      "1 Mild dizziness/lightheadedness" = 1,
                                      "2 Moderate dizziness/lightheadedness" = 2,
                                      "3 Severe dizziness/lightheadedness, incapacitating" = 3)))),
    
    checkboxGroupInput("LL_q5", label = h3("Lake Louise AMS Functional Score, Overall, if you had these AMS                         symptoms, how did they affect your activities?"),
                       width = '100%',
                       choices = list("0 Not at all" = 0, 
                                      "1 Symptoms present, but did not force any change in activity or                                           itinerary" = 1,
                                      "2 My symptoms forced me to stop the ascent or to go down on my own                                         power" = 2,
                                      "3 Had to be evacuated to a lower altitude" = 3)),
    br(),
    useShinyjs(),                                           # Include shinyjs in the UI
    extendShinyjs(text = jsResetCode), 

       actionButton("submit", strong("Submit")),
    hr(),
    tags$i(p(strong("References")),
    p("Roach, R. C., Bartsch, P., Hackett, P. H., & Olez, O. (1993). The Lake Louise acute mountain sickness scoring system. Hypoxia and Molecular Medicine: Proceedings of the 8th International Hypoxia Symposium Held at Lake Louise, Canada, February 9-13, 1993, 272, 272–274."),
    p("Roach, R., Hackett, P., Oelz, O., Bartsch, P., Luks, A., MacInnis, M., & JK, B. (2017). The new lake louise acute mountain sickness symptom score: 2017."))
    ),
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thank you, your response was submitted successfully!")
      )
    ) 
  )
  
  server <- function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      validate(
        need(input$f_name, "Missing Responses!"), need(input$l_name, "Missing Responses!"),
        need(input$id, "Missing Responses!"), need(input$condition, "Missing Responses!"),
        need(input$LL_q1, "Missing Responses!"), need(input$LL_q2, "Missing Responses!"),
        need(input$LL_q3, "Missing Responses!"), need(input$LL_q4, "Missing Responses!"),
        need(input$LL_q5, "Missing Responses!")
      )
      saveData(formData())
    })
    observeEvent(input$submit, {
      validate(
        need(input$f_name, "Missing Responses!"), need(input$l_name, "Missing Responses!"),
        need(input$id, "Missing Responses!"), need(input$condition, "Missing Responses!"),
        need(input$LL_q1, "Missing Responses!"), need(input$LL_q2, "Missing Responses!"),
        need(input$LL_q3, "Missing Responses!"), need(input$LL_q4, "Missing Responses!"),
        need(input$LL_q5, "Missing Responses!")
      )
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")    })
  }

shinyApp(ui, server)
