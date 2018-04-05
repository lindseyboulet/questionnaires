# Install and load required packages --------------------------------------

y <- c("shiny", "shinyjs", "V8", "taRifx", "shinyWidgets", "shinythemes")  
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}


# # Define the fields we want to save from the form -----------------------
# This builds the data table so every input needs to be represented here
fields <- c("id","weight", "height", "gender","age","meal", "period", "s_q1",
            "s_q2", "s_q3", "s_q4", "s_q5", "s_q6", "s_q7", "s_q8", "s_q9",
            "s_q10", "s_q11", "s_q12", "s_q13", "s_q14", "s_q15")

# Form reset code
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# Function that checks if data exists, if yes it loads data and appends new form data
# if no it creates new data frame

saveData <- function(data) 
  {
  data <- t(data)
  data <- data.frame(cbind("date" = as.character(Sys.Date()),
                           "time" = substr(Sys.time(), 12, 19), data))
  # convert factor to character
  data <- japply(data, which(sapply(data, class)=="factor"), as.character)
  # Create a unique filename 
  fileName <- here::here("questionnaire_data.csv")
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


# The layout of the questionnaire in HTML ---------------------------------
# Watch the parentheses 

  ui <- fluidPage(theme = shinytheme("slate"),
    titlePanel("Subject Demographics and Prescreening Questionnaire"), # page title
    div(id = "form",
    hr(),
    h3(strong("Sub Title or Study Title Here")),
    br(),
    fluidRow(
      column(3,
        textInput("id", "Subject ID", "")),
      column(3,
        numericInput("weight", "Weight (kg)", "")),
      column(3,
             numericInput("height", "Height (cm)", "")),
      column(3,
             selectizeInput("gender", "Gender", choices = list("Male" = "m",
                                                               "Female" = "f")))),
    
    fluidRow(
      column(3,
        numericInput("age", "Age ", "")),
      column(3,
             textInput("meal", "Time of last meal", "")),
      column(3, 
             textInput("period", "Date of last menstrual period", ""))),
    hr(),
    checkboxGroupInput("s_q1", label = h3("Have you refrained from caffeine, 
                                        alcohol and vigorous exercise 24 hours prior 
                                        to the experimental day?"),
                       width = "100%",
                    
                       choices = c("Yes" = "y", 
                                      "No" = "n")
                       ),
    
        checkboxGroupInput("s_q2", label = h3("Do you have a history of fainting or have ever
                                        experienced a syncopal episode (i.e. fainting)?  "),
                           width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q3", label = h3("Do you have a previous history of or a current
                                        respiratory disease or abnormality (e.g., asthma,
                                        chronic bronchitis, cystic fibrosis)?  "),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q4", label = h3("Do you have a previous history of or a current
                                        cardiovascular disease or abnormality (e.g.,
                                        cardiac arrhythmia, hypertension, myocardial infarction)?"), 
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q5", label = h3("Do you have a previous history of or a current
                                        neurological disease or abnormality (e.g., epilepsy,
                                        chronic migraines, stroke)?"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q6", label = h3("Are you currently on any kind of medication,
                                        particularly ones that may alter blood pressure 
                                        or cerebral blood flow (e.g., opiates,
                                        diuretics, vasodilators)?"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q7", label = h3("Do you have type I or II diabetes? "),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q8", label = h3("Do you suffer from any gastro-intestinal
                                        or liver diseases and/or other abnormality
                                        (e.g., ulcers, inflammatory bowel disease,
                                        or gastro-intestinal bleeding)?"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q9", label = h3("Do you partake in regular physical
                                        activity (e.g., moderate physical 
                                        activity 3-5 days/ week)? "),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q10", label = h3("Do you smoke?"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q11", label = h3("Do you have any drug allergies?"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    
    checkboxGroupInput("s_q12", label = h3("If female, are you pregnant,
                                         or are trying to become pregnant?
                                         (We will be providing a commercially
                                         available pregnancy test.)"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q13", label = h3("Are you using oral contraceptives?"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q14", label = h3("Have you ever been to high altitude
                                         (above 3,000m) and if so did you become sick?"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    
    checkboxGroupInput("s_q15", label = h3("Have you had all of your questions or
                                         concerns addressed"),
                       width = "100%",
                       choices = list("Yes" = "y", 
                                      "No" = "n")),
    tags$head(tags$style("#add{color: #4d3a7d;
                       font-weight: 500;
                       }")),
    
    br(),
    useShinyjs(),                                           # Include shinyjs in the UI
    extendShinyjs(text = jsResetCode), 

       actionButton("submit", strong("Submit")),
    br(""),
    hr()
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
    
    
    # When the Submit button is clicked, checks to see that all inputs are filled
    observeEvent(input$submit, {
      validate(
        need(input$id, "Missing Responses!"), need(input$weight, "Missing Responses!"),
        need(input$height, "Missing Responses!"), need(input$gender, "Missing Responses!"),
        need(input$age, "Missing Responses!"), need(input$meal, "Missing Responses!"),
        need(input$s_q1, "Missing Responses!"), need(input$s_q2, "Missing Responses!"),
        need(input$s_q3, "Missing Responses!"), need(input$s_q4, "Missing Responses!"),
        need(input$s_q5, "Missing Responses!"), need(input$s_q6, "Missing Responses!"),
        need(input$s_q7, "Missing Responses!"), need(input$s_q8, "Missing Responses!"),
        need(input$s_q9, "Missing Responses!"), need(input$s_q10, "Missing Responses!"),
        need(input$s_q11, "Missing Responses!"), need(input$s_q12, "Missing Responses!"),
        need(input$s_q13, "Missing Responses!"), need(input$s_q14, "Missing Responses!"),
        need(input$s_q15, "Missing Responses!")
)

     
      saveData(formData())
    })
    
    # checks the inputs again and saves the data
    observeEvent(input$submit, { 
      validate(
        need(input$id, "Missing Responses!"), need(input$weight, "Missing Responses!"),
        need(input$height, "Missing Responses!"), need(input$gender, "Missing Responses!"),
        need(input$age, "Missing Responses!"), need(input$meal, "Missing Responses!"),
        need(input$s_q1, "Missing Responses!"), need(input$s_q2, "Missing Responses!"),
        need(input$s_q3, "Missing Responses!"), need(input$s_q4, "Missing Responses!"),
        need(input$s_q5, "Missing Responses!"), need(input$s_q6, "Missing Responses!"),
        need(input$s_q7, "Missing Responses!"), need(input$s_q8, "Missing Responses!"),
        need(input$s_q9, "Missing Responses!"), need(input$s_q10, "Missing Responses!"),
        need(input$s_q11, "Missing Responses!"), need(input$s_q12, "Missing Responses!"),
        need(input$s_q13, "Missing Responses!"), need(input$s_q14, "Missing Responses!"),
        need(input$s_q15, "Missing Responses!")
      )
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")    })
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
     
    })     
  }
shinyApp(ui, server)
