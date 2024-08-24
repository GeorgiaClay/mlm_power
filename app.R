#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(designr)
library(dplyr )
library(stringr)
library(simr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Power Analysis for MLM"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Enter parameters below."),
      h3("Fixed effects:"),
      fluidRow(
      column(4, textInput("FE_0_name", value = "A", label = "Label")),
      column(4, textInput("FE_0_levels", value = "A1, A2", label = "Levels")),
      column(4, textInput("FE_0_ES", value = "0.2", label = "Effect sizes")),
      ),
      
      uiOutput("dynamic_text_inputs"),
      
      fluidRow(
        column(1, offset = 8,
               actionButton("removeFE", label = "", icon = icon("minus"), 
                            style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px")),
        column(1,
               actionButton("addnewFE", label = "", icon = icon("plus"), 
                            style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px"))),
      
      
      p("================================"),
      h3("Random effects"),
      textInput("RE1_name", value = "Subject", label = "Cluster name"),
      numericInput("RE1_N", label = "N", value = "30"),
      numericInput("RE1_var", label = "Variance", value = "0.5"),
      numericInput("Res_var", label = "Residual variance", value = 1)),
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("x"),
      verbatimTextOutput("y"),
      h1("Data structure:"),
      DT::dataTableOutput("designTab"),
      verbatimTextOutput("model"),
      verbatimTextOutput("pa")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # fixed effects #
  
  text_input_counter <- reactiveVal(0)
  
  # Reactive value to store the existing values of text inputs
  FE_names <- reactiveValues()
  FE_levels <- reactiveValues()
  FE_ESs <- reactiveValues()
  
  # Observe the button click and add a new text input
  observeEvent(input$addnewFE, {
    # Increment the counter
    new_count <- text_input_counter() + 1
    text_input_counter(new_count)
    
    # Update the UI with the new text input
    output$dynamic_text_inputs <- renderUI({
      # Generate a list of text input elements
      input_list <- lapply(1:new_count, function(i) {
        # Use the existing value if it exists, otherwise set to an empty string
        fluidRow(
             column(4, textInput(paste0("FE", i, "name", sep = "_"), value = toupper(letters[i+1]), label = "Label")),
             column(4, textInput(paste0("FE", i, "levels", sep = "_"), value = paste0(toupper(letters[i+1]), c(1,2), collapse = ", "), label = "Levels")),
             column(4, textInput(paste0("FE", i, "ES", sep = "_"), value = "0.2", label = "Effect sizes")),
           )
      })
      
      # Return the UI elements to be displayed
      do.call(tagList, input_list)
    })
  })
  
  # Observe the remove button click and remove the last text input
  observeEvent(input$removeFE, {
    new_count <- text_input_counter() - 1
    
    # Ensure the counter doesn't go below 0
    if (new_count >= 0) {
      text_input_counter(new_count)
      
      # Update the UI to remove the last text input
      output$dynamic_text_inputs <- renderUI({
        if (new_count == 0){NULL}
        else{
        input_list <- lapply(1:new_count, function(i) {
          # Use the existing value if it exists, otherwise set to an empty string
          fluidRow(
            column(4, textInput(paste0("FE", i, "name", sep = "_"), value = toupper(letters[i+1]), label = "Label")),
            column(4, textInput(paste0("FE", i, "levels", sep = "_"),value = paste0(toupper(letters[i+1]), collapse = ", "), label = "Levels")),
            column(4, textInput(paste0("FE", i, "ES", sep = "_"), value = "0.2", label = "Effect sizes")),
          )
        })
        do.call(tagList, input_list)
      }})
    }
  })
  
  # Observe changes to the text inputs and store their values
  observe({
    if (text_input_counter() == 0){
      # reset lists to being empty
      FE_names <- reactiveValues()
      FE_levels <- reactiveValues()
      FE_ESs <- reactiveValues()}
    else{
    lapply(1:text_input_counter(), function(i) {
      FE_names[[paste0("FE_name", i)]] <- input[[paste0("FE", i, "name", sep = "_")]]
      FE_levels[[paste0("FE_level", i)]] <- input[[paste0("FE", i, "levels", sep = "_")]]
      FE_ESs[[paste0("FE_name", i)]] <- input[[paste0("FE", i, "ES", sep = "_")]]
    })}
  })
  
  
  #####
  
  output$x <- renderText({text_input_counter()})
  
  output$y <- renderText({  
    toString(fixed_effects())
    })
  
  fixed_effect_sizes <- reactive({
    as.numeric(c(list(0,input$FE_0_ES),reactiveValuesToList(FE_ESs)))
  })
  
  fixed_effects <- reactive({
    c("A", reactiveValuesToList(FE_names))
  })
  
  # Make dataset
  getDF <- reactive({

    # Initialize the design with the first fixed effect
    current_design <- fixed.factor(input$FE_0_name, levels = str_split_1(input$FE_0_levels, ","))
    
    # Add additional fixed effects dynamically
    if (text_input_counter() > 0) {
      for (i in 1:text_input_counter()) {
        current_design <- current_design + fixed.factor(FE_names[[paste0("FE_name", i)]], 
                                                        levels = str_split_1(FE_levels[[paste0("FE_level", i)]], ","))
      }
    }

    #current_design <- current_design + fixed.factor(FE_names[["FE_name1"]], levels = str_split_1(FE_levels[["FE_level1"]], ","))
    # Add the random effect
    current_design <- current_design + random.factor(input$RE1_name, instances = input$RE1_N)
    
    data <- design.codes(current_design)
    return(data)
    })
  
  

  make_model <- reactive({
    fixef = unlist(fixed_effect_sizes(), use.names = F)
    rand = list(input$RE1_var)
    #rand = list(0.4)
    res = input$Res_var
    fixed= str_c(fixed_effects(), collapse = "+")
    formula = as.formula(paste0("y ~ ", fixed, "+ (1 |Subject)"))
    
    
    model <- makeLmer(formula, fixef=fixef, VarCorr=rand, sigma=res, data=getDF())
  })
  
  output$designTab <- renderDataTable({
    tab_head <- getDF() %>% 
      head(20) %>%
      DT::datatable(
        options = list(
          autoWidth = FALSE,
          columnDefs = list(
            list(width = '150px', targets = c(0, 1)))
        )
      )
    })
    
  
  
    #Calculate power
  output$model <- renderPrint({
    
    # check for errors
    if (length(str_split_1(input$FE_0_levels, ",")) != length(str_split_1(input$FE_0_ES, ",")) + 1) {
      stop("Incorrect number of effect sizes for levels of factor")
    }
    
    model <- make_model()
    
    
    sim <- powerSim(make_model(),fixed("FE", "lr"), nsim=10)
    output$pa <- renderPrint({sim})
    
    return(model)

  })
  
  
  # output$pa <- renderPrint({
  #   
  #   sim <- powerSim(make_model(),fixed("FE", "lr"), nsim=10)
  #   sim
 # })
 
   
}


# Run the application 
shinyApp(ui = ui, server = server)


