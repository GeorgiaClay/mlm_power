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
      
      # First fixed effect
      fluidRow(
      column(4, textInput("FE_name0", value = "A", label = "Label")),
      column(4, textInput("FE_levels0", value = "A1, A2", label = "Levels")),
      column(4, textInput("FE_ES0", value = "0.2", label = "Effect sizes")),
      ),
      
      # FE inputs
      uiOutput("fixed_effect_inputs"),
      
      # Buttons to add or remove FEs
      fluidRow(
        column(1, offset = 8,
               actionButton("removeFE", label = "", icon = icon("minus"), 
                            style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px")),
        column(1,
               actionButton("addnewFE", label = "", icon = icon("plus"), 
                            style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px"))),
      
      hr(style = "border-top: 1px solid #000000;"),
      
      
      h3("Random effects"),
      fluidRow(
        column(3, textInput("RE_name0", value = "Subject", label = "Cluster name")),
        column(3, numericInput("RE_N0", label = "N", value = "30")),
        column(3, numericInput("RE_var0", label = "Variance", value = "0.5")),
        column(3, htmlOutput("RE_dropdown0"))
      ),
      
      # RE inputs
      uiOutput("random_effect_inputs"),
      
      # Buttons to add or remove FEs
      fluidRow(
        column(1, offset = 8,
               actionButton("removeRE", label = "", icon = icon("minus"), 
                            style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px")),
        column(1,
               actionButton("addnewRE", label = "", icon = icon("plus"), 
                            style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px"))),
      
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
  
  
  #### Fixed effects ####
  
  FE_counter <- reactiveVal(0)
  
  # Reactive value to store the existing values of text inputs
  FE_names <- reactiveValues()
  FE_levels <- reactiveValues()
  FE_ESs <- reactiveValues()
  
  # Observe the button click and add a new text input
  observeEvent(input$addnewFE, {
    # Increment the counter
    new_count_FE <- FE_counter() + 1
    FE_counter(new_count_FE)
    
    # Update the UI with the new text input
    output$fixed_effect_inputs <- renderUI({
      # Generate a list of text input elements
      FE_input_list <- lapply(1:new_count_FE, function(i) {
        # Use the existing value if it exists, otherwise set to an empty string
        fluidRow(
             column(4, textInput(paste0("FE_name", i), value = toupper(letters[i+1]), label = "Label")),
             column(4, textInput(paste0("FE_levels", i), value = paste0(toupper(letters[i+1]), c(1,2), collapse = ", "), label = "Levels")),
             column(4, textInput(paste0("FE_ES", i), value = "0.2", label = "Effect sizes")),
           )
      })
      
      # Return the UI elements to be displayed
      do.call(tagList, FE_input_list)
    })
  })
  
  # Observe the remove button click and remove the last text input
  observeEvent(input$removeFE, {
    new_count_FE <- FE_counter() - 1
    
    # Ensure the counter doesn't go below 0
    if (new_count_FE >= 0) {
      FE_counter(new_count_FE)
      
      # Update the UI to remove the last text input
      output$fixed_effect_inputs <- renderUI({
        if (new_count_FE == 0){NULL}
        else{
        FE_input_list <- lapply(1:new_count_FE, function(i) {
          # Use the existing value if it exists, otherwise set to an empty string
          fluidRow(
            column(4, textInput(paste0("FE_name", i), value = toupper(letters[i+1]), label = "Label")),
            column(4, textInput(paste0("FE_levels", i), value = paste0(toupper(letters[i+1]), collapse = ", "), label = "Levels")),
            column(4, textInput(paste0("FE_ES", i), value = "0.2", label = "Effect sizes")),
          )
        })
        do.call(tagList, FE_input_list)
      }})
    }
  })
  
  # Observe changes to the text inputs and store their values
  observe({
    if (FE_counter() == 0){
      # reset lists to being empty
      FE_names <- reactiveValues()
      FE_levels <- reactiveValues()
      FE_ESs <- reactiveValues()}
    else{
    lapply(1:FE_counter(), function(i) {
      FE_names[[paste0("FE_name", i)]] <- input[[paste0("FE_name", i)]]
      FE_levels[[paste0("FE_levels", i)]] <- input[[paste0("FE_levels", i)]]
      FE_ESs[[paste0("FE_ES", i)]] <- input[[paste0("FE_ES", i)]]
    })}
  })
  
  
  
  
  #### Random effects ####
  
  output$RE_dropdown0 <- renderUI({
    selectInput("RE_dropdown0", label = "Between FEs", choices = unname(fixed_effects()), multiple = T)
  })
  
  RE_counter <- reactiveVal(0)
  
  # Reactive value to store the existing values of text inputs
  RE_names <- reactiveValues()
  RE_Ns <- reactiveValues()
  RE_vars <- reactiveValues()
  RE_groups <- reactiveValues()
  
  # Observe the button click and add a new text input
  observeEvent(input$addnewRE, {
    # Increment the counter
    new_count_RE <- RE_counter() + 1
    RE_counter(new_count_RE)
    
    # Update the UI with the new text input
    output$random_effect_inputs <- renderUI({
      # Generate a list of text input elements
      RE_input_list <- lapply(1:new_count_RE, function(i) {
        # Use the existing value if it exists, otherwise set to an empty string
        fluidRow(
          column(3, textInput(paste0("RE_name", i), value = paste0("RE", i+1), label = "Label")),
          column(3, numericInput(paste0("RE_N", i), value = 20, label = "N")),
          column(3, textInput(paste0("RE_var", i), value = "0.2", label = "Variance")),
          column(3, selectInput(paste0("RE_dropdown", i), label = "Between FEs", choices = unname(fixed_effects()), multiple = T))
        )
      })
      
      # Return the UI elements to be displayed
      do.call(tagList, RE_input_list)
    })
  })
  
  # Observe the remove button click and remove the last text input
  observeEvent(input$removeRE, {
    new_count_RE <- RE_counter() - 1
    
    # Ensure the counter doesn't go below 0
    if (new_count_RE >= 0) {
      RE_counter(new_count_RE)
      
      # Update the UI to remove the last text input
      output$random_effect_inputs <- renderUI({
        if (new_count_RE == 0){NULL}
        else{
          RE_input_list <- lapply(1:new_count_RE, function(i) {
            # Use the existing value if it exists, otherwise set to an empty string
            fluidRow(
              column(3, textInput(paste0("RE_name", i), value = paste0("RE", i+1), label = "Label")),
              column(3, numericInput(paste0("RE_N", i), value = 20, label = "N")),
              column(3, textInput(paste0("RE_var", i), value = "0.2", label = "Variance")),
              column(3, selectInput(paste0("RE_dropdown", i), label = "Between FEs", choices = unname(fixed_effects()), multiple = T))
            )
          })
          do.call(tagList, RE_input_list)
        }})
    }
  })
  
  # Observe changes to the text inputs and store their values
  observe({
    if (RE_counter() == 0){
      # reset lists to being empty
      RE_names <- reactiveValues()
      RE_Ns <- reactiveValues()
      RE_vars <- reactiveValues()
      RE_groups <- reactiveValues()}
    else{
      lapply(1:RE_counter(), function(i) {
        RE_names[[paste0("RE_name", i)]] <- input[[paste0("RE_name", i)]]
        RE_Ns[[paste0("RE_N", i)]] <- input[[paste0("RE_N", i)]]
        RE_vars[[paste0("RE_var", i)]] <- input[[paste0("RE_var", i)]]
        RE_groups[[paste0("RE_groups",  i)]] <- input[[paste0("RE_dropdown", i)]]
      })}
  })
  
  
  #####
  
  output$x <- renderText({FE_counter()})
  
  output$y <- renderText({  
    toString(random_variances())
    })
  
  fixed_effect_sizes <- reactive({
    as.numeric(c(list(0,input$FE_ES0),reactiveValuesToList(FE_ESs)))
  })
  
  fixed_effects <- reactive({
    c(input$FE_name0, reactiveValuesToList(FE_names))
  })
  
  random_effects <- reactive({
    c(input$RE_name0, reactiveValuesToList(RE_names))
  })
  
  random_variances <- reactive({
    as.numeric(c(input$RE_var0, reactiveValuesToList(RE_vars)))
  })
  
  # Make dataset
  getDF <- reactive({

    # Initialize the design with the first fixed effect
    current_design <- fixed.factor(input$FE_name0, levels = str_split_1(input$FE_levels0, ","))
    
    # Add additional fixed effects dynamically
    if (FE_counter() > 0) {
      for (i in 1:FE_counter()) {
        current_design <- current_design + fixed.factor(FE_names[[paste0("FE_name", i)]], 
                                                        levels = str_split_1(FE_levels[[paste0("FE_levels", i)]], ","))
      }
    }

    # Add the random effects
    
    if (is.null(input$RE_dropdown0)){
      groups = character(0)
    }
    else{groups = str_split_1(input$RE_dropdown0, ",")}
    
    current_design <- current_design + random.factor(input$RE_name0, instances = input$RE_N0, groups = groups)
    
    if (RE_counter() > 0) {
      for (i in 1:RE_counter()) {
        if (is.null(RE_groups[[paste0("RE_groups", i)]])){
          groups = character(0)
        }
        else{ groups = str_split_1(RE_groups[[paste0("RE_groups", i)]], ",")}
        
        current_design <- current_design + random.factor(RE_names[[paste0("RE_name", i)]], 
                                                        instances = RE_Ns[[paste0("RE_N", i)]],
                                                        groups = groups)
      }
    }
    
    
    data <- design.codes(current_design)
    return(data)
    })
  
  

  make_model <- reactive({
    fixed= str_c(fixed_effects()[1:(FE_counter()+1)], collapse = "+")
    fixef = unlist(fixed_effect_sizes()[1:(FE_counter()+2)], use.names = F)
    rand = random_variances()
    #rand = list(0.4)
    res = input$Res_var
    formula = as.formula(paste0("y ~ ", fixed, "+ (1 |Subject)"))
    
    
    model <- makeLmer(formula, fixef=fixef, VarCorr=rand, sigma=res, data=getDF())
  })
  
  output$designTab <- DT::renderDT({
    tab_head <- getDF() %>% 
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
    if (length(str_split_1(input$FE_levels0, ",")) != length(str_split_1(input$FE_ES0, ",")) + 1) {
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


