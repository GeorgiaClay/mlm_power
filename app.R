library(shiny)
library(shinyjs)
library(ggplot2)
library(designr)
library(dplyr)
library(simr)

# Helper function to create a label with a subheading
label_with_subheading <- function(main_label, subheading) {
  HTML(paste0(main_label, "<br><span class='subheading'>", subheading, "</span>"))
}


# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  # CSS to control subheading style
  tags$head(tags$style(HTML("
    .subheading {
      font-size: 0.8em;
      color: gray;
    }
  "))),
  
  titlePanel("Power Analysis for Multi-Level Models"),
  
  p(HTML("<ul>
         <li>Use this tool to carry out a power analysis for nested data</li>
         <li>Allows random intercept and slopes (up to 3 random effects, e.g. random intercept and slope for one factor with 3 levels, or random intercept and slope for two variables, each with 2 levels)</li>
         <li>Fixed effects are factor variables, with the first listed serving as the reference level</li>
         <li>Use the model summary on the main page to check the parameters have been entered correctly</li>
         </ul>")),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      h3("Enter parameters below."),
      h3("Fixed effects:"),
      
      # First fixed effect
      fluidRow(
      column(12, textInput("FE_name0", value = "A", label = "Label")),
      column(6, textInput("FE_levels0", value = "A1, A2", label = label_with_subheading("Levels","Add labels for each level separated by a comma"))),
      column(6, textInput("FE_ES0", value = "0.2", label = label_with_subheading("Effect sizes", "Each level from reference level, separated by a comma"))),
      column(4, htmlOutput("Ran_slopes0")),
      column(12, p("If you have a random slope, enter here the standard deviation of the slope of each level from the reference level. Then enter the random effects correlations, separated by a comma.")),
      column(4, textInput("FE_slopeSD0", value = "", label = "Slope SD")),
      column(4, textInput("FE_corr0", value = "", label = "Random effect correlations")),
      ),
      
      # FE inputs
      uiOutput("fixed_effect_inputs"),
      
      # Number of repetitions of each observation
      numericInput("FE_N0", value = 1, label = label_with_subheading("Repetitions", "Reps of each observation")),
      
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
        column(3, numericInput("RE_N0", label = "N", value = "10")),
        column(3, numericInput("RE_var0", label = "Variance", value = "0.5")),
        column(3, htmlOutput("RE_dropdown0"))
      ),
      
      # RE inputs
      uiOutput("random_effect_inputs"),
      
      # Buttons to add or remove REs
      # fluidRow(
      #   column(1, offset = 8,
      #          actionButton("removeRE", label = "", icon = icon("minus"), 
      #                       style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px")),
      #   column(1,
      #          actionButton("addnewRE", label = "", icon = icon("plus"), 
      #                       style = "height:15px; width:15px; font-size:8px; padding:2px; line-height:8px"))),
      
      numericInput("Res_var", label = "Residual variance", value = 1),
      actionButton("calculate", "Calculate"),
      ),
    
    # Main Panel
    mainPanel(
      h3("Data structure:"),
      DT::dataTableOutput("designTab"),
      h3("Model summary:"),
      verbatimTextOutput("model"),
      h3("Solution from Power Analysis:"),
      verbatimTextOutput("power"),
      h3("Warnings:"),
      verbatimTextOutput("warnings")
    )
  )
)

# Define server logic
server <- function(input, output) {

  
  
  #### Fixed effects ####
  
  output$Ran_slopes0 <- renderUI({
    selectInput("Ran_slopes0", choices= unname(random_effects()), label = "Random slopes?",  multiple = T)
  })
  
  FE_counter <- reactiveVal(0)
  
  # Reactive value to store the existing values of text inputs
  FE_names <- reactiveValues()
  FE_levels <- reactiveValues()
  FE_ESs <- reactiveValues()
  Ran_slopes<- reactiveValues()
  FE_slopeSD <- reactiveValues()
  FE_corrs <- reactiveValues()
  
  # Observe the button click and add a new text input
  observeEvent(input$addnewFE, {
    # Increment the counter
    new_count_FE <- FE_counter() + 1
    FE_counter(new_count_FE)
    
    # Update the UI with the new input
    output$fixed_effect_inputs <- renderUI({
      # Generate a list of text input elements
      FE_input_list <- lapply(1:new_count_FE, function(i) {
        # Update entries of list
        fluidRow(
             column(12, textInput(paste0("FE_name", i), value = toupper(letters[i+1]), label = "Label")),
             column(6, textInput(paste0("FE_levels", i), value = paste0(toupper(letters[i+1]), c(1,2), collapse = ", "), label = "Levels")),
             column(6, textInput(paste0("FE_ES", i), value = "0.2", label = "Effect sizes")),
             column(4, selectInput(paste0("Ran_slopes", i), choices= unname(random_effects()), label = "Random slopes",  multiple = T)),
             column(4, textInput(paste0("FE_slopeSD", i), value = "", label = "Slope SD")),
             column(4, textInput(paste0("FE_corr", i), value = "", label = "Random effect correlations"))
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
          # Update list entries
          fluidRow(
            column(12, textInput(paste0("FE_name", i), value = toupper(letters[i+1]), label = "Label")),
            column(6, textInput(paste0("FE_levels", i), value = paste0(toupper(letters[i+1]), collapse = ", "), label = "Levels")),
            column(6, textInput(paste0("FE_ES", i), value = "0.2", label = "Effect sizes")),
            column(4, selectInput(paste0("Ran_slopes", i), choices= unname(random_effects()), label = "Random slopes",  multiple = T)),
            column(4, textInput(paste0("FE_slopeSD", i), value = "", label = "Slope SD")),
            column(4, textInput(paste0("FE_corr", i), value = "", label = "Random effect correlations"))
            
          )
        })
        do.call(tagList, FE_input_list)
      }})
    }
  })
  
  observeEvent(input$addFE, {
    shinyjs::hide("addFE")
    shinyjs::hide("removeFE")
  })
  
  # Observe changes to the text inputs and store their values
  observe({
    if (FE_counter() == 0){
      FE_names <- reactiveValues()
      FE_levels <- reactiveValues()
      FE_ESs <- reactiveValues()
      Ran_slopes <- reactiveValues()
      FE_slopeSD <- reactiveValues()
      FE_corrs <- reactiveValues()}
    else{
    lapply(1:FE_counter(), function(i) {
      FE_names[[paste0("FE_name", i)]] <- input[[paste0("FE_name", i)]]
      FE_levels[[paste0("FE_levels", i)]] <- input[[paste0("FE_levels", i)]]
      FE_ESs[[paste0("FE_ES", i)]] <- input[[paste0("FE_ES", i)]]
      Ran_slopes[[paste0("FE_name", i)]] <- input[[paste0("Ran_slopes", i)]]
      FE_slopeSD[[paste0("FE_slopeSD", i)]] <- input[[paste0("FE_slopeSD", i)]]
      FE_corrs[[paste0("FE_corr", i)]] <- input[[paste0("FE_corr", i)]]
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
    
    # Update the UI with the new input
    output$random_effect_inputs <- renderUI({
      # Generate a list of elements
      RE_input_list <- lapply(1:new_count_RE, function(i) {
        # Update list entries
        fluidRow(
          column(3, textInput(paste0("RE_name", i), value = paste0("RE", i+1), label = "Label")),
          column(3, numericInput(paste0("RE_N", i), value = 20, label = "N")),
          column(3, textInput(paste0("RE_var", i), value = "0.2", label = "Variance")),
          column(3, selectInput(paste0("RE_dropdown", i), label = "Between FEs", choices = unname(fixed_effects()), multiple = T))
        )
      })
      
      do.call(tagList, RE_input_list)
    })
  })
  
  # Observe the remove button click and remove the last RE
  observeEvent(input$removeRE, {
    new_count_RE <- RE_counter() - 1
    
    # Ensure the counter doesn't go below 0
    if (new_count_RE >= 0) {
      RE_counter(new_count_RE)
      
      # Update the UI to remove the last RE
      output$random_effect_inputs <- renderUI({
        if (new_count_RE == 0){NULL}
        else{
          RE_input_list <- lapply(1:new_count_RE, function(i) {
            # Update list entries
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
  
  # Observe changes to the inputs and store their values
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
  
  
  #### Extract parameter values ####

  fixed_effect_sizes <- reactive({
    as.numeric(c(0, str_split_1(input$FE_ES0, ","), as.numeric(c(reactiveValuesToList(FE_ESs)))))
  })
  
  fixed_levels <- reactive({
    c("FE_levels0" = input$FE_levels0, reactiveValuesToList(FE_levels))
  })

  fixed_effects <- reactive({
    c("FE_name0" = input$FE_name0, reactiveValuesToList(FE_names))
  })
  
  random_slopeSDs <- reactive({
    as.numeric(c(str_split_1(input$FE_slopeSD0, ","), as.numeric(c(reactiveValuesToList(FE_slopeSD)))))
  })
  
  int_slope_corrs <- reactive({
    na.omit(as.numeric(str_split_1(paste(input$FE_corr0, unlist(reactiveValuesToList(FE_corrs)), sep = ","), ",")))
  })
  
  random_effects <- reactive({
    c("RE_name0" = input$RE_name0, reactiveValuesToList(RE_names))
  })
  
  random_variances <- reactive({
    as.numeric(c(input$RE_var0, reactiveValuesToList(RE_vars)))
  })
  
  # here the random slopes are indexed by the fixed effects
  random_slopes <- reactive({
    c("FE_name0" = input$Ran_slopes0, reactiveValuesToList(Ran_slopes))
  })
  
  # this is to reverse so the random slopes are indexed by the random effects, which is necessary to write the formula
  random_slope_list <- reactive({
    lapply(1:(RE_counter() + 1), function(i) {
      reactive({
        fixed_effects()[sapply(names(fixed_effects()), function(fe) random_effects()[[paste0('RE_name', i-1)]] %in% random_slopes()[[fe]])]
      })
    })
  })
  
  # All levels in factors with random slopes
  all_levels_ran <- reactive({
    lapply(1:(FE_counter()+1), function(i){
      if (fixed_effects()[i] %in% random_slope_list()[[1]]()){
        str_split_1(fixed_levels()[[i]], ",")
      }
      else {"NONE"}
    })
  })
  
  # All levels
  all_levels <- reactive({
    lapply(1:(FE_counter()+1), function(i){
        str_split_1(fixed_levels()[[i]], ",")
    })
  })
  
  
  #### Generate dataset ####
  
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
    else{groups = str_split_1(input$RE_dropdown0, ",")
    }
    
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
    
    # Create dataset
    data <- design.codes(current_design)

    # Extend by number of repetitions
    data <- extend(data,
                   within = str_c(c(random_effects()[1:(RE_counter() +1)], fixed_effects()[1:(FE_counter()+1)]), collapse = "+"),
                   n = input$FE_N0)

    return(data)
    })
  

  
  #### Use makeLmer to generate specified model ####
  
  make_model <- reactive({
    
    # Fixed levels and effect sizes
    fixed <- str_c(fixed_effects()[1:(FE_counter()+1)], collapse = "+")
    levels_no <- length((unlist(all_levels())))
    fixef <- na.omit(unlist(fixed_effect_sizes()[1:levels_no], use.names = F))
    
    # Random effects and residual variance
    res = input$Res_var
    random = paste0(str_c(c(1, unlist(random_slope_list()[[1]]())), collapse = "+"), "|", random_effects()[1])
    
    # Determine size of variance-covariance matrix (=Levels of factors with random slopes - Number of Factor variables + 1)
    matrix_size <-length(unlist(all_levels_ran())) - FE_counter()
    
    # Define variance covariance matrix for various matrix sizes:
    if (matrix_size == 1){
      rand <- random_variances()[[1]]
    }
    
    if (matrix_size == 2){
      rand <- matrix(c(random_variances()[[1]],
                       int_slope_corrs()[[1]]*sqrt(random_variances()[[1]])*random_slopeSDs()[[1]],
                       int_slope_corrs()[[1]]*sqrt(random_variances()[[1]])*random_slopeSDs()[[1]],
                       random_slopeSDs()[[1]]^2), matrix_size)
    }
    
    if (matrix_size == 3){
      rand <- matrix(c(random_variances()[[1]],
                           int_slope_corrs()[[1]]*sqrt(random_variances()[[1]])*random_slopeSDs()[[1]],
                           int_slope_corrs()[[2]]*sqrt(random_variances()[[1]])*random_slopeSDs()[[2]],
                           int_slope_corrs()[[1]]*sqrt(random_variances()[[1]])*random_slopeSDs()[[1]],
                           random_slopeSDs()[[1]]^2,
                           int_slope_corrs()[[3]]*random_slopeSDs()[[1]]*random_slopeSDs()[[2]],
                           int_slope_corrs()[[2]]*sqrt(random_variances()[[1]])*random_slopeSDs()[[2]],
                           int_slope_corrs()[[3]]*random_slopeSDs()[[1]]*random_slopeSDs()[[2]],
                           random_slopeSDs()[[2]]^2),
                         matrix_size)
    }
 
    # Define formula
    formula = as.formula(paste0("y ~ ", fixed, "+ (", random, ")"))
    
    # Specify model
    model <- makeLmer(formula, fixef=fixef, VarCorr=rand, sigma=res, data=getDF())
  })
  
  
  
  #### Create design table to visualise data structure ####
  
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
    

  
  #### Create model summary ####
  
  output$model <- renderPrint({
    
    # check for errors
    if (length(str_split_1(input$FE_levels0, ",")) != length(str_split_1(input$FE_ES0, ",")) + 1) {
      stop("Incorrect number of effect sizes for levels of factor")
    }
    
    model <- make_model()
    return(model)

  })
  
  
  #### Calculate power ####
  
  calculate_power <- reactive({
    
    if (input$calculate > 0){
      sim <- powerSim(make_model(), nsim=100,
                    progress = F)
      return(sim)
    }
  })
 
  output$power <- renderPrint({
  
    if (input$calculate >0){
      power <- calculate_power()$x/100}
    else {power <- noquote(c(""))}
    
    return(c("Power:"=power))
 
 })

  output$warnings <- renderPrint({
    if (input$calculate >0){
      warnings <- calculate_power()$warnings
    }
  
    else {warnings <- noquote(c(""))}
  
    return(c("Warnings:"=warnings))
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
