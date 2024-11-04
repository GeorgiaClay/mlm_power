# Helper function to create a label with a subheading
label_with_subheading <- function(main_label, subheading) {
  HTML(paste0(main_label, "<br><span class='subheading'>", subheading, "</span>"))
}


# Define UI
shinyUI(fluidPage(
  useShinyjs(),
  
  # CSS to control subheading style
  tags$head(tags$style(HTML("
    .subheading {
      font-size: 0.7em;
      color: gray;
    }
  "))),
  
  titlePanel("Power Analysis for Multi-Level Models"),
  
  p(HTML("<ul>
         <li>Use this tool to carry out a power analysis for nested data</li>
         <li>Allows random intercept and slopes (up to 3 random effects, e.g. random intercept and slope for one factor with 3 levels, or random intercept and slope for two variables, each with 2 levels)</li>
         <li>Fixed effects are factor variables, with the first listed serving as the reference level</li>
         <li> When variables require multiple inputs (e.g. effect sizes of variables with more than 2 levels), separate inputs with a comma</li>
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
        column(6, textInput("FE_ES0", value = "0.2", label = label_with_subheading("Effect sizes", "Each level from reference separated by comma"))),
        column(4, htmlOutput("Ran_slopes0")),
        column(12, p("If you have a random slope, enter here the standard deviation of the slope of each level from the reference level. Then enter the random effects correlations, separated by a comma.")),
        column(6, textInput("FE_slopeSD0", value = "", label = "Slope SD")),
        column(6, textInput("FE_corr0", value = "", label = "Random effect correlations")),
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
        column(12, textInput("RE_name0", value = "Subject", label = "Cluster name")),
        column(6, numericInput("RE_N0", label = "N", value = "10")),
        column(6, htmlOutput("RE_dropdown0")),
        column(6, numericInput("RE_var0", label = "Intercept Variance", value = "0.5")),
        column(6, numericInput("Res_var", label = "Residual variance", value = 1))
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
)