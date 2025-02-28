# Load necessary libraries
library(shiny)
library(tidyverse)
library(ggplot2)

# Data URL
thesisurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTFNrRTErLkmfrH-70v4mrIhPFYIIHrwp7jrxHPhdXroFto2M7A5C32IkcdujJtztn7Z_dTFvcY2moJ/pub?output=csv"

# Read the data
Thesis <- read_csv(thesisurl)

# Define UI for the app
ui <- navbarPage(
  title = "Clarity Relationships",
  
  # Adding a tab for the main content
  tabPanel("Original Variable Regressions",
           sidebarLayout(
             sidebarPanel(
               # Dropdown to choose X variable for regression
               selectInput("xvar", "Choose X variable:",
                           choices = c("kd", "TSS", "CDOM", "CHLA", "Secchi")),
               
               # Dropdown to choose Y variable for regression
               selectInput("yvar", "Choose Y variable:",
                           choices = c("kd", "TSS", "CDOM", "CHLA", "Secchi")),
               
               # Submit button
               actionButton("submit", "Run Regression")
             ),
             
             mainPanel(
               h3("Original Study Regressions"),
               p("In this tab, you can select the X and Y variables to perform a linear regression. "
                 , "The goal of this analysis is to understand the relationships between measures of water clarity and water quality parameters such as total suspended solids (TSS), chromophoric dissolved organic matter (CDOM), and chlorophyll-a (CHLA). Once the variables are chosen, a regression plot will be displayed, below this plot, you will find the equation, R-squared, and P-value of the relationship. We recommend exploring kd as the y input with TSS, CDOM, and CHLa as your x inputs, this will replicate the process of our initial study analysis (Please refer to 'About the Variables' tab for units/descriptions)"),
               # Display the regression plot
               plotOutput("regressionPlot"),
               
               # Display regression statistics
               verbatimTextOutput("regressionStats")
             )
           )
  ),
  
  # New tab to choose between "Residual_kd", "CDOM", and "CHLA"
  tabPanel("Residual kd Regressions",
           sidebarLayout(
             sidebarPanel(
               # New selectInput for choosing a variable for Y axis
               selectInput("newvar", "Choose Y variable for Regression:",
                           choices = c("Residual_kd", "CDOM", "CHLA")),
               
               # New selectInput for choosing an X variable for second regression
               selectInput("newxvar", "Choose X variable for Regression:",
                           choices = c("Residual_kd", "CDOM", "CHLA")),
               
               # Submit button for this new regression
               actionButton("submitNew", "Run New Regression")
             ),
             
             mainPanel(
               h3("Residual kd Regressions"),
               p("After exploring the data initially using the data on the previous tab, TSS was identified to be the strongest predictor of kd. Using the equation, the proportion of kd due to TSS effects was subtracted from the initial kd. What we are left with is what we call 'Residual-kd'. Using the two other environmental variables (CDOM and CHLA), explore the relationships they have with Residual-kd. Does one seem to explain the left over attenuation more? Visit the 'About the Variables' tab to learn more about variables and units."),
               
               # Display the new regression plot
               plotOutput("newRegressionPlot"),
               
               # Display new regression statistics
               verbatimTextOutput("newRegressionStats")
             )
           )
  ),
  
  # Add another tab
  tabPanel("About the Variables",
           h3("Light Attenuation (kd)"),
           p(" The intensity of light decreases underwater as depth increases due to the scattering and absorption of incoming light. Light intensity sensors are used to determine the amount of light at 0.5 m intervals under the water surface. Graphing this produces a light attenuation curve. Taking the natural log of this curve gives a linear relationship, the slope of which is our light attenuation coefficient (kd) in units 'm-1'. Light attenuation is important due to the significance of light in determining dominant primary producers. Waterbodies with little clarity, phytoplankton typically dominates primary production. This leads to more frequent harmful algal blooms, the release of toxins, and less aquatic vegetation. Waterbodies with high clarity are able to better support colonies of submerged aquatic vegetation. These help reduce present nutrients and prevent sediment resuspension"),
           h3("Total Suspended Solids (TSS)"),
           p("Measure of the total mass of particulates suspended in water column (mg/L). This does not distinguish between abiotic and biotic influences, hence the reason CHLa is measured as a separate variable as well. Higher TSS = higher light scattering. Some impacts of TSS can include smothering of benthic communities necessary for ecosystems. High general TSS indicates sediment presence, high TSS during peak production suggests algal dominance. TSS can be an indication of a lack of sedimentation and too much resuspension of particulates. This can be mitigated by increased submerged aquatic vegetation communities"),
           h3("Chlorophyll-a (CHLa)"),
           p("Measure of photosynthetically active organics in water (ug/L) and indicates the presence of phytoplankton in water column. Typically due to excess nutrient inputs in the water. CHLa is typically higher during times of peak production when light is most available (summer/warmer months). Increased algal presence reduces the amount of submerged aquatic vegetation. Cyanobacteria (blue-green algae) is a common type of algae in blooms. Common form of prokaryotic bloom-forming algae in lakes that uses buoyancy from an internal gas vacuole to float to water surface. Produces Microcystin, a liver toxin, fatal to livestock and pets when consumed, potential human carcinogen"),
           h3("Chromophoric Dissolved Organic Matter (CDOM)"),
           p("Measure of the light absorbance due to watershed organic matter that has dissolved into waterbodies. Blocks specific wavelengths from penetrating water (440 nm). Decreased light from absorbance will limit energy sources for microbial food webs. Dependent on location and watershed inputs: sandy soils typically allow for more leaching of organic material through the watershed into lakes/reservoirs"))
)

#--------------------------------------------------------------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for the dataset (fixed to Thesis)
  datasetInput <- reactive({
    Thesis  # The dataset is now fixed to Thesis
  })
  
  # Render the regression plot after submit
  output$regressionPlot <- renderPlot({
    req(input$submit)  # Only render plot after submit is clicked
    
    data <- datasetInput()  # Get the selected dataset
    xvar <- input$xvar  # Get the selected x-variable
    yvar <- input$yvar  # Get the selected y-variable
    
    # Fit linear model using backticks for variable names with spaces
    lm_model <- lm(as.formula(paste("`", yvar, "` ~ `", xvar, "`", sep = "")), data = data)
    
    # Create the regression plot with lm model
    ggplot(data, aes_string(x = xvar, y = yvar, color = "Station")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      theme_minimal(base_size = 15) +  # Use minimal theme
      theme(
        plot.background = element_rect(fill = "#D2B48C", color = "#D2B48C"),  # Sandstone background color
        panel.background = element_rect(fill = "#F5DEB3"),  # Lighter sand color for the plot
        axis.text = element_text(color = "#4B3621"),  # Dark brown color for text
        axis.title = element_text(color = "#4B3621"),  # Dark brown for axis titles
        plot.title = element_text(color = "#4B3621")  # Dark brown for plot title
      ) +
      labs(title = paste("Regression Plot: ", xvar, " vs ", yvar),
           x = xvar, y = yvar) +
      scale_color_discrete(name = "Station")  # Adds a legend for the 'Station' variable
  })
  
  # Render the regression statistics after submit
  output$regressionStats <- renderPrint({
    req(input$submit)  # Only render stats after submit is clicked
    
    data <- datasetInput()  # Get the selected dataset
    xvar <- input$xvar  # Get the selected x-variable
    yvar <- input$yvar  # Get the selected y-variable
    
    # Fit linear model using backticks for variable names with spaces
    lm_model <- lm(as.formula(paste("`", yvar, "` ~ `", xvar, "`", sep = "")), data = data)
    
    # Extract statistics
    summary_model <- summary(lm_model)
    p_value <- summary_model$coefficients[2, 4]  # p-value for the slope
    r_squared <- summary_model$r.squared  # R-squared
    coef_intercept <- summary_model$coefficients[1, 1]  # Intercept
    coef_slope <- summary_model$coefficients[2, 1]  # Slope
    
    # Display regression statistics
    cat("Regression Equation: y =", round(coef_slope, 3), "x", "+", round(coef_intercept, 3), "\n")
    cat("R-squared:", round(r_squared, 3), "\n")
    cat("p-value for the slope:", round(p_value, 3), "\n")
  })
  
  # Render new regression plot after submitNew is clicked
  output$newRegressionPlot <- renderPlot({
    req(input$submitNew)  # Only render plot after submit is clicked
    
    data <- datasetInput()  # Get the selected dataset
    yvar <- input$newvar  # Get the selected y-variable
    xvar <- input$newxvar  # Get the selected x-variable
    
    # Fit linear model using backticks for variable names with spaces
    lm_model <- lm(as.formula(paste("`", yvar, "` ~ `", xvar, "`", sep = "")), data = data)
    
    # Create the regression plot with lm model
    ggplot(data, aes_string(x = xvar, y = yvar, color = "Station")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      theme_minimal(base_size = 15) +  # Use minimal theme
      theme(
        plot.background = element_rect(fill = "#D2B48C", color = "#D2B48C"),  # Sandstone background color
        panel.background = element_rect(fill = "#F5DEB3"),  # Lighter sand color for the plot
        axis.text = element_text(color = "#4B3621"),  # Dark brown color for text
        axis.title = element_text(color = "#4B3621"),  # Dark brown for axis titles
        plot.title = element_text(color = "#4B3621")  # Dark brown for plot title
      ) +
      labs(title = paste("Regression Plot: ", xvar, " vs ", yvar),
           x = xvar, y = yvar) +
      scale_color_discrete(name = "Station")  # Adds a legend for the 'Station' variable
  })
  
  # Render the new regression statistics after submitNew is clicked
  output$newRegressionStats <- renderPrint({
    req(input$submitNew)  # Only render stats after submit is clicked
    
    data <- datasetInput()  # Get the selected dataset
    yvar <- input$newvar  # Get the selected y-variable
    xvar <- input$newxvar  # Get the selected x-variable
    
    # Fit linear model using backticks for variable names with spaces
    lm_model <- lm(as.formula(paste("`", yvar, "` ~ `", xvar, "`", sep = "")), data = data)
    
    # Extract statistics
    summary_model <- summary(lm_model)
    p_value <- summary_model$coefficients[2, 4]  # p-value for the slope
    r_squared <- summary_model$r.squared  # R-squared
    coef_intercept <- summary_model$coefficients[1, 1]  # Intercept
    coef_slope <- summary_model$coefficients[2, 1]  # Slope
    
    # Display regression statistics
    cat("Regression Equation: y =", round(coef_slope, 3), "x", "+", round(coef_intercept, 3), "\n")
    cat("R-squared:", round(r_squared, 3), "\n")
    cat("p-value for the slope:", round(p_value, 3), "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
