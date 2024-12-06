
library(shiny)
library(bslib)
library(shinythemes)
library(gt)
library(DT)


options(shiny.maxRequestSize = 30 * 1024^2)

# Define UI for application that draws a histogram
fluidPage(

    theme = shinytheme("cerulean"),
    # Add custom CSS
    tags$style(HTML("
    .align-left {
      text-align: left;
    }
  ")),
    titlePanel("Predictive Maintenance for Ball-Bearing Grease"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          
          
          # Render the hyperparameters
          h3("Hyperparameter Tuning"),
          tags$br(),
          # Render the nrounds
          p("nrounds: max number of boosting iterations."),
          sliderInput("nrounds", "round value:", min = 10, max = 1000, value = 10, step = 100),
          # Render the eta
          p("eta: learning rate. A lower value implies larger nrounds and more robust to overfitting but slower to compute."),
          sliderInput("eta", "eta value", min = 0.01, max = 1, value = 0.3, step = 0.05),
          # Render the max depth
          p("max depth: maximum depth of a tree."),
          sliderInput("max_depth", "max depth value", min = 1, max = 100, value = 6, step = 1),
          # Render the objective
          p("objective: defines the type of problem the model is solving."),
          tags$ul(
            tags$li('"reg:squarederror", for regression.'),
            tags$li('"binary:logistic", for binary classification.'),
            tags$li('"multi:softmax" for multi-class classification')),
          selectInput("objective", "Objective Selection:", choices = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Data Preview", 
                     
                     h3("Data Loading"),
                     fileInput("file", "Choose CSV File",
                               accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"
                               )),
                     uiOutput("select_columns_ui"),
                     h3("Filtered Data"),
                     DTOutput("selected_columns_table"),
                     tags$br(),
                     uiOutput("table_title"),
                     tags$br(),
                     #DTOutput("csv_table"),
                     tags$br(),
                     tags$br(),
                     uiOutput("corrplot_title"),
                     plotOutput("correlation_plot")
                     
            ),
            tabPanel("Train Model", 
                     h3("Here'a where you can train your model"),
                     p("Click on the button to start training"),
                     actionButton("trainBtn", "Train Model"),
                     uiOutput("waitForModel"),
                     br(),
                     uiOutput("modelOutputTitle"),
                     fluidRow(
                      column(5, gt_output("modelOutput"))
                       ),
                     plotOutput("feature_importance_plot")
                     #h3("True vs Predicted Values by Device"),
                     #plotOutput("deviceComparisonPlot")
            ),
            tabPanel("Testing",
                     h3("In here you can test your model"),
                     DTOutput("predictionsFromTest")
                     #h3("Graphical Evaluation of Predictions"),
                     #plotOutput("trueVsPredictedPlot")

            ),
            tabPanel("Metrics",
                     h3("Confusion Matrix and other metrics"),
                     h4("Confusion Matrix"),
                     plotOutput("confusionMatrix"),
                     h4("High Level Metrics"),
                     tableOutput("perfMetrics"),
                     plotOutput("rocPlot"),
                     
                     h4("Interpretation of AUC Values"),
                     dataTableOutput("aucTable"),
                     br(),
                     br()
                  
                     
            )
            
        )
    )
)
)