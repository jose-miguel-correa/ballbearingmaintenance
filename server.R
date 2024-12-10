# server.R
source("objectives.R")
source("modeltraining.R")
source("testing.R")

library(gt)
library(dplyr)
library(DT)
library(xgboost)
library(corrplot)
library(grid)
library(yardstick)
library(purrr)
library(gridExtra)
library(pROC)
library(ggplot2)

rsconnect::setAccountInfo(name='user',
                          token='token',
                          secret='secret')

server <- function(input, output, session) {
  
  # Read and reactively handle the uploaded CSV data
  csv_data <- reactive({
    req(input$file) # Ensure a file is uploaded
    read.csv(input$file$datapath, header = TRUE)
  })
  
  # UI to select columns
  output$select_columns_ui <- renderUI({
    req(csv_data())
    selectInput("select_columns", "Select Columns to Display:",
                choices = names(csv_data()),
                multiple = TRUE)
  })
  # Render selected columns table
  output$selected_columns_table <- renderDataTable({
    req(csv_data(), input$select_columns)
    selected_data <- csv_data()[, input$select_columns, drop = FALSE]
    datatable(selected_data)
  })
  
  
  
  
  model <- reactiveVal(NULL)
  
  
  updateSelectInput(session, "objective", choices = xgboost_objectives, selected = xgboost_objectives[4])
  
  observeEvent(input$trainBtn, {
    req(csv_data(), input$select_columns)
    selected_data <- csv_data()[, input$select_columns, drop = FALSE]
    nrounds <- input$nrounds
    eta <- input$eta
    max_depth <- input$max_depth
    objective <- input$objective
    
    withProgress(message = "Training model...", value = 0, {
      for (i in seq_len(nrounds)) {
        # Simulate training step
        Sys.sleep(0.1)  # Add delay to simulate training progress
        
        
      }
      incProgress(1 / nrounds, detail = paste("Round", i, "of", nrounds))
      trained_model <- xgb_model(selected_data, nrounds, eta, max_depth, objective)
      model(trained_model)
    })
  })
  
  # Create folder
  models_dir <- "models"
  if (!dir.exists(models_dir)) dir.create(models_dir)
  
  # Reactive value to track the available models
  available_models <- reactiveVal({
    list.files(models_dir, pattern = "\\.rds$", full.names = FALSE)
  })
  
  # Save the current xgboost model
  observeEvent(input$saveModelBtn, {
    req(model()) # Ensure a model exists
    model_name <- paste0("model_", Sys.Date(), "_", Sys.time(), ".rds")
    model_path <- file.path(models_dir, model_name)
    saveRDS(model(), model_path)
    available_models(list.files(models_dir, pattern = "\\.rds$", full.names = FALSE))
    updateSelectInput(session, "selectedModel", choices = available_models(), selected = model_name)
    showNotification("Model saved successfully!", type = "message")
  })
  
  # Plotear importancia de las características
  output$feature_importance_plot <- renderPlot({
    req(model())
    importance_matrix <- xgb.importance(model = model())
    xgb.plot.importance(importance_matrix)
  })

output$modelOutput <- render_gt({
  if (is.null(model())) {
    # Placeholder when the model is not trained
    data.frame(
      Message = "Model has not been trained yet."
    ) %>%
      gt() %>%
      tab_header(
        title = "Model Status"
      )
  } else {
    # Extract evaluation log from the model
    eval_log <- model()$evaluation_log  
    
    # Identify the appropriate metric dynamically
    metric_name <- names(eval_log)[which(grepl("train", names(eval_log)))][1]  # Detect first train metric
    metric_label <- gsub("train_", "", metric_name)  # Extract metric name (e.g., logloss or rmse)

    # Get the first 2 and last 2 values of the detected metric
    first_rows <- head(eval_log[[metric_name]], 2)
    last_rows <- tail(eval_log[[metric_name]], 2)
    
    # Combine the extracted values into a data frame
    train_metrics <- data.frame(
      "DESCRIPTION" = c("First Values", "", "Last Values", ""),
      "VALUE" = as.numeric(c(first_rows, last_rows))  # Ensure numeric type
    )
    
    # Render the data frame as a gt table with row-specific coloring
    train_metrics %>%
      gt() %>%
      tab_header(
        title = paste("Train", toupper(metric_label)),
        subtitle = "First and Last Values"
      ) 
  }
})

  
  output$modelOutputTitle <- renderUI({
    if (!is.null(model())) {
      h3("binary:hinge had a good behaviour, so check it out")
    } else {
      NULL
    }
  })
  
  # Render the dataset table
  output$csv_table <- renderDataTable({
    req(csv_data())
    datatable(csv_data())  # Display CSV data in a table
  })
  

  
  data4 <- reactive({
    req(model())
    req(csv_data(), input$select_columns)
    selected_data <- csv_data()[, input$select_columns, drop = FALSE]
    #data_test <- csv_data()[, 3:12]
    test_matrix <- xgb.DMatrix(data.matrix(selected_data[, -1]))
    predictions <- predict(model(), test_matrix)
    
    predictions <- as.numeric(sprintf("%.4f", predictions))
    data3 <- cbind(csv_data(), predictions = predictions)
    data3 <- as.data.frame(data3)
    
    # Return the selected columns as a dataframe
    data3 %>% select(failure, predictions)
  })
  
  # Render predictions table
  output$predictionsFromTest <- renderDataTable({
    req(data4())  # Ensure data4 is available
    datatable(data4())
  })
  
  # Render confusion matrix table
  cm <- reactive({
    req(data4())  # Ensure data4 is available
    data4 <- data4()
    data4$failure <- as.factor(data4$failure)
    data4$estimate <- as.factor(ifelse(data4$predictions >= 0.5, 1, 0))
    data4 %>% select(failure, estimate)
    
    cm <- data4 %>%
      conf_mat(failure, estimate) %>%
      tidy()
    
    cm
  })
  
  output$confusionMatrix <- renderPlot({
    req(cm())
    cm <- cm()
    cm_value <- as.list(cm$value)
    cm_matrix <- matrix(
      as_vector(cm_value),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(
        c("Predicted 0", "Predicted 1"),
        c("Truth 0", "Truth 1")
      )
    )
    table <- tableGrob(cm_matrix)

    # Add a title and arrange
    grid.arrange(
      textGrob("Confusion Matrix", gp = gpar(fontsize = 16, fontface = "bold")),
      table,
      nrow = 2,
      heights = c(0.2, 0.8)
    )
  })
  
  output$perfMetrics <- renderTable({
     req(cm())
     cm <- cm()
     cm$value
     #TN        FN    FP     TP
     #124388      0     44     62
     
      TN <- cm$value[1]
      FN <- cm$value[2]
      FP <- cm$value[3]
      TP <- cm$value[4]
      total <- as.numeric(nrow(csv_data()))
     
      #Accuracy
      model_accuracy_value <- (TP + TN) / total
      accuracy_value <- sprintf("%.2f%%", model_accuracy_value*100)
      accuracy_value
     
      #Precision
      model_precision_value <- TP / (TP + FP)
      precision_value <- sprintf("%.2f%%", model_precision_value*100)
      precision_value
     
      #Recall (sensitivity)
      model_recall_value <- TP / (TP + FN)
      recall_value <- sprintf("%.2f%%", model_recall_value*100)
      recall_value
     
      #Specificity
      model_specificity_value <- TN / (TN + FP)
      specificity_value <- sprintf("%.2f%%", model_specificity_value*100)
      specificity_value
     
      #F1 Score
      f1_model_value <- 2 * (model_precision_value * model_recall_value) / (model_precision_value + model_recall_value)
      f1_value <- sprintf("%.2f%%", f1_model_value*100)
      f1_value
      
      performance_parameters <- data.frame(
        Index = c("Accuracy", "Precision", "Recall", "Specificity", "F1-Score"),
        Value = c(accuracy_value, precision_value, recall_value, specificity_value, f1_value)
      )
      performance_parameters

  })
  
  # Render titles and plots conditionally
  output$table_title <- renderUI({
    if (isTruthy(csv_data())) {
      h3("Explore your data below")
    }
  })
  
  selected_data <- reactiveVal(NULL)
  output$correlation_plot <- renderPlot({
    req(csv_data(), input$select_columns)
    selected_data <- csv_data()[, input$select_columns, drop = FALSE]
    numeric_cols <- select(selected_data, where(is.numeric))
    if (ncol(numeric_cols) > 1) {
      corr_matrix <- cor(numeric_cols, use = "complete.obs")
      corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
    }
  })
  
  output$corrplot_title <- renderUI({
    if (isTruthy(selected_data())) {
      h3("Correlation Plot")
    }
  })
  
  output$rocPlot <- renderPlot({
    req(data4())  # Ensure predictions and truth values are available
    data <- data4()
    
    # Ensure the actual labels (truth values) are numeric
    actual_labels <- as.numeric(as.character(data$failure))
    
    # Get the predicted probabilities
    predicted_probs <- as.numeric(data$predictions)
    
    # Create the ROC object
    roc_obj <- roc(actual_labels, predicted_probs)
    
    # Plot the ROC curve
    plot(
      roc_obj,
      main = "ROC Curve",
      col = "blue",
      lwd = 2,
      legacy.axes = TRUE,
      xlab = "False Positive Rate",
      ylab = "True Positive Rate"
    )
    
    # Display the AUC value
    auc_value <- auc(roc_obj)
    legend("bottomright", legend = sprintf("AUC = %.4f", auc_value), col = "blue", lwd = 2)
  })
  
  output$aucTable <- renderDataTable({
    auc_data <- data.frame(
      `AUC Value` = c("0.90–1.00", "0.80–0.90", "0.70–0.80", "0.60–0.70", "0.50–0.60", "0.50"),
      `Model Performance` = c(
        "Excellent (highly accurate, near-perfect separation of classes)",
        "Good (strong model performance, effective at distinguishing classes)",
        "Fair (acceptable, but there's room for improvement)",
        "Poor (weak model, struggling to distinguish between classes)",
        "Fail (no discriminatory ability, performs close to random guessing)",
        "Random (pure chance; the model is no better than random guessing)"
      )
    )
    datatable(auc_data, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  output$trueVsPredictedPlot <- renderPlot({
    req(csv_data(), input$select_columns)
    data <- csv_data()[, input$select_columns, drop = FALSE]
    
    # Convert true and predicted values to numeric
    true_values <- as.numeric(as.character(data$failure))
    predicted_values <- as.numeric(data$predictions)
    
    # Create the scatter plot
    plot(
      predicted_values, true_values,
      xlab = "Predicted Values",
      ylab = "True Values",
      main = "True vs Predicted Values",
      col = "blue", pch = 16, cex = 0.8
    )
    
    # Add the ideal 45-degree line
    abline(0, 1, col = "red", lwd = 2, lty = 2)
    
    # Optional: Add a legend
    legend("topleft", legend = c("Predictions", "Ideal Line"),
           col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 2), lwd = c(NA, 2))
  })
  
  output$deviceComparisonPlot <- renderDT({
    req(csv_data(), input$select_columns)
    data <- csv_data()[, input$select_columns, drop = FALSE]
    
    # Extract device, true values, and predicted values
    device <- as.factor(data$device)  # Ensure device is a factor for grouping
    true_values <- as.numeric(as.character(data$failure))
    predicted_values <- as.numeric(data$predictions)
    
    # Combine data into a single frame
    comparison_data <- data.frame(
      Device = device,
      True = true_values,
      Predicted = predicted_values
    )
    datatable(comparison_data)
    
    # Create a grouped bar plot
    # library(ggplot2)
    # ggplot(comparison_data, aes(x = Device)) +
    #   geom_bar(aes(y = True, fill = "True Values"), stat = "identity", position = "dodge", alpha = 0.7) +
    #   geom_bar(aes(y = Predicted, fill = "Predicted Values"), stat = "identity", position = "dodge", alpha = 0.7) +
    #   scale_fill_manual(values = c("True Values" = "blue", "Predicted Values" = "orange")) +
    #   labs(
    #     title = "True vs Predicted Values by Device",
    #     x = "Device",
    #     y = "Value",
    #     fill = "Legend"
    #   ) +
    #   theme_minimal() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Update the dropdown menu with available models
  output$modelDropdown <- renderUI({
    selectInput("selectedModel", "Select a Model:", 
                choices = available_models(), 
                selected = NULL)
  })
  
  # Delete the selected model
  observeEvent(input$deleteModelBtn, {
    req(input$selectedModel)  # Ensure a model is selected
    model_path <- file.path(models_dir, input$selectedModel)
    if (file.exists(model_path)) {
      file.remove(model_path)
      available_models(list.files(models_dir, pattern = "\\.rds$", full.names = FALSE))  # Refresh model list
      showNotification("Model deleted successfully!", type = "warning")
    } else {
      showNotification("Model not found!", type = "error")
    }
  })
  
  # Load a selected model
  observeEvent(input$loadModelBtn, {
    req(input$selectedModel)  # Ensure a model is selected
    model_path <- file.path(models_dir, input$selectedModel)
    loaded_model <- readRDS(model_path)
    model(loaded_model)
    showNotification("Model loaded successfully!", type = "message")
  })
  
  # Update the dropdown menu with available models
  observe({
    updateSelectInput(
      session,
      "selectedModel",
      choices = available_models(),
      selected = NULL
    )
  })
  
  output$modelList <- renderTable({
    data.frame(Model_Name = available_models())
  })
  
  # Display the list of available models in a table
  output$modelList <- renderTable({
    data.frame(Model_Name = available_models())
  })
  
}
