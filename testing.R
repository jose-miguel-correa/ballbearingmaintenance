test_function <- function(model, data) {
  data_test <- data[, 3:12]
  test_matrix <- xgb.DMatrix(data.matrix(data_test[, -1])) 
  predictions <- predict(model, test_matrix)
  return(predictions)
  
}