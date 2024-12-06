#modeltraining.R
xgb_model <- function(training_data, nrounds, eta, max_depth, objective) {
  training.x <- model.matrix(failure ~ ., data = training_data)
  #create testing data
  #testing.x <- model.matrix(failure ~ ., data = prmdt)
  #create xgb model
  
  model.XGB <- xgboost(data = data.matrix(training.x[,-1]),
                       label = as.numeric(as.character(training_data$failure)),
                       eta = eta,
                       max_depth = max_depth,
                       nround = nrounds,
                       objective = objective,
                       #nfold = 20,
                       nthread = 100
  )
  
  return(model.XGB)
  
}