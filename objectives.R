xgboost_objectives <- c(
  "reg:squarederror", # 1
  "reg:squaredlogerror", # 2
  "reg:logistic", # 3
  "binary:logistic", # 4
  "binary:logitraw", # 5 no
  "binary:hinge",
  "reg:linear", # 6
  "reg:pseudohubererror", # 7
  "count:poisson", # 8
  "survival:cox", # 9
  "reg:gamma", # 10
  "reg:tweedie", # 11
  "reg:absoluteerror", # 12
  "multi:softmax", # 13
  "multi:softprob", # 14
  "rank:pairwise", # 15
  "rank:ndcg", # 16
  "rank:map", # 17
  "binary:hinge", # 18
  "survival:aft" # 19
)

