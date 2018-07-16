source("analysis_preprocessing.R")
library(data.table)
DT_2_long <- getData_2()[[1]]

model_demean <- lm(`preference` ~ `respondent` + factor(`core_id`), DT_2_long)
summary(model_demean)

res_demean <- DT_2_long[, .(`respondent`, `core_id`)][, res := residuals(model_demean)]
  
write.csv(res_demean, file="../data/raw_survey/processed/res_demean.csv")
