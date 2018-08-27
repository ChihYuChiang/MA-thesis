source("analysis_preprocessing.R")
library(data.table)
DT_2_long <- getData_2()[[1]]
write.csv(DT_2_long, file="../data/raw_survey/processed/DT_2_long.csv")

#Predicting preference by respondent and game id
model_demean <- lm(`preference` ~ `respondent` + factor(`core_id`), DT_2_long)
summary(model_demean)

#Acquire the residules and add back the overall mean
res_demean <- DT_2_long[, .(`respondent`, `core_id`)][, c("res", "allmean") := .(residuals(model_demean) + model_demean$coefficients[1], model_demean$coefficients[1])]
  
write.csv(res_demean, file="../data/raw_survey/processed/res_demean.csv")
