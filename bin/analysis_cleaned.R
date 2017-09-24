"
----------------------------------------------------------------------
## Setup
Data of game and player are read in and matched up.

- Game release data, `release` (year), is read in as an interval variable.
- Missing values are imputed with variable mean conveniently (`star_user` and `star_GS`).
----------------------------------------------------------------------
"
"
### Packages
"
library(tidyverse)
library(data.table)
library(corrplot)
library(modelr)
library(glmnet)
library(VGAM)
library(randomForest)
library(e1071)
library(car)
library(rlist)
library(pander)
set.seed(1)




"
### Read in data
"
#--Read in
#Core game info and group distance/probability data
core_cluster <- read_csv("../data/core_cluster.csv", col_names=TRUE) %>%
  mutate(group_survey = factor(group_survey),
         group_review = factor(group_review),
         core_id = factor(core_id)) %>%
  select(-X1)

#Core game tste scores (of dif numbers of features)
core_tsteScore <- read_csv("../data/tste_concat.csv", col_names=TRUE) %>%
  select(-X1)

#Core game traditional genre data
core_tGenre <- read_csv("../data/traditional_genre.csv", col_names=TRUE) %>%
  select(-X1, -group, -idTag, -game_title) %>%
  mutate(core_id = factor(core_id))
colnames(core_tGenre)[3:length(colnames(core_tGenre))] <- #Give genre columns identification
  unlist(lapply(X=colnames(core_tGenre)[3:length(colnames(core_tGenre))], function(X) {paste("tg_", X, sep="")}))

#Player survey data
survey <- read_csv("../data/survey.csv", col_names=TRUE) %>%
  mutate(race = factor(race),
         sex = factor(sex),
         core_id = factor(core_id)) %>%
  select(-id)


#--Impute missing with mean
imputation_mean <- function(c){
  c[is.na(c)] <- mean(c, na.rm=TRUE)
  return(c)
}
core_cluster <- mutate_each(core_cluster,
                            funs(imputation_mean(.)),
                            star_user, star_GS)


#--Match up
#Main df, key=player-game pair
df <- bind_cols(core_cluster, core_tsteScore) %>%
  left_join(core_tGenre, by=c("core_id")) %>%
  left_join(survey, by=c("core_id"), copy=FALSE)


#--Clean up unnecessary objs
rm(core_cluster, core_tsteScore, core_tGenre, survey, imputation_mean)








"
----------------------------------------------------------------------
## Variable
Acquire `player_df`; Compute and select variables to be used in models.

- Call the function to update the vars employed.
- Final response variable utilizes only `preference_3`.
- Mean-centered vars is marked with a suffix _ct.

- Player preference:
Name | Definition | Unit
-----|------------|------
`preference_1` | how much do you like | Likert 1-7=like
`preference_2` | how often play it | ordinary 1=never-7=everyday
`preference_3` | does it fit personal taste | Likert 1-7=fit

- Game characteristics:
Name | Definition | Unit
-----|------------|------
`distance_survey_mean_x` | group score from survey (distance from group mean in tste) | cosine distance
`distance_survey_median_x` | group score from survey (distance from group median in tste) | cosine distance
`probability_review_mean_x` | group score from review (mean probability to be categorized in the group by NN) | percentage
`probability_review_median_x` | group score from review (median probability to be categorized in the group by NN) | percentage
`group_survey` | group identity from survey | categorical 1-group number
`group_review` | group identity from review | categorical 1-group number
`tste_n_x` | group score from survey (tste), n=number of features | interval arbitrary
`tg_x` | if belongs to traditional genre categories | binary

- Player personality:
Name | Definition | Unit
-----|------------|------
`game_xxxxx` | Big-five personality in game | Likert 1-7
`real_xxxxx` | Big-five personality in real life | Likert 1-7
`gap_xxxxx` | personality gap (game - real) | Likert 1-7
`satis_xxxxx` | SDT satisfaction in real life | Likert 1-7
`dissatis_xxxxx` | SDT dissatisfaction in real life | Likert 1-7
`combined_xxxxx` | SDT combined (previous two) dissatisfaction in real life | Likert 1-7

- Control:
Name | Definition | Unit
-----|------------|------
`age` | player age | interval
`education` | player education | ordinary 1-7=PhD
`income` | player annual household income | ordinary 1-7=over 150,000 
`sex` | player sex | categorical 1=male
`race` | player race | categorical 1-5
`release` | game release year | interval year
`star_GS` | general game quality rated by GameSpot expert | interval 0-10
`star_user` | general game quality rated by GameSpot user | interval 0-10
----------------------------------------------------------------------
"
updateVars <- function(df.outcome="preference", df_player.outcome="game_extraversion"){
  #--Create response variable
  df <<- df %>%
    rowwise() %>% #Rowwise to make the ordinary functions work
    mutate(preference = mean(c(preference_3))) %>%
    ungroup() #Ungroup to cancel rowwise
  

  #--Mean-center predictor variables
  df <<- mutate_at(df, vars(starts_with("tste"),
                            starts_with("game"),
                            starts_with("real"),
                            starts_with("satis"),
                            starts_with("dissatis"),
                            starts_with("combined")), funs(ct = . - mean(.)))


  #--Compute personalty gap
  df <<- mutate(df,
                gap_extraversion = game_extraversion - real_extraversion,
                gap_agreeableness = game_agreeableness - real_agreeableness,
                gap_conscientiousness = game_conscientiousness - real_conscientiousness,
                gap_emotionstability = game_emotionstability - real_emotionstability,
                gap_openness = game_openness - real_openness,
                gap_sum = gap_extraversion + gap_agreeableness + gap_conscientiousness + gap_emotionstability + gap_openness,
                real_sum = real_extraversion + real_agreeableness + real_conscientiousness + real_emotionstability + real_openness,
                dissatis_sum = dissatis_autonomy + dissatis_relatedness + dissatis_competence,
                satis_sum = satis_autonomy + satis_relatedness + satis_competence,
                combined_sum = combined_autonomy + combined_relatedness + combined_competence
                )

  
  #--Acquire player df, key=player
  df_player <<- distinct(df, respondent, .keep_all=TRUE)
  
  
  #--Select variables to be included in regression (model formation)
  #Sets of predictor variables from file
  df_predictors <- read.csv("../data/vars/predictors.csv", header=TRUE, na.strings="")

  #Get column name as model id
  modelId <- colnames(df_predictors)
  
  #predictor variable as strings for each model
  predictorString <- apply(df_predictors, MARGIN=2, function(x) paste(na.omit(x), collapse="+"))
  
  #Make the dfs into a data frame
  dfs <<- data.frame(predictorString, row.names=modelId, stringsAsFactors=FALSE) %>%
    mutate(df_x = map(predictorString, ~ model.matrix(as.formula(paste(df.outcome, " ~ ", .x, sep="")), data=df)[, -1])) %>% #df with only predictor variables; [, -1] used to remove redundant intercept column
    mutate(df_yx = map(df_x, ~ bind_cols(select(df, df.outcome), data.frame(.x)))) #df also with outcome variables
  dfs_player <<- data.frame(predictorString, row.names=modelId, stringsAsFactors=FALSE) %>%
    mutate(df_x = map(predictorString, ~ model.matrix(as.formula(paste(df_player.outcome, " ~ ", .x, sep="")), data=df_player)[, -1])) %>% #df with only predictor variables; [, -1] used to remove redundant intercept column
    mutate(df_yx = map(df_x, ~ bind_cols(select(df_player, df_player.outcome), data.frame(.x)))) #df also with outcome variables
  
  #Set row names for reference
  row.names(dfs) <<- modelId
  row.names(dfs_player) <<- modelId
}

updateVars()








"
----------------------------------------------------------------------
## Models
Models applying the variables selected. Two ways to select variables:

- Use `select` to include vars in the models from `df`/`df_player`.
- Edited through `predictors.csv` (for complexed interaction terms).
- Column name of `predictors.csv` = model id
----------------------------------------------------------------------
"
....Models <- function() {}




"
### Tobit Model
"
........TobitFull <- function() {}

#Train models
dfs_player$model_tobit <- map(dfs_player$df_yx_selected,
                              ~ vglm(game_agreeableness ~ ., data=.x, family=tobit(Upper=7, Lower=1, imethod=1)))

#Summary
for(model in dfs_player$model_tobit) print(summary(model))
summary(dfs["gap_8_i", "model_tobit"][[1]])




"
### Simple linear model
"
........SimpleFull <- function() {}

#Train models
dfs$model_lm <- map(dfs$df_yx_selected, ~ lm(preference ~ ., data=.x))
dfs_player$model_lm <- map(dfs_player$df_yx_selected, ~ lm(gap_extraversion ~ ., data=.x))
model_lm <- lm(preference ~ ., data=df_c_selected) 

#Summary
for(model in dfs_player$model_lm) print(summary(model))
summary(dfs["gap_8_i", "model_lm"][[1]])




"
### Double Lasso variable selection

- Based on paper `Using Double-Lasso Selection for Principled Variable Selection`
- by Oleg Urminsky, Christian Hansen, and Victor Chernozhukov
"
........DoubleLasso <- function() {}


#--Function for updating lambda used in selection
#n = number of observation; p = number of independent variables; se = standard error of residual or dependent variable
updateLambda <- function(n, p, se) {se * (1.1 / sqrt(n)) * qnorm(1 - (.1 / log(n)) / (2 * p))}


#--Function for acquiring the indices of the selected variables in df_x
#df_x = matrix with only variables to be tested; y = dependent variable or treatment variables; lambda = the initial lambda computed in advance 
acquireBetaIndices <- function(df_x, y, lambda, n, p) {
  #Update lambda k times, k is selected based on literature
  k <- 1
  while(k < 15) {
    model_las <- glmnet(x=df_x, y=y, alpha=1, lambda=lambda, standardize=TRUE)
    beta <- coef(model_las)
    residual.se <- sd(y - predict(model_las, df_x))
    lambda <- updateLambda(n=n, p=p, se=residual.se)
    k <- k + 1
  }
  
  #Return the variable indices with absolute value of beta > 0
  return(which(abs(beta) > 0))
}


#--Function to perform double lasso selection
#df_yx = df with all variables; outcomeVar = string of the outcome var in the df; form = a switch to decide the test and treatment vars
#output = a new df_yx with variables selected from df_yx
lassoSelect <- function(df_yx, outcomeVar, form) {
  #--Setting up
  #The df with y and treatment variables (those vars will not be tested, and will always be included in the output df)
  df_ytreatment <- switch(form,
                          "1"=select(df_yx, matches(outcomeVar), matches("^real.+\\D_ct$"), matches("^game.+\\D_ct$"), matches("^gap.+[a-z]{4}$"), matches("^tste.+\\d_ct$")),
                          "2"=select(df_yx, matches(outcomeVar)),
                          "3"=select(df_yx, matches(outcomeVar), matches(sub("game", "real", outcomeVar))))

  #The df with only the variables to be tested (those vars will be tested, and not necessarily be included in the output df)
  df_test <- switch(form,
                    "1"=data.matrix(select(df_yx, -matches(outcomeVar), -matches("^real.+\\D_ct$"), -matches("^game.+\\D_ct$"), -matches("^gap.+[a-z]{4}$"), -matches("^tste.+\\d_ct$"))),
                    "2"=data.matrix(select(df_yx, -matches(outcomeVar))),
                    "3"=data.matrix(select(df_yx, -matches(outcomeVar), -matches(sub("game", "real", outcomeVar)))))
  
  #The number of observations
  n <- nrow(df_test)
  
  #The number of variables to be tested
  p <- ncol(df_test)
  
  
  #--Select vars that predict outcome
  #Lambda is initialized as the se of residuals of a simple linear using only treatments predicting dependent variable
  #If the treatment var is NULL, use the se pf dependent var to initiate
  residual.se <- if(ncol(df_ytreatment) == 1) {sd(df_yx[[outcomeVar]])} else {sd(residuals(lm(as.formula(paste(outcomeVar, " ~ .", sep="")), data=df_ytreatment)))}
  lambda <- updateLambda(n=n, p=p, se=residual.se)
  
  #by Lasso model: dependent variable ~ test variables
  betaIndices <- acquireBetaIndices(df_x=df_test, y=df_yx[[outcomeVar]], lambda=lambda, n=n, p=p)
  
  
  #--Select vars that predict treatments
  #Each column of the treatment variables as the y in the Lasso selection
  #Starting from 2 because 1 is the dependent variable
  if(ncol(df_ytreatment) != 1) { #Run only when treatment vars not NULL
    for(i in seq(2, ncol(df_ytreatment))) {
      #Acquire target treatment variable
      treatment <- df_ytreatment[[i]]
      
      #Lambda is initialized as the se of the target treatment variable
      treatment.se <- sd(treatment)
      lambda <- updateLambda(n=n, p=p, se=treatment.se)
      
      #Acquire the indices and union the result indices of each treatment variable
      betaIndices <- union(betaIndices, acquireBetaIndices(df_x=df_test, y=treatment, lambda=lambda, n=n, p=p))
    }
  }
  
  
  #Process the result indices to remove the first term (the interaction term)
  betaIndices <- setdiff((betaIndices - 1), 0)
  
  #Bind the selected variables with dependent and treatment variables
  df_yx_selected <- cbind(df_ytreatment, df_test[, betaIndices])
  
  #Return a new df_yx with variables selected
  return(df_yx_selected)
}


#--Update vars
updateVars(df.outcome="preference", df_player.outcome="game_agreeableness")


#--Use the function to acquire the selected dfs (the new dfs can be fed into the simple linear model)
dfs$df_yx_selected <- map(dfs$df_yx, ~ lassoSelect(., outcomeVar="preference", form="1"))
dfs_player$df_yx_selected <- map(dfs_player$df_yx, ~ lassoSelect(., outcomeVar="game_agreeableness", form="3"))








"
----------------------------------------------------------------------
## Information criteria
----------------------------------------------------------------------
"
...InformationCriteria <- function() {}




"
### BIC and BIC difference
"
........BIC <- function() {}


#Computation
dfs$BIC <- unlist(map(dfs$model_lm, BIC))
dfs$BIC_dif <- dfs$BIC - lag(dfs$BIC)

#Plot BIC
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), BIC)) +
  labs(x="Model", y="BIC") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId)

#Plot BIC difference
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), BIC_dif)) +
  labs(x="Model", y="BIC difference") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId) +
  geom_hline(yintercept=0, linetype=3)




"
### AIC and AIC difference
"
........AIC <- function() {}


#Computation
dfs$AIC <- unlist(map(dfs$model_lm, AIC))
dfs$AIC_dif <- dfs$AIC - lag(dfs$AIC)

#Plot AIC
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), AIC)) +
  labs(x="Model", y="AIC") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId)

#Plot AIC difference
ggplot() +
  geom_line(data=dfs, mapping=aes(seq(1, dim(dfs)[1]), AIC_dif)) +
  labs(x="Model", y="AIC difference") +
  scale_x_continuous(breaks=seq(1, dim(dfs)[1]), labels=dfs$modelId) +
  geom_hline(yintercept=0, linetype=3)








"
----------------------------------------------------------------------
## Description
----------------------------------------------------------------------
"
...Description <- function() {}




"
### Descriptive stats
"
........DescriptiveStats <- function() {}

#Update vars
updateVars()

#Get stats
summary(df)


#--Distribution of real and game personality (comparison)
dist_personality <- function(personality){
  #Acquire specific column by reg
  real <- select(df_player, matches(paste("^real.*", personality, "$", sep="")))
  game <- select(df_player, matches(paste("^game.*", personality, "$", sep="")))
  
  #Plot: red bars = game personality
  ggplot(data=as.data.frame(real, game)) +
    geom_histogram(mapping=aes(x=real), binwidth=1) +
    geom_histogram(mapping=aes(x=game), binwidth=1, fill="red", alpha=0.5) +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7)) +
    labs(x="score", title=personality)
}

#Call function for each personality
dist_personality("agreeableness")
dist_personality("conscientiousness")
dist_personality("emotionstability")
dist_personality("extraversion")
dist_personality("openness")




"
### Correlation
"
........Correlation <- function() {}

#Full matrix
cor(select(df, which(sapply(df, is.numeric))))

#Preference ~ game characteristics
corrplot(cor(select(df, preference, starts_with("distance_survey_mean"))),
         method="color", type="upper", addCoef.col="black", diag=FALSE, tl.srt=45, tl.cex=0.8, tl.col="black")

#Preference ~ player personality
corrplot(cor(select(df, preference, starts_with("gap"), starts_with("combined"))),
         method="color", type="upper", addCoef.col="black", diag=FALSE, tl.srt=45, tl.cex=0.8, tl.col="black")




"
### Difference between real and game personality
"
........Ttest <- function() {}

#Observation
mean(df_player$real_agreeableness)
mean(df_player$game_agreeableness)

#T test for each pair
#T test result dif: game - real
t.test(df_player$game_agreeableness, df_player$real_agreeableness, paired=TRUE)
t.test(df_player$game_conscientiousness, df_player$real_conscientiousness, paired=TRUE)
t.test(df_player$game_extraversion, df_player$real_extraversion, paired=TRUE)
t.test(df_player$game_emotionstability, df_player$real_emotionstability, paired=TRUE)
t.test(df_player$game_openness, df_player$real_openness, paired=TRUE)
