MAPSS Thesis II - model 1
================
Chih-Yu Chiang
September 05, 2017

-   [Setup](#setup)
-   [Variable](#variable)
-   [Model specification](#model-specification)
-   [Double Lasso selection](#double-lasso-selection)
-   [Simple linear models](#simple-linear-models)
-   [Information criteria](#information-criteria)
    -   [preference ~ tste + personality + (interaction)](#preference-tste-personality-interaction)
    -   [(link for the above position)](#link-for-the-above-position)
-   [Model summaries (Lasso select only interactions)](#model-summaries-lasso-select-only-interactions)
    -   [preference ~ tste + real](#preference-tste-real)
    -   [(link for the above position)](#link-for-the-above-position-1)
    -   [preference ~ tste + real + real\*tste](#preference-tste-real-realtste)
    -   [(link for the above position)](#link-for-the-above-position-2)
    -   [preference ~ tste + real + game](#preference-tste-real-game)
    -   [(link for the above position)](#link-for-the-above-position-3)
    -   [preference ~ tste + real + game + game\*tste](#preference-tste-real-game-gametste)
    -   [(link for the above position)](#link-for-the-above-position-4)
    -   [preference ~ tste + real + gap](#preference-tste-real-gap)
    -   [(link for the above position)](#link-for-the-above-position-5)
    -   [preference ~ tste + real + gap + gap\*tste](#preference-tste-real-gap-gaptste)
    -   [(link for the above position)](#link-for-the-above-position-6)
-   [Model summaries (Lasso select all variables)](#model-summaries-lasso-select-all-variables)
    -   [preference ~ tste + real](#preference-tste-real-1)
    -   [(link for the above position)](#link-for-the-above-position-7)
    -   [preference ~ tste + real + real\*tste](#preference-tste-real-realtste-1)
    -   [(link for the above position)](#link-for-the-above-position-8)
    -   [preference ~ tste + real + game](#preference-tste-real-game-1)
    -   [(link for the above position)](#link-for-the-above-position-9)
    -   [preference ~ tste + real + game + game\*tste](#preference-tste-real-game-gametste-1)
    -   [(link for the above position)](#link-for-the-above-position-10)
    -   [preference ~ tste + real + gap](#preference-tste-real-gap-1)
    -   [(link for the above position)](#link-for-the-above-position-11)
    -   [preference ~ tste + real + gap + gap\*tste](#preference-tste-real-gap-gaptste-1)
    -   [(link for the above position)](#link-for-the-above-position-12)
-   [Model summaries (traditional genres as predictors)](#model-summaries-traditional-genres-as-predictors)
    -   [preference ~ traditional genres (not selected)](#preference-traditional-genres-not-selected)
    -   [(link for the above position)](#link-for-the-above-position-13)
    -   [preference ~ traditional genres (selected)](#preference-traditional-genres-selected)
    -   [(link for the above position)](#link-for-the-above-position-14)

``` r
knitr::opts_chunk$set(
    message=FALSE,
    warning=FALSE
)

#Prevent result wrapping
options(width=120)
```

Setup
-----

Data of game and player are read in and matched up.

-   Game release data, `release` (year), is read in as an interval variable.
-   Missing values are imputed with variable mean conveniently (`star_user` and `star_GS`).

``` r
#--Package
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

#Player-related survey data
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
```

Variable
--------

Compute and select variables to be used in models.

-   Final response variable utilizes only `preference_3`.
-   Mean-centered vars is marked with a suffix \_ct.

-   Player preference:

| Name           | Definition                 | Unit                        |
|----------------|----------------------------|-----------------------------|
| `preference_1` | how much do you like       | Likert 1-7=like             |
| `preference_2` | how often play it          | ordinary 1=never-7=everyday |
| `preference_3` | does it fit personal taste | Likert 1-7=fit              |

-   Game characteristics:

<table style="width:36%;">
<colgroup>
<col width="8%" />
<col width="18%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th>Name</th>
<th>Definition</th>
<th>Unit</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>distance_survey_mean_x</code></td>
<td>group score from survey (distance from group mean in tste)</td>
<td>cosine distance</td>
</tr>
<tr class="even">
<td><code>distance_survey_median_x</code></td>
<td>group score from survey (distance from group median in tste)</td>
<td>cosine distance</td>
</tr>
<tr class="odd">
<td><code>probability_review_mean_x</code></td>
<td>group score from review (mean probability to be categorized in the group by NN)</td>
<td>percentage</td>
</tr>
<tr class="even">
<td><code>probability_review_median_x</code></td>
<td>group score from review (median probability to be categorized in the group by NN)</td>
<td>percentage</td>
</tr>
<tr class="odd">
<td><code>group_survey</code></td>
<td>group identity from survey</td>
<td>categorical 1-group number</td>
</tr>
<tr class="even">
<td><code>group_review</code></td>
<td>group identity from review</td>
<td>categorical 1-group number</td>
</tr>
<tr class="odd">
<td><code>tste_n_x</code></td>
<td>group score from survey (tste), n=number of features</td>
<td>interval arbitrary</td>
</tr>
<tr class="even">
<td><code>tg_x</code></td>
<td>if belongs to traditional genre categories</td>
<td>binary</td>
</tr>
</tbody>
</table>

-   Player personality:

<table style="width:36%;">
<colgroup>
<col width="8%" />
<col width="18%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th>Name</th>
<th>Definition</th>
<th>Unit</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>game_xxxxx</code></td>
<td>Big-five personality in game</td>
<td>Likert 1-7</td>
</tr>
<tr class="even">
<td><code>real_xxxxx</code></td>
<td>Big-five personality in real life</td>
<td>Likert 1-7</td>
</tr>
<tr class="odd">
<td><code>gap_xxxxx</code></td>
<td>personality gap (game - real)</td>
<td>Likert 1-7</td>
</tr>
<tr class="even">
<td><code>satis_xxxxx</code></td>
<td>SDT satisfaction in real life</td>
<td>Likert 1-7</td>
</tr>
<tr class="odd">
<td><code>dissatis_xxxxx</code></td>
<td>SDT dissatisfaction in real life</td>
<td>Likert 1-7</td>
</tr>
<tr class="even">
<td><code>combined_xxxxx</code></td>
<td>SDT combined (previous two) dissatisfaction in real life</td>
<td>Likert 1-7</td>
</tr>
</tbody>
</table>

-   Control:

| Name        | Definition                                    | Unit                      |
|-------------|-----------------------------------------------|---------------------------|
| `age`       | player age                                    | interval                  |
| `education` | player education                              | ordinary 1-7=PhD          |
| `income`    | player annual household income                | ordinary 1-7=over 150,000 |
| `sex`       | player sex                                    | categorical 1=male        |
| `race`      | player race                                   | categorical 1-5           |
| `release`   | game release year                             | interval year             |
| `star_GS`   | general game quality rated by GameSpot expert | interval 0-10             |
| `star_user` | general game quality rated by GameSpot user   | interval 0-10             |

``` r
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
                gap_sum = gap_extraversion + gap_agreeableness + gap_conscientiousness + (-gap_emotionstability) + gap_openness,
                real_sum = real_extraversion + real_agreeableness + real_conscientiousness + (-real_emotionstability) + real_openness,
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
```

Model specification
-------------------

![Analysis Framework](img/framework_1.png)

-   preference measurement = "how much does it fit taste?"

Double Lasso selection
----------------------

-   Based on paper `Using Double-Lasso Selection for Principled Variable Selection`
-   by Oleg Urminsky, Christian Hansen, and Victor Chernozhukov

``` r
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
                          "1"=select(df_yx, matches(outcomeVar), matches("^real.+\\D_ct$"), matches("^game.+\\D_ct$"), matches("^gap.+\\D_ct$"), matches("^tste.+\\d_ct$")),
                          "2"=select(df_yx, matches(outcomeVar)),
                          "3"=select(df_yx, matches(outcomeVar), matches(sub("game", "real", outcomeVar))))

  #The df with only the variables to be tested (those vars will be tested, and not necessarily be included in the output df)
  df_test <- switch(form,
                    "1"=data.matrix(select(df_yx, -matches(outcomeVar), -matches("^real.+\\D_ct$"), -matches("^game.+\\D_ct$"), -matches("^gap.+\\D_ct$"), -matches("^tste.+\\d_ct$"))),
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
updateVars(df.outcome="preference")

df_c <- mutate(df,
               c_age = age,
               c_education = education,
               c_income = income,
               c_race = race,
               c_sex = sex,
               c_release = release,
               c_star = star_user,
               c_starGS = star_GS)


#--Use the function to acquire the selected dfs (the new dfs can be fed into the simple linear model)
dfs$df_yx_selected_1 <- map(dfs$df_yx, ~ lassoSelect(., outcomeVar="preference", form="1"))
dfs$df_yx_selected_2 <- map(dfs$df_yx, ~ lassoSelect(., outcomeVar="preference", form="2"))

df_c_selected <- lassoSelect(select(df_c, preference, starts_with("c_"), starts_with("tg_")), outcomeVar="preference", form="2")
```

Simple linear models
--------------------

``` r
dfs$model_lm_1 <- map(dfs$df_yx_selected_1, ~ lm(preference ~ ., data=.x))
dfs$model_lm_2 <- map(dfs$df_yx_selected_2, ~ lm(preference ~ ., data=.x))
model_tGenre_selected <- lm(preference ~ ., data=df_c_selected)
model_tGenre <- lm(preference ~ ., data=select(df_c, preference, starts_with("c_"), starts_with("tg_")))
```

Information criteria
--------------------

### preference ~ tste + personality + (interaction)

### (link for the above position)

``` r
#--BIC
BIC_tGenre <- BIC(model_tGenre)
BIC_tGenre_selected <- BIC(model_tGenre_selected)
dfs$BIC_1 <- unlist(map(dfs$model_lm_1, BIC))
dfs$BIC_2 <- unlist(map(dfs$model_lm_2, BIC))

#Seperate batch models from dfs
dfs_real <- slice(dfs, 1:19)
dfs_realI <- slice(dfs, 20:38)
dfs_game <- slice(dfs, 39:57)
dfs_gameI <- slice(dfs, 58:76)
dfs_gap <- slice(dfs, 77:95)
dfs_gapI <- slice(dfs, 96:114)

#Batch models
ggplot() +
  geom_hline(yintercept=BIC_tGenre_selected, linetype="dotted") +
  geom_hline(yintercept=BIC_tGenre, alpha=0.5) +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), BIC_1, color="r")) +
  geom_line(data=dfs_realI, mapping=aes(seq(1, dim(dfs_realI)[1]), BIC_1, color="r"), alpha=0.3) +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), BIC_1, color="g")) +
  geom_line(data=dfs_gameI, mapping=aes(seq(1, dim(dfs_gameI)[1]), BIC_1, color="g"), alpha=0.3) +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), BIC_1, color="p")) +
  geom_line(data=dfs_gapI, mapping=aes(seq(1, dim(dfs_gapI)[1]), BIC_1, color="p"), alpha=0.3) +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), BIC_2, color="r"), linetype="dotted") +
  geom_line(data=dfs_realI, mapping=aes(seq(1, dim(dfs_realI)[1]), BIC_2, color="r"), alpha=0.3, linetype="dotted") +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), BIC_2, color="g"), linetype="dotted") +
  geom_line(data=dfs_gameI, mapping=aes(seq(1, dim(dfs_gameI)[1]), BIC_2, color="g"), alpha=0.3, linetype="dotted") +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), BIC_2, color="p"), linetype="dotted") +
  geom_line(data=dfs_gapI, mapping=aes(seq(1, dim(dfs_gapI)[1]), BIC_2, color="p"), alpha=0.3, linetype="dotted") +
  labs(x="Model (number of tste features)", y="BIC", title="Model BIC", subtitle="(light-colored = interaction models;  dotted = full selection models)") +
  theme(legend.position="top", legend.direction="vertical") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap)[1]), minor_breaks=NULL, labels=seq(2, 20)) +
  scale_color_manual(name="Type of models", values=c("r"="red", "g"="blue", "p"="black"),
                     labels=c("r"="tste + real + (tste*real)",
                              "g"="tste + real + game + (tste*game)",
                              "p"="tste + real + gap + (tste*gap)"))
```

![](report_0905_model1_files/figure-markdown_github-ascii_identifiers/ic_tste_inter-1.png)

``` r
#--AIC
AIC_tGenre <- AIC(model_tGenre)
AIC_tGenre_selected <- AIC(model_tGenre_selected)
dfs$AIC_1 <- unlist(map(dfs$model_lm_1, AIC))
dfs$AIC_2 <- unlist(map(dfs$model_lm_2, AIC))

#Seperate batch models from dfs
dfs_real <- slice(dfs, 1:19)
dfs_realI <- slice(dfs, 20:38)
dfs_game <- slice(dfs, 39:57)
dfs_gameI <- slice(dfs, 58:76)
dfs_gap <- slice(dfs, 77:95)
dfs_gapI <- slice(dfs, 96:114)

#Batch models
ggplot() +
  geom_hline(yintercept=AIC_tGenre_selected, linetype="dotted") +
  geom_hline(yintercept=AIC_tGenre, alpha=0.5) +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), AIC_1, color="r")) +
  geom_line(data=dfs_realI, mapping=aes(seq(1, dim(dfs_realI)[1]), AIC_1, color="r"), alpha=0.3) +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), AIC_1, color="g")) +
  geom_line(data=dfs_gameI, mapping=aes(seq(1, dim(dfs_gameI)[1]), AIC_1, color="g"), alpha=0.3) +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), AIC_1, color="p")) +
  geom_line(data=dfs_gapI, mapping=aes(seq(1, dim(dfs_gapI)[1]), AIC_1, color="p"), alpha=0.3) +
  geom_line(data=dfs_real, mapping=aes(seq(1, dim(dfs_real)[1]), AIC_2, color="r"), linetype="dotted") +
  geom_line(data=dfs_realI, mapping=aes(seq(1, dim(dfs_realI)[1]), AIC_2, color="r"), alpha=0.3, linetype="dotted") +
  geom_line(data=dfs_game, mapping=aes(seq(1, dim(dfs_game)[1]), AIC_2, color="g"), linetype="dotted") +
  geom_line(data=dfs_gameI, mapping=aes(seq(1, dim(dfs_gameI)[1]), AIC_2, color="g"), alpha=0.3, linetype="dotted") +
  geom_line(data=dfs_gap, mapping=aes(seq(1, dim(dfs_gap)[1]), AIC_2, color="p"), linetype="dotted") +
  geom_line(data=dfs_gapI, mapping=aes(seq(1, dim(dfs_gapI)[1]), AIC_2, color="p"), alpha=0.3, linetype="dotted") +
  labs(x="Model (number of tste features)", y="AIC", title="Model AIC", subtitle="(light-colored = interaction models;  dotted = full selection models)") +
  theme(legend.position="top", legend.direction="vertical") +
  scale_x_continuous(breaks=seq(1, dim(dfs_gap)[1]), minor_breaks=NULL, labels=seq(2, 20)) +
  scale_color_manual(name="Type of models", values=c("r"="red", "g"="blue", "p"="black"),
                     labels=c("r"="tste + real + (tste*real)",
                              "g"="tste + real + game + (tste*game)",
                              "p"="tste + real + gap + (tste*gap)"))
```

![](report_0905_model1_files/figure-markdown_github-ascii_identifiers/ic_tste_inter-2.png)

Model summaries (Lasso select only interactions)
------------------------------------------------

### preference ~ tste + real

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 1:19)$model_lm_1) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0348 -0.6725  0.3111  1.0941  2.4464 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.020798  14.150259   4.666 3.27e-06 ***
    ## real_extraversion_ct       0.049870   0.019884   2.508  0.01221 *  
    ## real_agreeableness_ct     -0.020132   0.027312  -0.737  0.46113    
    ## real_conscientiousness_ct  0.082197   0.029727   2.765  0.00574 ** 
    ## real_emotionstability_ct  -0.005470   0.029748  -0.184  0.85413    
    ## real_openness_ct           0.115378   0.024611   4.688 2.93e-06 ***
    ## tste_2_0_ct                0.093712   0.041299   2.269  0.02336 *  
    ## tste_2_1_ct               -0.067922   0.040265  -1.687  0.09177 .  
    ## release                   -0.030051   0.006959  -4.318 1.65e-05 ***
    ## education                  0.011783   0.026425   0.446  0.65572    
    ## income                     0.006074   0.016838   0.361  0.71835    
    ## race7                     -0.106437   0.134508  -0.791  0.42886    
    ## sex2                      -0.118691   0.071679  -1.656  0.09790 .  
    ## age                       -0.014417   0.004579  -3.148  0.00166 ** 
    ## race4                     -0.253304   0.141835  -1.786  0.07426 .  
    ## star_user                  0.010435   0.057911   0.180  0.85702    
    ## star_GS                    0.012172   0.048042   0.253  0.80001    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2121 degrees of freedom
    ## Multiple R-squared:  0.05656,    Adjusted R-squared:  0.04944 
    ## F-statistic: 7.948 on 16 and 2121 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9208 -0.6847  0.3094  1.0708  2.5200 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.395597  14.104801   4.282 1.94e-05 ***
    ## real_extraversion_ct       0.048667   0.019760   2.463  0.01386 *  
    ## real_agreeableness_ct     -0.020529   0.027143  -0.756  0.44955    
    ## real_conscientiousness_ct  0.082296   0.029542   2.786  0.00539 ** 
    ## real_emotionstability_ct  -0.002979   0.029563  -0.101  0.91974    
    ## real_openness_ct           0.117257   0.024459   4.794 1.75e-06 ***
    ## tste_3_0_ct               -0.046175   0.043421  -1.063  0.28771    
    ## tste_3_1_ct                0.147235   0.035306   4.170 3.17e-05 ***
    ## tste_3_2_ct                0.242562   0.049424   4.908 9.92e-07 ***
    ## release                   -0.027391   0.006936  -3.949 8.11e-05 ***
    ## education                  0.009879   0.026263   0.376  0.70684    
    ## income                     0.007499   0.016736   0.448  0.65414    
    ## race7                     -0.121491   0.133698  -0.909  0.36361    
    ## sex2                      -0.109390   0.071247  -1.535  0.12485    
    ## age                       -0.014225   0.004551  -3.126  0.00180 ** 
    ## race4                     -0.228271   0.141037  -1.619  0.10570    
    ## star_user                  0.009845   0.057664   0.171  0.86445    
    ## star_GS                    0.045619   0.048481   0.941  0.34683    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.455 on 2120 degrees of freedom
    ## Multiple R-squared:  0.06871,    Adjusted R-squared:  0.06124 
    ## F-statistic:   9.2 on 17 and 2120 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9740 -0.6758  0.3134  1.0672  2.6001 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.667993  14.462435   4.471 8.18e-06 ***
    ## real_extraversion_ct       0.048522   0.019746   2.457  0.01408 *  
    ## real_agreeableness_ct     -0.019396   0.027142  -0.715  0.47493    
    ## real_conscientiousness_ct  0.082561   0.029520   2.797  0.00521 ** 
    ## real_emotionstability_ct  -0.002648   0.029551  -0.090  0.92861    
    ## real_openness_ct           0.117704   0.024456   4.813 1.59e-06 ***
    ## tste_4_0_ct                0.257674   0.050030   5.150 2.84e-07 ***
    ## tste_4_1_ct                0.047486   0.056564   0.840  0.40127    
    ## tste_4_2_ct                0.055241   0.041533   1.330  0.18364    
    ## tste_4_3_ct               -0.160210   0.039345  -4.072 4.83e-05 ***
    ## release                   -0.029454   0.007109  -4.143 3.56e-05 ***
    ## education                  0.009583   0.026243   0.365  0.71502    
    ## income                     0.007026   0.016722   0.420  0.67441    
    ## race7                     -0.120644   0.133587  -0.903  0.36657    
    ## sex2                      -0.106493   0.071216  -1.495  0.13497    
    ## age                       -0.014023   0.004552  -3.081  0.00209 ** 
    ## race4                     -0.226296   0.140954  -1.605  0.10854    
    ## star_GS                    0.078692   0.052685   1.494  0.13542    
    ## star_user                 -0.039025   0.064681  -0.603  0.54634    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2119 degrees of freedom
    ## Multiple R-squared:  0.07067,    Adjusted R-squared:  0.06278 
    ## F-statistic: 8.952 on 18 and 2119 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9524 -0.6800  0.3048  1.0724  2.5821 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               53.229131  14.757534   3.607 0.000317 ***
    ## real_extraversion_ct       0.050516   0.019775   2.555 0.010703 *  
    ## real_agreeableness_ct     -0.019287   0.027177  -0.710 0.477980    
    ## real_conscientiousness_ct  0.082991   0.029556   2.808 0.005031 ** 
    ## real_emotionstability_ct  -0.003045   0.029586  -0.103 0.918037    
    ## real_openness_ct           0.116256   0.024498   4.746 2.22e-06 ***
    ## tste_5_0_ct                0.227376   0.047002   4.838 1.41e-06 ***
    ## tste_5_1_ct               -0.157447   0.052023  -3.026 0.002504 ** 
    ## tste_5_2_ct               -0.026395   0.040861  -0.646 0.518357    
    ## tste_5_3_ct               -0.088613   0.050674  -1.749 0.080491 .  
    ## tste_5_4_ct                0.060688   0.052833   1.149 0.250823    
    ## release                   -0.023752   0.007254  -3.274 0.001077 ** 
    ## education                  0.010035   0.026283   0.382 0.702631    
    ## income                     0.008172   0.016752   0.488 0.625699    
    ## race7                     -0.116605   0.133768  -0.872 0.383472    
    ## sex2                      -0.113600   0.071316  -1.593 0.111327    
    ## age                       -0.014159   0.004558  -3.106 0.001919 ** 
    ## race4                     -0.230771   0.141129  -1.635 0.102162    
    ## star_user                 -0.023157   0.067121  -0.345 0.730127    
    ## star_GS                    0.061741   0.053583   1.152 0.249349    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2118 degrees of freedom
    ## Multiple R-squared:  0.06871,    Adjusted R-squared:  0.06035 
    ## F-statistic: 8.224 on 19 and 2118 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0205 -0.6804  0.2994  1.0717  2.5816 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.029434  14.908399   3.490 0.000493 ***
    ## real_extraversion_ct       0.051343   0.019782   2.595 0.009511 ** 
    ## real_agreeableness_ct     -0.020842   0.027189  -0.767 0.443433    
    ## real_conscientiousness_ct  0.081902   0.029564   2.770 0.005648 ** 
    ## real_emotionstability_ct  -0.002377   0.029590  -0.080 0.935989    
    ## real_openness_ct           0.118062   0.024489   4.821 1.53e-06 ***
    ## tste_6_0_ct                0.054307   0.052709   1.030 0.302981    
    ## tste_6_1_ct               -0.112686   0.050521  -2.230 0.025820 *  
    ## tste_6_2_ct                0.240263   0.043474   5.527 3.67e-08 ***
    ## tste_6_3_ct                0.005949   0.053125   0.112 0.910850    
    ## tste_6_4_ct                0.109831   0.047585   2.308 0.021089 *  
    ## tste_6_5_ct                0.087693   0.055721   1.574 0.115688    
    ## release                   -0.023106   0.007329  -3.153 0.001641 ** 
    ## education                  0.009406   0.026288   0.358 0.720531    
    ## income                     0.008663   0.016757   0.517 0.605238    
    ## race7                     -0.110697   0.133845  -0.827 0.408301    
    ## sex2                      -0.110837   0.071336  -1.554 0.120399    
    ## age                       -0.014030   0.004559  -3.077 0.002114 ** 
    ## race4                     -0.225211   0.141168  -1.595 0.110785    
    ## star_user                 -0.041818   0.069568  -0.601 0.547836    
    ## star_GS                    0.068110   0.054614   1.247 0.212490    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2117 degrees of freedom
    ## Multiple R-squared:  0.0689, Adjusted R-squared:  0.0601 
    ## F-statistic: 7.833 on 20 and 2117 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9457 -0.6826  0.3002  1.0710  2.6037 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               54.956965  15.190731   3.618 0.000304 ***
    ## real_extraversion_ct       0.050226   0.019766   2.541 0.011125 *  
    ## real_agreeableness_ct     -0.017798   0.027171  -0.655 0.512508    
    ## real_conscientiousness_ct  0.082010   0.029530   2.777 0.005532 ** 
    ## real_emotionstability_ct  -0.003426   0.029562  -0.116 0.907743    
    ## real_openness_ct           0.116097   0.024458   4.747 2.21e-06 ***
    ## tste_7_0_ct               -0.179734   0.048052  -3.740 0.000189 ***
    ## tste_7_1_ct                0.023625   0.051759   0.456 0.648128    
    ## tste_7_2_ct               -0.162046   0.051136  -3.169 0.001552 ** 
    ## tste_7_3_ct               -0.199906   0.038913  -5.137 3.04e-07 ***
    ## tste_7_4_ct                0.006779   0.052723   0.129 0.897703    
    ## tste_7_5_ct                0.020373   0.056315   0.362 0.717563    
    ## tste_7_6_ct                0.039116   0.059848   0.654 0.513450    
    ## release                   -0.024638   0.007476  -3.296 0.000999 ***
    ## education                  0.010283   0.026259   0.392 0.695386    
    ## income                     0.008019   0.016737   0.479 0.631896    
    ## race7                     -0.110440   0.133612  -0.827 0.408572    
    ## sex2                      -0.114355   0.071244  -1.605 0.108619    
    ## age                       -0.013831   0.004553  -3.038 0.002414 ** 
    ## race4                     -0.221101   0.141012  -1.568 0.117042    
    ## star_user                 -0.024075   0.070359  -0.342 0.732256    
    ## star_GS                    0.067299   0.052901   1.272 0.203454    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2116 degrees of freedom
    ## Multiple R-squared:  0.07198,    Adjusted R-squared:  0.06277 
    ## F-statistic: 7.816 on 21 and 2116 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0904 -0.6878  0.3012  1.0624  2.7608 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               61.326122  15.899991   3.857 0.000118 ***
    ## real_extraversion_ct       0.050722   0.019737   2.570 0.010242 *  
    ## real_agreeableness_ct     -0.016511   0.027108  -0.609 0.542540    
    ## real_conscientiousness_ct  0.082243   0.029479   2.790 0.005321 ** 
    ## real_emotionstability_ct  -0.003322   0.029508  -0.113 0.910380    
    ## real_openness_ct           0.116620   0.024411   4.777  1.9e-06 ***
    ## tste_8_0_ct               -0.197242   0.050880  -3.877 0.000109 ***
    ## tste_8_1_ct                0.209196   0.057443   3.642 0.000277 ***
    ## tste_8_2_ct                0.096629   0.050818   1.901 0.057377 .  
    ## tste_8_3_ct                0.082796   0.047579   1.740 0.081975 .  
    ## tste_8_4_ct                0.135866   0.052335   2.596 0.009494 ** 
    ## tste_8_5_ct                0.086241   0.049434   1.745 0.081204 .  
    ## tste_8_6_ct               -0.117893   0.052976  -2.225 0.026160 *  
    ## tste_8_7_ct               -0.049688   0.051929  -0.957 0.338749    
    ## release                   -0.027679   0.007825  -3.537 0.000413 ***
    ## education                  0.010718   0.026200   0.409 0.682516    
    ## income                     0.008032   0.016698   0.481 0.630538    
    ## race7                     -0.097277   0.133380  -0.729 0.465883    
    ## sex2                      -0.116747   0.071045  -1.643 0.100474    
    ## age                       -0.013714   0.004545  -3.018 0.002578 ** 
    ## race4                     -0.207925   0.140747  -1.477 0.139744    
    ## star_user                 -0.083275   0.070766  -1.177 0.239423    
    ## star_GS                    0.094508   0.055066   1.716 0.086261 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.451 on 2115 degrees of freedom
    ## Multiple R-squared:  0.07644,    Adjusted R-squared:  0.06683 
    ## F-statistic: 7.957 on 22 and 2115 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0190 -0.6757  0.2917  1.0755  2.8952 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.722261  16.016841   3.916 9.29e-05 ***
    ## real_extraversion_ct       0.053307   0.019690   2.707 0.006838 ** 
    ## real_agreeableness_ct     -0.020460   0.027037  -0.757 0.449304    
    ## real_conscientiousness_ct  0.081492   0.029406   2.771 0.005634 ** 
    ## real_emotionstability_ct  -0.002377   0.029437  -0.081 0.935661    
    ## real_openness_ct           0.112788   0.024364   4.629 3.89e-06 ***
    ## tste_9_0_ct               -0.163571   0.053318  -3.068 0.002184 ** 
    ## tste_9_1_ct                0.045045   0.050700   0.888 0.374389    
    ## tste_9_2_ct               -0.349392   0.052254  -6.686 2.92e-11 ***
    ## tste_9_3_ct               -0.053423   0.057594  -0.928 0.353734    
    ## tste_9_4_ct               -0.012551   0.043762  -0.287 0.774291    
    ## tste_9_5_ct                0.029229   0.057487   0.508 0.611189    
    ## tste_9_6_ct               -0.009107   0.051363  -0.177 0.859292    
    ## tste_9_7_ct               -0.064710   0.052922  -1.223 0.221567    
    ## tste_9_8_ct                0.082622   0.046997   1.758 0.078888 .  
    ## release                   -0.028520   0.007873  -3.622 0.000299 ***
    ## education                  0.008743   0.026138   0.334 0.738048    
    ## income                     0.012116   0.016693   0.726 0.468014    
    ## race7                     -0.087280   0.133149  -0.656 0.512216    
    ## sex2                      -0.108645   0.070922  -1.532 0.125697    
    ## age                       -0.013994   0.004532  -3.088 0.002044 ** 
    ## race4                     -0.211707   0.140409  -1.508 0.131760    
    ## star_user                 -0.082244   0.071362  -1.152 0.249253    
    ## star_GS                    0.127678   0.055897   2.284 0.022460 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2114 degrees of freedom
    ## Multiple R-squared:  0.08132,    Adjusted R-squared:  0.07132 
    ## F-statistic: 8.136 on 23 and 2114 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9899 -0.6737  0.2848  1.0536  2.6112 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.656376  15.818012   3.961 7.71e-05 ***
    ## real_extraversion_ct       0.051994   0.019691   2.641 0.008338 ** 
    ## real_agreeableness_ct     -0.018524   0.027047  -0.685 0.493488    
    ## real_conscientiousness_ct  0.081095   0.029418   2.757 0.005890 ** 
    ## real_emotionstability_ct  -0.003581   0.029434  -0.122 0.903171    
    ## real_openness_ct           0.113204   0.024367   4.646 3.60e-06 ***
    ## tste_10_0_ct              -0.051246   0.045762  -1.120 0.262913    
    ## tste_10_1_ct               0.190319   0.050577   3.763 0.000172 ***
    ## tste_10_2_ct              -0.199185   0.052336  -3.806 0.000145 ***
    ## tste_10_3_ct              -0.178200   0.054227  -3.286 0.001032 ** 
    ## tste_10_4_ct              -0.057742   0.053190  -1.086 0.277786    
    ## tste_10_5_ct              -0.023701   0.053952  -0.439 0.660487    
    ## tste_10_6_ct              -0.139314   0.052191  -2.669 0.007659 ** 
    ## tste_10_7_ct               0.107195   0.045255   2.369 0.017940 *  
    ## tste_10_8_ct               0.031426   0.059595   0.527 0.598024    
    ## tste_10_9_ct               0.128267   0.050485   2.541 0.011134 *  
    ## release                   -0.028551   0.007789  -3.666 0.000253 ***
    ## education                  0.008389   0.026138   0.321 0.748285    
    ## income                     0.010463   0.016671   0.628 0.530326    
    ## race7                     -0.089783   0.133169  -0.674 0.500255    
    ## sex2                      -0.105555   0.070978  -1.487 0.137125    
    ## age                       -0.014047   0.004534  -3.098 0.001975 ** 
    ## race4                     -0.211407   0.140415  -1.506 0.132324    
    ## star_user                 -0.056851   0.070596  -0.805 0.420736    
    ## star_GS                    0.118750   0.055928   2.123 0.033847 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2113 degrees of freedom
    ## Multiple R-squared:  0.08181,    Adjusted R-squared:  0.07138 
    ## F-statistic: 7.845 on 24 and 2113 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0007 -0.6693  0.2879  1.0456  2.6875 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.780081  16.041404   4.038 5.58e-05 ***
    ## real_extraversion_ct       0.052971   0.019693   2.690 0.007205 ** 
    ## real_agreeableness_ct     -0.017988   0.027049  -0.665 0.506105    
    ## real_conscientiousness_ct  0.080136   0.029433   2.723 0.006530 ** 
    ## real_emotionstability_ct  -0.003637   0.029427  -0.124 0.901648    
    ## real_openness_ct           0.111933   0.024363   4.594 4.59e-06 ***
    ## tste_11_0_ct              -0.046947   0.056072  -0.837 0.402541    
    ## tste_11_1_ct               0.058382   0.052392   1.114 0.265266    
    ## tste_11_2_ct              -0.085054   0.046583  -1.826 0.068013 .  
    ## tste_11_3_ct               0.008164   0.056967   0.143 0.886065    
    ## tste_11_4_ct              -0.166075   0.057519  -2.887 0.003925 ** 
    ## tste_11_5_ct              -0.109098   0.050194  -2.174 0.029853 *  
    ## tste_11_6_ct               0.159325   0.049155   3.241 0.001208 ** 
    ## tste_11_7_ct               0.100044   0.056557   1.769 0.077055 .  
    ## tste_11_8_ct              -0.201874   0.042132  -4.791 1.77e-06 ***
    ## tste_11_9_ct               0.170557   0.049932   3.416 0.000648 ***
    ## tste_11_10_ct              0.069817   0.054726   1.276 0.202183    
    ## release                   -0.029664   0.007898  -3.756 0.000177 ***
    ## education                  0.009336   0.026126   0.357 0.720880    
    ## income                     0.011275   0.016684   0.676 0.499233    
    ## race7                     -0.097307   0.133068  -0.731 0.464705    
    ## sex2                      -0.104821   0.071014  -1.476 0.140077    
    ## age                       -0.013985   0.004531  -3.086 0.002054 ** 
    ## race4                     -0.208979   0.140363  -1.489 0.136679    
    ## star_user                 -0.081820   0.072082  -1.135 0.256467    
    ## star_GS                    0.156039   0.057371   2.720 0.006585 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2112 degrees of freedom
    ## Multiple R-squared:  0.0832, Adjusted R-squared:  0.07235 
    ## F-statistic: 7.667 on 25 and 2112 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0002 -0.6811  0.2961  1.0528  2.6441 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.750086  16.409136   3.215  0.00133 ** 
    ## real_extraversion_ct       0.052932   0.019720   2.684  0.00733 ** 
    ## real_agreeableness_ct     -0.017538   0.027065  -0.648  0.51706    
    ## real_conscientiousness_ct  0.079164   0.029455   2.688  0.00725 ** 
    ## real_emotionstability_ct  -0.002634   0.029458  -0.089  0.92877    
    ## real_openness_ct           0.114072   0.024361   4.683 3.01e-06 ***
    ## tste_12_0_ct               0.155808   0.060299   2.584  0.00983 ** 
    ## tste_12_1_ct               0.066280   0.050940   1.301  0.19335    
    ## tste_12_2_ct               0.057483   0.048574   1.183  0.23678    
    ## tste_12_3_ct              -0.002140   0.054920  -0.039  0.96892    
    ## tste_12_4_ct              -0.078478   0.050993  -1.539  0.12395    
    ## tste_12_5_ct               0.120766   0.054735   2.206  0.02746 *  
    ## tste_12_6_ct              -0.052868   0.055914  -0.946  0.34451    
    ## tste_12_7_ct               0.044918   0.053335   0.842  0.39978    
    ## tste_12_8_ct               0.161858   0.049126   3.295  0.00100 ** 
    ## tste_12_9_ct              -0.056146   0.051380  -1.093  0.27463    
    ## tste_12_10_ct              0.193045   0.045247   4.266 2.07e-05 ***
    ## tste_12_11_ct             -0.198482   0.048386  -4.102 4.25e-05 ***
    ## release                   -0.023953   0.008063  -2.971  0.00301 ** 
    ## education                  0.009817   0.026143   0.376  0.70732    
    ## income                     0.010615   0.016689   0.636  0.52481    
    ## race7                     -0.090322   0.133323  -0.677  0.49818    
    ## sex2                      -0.103571   0.071051  -1.458  0.14508    
    ## age                       -0.013832   0.004539  -3.047  0.00234 ** 
    ## race4                     -0.213429   0.140508  -1.519  0.12892    
    ## star_user                 -0.046058   0.072130  -0.639  0.52319    
    ## star_GS                    0.186873   0.060403   3.094  0.00200 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 2111 degrees of freedom
    ## Multiple R-squared:  0.08231,    Adjusted R-squared:  0.07101 
    ## F-statistic: 7.282 on 26 and 2111 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2269 -0.6817  0.2974  1.0553  2.8096 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.488407  16.794383   3.125 0.001800 ** 
    ## real_extraversion_ct       0.052148   0.019737   2.642 0.008298 ** 
    ## real_agreeableness_ct     -0.015264   0.027111  -0.563 0.573467    
    ## real_conscientiousness_ct  0.084491   0.029457   2.868 0.004168 ** 
    ## real_emotionstability_ct  -0.002434   0.029490  -0.083 0.934223    
    ## real_openness_ct           0.113536   0.024389   4.655 3.44e-06 ***
    ## tste_13_0_ct               0.012906   0.050504   0.256 0.798330    
    ## tste_13_1_ct              -0.096275   0.046677  -2.063 0.039274 *  
    ## tste_13_2_ct               0.193564   0.052018   3.721 0.000204 ***
    ## tste_13_3_ct               0.060022   0.050255   1.194 0.232478    
    ## tste_13_4_ct              -0.102500   0.053001  -1.934 0.053256 .  
    ## tste_13_5_ct               0.122697   0.051950   2.362 0.018275 *  
    ## tste_13_6_ct              -0.007364   0.055799  -0.132 0.895024    
    ## tste_13_7_ct              -0.068292   0.050777  -1.345 0.178791    
    ## tste_13_8_ct               0.084535   0.046700   1.810 0.070410 .  
    ## tste_13_9_ct               0.145842   0.056071   2.601 0.009359 ** 
    ## tste_13_10_ct              0.166370   0.048594   3.424 0.000630 ***
    ## tste_13_11_ct              0.107989   0.054183   1.993 0.046385 *  
    ## tste_13_12_ct              0.054678   0.051775   1.056 0.291054    
    ## release                   -0.023615   0.008248  -2.863 0.004237 ** 
    ## education                  0.011991   0.026176   0.458 0.646937    
    ## income                     0.008070   0.016694   0.483 0.628864    
    ## race7                     -0.092325   0.133464  -0.692 0.489164    
    ## sex2                      -0.114374   0.071173  -1.607 0.108208    
    ## age                       -0.013751   0.004547  -3.024 0.002523 ** 
    ## race4                     -0.205010   0.140678  -1.457 0.145181    
    ## star_user                 -0.087248   0.071541  -1.220 0.222771    
    ## star_GS                    0.179014   0.059428   3.012 0.002624 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2110 degrees of freedom
    ## Multiple R-squared:  0.08067,    Adjusted R-squared:  0.0689 
    ## F-statistic: 6.857 on 27 and 2110 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2121 -0.6793  0.2957  1.0541  2.7256 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.7829980 16.7326413   2.796  0.00522 ** 
    ## real_extraversion_ct       0.0545510  0.0197468   2.763  0.00579 ** 
    ## real_agreeableness_ct     -0.0158554  0.0270917  -0.585  0.55844    
    ## real_conscientiousness_ct  0.0808687  0.0294569   2.745  0.00610 ** 
    ## real_emotionstability_ct  -0.0031912  0.0294518  -0.108  0.91373    
    ## real_openness_ct           0.1131936  0.0243632   4.646 3.59e-06 ***
    ## tste_14_0_ct              -0.1018784  0.0513820  -1.983  0.04752 *  
    ## tste_14_1_ct              -0.0009821  0.0516151  -0.019  0.98482    
    ## tste_14_2_ct               0.0488363  0.0474917   1.028  0.30392    
    ## tste_14_3_ct               0.1164986  0.0464031   2.511  0.01213 *  
    ## tste_14_4_ct               0.0259343  0.0433865   0.598  0.55007    
    ## tste_14_5_ct              -0.0399329  0.0539840  -0.740  0.45955    
    ## tste_14_6_ct               0.0256146  0.0508626   0.504  0.61459    
    ## tste_14_7_ct              -0.2559280  0.0593836  -4.310 1.71e-05 ***
    ## tste_14_8_ct               0.1587335  0.0453928   3.497  0.00048 ***
    ## tste_14_9_ct              -0.0193628  0.0514863  -0.376  0.70690    
    ## tste_14_10_ct              0.1080083  0.0493442   2.189  0.02872 *  
    ## tste_14_11_ct             -0.0558130  0.0490311  -1.138  0.25512    
    ## tste_14_12_ct              0.0858530  0.0515617   1.665  0.09605 .  
    ## tste_14_13_ct              0.1148703  0.0521724   2.202  0.02779 *  
    ## release                   -0.0209428  0.0082275  -2.545  0.01098 *  
    ## education                  0.0085783  0.0261767   0.328  0.74317    
    ## income                     0.0093125  0.0166782   0.558  0.57665    
    ## race7                     -0.1038399  0.1333762  -0.779  0.43633    
    ## sex2                      -0.1069909  0.0711667  -1.503  0.13289    
    ## age                       -0.0133286  0.0045431  -2.934  0.00338 ** 
    ## race4                     -0.2024618  0.1405240  -1.441  0.14980    
    ## star_user                 -0.0387656  0.0719136  -0.539  0.58990    
    ## star_GS                    0.1701810  0.0595395   2.858  0.00430 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 2109 degrees of freedom
    ## Multiple R-squared:  0.08298,    Adjusted R-squared:  0.0708 
    ## F-statistic: 6.816 on 28 and 2109 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2041 -0.6701  0.2999  1.0689  2.7768 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               54.0392971 17.7029180   3.053 0.002297 ** 
    ## real_extraversion_ct       0.0537739  0.0197609   2.721 0.006558 ** 
    ## real_agreeableness_ct     -0.0179377  0.0271104  -0.662 0.508267    
    ## real_conscientiousness_ct  0.0823543  0.0294785   2.794 0.005258 ** 
    ## real_emotionstability_ct  -0.0009003  0.0295162  -0.031 0.975670    
    ## real_openness_ct           0.1143406  0.0243839   4.689 2.92e-06 ***
    ## tste_15_0_ct              -0.0607561  0.0479768  -1.266 0.205523    
    ## tste_15_1_ct              -0.1201470  0.0536875  -2.238 0.025332 *  
    ## tste_15_2_ct               0.1582373  0.0485340   3.260 0.001131 ** 
    ## tste_15_3_ct               0.0939135  0.0525302   1.788 0.073952 .  
    ## tste_15_4_ct               0.0075642  0.0469277   0.161 0.871960    
    ## tste_15_5_ct              -0.0724169  0.0537018  -1.348 0.177642    
    ## tste_15_6_ct              -0.1150501  0.0466060  -2.469 0.013644 *  
    ## tste_15_7_ct              -0.0651654  0.0516730  -1.261 0.207409    
    ## tste_15_8_ct              -0.1237063  0.0539890  -2.291 0.022043 *  
    ## tste_15_9_ct               0.0310879  0.0510998   0.608 0.543003    
    ## tste_15_10_ct              0.0476698  0.0532412   0.895 0.370699    
    ## tste_15_11_ct              0.1828548  0.0542628   3.370 0.000766 ***
    ## tste_15_12_ct             -0.0470656  0.0381418  -1.234 0.217354    
    ## tste_15_13_ct              0.0289127  0.0444327   0.651 0.515306    
    ## tste_15_14_ct              0.0494408  0.0468974   1.054 0.291898    
    ## release                   -0.0243103  0.0086941  -2.796 0.005218 ** 
    ## education                  0.0087924  0.0261966   0.336 0.737182    
    ## income                     0.0091706  0.0167061   0.549 0.583106    
    ## race7                     -0.0941948  0.1334443  -0.706 0.480345    
    ## sex2                      -0.1103023  0.0712293  -1.549 0.121640    
    ## age                       -0.0134130  0.0045458  -2.951 0.003206 ** 
    ## race4                     -0.2017048  0.1406568  -1.434 0.151714    
    ## star_user                 -0.0694327  0.0755886  -0.919 0.358431    
    ## star_GS                    0.1420998  0.0602685   2.358 0.018476 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2108 degrees of freedom
    ## Multiple R-squared:  0.08178,    Adjusted R-squared:  0.06915 
    ## F-statistic: 6.474 on 29 and 2108 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0614 -0.6824  0.2825  1.0531  2.7635 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               36.746557  17.684858   2.078 0.037844 *  
    ## real_extraversion_ct       0.056214   0.019698   2.854 0.004362 ** 
    ## real_agreeableness_ct     -0.020406   0.027010  -0.755 0.450039    
    ## real_conscientiousness_ct  0.082241   0.029351   2.802 0.005125 ** 
    ## real_emotionstability_ct   0.005238   0.029415   0.178 0.858678    
    ## real_openness_ct           0.113403   0.024294   4.668 3.23e-06 ***
    ## tste_16_0_ct               0.175210   0.048650   3.601 0.000324 ***
    ## tste_16_1_ct              -0.096230   0.049843  -1.931 0.053661 .  
    ## tste_16_2_ct              -0.053569   0.044938  -1.192 0.233377    
    ## tste_16_3_ct              -0.159419   0.050498  -3.157 0.001617 ** 
    ## tste_16_4_ct              -0.171458   0.041254  -4.156 3.37e-05 ***
    ## tste_16_5_ct               0.104794   0.048234   2.173 0.029920 *  
    ## tste_16_6_ct               0.102791   0.044084   2.332 0.019809 *  
    ## tste_16_7_ct               0.075396   0.044625   1.690 0.091262 .  
    ## tste_16_8_ct              -0.057069   0.051954  -1.098 0.272134    
    ## tste_16_9_ct               0.154129   0.044648   3.452 0.000567 ***
    ## tste_16_10_ct             -0.076377   0.044035  -1.734 0.082982 .  
    ## tste_16_11_ct              0.146011   0.050159   2.911 0.003641 ** 
    ## tste_16_12_ct              0.077388   0.046136   1.677 0.093614 .  
    ## tste_16_13_ct              0.056759   0.049735   1.141 0.253899    
    ## tste_16_14_ct              0.114459   0.046286   2.473 0.013482 *  
    ## tste_16_15_ct             -0.010936   0.050148  -0.218 0.827391    
    ## release                   -0.015761   0.008689  -1.814 0.069819 .  
    ## education                  0.007860   0.026088   0.301 0.763229    
    ## income                     0.012653   0.016662   0.759 0.447678    
    ## race7                     -0.094310   0.133042  -0.709 0.478481    
    ## sex2                      -0.113285   0.070893  -1.598 0.110197    
    ## age                       -0.012244   0.004539  -2.698 0.007037 ** 
    ## race4                     -0.196670   0.140159  -1.403 0.160706    
    ## star_user                 -0.064189   0.072306  -0.888 0.374784    
    ## star_GS                    0.146518   0.058459   2.506 0.012273 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.443 on 2107 degrees of freedom
    ## Multiple R-squared:  0.08934,    Adjusted R-squared:  0.07637 
    ## F-statistic:  6.89 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2745 -0.6863  0.2794  1.0469  2.8083 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               39.276357  18.229468   2.155  0.03131 *  
    ## real_extraversion_ct       0.055263   0.019672   2.809  0.00501 ** 
    ## real_agreeableness_ct     -0.016013   0.026999  -0.593  0.55317    
    ## real_conscientiousness_ct  0.082540   0.029326   2.815  0.00493 ** 
    ## real_emotionstability_ct   0.002664   0.029369   0.091  0.92773    
    ## real_openness_ct           0.113223   0.024275   4.664 3.29e-06 ***
    ## tste_17_0_ct               0.311334   0.048963   6.359 2.49e-10 ***
    ## tste_17_1_ct               0.060808   0.041889   1.452  0.14675    
    ## tste_17_2_ct              -0.069730   0.049951  -1.396  0.16287    
    ## tste_17_3_ct               0.051812   0.047708   1.086  0.27759    
    ## tste_17_4_ct               0.091830   0.050184   1.830  0.06741 .  
    ## tste_17_5_ct               0.017211   0.046766   0.368  0.71289    
    ## tste_17_6_ct              -0.052457   0.049127  -1.068  0.28574    
    ## tste_17_7_ct               0.091017   0.043153   2.109  0.03505 *  
    ## tste_17_8_ct              -0.068926   0.047909  -1.439  0.15039    
    ## tste_17_9_ct              -0.099666   0.046219  -2.156  0.03116 *  
    ## tste_17_10_ct              0.091547   0.042486   2.155  0.03129 *  
    ## tste_17_11_ct              0.032133   0.046794   0.687  0.49236    
    ## tste_17_12_ct             -0.170429   0.042865  -3.976 7.25e-05 ***
    ## tste_17_13_ct              0.091894   0.049132   1.870  0.06157 .  
    ## tste_17_14_ct              0.024589   0.043302   0.568  0.57019    
    ## tste_17_15_ct             -0.038835   0.055954  -0.694  0.48773    
    ## tste_17_16_ct             -0.044482   0.042777  -1.040  0.29853    
    ## release                   -0.017076   0.008931  -1.912  0.05602 .  
    ## education                  0.009049   0.026062   0.347  0.72847    
    ## income                     0.012606   0.016637   0.758  0.44871    
    ## race7                     -0.085681   0.132926  -0.645  0.51927    
    ## sex2                      -0.116675   0.070805  -1.648  0.09953 .  
    ## age                       -0.012252   0.004539  -2.699  0.00700 ** 
    ## race4                     -0.205172   0.140046  -1.465  0.14306    
    ## star_user                 -0.077555   0.076696  -1.011  0.31204    
    ## star_GS                    0.172627   0.060236   2.866  0.00420 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2106 degrees of freedom
    ## Multiple R-squared:  0.09168,    Adjusted R-squared:  0.07831 
    ## F-statistic: 6.857 on 31 and 2106 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1389 -0.6761  0.2919  1.0455  2.6344 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               48.5389870 18.0254833   2.693 0.007142 ** 
    ## real_extraversion_ct       0.0529702  0.0196728   2.693 0.007147 ** 
    ## real_agreeableness_ct     -0.0154407  0.0269949  -0.572 0.567392    
    ## real_conscientiousness_ct  0.0813017  0.0293458   2.770 0.005647 ** 
    ## real_emotionstability_ct  -0.0004716  0.0293885  -0.016 0.987199    
    ## real_openness_ct           0.1124602  0.0242768   4.632 3.84e-06 ***
    ## tste_18_0_ct              -0.1253975  0.0460791  -2.721 0.006555 ** 
    ## tste_18_1_ct               0.1606469  0.0480062   3.346 0.000833 ***
    ## tste_18_2_ct              -0.0191744  0.0435853  -0.440 0.660034    
    ## tste_18_3_ct              -0.0188349  0.0468075  -0.402 0.687437    
    ## tste_18_4_ct               0.0792089  0.0510870   1.550 0.121179    
    ## tste_18_5_ct              -0.0337542  0.0479667  -0.704 0.481697    
    ## tste_18_6_ct              -0.0036025  0.0503400  -0.072 0.942956    
    ## tste_18_7_ct              -0.0532483  0.0432540  -1.231 0.218438    
    ## tste_18_8_ct               0.0770615  0.0470449   1.638 0.101563    
    ## tste_18_9_ct              -0.1335173  0.0477663  -2.795 0.005234 ** 
    ## tste_18_10_ct              0.0810639  0.0458629   1.768 0.077285 .  
    ## tste_18_11_ct              0.0615010  0.0451330   1.363 0.173135    
    ## tste_18_12_ct             -0.0820410  0.0474992  -1.727 0.084277 .  
    ## tste_18_13_ct             -0.0312338  0.0467492  -0.668 0.504135    
    ## tste_18_14_ct              0.0436704  0.0507679   0.860 0.389778    
    ## tste_18_15_ct              0.1657439  0.0584856   2.834 0.004642 ** 
    ## tste_18_16_ct             -0.2908596  0.0513840  -5.661 1.72e-08 ***
    ## tste_18_17_ct              0.0488670  0.0456690   1.070 0.284731    
    ## release                   -0.0213390  0.0088468  -2.412 0.015948 *  
    ## education                  0.0089339  0.0260598   0.343 0.731766    
    ## income                     0.0123265  0.0166451   0.741 0.459052    
    ## race7                     -0.0944184  0.1328917  -0.710 0.477478    
    ## sex2                      -0.1097774  0.0708745  -1.549 0.121557    
    ## age                       -0.0122133  0.0045401  -2.690 0.007200 ** 
    ## race4                     -0.2021885  0.1400572  -1.444 0.148996    
    ## star_user                 -0.0799959  0.0770785  -1.038 0.299459    
    ## star_GS                    0.0908449  0.0609494   1.490 0.136243    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2105 degrees of freedom
    ## Multiple R-squared:  0.09194,    Adjusted R-squared:  0.07813 
    ## F-statistic:  6.66 on 32 and 2105 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2052 -0.6748  0.2721  1.0641  2.6855 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                4.119e+01  1.845e+01   2.232 0.025725 *  
    ## real_extraversion_ct       5.550e-02  1.974e-02   2.812 0.004975 ** 
    ## real_agreeableness_ct     -1.563e-02  2.708e-02  -0.577 0.563748    
    ## real_conscientiousness_ct  8.292e-02  2.944e-02   2.817 0.004897 ** 
    ## real_emotionstability_ct   3.404e-03  2.951e-02   0.115 0.908171    
    ## real_openness_ct           1.152e-01  2.433e-02   4.733 2.36e-06 ***
    ## tste_19_0_ct              -2.755e-05  4.816e-02  -0.001 0.999544    
    ## tste_19_1_ct               6.508e-02  5.029e-02   1.294 0.195779    
    ## tste_19_2_ct               2.948e-02  4.894e-02   0.602 0.546967    
    ## tste_19_3_ct              -1.161e-01  4.648e-02  -2.497 0.012596 *  
    ## tste_19_4_ct              -1.022e-01  4.904e-02  -2.083 0.037342 *  
    ## tste_19_5_ct              -3.116e-02  5.345e-02  -0.583 0.559972    
    ## tste_19_6_ct               8.356e-02  4.779e-02   1.749 0.080504 .  
    ## tste_19_7_ct               1.169e-01  4.679e-02   2.497 0.012589 *  
    ## tste_19_8_ct               3.968e-02  4.685e-02   0.847 0.397112    
    ## tste_19_9_ct              -7.256e-02  5.020e-02  -1.445 0.148520    
    ## tste_19_10_ct              1.284e-01  4.787e-02   2.682 0.007386 ** 
    ## tste_19_11_ct              8.409e-02  4.484e-02   1.875 0.060882 .  
    ## tste_19_12_ct              2.274e-01  4.562e-02   4.984 6.74e-07 ***
    ## tste_19_13_ct             -8.065e-03  4.320e-02  -0.187 0.851934    
    ## tste_19_14_ct              1.705e-01  4.493e-02   3.794 0.000152 ***
    ## tste_19_15_ct              1.411e-03  4.417e-02   0.032 0.974510    
    ## tste_19_16_ct              2.394e-02  4.687e-02   0.511 0.609573    
    ## tste_19_17_ct              6.129e-02  5.299e-02   1.157 0.247569    
    ## tste_19_18_ct              5.728e-03  4.099e-02   0.140 0.888870    
    ## release                   -1.799e-02  9.059e-03  -1.985 0.047219 *  
    ## education                  8.878e-03  2.612e-02   0.340 0.733970    
    ## income                     1.289e-02  1.668e-02   0.773 0.439787    
    ## race7                     -9.032e-02  1.332e-01  -0.678 0.497835    
    ## sex2                      -1.120e-01  7.106e-02  -1.576 0.115083    
    ## age                       -1.211e-02  4.552e-03  -2.661 0.007845 ** 
    ## race4                     -2.079e-01  1.403e-01  -1.482 0.138550    
    ## star_user                 -3.182e-02  7.463e-02  -0.426 0.669880    
    ## star_GS                    1.159e-01  6.032e-02   1.921 0.054835 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2104 degrees of freedom
    ## Multiple R-squared:  0.08859,    Adjusted R-squared:  0.07429 
    ## F-statistic: 6.197 on 33 and 2104 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0900 -0.7001  0.2725  1.0513  2.5624 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               38.267599  19.273227   1.986 0.047215 *  
    ## real_extraversion_ct       0.056283   0.019688   2.859 0.004295 ** 
    ## real_agreeableness_ct     -0.016027   0.027022  -0.593 0.553167    
    ## real_conscientiousness_ct  0.083142   0.029358   2.832 0.004670 ** 
    ## real_emotionstability_ct   0.004922   0.029425   0.167 0.867180    
    ## real_openness_ct           0.113240   0.024278   4.664 3.29e-06 ***
    ## tste_20_0_ct               0.073186   0.053512   1.368 0.171565    
    ## tste_20_1_ct               0.081517   0.046508   1.753 0.079794 .  
    ## tste_20_2_ct               0.091538   0.042464   2.156 0.031223 *  
    ## tste_20_3_ct               0.149782   0.048695   3.076 0.002126 ** 
    ## tste_20_4_ct               0.039180   0.044320   0.884 0.376774    
    ## tste_20_5_ct              -0.069986   0.043830  -1.597 0.110467    
    ## tste_20_6_ct               0.032868   0.049504   0.664 0.506804    
    ## tste_20_7_ct               0.189757   0.054120   3.506 0.000464 ***
    ## tste_20_8_ct              -0.018680   0.053734  -0.348 0.728145    
    ## tste_20_9_ct               0.097618   0.044212   2.208 0.027353 *  
    ## tste_20_10_ct              0.060905   0.048051   1.268 0.205113    
    ## tste_20_11_ct             -0.124810   0.045674  -2.733 0.006336 ** 
    ## tste_20_12_ct              0.123766   0.046064   2.687 0.007271 ** 
    ## tste_20_13_ct              0.114236   0.050339   2.269 0.023350 *  
    ## tste_20_14_ct             -0.098558   0.041044  -2.401 0.016425 *  
    ## tste_20_15_ct              0.116171   0.053260   2.181 0.029279 *  
    ## tste_20_16_ct             -0.094723   0.046154  -2.052 0.040261 *  
    ## tste_20_17_ct              0.036167   0.044895   0.806 0.420570    
    ## tste_20_18_ct              0.139426   0.042645   3.269 0.001095 ** 
    ## tste_20_19_ct             -0.043242   0.048620  -0.889 0.373896    
    ## release                   -0.016450   0.009461  -1.739 0.082248 .  
    ## education                  0.009256   0.026061   0.355 0.722491    
    ## income                     0.012437   0.016644   0.747 0.454991    
    ## race7                     -0.091652   0.132929  -0.689 0.490594    
    ## sex2                      -0.104205   0.070946  -1.469 0.142040    
    ## age                       -0.011969   0.004543  -2.634 0.008490 ** 
    ## race4                     -0.192915   0.140085  -1.377 0.168618    
    ## star_user                 -0.011653   0.077955  -0.149 0.881184    
    ## star_GS                    0.075172   0.061260   1.227 0.219923    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2103 degrees of freedom
    ## Multiple R-squared:  0.09315,    Adjusted R-squared:  0.07849 
    ## F-statistic: 6.353 on 34 and 2103 DF,  p-value: < 2.2e-16

### preference ~ tste + real + real\*tste

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 20:38)$model_lm_1) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0348 -0.6725  0.3111  1.0941  2.4464 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.020798  14.150259   4.666 3.27e-06 ***
    ## real_extraversion_ct       0.049870   0.019884   2.508  0.01221 *  
    ## real_agreeableness_ct     -0.020132   0.027312  -0.737  0.46113    
    ## real_conscientiousness_ct  0.082197   0.029727   2.765  0.00574 ** 
    ## real_emotionstability_ct  -0.005470   0.029748  -0.184  0.85413    
    ## real_openness_ct           0.115378   0.024611   4.688 2.93e-06 ***
    ## tste_2_0_ct                0.093712   0.041299   2.269  0.02336 *  
    ## tste_2_1_ct               -0.067922   0.040265  -1.687  0.09177 .  
    ## release                   -0.030051   0.006959  -4.318 1.65e-05 ***
    ## education                  0.011783   0.026425   0.446  0.65572    
    ## income                     0.006074   0.016838   0.361  0.71835    
    ## race7                     -0.106437   0.134508  -0.791  0.42886    
    ## sex2                      -0.118691   0.071679  -1.656  0.09790 .  
    ## age                       -0.014417   0.004579  -3.148  0.00166 ** 
    ## race4                     -0.253304   0.141835  -1.786  0.07426 .  
    ## star_user                  0.010435   0.057911   0.180  0.85702    
    ## star_GS                    0.012172   0.048042   0.253  0.80001    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2121 degrees of freedom
    ## Multiple R-squared:  0.05656,    Adjusted R-squared:  0.04944 
    ## F-statistic: 7.948 on 16 and 2121 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9283 -0.6896  0.3133  1.0744  2.5566 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      61.026011  14.101324   4.328 1.58e-05 ***
    ## real_extraversion_ct              0.049796   0.019706   2.527  0.01158 *  
    ## real_agreeableness_ct            -0.018252   0.027108  -0.673  0.50082    
    ## real_conscientiousness_ct         0.082181   0.029518   2.784  0.00541 ** 
    ## real_emotionstability_ct         -0.003517   0.029546  -0.119  0.90526    
    ## real_openness_ct                  0.118824   0.024462   4.858 1.28e-06 ***
    ## tste_3_0_ct                      -0.046500   0.043401  -1.071  0.28411    
    ## tste_3_1_ct                       0.147101   0.035294   4.168 3.20e-05 ***
    ## tste_3_2_ct                       0.248364   0.049627   5.005 6.06e-07 ***
    ## release                          -0.027722   0.006934  -3.998 6.61e-05 ***
    ## education                         0.010633   0.026264   0.405  0.68563    
    ## income                            0.006630   0.016735   0.396  0.69200    
    ## age                              -0.014180   0.004546  -3.119  0.00184 ** 
    ## sex2                             -0.111142   0.071183  -1.561  0.11859    
    ## race4                            -0.215553   0.140933  -1.529  0.12629    
    ## star_user                         0.007302   0.057668   0.127  0.89925    
    ## star_GS                           0.051576   0.048590   1.061  0.28861    
    ## real_extraversion_ct.tste_3_2_ct -0.037749   0.026226  -1.439  0.15019    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.455 on 2120 degrees of freedom
    ## Multiple R-squared:  0.06925,    Adjusted R-squared:  0.06179 
    ## F-statistic: 9.279 on 17 and 2120 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9710 -0.6824  0.3096  1.0726  2.5981 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.893424  14.459652   4.488 7.58e-06 ***
    ## real_extraversion_ct       0.049765   0.019697   2.527  0.01159 *  
    ## real_agreeableness_ct     -0.018128   0.027104  -0.669  0.50368    
    ## real_conscientiousness_ct  0.081513   0.029496   2.764  0.00577 ** 
    ## real_emotionstability_ct  -0.003293   0.029541  -0.111  0.91126    
    ## real_openness_ct           0.118085   0.024452   4.829 1.47e-06 ***
    ## tste_4_0_ct                0.256725   0.050017   5.133 3.12e-07 ***
    ## tste_4_1_ct                0.047372   0.056561   0.838  0.40239    
    ## tste_4_2_ct                0.054743   0.041527   1.318  0.18757    
    ## tste_4_3_ct               -0.161060   0.039332  -4.095 4.38e-05 ***
    ## release                   -0.029573   0.007108  -4.161 3.30e-05 ***
    ## education                  0.009025   0.026234   0.344  0.73087    
    ## income                     0.006796   0.016719   0.406  0.68444    
    ## age                       -0.013858   0.004548  -3.047  0.00234 ** 
    ## sex2                      -0.108767   0.071168  -1.528  0.12659    
    ## race4                     -0.220509   0.140802  -1.566  0.11748    
    ## star_GS                    0.079503   0.052675   1.509  0.13137    
    ## star_user                 -0.039320   0.064678  -0.608  0.54330    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2120 degrees of freedom
    ## Multiple R-squared:  0.07031,    Adjusted R-squared:  0.06286 
    ## F-statistic: 9.432 on 17 and 2120 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9481 -0.6789  0.3101  1.0757  2.5805 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               53.472380  14.754059   3.624 0.000297 ***
    ## real_extraversion_ct       0.051720   0.019726   2.622 0.008805 ** 
    ## real_agreeableness_ct     -0.018049   0.027138  -0.665 0.506071    
    ## real_conscientiousness_ct  0.081984   0.029531   2.776 0.005549 ** 
    ## real_emotionstability_ct  -0.003676   0.029575  -0.124 0.901107    
    ## real_openness_ct           0.116611   0.024493   4.761 2.06e-06 ***
    ## tste_5_0_ct                0.226759   0.046994   4.825 1.50e-06 ***
    ## tste_5_1_ct               -0.158355   0.052010  -3.045 0.002358 ** 
    ## tste_5_2_ct               -0.027157   0.040849  -0.665 0.506233    
    ## tste_5_3_ct               -0.087914   0.050665  -1.735 0.082852 .  
    ## tste_5_4_ct                0.060743   0.052830   1.150 0.250367    
    ## release                   -0.023879   0.007252  -3.293 0.001009 ** 
    ## education                  0.009503   0.026274   0.362 0.717619    
    ## income                     0.007949   0.016749   0.475 0.635103    
    ## age                       -0.013999   0.004554  -3.074 0.002140 ** 
    ## sex2                      -0.115832   0.071266  -1.625 0.104237    
    ## race4                     -0.225139   0.140973  -1.597 0.110408    
    ## star_user                 -0.023741   0.067114  -0.354 0.723567    
    ## star_GS                    0.062765   0.053567   1.172 0.241447    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2119 degrees of freedom
    ## Multiple R-squared:  0.06837,    Adjusted R-squared:  0.06046 
    ## F-statistic:  8.64 on 18 and 2119 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0163 -0.6750  0.3062  1.0764  2.5933 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.223471  14.905441   3.504 0.000468 ***
    ## real_extraversion_ct       0.052494   0.019731   2.660 0.007863 ** 
    ## real_agreeableness_ct     -0.019681   0.027150  -0.725 0.468598    
    ## real_conscientiousness_ct  0.080933   0.029538   2.740 0.006197 ** 
    ## real_emotionstability_ct  -0.002977   0.029579  -0.101 0.919831    
    ## real_openness_ct           0.118408   0.024483   4.836 1.42e-06 ***
    ## tste_6_0_ct                0.053731   0.052701   1.020 0.308055    
    ## tste_6_1_ct               -0.112905   0.050516  -2.235 0.025520 *  
    ## tste_6_2_ct                0.240184   0.043471   5.525 3.70e-08 ***
    ## tste_6_3_ct                0.006009   0.053121   0.113 0.909948    
    ## tste_6_4_ct                0.109794   0.047582   2.307 0.021124 *  
    ## tste_6_5_ct                0.089615   0.055668   1.610 0.107591    
    ## release                   -0.023208   0.007328  -3.167 0.001561 ** 
    ## education                  0.008899   0.026279   0.339 0.734933    
    ## income                     0.008461   0.016754   0.505 0.613611    
    ## age                       -0.013877   0.004555  -3.047 0.002343 ** 
    ## sex2                      -0.112942   0.071285  -1.584 0.113261    
    ## race4                     -0.219817   0.141007  -1.559 0.119167    
    ## star_user                 -0.042500   0.069558  -0.611 0.541260    
    ## star_GS                    0.069211   0.054593   1.268 0.205025    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2118 degrees of freedom
    ## Multiple R-squared:  0.0686, Adjusted R-squared:  0.06024 
    ## F-statistic:  8.21 on 19 and 2118 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9413 -0.6764  0.3005  1.0714  2.6145 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               55.197856  15.186799   3.635 0.000285 ***
    ## real_extraversion_ct       0.051359   0.019717   2.605 0.009259 ** 
    ## real_agreeableness_ct     -0.016606   0.027131  -0.612 0.540559    
    ## real_conscientiousness_ct  0.081052   0.029505   2.747 0.006064 ** 
    ## real_emotionstability_ct  -0.004027   0.029551  -0.136 0.891629    
    ## real_openness_ct           0.116436   0.024453   4.762 2.05e-06 ***
    ## tste_7_0_ct               -0.180567   0.048038  -3.759 0.000175 ***
    ## tste_7_1_ct                0.023501   0.051755   0.454 0.649817    
    ## tste_7_2_ct               -0.161015   0.051117  -3.150 0.001656 ** 
    ## tste_7_3_ct               -0.200145   0.038909  -5.144 2.94e-07 ***
    ## tste_7_4_ct                0.006130   0.052713   0.116 0.907434    
    ## tste_7_5_ct                0.020942   0.056306   0.372 0.709981    
    ## tste_7_6_ct                0.039295   0.059843   0.657 0.511487    
    ## release                   -0.024764   0.007474  -3.313 0.000937 ***
    ## education                  0.009788   0.026251   0.373 0.709281    
    ## income                     0.007807   0.016734   0.467 0.640881    
    ## age                       -0.013679   0.004549  -3.007 0.002671 ** 
    ## sex2                      -0.116486   0.071192  -1.636 0.101943    
    ## race4                     -0.215768   0.140854  -1.532 0.125709    
    ## star_user                 -0.024341   0.070353  -0.346 0.729391    
    ## star_GS                    0.068176   0.052886   1.289 0.197500    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2117 degrees of freedom
    ## Multiple R-squared:  0.07168,    Adjusted R-squared:  0.06291 
    ## F-statistic: 8.174 on 20 and 2117 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0872 -0.6898  0.2984  1.0627  2.7715 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               61.572845  15.894633   3.874 0.000110 ***
    ## real_extraversion_ct       0.051721   0.019687   2.627 0.008674 ** 
    ## real_agreeableness_ct     -0.015459   0.027067  -0.571 0.567969    
    ## real_conscientiousness_ct  0.081402   0.029453   2.764 0.005764 ** 
    ## real_emotionstability_ct  -0.003852   0.029496  -0.131 0.896112    
    ## real_openness_ct           0.116915   0.024405   4.791 1.78e-06 ***
    ## tste_8_0_ct               -0.197572   0.050872  -3.884 0.000106 ***
    ## tste_8_1_ct                0.209663   0.057433   3.651 0.000268 ***
    ## tste_8_2_ct                0.096703   0.050812   1.903 0.057157 .  
    ## tste_8_3_ct                0.082534   0.047573   1.735 0.082905 .  
    ## tste_8_4_ct                0.135858   0.052329   2.596 0.009490 ** 
    ## tste_8_5_ct                0.086076   0.049428   1.741 0.081748 .  
    ## tste_8_6_ct               -0.119681   0.052913  -2.262 0.023810 *  
    ## tste_8_7_ct               -0.049658   0.051923  -0.956 0.338989    
    ## release                   -0.027806   0.007823  -3.555 0.000387 ***
    ## education                  0.010287   0.026190   0.393 0.694510    
    ## income                     0.007847   0.016694   0.470 0.638379    
    ## age                       -0.013580   0.004540  -2.991 0.002814 ** 
    ## sex2                      -0.118623   0.070991  -1.671 0.094877 .  
    ## race4                     -0.203125   0.140577  -1.445 0.148624    
    ## star_user                 -0.083913   0.070753  -1.186 0.235756    
    ## star_GS                    0.095453   0.055045   1.734 0.083046 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.451 on 2116 degrees of freedom
    ## Multiple R-squared:  0.0762, Adjusted R-squared:  0.06704 
    ## F-statistic: 8.312 on 21 and 2116 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0158 -0.6786  0.2928  1.0797  2.9049 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               63.053036  16.006731   3.939 8.44e-05 ***
    ## real_extraversion_ct       0.054213   0.019639   2.760 0.005821 ** 
    ## real_agreeableness_ct     -0.019541   0.026997  -0.724 0.469261    
    ## real_conscientiousness_ct  0.080723   0.029379   2.748 0.006054 ** 
    ## real_emotionstability_ct  -0.002865   0.029423  -0.097 0.922446    
    ## real_openness_ct           0.113032   0.024358   4.641 3.69e-06 ***
    ## tste_9_0_ct               -0.164776   0.053280  -3.093 0.002010 ** 
    ## tste_9_1_ct                0.044251   0.050678   0.873 0.382671    
    ## tste_9_2_ct               -0.349937   0.052241  -6.699 2.69e-11 ***
    ## tste_9_3_ct               -0.054538   0.057562  -0.947 0.343504    
    ## tste_9_4_ct               -0.012549   0.043756  -0.287 0.774300    
    ## tste_9_5_ct                0.030257   0.057458   0.527 0.598530    
    ## tste_9_6_ct               -0.008990   0.051356  -0.175 0.861060    
    ## tste_9_7_ct               -0.064215   0.052910  -1.214 0.225012    
    ## tste_9_8_ct                0.082728   0.046990   1.761 0.078463 .  
    ## release                   -0.028689   0.007868  -3.646 0.000272 ***
    ## education                  0.008335   0.026127   0.319 0.749732    
    ## income                     0.011960   0.016689   0.717 0.473676    
    ## age                       -0.013873   0.004528  -3.064 0.002213 ** 
    ## sex2                      -0.110284   0.070868  -1.556 0.119814    
    ## race4                     -0.207413   0.140237  -1.479 0.139286    
    ## star_user                 -0.082864   0.071347  -1.161 0.245599    
    ## star_GS                    0.128576   0.055873   2.301 0.021475 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2115 degrees of freedom
    ## Multiple R-squared:  0.08113,    Adjusted R-squared:  0.07157 
    ## F-statistic: 8.488 on 22 and 2115 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9871 -0.6732  0.2816  1.0541  2.6206 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               63.011870  15.807182   3.986 6.94e-05 ***
    ## real_extraversion_ct       0.052916   0.019641   2.694 0.007111 ** 
    ## real_agreeableness_ct     -0.017574   0.027007  -0.651 0.515282    
    ## real_conscientiousness_ct  0.080306   0.029391   2.732 0.006341 ** 
    ## real_emotionstability_ct  -0.004084   0.029421  -0.139 0.889607    
    ## real_openness_ct           0.113460   0.024361   4.658 3.40e-06 ***
    ## tste_10_0_ct              -0.051533   0.045754  -1.126 0.260160    
    ## tste_10_1_ct               0.191734   0.050526   3.795 0.000152 ***
    ## tste_10_2_ct              -0.200280   0.052304  -3.829 0.000132 ***
    ## tste_10_3_ct              -0.177781   0.054217  -3.279 0.001058 ** 
    ## tste_10_4_ct              -0.058325   0.053176  -1.097 0.272841    
    ## tste_10_5_ct              -0.024232   0.053939  -0.449 0.653294    
    ## tste_10_6_ct              -0.139511   0.052184  -2.673 0.007565 ** 
    ## tste_10_7_ct               0.106359   0.045232   2.351 0.018794 *  
    ## tste_10_8_ct               0.032166   0.059577   0.540 0.589322    
    ## tste_10_9_ct               0.127983   0.050477   2.535 0.011301 *  
    ## release                   -0.028732   0.007783  -3.692 0.000228 ***
    ## education                  0.007979   0.026128   0.305 0.760115    
    ## income                     0.010293   0.016667   0.618 0.536934    
    ## age                       -0.013924   0.004530  -3.074 0.002142 ** 
    ## sex2                      -0.107265   0.070924  -1.512 0.130581    
    ## race4                     -0.206945   0.140241  -1.476 0.140190    
    ## star_user                 -0.057477   0.070581  -0.814 0.415537    
    ## star_GS                    0.119595   0.055906   2.139 0.032534 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2114 degrees of freedom
    ## Multiple R-squared:  0.08161,    Adjusted R-squared:  0.07162 
    ## F-statistic: 8.168 on 23 and 2114 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9972 -0.6682  0.2866  1.0512  2.6979 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               65.129543  16.032518   4.062 5.04e-05 ***
    ## real_extraversion_ct       0.053973   0.019643   2.748 0.006053 ** 
    ## real_agreeableness_ct     -0.016959   0.027009  -0.628 0.530139    
    ## real_conscientiousness_ct  0.079290   0.029407   2.696 0.007068 ** 
    ## real_emotionstability_ct  -0.004177   0.029415  -0.142 0.887097    
    ## real_openness_ct           0.112215   0.024357   4.607 4.33e-06 ***
    ## tste_11_0_ct              -0.047876   0.056051  -0.854 0.393117    
    ## tste_11_1_ct               0.057348   0.052368   1.095 0.273592    
    ## tste_11_2_ct              -0.085974   0.046561  -1.846 0.064964 .  
    ## tste_11_3_ct               0.008468   0.056959   0.149 0.881836    
    ## tste_11_4_ct              -0.165893   0.057512  -2.884 0.003960 ** 
    ## tste_11_5_ct              -0.109817   0.050179  -2.189 0.028742 *  
    ## tste_11_6_ct               0.159132   0.049149   3.238 0.001223 ** 
    ## tste_11_7_ct               0.099539   0.056547   1.760 0.078502 .  
    ## tste_11_8_ct              -0.202913   0.042103  -4.819 1.54e-06 ***
    ## tste_11_9_ct               0.170632   0.049926   3.418 0.000644 ***
    ## tste_11_10_ct              0.069675   0.054719   1.273 0.203047    
    ## release                   -0.029843   0.007893  -3.781 0.000161 ***
    ## education                  0.008893   0.026116   0.341 0.733497    
    ## income                     0.011087   0.016680   0.665 0.506316    
    ## age                       -0.013851   0.004527  -3.059 0.002245 ** 
    ## sex2                      -0.106680   0.070961  -1.503 0.132894    
    ## race4                     -0.204176   0.140194  -1.456 0.145436    
    ## star_user                 -0.082426   0.072070  -1.144 0.252881    
    ## star_GS                    0.156784   0.057356   2.734 0.006318 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.446 on 2113 degrees of freedom
    ## Multiple R-squared:  0.08297,    Adjusted R-squared:  0.07255 
    ## F-statistic: 7.966 on 24 and 2113 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0221 -0.6894  0.3041  1.0525  2.6620 
    ## 
    ## Coefficients:
    ##                                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            52.8593844 16.3825325   3.227 0.001272 ** 
    ## real_extraversion_ct                    0.0534260  0.0196614   2.717 0.006636 ** 
    ## real_agreeableness_ct                  -0.0225671  0.0271156  -0.832 0.405361    
    ## real_conscientiousness_ct               0.0770128  0.0295022   2.610 0.009107 ** 
    ## real_emotionstability_ct               -0.0019520  0.0294497  -0.066 0.947160    
    ## real_openness_ct                        0.1138237  0.0243335   4.678 3.09e-06 ***
    ## tste_12_0_ct                            0.1540781  0.0602252   2.558 0.010586 *  
    ## tste_12_1_ct                            0.0674383  0.0508908   1.325 0.185262    
    ## tste_12_2_ct                            0.0539591  0.0485547   1.111 0.266564    
    ## tste_12_3_ct                           -0.0009904  0.0548935  -0.018 0.985606    
    ## tste_12_4_ct                           -0.0746084  0.0510412  -1.462 0.143964    
    ## tste_12_5_ct                            0.1213476  0.0547460   2.217 0.026760 *  
    ## tste_12_6_ct                           -0.0566164  0.0558088  -1.014 0.310474    
    ## tste_12_7_ct                            0.0486251  0.0533048   0.912 0.361762    
    ## tste_12_8_ct                            0.1656390  0.0491039   3.373 0.000756 ***
    ## tste_12_9_ct                           -0.0571085  0.0512997  -1.113 0.265735    
    ## tste_12_10_ct                           0.1952104  0.0451689   4.322 1.62e-05 ***
    ## tste_12_11_ct                          -0.1987139  0.0483337  -4.111 4.09e-05 ***
    ## release                                -0.0239994  0.0080499  -2.981 0.002903 ** 
    ## education                               0.0120728  0.0261560   0.462 0.644438    
    ## income                                  0.0092351  0.0166785   0.554 0.579834    
    ## age                                    -0.0139889  0.0045352  -3.085 0.002065 ** 
    ## sex2                                   -0.1131863  0.0710174  -1.594 0.111135    
    ## real_conscientiousness_ct.tste_12_5_ct  0.0866517  0.0376713   2.300 0.021534 *  
    ## race4                                  -0.2222711  0.1403063  -1.584 0.113301    
    ## real_agreeableness_ct.tste_12_5_ct      0.0737820  0.0410956   1.795 0.072737 .  
    ## star_user                              -0.0452008  0.0720642  -0.627 0.530577    
    ## star_GS                                 0.1837748  0.0603496   3.045 0.002354 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.446 on 2110 degrees of freedom
    ## Multiple R-squared:  0.08475,    Adjusted R-squared:  0.07304 
    ## F-statistic: 7.236 on 27 and 2110 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2247 -0.6789  0.2998  1.0575  2.8218 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.818546  16.785528   3.147 0.001674 ** 
    ## real_extraversion_ct       0.053083   0.019688   2.696 0.007070 ** 
    ## real_agreeableness_ct     -0.014253   0.027068  -0.527 0.598562    
    ## real_conscientiousness_ct  0.083705   0.029431   2.844 0.004497 ** 
    ## real_emotionstability_ct  -0.002948   0.029477  -0.100 0.920340    
    ## real_openness_ct           0.113811   0.024383   4.668 3.24e-06 ***
    ## tste_13_0_ct               0.013463   0.050491   0.267 0.789767    
    ## tste_13_1_ct              -0.096713   0.046667  -2.072 0.038348 *  
    ## tste_13_2_ct               0.193912   0.052009   3.728 0.000198 ***
    ## tste_13_3_ct               0.059839   0.050249   1.191 0.233843    
    ## tste_13_4_ct              -0.100943   0.052947  -1.907 0.056721 .  
    ## tste_13_5_ct               0.123636   0.051926   2.381 0.017353 *  
    ## tste_13_6_ct              -0.008409   0.055772  -0.151 0.880161    
    ## tste_13_7_ct              -0.068699   0.050767  -1.353 0.176131    
    ## tste_13_8_ct               0.085768   0.046660   1.838 0.066181 .  
    ## tste_13_9_ct               0.146730   0.056049   2.618 0.008911 ** 
    ## tste_13_10_ct              0.166627   0.048586   3.429 0.000616 ***
    ## tste_13_11_ct              0.107557   0.054172   1.985 0.047223 *  
    ## tste_13_12_ct              0.054059   0.051761   1.044 0.296413    
    ## release                   -0.023784   0.008243  -2.885 0.003952 ** 
    ## education                  0.011592   0.026167   0.443 0.657805    
    ## income                     0.007903   0.016690   0.474 0.635899    
    ## age                       -0.013626   0.004543  -2.999 0.002737 ** 
    ## sex2                      -0.116141   0.071119  -1.633 0.102605    
    ## race4                     -0.200500   0.140509  -1.427 0.153741    
    ## star_user                 -0.087876   0.071526  -1.229 0.219365    
    ## star_GS                    0.179881   0.059408   3.028 0.002492 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2111 degrees of freedom
    ## Multiple R-squared:  0.08046,    Adjusted R-squared:  0.06913 
    ## F-statistic: 7.104 on 26 and 2111 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2094 -0.6780  0.2964  1.0548  2.7246 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               47.268509  16.719456   2.827 0.004741 ** 
    ## real_extraversion_ct       0.055598   0.019699   2.822 0.004812 ** 
    ## real_agreeableness_ct     -0.014743   0.027052  -0.545 0.585814    
    ## real_conscientiousness_ct  0.079956   0.029431   2.717 0.006646 ** 
    ## real_emotionstability_ct  -0.003782   0.029439  -0.128 0.897789    
    ## real_openness_ct           0.113519   0.024357   4.661 3.35e-06 ***
    ## tste_14_0_ct              -0.102207   0.051375  -1.989 0.046784 *  
    ## tste_14_1_ct              -0.002200   0.051587  -0.043 0.965984    
    ## tste_14_2_ct               0.049887   0.047468   1.051 0.293399    
    ## tste_14_3_ct               0.116079   0.046396   2.502 0.012427 *  
    ## tste_14_4_ct               0.025999   0.043382   0.599 0.549037    
    ## tste_14_5_ct              -0.039650   0.053978  -0.735 0.462683    
    ## tste_14_6_ct               0.024323   0.050831   0.479 0.632331    
    ## tste_14_7_ct              -0.255815   0.059378  -4.308 1.72e-05 ***
    ## tste_14_8_ct               0.157482   0.045360   3.472 0.000527 ***
    ## tste_14_9_ct              -0.018931   0.051478  -0.368 0.713099    
    ## tste_14_10_ct              0.109152   0.049318   2.213 0.026988 *  
    ## tste_14_11_ct             -0.055155   0.049019  -1.125 0.260647    
    ## tste_14_12_ct              0.086154   0.051555   1.671 0.094853 .  
    ## tste_14_13_ct              0.116557   0.052122   2.236 0.025441 *  
    ## release                   -0.021189   0.008221  -2.578 0.010018 *  
    ## education                  0.008104   0.026167   0.310 0.756825    
    ## income                     0.009129   0.016675   0.547 0.584125    
    ## age                       -0.013192   0.004539  -2.906 0.003698 ** 
    ## sex2                      -0.108885   0.071118  -1.531 0.125910    
    ## race4                     -0.197458   0.140364  -1.407 0.159647    
    ## star_user                 -0.039291   0.071904  -0.546 0.584818    
    ## star_GS                    0.170903   0.059527   2.871 0.004132 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 2110 degrees of freedom
    ## Multiple R-squared:  0.08271,    Adjusted R-squared:  0.07098 
    ## F-statistic: 7.047 on 27 and 2110 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2011 -0.6661  0.3069  1.0615  2.7882 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               54.471460  17.690223   3.079 0.002102 ** 
    ## real_extraversion_ct       0.054744   0.019711   2.777 0.005529 ** 
    ## real_agreeableness_ct     -0.016939   0.027070  -0.626 0.531548    
    ## real_conscientiousness_ct  0.081537   0.029452   2.768 0.005682 ** 
    ## real_emotionstability_ct  -0.001418   0.029504  -0.048 0.961668    
    ## real_openness_ct           0.114618   0.024378   4.702 2.75e-06 ***
    ## tste_15_0_ct              -0.060354   0.047968  -1.258 0.208450    
    ## tste_15_1_ct              -0.121737   0.053634  -2.270 0.023321 *  
    ## tste_15_2_ct               0.157843   0.048525   3.253 0.001161 ** 
    ## tste_15_3_ct               0.094480   0.052518   1.799 0.072159 .  
    ## tste_15_4_ct               0.007150   0.046918   0.152 0.878886    
    ## tste_15_5_ct              -0.071640   0.053684  -1.334 0.182192    
    ## tste_15_6_ct              -0.114900   0.046600  -2.466 0.013755 *  
    ## tste_15_7_ct              -0.066139   0.051648  -1.281 0.200487    
    ## tste_15_8_ct              -0.124520   0.053970  -2.307 0.021141 *  
    ## tste_15_9_ct               0.030756   0.051092   0.602 0.547248    
    ## tste_15_10_ct              0.048474   0.053223   0.911 0.362522    
    ## tste_15_11_ct              0.183013   0.054256   3.373 0.000757 ***
    ## tste_15_12_ct             -0.047382   0.038135  -1.242 0.214197    
    ## tste_15_13_ct              0.029954   0.044403   0.675 0.500014    
    ## tste_15_14_ct              0.048799   0.046883   1.041 0.298058    
    ## release                   -0.024529   0.008688  -2.823 0.004796 ** 
    ## education                  0.008353   0.026186   0.319 0.749766    
    ## income                     0.009007   0.016703   0.539 0.589773    
    ## age                       -0.013283   0.004542  -2.925 0.003484 ** 
    ## sex2                      -0.112037   0.071178  -1.574 0.115630    
    ## race4                     -0.197111   0.140489  -1.403 0.160755    
    ## star_user                 -0.070223   0.075571  -0.929 0.352875    
    ## star_GS                    0.142871   0.060251   2.371 0.017817 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2109 degrees of freedom
    ## Multiple R-squared:  0.08156,    Adjusted R-squared:  0.06937 
    ## F-statistic: 6.689 on 28 and 2109 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0509 -0.6880  0.2752  1.0600  2.7534 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   37.072659  17.675469   2.097 0.036076 *  
    ## real_extraversion_ct           0.057604   0.019666   2.929 0.003436 ** 
    ## real_agreeableness_ct         -0.018816   0.026993  -0.697 0.485821    
    ## real_conscientiousness_ct      0.080985   0.029340   2.760 0.005826 ** 
    ## real_emotionstability_ct       0.004694   0.029407   0.160 0.873191    
    ## real_openness_ct               0.114901   0.024391   4.711 2.63e-06 ***
    ## tste_16_0_ct                   0.175759   0.048637   3.614 0.000309 ***
    ## tste_16_1_ct                  -0.095931   0.049843  -1.925 0.054403 .  
    ## tste_16_2_ct                  -0.053871   0.044941  -1.199 0.230777    
    ## tste_16_3_ct                  -0.158480   0.050515  -3.137 0.001729 ** 
    ## tste_16_4_ct                  -0.170568   0.041265  -4.133 3.71e-05 ***
    ## tste_16_5_ct                   0.103465   0.048212   2.146 0.031983 *  
    ## tste_16_6_ct                   0.103615   0.044087   2.350 0.018852 *  
    ## tste_16_7_ct                   0.075289   0.044627   1.687 0.091737 .  
    ## tste_16_8_ct                  -0.055452   0.052020  -1.066 0.286558    
    ## tste_16_9_ct                   0.155305   0.044611   3.481 0.000509 ***
    ## tste_16_10_ct                 -0.077020   0.044054  -1.748 0.080557 .  
    ## tste_16_11_ct                  0.145173   0.050153   2.895 0.003835 ** 
    ## tste_16_12_ct                  0.078929   0.046157   1.710 0.087412 .  
    ## tste_16_13_ct                  0.055342   0.049744   1.113 0.266032    
    ## tste_16_14_ct                  0.115498   0.046258   2.497 0.012607 *  
    ## tste_16_15_ct                 -0.010722   0.050156  -0.214 0.830745    
    ## release                       -0.015931   0.008683  -1.835 0.066695 .  
    ## education                      0.007633   0.026084   0.293 0.769837    
    ## income                         0.012717   0.016666   0.763 0.445506    
    ## age                           -0.012087   0.004536  -2.665 0.007758 ** 
    ## sex2                          -0.114853   0.070855  -1.621 0.105178    
    ## race4                         -0.192633   0.140022  -1.376 0.169049    
    ## real_openness_ct.tste_16_7_ct -0.017272   0.031112  -0.555 0.578858    
    ## star_user                     -0.064030   0.072316  -0.885 0.376028    
    ## star_GS                        0.147051   0.058452   2.516 0.011952 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.444 on 2107 degrees of freedom
    ## Multiple R-squared:  0.08926,    Adjusted R-squared:  0.07629 
    ## F-statistic: 6.883 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2729 -0.6946  0.2752  1.0489  2.8206 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               39.644020  18.218014   2.176  0.02966 *  
    ## real_extraversion_ct       0.056149   0.019622   2.862  0.00426 ** 
    ## real_agreeableness_ct     -0.015103   0.026958  -0.560  0.57536    
    ## real_conscientiousness_ct  0.081792   0.029299   2.792  0.00529 ** 
    ## real_emotionstability_ct   0.002201   0.029357   0.075  0.94023    
    ## real_openness_ct           0.113473   0.024268   4.676 3.11e-06 ***
    ## tste_17_0_ct               0.312229   0.048936   6.380 2.17e-10 ***
    ## tste_17_1_ct               0.060881   0.041883   1.454  0.14621    
    ## tste_17_2_ct              -0.070495   0.049930  -1.412  0.15814    
    ## tste_17_3_ct               0.052478   0.047690   1.100  0.27129    
    ## tste_17_4_ct               0.092241   0.050173   1.838  0.06613 .  
    ## tste_17_5_ct               0.016002   0.046722   0.343  0.73201    
    ## tste_17_6_ct              -0.052525   0.049120  -1.069  0.28505    
    ## tste_17_7_ct               0.090685   0.043144   2.102  0.03568 *  
    ## tste_17_8_ct              -0.068675   0.047901  -1.434  0.15181    
    ## tste_17_9_ct              -0.100595   0.046190  -2.178  0.02953 *  
    ## tste_17_10_ct              0.090998   0.042471   2.143  0.03226 *  
    ## tste_17_11_ct              0.030862   0.046746   0.660  0.50918    
    ## tste_17_12_ct             -0.170165   0.042857  -3.971 7.41e-05 ***
    ## tste_17_13_ct              0.091296   0.049117   1.859  0.06320 .  
    ## tste_17_14_ct              0.024990   0.043291   0.577  0.56383    
    ## tste_17_15_ct             -0.039113   0.055945  -0.699  0.48455    
    ## tste_17_16_ct             -0.044979   0.042764  -1.052  0.29301    
    ## release                   -0.017263   0.008925  -1.934  0.05322 .  
    ## education                  0.008644   0.026050   0.332  0.74005    
    ## income                     0.012449   0.016633   0.748  0.45429    
    ## age                       -0.012136   0.004535  -2.676  0.00750 ** 
    ## sex2                      -0.118227   0.070754  -1.671  0.09488 .  
    ## race4                     -0.201037   0.139880  -1.437  0.15081    
    ## star_user                 -0.078050   0.076682  -1.018  0.30887    
    ## star_GS                    0.173453   0.060214   2.881  0.00401 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2107 degrees of freedom
    ## Multiple R-squared:  0.0915, Adjusted R-squared:  0.07857 
    ## F-statistic: 7.074 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1367 -0.6728  0.2909  1.0461  2.6461 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.0009513 18.0116346   2.721 0.006572 ** 
    ## real_extraversion_ct       0.0539486  0.0196223   2.749 0.006022 ** 
    ## real_agreeableness_ct     -0.0144339  0.0269545  -0.535 0.592368    
    ## real_conscientiousness_ct  0.0804806  0.0293196   2.745 0.006103 ** 
    ## real_emotionstability_ct  -0.0009901  0.0293760  -0.034 0.973117    
    ## real_openness_ct           0.1127427  0.0242707   4.645 3.61e-06 ***
    ## tste_18_0_ct              -0.1264374  0.0460504  -2.746 0.006091 ** 
    ## tste_18_1_ct               0.1622520  0.0479474   3.384 0.000728 ***
    ## tste_18_2_ct              -0.0193104  0.0435797  -0.443 0.657735    
    ## tste_18_3_ct              -0.0185158  0.0467999  -0.396 0.692412    
    ## tste_18_4_ct               0.0794905  0.0510795   1.556 0.119808    
    ## tste_18_5_ct              -0.0331148  0.0479526  -0.691 0.489909    
    ## tste_18_6_ct              -0.0036417  0.0503341  -0.072 0.942330    
    ## tste_18_7_ct              -0.0542796  0.0432246  -1.256 0.209343    
    ## tste_18_8_ct               0.0769505  0.0470391   1.636 0.102013    
    ## tste_18_9_ct              -0.1340476  0.0477549  -2.807 0.005047 ** 
    ## tste_18_10_ct              0.0818003  0.0458458   1.784 0.074528 .  
    ## tste_18_11_ct              0.0619243  0.0451237   1.372 0.170109    
    ## tste_18_12_ct             -0.0813153  0.0474826  -1.713 0.086947 .  
    ## tste_18_13_ct             -0.0307936  0.0467396  -0.659 0.510074    
    ## tste_18_14_ct              0.0436479  0.0507619   0.860 0.389967    
    ## tste_18_15_ct              0.1655856  0.0584783   2.832 0.004676 ** 
    ## tste_18_16_ct             -0.2898596  0.0513587  -5.644 1.89e-08 ***
    ## tste_18_17_ct              0.0495184  0.0456545   1.085 0.278208    
    ## release                   -0.0215730  0.0088396  -2.440 0.014750 *  
    ## education                  0.0084977  0.0260495   0.326 0.744297    
    ## income                     0.0121532  0.0166414   0.730 0.465289    
    ## age                       -0.0120853  0.0045360  -2.664 0.007773 ** 
    ## sex2                      -0.1114803  0.0708257  -1.574 0.115635    
    ## race4                     -0.1975673  0.1398896  -1.412 0.158007    
    ## star_user                 -0.0805028  0.0770661  -1.045 0.296331    
    ## star_GS                    0.0914648  0.0609360   1.501 0.133506    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2106 degrees of freedom
    ## Multiple R-squared:  0.09172,    Adjusted R-squared:  0.07835 
    ## F-statistic:  6.86 on 31 and 2106 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2201 -0.6718  0.2711  1.0711  2.6166 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   41.816280  18.439707   2.268 0.023447 *  
    ## real_extraversion_ct           0.056447   0.019685   2.867 0.004179 ** 
    ## real_agreeableness_ct         -0.016437   0.027074  -0.607 0.543846    
    ## real_conscientiousness_ct      0.081909   0.029408   2.785 0.005395 ** 
    ## real_emotionstability_ct       0.001975   0.029503   0.067 0.946630    
    ## real_openness_ct               0.112459   0.024452   4.599 4.49e-06 ***
    ## tste_19_0_ct                  -0.002301   0.048157  -0.048 0.961889    
    ## tste_19_1_ct                   0.063526   0.050306   1.263 0.206805    
    ## tste_19_2_ct                   0.030021   0.048935   0.613 0.539620    
    ## tste_19_3_ct                  -0.113906   0.046508  -2.449 0.014399 *  
    ## tste_19_4_ct                  -0.105576   0.048955  -2.157 0.031149 *  
    ## tste_19_5_ct                  -0.031891   0.053445  -0.597 0.550761    
    ## tste_19_6_ct                   0.082464   0.047787   1.726 0.084558 .  
    ## tste_19_7_ct                   0.117671   0.046770   2.516 0.011945 *  
    ## tste_19_8_ct                   0.041037   0.046842   0.876 0.381100    
    ## tste_19_9_ct                  -0.071558   0.050199  -1.425 0.154169    
    ## tste_19_10_ct                  0.131082   0.047929   2.735 0.006291 ** 
    ## tste_19_11_ct                  0.082922   0.044843   1.849 0.064578 .  
    ## tste_19_12_ct                  0.227133   0.045611   4.980 6.88e-07 ***
    ## tste_19_13_ct                 -0.009246   0.043174  -0.214 0.830453    
    ## tste_19_14_ct                  0.167809   0.044961   3.732 0.000195 ***
    ## tste_19_15_ct                  0.002999   0.044180   0.068 0.945886    
    ## tste_19_16_ct                  0.021047   0.046911   0.449 0.653733    
    ## tste_19_17_ct                  0.062674   0.052974   1.183 0.236896    
    ## tste_19_18_ct                  0.006263   0.040968   0.153 0.878509    
    ## release                       -0.018300   0.009051  -2.022 0.043320 *  
    ## education                      0.008060   0.026110   0.309 0.757588    
    ## income                         0.012461   0.016672   0.747 0.454902    
    ## age                           -0.011977   0.004547  -2.634 0.008505 ** 
    ## sex2                          -0.113061   0.071005  -1.592 0.111470    
    ## race4                         -0.196678   0.140277  -1.402 0.161043    
    ## real_openness_ct.tste_19_0_ct  0.033093   0.027793   1.191 0.233914    
    ## star_user                     -0.031928   0.074608  -0.428 0.668734    
    ## star_GS                        0.115816   0.060297   1.921 0.054897 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2104 degrees of freedom
    ## Multiple R-squared:  0.089,  Adjusted R-squared:  0.07472 
    ## F-statistic: 6.229 on 33 and 2104 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0883 -0.6965  0.2753  1.0534  2.5752 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               38.766626  19.257231   2.013 0.044232 *  
    ## real_extraversion_ct       0.057237   0.019637   2.915 0.003597 ** 
    ## real_agreeableness_ct     -0.015047   0.026981  -0.558 0.577113    
    ## real_conscientiousness_ct  0.082334   0.029331   2.807 0.005046 ** 
    ## real_emotionstability_ct   0.004438   0.029413   0.151 0.880090    
    ## real_openness_ct           0.113520   0.024272   4.677 3.09e-06 ***
    ## tste_20_0_ct               0.072807   0.053503   1.361 0.173721    
    ## tste_20_1_ct               0.081489   0.046503   1.752 0.079858 .  
    ## tste_20_2_ct               0.090467   0.042430   2.132 0.033111 *  
    ## tste_20_3_ct               0.149794   0.048689   3.077 0.002121 ** 
    ## tste_20_4_ct               0.038892   0.044312   0.878 0.380211    
    ## tste_20_5_ct              -0.070417   0.043820  -1.607 0.108211    
    ## tste_20_6_ct               0.032698   0.049497   0.661 0.508940    
    ## tste_20_7_ct               0.189709   0.054113   3.506 0.000465 ***
    ## tste_20_8_ct              -0.018811   0.053727  -0.350 0.726285    
    ## tste_20_9_ct               0.097644   0.044206   2.209 0.027294 *  
    ## tste_20_10_ct              0.060035   0.048028   1.250 0.211436    
    ## tste_20_11_ct             -0.126605   0.045594  -2.777 0.005538 ** 
    ## tste_20_12_ct              0.124630   0.046042   2.707 0.006847 ** 
    ## tste_20_13_ct              0.113469   0.050321   2.255 0.024241 *  
    ## tste_20_14_ct             -0.098946   0.041035  -2.411 0.015984 *  
    ## tste_20_15_ct              0.115275   0.053238   2.165 0.030477 *  
    ## tste_20_16_ct             -0.095114   0.046145  -2.061 0.039406 *  
    ## tste_20_17_ct              0.036502   0.044887   0.813 0.416200    
    ## tste_20_18_ct              0.140027   0.042631   3.285 0.001038 ** 
    ## tste_20_19_ct             -0.043575   0.048612  -0.896 0.370150    
    ## release                   -0.016702   0.009453  -1.767 0.077396 .  
    ## education                  0.008828   0.026050   0.339 0.734741    
    ## income                     0.012271   0.016640   0.737 0.460940    
    ## age                       -0.011843   0.004539  -2.609 0.009144 ** 
    ## sex2                      -0.105838   0.070898  -1.493 0.135633    
    ## race4                     -0.188465   0.139919  -1.347 0.178137    
    ## star_user                 -0.012400   0.077938  -0.159 0.873606    
    ## star_GS                    0.076104   0.061238   1.243 0.214096    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2104 degrees of freedom
    ## Multiple R-squared:  0.09294,    Adjusted R-squared:  0.07872 
    ## F-statistic: 6.533 on 33 and 2104 DF,  p-value: < 2.2e-16

### preference ~ tste + real + game

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 39:57)$model_lm_1) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9931 -0.6541  0.3070  1.0691  2.5972 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.6522646 14.1921015   4.556 5.52e-06 ***
    ## real_extraversion_ct       0.0411285  0.0233985   1.758  0.07894 .  
    ## real_agreeableness_ct     -0.0542494  0.0333147  -1.628  0.10359    
    ## real_conscientiousness_ct  0.0830848  0.0370427   2.243  0.02500 *  
    ## real_emotionstability_ct   0.0001318  0.0331876   0.004  0.99683    
    ## real_openness_ct           0.1427409  0.0330037   4.325 1.60e-05 ***
    ## game_extraversion_ct       0.0062347  0.0260999   0.239  0.81122    
    ## game_agreeableness_ct      0.0691362  0.0339170   2.038  0.04163 *  
    ## game_conscientiousness_ct  0.0043454  0.0399756   0.109  0.91345    
    ## game_emotionstability_ct  -0.0360643  0.0318305  -1.133  0.25734    
    ## game_openness_ct          -0.0376940  0.0435241  -0.866  0.38656    
    ## tste_2_0_ct                0.0959709  0.0413051   2.323  0.02025 *  
    ## tste_2_1_ct               -0.0743790  0.0402641  -1.847  0.06485 .  
    ## release                   -0.0293509  0.0069808  -4.205 2.73e-05 ***
    ## education                  0.0104698  0.0267227   0.392  0.69525    
    ## income                     0.0079546  0.0172654   0.461  0.64504    
    ## race7                     -0.1433373  0.1375388  -1.042  0.29746    
    ## sex2                      -0.0808576  0.0728770  -1.110  0.26734    
    ## age                       -0.0150509  0.0046288  -3.252  0.00117 ** 
    ## race4                     -0.2541625  0.1421136  -1.788  0.07385 .  
    ## race6                     -0.9691379  0.3497740  -2.771  0.00564 ** 
    ## race2                      0.0803499  0.1325024   0.606  0.54431    
    ## star_user                  0.0092683  0.0579591   0.160  0.87297    
    ## star_GS                    0.0099173  0.0480447   0.206  0.83648    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2114 degrees of freedom
    ## Multiple R-squared:  0.06308,    Adjusted R-squared:  0.05289 
    ## F-statistic: 6.188 on 23 and 2114 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8730 -0.6937  0.3037  1.0694  2.4998 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.869158  14.145624   4.162 3.29e-05 ***
    ## real_extraversion_ct       0.039770   0.023248   1.711 0.087288 .  
    ## real_agreeableness_ct     -0.057920   0.033110  -1.749 0.080381 .  
    ## real_conscientiousness_ct  0.082458   0.036807   2.240 0.025178 *  
    ## real_emotionstability_ct   0.002550   0.032975   0.077 0.938378    
    ## real_openness_ct           0.146612   0.032800   4.470 8.24e-06 ***
    ## game_extraversion_ct       0.005873   0.025930   0.226 0.820838    
    ## game_agreeableness_ct      0.074517   0.033715   2.210 0.027200 *  
    ## game_conscientiousness_ct  0.006334   0.039723   0.159 0.873317    
    ## game_emotionstability_ct  -0.036509   0.031630  -1.154 0.248526    
    ## game_openness_ct          -0.041551   0.043253  -0.961 0.336846    
    ## tste_3_0_ct               -0.043515   0.043427  -1.002 0.316449    
    ## tste_3_1_ct                0.154360   0.035306   4.372 1.29e-05 ***
    ## tste_3_2_ct                0.243954   0.049356   4.943 8.31e-07 ***
    ## release                   -0.026611   0.006957  -3.825 0.000135 ***
    ## education                  0.007901   0.026557   0.298 0.766108    
    ## income                     0.009312   0.017157   0.543 0.587360    
    ## race7                     -0.158922   0.136694  -1.163 0.245119    
    ## sex2                      -0.069351   0.072438  -0.957 0.338482    
    ## age                       -0.014890   0.004599  -3.237 0.001225 ** 
    ## race4                     -0.228635   0.141294  -1.618 0.105779    
    ## race6                     -0.946277   0.347599  -2.722 0.006536 ** 
    ## race2                      0.075986   0.131663   0.577 0.563916    
    ## star_user                  0.009049   0.057709   0.157 0.875411    
    ## star_GS                    0.043285   0.048476   0.893 0.372002    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.452 on 2113 degrees of freedom
    ## Multiple R-squared:  0.07541,    Adjusted R-squared:  0.06491 
    ## F-statistic: 7.181 on 24 and 2113 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9275 -0.6835  0.3006  1.0551  2.5790 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               63.144146  14.491327   4.357 1.38e-05 ***
    ## real_extraversion_ct       0.039758   0.023236   1.711  0.08722 .  
    ## real_agreeableness_ct     -0.056360   0.033092  -1.703  0.08869 .  
    ## real_conscientiousness_ct  0.083275   0.036782   2.264  0.02368 *  
    ## real_emotionstability_ct   0.002923   0.032958   0.089  0.92934    
    ## real_openness_ct           0.147070   0.032780   4.487 7.63e-06 ***
    ## game_extraversion_ct       0.005645   0.025914   0.218  0.82759    
    ## game_agreeableness_ct      0.073709   0.033684   2.188  0.02876 *  
    ## game_conscientiousness_ct  0.005331   0.039690   0.134  0.89316    
    ## game_emotionstability_ct  -0.036428   0.031604  -1.153  0.24919    
    ## game_openness_ct          -0.041401   0.043221  -0.958  0.33823    
    ## tste_4_0_ct                0.258112   0.049956   5.167 2.61e-07 ***
    ## tste_4_1_ct                0.048699   0.056510   0.862  0.38890    
    ## tste_4_2_ct                0.061376   0.041537   1.478  0.13966    
    ## tste_4_3_ct               -0.164554   0.039330  -4.184 2.98e-05 ***
    ## release                   -0.028676   0.007124  -4.025 5.90e-05 ***
    ## education                  0.007609   0.026537   0.287  0.77433    
    ## income                     0.008827   0.017143   0.515  0.60669    
    ## race7                     -0.158167   0.136583  -1.158  0.24698    
    ## sex2                      -0.066720   0.072403  -0.922  0.35689    
    ## age                       -0.014687   0.004599  -3.193  0.00143 ** 
    ## race4                     -0.226971   0.141200  -1.607  0.10811    
    ## race6                     -0.949383   0.347328  -2.733  0.00632 ** 
    ## race2                      0.074648   0.131622   0.567  0.57068    
    ## star_GS                    0.076425   0.052662   1.451  0.14686    
    ## star_user                 -0.039911   0.064656  -0.617  0.53711    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.451 on 2112 degrees of freedom
    ## Multiple R-squared:  0.07734,    Adjusted R-squared:  0.06641 
    ## F-statistic: 7.081 on 25 and 2112 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8861 -0.6862  0.3003  1.0547  2.5592 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.623114  14.784023   3.492  0.00049 ***
    ## real_extraversion_ct       0.041609   0.023269   1.788  0.07389 .  
    ## real_agreeableness_ct     -0.056032   0.033137  -1.691  0.09100 .  
    ## real_conscientiousness_ct  0.083185   0.036826   2.259  0.02399 *  
    ## real_emotionstability_ct   0.002372   0.032997   0.072  0.94271    
    ## real_openness_ct           0.146067   0.032818   4.451 9.00e-06 ***
    ## game_extraversion_ct       0.006134   0.025951   0.236  0.81317    
    ## game_agreeableness_ct      0.073359   0.033723   2.175  0.02972 *  
    ## game_conscientiousness_ct  0.006639   0.039744   0.167  0.86735    
    ## game_emotionstability_ct  -0.035981   0.031647  -1.137  0.25570    
    ## game_openness_ct          -0.042940   0.043305  -0.992  0.32152    
    ## tste_5_0_ct                0.232744   0.046980   4.954 7.84e-07 ***
    ## tste_5_1_ct               -0.159204   0.051992  -3.062  0.00223 ** 
    ## tste_5_2_ct               -0.029038   0.040805  -0.712  0.47677    
    ## tste_5_3_ct               -0.087763   0.050652  -1.733  0.08330 .  
    ## tste_5_4_ct                0.057287   0.052756   1.086  0.27766    
    ## release                   -0.022933   0.007268  -3.155  0.00163 ** 
    ## education                  0.007991   0.026576   0.301  0.76370    
    ## income                     0.010021   0.017173   0.584  0.55960    
    ## race7                     -0.154606   0.136775  -1.130  0.25845    
    ## sex2                      -0.073787   0.072491  -1.018  0.30885    
    ## age                       -0.014879   0.004606  -3.230  0.00126 ** 
    ## race4                     -0.232058   0.141376  -1.641  0.10086    
    ## race6                     -0.958004   0.347782  -2.755  0.00593 ** 
    ## race2                      0.065020   0.131781   0.493  0.62179    
    ## star_user                 -0.023738   0.067110  -0.354  0.72358    
    ## star_GS                    0.059432   0.053572   1.109  0.26739    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2111 degrees of freedom
    ## Multiple R-squared:  0.07538,    Adjusted R-squared:  0.06399 
    ## F-statistic: 6.619 on 26 and 2111 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9352 -0.6869  0.3030  1.0545  2.5636 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               50.423861  14.935336   3.376 0.000748 ***
    ## real_extraversion_ct       0.041761   0.023274   1.794 0.072905 .  
    ## real_agreeableness_ct     -0.056104   0.033143  -1.693 0.090647 .  
    ## real_conscientiousness_ct  0.081949   0.036846   2.224 0.026246 *  
    ## real_emotionstability_ct   0.003166   0.033000   0.096 0.923580    
    ## real_openness_ct           0.147853   0.032824   4.504 7.02e-06 ***
    ## game_extraversion_ct       0.008189   0.025967   0.315 0.752515    
    ## game_agreeableness_ct      0.071096   0.033760   2.106 0.035330 *  
    ## game_conscientiousness_ct  0.007301   0.039762   0.184 0.854323    
    ## game_emotionstability_ct  -0.035617   0.031653  -1.125 0.260631    
    ## game_openness_ct          -0.043968   0.043337  -1.015 0.310428    
    ## tste_6_0_ct                0.055825   0.052683   1.060 0.289423    
    ## tste_6_1_ct               -0.111559   0.050497  -2.209 0.027268 *  
    ## tste_6_2_ct                0.246208   0.043472   5.664 1.69e-08 ***
    ## tste_6_3_ct                0.010008   0.053055   0.189 0.850399    
    ## tste_6_4_ct                0.105034   0.047591   2.207 0.027420 *  
    ## tste_6_5_ct                0.085212   0.055724   1.529 0.126369    
    ## release                   -0.022284   0.007343  -3.035 0.002437 ** 
    ## education                  0.007484   0.026582   0.282 0.778321    
    ## income                     0.010364   0.017178   0.603 0.546355    
    ## race7                     -0.150547   0.136853  -1.100 0.271432    
    ## sex2                      -0.071542   0.072507  -0.987 0.323903    
    ## age                       -0.014804   0.004607  -3.213 0.001332 ** 
    ## race4                     -0.227236   0.141420  -1.607 0.108245    
    ## race6                     -0.967626   0.347892  -2.781 0.005461 ** 
    ## race2                      0.053269   0.131938   0.404 0.686440    
    ## star_user                 -0.041871   0.069544  -0.602 0.547188    
    ## star_GS                    0.065038   0.054593   1.191 0.233657    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2110 degrees of freedom
    ## Multiple R-squared:  0.07546,    Adjusted R-squared:  0.06363 
    ## F-statistic: 6.378 on 27 and 2110 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8814 -0.6935  0.2868  1.0562  2.5640 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               53.455036  15.214662   3.513 0.000452 ***
    ## real_extraversion_ct       0.041337   0.023250   1.778 0.075563 .  
    ## real_agreeableness_ct     -0.055114   0.033098  -1.665 0.096032 .  
    ## real_conscientiousness_ct  0.081908   0.036807   2.225 0.026162 *  
    ## real_emotionstability_ct   0.001457   0.032985   0.044 0.964766    
    ## real_openness_ct           0.145678   0.032817   4.439 9.50e-06 ***
    ## game_extraversion_ct       0.005631   0.025932   0.217 0.828104    
    ## game_agreeableness_ct      0.074457   0.033690   2.210 0.027207 *  
    ## game_conscientiousness_ct  0.007171   0.039707   0.181 0.856703    
    ## game_emotionstability_ct  -0.035216   0.031619  -1.114 0.265506    
    ## game_openness_ct          -0.042446   0.043322  -0.980 0.327305    
    ## tste_7_0_ct               -0.180085   0.048030  -3.749 0.000182 ***
    ## tste_7_1_ct                0.025317   0.051673   0.490 0.624217    
    ## tste_7_2_ct               -0.160095   0.051109  -3.132 0.001758 ** 
    ## tste_7_3_ct               -0.206448   0.038886  -5.309 1.22e-07 ***
    ## tste_7_4_ct                0.003771   0.052716   0.072 0.942987    
    ## tste_7_5_ct                0.022925   0.056242   0.408 0.683596    
    ## tste_7_6_ct                0.040807   0.059877   0.682 0.495618    
    ## release                   -0.023870   0.007489  -3.187 0.001456 ** 
    ## education                  0.008330   0.026550   0.314 0.753733    
    ## income                     0.009882   0.017155   0.576 0.564649    
    ## race7                     -0.147010   0.136639  -1.076 0.282096    
    ## sex2                      -0.074612   0.072401  -1.031 0.302873    
    ## age                       -0.014537   0.004601  -3.159 0.001604 ** 
    ## race4                     -0.221892   0.141259  -1.571 0.116374    
    ## race6                     -0.949254   0.347379  -2.733 0.006336 ** 
    ## race2                      0.069570   0.131669   0.528 0.597297    
    ## star_user                 -0.024539   0.070349  -0.349 0.727263    
    ## star_GS                    0.064669   0.052890   1.223 0.221578    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.451 on 2109 degrees of freedom
    ## Multiple R-squared:  0.07867,    Adjusted R-squared:  0.06644 
    ## F-statistic: 6.431 on 28 and 2109 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0031 -0.6912  0.2886  1.0309  2.7192 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               59.496280  15.921444   3.737 0.000191 ***
    ## real_extraversion_ct       0.041509   0.023212   1.788 0.073880 .  
    ## real_agreeableness_ct     -0.051945   0.033052  -1.572 0.116195    
    ## real_conscientiousness_ct  0.082208   0.036736   2.238 0.025339 *  
    ## real_emotionstability_ct   0.002538   0.032918   0.077 0.938559    
    ## real_openness_ct           0.149667   0.032730   4.573 5.09e-06 ***
    ## game_extraversion_ct       0.007515   0.025878   0.290 0.771544    
    ## game_agreeableness_ct      0.070597   0.033629   2.099 0.035908 *  
    ## game_conscientiousness_ct  0.007377   0.039617   0.186 0.852293    
    ## game_emotionstability_ct  -0.036615   0.031542  -1.161 0.245843    
    ## game_openness_ct          -0.050090   0.043213  -1.159 0.246531    
    ## tste_8_0_ct               -0.197448   0.050794  -3.887 0.000105 ***
    ## tste_8_1_ct                0.210092   0.057455   3.657 0.000262 ***
    ## tste_8_2_ct                0.100758   0.050813   1.983 0.047507 *  
    ## tste_8_3_ct                0.082177   0.047529   1.729 0.083958 .  
    ## tste_8_4_ct                0.136316   0.052277   2.608 0.009183 ** 
    ## tste_8_5_ct                0.092385   0.049365   1.871 0.061421 .  
    ## tste_8_6_ct               -0.118927   0.052928  -2.247 0.024745 *  
    ## tste_8_7_ct               -0.050313   0.051892  -0.970 0.332365    
    ## release                   -0.026748   0.007837  -3.413 0.000654 ***
    ## education                  0.008354   0.026493   0.315 0.752527    
    ## income                     0.009647   0.017117   0.564 0.573066    
    ## race7                     -0.136292   0.136385  -0.999 0.317757    
    ## sex2                      -0.076094   0.072215  -1.054 0.292134    
    ## age                       -0.014453   0.004592  -3.148 0.001670 ** 
    ## race4                     -0.210361   0.140985  -1.492 0.135828    
    ## race6                     -0.976566   0.346789  -2.816 0.004907 ** 
    ## race2                      0.055160   0.131394   0.420 0.674669    
    ## star_user                 -0.083664   0.070738  -1.183 0.237050    
    ## star_GS                    0.092438   0.055047   1.679 0.093248 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 2108 degrees of freedom
    ## Multiple R-squared:  0.08313,    Adjusted R-squared:  0.07052 
    ## F-statistic: 6.591 on 29 and 2108 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9361 -0.6883  0.2845  1.0552  2.8493 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.554270  16.053224   3.772 0.000166 ***
    ## real_extraversion_ct       0.044168   0.023171   1.906 0.056768 .  
    ## real_agreeableness_ct     -0.054153   0.032985  -1.642 0.100796    
    ## real_conscientiousness_ct  0.079596   0.036666   2.171 0.030056 *  
    ## real_emotionstability_ct   0.002353   0.032863   0.072 0.942925    
    ## real_openness_ct           0.145140   0.032704   4.438 9.55e-06 ***
    ## game_extraversion_ct       0.007393   0.025824   0.286 0.774683    
    ## game_agreeableness_ct      0.067051   0.033594   1.996 0.046076 *  
    ## game_conscientiousness_ct  0.010643   0.039557   0.269 0.787920    
    ## game_emotionstability_ct  -0.032859   0.031517  -1.043 0.297263    
    ## game_openness_ct          -0.049916   0.043163  -1.156 0.247629    
    ## tste_9_0_ct               -0.158720   0.053309  -2.977 0.002940 ** 
    ## tste_9_1_ct                0.039544   0.050697   0.780 0.435475    
    ## tste_9_2_ct               -0.348932   0.052307  -6.671 3.24e-11 ***
    ## tste_9_3_ct               -0.050250   0.057616  -0.872 0.383222    
    ## tste_9_4_ct               -0.016538   0.043787  -0.378 0.705702    
    ## tste_9_5_ct                0.018766   0.057524   0.326 0.744285    
    ## tste_9_6_ct               -0.014905   0.051346  -0.290 0.771628    
    ## tste_9_7_ct               -0.067870   0.052883  -1.283 0.199493    
    ## tste_9_8_ct                0.082536   0.046964   1.757 0.078992 .  
    ## release                   -0.027420   0.007892  -3.474 0.000522 ***
    ## education                  0.006517   0.026437   0.247 0.805310    
    ## income                     0.013458   0.017115   0.786 0.431768    
    ## race7                     -0.123406   0.136250  -0.906 0.365180    
    ## sex2                      -0.070082   0.072101  -0.972 0.331160    
    ## age                       -0.014693   0.004581  -3.207 0.001360 ** 
    ## race4                     -0.213379   0.140705  -1.517 0.129542    
    ## race6                     -0.909633   0.346472  -2.625 0.008717 ** 
    ## race2                      0.054641   0.131148   0.417 0.676989    
    ## star_user                 -0.082174   0.071369  -1.151 0.249702    
    ## star_GS                    0.124931   0.055915   2.234 0.025569 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2107 degrees of freedom
    ## Multiple R-squared:  0.08724,    Adjusted R-squared:  0.07424 
    ## F-statistic: 6.713 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9137 -0.6889  0.2844  1.0329  2.5934 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.624013  15.856227   3.823 0.000135 ***
    ## real_extraversion_ct       0.042831   0.023167   1.849 0.064625 .  
    ## real_agreeableness_ct     -0.051199   0.032982  -1.552 0.120728    
    ## real_conscientiousness_ct  0.078950   0.036673   2.153 0.031448 *  
    ## real_emotionstability_ct   0.001398   0.032861   0.043 0.966062    
    ## real_openness_ct           0.145071   0.032708   4.435 9.67e-06 ***
    ## game_extraversion_ct       0.007595   0.025825   0.294 0.768706    
    ## game_agreeableness_ct      0.065515   0.033622   1.949 0.051479 .  
    ## game_conscientiousness_ct  0.010698   0.039557   0.270 0.786843    
    ## game_emotionstability_ct  -0.033363   0.031520  -1.058 0.289959    
    ## game_openness_ct          -0.048664   0.043146  -1.128 0.259500    
    ## tste_10_0_ct              -0.046729   0.045737  -1.022 0.307051    
    ## tste_10_1_ct               0.190743   0.050593   3.770 0.000168 ***
    ## tste_10_2_ct              -0.193424   0.052308  -3.698 0.000223 ***
    ## tste_10_3_ct              -0.172249   0.054196  -3.178 0.001503 ** 
    ## tste_10_4_ct              -0.063009   0.053205  -1.184 0.236438    
    ## tste_10_5_ct              -0.021956   0.053895  -0.407 0.683770    
    ## tste_10_6_ct              -0.138567   0.052146  -2.657 0.007937 ** 
    ## tste_10_7_ct               0.115003   0.045328   2.537 0.011248 *  
    ## tste_10_8_ct               0.022122   0.059668   0.371 0.710861    
    ## tste_10_9_ct               0.129574   0.050460   2.568 0.010302 *  
    ## release                   -0.027512   0.007809  -3.523 0.000435 ***
    ## education                  0.006517   0.026437   0.247 0.805319    
    ## income                     0.011834   0.017093   0.692 0.488818    
    ## race7                     -0.125258   0.136272  -0.919 0.358106    
    ## sex2                      -0.067658   0.072143  -0.938 0.348435    
    ## age                       -0.014691   0.004583  -3.205 0.001369 ** 
    ## race4                     -0.212387   0.140709  -1.509 0.131344    
    ## race6                     -0.922944   0.346355  -2.665 0.007764 ** 
    ## race2                      0.065665   0.131180   0.501 0.616728    
    ## star_user                 -0.059420   0.070593  -0.842 0.400030    
    ## star_GS                    0.116708   0.055938   2.086 0.037063 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2106 degrees of freedom
    ## Multiple R-squared:  0.08777,    Adjusted R-squared:  0.07434 
    ## F-statistic: 6.536 on 31 and 2106 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9219 -0.6937  0.2932  1.0309  2.6491 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.941487  16.075934   3.915 9.32e-05 ***
    ## real_extraversion_ct       0.043066   0.023161   1.859 0.063101 .  
    ## real_agreeableness_ct     -0.051275   0.032976  -1.555 0.120109    
    ## real_conscientiousness_ct  0.079565   0.036664   2.170 0.030108 *  
    ## real_emotionstability_ct   0.001792   0.032862   0.055 0.956528    
    ## real_openness_ct           0.145914   0.032711   4.461 8.60e-06 ***
    ## game_extraversion_ct       0.009088   0.025812   0.352 0.724800    
    ## game_agreeableness_ct      0.066059   0.033595   1.966 0.049393 *  
    ## game_conscientiousness_ct  0.008185   0.039532   0.207 0.835995    
    ## game_emotionstability_ct  -0.034470   0.031510  -1.094 0.274116    
    ## game_openness_ct          -0.052621   0.043172  -1.219 0.223035    
    ## tste_11_0_ct              -0.050133   0.056120  -0.893 0.371792    
    ## tste_11_1_ct               0.055959   0.052397   1.068 0.285654    
    ## tste_11_2_ct              -0.077874   0.046638  -1.670 0.095119 .  
    ## tste_11_3_ct               0.006966   0.056918   0.122 0.902609    
    ## tste_11_4_ct              -0.172736   0.057578  -3.000 0.002731 ** 
    ## tste_11_5_ct              -0.099742   0.050226  -1.986 0.047178 *  
    ## tste_11_6_ct               0.158857   0.049120   3.234 0.001239 ** 
    ## tste_11_7_ct               0.098284   0.056512   1.739 0.082153 .  
    ## tste_11_8_ct              -0.199955   0.042123  -4.747 2.20e-06 ***
    ## tste_11_9_ct               0.172409   0.049916   3.454 0.000563 ***
    ## tste_11_10_ct              0.068560   0.054647   1.255 0.209768    
    ## release                   -0.028719   0.007916  -3.628 0.000292 ***
    ## education                  0.007128   0.026426   0.270 0.787391    
    ## income                     0.012196   0.017097   0.713 0.475715    
    ## race7                     -0.134365   0.136142  -0.987 0.323783    
    ## sex2                      -0.066025   0.072179  -0.915 0.360430    
    ## age                       -0.014605   0.004581  -3.188 0.001452 ** 
    ## race4                     -0.209742   0.140655  -1.491 0.136065    
    ## race6                     -0.912960   0.346227  -2.637 0.008429 ** 
    ## race2                      0.066650   0.131140   0.508 0.611340    
    ## star_user                 -0.084676   0.072057  -1.175 0.240080    
    ## star_GS                    0.153841   0.057390   2.681 0.007406 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.444 on 2105 degrees of freedom
    ## Multiple R-squared:  0.08921,    Adjusted R-squared:  0.07537 
    ## F-statistic: 6.443 on 32 and 2105 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9086 -0.6944  0.2963  1.0371  2.7013 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               50.822038  16.428339   3.094  0.00200 ** 
    ## real_extraversion_ct       0.043747   0.023188   1.887  0.05935 .  
    ## real_agreeableness_ct     -0.051997   0.032991  -1.576  0.11515    
    ## real_conscientiousness_ct  0.079693   0.036690   2.172  0.02996 *  
    ## real_emotionstability_ct   0.003247   0.032879   0.099  0.92135    
    ## real_openness_ct           0.149027   0.032673   4.561 5.38e-06 ***
    ## game_extraversion_ct       0.007419   0.025852   0.287  0.77416    
    ## game_agreeableness_ct      0.067784   0.033608   2.017  0.04383 *  
    ## game_conscientiousness_ct  0.006113   0.039584   0.154  0.87729    
    ## game_emotionstability_ct  -0.035920   0.031513  -1.140  0.25447    
    ## game_openness_ct          -0.053693   0.043182  -1.243  0.21386    
    ## tste_12_0_ct               0.160593   0.060289   2.664  0.00779 ** 
    ## tste_12_1_ct               0.064877   0.050924   1.274  0.20281    
    ## tste_12_2_ct               0.060772   0.048534   1.252  0.21065    
    ## tste_12_3_ct              -0.003484   0.054922  -0.063  0.94942    
    ## tste_12_4_ct              -0.072147   0.051015  -1.414  0.15744    
    ## tste_12_5_ct               0.121470   0.054666   2.222  0.02639 *  
    ## tste_12_6_ct              -0.047863   0.055874  -0.857  0.39175    
    ## tste_12_7_ct               0.051067   0.053320   0.958  0.33830    
    ## tste_12_8_ct               0.165984   0.049117   3.379  0.00074 ***
    ## tste_12_9_ct              -0.051925   0.051334  -1.012  0.31189    
    ## tste_12_10_ct              0.190304   0.045281   4.203 2.75e-05 ***
    ## tste_12_11_ct             -0.199995   0.048376  -4.134 3.70e-05 ***
    ## release                   -0.022970   0.008074  -2.845  0.00448 ** 
    ## education                  0.007316   0.026438   0.277  0.78202    
    ## income                     0.011729   0.017102   0.686  0.49293    
    ## race7                     -0.128728   0.136409  -0.944  0.34544    
    ## sex2                      -0.063286   0.072218  -0.876  0.38096    
    ## age                       -0.014455   0.004588  -3.151  0.00165 ** 
    ## race4                     -0.214996   0.140758  -1.527  0.12681    
    ## race6                     -0.941622   0.346614  -2.717  0.00665 ** 
    ## race2                      0.066749   0.131201   0.509  0.61098    
    ## star_user                 -0.048905   0.072103  -0.678  0.49767    
    ## star_GS                    0.186424   0.060409   3.086  0.00205 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2104 degrees of freedom
    ## Multiple R-squared:  0.08866,    Adjusted R-squared:  0.07437 
    ## F-statistic: 6.203 on 33 and 2104 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1337 -0.6822  0.2961  1.0432  2.7688 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.1714345 16.8341140   2.921 0.003527 ** 
    ## real_extraversion_ct       0.0443479  0.0232125   1.911 0.056203 .  
    ## real_agreeableness_ct     -0.0520429  0.0330309  -1.576 0.115273    
    ## real_conscientiousness_ct  0.0841623  0.0367077   2.293 0.021959 *  
    ## real_emotionstability_ct   0.0026347  0.0329105   0.080 0.936199    
    ## real_openness_ct           0.1471700  0.0327487   4.494 7.37e-06 ***
    ## game_extraversion_ct       0.0038128  0.0258928   0.147 0.882945    
    ## game_agreeableness_ct      0.0725135  0.0336969   2.152 0.031516 *  
    ## game_conscientiousness_ct  0.0081304  0.0396193   0.205 0.837426    
    ## game_emotionstability_ct  -0.0347887  0.0315725  -1.102 0.270646    
    ## game_openness_ct          -0.0506985  0.0432902  -1.171 0.241679    
    ## tste_13_0_ct               0.0063695  0.0504462   0.126 0.899535    
    ## tste_13_1_ct              -0.0905713  0.0466644  -1.941 0.052403 .  
    ## tste_13_2_ct               0.1996184  0.0520767   3.833 0.000130 ***
    ## tste_13_3_ct               0.0529281  0.0502126   1.054 0.291967    
    ## tste_13_4_ct              -0.1099040  0.0530015  -2.074 0.038238 *  
    ## tste_13_5_ct               0.1278732  0.0519411   2.462 0.013900 *  
    ## tste_13_6_ct               0.0005266  0.0558170   0.009 0.992473    
    ## tste_13_7_ct              -0.0741591  0.0507694  -1.461 0.144246    
    ## tste_13_8_ct               0.0815834  0.0466674   1.748 0.080577 .  
    ## tste_13_9_ct               0.1467722  0.0560388   2.619 0.008879 ** 
    ## tste_13_10_ct              0.1723094  0.0485681   3.548 0.000397 ***
    ## tste_13_11_ct              0.1027287  0.0542042   1.895 0.058201 .  
    ## tste_13_12_ct              0.0621334  0.0518303   1.199 0.230747    
    ## release                   -0.0219580  0.0082685  -2.656 0.007976 ** 
    ## education                  0.0095173  0.0264641   0.360 0.719159    
    ## income                     0.0099364  0.0171066   0.581 0.561405    
    ## race7                     -0.1288045  0.1365367  -0.943 0.345600    
    ## sex2                      -0.0739664  0.0722963  -1.023 0.306378    
    ## age                       -0.0144453  0.0045935  -3.145 0.001686 ** 
    ## race4                     -0.2078913  0.1409046  -1.475 0.140253    
    ## race6                     -0.9704451  0.3469226  -2.797 0.005200 ** 
    ## race2                      0.0568785  0.1312531   0.433 0.664805    
    ## star_user                 -0.0882220  0.0715059  -1.234 0.217426    
    ## star_GS                    0.1807914  0.0594425   3.041 0.002383 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.446 on 2103 degrees of freedom
    ## Multiple R-squared:  0.0874, Adjusted R-squared:  0.07264 
    ## F-statistic: 5.923 on 34 and 2103 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1253 -0.6896  0.3052  1.0512  2.7243 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               44.060895  16.757572   2.629 0.008618 ** 
    ## real_extraversion_ct       0.044694   0.023194   1.927 0.054118 .  
    ## real_agreeableness_ct     -0.051013   0.032984  -1.547 0.122117    
    ## real_conscientiousness_ct  0.080920   0.036689   2.206 0.027523 *  
    ## real_emotionstability_ct   0.002707   0.032869   0.082 0.934375    
    ## real_openness_ct           0.146018   0.032716   4.463 8.50e-06 ***
    ## game_extraversion_ct       0.009245   0.025847   0.358 0.720628    
    ## game_agreeableness_ct      0.070431   0.033631   2.094 0.036356 *  
    ## game_conscientiousness_ct  0.007691   0.039584   0.194 0.845966    
    ## game_emotionstability_ct  -0.036275   0.031528  -1.151 0.250037    
    ## game_openness_ct          -0.050287   0.043251  -1.163 0.245091    
    ## tste_14_0_ct              -0.102462   0.051319  -1.997 0.045998 *  
    ## tste_14_1_ct               0.006852   0.051625   0.133 0.894423    
    ## tste_14_2_ct               0.046748   0.047562   0.983 0.325783    
    ## tste_14_3_ct               0.118879   0.046358   2.564 0.010404 *  
    ## tste_14_4_ct               0.019026   0.043382   0.439 0.661022    
    ## tste_14_5_ct              -0.036061   0.053946  -0.668 0.503909    
    ## tste_14_6_ct               0.034488   0.050859   0.678 0.497773    
    ## tste_14_7_ct              -0.260960   0.059322  -4.399 1.14e-05 ***
    ## tste_14_8_ct               0.162188   0.045337   3.577 0.000355 ***
    ## tste_14_9_ct              -0.022410   0.051411  -0.436 0.662959    
    ## tste_14_10_ct              0.106482   0.049305   2.160 0.030911 *  
    ## tste_14_11_ct             -0.057639   0.048992  -1.176 0.239535    
    ## tste_14_12_ct              0.088336   0.051580   1.713 0.086935 .  
    ## tste_14_13_ct              0.112983   0.052137   2.167 0.030345 *  
    ## release                   -0.019569   0.008241  -2.375 0.017658 *  
    ## education                  0.006421   0.026466   0.243 0.808330    
    ## income                     0.010635   0.017090   0.622 0.533810    
    ## race7                     -0.145327   0.136441  -1.065 0.286939    
    ## sex2                      -0.067002   0.072302  -0.927 0.354193    
    ## age                       -0.014026   0.004590  -3.056 0.002274 ** 
    ## race4                     -0.204111   0.140761  -1.450 0.147191    
    ## race6                     -0.970655   0.346593  -2.801 0.005148 ** 
    ## race2                      0.055348   0.131143   0.422 0.673037    
    ## star_user                 -0.041566   0.071884  -0.578 0.563170    
    ## star_GS                    0.170784   0.059546   2.868 0.004170 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2102 degrees of freedom
    ## Multiple R-squared:  0.0896, Adjusted R-squared:  0.07444 
    ## F-statistic: 5.911 on 35 and 2102 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1192 -0.6913  0.2968  1.0291  2.7400 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               50.827963  17.736731   2.866 0.004202 ** 
    ## real_extraversion_ct       0.044078   0.023214   1.899 0.057735 .  
    ## real_agreeableness_ct     -0.053424   0.033024  -1.618 0.105868    
    ## real_conscientiousness_ct  0.083074   0.036735   2.261 0.023832 *  
    ## real_emotionstability_ct   0.004519   0.032923   0.137 0.890826    
    ## real_openness_ct           0.146468   0.032751   4.472 8.16e-06 ***
    ## game_extraversion_ct       0.008729   0.025881   0.337 0.735956    
    ## game_agreeableness_ct      0.070915   0.033675   2.106 0.035333 *  
    ## game_conscientiousness_ct  0.006922   0.039631   0.175 0.861356    
    ## game_emotionstability_ct  -0.034775   0.031568  -1.102 0.270771    
    ## game_openness_ct          -0.048796   0.043297  -1.127 0.259863    
    ## tste_15_0_ct              -0.061874   0.047974  -1.290 0.197280    
    ## tste_15_1_ct              -0.113759   0.053747  -2.117 0.034414 *  
    ## tste_15_2_ct               0.160310   0.048498   3.306 0.000964 ***
    ## tste_15_3_ct               0.090542   0.052477   1.725 0.084611 .  
    ## tste_15_4_ct               0.013799   0.046986   0.294 0.769029    
    ## tste_15_5_ct              -0.073221   0.053664  -1.364 0.172578    
    ## tste_15_6_ct              -0.112600   0.046547  -2.419 0.015645 *  
    ## tste_15_7_ct              -0.063003   0.051615  -1.221 0.222360    
    ## tste_15_8_ct              -0.126097   0.053907  -2.339 0.019421 *  
    ## tste_15_9_ct               0.037529   0.051041   0.735 0.462247    
    ## tste_15_10_ct              0.043808   0.053209   0.823 0.410415    
    ## tste_15_11_ct              0.191794   0.054273   3.534 0.000418 ***
    ## tste_15_12_ct             -0.052990   0.038152  -1.389 0.165005    
    ## tste_15_13_ct              0.030567   0.044399   0.688 0.491240    
    ## tste_15_14_ct              0.053076   0.046895   1.132 0.257844    
    ## release                   -0.022692   0.008712  -2.605 0.009259 ** 
    ## education                  0.006604   0.026489   0.249 0.803136    
    ## income                     0.010527   0.017120   0.615 0.538695    
    ## race7                     -0.134932   0.136514  -0.988 0.323065    
    ## sex2                      -0.070496   0.072371  -0.974 0.330122    
    ## age                       -0.014150   0.004592  -3.081 0.002088 ** 
    ## race4                     -0.203846   0.140899  -1.447 0.148115    
    ## race6                     -0.966364   0.347098  -2.784 0.005415 ** 
    ## race2                      0.048213   0.131273   0.367 0.713455    
    ## star_user                 -0.071929   0.075570  -0.952 0.341299    
    ## star_GS                    0.142508   0.060252   2.365 0.018111 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.446 on 2101 degrees of freedom
    ## Multiple R-squared:  0.08833,    Adjusted R-squared:  0.07271 
    ## F-statistic: 5.655 on 36 and 2101 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9607 -0.6865  0.2778  1.0331  2.7347 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               33.109305  17.715216   1.869 0.061765 .  
    ## real_extraversion_ct       0.049051   0.023146   2.119 0.034191 *  
    ## real_agreeableness_ct     -0.059824   0.032911  -1.818 0.069244 .  
    ## real_conscientiousness_ct  0.080685   0.036581   2.206 0.027517 *  
    ## real_emotionstability_ct   0.010294   0.032798   0.314 0.753660    
    ## real_openness_ct           0.145947   0.032613   4.475 8.04e-06 ***
    ## game_extraversion_ct       0.001943   0.025801   0.075 0.939973    
    ## game_agreeableness_ct      0.077494   0.033556   2.309 0.021019 *  
    ## game_conscientiousness_ct  0.010336   0.039495   0.262 0.793577    
    ## game_emotionstability_ct  -0.035500   0.031444  -1.129 0.259026    
    ## game_openness_ct          -0.047965   0.043112  -1.113 0.266021    
    ## tste_16_0_ct               0.173775   0.048590   3.576 0.000356 ***
    ## tste_16_1_ct              -0.108689   0.049876  -2.179 0.029429 *  
    ## tste_16_2_ct              -0.052652   0.044871  -1.173 0.240769    
    ## tste_16_3_ct              -0.164193   0.050456  -3.254 0.001155 ** 
    ## tste_16_4_ct              -0.169501   0.041178  -4.116 4.00e-05 ***
    ## tste_16_5_ct               0.103543   0.048203   2.148 0.031822 *  
    ## tste_16_6_ct               0.102931   0.044058   2.336 0.019571 *  
    ## tste_16_7_ct               0.077639   0.044572   1.742 0.081679 .  
    ## tste_16_8_ct              -0.064721   0.051943  -1.246 0.212901    
    ## tste_16_9_ct               0.156881   0.044773   3.504 0.000468 ***
    ## tste_16_10_ct             -0.079219   0.043979  -1.801 0.071798 .  
    ## tste_16_11_ct              0.155166   0.050250   3.088 0.002042 ** 
    ## tste_16_12_ct              0.080063   0.046114   1.736 0.082674 .  
    ## tste_16_13_ct              0.051523   0.049663   1.037 0.299643    
    ## tste_16_14_ct              0.112753   0.046233   2.439 0.014817 *  
    ## tste_16_15_ct             -0.016466   0.050127  -0.328 0.742574    
    ## release                   -0.013941   0.008704  -1.602 0.109394    
    ## education                  0.005329   0.026372   0.202 0.839870    
    ## income                     0.014728   0.017076   0.863 0.388509    
    ## race7                     -0.128975   0.136026  -0.948 0.343155    
    ## sex2                      -0.072250   0.071999  -1.003 0.315736    
    ## age                       -0.012885   0.004584  -2.811 0.004987 ** 
    ## race4                     -0.198294   0.140374  -1.413 0.157919    
    ## race6                     -0.953106   0.345824  -2.756 0.005901 ** 
    ## race2                      0.071708   0.130884   0.548 0.583837    
    ## star_user                 -0.067944   0.072318  -0.940 0.347580    
    ## star_GS                    0.149716   0.058465   2.561 0.010513 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.44 on 2100 degrees of freedom
    ## Multiple R-squared:  0.09629,    Adjusted R-squared:  0.08037 
    ## F-statistic: 6.047 on 37 and 2100 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1805 -0.7073  0.2756  1.0294  2.7858 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               35.373708  18.260081   1.937  0.05285 .  
    ## real_extraversion_ct       0.047759   0.023120   2.066  0.03898 *  
    ## real_agreeableness_ct     -0.053847   0.032902  -1.637  0.10186    
    ## real_conscientiousness_ct  0.081973   0.036555   2.242  0.02504 *  
    ## real_emotionstability_ct   0.006127   0.032758   0.187  0.85164    
    ## real_openness_ct           0.143561   0.032587   4.405 1.11e-05 ***
    ## game_extraversion_ct       0.002181   0.025775   0.085  0.93259    
    ## game_agreeableness_ct      0.075114   0.033553   2.239  0.02528 *  
    ## game_conscientiousness_ct  0.008786   0.039463   0.223  0.82383    
    ## game_emotionstability_ct  -0.031703   0.031429  -1.009  0.31323    
    ## game_openness_ct          -0.043694   0.043056  -1.015  0.31031    
    ## tste_17_0_ct               0.316951   0.048924   6.478 1.15e-10 ***
    ## tste_17_1_ct               0.063144   0.041911   1.507  0.13206    
    ## tste_17_2_ct              -0.070389   0.049905  -1.410  0.15855    
    ## tste_17_3_ct               0.044379   0.047739   0.930  0.35267    
    ## tste_17_4_ct               0.090081   0.050096   1.798  0.07230 .  
    ## tste_17_5_ct               0.021695   0.046765   0.464  0.64275    
    ## tste_17_6_ct              -0.047350   0.049214  -0.962  0.33610    
    ## tste_17_7_ct               0.091818   0.043102   2.130  0.03327 *  
    ## tste_17_8_ct              -0.068096   0.047881  -1.422  0.15511    
    ## tste_17_9_ct              -0.106377   0.046169  -2.304  0.02132 *  
    ## tste_17_10_ct              0.096132   0.042447   2.265  0.02363 *  
    ## tste_17_11_ct              0.040110   0.046805   0.857  0.39156    
    ## tste_17_12_ct             -0.168752   0.042783  -3.944 8.26e-05 ***
    ## tste_17_13_ct              0.092888   0.049122   1.891  0.05877 .  
    ## tste_17_14_ct              0.031921   0.043335   0.737  0.46144    
    ## tste_17_15_ct             -0.034903   0.055915  -0.624  0.53255    
    ## tste_17_16_ct             -0.051511   0.042824  -1.203  0.22917    
    ## release                   -0.015132   0.008947  -1.691  0.09094 .  
    ## education                  0.007010   0.026346   0.266  0.79021    
    ## income                     0.014994   0.017052   0.879  0.37933    
    ## race7                     -0.119909   0.135915  -0.882  0.37775    
    ## sex2                      -0.076491   0.071913  -1.064  0.28761    
    ## age                       -0.012971   0.004585  -2.829  0.00471 ** 
    ## race4                     -0.208174   0.140279  -1.484  0.13796    
    ## race6                     -0.989690   0.345875  -2.861  0.00426 ** 
    ## race2                      0.059991   0.130761   0.459  0.64644    
    ## star_user                 -0.078127   0.076618  -1.020  0.30799    
    ## star_GS                    0.174706   0.060234   2.900  0.00376 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.439 on 2099 degrees of freedom
    ## Multiple R-squared:  0.09862,    Adjusted R-squared:  0.0823 
    ## F-statistic: 6.043 on 38 and 2099 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1045 -0.6943  0.2872  1.0417  2.6348 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               45.233179  18.047901   2.506  0.01228 *  
    ## real_extraversion_ct       0.044276   0.023109   1.916  0.05551 .  
    ## real_agreeableness_ct     -0.052825   0.032921  -1.605  0.10873    
    ## real_conscientiousness_ct  0.083581   0.036593   2.284  0.02247 *  
    ## real_emotionstability_ct   0.003706   0.032780   0.113  0.90999    
    ## real_openness_ct           0.143199   0.032606   4.392 1.18e-05 ***
    ## game_extraversion_ct       0.005082   0.025792   0.197  0.84380    
    ## game_agreeableness_ct      0.074264   0.033545   2.214  0.02694 *  
    ## game_conscientiousness_ct  0.003982   0.039474   0.101  0.91966    
    ## game_emotionstability_ct  -0.033003   0.031441  -1.050  0.29398    
    ## game_openness_ct          -0.044212   0.043086  -1.026  0.30495    
    ## tste_18_0_ct              -0.130146   0.046118  -2.822  0.00482 ** 
    ## tste_18_1_ct               0.155629   0.047999   3.242  0.00120 ** 
    ## tste_18_2_ct              -0.013942   0.043554  -0.320  0.74891    
    ## tste_18_3_ct              -0.018174   0.046785  -0.388  0.69772    
    ## tste_18_4_ct               0.073341   0.051063   1.436  0.15107    
    ## tste_18_5_ct              -0.029778   0.047987  -0.621  0.53497    
    ## tste_18_6_ct              -0.002003   0.050286  -0.040  0.96822    
    ## tste_18_7_ct              -0.055506   0.043192  -1.285  0.19890    
    ## tste_18_8_ct               0.079319   0.046976   1.689  0.09146 .  
    ## tste_18_9_ct              -0.132426   0.047753  -2.773  0.00560 ** 
    ## tste_18_10_ct              0.084994   0.045833   1.854  0.06382 .  
    ## tste_18_11_ct              0.062703   0.045068   1.391  0.16429    
    ## tste_18_12_ct             -0.087473   0.047516  -1.841  0.06578 .  
    ## tste_18_13_ct             -0.032165   0.046715  -0.689  0.49119    
    ## tste_18_14_ct              0.052466   0.050774   1.033  0.30158    
    ## tste_18_15_ct              0.172032   0.058513   2.940  0.00332 ** 
    ## tste_18_16_ct             -0.295931   0.051332  -5.765 9.37e-09 ***
    ## tste_18_17_ct              0.045402   0.045628   0.995  0.31984    
    ## release                   -0.019675   0.008859  -2.221  0.02647 *  
    ## education                  0.006798   0.026346   0.258  0.79641    
    ## income                     0.014296   0.017056   0.838  0.40202    
    ## race7                     -0.132886   0.135877  -0.978  0.32820    
    ## sex2                      -0.069386   0.071983  -0.964  0.33519    
    ## age                       -0.012935   0.004586  -2.820  0.00484 ** 
    ## race4                     -0.205215   0.140277  -1.463  0.14364    
    ## race6                     -1.002294   0.345816  -2.898  0.00379 ** 
    ## race2                      0.053304   0.130828   0.407  0.68373    
    ## star_user                 -0.082669   0.077028  -1.073  0.28329    
    ## star_GS                    0.091117   0.060999   1.494  0.13539    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.439 on 2098 degrees of freedom
    ## Multiple R-squared:  0.09892,    Adjusted R-squared:  0.08217 
    ## F-statistic: 5.906 on 39 and 2098 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1108 -0.6772  0.2864  1.0491  2.6647 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               37.304389  18.492129   2.017  0.04379 *  
    ## real_extraversion_ct       0.047695   0.023183   2.057  0.03978 *  
    ## real_agreeableness_ct     -0.052752   0.032986  -1.599  0.10992    
    ## real_conscientiousness_ct  0.082954   0.036678   2.262  0.02382 *  
    ## real_emotionstability_ct   0.006753   0.032886   0.205  0.83731    
    ## real_openness_ct           0.143830   0.032676   4.402 1.13e-05 ***
    ## game_extraversion_ct       0.002936   0.025863   0.114  0.90962    
    ## game_agreeableness_ct      0.074288   0.033632   2.209  0.02729 *  
    ## game_conscientiousness_ct  0.007560   0.039574   0.191  0.84851    
    ## game_emotionstability_ct  -0.031254   0.031515  -0.992  0.32145    
    ## game_openness_ct          -0.040494   0.043173  -0.938  0.34839    
    ## tste_19_0_ct               0.010816   0.048289   0.224  0.82279    
    ## tste_19_1_ct               0.068520   0.050287   1.363  0.17316    
    ## tste_19_2_ct               0.025565   0.048904   0.523  0.60120    
    ## tste_19_3_ct              -0.116200   0.046412  -2.504  0.01237 *  
    ## tste_19_4_ct              -0.106807   0.049024  -2.179  0.02947 *  
    ## tste_19_5_ct              -0.030635   0.053525  -0.572  0.56715    
    ## tste_19_6_ct               0.083050   0.047719   1.740  0.08194 .  
    ## tste_19_7_ct               0.113901   0.046832   2.432  0.01509 *  
    ## tste_19_8_ct               0.040939   0.046807   0.875  0.38187    
    ## tste_19_9_ct              -0.070348   0.050170  -1.402  0.16101    
    ## tste_19_10_ct              0.131216   0.047819   2.744  0.00612 ** 
    ## tste_19_11_ct              0.091978   0.044828   2.052  0.04031 *  
    ## tste_19_12_ct              0.231121   0.045572   5.072 4.29e-07 ***
    ## tste_19_13_ct             -0.010428   0.043211  -0.241  0.80933    
    ## tste_19_14_ct              0.175388   0.044928   3.904 9.77e-05 ***
    ## tste_19_15_ct              0.006693   0.044116   0.152  0.87943    
    ## tste_19_16_ct              0.027971   0.046812   0.598  0.55022    
    ## tste_19_17_ct              0.053234   0.052956   1.005  0.31489    
    ## tste_19_18_ct              0.004275   0.040985   0.104  0.91693    
    ## release                   -0.016047   0.009079  -1.768  0.07727 .  
    ## education                  0.006975   0.026410   0.264  0.79173    
    ## income                     0.015316   0.017095   0.896  0.37040    
    ## race7                     -0.125059   0.136228  -0.918  0.35872    
    ## sex2                      -0.072508   0.072179  -1.005  0.31522    
    ## age                       -0.012833   0.004598  -2.791  0.00530 ** 
    ## race4                     -0.211069   0.140579  -1.501  0.13340    
    ## race6                     -0.986980   0.346773  -2.846  0.00447 ** 
    ## race2                      0.057461   0.131184   0.438  0.66142    
    ## star_user                 -0.031575   0.074591  -0.423  0.67211    
    ## star_GS                    0.116072   0.060374   1.923  0.05467 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2097 degrees of freedom
    ## Multiple R-squared:  0.09539,    Adjusted R-squared:  0.07814 
    ## F-statistic: 5.528 on 40 and 2097 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0698 -0.6984  0.2772  1.0156  2.7446 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               34.710249  19.307555   1.798 0.072360 .  
    ## real_extraversion_ct       0.047514   0.023120   2.055 0.039994 *  
    ## real_agreeableness_ct     -0.052319   0.032911  -1.590 0.112044    
    ## real_conscientiousness_ct  0.084628   0.036594   2.313 0.020841 *  
    ## real_emotionstability_ct   0.007466   0.032796   0.228 0.819945    
    ## real_openness_ct           0.141283   0.032609   4.333 1.54e-05 ***
    ## game_extraversion_ct       0.004408   0.025777   0.171 0.864237    
    ## game_agreeableness_ct      0.072881   0.033558   2.172 0.029983 *  
    ## game_conscientiousness_ct  0.004667   0.039490   0.118 0.905937    
    ## game_emotionstability_ct  -0.029992   0.031445  -0.954 0.340309    
    ## game_openness_ct          -0.038565   0.043076  -0.895 0.370733    
    ## tste_20_0_ct               0.085296   0.053558   1.593 0.111405    
    ## tste_20_1_ct               0.086635   0.046498   1.863 0.062572 .  
    ## tste_20_2_ct               0.093962   0.042397   2.216 0.026783 *  
    ## tste_20_3_ct               0.150814   0.048672   3.099 0.001970 ** 
    ## tste_20_4_ct               0.033503   0.044300   0.756 0.449564    
    ## tste_20_5_ct              -0.070384   0.043774  -1.608 0.108009    
    ## tste_20_6_ct               0.025510   0.049492   0.515 0.606304    
    ## tste_20_7_ct               0.188309   0.054082   3.482 0.000508 ***
    ## tste_20_8_ct              -0.022117   0.053708  -0.412 0.680525    
    ## tste_20_9_ct               0.102986   0.044162   2.332 0.019796 *  
    ## tste_20_10_ct              0.060691   0.047988   1.265 0.206111    
    ## tste_20_11_ct             -0.124444   0.045609  -2.729 0.006415 ** 
    ## tste_20_12_ct              0.128701   0.046063   2.794 0.005253 ** 
    ## tste_20_13_ct              0.111348   0.050270   2.215 0.026868 *  
    ## tste_20_14_ct             -0.100193   0.041046  -2.441 0.014730 *  
    ## tste_20_15_ct              0.118788   0.053258   2.230 0.025824 *  
    ## tste_20_16_ct             -0.092216   0.046169  -1.997 0.045918 *  
    ## tste_20_17_ct              0.031860   0.044933   0.709 0.478373    
    ## tste_20_18_ct              0.138438   0.042567   3.252 0.001163 ** 
    ## tste_20_19_ct             -0.035116   0.048603  -0.723 0.470061    
    ## release                   -0.014674   0.009479  -1.548 0.121763    
    ## education                  0.007715   0.026352   0.293 0.769741    
    ## income                     0.014477   0.017057   0.849 0.396139    
    ## race7                     -0.126219   0.135944  -0.928 0.353275    
    ## sex2                      -0.065281   0.072059  -0.906 0.365069    
    ## age                       -0.012590   0.004588  -2.744 0.006125 ** 
    ## race4                     -0.194821   0.140333  -1.388 0.165199    
    ## race6                     -0.985284   0.346145  -2.846 0.004464 ** 
    ## race2                      0.074618   0.130779   0.571 0.568356    
    ## star_user                 -0.013750   0.077922  -0.176 0.859952    
    ## star_GS                    0.077458   0.061285   1.264 0.206408    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.439 on 2096 degrees of freedom
    ## Multiple R-squared:  0.09991,    Adjusted R-squared:  0.0823 
    ## F-statistic: 5.675 on 41 and 2096 DF,  p-value: < 2.2e-16

### preference ~ tste + real + game + game\*tste

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 58:76)$model_lm_1) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9931 -0.6541  0.3070  1.0691  2.5972 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.6522646 14.1921015   4.556 5.52e-06 ***
    ## real_extraversion_ct       0.0411285  0.0233985   1.758  0.07894 .  
    ## real_agreeableness_ct     -0.0542494  0.0333147  -1.628  0.10359    
    ## real_conscientiousness_ct  0.0830848  0.0370427   2.243  0.02500 *  
    ## real_emotionstability_ct   0.0001318  0.0331876   0.004  0.99683    
    ## real_openness_ct           0.1427409  0.0330037   4.325 1.60e-05 ***
    ## game_extraversion_ct       0.0062347  0.0260999   0.239  0.81122    
    ## game_agreeableness_ct      0.0691362  0.0339170   2.038  0.04163 *  
    ## game_conscientiousness_ct  0.0043454  0.0399756   0.109  0.91345    
    ## game_emotionstability_ct  -0.0360643  0.0318305  -1.133  0.25734    
    ## game_openness_ct          -0.0376940  0.0435241  -0.866  0.38656    
    ## tste_2_0_ct                0.0959709  0.0413051   2.323  0.02025 *  
    ## tste_2_1_ct               -0.0743790  0.0402641  -1.847  0.06485 .  
    ## release                   -0.0293509  0.0069808  -4.205 2.73e-05 ***
    ## education                  0.0104698  0.0267227   0.392  0.69525    
    ## income                     0.0079546  0.0172654   0.461  0.64504    
    ## race7                     -0.1433373  0.1375388  -1.042  0.29746    
    ## sex2                      -0.0808576  0.0728770  -1.110  0.26734    
    ## age                       -0.0150509  0.0046288  -3.252  0.00117 ** 
    ## race4                     -0.2541625  0.1421136  -1.788  0.07385 .  
    ## race6                     -0.9691379  0.3497740  -2.771  0.00564 ** 
    ## race2                      0.0803499  0.1325024   0.606  0.54431    
    ## star_user                  0.0092683  0.0579591   0.160  0.87297    
    ## star_GS                    0.0099173  0.0480447   0.206  0.83648    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2114 degrees of freedom
    ## Multiple R-squared:  0.06308,    Adjusted R-squared:  0.05289 
    ## F-statistic: 6.188 on 23 and 2114 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8730 -0.6937  0.3037  1.0694  2.4998 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.869158  14.145624   4.162 3.29e-05 ***
    ## real_extraversion_ct       0.039770   0.023248   1.711 0.087288 .  
    ## real_agreeableness_ct     -0.057920   0.033110  -1.749 0.080381 .  
    ## real_conscientiousness_ct  0.082458   0.036807   2.240 0.025178 *  
    ## real_emotionstability_ct   0.002550   0.032975   0.077 0.938378    
    ## real_openness_ct           0.146612   0.032800   4.470 8.24e-06 ***
    ## game_extraversion_ct       0.005873   0.025930   0.226 0.820838    
    ## game_agreeableness_ct      0.074517   0.033715   2.210 0.027200 *  
    ## game_conscientiousness_ct  0.006334   0.039723   0.159 0.873317    
    ## game_emotionstability_ct  -0.036509   0.031630  -1.154 0.248526    
    ## game_openness_ct          -0.041551   0.043253  -0.961 0.336846    
    ## tste_3_0_ct               -0.043515   0.043427  -1.002 0.316449    
    ## tste_3_1_ct                0.154360   0.035306   4.372 1.29e-05 ***
    ## tste_3_2_ct                0.243954   0.049356   4.943 8.31e-07 ***
    ## release                   -0.026611   0.006957  -3.825 0.000135 ***
    ## education                  0.007901   0.026557   0.298 0.766108    
    ## income                     0.009312   0.017157   0.543 0.587360    
    ## age                       -0.014890   0.004599  -3.237 0.001225 ** 
    ## sex2                      -0.069351   0.072438  -0.957 0.338482    
    ## race4                     -0.228635   0.141294  -1.618 0.105779    
    ## race6                     -0.946277   0.347599  -2.722 0.006536 ** 
    ## race7                     -0.158922   0.136694  -1.163 0.245119    
    ## race2                      0.075986   0.131663   0.577 0.563916    
    ## star_user                  0.009049   0.057709   0.157 0.875411    
    ## star_GS                    0.043285   0.048476   0.893 0.372002    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.452 on 2113 degrees of freedom
    ## Multiple R-squared:  0.07541,    Adjusted R-squared:  0.06491 
    ## F-statistic: 7.181 on 24 and 2113 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9411 -0.6906  0.3017  1.0478  2.5787 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          63.108267  14.493953   4.354 1.40e-05 ***
    ## real_extraversion_ct                  0.039922   0.023242   1.718  0.08601 .  
    ## real_agreeableness_ct                -0.055957   0.033107  -1.690  0.09114 .  
    ## real_conscientiousness_ct             0.084072   0.036819   2.283  0.02251 *  
    ## real_emotionstability_ct              0.002887   0.032964   0.088  0.93023    
    ## real_openness_ct                      0.146767   0.032791   4.476 8.02e-06 ***
    ## game_extraversion_ct                  0.005646   0.025918   0.218  0.82757    
    ## game_agreeableness_ct                 0.073874   0.033691   2.193  0.02844 *  
    ## game_conscientiousness_ct             0.004485   0.039729   0.113  0.91012    
    ## game_emotionstability_ct             -0.035177   0.031698  -1.110  0.26722    
    ## game_openness_ct                     -0.040319   0.043276  -0.932  0.35162    
    ## tste_4_0_ct                           0.257951   0.049966   5.163 2.66e-07 ***
    ## tste_4_1_ct                           0.049133   0.056525   0.869  0.38483    
    ## tste_4_2_ct                           0.060985   0.041550   1.468  0.14233    
    ## tste_4_3_ct                          -0.164294   0.039340  -4.176 3.08e-05 ***
    ## release                              -0.028660   0.007125  -4.022 5.97e-05 ***
    ## education                             0.007870   0.026546   0.296  0.76690    
    ## income                                0.008494   0.017158   0.495  0.62059    
    ## age                                  -0.014672   0.004600  -3.189  0.00145 ** 
    ## sex2                                 -0.067611   0.072435  -0.933  0.35072    
    ## race4                                -0.227740   0.141232  -1.613  0.10700    
    ## race6                                -0.948439   0.347391  -2.730  0.00638 ** 
    ## race7                                -0.156743   0.136633  -1.147  0.25143    
    ## game_emotionstability_ct.tste_4_1_ct  0.019560   0.036905   0.530  0.59615    
    ## race2                                 0.078454   0.131840   0.595  0.55186    
    ## star_GS                               0.075996   0.052677   1.443  0.14926    
    ## star_user                            -0.038888   0.064696  -0.601  0.54784    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.451 on 2111 degrees of freedom
    ## Multiple R-squared:  0.07746,    Adjusted R-squared:  0.0661 
    ## F-statistic: 6.817 on 26 and 2111 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8804 -0.6853  0.3016  1.0556  2.5585 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          51.653666  14.787628   3.493 0.000487 ***
    ## real_extraversion_ct                  0.041433   0.023282   1.780 0.075285 .  
    ## real_agreeableness_ct                -0.056073   0.033145  -1.692 0.090841 .  
    ## real_conscientiousness_ct             0.082726   0.036869   2.244 0.024949 *  
    ## real_emotionstability_ct              0.002161   0.033012   0.065 0.947808    
    ## real_openness_ct                      0.146224   0.032830   4.454 8.87e-06 ***
    ## game_extraversion_ct                  0.006265   0.025961   0.241 0.809334    
    ## game_agreeableness_ct                 0.073309   0.033731   2.173 0.029863 *  
    ## game_conscientiousness_ct             0.007053   0.039779   0.177 0.859291    
    ## game_emotionstability_ct             -0.036568   0.031721  -1.153 0.249122    
    ## game_openness_ct                     -0.043625   0.043381  -1.006 0.314709    
    ## tste_5_0_ct                           0.232804   0.046990   4.954 7.84e-07 ***
    ## tste_5_1_ct                          -0.159040   0.052007  -3.058 0.002256 ** 
    ## tste_5_2_ct                          -0.029366   0.040830  -0.719 0.472075    
    ## tste_5_3_ct                          -0.087460   0.050674  -1.726 0.084503 .  
    ## tste_5_4_ct                           0.057761   0.052794   1.094 0.274043    
    ## release                              -0.022946   0.007270  -3.156 0.001620 ** 
    ## education                             0.007832   0.026588   0.295 0.768339    
    ## income                                0.010075   0.017177   0.587 0.557571    
    ## age                                  -0.014863   0.004607  -3.226 0.001275 ** 
    ## sex2                                 -0.072993   0.072560  -1.006 0.314549    
    ## race4                                -0.231484   0.141421  -1.637 0.101811    
    ## race6                                -0.956192   0.347916  -2.748 0.006041 ** 
    ## race7                                -0.155102   0.136816  -1.134 0.257070    
    ## game_emotionstability_ct.tste_5_3_ct  0.009799   0.034331   0.285 0.775337    
    ## race2                                 0.064019   0.131856   0.486 0.627356    
    ## star_user                            -0.024660   0.067202  -0.367 0.713687    
    ## star_GS                               0.059906   0.053610   1.117 0.263931    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2110 degrees of freedom
    ## Multiple R-squared:  0.07542,    Adjusted R-squared:  0.06359 
    ## F-statistic: 6.374 on 27 and 2110 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9748 -0.6907  0.3052  1.0498  2.5542 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          50.523959  14.935278   3.383  0.00073 ***
    ## real_extraversion_ct                  0.041489   0.023275   1.783  0.07480 .  
    ## real_agreeableness_ct                -0.056484   0.033144  -1.704  0.08849 .  
    ## real_conscientiousness_ct             0.080521   0.036870   2.184  0.02908 *  
    ## real_emotionstability_ct              0.002912   0.033000   0.088  0.92968    
    ## real_openness_ct                      0.148640   0.032832   4.527 6.31e-06 ***
    ## game_extraversion_ct                  0.007877   0.025968   0.303  0.76165    
    ## game_agreeableness_ct                 0.070967   0.033760   2.102  0.03566 *  
    ## game_conscientiousness_ct             0.009121   0.039799   0.229  0.81875    
    ## game_emotionstability_ct             -0.038086   0.031740  -1.200  0.23029    
    ## game_openness_ct                     -0.045822   0.043372  -1.057  0.29086    
    ## tste_6_0_ct                           0.055600   0.052682   1.055  0.29136    
    ## tste_6_1_ct                          -0.110182   0.050513  -2.181  0.02927 *  
    ## tste_6_2_ct                           0.246419   0.043471   5.669 1.64e-08 ***
    ## tste_6_3_ct                           0.008180   0.053082   0.154  0.87754    
    ## tste_6_4_ct                           0.105198   0.047590   2.211  0.02718 *  
    ## tste_6_5_ct                           0.086341   0.055733   1.549  0.12148    
    ## release                              -0.022329   0.007343  -3.041  0.00239 ** 
    ## education                             0.007179   0.026582   0.270  0.78714    
    ## income                                0.011028   0.017189   0.642  0.52122    
    ## age                                  -0.014827   0.004607  -3.219  0.00131 ** 
    ## sex2                                 -0.068321   0.072570  -0.941  0.34658    
    ## race4                                -0.226319   0.141420  -1.600  0.10967    
    ## race6                                -0.961001   0.347941  -2.762  0.00580 ** 
    ## race7                                -0.151046   0.136851  -1.104  0.26984    
    ## game_emotionstability_ct.tste_6_1_ct  0.035811   0.034107   1.050  0.29386    
    ## race2                                 0.050018   0.131971   0.379  0.70472    
    ## star_user                            -0.044954   0.069604  -0.646  0.51845    
    ## star_GS                               0.066481   0.054609   1.217  0.22359    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2109 degrees of freedom
    ## Multiple R-squared:  0.07594,    Adjusted R-squared:  0.06367 
    ## F-statistic:  6.19 on 28 and 2109 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8814 -0.6935  0.2868  1.0562  2.5640 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               53.455036  15.214662   3.513 0.000452 ***
    ## real_extraversion_ct       0.041337   0.023250   1.778 0.075563 .  
    ## real_agreeableness_ct     -0.055114   0.033098  -1.665 0.096032 .  
    ## real_conscientiousness_ct  0.081908   0.036807   2.225 0.026162 *  
    ## real_emotionstability_ct   0.001457   0.032985   0.044 0.964766    
    ## real_openness_ct           0.145678   0.032817   4.439 9.50e-06 ***
    ## game_extraversion_ct       0.005631   0.025932   0.217 0.828104    
    ## game_agreeableness_ct      0.074457   0.033690   2.210 0.027207 *  
    ## game_conscientiousness_ct  0.007171   0.039707   0.181 0.856703    
    ## game_emotionstability_ct  -0.035216   0.031619  -1.114 0.265506    
    ## game_openness_ct          -0.042446   0.043322  -0.980 0.327305    
    ## tste_7_0_ct               -0.180085   0.048030  -3.749 0.000182 ***
    ## tste_7_1_ct                0.025317   0.051673   0.490 0.624217    
    ## tste_7_2_ct               -0.160095   0.051109  -3.132 0.001758 ** 
    ## tste_7_3_ct               -0.206448   0.038886  -5.309 1.22e-07 ***
    ## tste_7_4_ct                0.003771   0.052716   0.072 0.942987    
    ## tste_7_5_ct                0.022925   0.056242   0.408 0.683596    
    ## tste_7_6_ct                0.040807   0.059877   0.682 0.495618    
    ## release                   -0.023870   0.007489  -3.187 0.001456 ** 
    ## education                  0.008330   0.026550   0.314 0.753733    
    ## income                     0.009882   0.017155   0.576 0.564649    
    ## age                       -0.014537   0.004601  -3.159 0.001604 ** 
    ## sex2                      -0.074612   0.072401  -1.031 0.302873    
    ## race4                     -0.221892   0.141259  -1.571 0.116374    
    ## race6                     -0.949254   0.347379  -2.733 0.006336 ** 
    ## race7                     -0.147010   0.136639  -1.076 0.282096    
    ## race2                      0.069570   0.131669   0.528 0.597297    
    ## star_user                 -0.024539   0.070349  -0.349 0.727263    
    ## star_GS                    0.064669   0.052890   1.223 0.221578    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.451 on 2109 degrees of freedom
    ## Multiple R-squared:  0.07867,    Adjusted R-squared:  0.06644 
    ## F-statistic: 6.431 on 28 and 2109 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0354 -0.6891  0.2932  1.0305  2.7176 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          59.346198  15.920686   3.728 0.000198 ***
    ## real_extraversion_ct                  0.040995   0.023214   1.766 0.077552 .  
    ## real_agreeableness_ct                -0.051298   0.033054  -1.552 0.120824    
    ## real_conscientiousness_ct             0.082088   0.036733   2.235 0.025542 *  
    ## real_emotionstability_ct              0.001514   0.032927   0.046 0.963325    
    ## real_openness_ct                      0.150462   0.032735   4.596 4.55e-06 ***
    ## game_extraversion_ct                  0.006944   0.025881   0.268 0.788500    
    ## game_agreeableness_ct                 0.070846   0.033627   2.107 0.035250 *  
    ## game_conscientiousness_ct             0.007709   0.039615   0.195 0.845721    
    ## game_emotionstability_ct             -0.038392   0.031577  -1.216 0.224192    
    ## game_openness_ct                     -0.050211   0.043209  -1.162 0.245354    
    ## tste_8_0_ct                          -0.196840   0.050792  -3.875 0.000110 ***
    ## tste_8_1_ct                           0.209188   0.057456   3.641 0.000278 ***
    ## tste_8_2_ct                           0.101513   0.050813   1.998 0.045870 *  
    ## tste_8_3_ct                           0.081685   0.047527   1.719 0.085818 .  
    ## tste_8_4_ct                           0.134446   0.052298   2.571 0.010215 *  
    ## tste_8_5_ct                           0.090698   0.049383   1.837 0.066406 .  
    ## tste_8_6_ct                          -0.118055   0.052929  -2.230 0.025823 *  
    ## tste_8_7_ct                          -0.049659   0.051890  -0.957 0.338682    
    ## release                              -0.026674   0.007836  -3.404 0.000677 ***
    ## education                             0.008185   0.026491   0.309 0.757380    
    ## income                                0.010741   0.017141   0.627 0.530972    
    ## age                                  -0.014457   0.004591  -3.149 0.001663 ** 
    ## sex2                                 -0.075022   0.072215  -1.039 0.298988    
    ## race4                                -0.212992   0.140992  -1.511 0.131023    
    ## race6                                -0.969748   0.346811  -2.796 0.005218 ** 
    ## race7                                -0.131985   0.136425  -0.967 0.333430    
    ## game_emotionstability_ct.tste_8_7_ct -0.037883   0.032719  -1.158 0.247061    
    ## race2                                 0.054618   0.131385   0.416 0.677664    
    ## star_user                            -0.083710   0.070732  -1.183 0.236753    
    ## star_GS                               0.092116   0.055043   1.674 0.094372 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 2107 degrees of freedom
    ## Multiple R-squared:  0.08372,    Adjusted R-squared:  0.07067 
    ## F-statistic: 6.417 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9361 -0.6883  0.2845  1.0552  2.8493 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.554270  16.053224   3.772 0.000166 ***
    ## real_extraversion_ct       0.044168   0.023171   1.906 0.056768 .  
    ## real_agreeableness_ct     -0.054153   0.032985  -1.642 0.100796    
    ## real_conscientiousness_ct  0.079596   0.036666   2.171 0.030056 *  
    ## real_emotionstability_ct   0.002353   0.032863   0.072 0.942925    
    ## real_openness_ct           0.145140   0.032704   4.438 9.55e-06 ***
    ## game_extraversion_ct       0.007393   0.025824   0.286 0.774683    
    ## game_agreeableness_ct      0.067051   0.033594   1.996 0.046076 *  
    ## game_conscientiousness_ct  0.010643   0.039557   0.269 0.787920    
    ## game_emotionstability_ct  -0.032859   0.031517  -1.043 0.297263    
    ## game_openness_ct          -0.049916   0.043163  -1.156 0.247629    
    ## tste_9_0_ct               -0.158720   0.053309  -2.977 0.002940 ** 
    ## tste_9_1_ct                0.039544   0.050697   0.780 0.435475    
    ## tste_9_2_ct               -0.348932   0.052307  -6.671 3.24e-11 ***
    ## tste_9_3_ct               -0.050250   0.057616  -0.872 0.383222    
    ## tste_9_4_ct               -0.016538   0.043787  -0.378 0.705702    
    ## tste_9_5_ct                0.018766   0.057524   0.326 0.744285    
    ## tste_9_6_ct               -0.014905   0.051346  -0.290 0.771628    
    ## tste_9_7_ct               -0.067870   0.052883  -1.283 0.199493    
    ## tste_9_8_ct                0.082536   0.046964   1.757 0.078992 .  
    ## release                   -0.027420   0.007892  -3.474 0.000522 ***
    ## education                  0.006517   0.026437   0.247 0.805310    
    ## income                     0.013458   0.017115   0.786 0.431768    
    ## age                       -0.014693   0.004581  -3.207 0.001360 ** 
    ## sex2                      -0.070082   0.072101  -0.972 0.331160    
    ## race4                     -0.213379   0.140705  -1.517 0.129542    
    ## race6                     -0.909633   0.346472  -2.625 0.008717 ** 
    ## race7                     -0.123406   0.136250  -0.906 0.365180    
    ## race2                      0.054641   0.131148   0.417 0.676989    
    ## star_user                 -0.082174   0.071369  -1.151 0.249702    
    ## star_GS                    0.124931   0.055915   2.234 0.025569 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2107 degrees of freedom
    ## Multiple R-squared:  0.08724,    Adjusted R-squared:  0.07424 
    ## F-statistic: 6.713 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9137 -0.6889  0.2844  1.0329  2.5934 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.624013  15.856227   3.823 0.000135 ***
    ## real_extraversion_ct       0.042831   0.023167   1.849 0.064625 .  
    ## real_agreeableness_ct     -0.051199   0.032982  -1.552 0.120728    
    ## real_conscientiousness_ct  0.078950   0.036673   2.153 0.031448 *  
    ## real_emotionstability_ct   0.001398   0.032861   0.043 0.966062    
    ## real_openness_ct           0.145071   0.032708   4.435 9.67e-06 ***
    ## game_extraversion_ct       0.007595   0.025825   0.294 0.768706    
    ## game_agreeableness_ct      0.065515   0.033622   1.949 0.051479 .  
    ## game_conscientiousness_ct  0.010698   0.039557   0.270 0.786843    
    ## game_emotionstability_ct  -0.033363   0.031520  -1.058 0.289959    
    ## game_openness_ct          -0.048664   0.043146  -1.128 0.259500    
    ## tste_10_0_ct              -0.046729   0.045737  -1.022 0.307051    
    ## tste_10_1_ct               0.190743   0.050593   3.770 0.000168 ***
    ## tste_10_2_ct              -0.193424   0.052308  -3.698 0.000223 ***
    ## tste_10_3_ct              -0.172249   0.054196  -3.178 0.001503 ** 
    ## tste_10_4_ct              -0.063009   0.053205  -1.184 0.236438    
    ## tste_10_5_ct              -0.021956   0.053895  -0.407 0.683770    
    ## tste_10_6_ct              -0.138567   0.052146  -2.657 0.007937 ** 
    ## tste_10_7_ct               0.115003   0.045328   2.537 0.011248 *  
    ## tste_10_8_ct               0.022122   0.059668   0.371 0.710861    
    ## tste_10_9_ct               0.129574   0.050460   2.568 0.010302 *  
    ## release                   -0.027512   0.007809  -3.523 0.000435 ***
    ## education                  0.006517   0.026437   0.247 0.805319    
    ## income                     0.011834   0.017093   0.692 0.488818    
    ## age                       -0.014691   0.004583  -3.205 0.001369 ** 
    ## sex2                      -0.067658   0.072143  -0.938 0.348435    
    ## race4                     -0.212387   0.140709  -1.509 0.131344    
    ## race6                     -0.922944   0.346355  -2.665 0.007764 ** 
    ## race7                     -0.125258   0.136272  -0.919 0.358106    
    ## race2                      0.065665   0.131180   0.501 0.616728    
    ## star_user                 -0.059420   0.070593  -0.842 0.400030    
    ## star_GS                    0.116708   0.055938   2.086 0.037063 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.445 on 2106 degrees of freedom
    ## Multiple R-squared:  0.08777,    Adjusted R-squared:  0.07434 
    ## F-statistic: 6.536 on 31 and 2106 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9075 -0.6944  0.2920  1.0398  2.6442 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.415622  16.084102   4.005 6.42e-05 ***
    ## real_extraversion_ct       0.051771   0.022938   2.257 0.024112 *  
    ## real_agreeableness_ct     -0.060060   0.032677  -1.838 0.066204 .  
    ## real_conscientiousness_ct  0.076537   0.036520   2.096 0.036221 *  
    ## real_emotionstability_ct   0.003726   0.032737   0.114 0.909404    
    ## real_openness_ct           0.136796   0.032590   4.197 2.81e-05 ***
    ## game_extraversion_ct      -0.003117   0.025408  -0.123 0.902368    
    ## game_agreeableness_ct      0.076299   0.033283   2.292 0.021978 *  
    ## game_conscientiousness_ct  0.008220   0.039312   0.209 0.834388    
    ## game_emotionstability_ct  -0.032747   0.031230  -1.049 0.294497    
    ## game_openness_ct          -0.037585   0.042883  -0.876 0.380880    
    ## tste_11_0_ct              -0.050786   0.056185  -0.904 0.366146    
    ## tste_11_1_ct               0.060287   0.052399   1.151 0.250057    
    ## tste_11_2_ct              -0.081619   0.046662  -1.749 0.080407 .  
    ## tste_11_3_ct               0.005016   0.056987   0.088 0.929867    
    ## tste_11_4_ct              -0.167227   0.057622  -2.902 0.003745 ** 
    ## tste_11_5_ct              -0.103379   0.050273  -2.056 0.039870 *  
    ## tste_11_6_ct               0.160777   0.049181   3.269 0.001096 ** 
    ## tste_11_7_ct               0.098478   0.056584   1.740 0.081941 .  
    ## tste_11_8_ct              -0.202802   0.042150  -4.811 1.60e-06 ***
    ## tste_11_9_ct               0.172862   0.049983   3.458 0.000554 ***
    ## tste_11_10_ct              0.068563   0.054722   1.253 0.210364    
    ## release                   -0.029474   0.007920  -3.722 0.000203 ***
    ## education                  0.003049   0.026420   0.115 0.908145    
    ## income                     0.010133   0.017097   0.593 0.553467    
    ## age                       -0.013649   0.004573  -2.984 0.002873 ** 
    ## sex2                      -0.078591   0.072113  -1.090 0.275911    
    ## race4                     -0.194788   0.140629  -1.385 0.166164    
    ## race2                      0.089694   0.130739   0.686 0.492754    
    ## star_user                 -0.085246   0.072145  -1.182 0.237502    
    ## star_GS                    0.157070   0.057447   2.734 0.006306 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.446 on 2107 degrees of freedom
    ## Multiple R-squared:  0.08586,    Adjusted R-squared:  0.07285 
    ## F-statistic: 6.597 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9009 -0.7113  0.3131  1.0372  2.5628 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    52.132052  16.439720   3.171 0.001540 ** 
    ## real_extraversion_ct            0.052692   0.022974   2.294 0.021917 *  
    ## real_agreeableness_ct          -0.061178   0.032699  -1.871 0.061489 .  
    ## real_conscientiousness_ct       0.076466   0.036554   2.092 0.036569 *  
    ## real_emotionstability_ct        0.005845   0.032766   0.178 0.858444    
    ## real_openness_ct                0.139344   0.032567   4.279 1.96e-05 ***
    ## game_extraversion_ct           -0.005658   0.025471  -0.222 0.824232    
    ## game_agreeableness_ct           0.078636   0.033292   2.362 0.018266 *  
    ## game_conscientiousness_ct       0.006564   0.039377   0.167 0.867633    
    ## game_emotionstability_ct       -0.035347   0.031266  -1.131 0.258388    
    ## game_openness_ct               -0.038422   0.042903  -0.896 0.370598    
    ## tste_12_0_ct                    0.153781   0.060335   2.549 0.010880 *  
    ## tste_12_1_ct                    0.069332   0.050976   1.360 0.173940    
    ## tste_12_2_ct                    0.057990   0.048597   1.193 0.232886    
    ## tste_12_3_ct                   -0.004642   0.055012  -0.084 0.932765    
    ## tste_12_4_ct                   -0.071444   0.051108  -1.398 0.162291    
    ## tste_12_5_ct                    0.121888   0.054770   2.225 0.026155 *  
    ## tste_12_6_ct                   -0.051994   0.055881  -0.930 0.352252    
    ## tste_12_7_ct                    0.046136   0.053389   0.864 0.387609    
    ## tste_12_8_ct                    0.165589   0.049197   3.366 0.000777 ***
    ## tste_12_9_ct                   -0.053686   0.051392  -1.045 0.296312    
    ## tste_12_10_ct                   0.193115   0.045308   4.262 2.11e-05 ***
    ## tste_12_11_ct                  -0.204830   0.048627  -4.212 2.64e-05 ***
    ## release                        -0.023646   0.008079  -2.927 0.003460 ** 
    ## education                       0.003391   0.026441   0.128 0.897964    
    ## income                          0.009625   0.017106   0.563 0.573710    
    ## age                            -0.013561   0.004583  -2.959 0.003120 ** 
    ## sex2                           -0.075184   0.072184  -1.042 0.297740    
    ## race4                          -0.202229   0.140792  -1.436 0.151046    
    ## race2                           0.092758   0.130957   0.708 0.478832    
    ## star_user                      -0.048567   0.072207  -0.673 0.501270    
    ## star_GS                         0.189684   0.060517   3.134 0.001746 ** 
    ## game_openness_ct.tste_12_11_ct -0.025578   0.039064  -0.655 0.512692    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2105 degrees of freedom
    ## Multiple R-squared:  0.08535,    Adjusted R-squared:  0.07144 
    ## F-statistic: 6.138 on 32 and 2105 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1238 -0.6730  0.3028  1.0520  2.7678 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               50.753343  16.846407   3.013 0.002620 ** 
    ## real_extraversion_ct       0.053216   0.023000   2.314 0.020780 *  
    ## real_agreeableness_ct     -0.061513   0.032741  -1.879 0.060412 .  
    ## real_conscientiousness_ct  0.081311   0.036570   2.223 0.026292 *  
    ## real_emotionstability_ct   0.004904   0.032792   0.150 0.881125    
    ## real_openness_ct           0.137713   0.032638   4.219 2.55e-05 ***
    ## game_extraversion_ct      -0.008874   0.025487  -0.348 0.727749    
    ## game_agreeableness_ct      0.083700   0.033391   2.507 0.012261 *  
    ## game_conscientiousness_ct  0.007949   0.039402   0.202 0.840132    
    ## game_emotionstability_ct  -0.033412   0.031298  -1.068 0.285849    
    ## game_openness_ct          -0.035058   0.043013  -0.815 0.415131    
    ## tste_13_0_ct               0.011891   0.050487   0.236 0.813826    
    ## tste_13_1_ct              -0.091902   0.046728  -1.967 0.049345 *  
    ## tste_13_2_ct               0.198458   0.052153   3.805 0.000146 ***
    ## tste_13_3_ct               0.057938   0.050258   1.153 0.249120    
    ## tste_13_4_ct              -0.104536   0.053001  -1.972 0.048701 *  
    ## tste_13_5_ct               0.127909   0.052002   2.460 0.013985 *  
    ## tste_13_6_ct              -0.001410   0.055874  -0.025 0.979863    
    ## tste_13_7_ct              -0.069011   0.050800  -1.358 0.174457    
    ## tste_13_8_ct               0.084749   0.046702   1.815 0.069717 .  
    ## tste_13_9_ct               0.149089   0.056101   2.658 0.007931 ** 
    ## tste_13_10_ct              0.172218   0.048642   3.541 0.000408 ***
    ## tste_13_11_ct              0.098614   0.054269   1.817 0.069339 .  
    ## tste_13_12_ct              0.060417   0.051902   1.164 0.244532    
    ## release                   -0.022758   0.008274  -2.751 0.006000 ** 
    ## education                  0.005295   0.026464   0.200 0.841445    
    ## income                     0.007732   0.017109   0.452 0.651389    
    ## age                       -0.013487   0.004588  -2.939 0.003323 ** 
    ## sex2                      -0.087343   0.072237  -1.209 0.226755    
    ## race4                     -0.193157   0.140910  -1.371 0.170591    
    ## race2                      0.079627   0.130893   0.608 0.543033    
    ## star_user                 -0.088693   0.071603  -1.239 0.215607    
    ## star_GS                    0.182279   0.059512   3.063 0.002220 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2105 degrees of freedom
    ## Multiple R-squared:  0.0837, Adjusted R-squared:  0.06977 
    ## F-statistic: 6.009 on 32 and 2105 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0978 -0.6824  0.3065  1.0482  2.7050 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            45.813876  16.769086   2.732 0.006347 ** 
    ## real_extraversion_ct                    0.053466   0.023000   2.325 0.020187 *  
    ## real_agreeableness_ct                  -0.059573   0.032726  -1.820 0.068847 .  
    ## real_conscientiousness_ct               0.077137   0.036564   2.110 0.035006 *  
    ## real_emotionstability_ct                0.004135   0.032772   0.126 0.899604    
    ## real_openness_ct                        0.136089   0.032626   4.171 3.15e-05 ***
    ## game_extraversion_ct                   -0.003333   0.025468  -0.131 0.895877    
    ## game_agreeableness_ct                   0.080470   0.033374   2.411 0.015987 *  
    ## game_conscientiousness_ct               0.009657   0.039513   0.244 0.806952    
    ## game_emotionstability_ct               -0.034150   0.031265  -1.092 0.274835    
    ## game_openness_ct                       -0.034743   0.042991  -0.808 0.419101    
    ## tste_14_0_ct                           -0.103255   0.051410  -2.008 0.044723 *  
    ## tste_14_1_ct                            0.004677   0.051707   0.090 0.927929    
    ## tste_14_2_ct                            0.052765   0.047614   1.108 0.267916    
    ## tste_14_3_ct                            0.118193   0.046442   2.545 0.011000 *  
    ## tste_14_4_ct                            0.021412   0.043454   0.493 0.622239    
    ## tste_14_5_ct                           -0.035024   0.054054  -0.648 0.517096    
    ## tste_14_6_ct                            0.026027   0.050859   0.512 0.608876    
    ## tste_14_7_ct                           -0.257985   0.059460  -4.339 1.50e-05 ***
    ## tste_14_8_ct                            0.160028   0.045384   3.526 0.000431 ***
    ## tste_14_9_ct                           -0.020911   0.051511  -0.406 0.684828    
    ## tste_14_10_ct                           0.107206   0.049378   2.171 0.030034 *  
    ## tste_14_11_ct                          -0.057922   0.049072  -1.180 0.237993    
    ## tste_14_12_ct                           0.084226   0.051647   1.631 0.103084    
    ## tste_14_13_ct                           0.114883   0.052169   2.202 0.027765 *  
    ## release                                -0.020455   0.008246  -2.481 0.013192 *  
    ## education                               0.001558   0.026496   0.059 0.953104    
    ## income                                  0.008371   0.017098   0.490 0.624480    
    ## age                                    -0.013048   0.004586  -2.845 0.004481 ** 
    ## sex2                                   -0.080489   0.072266  -1.114 0.265494    
    ## race4                                  -0.184945   0.141002  -1.312 0.189785    
    ## game_conscientiousness_ct.tste_14_1_ct  0.019068   0.037934   0.503 0.615254    
    ## race2                                   0.075151   0.131090   0.573 0.566521    
    ## star_user                              -0.041783   0.072007  -0.580 0.561800    
    ## star_GS                                 0.172285   0.059639   2.889 0.003907 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 2103 degrees of freedom
    ## Multiple R-squared:  0.08592,    Adjusted R-squared:  0.07114 
    ## F-statistic: 5.814 on 34 and 2103 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0525 -0.6929  0.3163  1.0372  2.7400 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            53.505568  17.752680   3.014 0.002610 ** 
    ## real_extraversion_ct                    0.052517   0.023009   2.282 0.022561 *  
    ## real_agreeableness_ct                  -0.062132   0.032745  -1.897 0.057907 .  
    ## real_conscientiousness_ct               0.079408   0.036601   2.170 0.030152 *  
    ## real_emotionstability_ct                0.006960   0.032809   0.212 0.832020    
    ## real_openness_ct                        0.136155   0.032657   4.169 3.18e-05 ***
    ## game_extraversion_ct                   -0.003025   0.025503  -0.119 0.905597    
    ## game_agreeableness_ct                   0.080935   0.033387   2.424 0.015427 *  
    ## game_conscientiousness_ct               0.009342   0.039504   0.236 0.813077    
    ## game_emotionstability_ct               -0.034001   0.031308  -1.086 0.277597    
    ## game_openness_ct                       -0.032849   0.043026  -0.763 0.445269    
    ## tste_15_0_ct                           -0.061498   0.048087  -1.279 0.201079    
    ## tste_15_1_ct                           -0.119470   0.053769  -2.222 0.026396 *  
    ## tste_15_2_ct                            0.158332   0.048569   3.260 0.001132 ** 
    ## tste_15_3_ct                            0.092887   0.052557   1.767 0.077316 .  
    ## tste_15_4_ct                            0.015112   0.047047   0.321 0.748074    
    ## tste_15_5_ct                           -0.069586   0.053728  -1.295 0.195410    
    ## tste_15_6_ct                           -0.112795   0.046624  -2.419 0.015637 *  
    ## tste_15_7_ct                           -0.065654   0.051675  -1.271 0.204046    
    ## tste_15_8_ct                           -0.124363   0.054004  -2.303 0.021384 *  
    ## tste_15_9_ct                            0.032493   0.051093   0.636 0.524866    
    ## tste_15_10_ct                           0.049732   0.053250   0.934 0.350445    
    ## tste_15_11_ct                           0.188834   0.054348   3.475 0.000522 ***
    ## tste_15_12_ct                          -0.047338   0.038164  -1.240 0.214970    
    ## tste_15_13_ct                           0.032103   0.044437   0.722 0.470102    
    ## tste_15_14_ct                           0.051500   0.046958   1.097 0.272887    
    ## release                                -0.024028   0.008719  -2.756 0.005905 ** 
    ## education                               0.001960   0.026493   0.074 0.941030    
    ## income                                  0.008214   0.017125   0.480 0.631535    
    ## age                                    -0.013170   0.004587  -2.871 0.004127 ** 
    ## sex2                                   -0.081779   0.072355  -1.130 0.258506    
    ## race4                                  -0.188023   0.140913  -1.334 0.182245    
    ## game_conscientiousness_ct.tste_15_4_ct  0.030481   0.033802   0.902 0.367281    
    ## race2                                   0.069400   0.130942   0.530 0.596166    
    ## star_user                              -0.074560   0.075699  -0.985 0.324761    
    ## star_GS                                 0.143512   0.060333   2.379 0.017465 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2102 degrees of freedom
    ## Multiple R-squared:  0.08499,    Adjusted R-squared:  0.06975 
    ## F-statistic: 5.578 on 35 and 2102 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9088 -0.6903  0.2818  1.0364  2.7254 
    ## 
    ## Coefficients:
    ##                                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            35.3175434 17.7183119   1.993 0.046360 *  
    ## real_extraversion_ct                    0.0572880  0.0229350   2.498 0.012571 *  
    ## real_agreeableness_ct                  -0.0678728  0.0326357  -2.080 0.037673 *  
    ## real_conscientiousness_ct               0.0767994  0.0364456   2.107 0.035215 *  
    ## real_emotionstability_ct                0.0109631  0.0327026   0.335 0.737481    
    ## real_openness_ct                        0.1337264  0.0325917   4.103 4.23e-05 ***
    ## game_extraversion_ct                   -0.0095231  0.0254098  -0.375 0.707861    
    ## game_agreeableness_ct                   0.0871820  0.0332640   2.621 0.008833 ** 
    ## game_conscientiousness_ct               0.0132571  0.0393566   0.337 0.736267    
    ## game_emotionstability_ct               -0.0342417  0.0311682  -1.099 0.272063    
    ## game_openness_ct                       -0.0302350  0.0428793  -0.705 0.480815    
    ## tste_16_0_ct                            0.1757625  0.0486430   3.613 0.000309 ***
    ## tste_16_1_ct                           -0.1018785  0.0498655  -2.043 0.041171 *  
    ## tste_16_2_ct                           -0.0540043  0.0449324  -1.202 0.229537    
    ## tste_16_3_ct                           -0.1612364  0.0505147  -3.192 0.001434 ** 
    ## tste_16_4_ct                           -0.1712341  0.0412319  -4.153 3.41e-05 ***
    ## tste_16_5_ct                            0.1059239  0.0482409   2.196 0.028220 *  
    ## tste_16_6_ct                            0.1006794  0.0441072   2.283 0.022553 *  
    ## tste_16_7_ct                            0.0733781  0.0446390   1.644 0.100365    
    ## tste_16_8_ct                           -0.0625908  0.0520096  -1.203 0.228939    
    ## tste_16_9_ct                            0.1609606  0.0447996   3.593 0.000335 ***
    ## tste_16_10_ct                          -0.0776162  0.0440354  -1.763 0.078115 .  
    ## tste_16_11_ct                           0.1563686  0.0503038   3.108 0.001906 ** 
    ## tste_16_12_ct                           0.0775994  0.0461590   1.681 0.092886 .  
    ## tste_16_13_ct                           0.0548753  0.0497131   1.104 0.269789    
    ## tste_16_14_ct                           0.1146815  0.0462602   2.479 0.013251 *  
    ## tste_16_15_ct                          -0.0130311  0.0502230  -0.259 0.795301    
    ## release                                -0.0150417  0.0087053  -1.728 0.084158 .  
    ## education                               0.0007174  0.0263704   0.027 0.978299    
    ## income                                  0.0121436  0.0170833   0.711 0.477260    
    ## age                                    -0.0119128  0.0045780  -2.602 0.009329 ** 
    ## sex2                                   -0.0828154  0.0719664  -1.151 0.249966    
    ## race4                                  -0.1809460  0.1403768  -1.289 0.197539    
    ## game_conscientiousness_ct.tste_16_1_ct -0.0419925  0.0357509  -1.175 0.240294    
    ## race2                                   0.0932473  0.1305246   0.714 0.475057    
    ## star_user                              -0.0715325  0.0724214  -0.988 0.323401    
    ## star_GS                                 0.1515573  0.0585316   2.589 0.009683 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2101 degrees of freedom
    ## Multiple R-squared:  0.09331,    Adjusted R-squared:  0.07778 
    ## F-statistic: 6.006 on 36 and 2101 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1612 -0.6967  0.2835  1.0378  2.7707 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               36.921702  18.274197   2.020  0.04347 *  
    ## real_extraversion_ct       0.056599   0.022909   2.471  0.01357 *  
    ## real_agreeableness_ct     -0.063867   0.032613  -1.958  0.05033 .  
    ## real_conscientiousness_ct  0.079106   0.036419   2.172  0.02996 *  
    ## real_emotionstability_ct   0.008767   0.032644   0.269  0.78829    
    ## real_openness_ct           0.134036   0.032480   4.127 3.82e-05 ***
    ## game_extraversion_ct      -0.010525   0.025371  -0.415  0.67829    
    ## game_agreeableness_ct      0.086683   0.033250   2.607  0.00920 ** 
    ## game_conscientiousness_ct  0.008389   0.039253   0.214  0.83079    
    ## game_emotionstability_ct  -0.030772   0.031161  -0.988  0.32350    
    ## game_openness_ct          -0.028058   0.042787  -0.656  0.51205    
    ## tste_17_0_ct               0.314805   0.048966   6.429 1.58e-10 ***
    ## tste_17_1_ct               0.065331   0.041972   1.557  0.11973    
    ## tste_17_2_ct              -0.074623   0.049957  -1.494  0.13539    
    ## tste_17_3_ct               0.051092   0.047752   1.070  0.28477    
    ## tste_17_4_ct               0.090624   0.050173   1.806  0.07102 .  
    ## tste_17_5_ct               0.022808   0.046783   0.488  0.62594    
    ## tste_17_6_ct              -0.056196   0.049201  -1.142  0.25351    
    ## tste_17_7_ct               0.092025   0.043169   2.132  0.03315 *  
    ## tste_17_8_ct              -0.065721   0.047950  -1.371  0.17064    
    ## tste_17_9_ct              -0.105404   0.046220  -2.281  0.02268 *  
    ## tste_17_10_ct              0.092762   0.042490   2.183  0.02914 *  
    ## tste_17_11_ct              0.038253   0.046844   0.817  0.41425    
    ## tste_17_12_ct             -0.169330   0.042849  -3.952 8.01e-05 ***
    ## tste_17_13_ct              0.086715   0.049157   1.764  0.07787 .  
    ## tste_17_14_ct              0.029644   0.043392   0.683  0.49458    
    ## tste_17_15_ct             -0.034863   0.056003  -0.623  0.53366    
    ## tste_17_16_ct             -0.049485   0.042882  -1.154  0.24865    
    ## release                   -0.015920   0.008953  -1.778  0.07553 .  
    ## education                  0.002655   0.026347   0.101  0.91976    
    ## income                     0.012779   0.017056   0.749  0.45380    
    ## age                       -0.011990   0.004579  -2.618  0.00890 ** 
    ## sex2                      -0.089685   0.071866  -1.248  0.21219    
    ## race4                     -0.192676   0.140288  -1.373  0.16976    
    ## race2                      0.083343   0.130414   0.639  0.52285    
    ## star_user                 -0.076855   0.076728  -1.002  0.31662    
    ## star_GS                    0.175622   0.060311   2.912  0.00363 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2101 degrees of freedom
    ## Multiple R-squared:  0.09484,    Adjusted R-squared:  0.07933 
    ## F-statistic: 6.115 on 36 and 2101 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0711 -0.6824  0.2913  1.0511  2.5914 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.834680  18.060768   2.593  0.00958 ** 
    ## real_extraversion_ct       0.053444   0.022900   2.334  0.01970 *  
    ## real_agreeableness_ct     -0.062877   0.032631  -1.927  0.05413 .  
    ## real_conscientiousness_ct  0.080390   0.036459   2.205  0.02757 *  
    ## real_emotionstability_ct   0.006157   0.032667   0.188  0.85052    
    ## real_openness_ct           0.133505   0.032503   4.107 4.15e-05 ***
    ## game_extraversion_ct      -0.008145   0.025387  -0.321  0.74838    
    ## game_agreeableness_ct      0.085959   0.033237   2.586  0.00977 ** 
    ## game_conscientiousness_ct  0.004017   0.039265   0.102  0.91852    
    ## game_emotionstability_ct  -0.031696   0.031174  -1.017  0.30940    
    ## game_openness_ct          -0.028102   0.042819  -0.656  0.51171    
    ## tste_18_0_ct              -0.126348   0.046160  -2.737  0.00625 ** 
    ## tste_18_1_ct               0.155745   0.048011   3.244  0.00120 ** 
    ## tste_18_2_ct              -0.015902   0.043623  -0.365  0.71550    
    ## tste_18_3_ct              -0.016928   0.046861  -0.361  0.71797    
    ## tste_18_4_ct               0.075896   0.051139   1.484  0.13793    
    ## tste_18_5_ct              -0.027654   0.048058  -0.575  0.56506    
    ## tste_18_6_ct              -0.003760   0.050367  -0.075  0.94050    
    ## tste_18_7_ct              -0.054930   0.043237  -1.270  0.20407    
    ## tste_18_8_ct               0.080079   0.047055   1.702  0.08894 .  
    ## tste_18_9_ct              -0.137937   0.047797  -2.886  0.00394 ** 
    ## tste_18_10_ct              0.087031   0.045895   1.896  0.05806 .  
    ## tste_18_11_ct              0.063118   0.045141   1.398  0.16219    
    ## tste_18_12_ct             -0.079615   0.047522  -1.675  0.09402 .  
    ## tste_18_13_ct             -0.033253   0.046785  -0.711  0.47731    
    ## tste_18_14_ct              0.050808   0.050857   0.999  0.31790    
    ## tste_18_15_ct              0.163233   0.058538   2.788  0.00534 ** 
    ## tste_18_16_ct             -0.295025   0.051394  -5.740 1.08e-08 ***
    ## tste_18_17_ct              0.046545   0.045699   1.019  0.30855    
    ## release                   -0.020494   0.008865  -2.312  0.02088 *  
    ## education                  0.002461   0.026351   0.093  0.92558    
    ## income                     0.012055   0.017062   0.707  0.47994    
    ## age                       -0.011935   0.004581  -2.605  0.00925 ** 
    ## sex2                      -0.083326   0.071937  -1.158  0.24686    
    ## race4                     -0.189153   0.140295  -1.348  0.17772    
    ## race2                      0.077822   0.130490   0.596  0.55098    
    ## star_user                 -0.081644   0.077149  -1.058  0.29006    
    ## star_GS                    0.093272   0.061090   1.527  0.12697    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2100 degrees of freedom
    ## Multiple R-squared:  0.09499,    Adjusted R-squared:  0.07905 
    ## F-statistic: 5.957 on 37 and 2100 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0906 -0.6768  0.2886  1.0625  2.6547 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               39.009739  18.502681   2.108  0.03512 *  
    ## real_extraversion_ct       0.056614   0.022970   2.465  0.01379 *  
    ## real_agreeableness_ct     -0.062722   0.032694  -1.918  0.05519 .  
    ## real_conscientiousness_ct  0.079920   0.036540   2.187  0.02884 *  
    ## real_emotionstability_ct   0.009297   0.032775   0.284  0.77671    
    ## real_openness_ct           0.134390   0.032572   4.126 3.84e-05 ***
    ## game_extraversion_ct      -0.010006   0.025452  -0.393  0.69425    
    ## game_agreeableness_ct      0.085812   0.033325   2.575  0.01009 *  
    ## game_conscientiousness_ct  0.007526   0.039364   0.191  0.84840    
    ## game_emotionstability_ct  -0.030194   0.031246  -0.966  0.33400    
    ## game_openness_ct          -0.024837   0.042906  -0.579  0.56273    
    ## tste_19_0_ct               0.003474   0.048293   0.072  0.94266    
    ## tste_19_1_ct               0.063144   0.050327   1.255  0.20974    
    ## tste_19_2_ct               0.024392   0.048980   0.498  0.61854    
    ## tste_19_3_ct              -0.116759   0.046485  -2.512  0.01209 *  
    ## tste_19_4_ct              -0.109623   0.049011  -2.237  0.02541 *  
    ## tste_19_5_ct              -0.035827   0.053573  -0.669  0.50372    
    ## tste_19_6_ct               0.081616   0.047789   1.708  0.08781 .  
    ## tste_19_7_ct               0.111999   0.046886   2.389  0.01699 *  
    ## tste_19_8_ct               0.037670   0.046858   0.804  0.42152    
    ## tste_19_9_ct              -0.075004   0.050226  -1.493  0.13550    
    ## tste_19_10_ct              0.129607   0.047892   2.706  0.00686 ** 
    ## tste_19_11_ct              0.089879   0.044893   2.002  0.04540 *  
    ## tste_19_12_ct              0.228441   0.045632   5.006 6.02e-07 ***
    ## tste_19_13_ct             -0.012129   0.043264  -0.280  0.77924    
    ## tste_19_14_ct              0.175900   0.044998   3.909 9.56e-05 ***
    ## tste_19_15_ct              0.002336   0.044161   0.053  0.95781    
    ## tste_19_16_ct              0.024523   0.046871   0.523  0.60090    
    ## tste_19_17_ct              0.055599   0.053025   1.049  0.29452    
    ## tste_19_18_ct              0.002615   0.041034   0.064  0.94920    
    ## release                   -0.016912   0.009083  -1.862  0.06275 .  
    ## education                  0.002709   0.026412   0.103  0.91830    
    ## income                     0.013099   0.017099   0.766  0.44372    
    ## age                       -0.011869   0.004593  -2.584  0.00983 ** 
    ## sex2                      -0.086056   0.072128  -1.193  0.23296    
    ## race4                     -0.195368   0.140586  -1.390  0.16478    
    ## race2                      0.081491   0.130822   0.623  0.53341    
    ## star_user                 -0.032522   0.074702  -0.435  0.66335    
    ## star_GS                    0.118739   0.060452   1.964  0.04964 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.444 on 2099 degrees of freedom
    ## Multiple R-squared:  0.09162,    Adjusted R-squared:  0.07517 
    ## F-statistic: 5.571 on 38 and 2099 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0247 -0.6962  0.2862  1.0310  2.5406 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                            36.328471  19.318463   1.881 0.060178 .  
    ## real_extraversion_ct                    0.056081   0.022914   2.447 0.014469 *  
    ## real_agreeableness_ct                  -0.061494   0.032635  -1.884 0.059662 .  
    ## real_conscientiousness_ct               0.081633   0.036459   2.239 0.025258 *  
    ## real_emotionstability_ct                0.009599   0.032690   0.294 0.769057    
    ## real_openness_ct                        0.132435   0.032516   4.073 4.81e-05 ***
    ## game_extraversion_ct                   -0.008301   0.025374  -0.327 0.743608    
    ## game_agreeableness_ct                   0.083687   0.033267   2.516 0.011956 *  
    ## game_conscientiousness_ct               0.005261   0.039292   0.134 0.893488    
    ## game_emotionstability_ct               -0.030207   0.031222  -0.967 0.333411    
    ## game_openness_ct                       -0.024011   0.042834  -0.561 0.575152    
    ## tste_20_0_ct                            0.082126   0.053639   1.531 0.125901    
    ## tste_20_1_ct                            0.083223   0.046571   1.787 0.074078 .  
    ## tste_20_2_ct                            0.091585   0.042452   2.157 0.031089 *  
    ## tste_20_3_ct                            0.155411   0.048760   3.187 0.001457 ** 
    ## tste_20_4_ct                            0.033545   0.044375   0.756 0.449770    
    ## tste_20_5_ct                           -0.072609   0.043862  -1.655 0.097993 .  
    ## tste_20_6_ct                            0.026680   0.049574   0.538 0.590502    
    ## tste_20_7_ct                            0.188740   0.054184   3.483 0.000505 ***
    ## tste_20_8_ct                           -0.016113   0.053762  -0.300 0.764424    
    ## tste_20_9_ct                            0.101484   0.044235   2.294 0.021877 *  
    ## tste_20_10_ct                           0.063176   0.048045   1.315 0.188676    
    ## tste_20_11_ct                          -0.126611   0.045610  -2.776 0.005553 ** 
    ## tste_20_12_ct                           0.122316   0.046072   2.655 0.007994 ** 
    ## tste_20_13_ct                           0.112577   0.050344   2.236 0.025445 *  
    ## tste_20_14_ct                          -0.099047   0.041120  -2.409 0.016093 *  
    ## tste_20_15_ct                           0.111987   0.053320   2.100 0.035823 *  
    ## tste_20_16_ct                          -0.097390   0.046212  -2.107 0.035195 *  
    ## tste_20_17_ct                           0.034438   0.045000   0.765 0.444192    
    ## tste_20_18_ct                           0.139823   0.042642   3.279 0.001059 ** 
    ## tste_20_19_ct                          -0.039439   0.048660  -0.810 0.417745    
    ## release                                -0.015488   0.009484  -1.633 0.102602    
    ## education                               0.003206   0.026358   0.122 0.903188    
    ## income                                  0.012452   0.017065   0.730 0.465646    
    ## age                                    -0.011562   0.004585  -2.522 0.011746 *  
    ## sex2                                   -0.077432   0.072033  -1.075 0.282519    
    ## race4                                  -0.179236   0.140351  -1.277 0.201724    
    ## game_emotionstability_ct.tste_20_14_ct -0.021317   0.026208  -0.813 0.416097    
    ## race2                                   0.097291   0.130440   0.746 0.455832    
    ## star_user                              -0.015858   0.078072  -0.203 0.839056    
    ## star_GS                                 0.079148   0.061367   1.290 0.197281    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2097 degrees of freedom
    ## Multiple R-squared:  0.09643,    Adjusted R-squared:  0.07919 
    ## F-statistic: 5.595 on 40 and 2097 DF,  p-value: < 2.2e-16

### preference ~ tste + real + gap

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 77:95)$model_lm_1) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9676 -0.6663  0.3197  1.0787  2.4729 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.022394  14.205477   4.648 3.56e-06 ***
    ## real_extraversion_ct       0.043571   0.025104   1.736 0.082772 .  
    ## real_agreeableness_ct      0.016532   0.031423   0.526 0.598865    
    ## real_conscientiousness_ct  0.084261   0.034907   2.414 0.015869 *  
    ## real_emotionstability_ct  -0.032134   0.034602  -0.929 0.353155    
    ## real_openness_ct           0.111504   0.033476   3.331 0.000881 ***
    ## tste_2_0_ct                0.096247   0.041350   2.328 0.020026 *  
    ## tste_2_1_ct               -0.070688   0.040306  -1.754 0.079610 .  
    ## release                   -0.030065   0.006987  -4.303 1.76e-05 ***
    ## gap_openness              -0.021645   0.043236  -0.501 0.616690    
    ## income                     0.005663   0.017266   0.328 0.742979    
    ## gap_extraversion          -0.006779   0.025694  -0.264 0.791918    
    ## gap_agreeableness          0.080118   0.033610   2.384 0.017225 *  
    ## gap_emotionstability      -0.034297   0.031562  -1.087 0.277309    
    ## age                       -0.014033   0.004622  -3.036 0.002428 ** 
    ## sex2                      -0.094816   0.072814  -1.302 0.193003    
    ## gap_conscientiousness      0.004346   0.039764   0.109 0.912983    
    ## race4                     -0.238258   0.142118  -1.676 0.093792 .  
    ## education                  0.006206   0.026723   0.232 0.816388    
    ## race2                      0.104013   0.132136   0.787 0.431271    
    ## star_user                  0.007665   0.058043   0.132 0.894948    
    ## star_GS                    0.013628   0.048089   0.283 0.776900    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2116 degrees of freedom
    ## Multiple R-squared:  0.05929,    Adjusted R-squared:  0.04995 
    ## F-statistic: 6.351 on 21 and 2116 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8480 -0.6883  0.3104  1.0782  2.5237 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.327697  14.157877   4.261 2.12e-05 ***
    ## real_extraversion_ct       0.041835   0.024944   1.677 0.093655 .  
    ## real_agreeableness_ct      0.018248   0.031226   0.584 0.559022    
    ## real_conscientiousness_ct  0.085788   0.034686   2.473 0.013466 *  
    ## real_emotionstability_ct  -0.030027   0.034382  -0.873 0.382581    
    ## real_openness_ct           0.111533   0.033263   3.353 0.000813 ***
    ## tste_3_0_ct               -0.046926   0.043474  -1.079 0.280529    
    ## tste_3_1_ct                0.151829   0.035343   4.296 1.82e-05 ***
    ## tste_3_2_ct                0.244334   0.049414   4.945 8.23e-07 ***
    ## release                   -0.027370   0.006963  -3.931 8.74e-05 ***
    ## gap_openness              -0.025567   0.042968  -0.595 0.551897    
    ## income                     0.007114   0.017158   0.415 0.678453    
    ## gap_extraversion          -0.007257   0.025527  -0.284 0.776203    
    ## gap_agreeableness          0.084978   0.033409   2.544 0.011043 *  
    ## gap_emotionstability      -0.034147   0.031363  -1.089 0.276381    
    ## age                       -0.013868   0.004593  -3.020 0.002562 ** 
    ## sex2                      -0.083385   0.072375  -1.152 0.249401    
    ## gap_conscientiousness      0.006951   0.039513   0.176 0.860379    
    ## race4                     -0.212103   0.141302  -1.501 0.133488    
    ## education                  0.003708   0.026557   0.140 0.888956    
    ## race2                      0.100747   0.131295   0.767 0.442972    
    ## star_user                  0.006920   0.057791   0.120 0.904694    
    ## star_GS                    0.047301   0.048524   0.975 0.329771    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.455 on 2115 degrees of freedom
    ## Multiple R-squared:  0.07168,    Adjusted R-squared:  0.06202 
    ## F-statistic: 7.423 on 22 and 2115 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9022 -0.6807  0.3069  1.0635  2.6029 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.589799  14.504874   4.453 8.91e-06 ***
    ## real_extraversion_ct       0.041604   0.024923   1.669  0.09521 .  
    ## real_agreeableness_ct      0.018983   0.031221   0.608  0.54323    
    ## real_conscientiousness_ct  0.085572   0.034660   2.469  0.01363 *  
    ## real_emotionstability_ct  -0.029572   0.034364  -0.861  0.38958    
    ## real_openness_ct           0.112145   0.033259   3.372  0.00076 ***
    ## tste_4_0_ct                0.258482   0.050020   5.168 2.59e-07 ***
    ## tste_4_1_ct                0.049064   0.056597   0.867  0.38609    
    ## tste_4_2_ct                0.057063   0.041574   1.373  0.17004    
    ## tste_4_3_ct               -0.164001   0.039379  -4.165 3.24e-05 ***
    ## release                   -0.029427   0.007130  -4.127 3.82e-05 ***
    ## gap_openness              -0.025390   0.042937  -0.591  0.55435    
    ## income                     0.006625   0.017144   0.386  0.69920    
    ## gap_extraversion          -0.007477   0.025512  -0.293  0.76951    
    ## gap_agreeableness          0.084210   0.033379   2.523  0.01171 *  
    ## gap_emotionstability      -0.034094   0.031338  -1.088  0.27675    
    ## age                       -0.013665   0.004593  -2.975  0.00296 ** 
    ## sex2                      -0.080755   0.072342  -1.116  0.26442    
    ## gap_conscientiousness      0.005910   0.039480   0.150  0.88101    
    ## race4                     -0.210537   0.141210  -1.491  0.13613    
    ## education                  0.003418   0.026538   0.129  0.89753    
    ## race2                      0.099423   0.131258   0.757  0.44886    
    ## star_GS                    0.080251   0.052720   1.522  0.12810    
    ## star_user                 -0.041872   0.064751  -0.647  0.51792    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2114 degrees of freedom
    ## Multiple R-squared:  0.07359,    Adjusted R-squared:  0.06351 
    ## F-statistic: 7.301 on 23 and 2114 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8612 -0.6798  0.3097  1.0693  2.5830 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               53.032851  14.798622   3.584 0.000347 ***
    ## real_extraversion_ct       0.043927   0.024959   1.760 0.078560 .  
    ## real_agreeableness_ct      0.018987   0.031255   0.607 0.543590    
    ## real_conscientiousness_ct  0.086764   0.034705   2.500 0.012492 *  
    ## real_emotionstability_ct  -0.029687   0.034411  -0.863 0.388392    
    ## real_openness_ct           0.109625   0.033339   3.288 0.001025 ** 
    ## tste_5_0_ct                0.229889   0.047039   4.887  1.1e-06 ***
    ## tste_5_1_ct               -0.160035   0.052062  -3.074 0.002140 ** 
    ## tste_5_2_ct               -0.028640   0.040859  -0.701 0.483410    
    ## tste_5_3_ct               -0.089317   0.050718  -1.761 0.078375 .  
    ## tste_5_4_ct                0.059893   0.052831   1.134 0.257062    
    ## release                   -0.023666   0.007275  -3.253 0.001159 ** 
    ## gap_openness              -0.026850   0.043021  -0.624 0.532614    
    ## income                     0.007785   0.017174   0.453 0.650393    
    ## gap_extraversion          -0.007016   0.025547  -0.275 0.783634    
    ## gap_agreeableness          0.084049   0.033416   2.515 0.011970 *  
    ## gap_emotionstability      -0.033788   0.031381  -1.077 0.281743    
    ## age                       -0.013851   0.004600  -3.011 0.002632 ** 
    ## sex2                      -0.087840   0.072429  -1.213 0.225352    
    ## gap_conscientiousness      0.007069   0.039534   0.179 0.858107    
    ## race4                     -0.215691   0.141386  -1.526 0.127272    
    ## education                  0.003770   0.026577   0.142 0.887223    
    ## race2                      0.089580   0.131421   0.682 0.495552    
    ## star_user                 -0.025943   0.067204  -0.386 0.699507    
    ## star_GS                    0.063409   0.053626   1.182 0.237169    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.455 on 2113 degrees of freedom
    ## Multiple R-squared:  0.0716, Adjusted R-squared:  0.06105 
    ## F-statistic:  6.79 on 24 and 2113 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9245 -0.6876  0.3068  1.0668  2.5518 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.855246  14.950738   3.468 0.000534 ***
    ## real_extraversion_ct       0.046128   0.024983   1.846 0.064977 .  
    ## real_agreeableness_ct      0.016634   0.031288   0.532 0.595025    
    ## real_conscientiousness_ct  0.086140   0.034712   2.482 0.013158 *  
    ## real_emotionstability_ct  -0.028558   0.034422  -0.830 0.406827    
    ## real_openness_ct           0.110372   0.033341   3.310 0.000947 ***
    ## tste_6_0_ct                0.058255   0.052752   1.104 0.269578    
    ## tste_6_1_ct               -0.113201   0.050575  -2.238 0.025305 *  
    ## tste_6_2_ct                0.243196   0.043529   5.587 2.61e-08 ***
    ## tste_6_3_ct                0.008622   0.053137   0.162 0.871120    
    ## tste_6_4_ct                0.105179   0.047666   2.207 0.027452 *  
    ## tste_6_5_ct                0.086392   0.055763   1.549 0.121468    
    ## release                   -0.023027   0.007350  -3.133 0.001755 ** 
    ## gap_openness              -0.027844   0.043055  -0.647 0.517885    
    ## income                     0.008093   0.017179   0.471 0.637609    
    ## gap_extraversion          -0.004961   0.025565  -0.194 0.846165    
    ## gap_agreeableness          0.081958   0.033452   2.450 0.014364 *  
    ## gap_emotionstability      -0.033587   0.031387  -1.070 0.284698    
    ## age                       -0.013774   0.004601  -2.994 0.002787 ** 
    ## sex2                      -0.085632   0.072446  -1.182 0.237335    
    ## gap_conscientiousness      0.007562   0.039551   0.191 0.848392    
    ## race4                     -0.210985   0.141431  -1.492 0.135906    
    ## education                  0.003228   0.026583   0.121 0.903374    
    ## race2                      0.077684   0.131589   0.590 0.555018    
    ## star_user                 -0.044241   0.069642  -0.635 0.525333    
    ## star_GS                    0.068817   0.054648   1.259 0.208068    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2112 degrees of freedom
    ## Multiple R-squared:  0.07164,    Adjusted R-squared:  0.06065 
    ## F-statistic: 6.519 on 25 and 2112 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8567 -0.6859  0.2904  1.0666  2.5886 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               54.999577  15.227129   3.612 0.000311 ***
    ## real_extraversion_ct       0.043187   0.024959   1.730 0.083713 .  
    ## real_agreeableness_ct      0.021016   0.031260   0.672 0.501474    
    ## real_conscientiousness_ct  0.086040   0.034662   2.482 0.013132 *  
    ## real_emotionstability_ct  -0.029931   0.034370  -0.871 0.383933    
    ## real_openness_ct           0.109613   0.033283   3.293 0.001006 ** 
    ## tste_7_0_ct               -0.183566   0.048078  -3.818 0.000138 ***
    ## tste_7_1_ct                0.025963   0.051750   0.502 0.615933    
    ## tste_7_2_ct               -0.159619   0.051163  -3.120 0.001834 ** 
    ## tste_7_3_ct               -0.203790   0.038931  -5.235 1.82e-07 ***
    ## tste_7_4_ct                0.005227   0.052785   0.099 0.921125    
    ## tste_7_5_ct                0.023004   0.056317   0.408 0.682970    
    ## tste_7_6_ct                0.041309   0.059967   0.689 0.490987    
    ## release                   -0.024669   0.007495  -3.292 0.001013 ** 
    ## gap_openness              -0.026604   0.043035  -0.618 0.536507    
    ## income                     0.007652   0.017156   0.446 0.655620    
    ## gap_extraversion          -0.007273   0.025525  -0.285 0.775726    
    ## gap_agreeableness          0.085176   0.033385   2.551 0.010801 *  
    ## gap_emotionstability      -0.033228   0.031349  -1.060 0.289296    
    ## age                       -0.013522   0.004594  -2.943 0.003284 ** 
    ## sex2                      -0.088388   0.072335  -1.222 0.221870    
    ## gap_conscientiousness      0.007422   0.039492   0.188 0.850941    
    ## race4                     -0.205824   0.141260  -1.457 0.145250    
    ## education                  0.004152   0.026549   0.156 0.875744    
    ## race2                      0.093531   0.131291   0.712 0.476302    
    ## star_user                 -0.027055   0.070449  -0.384 0.700988    
    ## star_GS                    0.068585   0.052942   1.295 0.195300    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2111 degrees of freedom
    ## Multiple R-squared:  0.07499,    Adjusted R-squared:  0.0636 
    ## F-statistic: 6.583 on 26 and 2111 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9941 -0.6804  0.3028  1.0439  2.7130 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               61.114263  15.935454   3.835 0.000129 ***
    ## real_extraversion_ct       0.045198   0.024913   1.814 0.069780 .  
    ## real_agreeableness_ct      0.020335   0.031168   0.652 0.514190    
    ## real_conscientiousness_ct  0.086407   0.034597   2.498 0.012582 *  
    ## real_emotionstability_ct  -0.030296   0.034303  -0.883 0.377232    
    ## real_openness_ct           0.105972   0.033225   3.190 0.001446 ** 
    ## tste_8_0_ct               -0.199105   0.050871  -3.914 9.37e-05 ***
    ## tste_8_1_ct                0.207717   0.057537   3.610 0.000313 ***
    ## tste_8_2_ct                0.099339   0.050892   1.952 0.051074 .  
    ## tste_8_3_ct                0.083446   0.047600   1.753 0.079733 .  
    ## tste_8_4_ct                0.138113   0.052357   2.638 0.008403 ** 
    ## tste_8_5_ct                0.089219   0.049432   1.805 0.071237 .  
    ## tste_8_6_ct               -0.119111   0.052937  -2.250 0.024548 *  
    ## tste_8_7_ct               -0.046222   0.051954  -0.890 0.373749    
    ## release                   -0.027579   0.007843  -3.516 0.000447 ***
    ## gap_openness              -0.034059   0.042930  -0.793 0.427659    
    ## income                     0.007330   0.017118   0.428 0.668565    
    ## gap_extraversion          -0.005455   0.025473  -0.214 0.830462    
    ## gap_agreeableness          0.081858   0.033319   2.457 0.014098 *  
    ## gap_emotionstability      -0.035053   0.031276  -1.121 0.262511    
    ## age                       -0.013435   0.004586  -2.930 0.003427 ** 
    ## sex2                      -0.089946   0.072154  -1.247 0.212685    
    ## gap_conscientiousness      0.007121   0.039406   0.181 0.856623    
    ## race4                     -0.194645   0.140992  -1.381 0.167566    
    ## education                  0.004098   0.026494   0.155 0.877083    
    ## race2                      0.078760   0.131034   0.601 0.547860    
    ## star_user                 -0.086282   0.070837  -1.218 0.223346    
    ## star_GS                    0.095852   0.055103   1.739 0.082093 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.45 on 2110 degrees of freedom
    ## Multiple R-squared:  0.07934,    Adjusted R-squared:  0.06755 
    ## F-statistic: 6.734 on 27 and 2110 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9259 -0.6942  0.2910  1.0629  2.8502 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               61.969319  16.060158   3.859 0.000117 ***
    ## real_extraversion_ct       0.048116   0.024845   1.937 0.052924 .  
    ## real_agreeableness_ct      0.014256   0.031110   0.458 0.646832    
    ## real_conscientiousness_ct  0.087348   0.034522   2.530 0.011471 *  
    ## real_emotionstability_ct  -0.026923   0.034223  -0.787 0.431538    
    ## real_openness_ct           0.100961   0.033172   3.044 0.002367 ** 
    ## tste_9_0_ct               -0.160968   0.053339  -3.018 0.002577 ** 
    ## tste_9_1_ct                0.043629   0.050717   0.860 0.389759    
    ## tste_9_2_ct               -0.351803   0.052364  -6.718 2.35e-11 ***
    ## tste_9_3_ct               -0.052943   0.057651  -0.918 0.358548    
    ## tste_9_4_ct               -0.011170   0.043798  -0.255 0.798730    
    ## tste_9_5_ct                0.021110   0.057572   0.367 0.713898    
    ## tste_9_6_ct               -0.009627   0.051377  -0.187 0.851385    
    ## tste_9_7_ct               -0.065391   0.052939  -1.235 0.216883    
    ## tste_9_8_ct                0.081689   0.047024   1.737 0.082500 .  
    ## release                   -0.028154   0.007895  -3.566 0.000370 ***
    ## gap_openness              -0.035091   0.042869  -0.819 0.413136    
    ## income                     0.011431   0.017115   0.668 0.504268    
    ## gap_extraversion          -0.004588   0.025414  -0.181 0.856762    
    ## gap_agreeableness          0.077414   0.033278   2.326 0.020098 *  
    ## gap_emotionstability      -0.031394   0.031233  -1.005 0.314943    
    ## age                       -0.013764   0.004574  -3.009 0.002651 ** 
    ## sex2                      -0.082525   0.072028  -1.146 0.252036    
    ## gap_conscientiousness      0.010451   0.039335   0.266 0.790498    
    ## race4                     -0.199021   0.140673  -1.415 0.157282    
    ## education                  0.002515   0.026430   0.095 0.924197    
    ## race2                      0.076339   0.130750   0.584 0.559382    
    ## star_user                 -0.084136   0.071450  -1.178 0.239112    
    ## star_GS                    0.128911   0.055952   2.304 0.021323 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2109 degrees of freedom
    ## Multiple R-squared:  0.08397,    Adjusted R-squared:  0.07181 
    ## F-statistic: 6.905 on 28 and 2109 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9018 -0.6865  0.2823  1.0438  2.5748 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.195063  15.861590   3.921  9.1e-05 ***
    ## real_extraversion_ct       0.046884   0.024851   1.887 0.059355 .  
    ## real_agreeableness_ct      0.015756   0.031136   0.506 0.612894    
    ## real_conscientiousness_ct  0.086652   0.034540   2.509 0.012190 *  
    ## real_emotionstability_ct  -0.028394   0.034218  -0.830 0.406746    
    ## real_openness_ct           0.102297   0.033164   3.085 0.002065 ** 
    ## tste_10_0_ct              -0.047497   0.045797  -1.037 0.299799    
    ## tste_10_1_ct               0.190259   0.050613   3.759 0.000175 ***
    ## tste_10_2_ct              -0.198039   0.052333  -3.784 0.000158 ***
    ## tste_10_3_ct              -0.175721   0.054247  -3.239 0.001217 ** 
    ## tste_10_4_ct              -0.058918   0.053246  -1.107 0.268629    
    ## tste_10_5_ct              -0.022532   0.053957  -0.418 0.676295    
    ## tste_10_6_ct              -0.140277   0.052212  -2.687 0.007274 ** 
    ## tste_10_7_ct               0.109197   0.045330   2.409 0.016084 *  
    ## tste_10_8_ct               0.024163   0.059730   0.405 0.685855    
    ## tste_10_9_ct               0.131830   0.050517   2.610 0.009128 ** 
    ## release                   -0.028325   0.007811  -3.626 0.000294 ***
    ## gap_openness              -0.033534   0.042851  -0.783 0.433975    
    ## income                     0.009707   0.017092   0.568 0.570172    
    ## gap_extraversion          -0.004549   0.025418  -0.179 0.857968    
    ## gap_agreeableness          0.075959   0.033309   2.280 0.022680 *  
    ## gap_emotionstability      -0.031884   0.031238  -1.021 0.307527    
    ## age                       -0.013747   0.004576  -3.004 0.002694 ** 
    ## sex2                      -0.080243   0.072075  -1.113 0.265691    
    ## gap_conscientiousness      0.010455   0.039336   0.266 0.790435    
    ## race4                     -0.197771   0.140681  -1.406 0.159928    
    ## education                  0.002452   0.026432   0.093 0.926096    
    ## race2                      0.088030   0.130780   0.673 0.500950    
    ## star_user                 -0.060640   0.070678  -0.858 0.391006    
    ## star_GS                    0.120113   0.055984   2.145 0.032029 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2108 degrees of freedom
    ## Multiple R-squared:  0.0844, Adjusted R-squared:  0.07181 
    ## F-statistic: 6.701 on 29 and 2108 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9075 -0.6944  0.2920  1.0398  2.6442 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.391082  16.084296   4.003 6.46e-05 ***
    ## real_extraversion_ct       0.048654   0.024858   1.957 0.050444 .  
    ## real_agreeableness_ct      0.016238   0.031137   0.522 0.602068    
    ## real_conscientiousness_ct  0.084757   0.034562   2.452 0.014274 *  
    ## real_emotionstability_ct  -0.029021   0.034200  -0.849 0.396212    
    ## real_openness_ct           0.099211   0.033170   2.991 0.002813 ** 
    ## tste_11_0_ct              -0.050786   0.056185  -0.904 0.366146    
    ## tste_11_1_ct               0.060287   0.052399   1.151 0.250057    
    ## tste_11_2_ct              -0.081619   0.046662  -1.749 0.080407 .  
    ## tste_11_3_ct               0.005016   0.056987   0.088 0.929867    
    ## tste_11_4_ct              -0.167227   0.057622  -2.902 0.003745 ** 
    ## tste_11_5_ct              -0.103379   0.050273  -2.056 0.039870 *  
    ## tste_11_6_ct               0.160777   0.049181   3.269 0.001096 ** 
    ## tste_11_7_ct               0.098478   0.056584   1.740 0.081941 .  
    ## tste_11_8_ct              -0.202802   0.042150  -4.811 1.60e-06 ***
    ## tste_11_9_ct               0.172862   0.049983   3.458 0.000554 ***
    ## tste_11_10_ct              0.068563   0.054722   1.253 0.210364    
    ## release                   -0.029474   0.007920  -3.722 0.000203 ***
    ## gap_openness              -0.037585   0.042883  -0.876 0.380880    
    ## income                     0.010133   0.017097   0.593 0.553467    
    ## gap_extraversion          -0.003117   0.025408  -0.123 0.902368    
    ## gap_agreeableness          0.076299   0.033283   2.292 0.021978 *  
    ## gap_emotionstability      -0.032747   0.031230  -1.049 0.294497    
    ## age                       -0.013649   0.004573  -2.984 0.002873 ** 
    ## sex2                      -0.078591   0.072113  -1.090 0.275911    
    ## gap_conscientiousness      0.008220   0.039312   0.209 0.834388    
    ## race4                     -0.194788   0.140629  -1.385 0.166164    
    ## education                  0.003049   0.026420   0.115 0.908145    
    ## race2                      0.089694   0.130739   0.686 0.492754    
    ## star_user                 -0.085246   0.072145  -1.182 0.237502    
    ## star_GS                    0.157070   0.057447   2.734 0.006306 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.446 on 2107 degrees of freedom
    ## Multiple R-squared:  0.08586,    Adjusted R-squared:  0.07285 
    ## F-statistic: 6.597 on 30 and 2107 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.898 -0.701  0.305  1.036  2.591 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.131904  16.437776   3.171 0.001539 ** 
    ## real_extraversion_ct       0.047521   0.024889   1.909 0.056359 .  
    ## real_agreeableness_ct      0.017521   0.031140   0.563 0.573733    
    ## real_conscientiousness_ct  0.082843   0.034595   2.395 0.016724 *  
    ## real_emotionstability_ct  -0.029067   0.034230  -0.849 0.395883    
    ## real_openness_ct           0.101355   0.033202   3.053 0.002296 ** 
    ## tste_12_0_ct               0.153791   0.060327   2.549 0.010864 *  
    ## tste_12_1_ct               0.068575   0.050956   1.346 0.178515    
    ## tste_12_2_ct               0.057739   0.048589   1.188 0.234841    
    ## tste_12_3_ct              -0.005225   0.054997  -0.095 0.924318    
    ## tste_12_4_ct              -0.072466   0.051077  -1.419 0.156118    
    ## tste_12_5_ct               0.120826   0.054738   2.207 0.027397 *  
    ## tste_12_6_ct              -0.052347   0.055871  -0.937 0.348901    
    ## tste_12_7_ct               0.045020   0.053355   0.844 0.398884    
    ## tste_12_8_ct               0.165963   0.049187   3.374 0.000754 ***
    ## tste_12_9_ct              -0.054421   0.051373  -1.059 0.289569    
    ## tste_12_10_ct              0.193628   0.045295   4.275 2.00e-05 ***
    ## tste_12_11_ct             -0.202074   0.048438  -4.172 3.14e-05 ***
    ## release                   -0.023652   0.008078  -2.928 0.003448 ** 
    ## gap_openness              -0.038393   0.042897  -0.895 0.370893    
    ## income                     0.009635   0.017104   0.563 0.573284    
    ## gap_extraversion          -0.004976   0.025446  -0.196 0.844979    
    ## gap_agreeableness          0.078660   0.033287   2.363 0.018216 *  
    ## gap_emotionstability      -0.034460   0.031233  -1.103 0.270015    
    ## age                       -0.013504   0.004581  -2.948 0.003239 ** 
    ## sex2                      -0.076387   0.072151  -1.059 0.289856    
    ## gap_conscientiousness      0.006045   0.039363   0.154 0.877971    
    ## race4                     -0.200603   0.140751  -1.425 0.154237    
    ## education                  0.003192   0.026435   0.121 0.903903    
    ## race2                      0.089209   0.130827   0.682 0.495384    
    ## star_user                 -0.048653   0.072197  -0.674 0.500454    
    ## star_GS                    0.188465   0.060480   3.116 0.001857 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2106 degrees of freedom
    ## Multiple R-squared:  0.08516,    Adjusted R-squared:  0.0717 
    ## F-statistic: 6.324 on 31 and 2106 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1238 -0.6730  0.3028  1.0520  2.7678 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               50.730965  16.846671   3.011 0.002632 ** 
    ## real_extraversion_ct       0.044342   0.024902   1.781 0.075113 .  
    ## real_agreeableness_ct      0.022187   0.031216   0.711 0.477310    
    ## real_conscientiousness_ct  0.089260   0.034590   2.581 0.009932 ** 
    ## real_emotionstability_ct  -0.028507   0.034275  -0.832 0.405661    
    ## real_openness_ct           0.102655   0.033231   3.089 0.002034 ** 
    ## tste_13_0_ct               0.011891   0.050487   0.236 0.813826    
    ## tste_13_1_ct              -0.091902   0.046728  -1.967 0.049345 *  
    ## tste_13_2_ct               0.198458   0.052153   3.805 0.000146 ***
    ## tste_13_3_ct               0.057938   0.050258   1.153 0.249120    
    ## tste_13_4_ct              -0.104536   0.053001  -1.972 0.048701 *  
    ## tste_13_5_ct               0.127909   0.052002   2.460 0.013985 *  
    ## tste_13_6_ct              -0.001410   0.055874  -0.025 0.979863    
    ## tste_13_7_ct              -0.069011   0.050800  -1.358 0.174457    
    ## tste_13_8_ct               0.084749   0.046702   1.815 0.069717 .  
    ## tste_13_9_ct               0.149089   0.056101   2.658 0.007931 ** 
    ## tste_13_10_ct              0.172218   0.048642   3.541 0.000408 ***
    ## tste_13_11_ct              0.098614   0.054269   1.817 0.069339 .  
    ## tste_13_12_ct              0.060417   0.051902   1.164 0.244532    
    ## release                   -0.022758   0.008274  -2.751 0.006000 ** 
    ## gap_openness              -0.035058   0.043013  -0.815 0.415131    
    ## income                     0.007732   0.017109   0.452 0.651389    
    ## gap_extraversion          -0.008874   0.025487  -0.348 0.727749    
    ## gap_agreeableness          0.083700   0.033391   2.507 0.012261 *  
    ## gap_emotionstability      -0.033412   0.031298  -1.068 0.285849    
    ## age                       -0.013487   0.004588  -2.939 0.003323 ** 
    ## sex2                      -0.087343   0.072237  -1.209 0.226755    
    ## gap_conscientiousness      0.007949   0.039402   0.202 0.840132    
    ## race4                     -0.193157   0.140910  -1.371 0.170591    
    ## education                  0.005295   0.026464   0.200 0.841445    
    ## race2                      0.079627   0.130893   0.608 0.543033    
    ## star_user                 -0.088693   0.071603  -1.239 0.215607    
    ## star_GS                    0.182279   0.059512   3.063 0.002220 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2105 degrees of freedom
    ## Multiple R-squared:  0.0837, Adjusted R-squared:  0.06977 
    ## F-statistic: 6.009 on 32 and 2105 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1139 -0.6870  0.3111  1.0511  2.7237 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               45.723605  16.766073   2.727 0.006441 ** 
    ## real_extraversion_ct       0.050031   0.024923   2.007 0.044832 *  
    ## real_agreeableness_ct      0.021154   0.031199   0.678 0.497832    
    ## real_conscientiousness_ct  0.085524   0.034596   2.472 0.013513 *  
    ## real_emotionstability_ct  -0.029773   0.034240  -0.870 0.384652    
    ## real_openness_ct           0.102099   0.033203   3.075 0.002132 ** 
    ## tste_14_0_ct              -0.103493   0.051399  -2.014 0.044186 *  
    ## tste_14_1_ct               0.005460   0.051674   0.106 0.915864    
    ## tste_14_2_ct               0.052315   0.047597   1.099 0.271843    
    ## tste_14_3_ct               0.117835   0.046428   2.538 0.011220 *  
    ## tste_14_4_ct               0.021697   0.043442   0.499 0.617512    
    ## tste_14_5_ct              -0.034370   0.054029  -0.636 0.524755    
    ## tste_14_6_ct               0.025942   0.050849   0.510 0.609979    
    ## tste_14_7_ct              -0.259019   0.059413  -4.360 1.37e-05 ***
    ## tste_14_8_ct               0.159871   0.045375   3.523 0.000435 ***
    ## tste_14_9_ct              -0.020246   0.051485  -0.393 0.694177    
    ## tste_14_10_ct              0.107625   0.049362   2.180 0.029345 *  
    ## tste_14_11_ct             -0.058019   0.049063  -1.183 0.237124    
    ## tste_14_12_ct              0.083845   0.051633   1.624 0.104554    
    ## tste_14_13_ct              0.114662   0.052158   2.198 0.028032 *  
    ## release                   -0.020427   0.008244  -2.478 0.013302 *  
    ## gap_openness              -0.034412   0.042979  -0.801 0.423406    
    ## income                     0.008464   0.017094   0.495 0.620560    
    ## gap_extraversion          -0.003799   0.025446  -0.149 0.881321    
    ## gap_agreeableness          0.081373   0.033320   2.442 0.014680 *  
    ## gap_emotionstability      -0.034423   0.031255  -1.101 0.270865    
    ## age                       -0.013048   0.004585  -2.846 0.004473 ** 
    ## sex2                      -0.080600   0.072252  -1.116 0.264752    
    ## gap_conscientiousness      0.007998   0.039368   0.203 0.839038    
    ## race4                     -0.188693   0.140780  -1.340 0.180279    
    ## education                  0.002126   0.026467   0.080 0.935983    
    ## race2                      0.079376   0.130797   0.607 0.544007    
    ## star_user                 -0.041347   0.071989  -0.574 0.565798    
    ## star_GS                    0.172224   0.059628   2.888 0.003913 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.447 on 2104 degrees of freedom
    ## Multiple R-squared:  0.08581,    Adjusted R-squared:  0.07147 
    ## F-statistic: 5.985 on 33 and 2104 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1042 -0.6869  0.3129  1.0407  2.7356 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.932391  17.742192   2.983 0.002883 ** 
    ## real_extraversion_ct       0.048931   0.024942   1.962 0.049914 *  
    ## real_agreeableness_ct      0.019148   0.031220   0.613 0.539742    
    ## real_conscientiousness_ct  0.086879   0.034618   2.510 0.012158 *  
    ## real_emotionstability_ct  -0.026595   0.034312  -0.775 0.438372    
    ## real_openness_ct           0.103834   0.033236   3.124 0.001807 ** 
    ## tste_15_0_ct              -0.063315   0.048043  -1.318 0.187684    
    ## tste_15_1_ct              -0.118855   0.053763  -2.211 0.027161 *  
    ## tste_15_2_ct               0.158705   0.048565   3.268 0.001101 ** 
    ## tste_15_3_ct               0.092236   0.052550   1.755 0.079370 .  
    ## tste_15_4_ct               0.015208   0.047044   0.323 0.746531    
    ## tste_15_5_ct              -0.069162   0.053723  -1.287 0.198108    
    ## tste_15_6_ct              -0.112243   0.046618  -2.408 0.016138 *  
    ## tste_15_7_ct              -0.065300   0.051672  -1.264 0.206456    
    ## tste_15_8_ct              -0.125829   0.053977  -2.331 0.019839 *  
    ## tste_15_9_ct               0.032511   0.051090   0.636 0.524619    
    ## tste_15_10_ct              0.050255   0.053244   0.944 0.345349    
    ## tste_15_11_ct              0.188414   0.054344   3.467 0.000537 ***
    ## tste_15_12_ct             -0.047722   0.038160  -1.251 0.211225    
    ## tste_15_13_ct              0.031851   0.044434   0.717 0.473561    
    ## tste_15_14_ct              0.051871   0.046954   1.105 0.269410    
    ## release                   -0.023764   0.008714  -2.727 0.006442 ** 
    ## gap_openness              -0.033193   0.043023  -0.772 0.440486    
    ## income                     0.008408   0.017123   0.491 0.623474    
    ## gap_extraversion          -0.004065   0.025476  -0.160 0.873228    
    ## gap_agreeableness          0.081944   0.033366   2.456 0.014135 *  
    ## gap_emotionstability      -0.033221   0.031295  -1.062 0.288555    
    ## age                       -0.013175   0.004586  -2.873 0.004113 ** 
    ## sex2                      -0.083808   0.072317  -1.159 0.246631    
    ## gap_conscientiousness      0.006971   0.039415   0.177 0.859635    
    ## race4                     -0.188693   0.140905  -1.339 0.180668    
    ## education                  0.002331   0.026488   0.088 0.929877    
    ## race2                      0.071678   0.130912   0.548 0.584073    
    ## star_user                 -0.072814   0.075671  -0.962 0.336036    
    ## star_GS                    0.143514   0.060330   2.379 0.017458 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2103 degrees of freedom
    ## Multiple R-squared:  0.08463,    Adjusted R-squared:  0.06983 
    ## F-statistic: 5.719 on 34 and 2103 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9453 -0.6821  0.2853  1.0411  2.7272 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               35.147619  17.719894   1.984 0.047440 *  
    ## real_extraversion_ct       0.047187   0.024837   1.900 0.057581 .  
    ## real_agreeableness_ct      0.019225   0.031088   0.618 0.536381    
    ## real_conscientiousness_ct  0.087985   0.034465   2.553 0.010755 *  
    ## real_emotionstability_ct  -0.021627   0.034172  -0.633 0.526876    
    ## real_openness_ct           0.104021   0.033100   3.143 0.001698 ** 
    ## tste_16_0_ct               0.177775   0.048617   3.657 0.000262 ***
    ## tste_16_1_ct              -0.101268   0.049867  -2.031 0.042406 *  
    ## tste_16_2_ct              -0.054677   0.044933  -1.217 0.223794    
    ## tste_16_3_ct              -0.160887   0.050518  -3.185 0.001470 ** 
    ## tste_16_4_ct              -0.170749   0.041234  -4.141 3.59e-05 ***
    ## tste_16_5_ct               0.105803   0.048245   2.193 0.028414 *  
    ## tste_16_6_ct               0.100181   0.044109   2.271 0.023235 *  
    ## tste_16_7_ct               0.074744   0.044628   1.675 0.094118 .  
    ## tste_16_8_ct              -0.061957   0.052011  -1.191 0.233701    
    ## tste_16_9_ct               0.159987   0.044796   3.571 0.000363 ***
    ## tste_16_10_ct             -0.077274   0.044038  -1.755 0.079458 .  
    ## tste_16_11_ct              0.156386   0.050308   3.109 0.001905 ** 
    ## tste_16_12_ct              0.078169   0.046161   1.693 0.090525 .  
    ## tste_16_13_ct              0.054389   0.049716   1.094 0.274080    
    ## tste_16_14_ct              0.115315   0.046261   2.493 0.012754 *  
    ## tste_16_15_ct             -0.015011   0.050199  -0.299 0.764947    
    ## release                   -0.014974   0.008706  -1.720 0.085580 .  
    ## gap_openness              -0.032608   0.042836  -0.761 0.446602    
    ## income                     0.012653   0.017079   0.741 0.458876    
    ## gap_extraversion          -0.010610   0.025395  -0.418 0.676136    
    ## gap_agreeableness          0.088340   0.033252   2.657 0.007952 ** 
    ## gap_emotionstability      -0.034087   0.031171  -1.094 0.274275    
    ## age                       -0.011931   0.004578  -2.606 0.009229 ** 
    ## sex2                      -0.085253   0.071943  -1.185 0.236150    
    ## gap_conscientiousness      0.010361   0.039283   0.264 0.791995    
    ## race4                     -0.183372   0.140374  -1.306 0.191592    
    ## education                  0.001141   0.026370   0.043 0.965498    
    ## race2                      0.094755   0.130530   0.726 0.467965    
    ## star_user                 -0.070010   0.072416  -0.967 0.333769    
    ## star_GS                    0.151135   0.058536   2.582 0.009892 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2102 degrees of freedom
    ## Multiple R-squared:  0.09272,    Adjusted R-squared:  0.07761 
    ## F-statistic: 6.137 on 35 and 2102 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1612 -0.6967  0.2835  1.0378  2.7707 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               36.897732  18.274556   2.019  0.04361 *  
    ## real_extraversion_ct       0.046074   0.024809   1.857  0.06344 .  
    ## real_agreeableness_ct      0.022816   0.031076   0.734  0.46291    
    ## real_conscientiousness_ct  0.087495   0.034438   2.541  0.01114 *  
    ## real_emotionstability_ct  -0.022005   0.034129  -0.645  0.51916    
    ## real_openness_ct           0.105977   0.033073   3.204  0.00137 ** 
    ## tste_17_0_ct               0.314805   0.048966   6.429 1.58e-10 ***
    ## tste_17_1_ct               0.065331   0.041972   1.557  0.11973    
    ## tste_17_2_ct              -0.074623   0.049957  -1.494  0.13539    
    ## tste_17_3_ct               0.051092   0.047752   1.070  0.28477    
    ## tste_17_4_ct               0.090624   0.050173   1.806  0.07102 .  
    ## tste_17_5_ct               0.022808   0.046783   0.488  0.62594    
    ## tste_17_6_ct              -0.056196   0.049201  -1.142  0.25351    
    ## tste_17_7_ct               0.092025   0.043169   2.132  0.03315 *  
    ## tste_17_8_ct              -0.065721   0.047950  -1.371  0.17064    
    ## tste_17_9_ct              -0.105404   0.046220  -2.281  0.02268 *  
    ## tste_17_10_ct              0.092762   0.042490   2.183  0.02914 *  
    ## tste_17_11_ct              0.038253   0.046844   0.817  0.41425    
    ## tste_17_12_ct             -0.169330   0.042849  -3.952 8.01e-05 ***
    ## tste_17_13_ct              0.086715   0.049157   1.764  0.07787 .  
    ## tste_17_14_ct              0.029644   0.043392   0.683  0.49458    
    ## tste_17_15_ct             -0.034863   0.056003  -0.623  0.53366    
    ## tste_17_16_ct             -0.049485   0.042882  -1.154  0.24865    
    ## release                   -0.015920   0.008953  -1.778  0.07553 .  
    ## gap_openness              -0.028058   0.042787  -0.656  0.51205    
    ## income                     0.012779   0.017056   0.749  0.45380    
    ## gap_extraversion          -0.010525   0.025371  -0.415  0.67829    
    ## gap_agreeableness          0.086683   0.033250   2.607  0.00920 ** 
    ## gap_emotionstability      -0.030772   0.031161  -0.988  0.32350    
    ## age                       -0.011990   0.004579  -2.618  0.00890 ** 
    ## sex2                      -0.089685   0.071866  -1.248  0.21219    
    ## gap_conscientiousness      0.008389   0.039253   0.214  0.83079    
    ## race4                     -0.192676   0.140288  -1.373  0.16976    
    ## education                  0.002655   0.026347   0.101  0.91976    
    ## race2                      0.083343   0.130414   0.639  0.52285    
    ## star_user                 -0.076855   0.076728  -1.002  0.31662    
    ## star_GS                    0.175622   0.060311   2.912  0.00363 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2101 degrees of freedom
    ## Multiple R-squared:  0.09484,    Adjusted R-squared:  0.07933 
    ## F-statistic: 6.115 on 36 and 2101 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0711 -0.6824  0.2913  1.0511  2.5914 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.808903  18.061328   2.592  0.00962 ** 
    ## real_extraversion_ct       0.045300   0.024835   1.824  0.06829 .  
    ## real_agreeableness_ct      0.023082   0.031057   0.743  0.45744    
    ## real_conscientiousness_ct  0.084408   0.034444   2.451  0.01434 *  
    ## real_emotionstability_ct  -0.025539   0.034151  -0.748  0.45465    
    ## real_openness_ct           0.105404   0.033071   3.187  0.00146 ** 
    ## tste_18_0_ct              -0.126348   0.046160  -2.737  0.00625 ** 
    ## tste_18_1_ct               0.155745   0.048011   3.244  0.00120 ** 
    ## tste_18_2_ct              -0.015902   0.043623  -0.365  0.71550    
    ## tste_18_3_ct              -0.016928   0.046861  -0.361  0.71797    
    ## tste_18_4_ct               0.075896   0.051139   1.484  0.13793    
    ## tste_18_5_ct              -0.027654   0.048058  -0.575  0.56506    
    ## tste_18_6_ct              -0.003760   0.050367  -0.075  0.94050    
    ## tste_18_7_ct              -0.054930   0.043237  -1.270  0.20407    
    ## tste_18_8_ct               0.080079   0.047055   1.702  0.08894 .  
    ## tste_18_9_ct              -0.137937   0.047797  -2.886  0.00394 ** 
    ## tste_18_10_ct              0.087031   0.045895   1.896  0.05806 .  
    ## tste_18_11_ct              0.063118   0.045141   1.398  0.16219    
    ## tste_18_12_ct             -0.079615   0.047522  -1.675  0.09402 .  
    ## tste_18_13_ct             -0.033253   0.046785  -0.711  0.47731    
    ## tste_18_14_ct              0.050808   0.050857   0.999  0.31790    
    ## tste_18_15_ct              0.163233   0.058538   2.788  0.00534 ** 
    ## tste_18_16_ct             -0.295025   0.051394  -5.740 1.08e-08 ***
    ## tste_18_17_ct              0.046545   0.045699   1.019  0.30855    
    ## release                   -0.020494   0.008865  -2.312  0.02088 *  
    ## gap_openness              -0.028102   0.042819  -0.656  0.51171    
    ## income                     0.012055   0.017062   0.707  0.47994    
    ## gap_extraversion          -0.008145   0.025387  -0.321  0.74838    
    ## gap_agreeableness          0.085959   0.033237   2.586  0.00977 ** 
    ## gap_emotionstability      -0.031696   0.031174  -1.017  0.30940    
    ## age                       -0.011935   0.004581  -2.605  0.00925 ** 
    ## sex2                      -0.083326   0.071937  -1.158  0.24686    
    ## gap_conscientiousness      0.004017   0.039265   0.102  0.91852    
    ## race4                     -0.189153   0.140295  -1.348  0.17772    
    ## education                  0.002461   0.026351   0.093  0.92558    
    ## race2                      0.077822   0.130490   0.596  0.55098    
    ## star_user                 -0.081644   0.077149  -1.058  0.29006    
    ## star_GS                    0.093272   0.061090   1.527  0.12697    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2100 degrees of freedom
    ## Multiple R-squared:  0.09499,    Adjusted R-squared:  0.07905 
    ## F-statistic: 5.957 on 37 and 2100 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0906 -0.6768  0.2886  1.0625  2.6547 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               38.984611  18.502965   2.107 0.035241 *  
    ## real_extraversion_ct       0.046608   0.024901   1.872 0.061386 .  
    ## real_agreeableness_ct      0.023089   0.031168   0.741 0.458896    
    ## real_conscientiousness_ct  0.087446   0.034563   2.530 0.011478 *  
    ## real_emotionstability_ct  -0.020897   0.034292  -0.609 0.542330    
    ## real_openness_ct           0.109553   0.033149   3.305 0.000966 ***
    ## tste_19_0_ct               0.003474   0.048293   0.072 0.942661    
    ## tste_19_1_ct               0.063144   0.050327   1.255 0.209737    
    ## tste_19_2_ct               0.024392   0.048980   0.498 0.618537    
    ## tste_19_3_ct              -0.116759   0.046485  -2.512 0.012087 *  
    ## tste_19_4_ct              -0.109623   0.049011  -2.237 0.025411 *  
    ## tste_19_5_ct              -0.035827   0.053573  -0.669 0.503722    
    ## tste_19_6_ct               0.081616   0.047789   1.708 0.087814 .  
    ## tste_19_7_ct               0.111999   0.046886   2.389 0.016995 *  
    ## tste_19_8_ct               0.037670   0.046858   0.804 0.421525    
    ## tste_19_9_ct              -0.075004   0.050226  -1.493 0.135504    
    ## tste_19_10_ct              0.129607   0.047892   2.706 0.006861 ** 
    ## tste_19_11_ct              0.089879   0.044893   2.002 0.045405 *  
    ## tste_19_12_ct              0.228441   0.045632   5.006 6.02e-07 ***
    ## tste_19_13_ct             -0.012129   0.043264  -0.280 0.779238    
    ## tste_19_14_ct              0.175900   0.044998   3.909 9.56e-05 ***
    ## tste_19_15_ct              0.002336   0.044161   0.053 0.957812    
    ## tste_19_16_ct              0.024523   0.046871   0.523 0.600898    
    ## tste_19_17_ct              0.055599   0.053025   1.049 0.294515    
    ## tste_19_18_ct              0.002615   0.041034   0.064 0.949198    
    ## release                   -0.016912   0.009083  -1.862 0.062752 .  
    ## gap_openness              -0.024837   0.042906  -0.579 0.562732    
    ## income                     0.013099   0.017099   0.766 0.443723    
    ## gap_extraversion          -0.010006   0.025452  -0.393 0.694253    
    ## gap_agreeableness          0.085812   0.033325   2.575 0.010092 *  
    ## gap_emotionstability      -0.030194   0.031246  -0.966 0.333998    
    ## age                       -0.011869   0.004593  -2.584 0.009828 ** 
    ## sex2                      -0.086056   0.072128  -1.193 0.232963    
    ## gap_conscientiousness      0.007526   0.039364   0.191 0.848397    
    ## race4                     -0.195368   0.140586  -1.390 0.164777    
    ## education                  0.002709   0.026412   0.103 0.918302    
    ## race2                      0.081491   0.130822   0.623 0.533410    
    ## star_user                 -0.032522   0.074702  -0.435 0.663349    
    ## star_GS                    0.118739   0.060452   1.964 0.049640 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.444 on 2099 degrees of freedom
    ## Multiple R-squared:  0.09162,    Adjusted R-squared:  0.07517 
    ## F-statistic: 5.571 on 38 and 2099 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0289 -0.6870  0.2805  1.0312  2.5325 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               36.403857  19.316920   1.885 0.059628 .  
    ## real_extraversion_ct       0.048002   0.024831   1.933 0.053357 .  
    ## real_agreeableness_ct      0.021992   0.031115   0.707 0.479773    
    ## real_conscientiousness_ct  0.086138   0.034469   2.499 0.012530 *  
    ## real_emotionstability_ct  -0.018855   0.034208  -0.551 0.581556    
    ## real_openness_ct           0.108859   0.033068   3.292 0.001011 ** 
    ## tste_20_0_ct               0.082502   0.053633   1.538 0.124133    
    ## tste_20_1_ct               0.083748   0.046562   1.799 0.072222 .  
    ## tste_20_2_ct               0.090511   0.042428   2.133 0.033016 *  
    ## tste_20_3_ct               0.154281   0.048736   3.166 0.001570 ** 
    ## tste_20_4_ct               0.033862   0.044370   0.763 0.445450    
    ## tste_20_5_ct              -0.071534   0.043839  -1.632 0.102881    
    ## tste_20_6_ct               0.026499   0.049569   0.535 0.592993    
    ## tste_20_7_ct               0.187831   0.054168   3.468 0.000536 ***
    ## tste_20_8_ct              -0.016664   0.053753  -0.310 0.756583    
    ## tste_20_9_ct               0.101024   0.044228   2.284 0.022461 *  
    ## tste_20_10_ct              0.062719   0.048038   1.306 0.191829    
    ## tste_20_11_ct             -0.126939   0.045605  -2.783 0.005427 ** 
    ## tste_20_12_ct              0.122982   0.046061   2.670 0.007645 ** 
    ## tste_20_13_ct              0.113382   0.050330   2.253 0.024376 *  
    ## tste_20_14_ct             -0.098014   0.041097  -2.385 0.017169 *  
    ## tste_20_15_ct              0.113203   0.053295   2.124 0.033779 *  
    ## tste_20_16_ct             -0.097733   0.046206  -2.115 0.034535 *  
    ## tste_20_17_ct              0.034847   0.044994   0.774 0.438728    
    ## tste_20_18_ct              0.138972   0.042626   3.260 0.001131 ** 
    ## tste_20_19_ct             -0.039555   0.048656  -0.813 0.416332    
    ## release                   -0.015545   0.009483  -1.639 0.101320    
    ## gap_openness              -0.022975   0.042811  -0.537 0.591554    
    ## income                     0.012332   0.017063   0.723 0.469904    
    ## gap_extraversion          -0.008427   0.025372  -0.332 0.739823    
    ## gap_agreeableness          0.084230   0.033257   2.533 0.011392 *  
    ## gap_emotionstability      -0.028874   0.031176  -0.926 0.354473    
    ## age                       -0.011633   0.004584  -2.538 0.011221 *  
    ## sex2                      -0.078724   0.072010  -1.093 0.274414    
    ## gap_conscientiousness      0.004555   0.039279   0.116 0.907686    
    ## race4                     -0.179034   0.140340  -1.276 0.202197    
    ## education                  0.003439   0.026354   0.130 0.896197    
    ## race2                      0.098222   0.130425   0.753 0.451478    
    ## star_user                 -0.013873   0.078027  -0.178 0.858895    
    ## star_GS                    0.078865   0.061361   1.285 0.198847    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2098 degrees of freedom
    ## Multiple R-squared:  0.09614,    Adjusted R-squared:  0.07934 
    ## F-statistic: 5.722 on 39 and 2098 DF,  p-value: < 2.2e-16

### preference ~ tste + real + gap + gap\*tste

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 96:114)$model_lm_1) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0060 -0.6790  0.3158  1.0780  2.5091 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       65.7944645 14.2208266   4.627 3.94e-06 ***
    ## real_extraversion_ct               0.0486829  0.0252480   1.928  0.05397 .  
    ## real_agreeableness_ct              0.0135999  0.0314877   0.432  0.66585    
    ## real_conscientiousness_ct          0.0831015  0.0350327   2.372  0.01778 *  
    ## real_emotionstability_ct          -0.0278057  0.0347015  -0.801  0.42306    
    ## real_openness_ct                   0.1081237  0.0335451   3.223  0.00129 ** 
    ## tste_2_0_ct                        0.1646503  0.0507141   3.247  0.00119 ** 
    ## tste_2_1_ct                       -0.0400901  0.0503442  -0.796  0.42594    
    ## release                           -0.0299646  0.0069945  -4.284 1.92e-05 ***
    ## gap_openness                      -0.0294233  0.0434194  -0.678  0.49807    
    ## income                             0.0051963  0.0173229   0.300  0.76423    
    ## gap_extraversion                  -0.0054700  0.0258285  -0.212  0.83230    
    ## gap_agreeableness                  0.0773700  0.0337474   2.293  0.02197 *  
    ## gap_emotionstability              -0.0404436  0.0317601  -1.273  0.20301    
    ## age                               -0.0136743  0.0046326  -2.952  0.00319 ** 
    ## sex2                              -0.0922604  0.0729503  -1.265  0.20612    
    ## gap_conscientiousness              0.0003295  0.0399544   0.008  0.99342    
    ## race4                             -0.2305198  0.1423796  -1.619  0.10559    
    ## education                          0.0055115  0.0267497   0.206  0.83678    
    ## race2                              0.1029940  0.1323687   0.778  0.43661    
    ## star_user                          0.0063412  0.0580874   0.109  0.91308    
    ## star_GS                            0.0172847  0.0481531   0.359  0.71967    
    ## gap_extraversion.tste_2_0_ct      -0.0244651  0.0246342  -0.993  0.32076    
    ## gap_agreeableness.tste_2_0_ct     -0.0627639  0.0343938  -1.825  0.06816 .  
    ## gap_conscientiousness.tste_2_0_ct  0.0070583  0.0392961   0.180  0.85747    
    ## gap_emotionstability.tste_2_0_ct   0.0222340  0.0290945   0.764  0.44483    
    ## gap_openness.tste_2_0_ct          -0.0242476  0.0389304  -0.623  0.53345    
    ## gap_extraversion.tste_2_1_ct      -0.0262660  0.0252024  -1.042  0.29744    
    ## gap_agreeableness.tste_2_1_ct     -0.0336668  0.0334535  -1.006  0.31435    
    ## gap_conscientiousness.tste_2_1_ct  0.0190782  0.0386319   0.494  0.62147    
    ## gap_openness.tste_2_1_ct           0.0183259  0.0385794   0.475  0.63482    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2107 degrees of freedom
    ## Multiple R-squared:  0.06287,    Adjusted R-squared:  0.04953 
    ## F-statistic: 4.712 on 30 and 2107 DF,  p-value: 9.622e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9071 -0.6832  0.3041  1.0627  2.5377 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       62.142038  14.133443   4.397 1.15e-05 ***
    ## real_extraversion_ct               0.049154   0.025028   1.964 0.049669 *  
    ## real_agreeableness_ct              0.013465   0.031195   0.432 0.666051    
    ## real_conscientiousness_ct          0.085369   0.034714   2.459 0.014004 *  
    ## real_emotionstability_ct          -0.024116   0.034368  -0.702 0.482936    
    ## real_openness_ct                   0.104941   0.033245   3.157 0.001619 ** 
    ## tste_3_0_ct                       -0.112284   0.053720  -2.090 0.036722 *  
    ## tste_3_1_ct                        0.164960   0.044242   3.729 0.000198 ***
    ## tste_3_2_ct                        0.183988   0.061014   3.016 0.002596 ** 
    ## release                           -0.028285   0.006951  -4.069 4.89e-05 ***
    ## gap_openness                      -0.031693   0.043056  -0.736 0.461748    
    ## income                             0.006136   0.017175   0.357 0.720944    
    ## gap_extraversion                  -0.011551   0.025646  -0.450 0.652473    
    ## gap_agreeableness                  0.080186   0.033466   2.396 0.016658 *  
    ## gap_emotionstability              -0.038712   0.031475  -1.230 0.218857    
    ## age                               -0.013027   0.004592  -2.837 0.004597 ** 
    ## sex2                              -0.082037   0.072263  -1.135 0.256396    
    ## gap_conscientiousness              0.003806   0.039607   0.096 0.923450    
    ## race4                             -0.204939   0.141128  -1.452 0.146609    
    ## education                          0.004819   0.026513   0.182 0.855788    
    ## race2                              0.122620   0.131247   0.934 0.350272    
    ## star_user                          0.004817   0.057652   0.084 0.933415    
    ## gap_extraversion.tste_3_0_ct       0.040551   0.026430   1.534 0.125117    
    ## gap_agreeableness.tste_3_0_ct      0.039946   0.037274   1.072 0.283991    
    ## gap_conscientiousness.tste_3_0_ct  0.020661   0.042599   0.485 0.627722    
    ## gap_emotionstability.tste_3_0_ct   0.072550   0.033902   2.140 0.032467 *  
    ## gap_openness.tste_3_0_ct           0.039181   0.042255   0.927 0.353900    
    ## star_GS                            0.049012   0.048474   1.011 0.312081    
    ## gap_extraversion.tste_3_1_ct       0.012529   0.021678   0.578 0.563360    
    ## gap_agreeableness.tste_3_1_ct     -0.019579   0.029509  -0.663 0.507089    
    ## gap_conscientiousness.tste_3_1_ct  0.018871   0.033581   0.562 0.574212    
    ## gap_emotionstability.tste_3_1_ct   0.089912   0.026435   3.401 0.000683 ***
    ## gap_openness.tste_3_1_ct           0.011902   0.034103   0.349 0.727135    
    ## gap_extraversion.tste_3_2_ct       0.010269   0.031895   0.322 0.747510    
    ## gap_agreeableness.tste_3_2_ct      0.051937   0.043030   1.207 0.227568    
    ## gap_openness.tste_3_2_ct           0.064485   0.048361   1.333 0.182544    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.45 on 2102 degrees of freedom
    ## Multiple R-squared:  0.08329,    Adjusted R-squared:  0.06803 
    ## F-statistic: 5.457 on 35 and 2102 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0480 -0.6875  0.3084  1.0473  2.6513 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       65.726970  14.498170   4.533 6.13e-06 ***
    ## real_extraversion_ct               0.046661   0.025049   1.863  0.06263 .  
    ## real_agreeableness_ct              0.016372   0.031320   0.523  0.60121    
    ## real_conscientiousness_ct          0.084009   0.034764   2.417  0.01575 *  
    ## real_emotionstability_ct          -0.025509   0.034461  -0.740  0.45923    
    ## real_openness_ct                   0.107017   0.033309   3.213  0.00133 ** 
    ## tste_4_0_ct                        0.191726   0.061017   3.142  0.00170 ** 
    ## tste_4_1_ct                        0.043570   0.069775   0.624  0.53241    
    ## tste_4_2_ct                        0.001860   0.051559   0.036  0.97122    
    ## tste_4_3_ct                       -0.203983   0.048490  -4.207 2.70e-05 ***
    ## release                           -0.029999   0.007127  -4.209 2.67e-05 ***
    ## gap_openness                      -0.029207   0.043119  -0.677  0.49825    
    ## income                             0.004190   0.017192   0.244  0.80749    
    ## gap_extraversion                  -0.009491   0.025622  -0.370  0.71112    
    ## gap_agreeableness                  0.082545   0.033516   2.463  0.01386 *  
    ## gap_emotionstability              -0.041242   0.031563  -1.307  0.19147    
    ## age                               -0.012773   0.004601  -2.776  0.00556 ** 
    ## sex2                              -0.086073   0.072392  -1.189  0.23458    
    ## gap_conscientiousness              0.005830   0.039761   0.147  0.88345    
    ## race4                             -0.203808   0.141327  -1.442  0.14942    
    ## education                          0.005200   0.026580   0.196  0.84492    
    ## race2                              0.117632   0.131485   0.895  0.37108    
    ## star_GS                            0.090315   0.052798   1.711  0.08731 .  
    ## gap_extraversion.tste_4_0_ct       0.024620   0.031366   0.785  0.43259    
    ## gap_agreeableness.tste_4_0_ct      0.058947   0.042356   1.392  0.16416    
    ## gap_conscientiousness.tste_4_0_ct -0.014315   0.047407  -0.302  0.76272    
    ## gap_openness.tste_4_0_ct           0.069031   0.048397   1.426  0.15392    
    ## star_user                         -0.053165   0.064790  -0.821  0.41199    
    ## gap_extraversion.tste_4_1_ct      -0.063545   0.032676  -1.945  0.05194 .  
    ## gap_agreeableness.tste_4_1_ct      0.070556   0.045222   1.560  0.11886    
    ## gap_conscientiousness.tste_4_1_ct  0.056885   0.050894   1.118  0.26381    
    ## gap_openness.tste_4_1_ct           0.067238   0.048588   1.384  0.16656    
    ## gap_extraversion.tste_4_2_ct       0.024091   0.025965   0.928  0.35361    
    ## gap_agreeableness.tste_4_2_ct      0.067624   0.034993   1.933  0.05343 .  
    ## gap_conscientiousness.tste_4_2_ct -0.016301   0.039807  -0.410  0.68221    
    ## gap_openness.tste_4_2_ct           0.007020   0.040130   0.175  0.86114    
    ## gap_extraversion.tste_4_3_ct      -0.013462   0.022569  -0.597  0.55091    
    ## gap_agreeableness.tste_4_3_ct      0.053837   0.030497   1.765  0.07766 .  
    ## gap_conscientiousness.tste_4_3_ct  0.016835   0.036006   0.468  0.64014    
    ## gap_emotionstability.tste_4_3_ct  -0.052554   0.025946  -2.026  0.04294 *  
    ## gap_openness.tste_4_3_ct           0.017577   0.035518   0.495  0.62073    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.451 on 2097 degrees of freedom
    ## Multiple R-squared:  0.08467,    Adjusted R-squared:  0.06721 
    ## F-statistic: 4.849 on 40 and 2097 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1267 -0.6941  0.3079  1.0443  2.7392 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       53.920357  14.783468   3.647 0.000271 ***
    ## real_extraversion_ct               0.050946   0.025058   2.033 0.042167 *  
    ## real_agreeableness_ct              0.014647   0.031305   0.468 0.639913    
    ## real_conscientiousness_ct          0.089937   0.034812   2.584 0.009847 ** 
    ## real_emotionstability_ct          -0.019931   0.034471  -0.578 0.563197    
    ## real_openness_ct                   0.107160   0.033336   3.215 0.001326 ** 
    ## tste_5_0_ct                        0.144665   0.057564   2.513 0.012042 *  
    ## tste_5_1_ct                       -0.175882   0.065377  -2.690 0.007196 ** 
    ## tste_5_2_ct                       -0.084861   0.051002  -1.664 0.096286 .  
    ## tste_5_3_ct                       -0.065848   0.062868  -1.047 0.295035    
    ## tste_5_4_ct                        0.044926   0.063729   0.705 0.480917    
    ## release                           -0.024130   0.007267  -3.320 0.000914 ***
    ## gap_openness                      -0.026218   0.043284  -0.606 0.544760    
    ## gap_extraversion.tste_5_0_ct       0.039688   0.029611   1.340 0.180296    
    ## income                             0.004790   0.017195   0.279 0.780602    
    ## gap_extraversion                  -0.009796   0.025610  -0.383 0.702126    
    ## gap_agreeableness                  0.084377   0.033604   2.511 0.012117 *  
    ## gap_emotionstability              -0.040265   0.031573  -1.275 0.202346    
    ## age                               -0.013183   0.004600  -2.866 0.004202 ** 
    ## sex2                              -0.100615   0.072405  -1.390 0.164793    
    ## gap_conscientiousness              0.012407   0.039878   0.311 0.755748    
    ## race4                             -0.225522   0.141239  -1.597 0.110474    
    ## education                          0.008705   0.026570   0.328 0.743223    
    ## race2                              0.101926   0.131424   0.776 0.438103    
    ## gap_agreeableness.tste_5_0_ct      0.082948   0.041199   2.013 0.044204 *  
    ## gap_openness.tste_5_0_ct           0.036674   0.046158   0.795 0.426981    
    ## star_user                         -0.024903   0.067259  -0.370 0.711223    
    ## gap_extraversion.tste_5_1_ct       0.020678   0.032441   0.637 0.523922    
    ## gap_agreeableness.tste_5_1_ct      0.033222   0.042994   0.773 0.439777    
    ## gap_conscientiousness.tste_5_1_ct -0.154500   0.050446  -3.063 0.002222 ** 
    ## gap_openness.tste_5_1_ct           0.009160   0.049375   0.186 0.852837    
    ## star_GS                            0.064796   0.053665   1.207 0.227408    
    ## gap_extraversion.tste_5_2_ct      -0.005096   0.023888  -0.213 0.831081    
    ## gap_agreeableness.tste_5_2_ct      0.048616   0.033217   1.464 0.143456    
    ## gap_conscientiousness.tste_5_2_ct  0.084968   0.037882   2.243 0.025004 *  
    ## gap_emotionstability.tste_5_2_ct  -0.056482   0.027030  -2.090 0.036774 *  
    ## gap_openness.tste_5_2_ct           0.029765   0.038576   0.772 0.440443    
    ## gap_extraversion.tste_5_3_ct       0.028297   0.030665   0.923 0.356224    
    ## gap_agreeableness.tste_5_3_ct     -0.049902   0.042487  -1.175 0.240317    
    ## gap_conscientiousness.tste_5_3_ct  0.009598   0.049764   0.193 0.847076    
    ## gap_openness.tste_5_3_ct          -0.080633   0.046564  -1.732 0.083481 .  
    ## gap_extraversion.tste_5_4_ct       0.034277   0.028397   1.207 0.227549    
    ## gap_agreeableness.tste_5_4_ct     -0.011723   0.038373  -0.306 0.760008    
    ## gap_conscientiousness.tste_5_4_ct -0.052146   0.041423  -1.259 0.208222    
    ## gap_openness.tste_5_4_ct          -0.008976   0.043310  -0.207 0.835830    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.45 on 2093 degrees of freedom
    ## Multiple R-squared:  0.08729,    Adjusted R-squared:  0.0681 
    ## F-statistic: 4.549 on 44 and 2093 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1725 -0.6720  0.3077  1.0449  3.0428 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       53.666076  14.923664   3.596 0.000331 ***
    ## real_extraversion_ct               0.053107   0.025066   2.119 0.034234 *  
    ## real_agreeableness_ct              0.011092   0.031303   0.354 0.723114    
    ## real_conscientiousness_ct          0.090364   0.034806   2.596 0.009492 ** 
    ## real_emotionstability_ct          -0.017717   0.034469  -0.514 0.607303    
    ## real_openness_ct                   0.106994   0.033358   3.207 0.001359 ** 
    ## tste_6_0_ct                        0.072917   0.064758   1.126 0.260293    
    ## tste_6_1_ct                       -0.099044   0.063400  -1.562 0.118390    
    ## tste_6_2_ct                        0.185330   0.053507   3.464 0.000544 ***
    ## tste_6_3_ct                       -0.016265   0.066729  -0.244 0.807448    
    ## tste_6_4_ct                       -0.006607   0.059140  -0.112 0.911055    
    ## tste_6_5_ct                        0.085105   0.067500   1.261 0.207519    
    ## release                           -0.023943   0.007337  -3.263 0.001119 ** 
    ## gap_openness                      -0.029958   0.043274  -0.692 0.488825    
    ## income                             0.004900   0.017185   0.285 0.775576    
    ## gap_extraversion                  -0.009118   0.025666  -0.355 0.722425    
    ## gap_agreeableness                  0.078793   0.033608   2.345 0.019146 *  
    ## gap_emotionstability              -0.040954   0.031558  -1.298 0.194529    
    ## age                               -0.012894   0.004596  -2.805 0.005072 ** 
    ## sex2                              -0.100985   0.072318  -1.396 0.162743    
    ## gap_conscientiousness              0.016467   0.039870   0.413 0.679636    
    ## race4                             -0.223550   0.141307  -1.582 0.113799    
    ## education                          0.006221   0.026551   0.234 0.814766    
    ## race2                              0.087866   0.131453   0.668 0.503940    
    ## star_user                         -0.045334   0.069637  -0.651 0.515115    
    ## star_GS                            0.070855   0.054645   1.297 0.194892    
    ## gap_extraversion.tste_6_0_ct       0.004144   0.032315   0.128 0.897961    
    ## gap_agreeableness.tste_6_0_ct     -0.008659   0.044240  -0.196 0.844848    
    ## gap_conscientiousness.tste_6_0_ct  0.016389   0.047603   0.344 0.730673    
    ## gap_openness.tste_6_0_ct          -0.030868   0.050071  -0.616 0.537642    
    ## gap_extraversion.tste_6_1_ct       0.031272   0.030198   1.036 0.300522    
    ## gap_agreeableness.tste_6_1_ct     -0.017221   0.041414  -0.416 0.677580    
    ## gap_conscientiousness.tste_6_1_ct -0.041997   0.046736  -0.899 0.368973    
    ## gap_openness.tste_6_1_ct          -0.069129   0.044856  -1.541 0.123434    
    ## gap_extraversion.tste_6_2_ct       0.029373   0.027378   1.073 0.283464    
    ## gap_agreeableness.tste_6_2_ct      0.060987   0.037144   1.642 0.100763    
    ## gap_conscientiousness.tste_6_2_ct -0.028196   0.038986  -0.723 0.469618    
    ## gap_openness.tste_6_2_ct           0.017440   0.042055   0.415 0.678406    
    ## gap_extraversion.tste_6_3_ct      -0.013054   0.032432  -0.403 0.687347    
    ## gap_agreeableness.tste_6_3_ct      0.020882   0.044098   0.474 0.635877    
    ## gap_conscientiousness.tste_6_3_ct  0.187189   0.050780   3.686 0.000233 ***
    ## gap_openness.tste_6_3_ct           0.011054   0.050933   0.217 0.828206    
    ## gap_extraversion.tste_6_4_ct       0.022777   0.029151   0.781 0.434690    
    ## gap_agreeableness.tste_6_4_ct      0.081816   0.039687   2.062 0.039375 *  
    ## gap_emotionstability.tste_6_4_ct  -0.100464   0.030623  -3.281 0.001052 ** 
    ## gap_openness.tste_6_4_ct           0.053165   0.044996   1.182 0.237524    
    ## gap_extraversion.tste_6_5_ct      -0.006184   0.030915  -0.200 0.841469    
    ## gap_agreeableness.tste_6_5_ct     -0.029428   0.046210  -0.637 0.524307    
    ## gap_conscientiousness.tste_6_5_ct  0.050795   0.049336   1.030 0.303331    
    ## gap_emotionstability.tste_6_5_ct  -0.084195   0.038308  -2.198 0.028070 *  
    ## gap_openness.tste_6_5_ct          -0.036084   0.051541  -0.700 0.483936    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.448 on 2087 degrees of freedom
    ## Multiple R-squared:  0.09289,    Adjusted R-squared:  0.07116 
    ## F-statistic: 4.274 on 50 and 2087 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0243 -0.6952  0.2984  1.0470  2.7823 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       55.620596  15.233166   3.651 0.000267 ***
    ## real_extraversion_ct               0.050163   0.025103   1.998 0.045814 *  
    ## real_agreeableness_ct              0.013896   0.031383   0.443 0.657968    
    ## real_conscientiousness_ct          0.091675   0.034857   2.630 0.008600 ** 
    ## real_emotionstability_ct          -0.020687   0.034519  -0.599 0.549042    
    ## real_openness_ct                   0.102257   0.033409   3.061 0.002236 ** 
    ## tste_7_0_ct                       -0.198976   0.058845  -3.381 0.000735 ***
    ## tste_7_1_ct                       -0.001461   0.065383  -0.022 0.982179    
    ## tste_7_2_ct                       -0.062464   0.062754  -0.995 0.319665    
    ## tste_7_3_ct                       -0.171476   0.048867  -3.509 0.000459 ***
    ## tste_7_4_ct                       -0.007770   0.064799  -0.120 0.904570    
    ## tste_7_5_ct                        0.014101   0.069595   0.203 0.839460    
    ## tste_7_6_ct                        0.036071   0.073109   0.493 0.621792    
    ## release                           -0.025014   0.007498  -3.336 0.000864 ***
    ## gap_openness                      -0.035712   0.043427  -0.822 0.410980    
    ## income                             0.005308   0.017221   0.308 0.757942    
    ## gap_extraversion                  -0.006721   0.025786  -0.261 0.794388    
    ## gap_agreeableness                  0.078186   0.033674   2.322 0.020338 *  
    ## gap_emotionstability              -0.039090   0.031651  -1.235 0.216959    
    ## age                               -0.013218   0.004609  -2.868 0.004170 ** 
    ## sex2                              -0.104384   0.072516  -1.439 0.150171    
    ## gap_conscientiousness              0.014051   0.039927   0.352 0.724938    
    ## race4                             -0.223531   0.141983  -1.574 0.115558    
    ## education                          0.010029   0.026629   0.377 0.706508    
    ## race2                              0.100949   0.131667   0.767 0.443348    
    ## star_user                         -0.017384   0.070705  -0.246 0.805806    
    ## gap_extraversion.tste_7_0_ct       0.015540   0.028973   0.536 0.591752    
    ## gap_agreeableness.tste_7_0_ct      0.016466   0.039277   0.419 0.675099    
    ## gap_conscientiousness.tste_7_0_ct -0.009910   0.044841  -0.221 0.825118    
    ## gap_openness.tste_7_0_ct          -0.017280   0.045290  -0.382 0.702843    
    ## gap_extraversion.tste_7_1_ct      -0.028220   0.032496  -0.868 0.385261    
    ## gap_agreeableness.tste_7_1_ct      0.087324   0.047873   1.824 0.068288 .  
    ## gap_conscientiousness.tste_7_1_ct  0.013337   0.054647   0.244 0.807209    
    ## gap_emotionstability.tste_7_1_ct   0.001399   0.039062   0.036 0.971424    
    ## gap_openness.tste_7_1_ct           0.066222   0.053534   1.237 0.216231    
    ## star_GS                            0.065879   0.053055   1.242 0.214481    
    ## gap_extraversion.tste_7_2_ct      -0.055210   0.030481  -1.811 0.070241 .  
    ## gap_agreeableness.tste_7_2_ct     -0.045421   0.040742  -1.115 0.265052    
    ## gap_conscientiousness.tste_7_2_ct -0.031919   0.046469  -0.687 0.492224    
    ## gap_openness.tste_7_2_ct          -0.026510   0.046639  -0.568 0.569819    
    ## gap_extraversion.tste_7_3_ct      -0.001973   0.024337  -0.081 0.935388    
    ## gap_agreeableness.tste_7_3_ct     -0.047839   0.032291  -1.481 0.138631    
    ## gap_conscientiousness.tste_7_3_ct -0.005326   0.036734  -0.145 0.884733    
    ## gap_openness.tste_7_3_ct          -0.026982   0.037273  -0.724 0.469207    
    ## gap_extraversion.tste_7_4_ct      -0.017432   0.031540  -0.553 0.580532    
    ## gap_agreeableness.tste_7_4_ct      0.031707   0.043110   0.735 0.462130    
    ## gap_conscientiousness.tste_7_4_ct -0.076030   0.050524  -1.505 0.132523    
    ## gap_openness.tste_7_4_ct           0.060976   0.047317   1.289 0.197659    
    ## gap_extraversion.tste_7_5_ct      -0.028759   0.033178  -0.867 0.386142    
    ## gap_agreeableness.tste_7_5_ct     -0.009309   0.046635  -0.200 0.841795    
    ## gap_conscientiousness.tste_7_5_ct  0.185490   0.050900   3.644 0.000275 ***
    ## gap_openness.tste_7_5_ct           0.041632   0.050922   0.818 0.413701    
    ## gap_extraversion.tste_7_6_ct      -0.023796   0.034189  -0.696 0.486501    
    ## gap_agreeableness.tste_7_6_ct      0.008371   0.045301   0.185 0.853422    
    ## gap_conscientiousness.tste_7_6_ct  0.135544   0.051935   2.610 0.009122 ** 
    ## gap_openness.tste_7_6_ct           0.009043   0.052740   0.171 0.863881    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.449 on 2082 degrees of freedom
    ## Multiple R-squared:  0.09302,    Adjusted R-squared:  0.06906 
    ## F-statistic: 3.882 on 55 and 2082 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2279 -0.6784  0.2657  1.0183  3.2005 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       61.1358795 15.9185099   3.841 0.000126 ***
    ## real_extraversion_ct               0.0517367  0.0250219   2.068 0.038796 *  
    ## real_agreeableness_ct              0.0151703  0.0312341   0.486 0.627234    
    ## real_conscientiousness_ct          0.0857748  0.0347046   2.472 0.013532 *  
    ## real_emotionstability_ct          -0.0289087  0.0344359  -0.839 0.401290    
    ## real_openness_ct                   0.0987560  0.0332469   2.970 0.003008 ** 
    ## tste_8_0_ct                       -0.1954059  0.0645505  -3.027 0.002499 ** 
    ## tste_8_1_ct                        0.1655110  0.0720958   2.296 0.021792 *  
    ## tste_8_2_ct                        0.0969499  0.0619044   1.566 0.117472    
    ## tste_8_3_ct                        0.0651714  0.0595502   1.094 0.273909    
    ## tste_8_4_ct                        0.0941547  0.0635293   1.482 0.138474    
    ## tste_8_5_ct                        0.0597760  0.0612149   0.976 0.328933    
    ## tste_8_6_ct                       -0.1765568  0.0638150  -2.767 0.005713 ** 
    ## tste_8_7_ct                        0.0026344  0.0655260   0.040 0.967934    
    ## release                           -0.0276382  0.0078348  -3.528 0.000428 ***
    ## gap_openness                      -0.0457441  0.0431848  -1.059 0.289603    
    ## gap_emotionstability.tste_8_1_ct  -0.1605277  0.0422770  -3.797 0.000151 ***
    ## income                             0.0044340  0.0171541   0.258 0.796060    
    ## gap_extraversion                  -0.0064298  0.0256306  -0.251 0.801945    
    ## gap_agreeableness                  0.0777471  0.0335939   2.314 0.020747 *  
    ## gap_emotionstability              -0.0467642  0.0314900  -1.485 0.137683    
    ## age                               -0.0136128  0.0045856  -2.969 0.003026 ** 
    ## sex2                              -0.1034846  0.0720893  -1.436 0.151294    
    ## gap_conscientiousness              0.0102424  0.0397833   0.257 0.796853    
    ## race4                             -0.1995575  0.1408664  -1.417 0.156737    
    ## education                          0.0077933  0.0264897   0.294 0.768633    
    ## race2                              0.0998988  0.1311595   0.762 0.446350    
    ## star_user                         -0.0721523  0.0709353  -1.017 0.309198    
    ## gap_extraversion.tste_8_0_ct       0.0548744  0.0319625   1.717 0.086158 .  
    ## gap_agreeableness.tste_8_0_ct     -0.0570266  0.0435438  -1.310 0.190463    
    ## gap_openness.tste_8_0_ct          -0.1059058  0.0509410  -2.079 0.037741 *  
    ## star_GS                            0.0944772  0.0550550   1.716 0.086302 .  
    ## gap_extraversion.tste_8_1_ct      -0.0098239  0.0353476  -0.278 0.781099    
    ## gap_agreeableness.tste_8_1_ct      0.0401734  0.0488054   0.823 0.410526    
    ## gap_conscientiousness.tste_8_1_ct  0.0317537  0.0573514   0.554 0.579865    
    ## gap_openness.tste_8_1_ct          -0.0352089  0.0544537  -0.647 0.517972    
    ## gap_extraversion.tste_8_2_ct       0.0301888  0.0289808   1.042 0.297679    
    ## gap_agreeableness.tste_8_2_ct      0.0059336  0.0407612   0.146 0.884275    
    ## gap_conscientiousness.tste_8_2_ct -0.0431733  0.0439938  -0.981 0.326534    
    ## gap_openness.tste_8_2_ct          -0.0694409  0.0449173  -1.546 0.122264    
    ## gap_extraversion.tste_8_3_ct       0.0086842  0.0301424   0.288 0.773293    
    ## gap_agreeableness.tste_8_3_ct      0.0456244  0.0397770   1.147 0.251511    
    ## gap_conscientiousness.tste_8_3_ct -0.1255335  0.0459883  -2.730 0.006393 ** 
    ## gap_openness.tste_8_3_ct           0.0284988  0.0464988   0.613 0.540014    
    ## gap_extraversion.tste_8_4_ct       0.0279182  0.0313948   0.889 0.373966    
    ## gap_agreeableness.tste_8_4_ct     -0.0004573  0.0434831  -0.011 0.991610    
    ## gap_conscientiousness.tste_8_4_ct  0.0573939  0.0465827   1.232 0.218056    
    ## gap_openness.tste_8_4_ct           0.0046641  0.0477272   0.098 0.922161    
    ## gap_extraversion.tste_8_5_ct       0.0167053  0.0281403   0.594 0.552816    
    ## gap_agreeableness.tste_8_5_ct      0.0144604  0.0390216   0.371 0.710993    
    ## gap_conscientiousness.tste_8_5_ct  0.0564640  0.0434236   1.300 0.193639    
    ## gap_openness.tste_8_5_ct           0.0017127  0.0432459   0.040 0.968413    
    ## gap_extraversion.tste_8_6_ct       0.0687065  0.0315854   2.175 0.029723 *  
    ## gap_agreeableness.tste_8_6_ct      0.0444302  0.0448900   0.990 0.322409    
    ## gap_conscientiousness.tste_8_6_ct -0.0943578  0.0497725  -1.896 0.058128 .  
    ## gap_emotionstability.tste_8_6_ct  -0.0170792  0.0336490  -0.508 0.611809    
    ## gap_openness.tste_8_6_ct          -0.0569277  0.0512210  -1.111 0.266519    
    ## gap_extraversion.tste_8_7_ct      -0.0099332  0.0311658  -0.319 0.749971    
    ## gap_agreeableness.tste_8_7_ct     -0.0694500  0.0430116  -1.615 0.106531    
    ## gap_conscientiousness.tste_8_7_ct  0.0364459  0.0509127   0.716 0.474164    
    ## gap_openness.tste_8_7_ct          -0.0725805  0.0499950  -1.452 0.146720    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2077 degrees of freedom
    ## Multiple R-squared:  0.1047, Adjusted R-squared:  0.07879 
    ## F-statistic: 4.046 on 60 and 2077 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1339 -0.6852  0.3024  1.0101  2.9098 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       62.833947  16.078452   3.908 9.61e-05 ***
    ## real_extraversion_ct               0.056017   0.025051   2.236 0.025453 *  
    ## real_agreeableness_ct              0.009963   0.031216   0.319 0.749642    
    ## real_conscientiousness_ct          0.091436   0.034742   2.632 0.008554 ** 
    ## real_emotionstability_ct          -0.023035   0.034415  -0.669 0.503358    
    ## real_openness_ct                   0.091229   0.033303   2.739 0.006209 ** 
    ## tste_9_0_ct                       -0.152430   0.067386  -2.262 0.023798 *  
    ## tste_9_1_ct                       -0.035389   0.061423  -0.576 0.564572    
    ## tste_9_2_ct                       -0.330734   0.064575  -5.122 3.31e-07 ***
    ## tste_9_3_ct                       -0.096171   0.070779  -1.359 0.174372    
    ## tste_9_4_ct                        0.027221   0.055100   0.494 0.621339    
    ## tste_9_5_ct                        0.057847   0.071681   0.807 0.419756    
    ## tste_9_6_ct                        0.025803   0.064368   0.401 0.688554    
    ## tste_9_7_ct                       -0.031494   0.066009  -0.477 0.633337    
    ## tste_9_8_ct                        0.043158   0.059183   0.729 0.465942    
    ## release                           -0.028605   0.007904  -3.619 0.000303 ***
    ## gap_openness                      -0.046339   0.043339  -1.069 0.285090    
    ## gap_extraversion.tste_9_2_ct       0.006748   0.031187   0.216 0.828732    
    ## income                             0.007477   0.017181   0.435 0.663484    
    ## gap_extraversion                  -0.002564   0.025679  -0.100 0.920465    
    ## gap_agreeableness                  0.074579   0.033625   2.218 0.026663 *  
    ## gap_emotionstability              -0.044227   0.031579  -1.401 0.161512    
    ## age                               -0.013192   0.004593  -2.872 0.004117 ** 
    ## sex2                              -0.092647   0.072178  -1.284 0.199431    
    ## gap_conscientiousness              0.020752   0.040006   0.519 0.604011    
    ## race4                             -0.172828   0.141586  -1.221 0.222354    
    ## education                          0.005916   0.026513   0.223 0.823438    
    ## race2                              0.105068   0.131167   0.801 0.423207    
    ## star_user                         -0.085082   0.071620  -1.188 0.234982    
    ## gap_extraversion.tste_9_0_ct      -0.006208   0.033113  -0.187 0.851312    
    ## gap_agreeableness.tste_9_0_ct      0.051175   0.049629   1.031 0.302588    
    ## gap_conscientiousness.tste_9_0_ct  0.016847   0.054075   0.312 0.755411    
    ## gap_emotionstability.tste_9_0_ct   0.061838   0.040966   1.509 0.131328    
    ## gap_openness.tste_9_0_ct           0.001779   0.055346   0.032 0.974356    
    ## star_GS                            0.132971   0.056104   2.370 0.017874 *  
    ## gap_extraversion.tste_9_1_ct       0.037699   0.031304   1.204 0.228619    
    ## gap_agreeableness.tste_9_1_ct      0.045819   0.041080   1.115 0.264835    
    ## gap_openness.tste_9_1_ct           0.038247   0.047902   0.798 0.424710    
    ## gap_agreeableness.tste_9_2_ct     -0.008296   0.041961  -0.198 0.843289    
    ## gap_conscientiousness.tste_9_2_ct -0.047525   0.047307  -1.005 0.315200    
    ## gap_openness.tste_9_2_ct          -0.058180   0.048122  -1.209 0.226794    
    ## gap_extraversion.tste_9_3_ct       0.077208   0.032563   2.371 0.017828 *  
    ## gap_agreeableness.tste_9_3_ct     -0.012030   0.045777  -0.263 0.792743    
    ## gap_conscientiousness.tste_9_3_ct -0.005884   0.053343  -0.110 0.912181    
    ## gap_openness.tste_9_3_ct          -0.075191   0.050854  -1.479 0.139407    
    ## gap_extraversion.tste_9_4_ct      -0.016740   0.026071  -0.642 0.520885    
    ## gap_agreeableness.tste_9_4_ct     -0.051811   0.036165  -1.433 0.152116    
    ## gap_conscientiousness.tste_9_4_ct  0.005490   0.040093   0.137 0.891094    
    ## gap_openness.tste_9_4_ct          -0.000978   0.041399  -0.024 0.981155    
    ## gap_extraversion.tste_9_5_ct      -0.027262   0.034481  -0.791 0.429237    
    ## gap_agreeableness.tste_9_5_ct     -0.009658   0.047610  -0.203 0.839260    
    ## gap_conscientiousness.tste_9_5_ct -0.126629   0.055241  -2.292 0.021989 *  
    ## gap_openness.tste_9_5_ct           0.056506   0.054331   1.040 0.298454    
    ## gap_extraversion.tste_9_6_ct      -0.014355   0.030933  -0.464 0.642640    
    ## gap_agreeableness.tste_9_6_ct     -0.074514   0.040367  -1.846 0.065043 .  
    ## gap_openness.tste_9_6_ct           0.019026   0.047375   0.402 0.688021    
    ## gap_extraversion.tste_9_7_ct       0.015757   0.033365   0.472 0.636790    
    ## gap_agreeableness.tste_9_7_ct     -0.042280   0.045364  -0.932 0.351432    
    ## gap_conscientiousness.tste_9_7_ct -0.039356   0.050655  -0.777 0.437287    
    ## gap_openness.tste_9_7_ct          -0.072512   0.049572  -1.463 0.143682    
    ## gap_extraversion.tste_9_8_ct      -0.013329   0.028351  -0.470 0.638318    
    ## gap_agreeableness.tste_9_8_ct      0.039473   0.038908   1.015 0.310452    
    ## gap_conscientiousness.tste_9_8_ct  0.080189   0.041043   1.954 0.050864 .  
    ## gap_emotionstability.tste_9_8_ct  -0.091728   0.030900  -2.969 0.003027 ** 
    ## gap_openness.tste_9_8_ct           0.006519   0.044055   0.148 0.882374    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2073 degrees of freedom
    ## Multiple R-squared:  0.1062, Adjusted R-squared:  0.07863 
    ## F-statistic:  3.85 on 64 and 2073 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1232 -0.6900  0.2961  1.0106  3.1948 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        63.661251  15.889641   4.006 6.38e-05 ***
    ## real_extraversion_ct                0.055875   0.025034   2.232 0.025722 *  
    ## real_agreeableness_ct               0.009270   0.031342   0.296 0.767432    
    ## real_conscientiousness_ct           0.085899   0.034709   2.475 0.013409 *  
    ## real_emotionstability_ct           -0.026065   0.034399  -0.758 0.448699    
    ## real_openness_ct                    0.087488   0.033257   2.631 0.008586 ** 
    ## tste_10_0_ct                       -0.005437   0.057554  -0.094 0.924749    
    ## tste_10_1_ct                        0.217288   0.063069   3.445 0.000582 ***
    ## tste_10_2_ct                       -0.244028   0.066843  -3.651 0.000268 ***
    ## tste_10_3_ct                       -0.152689   0.067475  -2.263 0.023746 *  
    ## tste_10_4_ct                       -0.043575   0.066815  -0.652 0.514359    
    ## tste_10_5_ct                       -0.038815   0.065180  -0.596 0.551574    
    ## tste_10_6_ct                       -0.119828   0.065670  -1.825 0.068189 .  
    ## tste_10_7_ct                        0.031748   0.056199   0.565 0.572198    
    ## tste_10_8_ct                        0.046823   0.073374   0.638 0.523453    
    ## tste_10_9_ct                        0.098819   0.062303   1.586 0.112870    
    ## release                            -0.029101   0.007825  -3.719 0.000205 ***
    ## gap_openness                       -0.050425   0.043285  -1.165 0.244168    
    ## income                              0.005417   0.017176   0.315 0.752503    
    ## gap_extraversion                   -0.004424   0.025772  -0.172 0.863732    
    ## gap_agreeableness                   0.071806   0.033715   2.130 0.033308 *  
    ## gap_emotionstability               -0.046084   0.031556  -1.460 0.144329    
    ## age                                -0.013394   0.004590  -2.918 0.003559 ** 
    ## sex2                               -0.092410   0.072226  -1.279 0.200876    
    ## gap_conscientiousness               0.015147   0.039804   0.381 0.703574    
    ## race4                              -0.188592   0.141615  -1.332 0.183099    
    ## education                           0.003286   0.026486   0.124 0.901260    
    ## race2                               0.120897   0.131364   0.920 0.357510    
    ## star_user                          -0.053138   0.070885  -0.750 0.453561    
    ## star_GS                             0.125202   0.056032   2.234 0.025557 *  
    ## gap_extraversion.tste_10_0_ct       0.014639   0.028274   0.518 0.604683    
    ## gap_agreeableness.tste_10_0_ct     -0.075110   0.038612  -1.945 0.051877 .  
    ## gap_conscientiousness.tste_10_0_ct  0.029933   0.044773   0.669 0.503865    
    ## gap_emotionstability.tste_10_0_ct   0.077946   0.029803   2.615 0.008977 ** 
    ## gap_openness.tste_10_0_ct          -0.041605   0.043949  -0.947 0.343923    
    ## gap_extraversion.tste_10_1_ct      -0.023062   0.030554  -0.755 0.450455    
    ## gap_agreeableness.tste_10_1_ct     -0.029150   0.041631  -0.700 0.483884    
    ## gap_conscientiousness.tste_10_1_ct  0.056474   0.049018   1.152 0.249408    
    ## gap_openness.tste_10_1_ct           0.025198   0.048617   0.518 0.604301    
    ## gap_extraversion.tste_10_2_ct       0.057547   0.033921   1.697 0.089938 .  
    ## gap_agreeableness.tste_10_2_ct      0.025455   0.045473   0.560 0.575679    
    ## gap_openness.tste_10_2_ct          -0.075406   0.051117  -1.475 0.140317    
    ## gap_extraversion.tste_10_3_ct      -0.056647   0.033063  -1.713 0.086805 .  
    ## gap_agreeableness.tste_10_3_ct      0.077330   0.044504   1.738 0.082429 .  
    ## gap_conscientiousness.tste_10_3_ct  0.039754   0.050245   0.791 0.428913    
    ## gap_openness.tste_10_3_ct           0.017479   0.052272   0.334 0.738122    
    ## gap_extraversion.tste_10_4_ct       0.007854   0.032546   0.241 0.809333    
    ## gap_agreeableness.tste_10_4_ct     -0.036735   0.044401  -0.827 0.408133    
    ## gap_conscientiousness.tste_10_4_ct  0.006289   0.050539   0.124 0.900986    
    ## gap_openness.tste_10_4_ct           0.005146   0.052142   0.099 0.921398    
    ## gap_extraversion.tste_10_5_ct      -0.006206   0.032004  -0.194 0.846258    
    ## gap_agreeableness.tste_10_5_ct      0.034197   0.043121   0.793 0.427842    
    ## gap_openness.tste_10_5_ct           0.025945   0.049489   0.524 0.600156    
    ## gap_extraversion.tste_10_6_ct       0.046027   0.030430   1.513 0.130546    
    ## gap_agreeableness.tste_10_6_ct     -0.016708   0.043063  -0.388 0.698058    
    ## gap_conscientiousness.tste_10_6_ct -0.123430   0.046008  -2.683 0.007359 ** 
    ## gap_openness.tste_10_6_ct          -0.087789   0.046328  -1.895 0.058239 .  
    ## gap_extraversion.tste_10_7_ct       0.056895   0.028116   2.024 0.043144 *  
    ## gap_agreeableness.tste_10_7_ct      0.062253   0.040630   1.532 0.125630    
    ## gap_emotionstability.tste_10_7_ct   0.051587   0.033030   1.562 0.118487    
    ## gap_openness.tste_10_7_ct           0.002762   0.045506   0.061 0.951606    
    ## gap_extraversion.tste_10_8_ct      -0.013602   0.034961  -0.389 0.697276    
    ## gap_agreeableness.tste_10_8_ct      0.003450   0.046560   0.074 0.940947    
    ## gap_conscientiousness.tste_10_8_ct -0.152525   0.053199  -2.867 0.004185 ** 
    ## gap_openness.tste_10_8_ct           0.043694   0.054477   0.802 0.422609    
    ## gap_extraversion.tste_10_9_ct       0.025405   0.032231   0.788 0.430659    
    ## gap_agreeableness.tste_10_9_ct      0.050602   0.044309   1.142 0.253576    
    ## gap_conscientiousness.tste_10_9_ct -0.084146   0.048166  -1.747 0.080785 .  
    ## gap_openness.tste_10_9_ct           0.022178   0.050160   0.442 0.658424    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.439 on 2069 degrees of freedom
    ## Multiple R-squared:  0.1111, Adjusted R-squared:  0.08189 
    ## F-statistic: 3.803 on 68 and 2069 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0577 -0.6844  0.2905  1.0288  3.3179 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         65.9927482 16.1173360   4.095 4.39e-05 ***
    ## real_extraversion_ct                 0.0563613  0.0250226   2.252 0.024401 *  
    ## real_agreeableness_ct                0.0108773  0.0313528   0.347 0.728678    
    ## real_conscientiousness_ct            0.0821900  0.0347307   2.366 0.018049 *  
    ## real_emotionstability_ct            -0.0288843  0.0344541  -0.838 0.401936    
    ## real_openness_ct                     0.0828134  0.0333254   2.485 0.013034 *  
    ## tste_11_0_ct                        -0.0927246  0.0700576  -1.324 0.185800    
    ## tste_11_1_ct                         0.0135657  0.0656660   0.207 0.836353    
    ## tste_11_2_ct                        -0.1057714  0.0579853  -1.824 0.068281 .  
    ## tste_11_3_ct                        -0.0158806  0.0706850  -0.225 0.822260    
    ## tste_11_4_ct                        -0.0981597  0.0716534  -1.370 0.170860    
    ## tste_11_5_ct                        -0.1153533  0.0631777  -1.826 0.068017 .  
    ## tste_11_6_ct                         0.1462384  0.0605283   2.416 0.015777 *  
    ## tste_11_7_ct                         0.0779860  0.0708252   1.101 0.270979    
    ## tste_11_8_ct                        -0.2315388  0.0532891  -4.345 1.46e-05 ***
    ## tste_11_9_ct                         0.1482831  0.0627760   2.362 0.018264 *  
    ## tste_11_10_ct                        0.0572926  0.0664608   0.862 0.388760    
    ## release                             -0.0303414  0.0079366  -3.823 0.000136 ***
    ## gap_openness                        -0.0570139  0.0434243  -1.313 0.189347    
    ## income                               0.0074149  0.0172044   0.431 0.666520    
    ## gap_extraversion                    -0.0022840  0.0257254  -0.089 0.929262    
    ## gap_agreeableness                    0.0700266  0.0336564   2.081 0.037591 *  
    ## gap_emotionstability                -0.0467478  0.0315922  -1.480 0.139100    
    ## age                                 -0.0134037  0.0045955  -2.917 0.003576 ** 
    ## sex2                                -0.0868119  0.0723895  -1.199 0.230575    
    ## gap_conscientiousness                0.0064745  0.0398455   0.162 0.870936    
    ## race4                               -0.1926370  0.1417982  -1.359 0.174445    
    ## education                            0.0075804  0.0265646   0.285 0.775399    
    ## race2                                0.1286143  0.1314140   0.979 0.327845    
    ## star_user                           -0.0659756  0.0725584  -0.909 0.363310    
    ## gap_extraversion.tste_11_0_ct        0.0386953  0.0353626   1.094 0.273975    
    ## gap_agreeableness.tste_11_0_ct       0.0076783  0.0471575   0.163 0.870674    
    ## gap_conscientiousness.tste_11_0_ct  -0.1411787  0.0535988  -2.634 0.008502 ** 
    ## gap_openness.tste_11_0_ct            0.0438138  0.0539062   0.813 0.416440    
    ## star_GS                              0.1536180  0.0576518   2.665 0.007769 ** 
    ## gap_extraversion.tste_11_1_ct        0.0409932  0.0311847   1.315 0.188815    
    ## gap_agreeableness.tste_11_1_ct       0.0263636  0.0418700   0.630 0.528991    
    ## gap_conscientiousness.tste_11_1_ct  -0.0093728  0.0492504  -0.190 0.849085    
    ## gap_openness.tste_11_1_ct           -0.0176736  0.0489946  -0.361 0.718341    
    ## gap_extraversion.tste_11_2_ct        0.0622741  0.0283682   2.195 0.028260 *  
    ## gap_agreeableness.tste_11_2_ct       0.0228335  0.0401794   0.568 0.569900    
    ## gap_conscientiousness.tste_11_2_ct  -0.0036446  0.0448872  -0.081 0.935294    
    ## gap_emotionstability.tste_11_2_ct    0.0785779  0.0349211   2.250 0.024544 *  
    ## gap_openness.tste_11_2_ct           -0.0668993  0.0462371  -1.447 0.148084    
    ## gap_extraversion.tste_11_3_ct        0.0204606  0.0348490   0.587 0.557187    
    ## gap_agreeableness.tste_11_3_ct       0.0336704  0.0476485   0.707 0.479870    
    ## gap_conscientiousness.tste_11_3_ct  -0.0237307  0.0547262  -0.434 0.664606    
    ## gap_emotionstability.tste_11_3_ct    0.0970949  0.0437975   2.217 0.026738 *  
    ## gap_openness.tste_11_3_ct            0.0448444  0.0541570   0.828 0.407741    
    ## gap_extraversion.tste_11_4_ct       -0.0336106  0.0334519  -1.005 0.315139    
    ## gap_agreeableness.tste_11_4_ct      -0.0536564  0.0449351  -1.194 0.232582    
    ## gap_openness.tste_11_4_ct            0.0084303  0.0534326   0.158 0.874649    
    ## gap_extraversion.tste_11_5_ct       -0.0186469  0.0310718  -0.600 0.548492    
    ## gap_agreeableness.tste_11_5_ct       0.0338034  0.0428826   0.788 0.430625    
    ## gap_conscientiousness.tste_11_5_ct   0.0754945  0.0496864   1.519 0.128811    
    ## gap_openness.tste_11_5_ct            0.0054553  0.0480098   0.114 0.909543    
    ## gap_extraversion.tste_11_6_ct        0.0030199  0.0304104   0.099 0.920906    
    ## gap_agreeableness.tste_11_6_ct       0.0091089  0.0429510   0.212 0.832069    
    ## gap_conscientiousness.tste_11_6_ct   0.0185497  0.0458643   0.404 0.685926    
    ## gap_openness.tste_11_6_ct            0.0395192  0.0474072   0.834 0.404597    
    ## gap_extraversion.tste_11_7_ct        0.0485509  0.0350514   1.385 0.166162    
    ## gap_agreeableness.tste_11_7_ct      -0.0770369  0.0503871  -1.529 0.126442    
    ## gap_conscientiousness.tste_11_7_ct   0.0460372  0.0564196   0.816 0.414607    
    ## gap_emotionstability.tste_11_7_ct   -0.0096816  0.0423140  -0.229 0.819045    
    ## gap_openness.tste_11_7_ct           -0.0416036  0.0556180  -0.748 0.454531    
    ## gap_extraversion.tste_11_8_ct        0.0093994  0.0268728   0.350 0.726544    
    ## gap_agreeableness.tste_11_8_ct       0.0515972  0.0371512   1.389 0.165031    
    ## gap_conscientiousness.tste_11_8_ct   0.0560685  0.0429817   1.304 0.192218    
    ## gap_emotionstability.tste_11_8_ct   -0.0158659  0.0296186  -0.536 0.592243    
    ## gap_openness.tste_11_8_ct           -0.0349102  0.0420136  -0.831 0.406112    
    ## gap_extraversion.tste_11_9_ct       -0.0338442  0.0325729  -1.039 0.298913    
    ## gap_agreeableness.tste_11_9_ct       0.0574277  0.0436180   1.317 0.188117    
    ## gap_conscientiousness.tste_11_9_ct   0.0807933  0.0519457   1.555 0.120019    
    ## gap_openness.tste_11_9_ct            0.0781496  0.0502177   1.556 0.119810    
    ## gap_extraversion.tste_11_10_ct      -0.0070168  0.0304642  -0.230 0.817858    
    ## gap_agreeableness.tste_11_10_ct     -0.0003882  0.0427286  -0.009 0.992752    
    ## gap_conscientiousness.tste_11_10_ct  0.0958788  0.0465924   2.058 0.039733 *  
    ## gap_openness.tste_11_10_ct          -0.0033966  0.0467032  -0.073 0.942030    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.438 on 2060 degrees of freedom
    ## Multiple R-squared:  0.1161, Adjusted R-squared:  0.08301 
    ## F-statistic: 3.512 on 77 and 2060 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1898 -0.6753  0.2791  1.0303  3.1725 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         51.5002883 16.4732480   3.126 0.001795 ** 
    ## real_extraversion_ct                 0.0562820  0.0251295   2.240 0.025218 *  
    ## real_agreeableness_ct                0.0170952  0.0313382   0.546 0.585465    
    ## real_conscientiousness_ct            0.0765679  0.0348977   2.194 0.028342 *  
    ## real_emotionstability_ct            -0.0359956  0.0345661  -1.041 0.297833    
    ## real_openness_ct                     0.0851076  0.0333625   2.551 0.010813 *  
    ## tste_12_0_ct                         0.1511396  0.0738596   2.046 0.040853 *  
    ## tste_12_1_ct                         0.0609081  0.0613941   0.992 0.321273    
    ## tste_12_2_ct                         0.0364695  0.0621280   0.587 0.557264    
    ## tste_12_3_ct                         0.0001563  0.0688389   0.002 0.998189    
    ## tste_12_4_ct                        -0.0257210  0.0642811  -0.400 0.689100    
    ## tste_12_5_ct                         0.1103885  0.0680167   1.623 0.104751    
    ## tste_12_6_ct                        -0.0888667  0.0686330  -1.295 0.195531    
    ## tste_12_7_ct                         0.0888136  0.0683013   1.300 0.193637    
    ## tste_12_8_ct                         0.1771236  0.0603262   2.936 0.003361 ** 
    ## tste_12_9_ct                        -0.0888027  0.0646342  -1.374 0.169614    
    ## tste_12_10_ct                        0.2086179  0.0560758   3.720 0.000204 ***
    ## tste_12_11_ct                       -0.1379534  0.0599472  -2.301 0.021477 *  
    ## release                             -0.0234000  0.0080957  -2.890 0.003887 ** 
    ## gap_openness                        -0.0630203  0.0435030  -1.449 0.147590    
    ## income                               0.0062494  0.0172156   0.363 0.716636    
    ## gap_extraversion                    -0.0002913  0.0258501  -0.011 0.991010    
    ## gap_agreeableness                    0.0680619  0.0338114   2.013 0.044246 *  
    ## gap_emotionstability                -0.0536664  0.0316319  -1.697 0.089925 .  
    ## age                                 -0.0130600  0.0046042  -2.837 0.004605 ** 
    ## sex2                                -0.0793164  0.0724507  -1.095 0.273748    
    ## gap_conscientiousness               -0.0040869  0.0400361  -0.102 0.918703    
    ## race4                               -0.1941156  0.1417094  -1.370 0.170894    
    ## education                            0.0083903  0.0265497   0.316 0.752018    
    ## race2                                0.1169064  0.1315451   0.889 0.374259    
    ## star_user                           -0.0318269  0.0725609  -0.439 0.660981    
    ## star_GS                              0.1847620  0.0607735   3.040 0.002394 ** 
    ## gap_extraversion.tste_12_0_ct        0.0116949  0.0336679   0.347 0.728357    
    ## gap_agreeableness.tste_12_0_ct      -0.0157175  0.0465215  -0.338 0.735508    
    ## gap_conscientiousness.tste_12_0_ct   0.0874000  0.0544022   1.607 0.108306    
    ## gap_openness.tste_12_0_ct           -0.0543980  0.0512882  -1.061 0.288981    
    ## gap_extraversion.tste_12_1_ct       -0.0031036  0.0275098  -0.113 0.910187    
    ## gap_agreeableness.tste_12_1_ct      -0.0127644  0.0387799  -0.329 0.742075    
    ## gap_openness.tste_12_1_ct            0.0405795  0.0450221   0.901 0.367522    
    ## gap_extraversion.tste_12_2_ct       -0.0365845  0.0301225  -1.215 0.224687    
    ## gap_agreeableness.tste_12_2_ct       0.0297757  0.0419209   0.710 0.477608    
    ## gap_conscientiousness.tste_12_2_ct   0.0429579  0.0443391   0.969 0.332734    
    ## gap_openness.tste_12_2_ct            0.0931312  0.0469965   1.982 0.047650 *  
    ## gap_extraversion.tste_12_3_ct       -0.0306842  0.0342562  -0.896 0.370503    
    ## gap_agreeableness.tste_12_3_ct       0.0622970  0.0450716   1.382 0.167067    
    ## gap_conscientiousness.tste_12_3_ct   0.0542047  0.0513303   1.056 0.291093    
    ## gap_openness.tste_12_3_ct            0.0073523  0.0532940   0.138 0.890288    
    ## gap_extraversion.tste_12_4_ct       -0.0096271  0.0322443  -0.299 0.765301    
    ## gap_agreeableness.tste_12_4_ct      -0.0326674  0.0426345  -0.766 0.443633    
    ## gap_openness.tste_12_4_ct           -0.0420190  0.0495181  -0.849 0.396226    
    ## gap_extraversion.tste_12_5_ct        0.0482254  0.0336923   1.431 0.152482    
    ## gap_agreeableness.tste_12_5_ct      -0.0179658  0.0478645  -0.375 0.707440    
    ## gap_conscientiousness.tste_12_5_ct  -0.1117544  0.0532993  -2.097 0.036139 *  
    ## gap_emotionstability.tste_12_5_ct    0.0891505  0.0411195   2.168 0.030267 *  
    ## gap_openness.tste_12_5_ct            0.0240107  0.0553361   0.434 0.664402    
    ## gap_extraversion.tste_12_6_ct       -0.0143495  0.0339229  -0.423 0.672337    
    ## gap_agreeableness.tste_12_6_ct       0.1032072  0.0474898   2.173 0.029875 *  
    ## gap_conscientiousness.tste_12_6_ct   0.0226490  0.0502836   0.450 0.652452    
    ## gap_openness.tste_12_6_ct            0.0062403  0.0533309   0.117 0.906863    
    ## gap_extraversion.tste_12_7_ct       -0.0696888  0.0334359  -2.084 0.037260 *  
    ## gap_agreeableness.tste_12_7_ct       0.1051310  0.0462741   2.272 0.023194 *  
    ## gap_conscientiousness.tste_12_7_ct   0.0200104  0.0499660   0.400 0.688844    
    ## gap_openness.tste_12_7_ct           -0.0090404  0.0546680  -0.165 0.868669    
    ## gap_extraversion.tste_12_8_ct        0.0060886  0.0289272   0.210 0.833313    
    ## gap_agreeableness.tste_12_8_ct      -0.0232411  0.0385497  -0.603 0.546651    
    ## gap_conscientiousness.tste_12_8_ct   0.0534492  0.0425041   1.258 0.208713    
    ## gap_openness.tste_12_8_ct           -0.0443030  0.0447321  -0.990 0.322091    
    ## gap_extraversion.tste_12_9_ct        0.0309708  0.0333286   0.929 0.352865    
    ## gap_agreeableness.tste_12_9_ct       0.0552841  0.0437637   1.263 0.206646    
    ## gap_openness.tste_12_9_ct           -0.0560098  0.0513189  -1.091 0.275222    
    ## gap_extraversion.tste_12_10_ct      -0.0465530  0.0274839  -1.694 0.090450 .  
    ## gap_agreeableness.tste_12_10_ct     -0.0127893  0.0364757  -0.351 0.725905    
    ## gap_conscientiousness.tste_12_10_ct  0.0660439  0.0429972   1.536 0.124691    
    ## gap_openness.tste_12_10_ct           0.0732705  0.0425330   1.723 0.085098 .  
    ## gap_extraversion.tste_12_11_ct      -0.0522108  0.0299097  -1.746 0.081027 .  
    ## gap_agreeableness.tste_12_11_ct     -0.0175904  0.0438541  -0.401 0.688379    
    ## gap_conscientiousness.tste_12_11_ct -0.0252646  0.0458395  -0.551 0.581589    
    ## gap_emotionstability.tste_12_11_ct  -0.0744466  0.0358217  -2.078 0.037810 *  
    ## gap_openness.tste_12_11_ct          -0.0565855  0.0473176  -1.196 0.231887    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.44 on 2059 degrees of freedom
    ## Multiple R-squared:  0.1148, Adjusted R-squared:  0.08127 
    ## F-statistic: 3.423 on 78 and 2059 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3529 -0.6729  0.2668  1.0270  2.8653 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         49.8145283 16.8891120   2.950 0.003219 ** 
    ## real_extraversion_ct                 0.0538358  0.0251374   2.142 0.032338 *  
    ## real_agreeableness_ct                0.0241695  0.0313697   0.770 0.441108    
    ## real_conscientiousness_ct            0.0883542  0.0349044   2.531 0.011437 *  
    ## real_emotionstability_ct            -0.0308008  0.0346247  -0.890 0.373807    
    ## real_openness_ct                     0.0851364  0.0334258   2.547 0.010937 *  
    ## tste_13_0_ct                         0.0313114  0.0622416   0.503 0.614975    
    ## tste_13_1_ct                        -0.1082463  0.0586133  -1.847 0.064922 .  
    ## tste_13_2_ct                         0.1821962  0.0637099   2.860 0.004282 ** 
    ## tste_13_3_ct                         0.0134863  0.0620268   0.217 0.827898    
    ## tste_13_4_ct                        -0.0295861  0.0653768  -0.453 0.650923    
    ## tste_13_5_ct                         0.1014605  0.0652134   1.556 0.119904    
    ## tste_13_6_ct                         0.0210294  0.0696600   0.302 0.762769    
    ## tste_13_7_ct                        -0.0890876  0.0635923  -1.401 0.161390    
    ## tste_13_8_ct                         0.0934170  0.0581837   1.606 0.108526    
    ## tste_13_9_ct                         0.0797234  0.0722632   1.103 0.270054    
    ## tste_13_10_ct                        0.1915684  0.0611108   3.135 0.001744 ** 
    ## tste_13_11_ct                        0.1041478  0.0675056   1.543 0.123033    
    ## tste_13_12_ct                        0.0628942  0.0650415   0.967 0.333665    
    ## release                             -0.0223568  0.0082951  -2.695 0.007092 ** 
    ## gap_openness                        -0.0608635  0.0437026  -1.393 0.163869    
    ## gap_extraversion.tste_13_4_ct       -0.0559435  0.0331626  -1.687 0.091766 .  
    ## income                               0.0048341  0.0172236   0.281 0.778995    
    ## gap_extraversion                    -0.0046405  0.0258812  -0.179 0.857720    
    ## gap_agreeableness                    0.0732500  0.0338161   2.166 0.030416 *  
    ## gap_emotionstability                -0.0427204  0.0317603  -1.345 0.178746    
    ## age                                 -0.0134034  0.0046063  -2.910 0.003655 ** 
    ## sex2                                -0.0862254  0.0725525  -1.188 0.234791    
    ## gap_conscientiousness                0.0089072  0.0401832   0.222 0.824598    
    ## race4                               -0.1782422  0.1414636  -1.260 0.207817    
    ## education                            0.0113475  0.0266012   0.427 0.669732    
    ## race2                                0.1323757  0.1315234   1.006 0.314303    
    ## star_user                           -0.0753356  0.0720354  -1.046 0.295770    
    ## gap_extraversion.tste_13_0_ct        0.0034640  0.0314540   0.110 0.912317    
    ## gap_agreeableness.tste_13_0_ct      -0.0112065  0.0434869  -0.258 0.796666    
    ## gap_conscientiousness.tste_13_0_ct  -0.0661927  0.0480231  -1.378 0.168245    
    ## gap_openness.tste_13_0_ct           -0.0093884  0.0492273  -0.191 0.848768    
    ## star_GS                              0.1801525  0.0597402   3.016 0.002596 ** 
    ## gap_extraversion.tste_13_1_ct        0.0540671  0.0284129   1.903 0.057192 .  
    ## gap_agreeableness.tste_13_1_ct      -0.0248810  0.0376694  -0.661 0.509001    
    ## gap_conscientiousness.tste_13_1_ct   0.0824648  0.0430734   1.915 0.055693 .  
    ## gap_emotionstability.tste_13_1_ct    0.1173052  0.0315121   3.723 0.000203 ***
    ## gap_openness.tste_13_1_ct            0.0001420  0.0442229   0.003 0.997438    
    ## gap_extraversion.tste_13_2_ct        0.0163915  0.0293227   0.559 0.576218    
    ## gap_agreeableness.tste_13_2_ct      -0.0474565  0.0408196  -1.163 0.245131    
    ## gap_conscientiousness.tste_13_2_ct   0.1096436  0.0472019   2.323 0.020284 *  
    ## gap_openness.tste_13_2_ct           -0.0069045  0.0449395  -0.154 0.877909    
    ## gap_extraversion.tste_13_3_ct        0.0496835  0.0311140   1.597 0.110459    
    ## gap_agreeableness.tste_13_3_ct      -0.0570841  0.0408311  -1.398 0.162247    
    ## gap_openness.tste_13_3_ct            0.0213924  0.0484025   0.442 0.658558    
    ## gap_agreeableness.tste_13_4_ct      -0.0897636  0.0454398  -1.975 0.048352 *  
    ## gap_openness.tste_13_4_ct            0.0551414  0.0517825   1.065 0.287062    
    ## gap_extraversion.tste_13_5_ct        0.0243216  0.0312014   0.780 0.435774    
    ## gap_agreeableness.tste_13_5_ct       0.0145178  0.0435026   0.334 0.738624    
    ## gap_conscientiousness.tste_13_5_ct   0.0299831  0.0497867   0.602 0.547087    
    ## gap_emotionstability.tste_13_5_ct    0.0457230  0.0386863   1.182 0.237386    
    ## gap_openness.tste_13_5_ct            0.0299768  0.0495967   0.604 0.545637    
    ## gap_extraversion.tste_13_6_ct        0.0164567  0.0334248   0.492 0.622525    
    ## gap_agreeableness.tste_13_6_ct      -0.0361154  0.0435582  -0.829 0.407127    
    ## gap_conscientiousness.tste_13_6_ct   0.0657789  0.0524453   1.254 0.209898    
    ## gap_openness.tste_13_6_ct           -0.0762434  0.0524915  -1.452 0.146518    
    ## gap_extraversion.tste_13_7_ct        0.0009776  0.0315396   0.031 0.975275    
    ## gap_agreeableness.tste_13_7_ct       0.0593557  0.0427724   1.388 0.165376    
    ## gap_conscientiousness.tste_13_7_ct  -0.0688755  0.0481508  -1.430 0.152751    
    ## gap_openness.tste_13_7_ct            0.0159527  0.0477290   0.334 0.738236    
    ## gap_extraversion.tste_13_8_ct        0.0157958  0.0293860   0.538 0.590960    
    ## gap_agreeableness.tste_13_8_ct      -0.0700682  0.0394156  -1.778 0.075605 .  
    ## gap_conscientiousness.tste_13_8_ct  -0.0371482  0.0470503  -0.790 0.429887    
    ## gap_openness.tste_13_8_ct           -0.0101749  0.0473025  -0.215 0.829708    
    ## gap_extraversion.tste_13_9_ct        0.0150065  0.0351213   0.427 0.669223    
    ## gap_agreeableness.tste_13_9_ct      -0.0137493  0.0505045  -0.272 0.785465    
    ## gap_conscientiousness.tste_13_9_ct   0.0885120  0.0532108   1.663 0.096381 .  
    ## gap_openness.tste_13_9_ct            0.1065905  0.0564897   1.887 0.059314 .  
    ## gap_extraversion.tste_13_10_ct      -0.0425210  0.0311048  -1.367 0.171768    
    ## gap_agreeableness.tste_13_10_ct      0.0257851  0.0410810   0.628 0.530293    
    ## gap_openness.tste_13_10_ct           0.0545137  0.0468182   1.164 0.244410    
    ## gap_extraversion.tste_13_11_ct      -0.0378342  0.0326597  -1.158 0.246820    
    ## gap_agreeableness.tste_13_11_ct      0.0568920  0.0451995   1.259 0.208286    
    ## gap_conscientiousness.tste_13_11_ct -0.0636513  0.0533061  -1.194 0.232588    
    ## gap_openness.tste_13_11_ct           0.0573040  0.0524320   1.093 0.274557    
    ## gap_extraversion.tste_13_12_ct      -0.0066084  0.0317388  -0.208 0.835085    
    ## gap_agreeableness.tste_13_12_ct      0.0008951  0.0472590   0.019 0.984890    
    ## gap_conscientiousness.tste_13_12_ct -0.0098842  0.0527699  -0.187 0.851438    
    ## gap_emotionstability.tste_13_12_ct   0.0409649  0.0371665   1.102 0.270504    
    ## gap_openness.tste_13_12_ct           0.0537623  0.0538279   0.999 0.318019    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.44 on 2053 degrees of freedom
    ## Multiple R-squared:  0.1165, Adjusted R-squared:  0.08031 
    ## F-statistic: 3.222 on 84 and 2053 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4080 -0.6926  0.2589  1.0471  3.2628 
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         45.004691  16.832643   2.674 0.007563 ** 
    ## real_extraversion_ct                 0.055638   0.025171   2.210 0.027188 *  
    ## real_agreeableness_ct                0.024024   0.031524   0.762 0.446101    
    ## real_conscientiousness_ct            0.080814   0.035030   2.307 0.021153 *  
    ## real_emotionstability_ct            -0.036222   0.034716  -1.043 0.296895    
    ## real_openness_ct                     0.087407   0.033476   2.611 0.009094 ** 
    ## tste_14_0_ct                        -0.076454   0.065394  -1.169 0.242488    
    ## tste_14_1_ct                         0.015231   0.063479   0.240 0.810399    
    ## tste_14_2_ct                         0.085047   0.061646   1.380 0.167860    
    ## tste_14_3_ct                         0.078179   0.059019   1.325 0.185439    
    ## tste_14_4_ct                         0.020983   0.055418   0.379 0.705004    
    ## tste_14_5_ct                         0.004219   0.067558   0.062 0.950211    
    ## tste_14_6_ct                         0.045700   0.064192   0.712 0.476592    
    ## tste_14_7_ct                        -0.245918   0.072882  -3.374 0.000754 ***
    ## tste_14_8_ct                         0.113321   0.056296   2.013 0.044251 *  
    ## tste_14_9_ct                         0.003682   0.064949   0.057 0.954792    
    ## tste_14_10_ct                        0.100732   0.061943   1.626 0.104059    
    ## tste_14_11_ct                       -0.023229   0.061279  -0.379 0.704676    
    ## tste_14_12_ct                        0.104763   0.063430   1.652 0.098766 .  
    ## tste_14_13_ct                        0.123193   0.064757   1.902 0.057260 .  
    ## release                             -0.020093   0.008277  -2.427 0.015291 *  
    ## gap_openness                        -0.062455   0.043860  -1.424 0.154604    
    ## income                               0.005122   0.017277   0.296 0.766898    
    ## gap_extraversion                     0.005945   0.025951   0.229 0.818841    
    ## gap_agreeableness                    0.072307   0.033910   2.132 0.033097 *  
    ## gap_emotionstability                -0.044807   0.031909  -1.404 0.160410    
    ## age                                 -0.013423   0.004625  -2.902 0.003745 ** 
    ## sex2                                -0.086375   0.072865  -1.185 0.235988    
    ## gap_conscientiousness                0.005835   0.040529   0.144 0.885530    
    ## race4                               -0.176159   0.141897  -1.241 0.214578    
    ## education                            0.004025   0.026667   0.151 0.880042    
    ## race2                                0.081573   0.131917   0.618 0.536405    
    ## star_user                           -0.024114   0.072678  -0.332 0.740082    
    ## gap_extraversion.tste_14_0_ct       -0.065645   0.031165  -2.106 0.035294 *  
    ## gap_agreeableness.tste_14_0_ct       0.032707   0.042934   0.762 0.446267    
    ## gap_conscientiousness.tste_14_0_ct   0.145226   0.048220   3.012 0.002629 ** 
    ## gap_openness.tste_14_0_ct            0.007476   0.048641   0.154 0.877860    
    ## star_GS                              0.163872   0.060187   2.723 0.006530 ** 
    ## gap_extraversion.tste_14_1_ct       -0.006003   0.030076  -0.200 0.841828    
    ## gap_agreeableness.tste_14_1_ct       0.012522   0.041602   0.301 0.763454    
    ## gap_openness.tste_14_1_ct            0.006016   0.046416   0.130 0.896888    
    ## gap_extraversion.tste_14_2_ct       -0.014094   0.030179  -0.467 0.640530    
    ## gap_agreeableness.tste_14_2_ct      -0.010803   0.040842  -0.265 0.791418    
    ## gap_conscientiousness.tste_14_2_ct  -0.003312   0.047029  -0.070 0.943858    
    ## gap_openness.tste_14_2_ct           -0.025258   0.047062  -0.537 0.591543    
    ## gap_extraversion.tste_14_3_ct        0.006984   0.029316   0.238 0.811739    
    ## gap_agreeableness.tste_14_3_ct       0.034847   0.039903   0.873 0.382604    
    ## gap_conscientiousness.tste_14_3_ct   0.046076   0.045231   1.019 0.308470    
    ## gap_openness.tste_14_3_ct            0.010061   0.044696   0.225 0.821929    
    ## gap_extraversion.tste_14_4_ct        0.017838   0.028338   0.629 0.529109    
    ## gap_agreeableness.tste_14_4_ct      -0.061816   0.037483  -1.649 0.099270 .  
    ## gap_conscientiousness.tste_14_4_ct  -0.062602   0.042233  -1.482 0.138415    
    ## gap_openness.tste_14_4_ct            0.032087   0.044260   0.725 0.468557    
    ## gap_extraversion.tste_14_5_ct        0.023239   0.033766   0.688 0.491376    
    ## gap_agreeableness.tste_14_5_ct      -0.134280   0.045645  -2.942 0.003299 ** 
    ## gap_conscientiousness.tste_14_5_ct   0.070992   0.052651   1.348 0.177698    
    ## gap_openness.tste_14_5_ct           -0.053759   0.052451  -1.025 0.305512    
    ## gap_extraversion.tste_14_6_ct       -0.025849   0.031309  -0.826 0.409115    
    ## gap_agreeableness.tste_14_6_ct       0.059450   0.044336   1.341 0.180099    
    ## gap_emotionstability.tste_14_6_ct   -0.051369   0.035331  -1.454 0.146116    
    ## gap_openness.tste_14_6_ct           -0.074587   0.050824  -1.468 0.142375    
    ## gap_extraversion.tste_14_7_ct       -0.044643   0.033950  -1.315 0.188670    
    ## gap_agreeableness.tste_14_7_ct       0.104686   0.051617   2.028 0.042678 *  
    ## gap_conscientiousness.tste_14_7_ct  -0.122649   0.056773  -2.160 0.030862 *  
    ## gap_emotionstability.tste_14_7_ct   -0.068157   0.041893  -1.627 0.103906    
    ## gap_openness.tste_14_7_ct           -0.015621   0.057500  -0.272 0.785908    
    ## gap_extraversion.tste_14_8_ct        0.015543   0.028116   0.553 0.580444    
    ## gap_agreeableness.tste_14_8_ct       0.058248   0.038375   1.518 0.129204    
    ## gap_conscientiousness.tste_14_8_ct   0.032396   0.038277   0.846 0.397455    
    ## gap_emotionstability.tste_14_8_ct   -0.032423   0.032360  -1.002 0.316491    
    ## gap_openness.tste_14_8_ct           -0.005963   0.044286  -0.135 0.892901    
    ## gap_extraversion.tste_14_9_ct       -0.043905   0.032153  -1.366 0.172235    
    ## gap_agreeableness.tste_14_9_ct      -0.001637   0.044107  -0.037 0.970406    
    ## gap_openness.tste_14_9_ct            0.062150   0.049843   1.247 0.212570    
    ## gap_extraversion.tste_14_10_ct      -0.016171   0.031952  -0.506 0.612851    
    ## gap_agreeableness.tste_14_10_ct      0.001244   0.044184   0.028 0.977540    
    ## gap_conscientiousness.tste_14_10_ct  0.054795   0.051315   1.068 0.285723    
    ## gap_openness.tste_14_10_ct           0.025370   0.050925   0.498 0.618415    
    ## gap_extraversion.tste_14_11_ct       0.015336   0.030133   0.509 0.610842    
    ## gap_agreeableness.tste_14_11_ct     -0.050852   0.040501  -1.256 0.209414    
    ## gap_conscientiousness.tste_14_11_ct -0.060101   0.048667  -1.235 0.216997    
    ## gap_openness.tste_14_11_ct          -0.045411   0.047030  -0.966 0.334371    
    ## gap_extraversion.tste_14_12_ct      -0.054262   0.032372  -1.676 0.093855 .  
    ## gap_agreeableness.tste_14_12_ct      0.040052   0.045661   0.877 0.380501    
    ## gap_conscientiousness.tste_14_12_ct  0.049789   0.050896   0.978 0.328070    
    ## gap_emotionstability.tste_14_12_ct  -0.045798   0.039839  -1.150 0.250449    
    ## gap_openness.tste_14_12_ct           0.001502   0.050725   0.030 0.976387    
    ## gap_extraversion.tste_14_13_ct      -0.032787   0.031011  -1.057 0.290511    
    ## gap_agreeableness.tste_14_13_ct     -0.044776   0.044803  -0.999 0.317720    
    ## gap_conscientiousness.tste_14_13_ct  0.082279   0.049951   1.647 0.099670 .  
    ## gap_emotionstability.tste_14_13_ct   0.063194   0.038503   1.641 0.100896    
    ## gap_openness.tste_14_13_ct           0.110330   0.050848   2.170 0.030138 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2046 degrees of freedom
    ## Multiple R-squared:  0.118,  Adjusted R-squared:  0.07882 
    ## F-statistic: 3.009 on 91 and 2046 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3437 -0.6538  0.2713  1.0160  3.5981 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                         52.5198628 17.8554730   2.941  0.00330 **
    ## real_extraversion_ct                 0.0607083  0.0251887   2.410  0.01603 * 
    ## real_agreeableness_ct                0.0196444  0.0314851   0.624  0.53274   
    ## real_conscientiousness_ct            0.0819656  0.0350664   2.337  0.01951 * 
    ## real_emotionstability_ct            -0.0259910  0.0347424  -0.748  0.45448   
    ## real_openness_ct                     0.0867456  0.0334860   2.591  0.00965 **
    ## tste_15_0_ct                        -0.0097803  0.0609405  -0.160  0.87251   
    ## tste_15_1_ct                        -0.1347152  0.0653080  -2.063  0.03926 * 
    ## tste_15_2_ct                         0.1288042  0.0607479   2.120  0.03410 * 
    ## tste_15_3_ct                         0.0894336  0.0647029   1.382  0.16706   
    ## tste_15_4_ct                         0.0184798  0.0583914   0.316  0.75167   
    ## tste_15_5_ct                        -0.0641368  0.0671899  -0.955  0.33991   
    ## tste_15_6_ct                        -0.0347541  0.0589660  -0.589  0.55566   
    ## tste_15_7_ct                        -0.0897827  0.0630780  -1.423  0.15478   
    ## tste_15_8_ct                        -0.1082436  0.0666562  -1.624  0.10455   
    ## tste_15_9_ct                         0.0033174  0.0645061   0.051  0.95899   
    ## tste_15_10_ct                        0.0318264  0.0669746   0.475  0.63469   
    ## tste_15_11_ct                        0.1762015  0.0688865   2.558  0.01060 * 
    ## tste_15_12_ct                       -0.0491845  0.0482494  -1.019  0.30814   
    ## tste_15_13_ct                        0.0427434  0.0568337   0.752  0.45209   
    ## tste_15_14_ct                        0.0299945  0.0586549   0.511  0.60915   
    ## release                             -0.0236123  0.0087704  -2.692  0.00715 **
    ## gap_openness                        -0.0590413  0.0438373  -1.347  0.17818   
    ## gap_extraversion.tste_15_2_ct        0.0365881  0.0309196   1.183  0.23682   
    ## income                               0.0057941  0.0172893   0.335  0.73756   
    ## gap_extraversion                     0.0092482  0.0259325   0.357  0.72141   
    ## gap_agreeableness                    0.0635701  0.0339686   1.871  0.06143 . 
    ## gap_emotionstability                -0.0436984  0.0318965  -1.370  0.17084   
    ## age                                 -0.0131936  0.0046233  -2.854  0.00436 **
    ## sex2                                -0.0905816  0.0728472  -1.243  0.21385   
    ## gap_conscientiousness                0.0073190  0.0405299   0.181  0.85671   
    ## race4                               -0.1836991  0.1420626  -1.293  0.19613   
    ## education                            0.0045000  0.0266448   0.169  0.86590   
    ## race2                                0.0935391  0.1320393   0.708  0.47877   
    ## gap_extraversion.tste_15_0_ct       -0.0253599  0.0305577  -0.830  0.40669   
    ## gap_agreeableness.tste_15_0_ct       0.0170901  0.0429547   0.398  0.69077   
    ## gap_conscientiousness.tste_15_0_ct  -0.0681787  0.0472579  -1.443  0.14926   
    ## gap_openness.tste_15_0_ct           -0.0206757  0.0475183  -0.435  0.66353   
    ## star_user                           -0.0504442  0.0762750  -0.661  0.50846   
    ## gap_extraversion.tste_15_1_ct        0.0451222  0.0307214   1.469  0.14205   
    ## gap_agreeableness.tste_15_1_ct       0.0032907  0.0404242   0.081  0.93513   
    ## gap_openness.tste_15_1_ct           -0.0562563  0.0482576  -1.166  0.24385   
    ## star_GS                              0.1346819  0.0607696   2.216  0.02678 * 
    ## gap_agreeableness.tste_15_2_ct       0.0201451  0.0413044   0.488  0.62580   
    ## gap_openness.tste_15_2_ct           -0.0478936  0.0493475  -0.971  0.33189   
    ## gap_extraversion.tste_15_3_ct       -0.0081336  0.0303985  -0.268  0.78906   
    ## gap_agreeableness.tste_15_3_ct      -0.0317755  0.0460289  -0.690  0.49006   
    ## gap_conscientiousness.tste_15_3_ct   0.1383662  0.0489353   2.828  0.00474 **
    ## gap_emotionstability.tste_15_3_ct    0.0027322  0.0394901   0.069  0.94485   
    ## gap_openness.tste_15_3_ct            0.0071652  0.0518742   0.138  0.89015   
    ## gap_extraversion.tste_15_4_ct        0.0104715  0.0296416   0.353  0.72392   
    ## gap_agreeableness.tste_15_4_ct       0.0079706  0.0409053   0.195  0.84553   
    ## gap_conscientiousness.tste_15_4_ct   0.0668046  0.0446650   1.496  0.13489   
    ## gap_emotionstability.tste_15_4_ct    0.0646378  0.0342469   1.887  0.05925 . 
    ## gap_openness.tste_15_4_ct           -0.0070281  0.0458813  -0.153  0.87827   
    ## gap_extraversion.tste_15_5_ct        0.0456183  0.0320810   1.422  0.15519   
    ## gap_agreeableness.tste_15_5_ct      -0.0895505  0.0440756  -2.032  0.04231 * 
    ## gap_conscientiousness.tste_15_5_ct  -0.0363339  0.0517460  -0.702  0.48266   
    ## gap_openness.tste_15_5_ct           -0.0455969  0.0507066  -0.899  0.36864   
    ## gap_extraversion.tste_15_6_ct       -0.0527600  0.0294606  -1.791  0.07346 . 
    ## gap_agreeableness.tste_15_6_ct      -0.0492836  0.0400621  -1.230  0.21877   
    ## gap_conscientiousness.tste_15_6_ct   0.0821649  0.0428281   1.918  0.05519 . 
    ## gap_openness.tste_15_6_ct           -0.0380707  0.0450788  -0.845  0.39847   
    ## gap_extraversion.tste_15_7_ct        0.0056205  0.0314281   0.179  0.85808   
    ## gap_agreeableness.tste_15_7_ct       0.0529508  0.0397726   1.331  0.18323   
    ## gap_openness.tste_15_7_ct            0.0052274  0.0470820   0.111  0.91161   
    ## gap_extraversion.tste_15_8_ct       -0.0115683  0.0337211  -0.343  0.73159   
    ## gap_agreeableness.tste_15_8_ct       0.0565806  0.0478059   1.184  0.23673   
    ## gap_conscientiousness.tste_15_8_ct   0.0258433  0.0530983   0.487  0.62652   
    ## gap_emotionstability.tste_15_8_ct   -0.0049774  0.0425690  -0.117  0.90693   
    ## gap_openness.tste_15_8_ct           -0.0973317  0.0546908  -1.780  0.07528 . 
    ## gap_extraversion.tste_15_9_ct        0.0065243  0.0301637   0.216  0.82878   
    ## gap_agreeableness.tste_15_9_ct       0.0516991  0.0444964   1.162  0.24542   
    ## gap_conscientiousness.tste_15_9_ct   0.0391017  0.0503499   0.777  0.43748   
    ## gap_emotionstability.tste_15_9_ct   -0.0415841  0.0385381  -1.079  0.28070   
    ## gap_openness.tste_15_9_ct           -0.0251837  0.0505517  -0.498  0.61841   
    ## gap_extraversion.tste_15_10_ct       0.0542332  0.0321133   1.689  0.09141 . 
    ## gap_agreeableness.tste_15_10_ct     -0.0976621  0.0436585  -2.237  0.02540 * 
    ## gap_openness.tste_15_10_ct          -0.0117283  0.0503132  -0.233  0.81570   
    ## gap_extraversion.tste_15_11_ct      -0.0370610  0.0340725  -1.088  0.27685   
    ## gap_agreeableness.tste_15_11_ct      0.0134577  0.0455766   0.295  0.76781   
    ## gap_conscientiousness.tste_15_11_ct  0.1437890  0.0537839   2.673  0.00757 **
    ## gap_openness.tste_15_11_ct           0.0418826  0.0545793   0.767  0.44295   
    ## gap_extraversion.tste_15_12_ct      -0.0140967  0.0238570  -0.591  0.55466   
    ## gap_agreeableness.tste_15_12_ct      0.0325947  0.0345905   0.942  0.34615   
    ## gap_conscientiousness.tste_15_12_ct -0.0539322  0.0385081  -1.401  0.16150   
    ## gap_openness.tste_15_12_ct           0.0358646  0.0373117   0.961  0.33656   
    ## gap_extraversion.tste_15_13_ct       0.0103190  0.0288962   0.357  0.72105   
    ## gap_agreeableness.tste_15_13_ct     -0.0461743  0.0392619  -1.176  0.23971   
    ## gap_conscientiousness.tste_15_13_ct  0.0446298  0.0436450   1.023  0.30664   
    ## gap_emotionstability.tste_15_13_ct   0.0534320  0.0339711   1.573  0.11590   
    ## gap_openness.tste_15_13_ct          -0.0006742  0.0439406  -0.015  0.98776   
    ## gap_extraversion.tste_15_14_ct       0.0252169  0.0270776   0.931  0.35182   
    ## gap_agreeableness.tste_15_14_ct     -0.0052729  0.0368017  -0.143  0.88608   
    ## gap_conscientiousness.tste_15_14_ct  0.0025725  0.0413816   0.062  0.95044   
    ## gap_openness.tste_15_14_ct          -0.0250754  0.0425261  -0.590  0.55549   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2042 degrees of freedom
    ## Multiple R-squared:  0.1199, Adjusted R-squared:  0.07897 
    ## F-statistic: 2.929 on 95 and 2042 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1567 -0.6763  0.2629  1.0240  3.3918 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         34.5877658 17.8886918   1.933 0.053313 .  
    ## real_extraversion_ct                 0.0568587  0.0251319   2.262 0.023777 *  
    ## real_agreeableness_ct                0.0166616  0.0313423   0.532 0.595060    
    ## real_conscientiousness_ct            0.0815188  0.0349588   2.332 0.019806 *  
    ## real_emotionstability_ct            -0.0229593  0.0346942  -0.662 0.508198    
    ## real_openness_ct                     0.0873266  0.0333849   2.616 0.008969 ** 
    ## tste_16_0_ct                         0.1813080  0.0618968   2.929 0.003436 ** 
    ## tste_16_1_ct                        -0.1003415  0.0619144  -1.621 0.105248    
    ## tste_16_2_ct                        -0.0347392  0.0566796  -0.613 0.540007    
    ## tste_16_3_ct                        -0.1344391  0.0622078  -2.161 0.030802 *  
    ## tste_16_4_ct                        -0.1410515  0.0524504  -2.689 0.007220 ** 
    ## tste_16_5_ct                         0.1256415  0.0616232   2.039 0.041592 *  
    ## tste_16_6_ct                         0.0815474  0.0550471   1.481 0.138652    
    ## tste_16_7_ct                         0.0421460  0.0549919   0.766 0.443525    
    ## tste_16_8_ct                        -0.1351433  0.0647421  -2.087 0.036975 *  
    ## tste_16_9_ct                         0.2040251  0.0566578   3.601 0.000325 ***
    ## tste_16_10_ct                       -0.0855227  0.0552669  -1.547 0.121911    
    ## tste_16_11_ct                        0.1601825  0.0620972   2.580 0.009963 ** 
    ## tste_16_12_ct                        0.0407910  0.0586088   0.696 0.486516    
    ## tste_16_13_ct                        0.0365113  0.0627668   0.582 0.560834    
    ## tste_16_14_ct                        0.1270877  0.0582938   2.180 0.029362 *  
    ## tste_16_15_ct                        0.0057669  0.0631341   0.091 0.927228    
    ## release                             -0.0147008  0.0087889  -1.673 0.094550 .  
    ## gap_openness                        -0.0663530  0.0438642  -1.513 0.130513    
    ## income                               0.0114231  0.0172580   0.662 0.508111    
    ## gap_extraversion                    -0.0058671  0.0259606  -0.226 0.821223    
    ## gap_agreeableness                    0.0635121  0.0339421   1.871 0.061462 .  
    ## gap_emotionstability                -0.0439741  0.0317229  -1.386 0.165840    
    ## age                                 -0.0120228  0.0046201  -2.602 0.009328 ** 
    ## sex2                                -0.0794732  0.0726075  -1.095 0.273839    
    ## gap_conscientiousness                0.0056725  0.0401690   0.141 0.887713    
    ## race4                               -0.1852757  0.1415479  -1.309 0.190707    
    ## education                            0.0040100  0.0266341   0.151 0.880338    
    ## race2                                0.1111069  0.1316548   0.844 0.398810    
    ## star_user                           -0.0693213  0.0731176  -0.948 0.343202    
    ## gap_extraversion.tste_16_0_ct       -0.0210166  0.0316699  -0.664 0.507013    
    ## gap_agreeableness.tste_16_0_ct      -0.0615860  0.0440688  -1.397 0.162416    
    ## gap_conscientiousness.tste_16_0_ct   0.0454046  0.0455265   0.997 0.318726    
    ## gap_openness.tste_16_0_ct            0.0930253  0.0492394   1.889 0.059001 .  
    ## star_GS                              0.1530149  0.0589175   2.597 0.009469 ** 
    ## gap_extraversion.tste_16_1_ct       -0.0060898  0.0299217  -0.204 0.838746    
    ## gap_agreeableness.tste_16_1_ct      -0.0323556  0.0407942  -0.793 0.427787    
    ## gap_openness.tste_16_1_ct            0.0438198  0.0455440   0.962 0.336092    
    ## gap_extraversion.tste_16_2_ct       -0.0256800  0.0275797  -0.931 0.351901    
    ## gap_agreeableness.tste_16_2_ct      -0.0068786  0.0397422  -0.173 0.862605    
    ## gap_conscientiousness.tste_16_2_ct   0.0831289  0.0444641   1.870 0.061686 .  
    ## gap_emotionstability.tste_16_2_ct    0.0977516  0.0327227   2.987 0.002848 ** 
    ## gap_openness.tste_16_2_ct            0.0543461  0.0446996   1.216 0.224200    
    ## gap_extraversion.tste_16_3_ct       -0.0525064  0.0296745  -1.769 0.076975 .  
    ## gap_agreeableness.tste_16_3_ct       0.0873636  0.0409648   2.133 0.033073 *  
    ## gap_conscientiousness.tste_16_3_ct  -0.0685133  0.0469497  -1.459 0.144639    
    ## gap_openness.tste_16_3_ct            0.0393711  0.0455996   0.863 0.388014    
    ## gap_extraversion.tste_16_4_ct       -0.0077612  0.0260516  -0.298 0.765796    
    ## gap_agreeableness.tste_16_4_ct       0.0097428  0.0348684   0.279 0.779953    
    ## gap_conscientiousness.tste_16_4_ct  -0.0209300  0.0404558  -0.517 0.604965    
    ## gap_openness.tste_16_4_ct           -0.0301190  0.0406205  -0.741 0.458491    
    ## gap_extraversion.tste_16_5_ct        0.0001370  0.0311140   0.004 0.996488    
    ## gap_agreeableness.tste_16_5_ct       0.0031142  0.0440963   0.071 0.943705    
    ## gap_conscientiousness.tste_16_5_ct   0.0086044  0.0481364   0.179 0.858151    
    ## gap_emotionstability.tste_16_5_ct   -0.0044462  0.0341230  -0.130 0.896342    
    ## gap_openness.tste_16_5_ct           -0.0474519  0.0501358  -0.946 0.344023    
    ## gap_extraversion.tste_16_6_ct        0.0059903  0.0279467   0.214 0.830298    
    ## gap_agreeableness.tste_16_6_ct       0.0453897  0.0423418   1.072 0.283855    
    ## gap_conscientiousness.tste_16_6_ct  -0.0638113  0.0449928  -1.418 0.156269    
    ## gap_emotionstability.tste_16_6_ct    0.0321811  0.0347806   0.925 0.354940    
    ## gap_openness.tste_16_6_ct            0.0367849  0.0457569   0.804 0.421537    
    ## gap_extraversion.tste_16_7_ct       -0.0061738  0.0273357  -0.226 0.821339    
    ## gap_agreeableness.tste_16_7_ct       0.0770043  0.0371147   2.075 0.038134 *  
    ## gap_openness.tste_16_7_ct            0.0312730  0.0434986   0.719 0.472259    
    ## gap_extraversion.tste_16_8_ct        0.0632863  0.0307567   2.058 0.039751 *  
    ## gap_agreeableness.tste_16_8_ct      -0.0099616  0.0413693  -0.241 0.809736    
    ## gap_openness.tste_16_8_ct            0.0091616  0.0489429   0.187 0.851530    
    ## gap_extraversion.tste_16_9_ct       -0.0368799  0.0285018  -1.294 0.195829    
    ## gap_agreeableness.tste_16_9_ct      -0.0491439  0.0381098  -1.290 0.197359    
    ## gap_conscientiousness.tste_16_9_ct   0.1051208  0.0431552   2.436 0.014941 *  
    ## gap_openness.tste_16_9_ct           -0.0175266  0.0431869  -0.406 0.684909    
    ## gap_extraversion.tste_16_10_ct       0.0510186  0.0273047   1.868 0.061837 .  
    ## gap_agreeableness.tste_16_10_ct     -0.0267463  0.0356799  -0.750 0.453571    
    ## gap_conscientiousness.tste_16_10_ct -0.0498745  0.0422263  -1.181 0.237691    
    ## gap_openness.tste_16_10_ct          -0.0624482  0.0412837  -1.513 0.130521    
    ## gap_extraversion.tste_16_11_ct       0.0398749  0.0285509   1.397 0.162679    
    ## gap_agreeableness.tste_16_11_ct     -0.0190248  0.0411621  -0.462 0.643992    
    ## gap_emotionstability.tste_16_11_ct   0.0630585  0.0336127   1.876 0.060794 .  
    ## gap_openness.tste_16_11_ct          -0.0174813  0.0469923  -0.372 0.709928    
    ## gap_extraversion.tste_16_12_ct       0.0078110  0.0291119   0.268 0.788487    
    ## gap_agreeableness.tste_16_12_ct     -0.0042899  0.0419188  -0.102 0.918499    
    ## gap_conscientiousness.tste_16_12_ct  0.0234219  0.0488153   0.480 0.631416    
    ## gap_emotionstability.tste_16_12_ct   0.0016202  0.0366854   0.044 0.964778    
    ## gap_openness.tste_16_12_ct           0.0417976  0.0484714   0.862 0.388617    
    ## gap_extraversion.tste_16_13_ct      -0.0198157  0.0308639  -0.642 0.520923    
    ## gap_agreeableness.tste_16_13_ct      0.0540665  0.0426061   1.269 0.204591    
    ## gap_conscientiousness.tste_16_13_ct  0.0460677  0.0471069   0.978 0.328220    
    ## gap_openness.tste_16_13_ct           0.0168406  0.0481776   0.350 0.726712    
    ## gap_extraversion.tste_16_14_ct       0.0007915  0.0277359   0.029 0.977236    
    ## gap_agreeableness.tste_16_14_ct     -0.0343152  0.0364030  -0.943 0.345973    
    ## gap_conscientiousness.tste_16_14_ct -0.0087604  0.0403617  -0.217 0.828193    
    ## gap_openness.tste_16_14_ct           0.0203577  0.0424801   0.479 0.631828    
    ## gap_extraversion.tste_16_15_ct      -0.0147955  0.0317105  -0.467 0.640849    
    ## gap_agreeableness.tste_16_15_ct      0.0106134  0.0441645   0.240 0.810110    
    ## gap_conscientiousness.tste_16_15_ct  0.0937599  0.0493092   1.901 0.057382 .  
    ## gap_openness.tste_16_15_ct          -0.0667878  0.0511147  -1.307 0.191487    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.436 on 2037 degrees of freedom
    ## Multiple R-squared:  0.1283, Adjusted R-squared:  0.08546 
    ## F-statistic: 2.997 on 100 and 2037 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3854 -0.6684  0.2226  1.0296  3.3778 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         35.4146198 18.4804906   1.916  0.05546 .  
    ## real_extraversion_ct                 0.0541944  0.0251406   2.156  0.03123 *  
    ## real_agreeableness_ct                0.0295796  0.0314578   0.940  0.34718    
    ## real_conscientiousness_ct            0.0868409  0.0349730   2.483  0.01311 *  
    ## real_emotionstability_ct            -0.0273149  0.0347227  -0.787  0.43157    
    ## real_openness_ct                     0.0882249  0.0333506   2.645  0.00822 ** 
    ## tste_17_0_ct                         0.3221263  0.0618612   5.207 2.11e-07 ***
    ## tste_17_1_ct                         0.0853546  0.0534913   1.596  0.11072    
    ## tste_17_2_ct                        -0.0211106  0.0642447  -0.329  0.74249    
    ## tste_17_3_ct                         0.0435473  0.0595100   0.732  0.46440    
    ## tste_17_4_ct                         0.1184912  0.0634353   1.868  0.06192 .  
    ## tste_17_5_ct                         0.0213537  0.0586875   0.364  0.71601    
    ## tste_17_6_ct                        -0.0726735  0.0612006  -1.187  0.23518    
    ## tste_17_7_ct                         0.1044568  0.0550415   1.898  0.05787 .  
    ## tste_17_8_ct                        -0.0792532  0.0599387  -1.322  0.18624    
    ## tste_17_9_ct                        -0.0772394  0.0583408  -1.324  0.18567    
    ## tste_17_10_ct                        0.0361695  0.0534286   0.677  0.49850    
    ## tste_17_11_ct                        0.0256408  0.0582871   0.440  0.66005    
    ## tste_17_12_ct                       -0.1246891  0.0517477  -2.410  0.01606 *  
    ## tste_17_13_ct                        0.1045666  0.0618091   1.692  0.09084 .  
    ## tste_17_14_ct                        0.0369408  0.0556904   0.663  0.50720    
    ## tste_17_15_ct                       -0.0488968  0.0689829  -0.709  0.47851    
    ## tste_17_16_ct                       -0.0376144  0.0531559  -0.708  0.47926    
    ## release                             -0.0152241  0.0090537  -1.682  0.09281 .  
    ## gap_openness                        -0.0517628  0.0437964  -1.182  0.23739    
    ## gap_extraversion.tste_17_12_ct      -0.0364308  0.0256498  -1.420  0.15567    
    ## income                               0.0100332  0.0172345   0.582  0.56053    
    ## gap_extraversion                    -0.0011514  0.0259933  -0.044  0.96467    
    ## gap_agreeableness                    0.0742153  0.0340239   2.181  0.02928 *  
    ## gap_emotionstability                -0.0471830  0.0318750  -1.480  0.13896    
    ## age                                 -0.0118901  0.0046158  -2.576  0.01007 *  
    ## sex2                                -0.0936479  0.0724799  -1.292  0.19649    
    ## gap_conscientiousness                0.0174854  0.0403600   0.433  0.66489    
    ## race4                               -0.1977472  0.1413487  -1.399  0.16196    
    ## education                            0.0059054  0.0265925   0.222  0.82428    
    ## race2                                0.1032452  0.1315972   0.785  0.43281    
    ## star_user                           -0.0742011  0.0778006  -0.954  0.34033    
    ## gap_extraversion.tste_17_0_ct       -0.0097517  0.0293211  -0.333  0.73948    
    ## gap_agreeableness.tste_17_0_ct      -0.0454163  0.0409509  -1.109  0.26754    
    ## gap_conscientiousness.tste_17_0_ct   0.0619690  0.0476748   1.300  0.19381    
    ## gap_openness.tste_17_0_ct            0.0223739  0.0488333   0.458  0.64688    
    ## star_GS                              0.1828342  0.0608594   3.004  0.00270 ** 
    ## gap_extraversion.tste_17_1_ct       -0.0119560  0.0258236  -0.463  0.64342    
    ## gap_agreeableness.tste_17_1_ct      -0.0231808  0.0378650  -0.612  0.54048    
    ## gap_conscientiousness.tste_17_1_ct  -0.0366173  0.0424421  -0.863  0.38837    
    ## gap_emotionstability.tste_17_1_ct    0.0593937  0.0315129   1.885  0.05961 .  
    ## gap_openness.tste_17_1_ct            0.0754683  0.0435374   1.733  0.08317 .  
    ## gap_extraversion.tste_17_2_ct       -0.0487316  0.0328396  -1.484  0.13798    
    ## gap_agreeableness.tste_17_2_ct       0.0275973  0.0468904   0.589  0.55623    
    ## gap_conscientiousness.tste_17_2_ct  -0.1102355  0.0508813  -2.167  0.03039 *  
    ## gap_emotionstability.tste_17_2_ct   -0.0831101  0.0381232  -2.180  0.02937 *  
    ## gap_openness.tste_17_2_ct           -0.0246662  0.0510451  -0.483  0.62899    
    ## gap_extraversion.tste_17_3_ct       -0.0083208  0.0299319  -0.278  0.78105    
    ## gap_agreeableness.tste_17_3_ct      -0.0337359  0.0424469  -0.795  0.42683    
    ## gap_emotionstability.tste_17_3_ct   -0.0257224  0.0356193  -0.722  0.47029    
    ## gap_openness.tste_17_3_ct            0.0546786  0.0459076   1.191  0.23377    
    ## gap_extraversion.tste_17_4_ct        0.0053364  0.0315473   0.169  0.86569    
    ## gap_agreeableness.tste_17_4_ct      -0.0748065  0.0444043  -1.685  0.09221 .  
    ## gap_conscientiousness.tste_17_4_ct  -0.0516876  0.0477487  -1.082  0.27916    
    ## gap_openness.tste_17_4_ct            0.0081428  0.0515328   0.158  0.87446    
    ## gap_extraversion.tste_17_5_ct        0.0455737  0.0268622   1.697  0.08993 .  
    ## gap_agreeableness.tste_17_5_ct      -0.0072668  0.0365348  -0.199  0.84236    
    ## gap_conscientiousness.tste_17_5_ct  -0.0268524  0.0406411  -0.661  0.50887    
    ## gap_openness.tste_17_5_ct           -0.0886454  0.0421278  -2.104  0.03548 *  
    ## gap_extraversion.tste_17_6_ct       -0.0046104  0.0303766  -0.152  0.87938    
    ## gap_agreeableness.tste_17_6_ct       0.0668684  0.0458138   1.460  0.14456    
    ## gap_conscientiousness.tste_17_6_ct  -0.0943090  0.0500907  -1.883  0.05988 .  
    ## gap_emotionstability.tste_17_6_ct   -0.0026089  0.0369020  -0.071  0.94365    
    ## gap_openness.tste_17_6_ct            0.0151679  0.0511609   0.296  0.76690    
    ## gap_extraversion.tste_17_7_ct       -0.0093286  0.0274582  -0.340  0.73409    
    ## gap_agreeableness.tste_17_7_ct      -0.0416062  0.0365723  -1.138  0.25540    
    ## gap_conscientiousness.tste_17_7_ct   0.0043084  0.0414348   0.104  0.91719    
    ## gap_openness.tste_17_7_ct            0.0401927  0.0430569   0.933  0.35068    
    ## gap_extraversion.tste_17_8_ct        0.0419018  0.0304682   1.375  0.16920    
    ## gap_agreeableness.tste_17_8_ct      -0.0747976  0.0441167  -1.695  0.09014 .  
    ## gap_emotionstability.tste_17_8_ct   -0.0446349  0.0360639  -1.238  0.21598    
    ## gap_openness.tste_17_8_ct           -0.0264796  0.0481630  -0.550  0.58252    
    ## gap_extraversion.tste_17_9_ct        0.0236732  0.0300330   0.788  0.43065    
    ## gap_agreeableness.tste_17_9_ct      -0.0231634  0.0390561  -0.593  0.55319    
    ## gap_conscientiousness.tste_17_9_ct  -0.0742740  0.0441713  -1.681  0.09282 .  
    ## gap_openness.tste_17_9_ct           -0.0529790  0.0455464  -1.163  0.24489    
    ## gap_extraversion.tste_17_10_ct       0.0222059  0.0271465   0.818  0.41345    
    ## gap_agreeableness.tste_17_10_ct      0.0755941  0.0392964   1.924  0.05453 .  
    ## gap_emotionstability.tste_17_10_ct   0.0052118  0.0321767   0.162  0.87134    
    ## gap_openness.tste_17_10_ct           0.0352379  0.0440436   0.800  0.42376    
    ## gap_extraversion.tste_17_11_ct      -0.0161517  0.0282091  -0.573  0.56700    
    ## gap_agreeableness.tste_17_11_ct      0.0343696  0.0397333   0.865  0.38714    
    ## gap_conscientiousness.tste_17_11_ct  0.0989290  0.0455396   2.172  0.02994 *  
    ## gap_openness.tste_17_11_ct          -0.0212792  0.0434811  -0.489  0.62462    
    ## gap_agreeableness.tste_17_12_ct      0.0058148  0.0346720   0.168  0.86683    
    ## gap_openness.tste_17_12_ct          -0.0294763  0.0394792  -0.747  0.45537    
    ## gap_extraversion.tste_17_13_ct      -0.0004564  0.0280655  -0.016  0.98703    
    ## gap_agreeableness.tste_17_13_ct      0.0077300  0.0371835   0.208  0.83534    
    ## gap_openness.tste_17_13_ct          -0.0483942  0.0447131  -1.082  0.27924    
    ## gap_extraversion.tste_17_14_ct       0.0269012  0.0270628   0.994  0.32033    
    ## gap_agreeableness.tste_17_14_ct     -0.0447454  0.0357247  -1.253  0.21053    
    ## gap_conscientiousness.tste_17_14_ct -0.0311914  0.0432656  -0.721  0.47104    
    ## gap_openness.tste_17_14_ct          -0.0044392  0.0421404  -0.105  0.91611    
    ## gap_extraversion.tste_17_15_ct       0.0271441  0.0315716   0.860  0.39002    
    ## gap_agreeableness.tste_17_15_ct     -0.0468909  0.0450103  -1.042  0.29764    
    ## gap_conscientiousness.tste_17_15_ct  0.1280736  0.0493103   2.597  0.00946 ** 
    ## gap_openness.tste_17_15_ct          -0.0333960  0.0518333  -0.644  0.51946    
    ## gap_extraversion.tste_17_16_ct      -0.0080102  0.0261958  -0.306  0.75980    
    ## gap_agreeableness.tste_17_16_ct      0.0250490  0.0352828   0.710  0.47782    
    ## gap_conscientiousness.tste_17_16_ct -0.0428410  0.0386914  -1.107  0.26832    
    ## gap_openness.tste_17_16_ct          -0.0025506  0.0391399  -0.065  0.94805    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.434 on 2032 degrees of freedom
    ## Multiple R-squared:  0.1328, Adjusted R-squared:  0.08797 
    ## F-statistic: 2.963 on 105 and 2032 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1768 -0.6814  0.2506  1.0301  3.5010 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         48.1751407 18.2790183   2.636 0.008464 ** 
    ## real_extraversion_ct                 0.0513139  0.0251741   2.038 0.041643 *  
    ## real_agreeableness_ct                0.0261240  0.0314801   0.830 0.406717    
    ## real_conscientiousness_ct            0.0868099  0.0350309   2.478 0.013289 *  
    ## real_emotionstability_ct            -0.0237025  0.0347708  -0.682 0.495520    
    ## real_openness_ct                     0.0905356  0.0333962   2.711 0.006765 ** 
    ## tste_18_0_ct                        -0.0927933  0.0589119  -1.575 0.115385    
    ## tste_18_1_ct                         0.1820228  0.0589460   3.088 0.002043 ** 
    ## tste_18_2_ct                        -0.0648031  0.0537457  -1.206 0.228060    
    ## tste_18_3_ct                        -0.0006111  0.0582167  -0.010 0.991626    
    ## tste_18_4_ct                         0.0819085  0.0633096   1.294 0.195890    
    ## tste_18_5_ct                        -0.0590807  0.0615036  -0.961 0.336865    
    ## tste_18_6_ct                        -0.0327585  0.0620097  -0.528 0.597363    
    ## tste_18_7_ct                        -0.0773621  0.0550671  -1.405 0.160213    
    ## tste_18_8_ct                         0.0132188  0.0599921   0.220 0.825627    
    ## tste_18_9_ct                        -0.0797321  0.0605624  -1.317 0.188145    
    ## tste_18_10_ct                        0.1014018  0.0586061   1.730 0.083742 .  
    ## tste_18_11_ct                        0.0163167  0.0565216   0.289 0.772855    
    ## tste_18_12_ct                       -0.0489007  0.0603453  -0.810 0.417836    
    ## tste_18_13_ct                       -0.0437933  0.0593565  -0.738 0.460720    
    ## tste_18_14_ct                        0.0556922  0.0640679   0.869 0.384803    
    ## tste_18_15_ct                        0.2632024  0.0720139   3.655 0.000264 ***
    ## tste_18_16_ct                       -0.3046532  0.0632480  -4.817 1.57e-06 ***
    ## tste_18_17_ct                        0.0551491  0.0585732   0.942 0.346539    
    ## release                             -0.0211887  0.0089717  -2.362 0.018285 *  
    ## gap_openness                        -0.0457087  0.0440558  -1.038 0.299618    
    ## gap_extraversion.tste_18_16_ct       0.0069373  0.0318148   0.218 0.827411    
    ## gap_emotionstability.tste_18_9_ct    0.1225800  0.0371295   3.301 0.000979 ***
    ## income                               0.0106293  0.0173317   0.613 0.539754    
    ## gap_extraversion                    -0.0032015  0.0260866  -0.123 0.902335    
    ## gap_agreeableness                    0.0658572  0.0340532   1.934 0.053258 .  
    ## gap_emotionstability                -0.0434462  0.0318160  -1.366 0.172234    
    ## age                                 -0.0114597  0.0046186  -2.481 0.013174 *  
    ## sex2                                -0.0843969  0.0726556  -1.162 0.245533    
    ## gap_conscientiousness                0.0106264  0.0406931   0.261 0.794014    
    ## race4                               -0.1983405  0.1419676  -1.397 0.162541    
    ## education                            0.0081075  0.0266717   0.304 0.761178    
    ## race2                                0.1062784  0.1320009   0.805 0.420837    
    ## star_user                           -0.0988736  0.0782295  -1.264 0.206415    
    ## gap_extraversion.tste_18_0_ct       -0.0225715  0.0301439  -0.749 0.454070    
    ## gap_agreeableness.tste_18_0_ct       0.0142107  0.0399636   0.356 0.722184    
    ## gap_conscientiousness.tste_18_0_ct   0.0295060  0.0455577   0.648 0.517276    
    ## gap_openness.tste_18_0_ct           -0.0196724  0.0466393  -0.422 0.673216    
    ## star_GS                              0.1108440  0.0617097   1.796 0.072609 .  
    ## gap_extraversion.tste_18_1_ct       -0.0419219  0.0273071  -1.535 0.124891    
    ## gap_agreeableness.tste_18_1_ct      -0.0085126  0.0381357  -0.223 0.823388    
    ## gap_conscientiousness.tste_18_1_ct  -0.0683983  0.0413499  -1.654 0.098254 .  
    ## gap_openness.tste_18_1_ct            0.0723750  0.0437668   1.654 0.098353 .  
    ## gap_extraversion.tste_18_2_ct        0.0051461  0.0261185   0.197 0.843825    
    ## gap_agreeableness.tste_18_2_ct       0.0715193  0.0354519   2.017 0.043789 *  
    ## gap_conscientiousness.tste_18_2_ct   0.0757818  0.0404023   1.876 0.060843 .  
    ## gap_openness.tste_18_2_ct           -0.0033950  0.0409709  -0.083 0.933969    
    ## gap_extraversion.tste_18_3_ct       -0.0302523  0.0287083  -1.054 0.292107    
    ## gap_agreeableness.tste_18_3_ct       0.0004237  0.0410157   0.010 0.991759    
    ## gap_conscientiousness.tste_18_3_ct  -0.0689838  0.0464552  -1.485 0.137712    
    ## gap_emotionstability.tste_18_3_ct   -0.0234387  0.0348418  -0.673 0.501204    
    ## gap_openness.tste_18_3_ct            0.0440074  0.0439156   1.002 0.316420    
    ## gap_extraversion.tste_18_4_ct       -0.0232053  0.0320176  -0.725 0.468679    
    ## gap_agreeableness.tste_18_4_ct      -0.0006871  0.0438200  -0.016 0.987491    
    ## gap_conscientiousness.tste_18_4_ct   0.0417232  0.0478953   0.871 0.383784    
    ## gap_openness.tste_18_4_ct            0.0014508  0.0504310   0.029 0.977053    
    ## gap_extraversion.tste_18_5_ct       -0.0086659  0.0302590  -0.286 0.774607    
    ## gap_agreeableness.tste_18_5_ct       0.0232309  0.0396714   0.586 0.558221    
    ## gap_conscientiousness.tste_18_5_ct   0.1479399  0.0442998   3.340 0.000854 ***
    ## gap_openness.tste_18_5_ct            0.0121891  0.0459166   0.265 0.790681    
    ## gap_extraversion.tste_18_6_ct        0.0294148  0.0306084   0.961 0.336663    
    ## gap_agreeableness.tste_18_6_ct      -0.0267158  0.0405933  -0.658 0.510528    
    ## gap_conscientiousness.tste_18_6_ct   0.0297148  0.0452951   0.656 0.511881    
    ## gap_openness.tste_18_6_ct            0.0278374  0.0460277   0.605 0.545382    
    ## gap_extraversion.tste_18_7_ct       -0.0015845  0.0274200  -0.058 0.953924    
    ## gap_agreeableness.tste_18_7_ct       0.0402585  0.0391098   1.029 0.303429    
    ## gap_conscientiousness.tste_18_7_ct  -0.0150682  0.0413846  -0.364 0.715819    
    ## gap_openness.tste_18_7_ct            0.0333721  0.0426407   0.783 0.433933    
    ## gap_extraversion.tste_18_8_ct        0.0493593  0.0295546   1.670 0.095052 .  
    ## gap_agreeableness.tste_18_8_ct       0.0024881  0.0415018   0.060 0.952200    
    ## gap_conscientiousness.tste_18_8_ct  -0.0178647  0.0439295  -0.407 0.684295    
    ## gap_openness.tste_18_8_ct            0.0179008  0.0478156   0.374 0.708167    
    ## gap_extraversion.tste_18_9_ct       -0.0387113  0.0306482  -1.263 0.206704    
    ## gap_agreeableness.tste_18_9_ct       0.0447134  0.0428507   1.043 0.296856    
    ## gap_conscientiousness.tste_18_9_ct   0.0212666  0.0467552   0.455 0.649265    
    ## gap_openness.tste_18_9_ct            0.0129634  0.0482720   0.269 0.788304    
    ## gap_extraversion.tste_18_10_ct       0.0070897  0.0291718   0.243 0.808005    
    ## gap_agreeableness.tste_18_10_ct     -0.0966962  0.0397672  -2.432 0.015120 *  
    ## gap_conscientiousness.tste_18_10_ct  0.0505526  0.0437739   1.155 0.248285    
    ## gap_openness.tste_18_10_ct           0.0163698  0.0466973   0.351 0.725961    
    ## gap_extraversion.tste_18_11_ct      -0.0165232  0.0286676  -0.576 0.564427    
    ## gap_agreeableness.tste_18_11_ct      0.0761331  0.0415861   1.831 0.067287 .  
    ## gap_conscientiousness.tste_18_11_ct  0.0103176  0.0457872   0.225 0.821738    
    ## gap_emotionstability.tste_18_11_ct  -0.0217794  0.0331206  -0.658 0.510883    
    ## gap_openness.tste_18_11_ct           0.0584139  0.0454266   1.286 0.198626    
    ## gap_extraversion.tste_18_12_ct      -0.0030860  0.0288795  -0.107 0.914912    
    ## gap_agreeableness.tste_18_12_ct     -0.0435337  0.0419684  -1.037 0.299721    
    ## gap_emotionstability.tste_18_12_ct  -0.0147023  0.0348425  -0.422 0.673096    
    ## gap_openness.tste_18_12_ct          -0.0124865  0.0462944  -0.270 0.787404    
    ## gap_extraversion.tste_18_13_ct      -0.0002965  0.0293137  -0.010 0.991931    
    ## gap_agreeableness.tste_18_13_ct     -0.0284424  0.0430582  -0.661 0.508971    
    ## gap_emotionstability.tste_18_13_ct  -0.0316477  0.0347996  -0.909 0.363233    
    ## gap_openness.tste_18_13_ct           0.0203748  0.0486919   0.418 0.675668    
    ## gap_extraversion.tste_18_14_ct      -0.0281846  0.0307255  -0.917 0.359093    
    ## gap_agreeableness.tste_18_14_ct     -0.0149643  0.0416243  -0.360 0.719253    
    ## gap_openness.tste_18_14_ct           0.0500673  0.0497575   1.006 0.314426    
    ## gap_extraversion.tste_18_15_ct      -0.0648439  0.0327068  -1.983 0.047549 *  
    ## gap_agreeableness.tste_18_15_ct     -0.0093133  0.0440655  -0.211 0.832635    
    ## gap_openness.tste_18_15_ct          -0.0773556  0.0531270  -1.456 0.145533    
    ## gap_agreeableness.tste_18_16_ct     -0.0012606  0.0439504  -0.029 0.977121    
    ## gap_conscientiousness.tste_18_16_ct -0.0300820  0.0487600  -0.617 0.537344    
    ## gap_openness.tste_18_16_ct          -0.0150151  0.0507554  -0.296 0.767388    
    ## gap_extraversion.tste_18_17_ct      -0.0316992  0.0309560  -1.024 0.305954    
    ## gap_agreeableness.tste_18_17_ct     -0.0079647  0.0434851  -0.183 0.854691    
    ## gap_emotionstability.tste_18_17_ct  -0.0015018  0.0322355  -0.047 0.962846    
    ## gap_openness.tste_18_17_ct           0.0598824  0.0496470   1.206 0.227896    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.436 on 2027 degrees of freedom
    ## Multiple R-squared:  0.1333, Adjusted R-squared:  0.08628 
    ## F-statistic: 2.834 on 110 and 2027 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3190 -0.6763  0.2384  1.0587  3.6233 
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                         35.799259  18.755926   1.909  0.05644 . 
    ## real_extraversion_ct                 0.052988   0.025348   2.090  0.03671 * 
    ## real_agreeableness_ct                0.023912   0.031670   0.755  0.45032   
    ## real_conscientiousness_ct            0.087210   0.035267   2.473  0.01349 * 
    ## real_emotionstability_ct            -0.020609   0.035013  -0.589  0.55618   
    ## real_openness_ct                     0.095190   0.033525   2.839  0.00457 **
    ## tste_19_0_ct                         0.003483   0.059698   0.058  0.95348   
    ## tste_19_1_ct                         0.060250   0.065004   0.927  0.35410   
    ## tste_19_2_ct                         0.027578   0.060840   0.453  0.65040   
    ## tste_19_3_ct                        -0.146142   0.059070  -2.474  0.01344 * 
    ## tste_19_4_ct                        -0.084588   0.063008  -1.342  0.17959   
    ## tste_19_5_ct                        -0.001777   0.068126  -0.026  0.97919   
    ## tste_19_6_ct                         0.090150   0.062532   1.442  0.14956   
    ## tste_19_7_ct                         0.096988   0.058226   1.666  0.09592 . 
    ## tste_19_8_ct                         0.050798   0.058322   0.871  0.38387   
    ## tste_19_9_ct                        -0.067641   0.063165  -1.071  0.28436   
    ## tste_19_10_ct                        0.147665   0.061114   2.416  0.01577 * 
    ## tste_19_11_ct                        0.123634   0.056075   2.205  0.02758 * 
    ## tste_19_12_ct                        0.184724   0.056957   3.243  0.00120 **
    ## tste_19_13_ct                       -0.004580   0.055365  -0.083  0.93407   
    ## tste_19_14_ct                        0.153488   0.056703   2.707  0.00685 **
    ## tste_19_15_ct                        0.027506   0.057082   0.482  0.62994   
    ## tste_19_16_ct                        0.043781   0.059475   0.736  0.46174   
    ## tste_19_17_ct                        0.109785   0.064335   1.706  0.08808 . 
    ## tste_19_18_ct                       -0.036267   0.052713  -0.688  0.49153   
    ## release                             -0.015371   0.009207  -1.669  0.09517 . 
    ## gap_openness                        -0.050892   0.044138  -1.153  0.24903   
    ## gap_extraversion.tste_19_12_ct       0.043734   0.026561   1.647  0.09981 . 
    ## income                               0.013874   0.017384   0.798  0.42491   
    ## gap_extraversion                    -0.003064   0.026198  -0.117  0.90692   
    ## gap_agreeableness                    0.074909   0.034195   2.191  0.02859 * 
    ## gap_emotionstability                -0.038777   0.032012  -1.211  0.22591   
    ## age                                 -0.012270   0.004659  -2.634  0.00851 **
    ## sex2                                -0.095436   0.073169  -1.304  0.19227   
    ## gap_conscientiousness                0.005803   0.040636   0.143  0.88645   
    ## race4                               -0.216709   0.142597  -1.520  0.12873   
    ## education                            0.009053   0.026837   0.337  0.73589   
    ## race2                                0.081925   0.132668   0.618  0.53696   
    ## star_user                           -0.029483   0.075870  -0.389  0.69762   
    ## star_GS                              0.125944   0.061265   2.056  0.03994 * 
    ## gap_extraversion.tste_19_0_ct        0.021456   0.027277   0.787  0.43162   
    ## gap_agreeableness.tste_19_0_ct       0.033506   0.037626   0.891  0.37330   
    ## gap_conscientiousness.tste_19_0_ct  -0.051513   0.042452  -1.213  0.22510   
    ## gap_openness.tste_19_0_ct           -0.035178   0.043853  -0.802  0.42255   
    ## gap_extraversion.tste_19_1_ct       -0.020368   0.032868  -0.620  0.53553   
    ## gap_agreeableness.tste_19_1_ct       0.023887   0.043498   0.549  0.58296   
    ## gap_openness.tste_19_1_ct            0.024068   0.050231   0.479  0.63189   
    ## gap_extraversion.tste_19_2_ct       -0.029307   0.030466  -0.962  0.33618   
    ## gap_agreeableness.tste_19_2_ct       0.101024   0.043510   2.322  0.02034 * 
    ## gap_emotionstability.tste_19_2_ct   -0.023328   0.036105  -0.646  0.51828   
    ## gap_openness.tste_19_2_ct           -0.042286   0.049598  -0.853  0.39400   
    ## gap_extraversion.tste_19_3_ct        0.008837   0.029864   0.296  0.76732   
    ## gap_agreeableness.tste_19_3_ct       0.044042   0.041688   1.056  0.29089   
    ## gap_conscientiousness.tste_19_3_ct   0.022413   0.045376   0.494  0.62139   
    ## gap_openness.tste_19_3_ct            0.006992   0.044675   0.157  0.87564   
    ## gap_extraversion.tste_19_4_ct       -0.004844   0.031339  -0.155  0.87718   
    ## gap_agreeableness.tste_19_4_ct      -0.010210   0.042757  -0.239  0.81128   
    ## gap_conscientiousness.tste_19_4_ct  -0.012501   0.052139  -0.240  0.81054   
    ## gap_openness.tste_19_4_ct           -0.040618   0.050046  -0.812  0.41710   
    ## gap_extraversion.tste_19_5_ct       -0.030980   0.031788  -0.975  0.32988   
    ## gap_agreeableness.tste_19_5_ct       0.008935   0.042929   0.208  0.83514   
    ## gap_conscientiousness.tste_19_5_ct  -0.049772   0.047785  -1.042  0.29773   
    ## gap_openness.tste_19_5_ct           -0.013318   0.049888  -0.267  0.78953   
    ## gap_extraversion.tste_19_6_ct        0.016482   0.030388   0.542  0.58762   
    ## gap_agreeableness.tste_19_6_ct      -0.095002   0.041785  -2.274  0.02310 * 
    ## gap_conscientiousness.tste_19_6_ct  -0.041905   0.048499  -0.864  0.38767   
    ## gap_openness.tste_19_6_ct            0.033274   0.048293   0.689  0.49091   
    ## gap_extraversion.tste_19_7_ct        0.005521   0.028930   0.191  0.84867   
    ## gap_agreeableness.tste_19_7_ct       0.002114   0.038948   0.054  0.95673   
    ## gap_openness.tste_19_7_ct           -0.013625   0.045076  -0.302  0.76249   
    ## gap_extraversion.tste_19_8_ct       -0.028464   0.028979  -0.982  0.32612   
    ## gap_agreeableness.tste_19_8_ct      -0.034934   0.038703  -0.903  0.36683   
    ## gap_conscientiousness.tste_19_8_ct   0.088716   0.044884   1.977  0.04823 * 
    ## gap_openness.tste_19_8_ct            0.027667   0.045527   0.608  0.54344   
    ## gap_extraversion.tste_19_9_ct        0.026176   0.031435   0.833  0.40511   
    ## gap_agreeableness.tste_19_9_ct       0.005522   0.043839   0.126  0.89978   
    ## gap_conscientiousness.tste_19_9_ct  -0.075498   0.046665  -1.618  0.10585   
    ## gap_openness.tste_19_9_ct           -0.087665   0.048631  -1.803  0.07159 . 
    ## gap_extraversion.tste_19_10_ct      -0.040126   0.029014  -1.383  0.16681   
    ## gap_agreeableness.tste_19_10_ct      0.049083   0.040969   1.198  0.23103   
    ## gap_conscientiousness.tste_19_10_ct  0.008787   0.045295   0.194  0.84620   
    ## gap_openness.tste_19_10_ct           0.039944   0.045834   0.871  0.38359   
    ## gap_extraversion.tste_19_11_ct      -0.003043   0.027322  -0.111  0.91133   
    ## gap_agreeableness.tste_19_11_ct     -0.038093   0.035454  -1.074  0.28274   
    ## gap_conscientiousness.tste_19_11_ct  0.011581   0.040668   0.285  0.77585   
    ## gap_openness.tste_19_11_ct          -0.047762   0.042021  -1.137  0.25584   
    ## gap_agreeableness.tste_19_12_ct     -0.020403   0.037037  -0.551  0.58177   
    ## gap_conscientiousness.tste_19_12_ct -0.024141   0.040417  -0.597  0.55038   
    ## gap_openness.tste_19_12_ct           0.011688   0.042452   0.275  0.78309   
    ## gap_extraversion.tste_19_13_ct       0.003070   0.027516   0.112  0.91117   
    ## gap_agreeableness.tste_19_13_ct      0.025126   0.040366   0.622  0.53371   
    ## gap_conscientiousness.tste_19_13_ct -0.089882   0.044976  -1.998  0.04580 * 
    ## gap_emotionstability.tste_19_13_ct   0.077092   0.034368   2.243  0.02500 * 
    ## gap_openness.tste_19_13_ct           0.033889   0.045316   0.748  0.45465   
    ## gap_extraversion.tste_19_14_ct       0.033927   0.028983   1.171  0.24189   
    ## gap_agreeableness.tste_19_14_ct     -0.035078   0.043929  -0.799  0.42466   
    ## gap_emotionstability.tste_19_14_ct  -0.007632   0.033632  -0.227  0.82051   
    ## gap_openness.tste_19_14_ct          -0.018871   0.046628  -0.405  0.68572   
    ## gap_extraversion.tste_19_15_ct       0.017503   0.028437   0.616  0.53829   
    ## gap_agreeableness.tste_19_15_ct     -0.013560   0.040963  -0.331  0.74066   
    ## gap_emotionstability.tste_19_15_ct   0.028828   0.031895   0.904  0.36620   
    ## gap_openness.tste_19_15_ct          -0.076101   0.044477  -1.711  0.08723 . 
    ## gap_extraversion.tste_19_16_ct      -0.053720   0.029276  -1.835  0.06666 . 
    ## gap_agreeableness.tste_19_16_ct      0.054040   0.041664   1.297  0.19476   
    ## gap_conscientiousness.tste_19_16_ct  0.008542   0.044522   0.192  0.84787   
    ## gap_emotionstability.tste_19_16_ct  -0.043398   0.034100  -1.273  0.20328   
    ## gap_openness.tste_19_16_ct           0.027904   0.046961   0.594  0.55245   
    ## gap_extraversion.tste_19_17_ct      -0.020744   0.030554  -0.679  0.49726   
    ## gap_agreeableness.tste_19_17_ct     -0.041708   0.040086  -1.040  0.29826   
    ## gap_openness.tste_19_17_ct          -0.044907   0.047710  -0.941  0.34669   
    ## gap_extraversion.tste_19_18_ct       0.008737   0.026895   0.325  0.74533   
    ## gap_agreeableness.tste_19_18_ct      0.040361   0.036611   1.102  0.27041   
    ## gap_conscientiousness.tste_19_18_ct -0.034589   0.039932  -0.866  0.38649   
    ## gap_openness.tste_19_18_ct           0.040818   0.040851   0.999  0.31782   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.442 on 2024 degrees of freedom
    ## Multiple R-squared:  0.1264, Adjusted R-squared:  0.07764 
    ## F-statistic: 2.592 on 113 and 2024 DF,  p-value: 2.666e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0959 -0.6747  0.2500  1.0190  3.5020 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         36.9602470 19.5402133   1.891  0.05870 .  
    ## real_extraversion_ct                 0.0515835  0.0252389   2.044  0.04110 *  
    ## real_agreeableness_ct                0.0266547  0.0315255   0.845  0.39793    
    ## real_conscientiousness_ct            0.0772685  0.0351148   2.200  0.02789 *  
    ## real_emotionstability_ct            -0.0246873  0.0348179  -0.709  0.47838    
    ## real_openness_ct                     0.0957157  0.0334000   2.866  0.00420 ** 
    ## tste_20_0_ct                         0.0254386  0.0665991   0.382  0.70253    
    ## tste_20_1_ct                         0.0777784  0.0580429   1.340  0.18039    
    ## tste_20_2_ct                         0.1011431  0.0543282   1.862  0.06279 .  
    ## tste_20_3_ct                         0.1106444  0.0617753   1.791  0.07343 .  
    ## tste_20_4_ct                         0.0276228  0.0555223   0.498  0.61888    
    ## tste_20_5_ct                        -0.0650851  0.0565370  -1.151  0.24979    
    ## tste_20_6_ct                         0.1082113  0.0653990   1.655  0.09815 .  
    ## tste_20_7_ct                         0.2781625  0.0679119   4.096 4.37e-05 ***
    ## tste_20_8_ct                         0.0005259  0.0659681   0.008  0.99364    
    ## tste_20_9_ct                         0.1041370  0.0570969   1.824  0.06832 .  
    ## tste_20_10_ct                        0.0900096  0.0620726   1.450  0.14719    
    ## tste_20_11_ct                       -0.1428541  0.0577390  -2.474  0.01344 *  
    ## tste_20_12_ct                        0.1078173  0.0589474   1.829  0.06754 .  
    ## tste_20_13_ct                        0.1605865  0.0632036   2.541  0.01113 *  
    ## tste_20_14_ct                       -0.1297814  0.0517901  -2.506  0.01229 *  
    ## tste_20_15_ct                        0.1151904  0.0668508   1.723  0.08502 .  
    ## tste_20_16_ct                       -0.0487219  0.0583233  -0.835  0.40360    
    ## tste_20_17_ct                        0.0781583  0.0556844   1.404  0.16059    
    ## tste_20_18_ct                        0.1030661  0.0535058   1.926  0.05421 .  
    ## tste_20_19_ct                       -0.0058721  0.0600630  -0.098  0.92213    
    ## release                             -0.0158237  0.0095925  -1.650  0.09918 .  
    ## gap_openness                        -0.0454591  0.0440433  -1.032  0.30213    
    ## income                               0.0133084  0.0173390   0.768  0.44285    
    ## gap_extraversion                    -0.0055463  0.0260051  -0.213  0.83113    
    ## gap_agreeableness                    0.0707600  0.0340476   2.078  0.03781 *  
    ## gap_emotionstability                -0.0445829  0.0318688  -1.399  0.16198    
    ## age                                 -0.0113357  0.0046371  -2.445  0.01459 *  
    ## sex2                                -0.0797621  0.0729590  -1.093  0.27442    
    ## gap_conscientiousness                0.0051995  0.0406697   0.128  0.89828    
    ## race4                               -0.1791144  0.1422815  -1.259  0.20822    
    ## education                            0.0041912  0.0266746   0.157  0.87516    
    ## race2                                0.1050789  0.1321226   0.795  0.42652    
    ## star_user                           -0.0207471  0.0794310  -0.261  0.79397    
    ## star_GS                              0.0851303  0.0622906   1.367  0.17188    
    ## gap_extraversion.tste_20_0_ct        0.0084404  0.0318072   0.265  0.79076    
    ## gap_agreeableness.tste_20_0_ct       0.0572912  0.0431735   1.327  0.18466    
    ## gap_conscientiousness.tste_20_0_ct   0.1034993  0.0501182   2.065  0.03904 *  
    ## gap_openness.tste_20_0_ct           -0.0033767  0.0492152  -0.069  0.94531    
    ## gap_extraversion.tste_20_1_ct        0.0093670  0.0287350   0.326  0.74447    
    ## gap_agreeableness.tste_20_1_ct       0.0265626  0.0387279   0.686  0.49287    
    ## gap_conscientiousness.tste_20_1_ct  -0.0655088  0.0416227  -1.574  0.11567    
    ## gap_openness.tste_20_1_ct            0.0308618  0.0453144   0.681  0.49591    
    ## gap_extraversion.tste_20_2_ct       -0.0002914  0.0284328  -0.010  0.99182    
    ## gap_agreeableness.tste_20_2_ct      -0.0155241  0.0370405  -0.419  0.67518    
    ## gap_openness.tste_20_2_ct           -0.0006859  0.0410121  -0.017  0.98666    
    ## gap_extraversion.tste_20_3_ct        0.0451557  0.0310636   1.454  0.14620    
    ## gap_agreeableness.tste_20_3_ct      -0.0471561  0.0410721  -1.148  0.25105    
    ## gap_conscientiousness.tste_20_3_ct   0.0216931  0.0470427   0.461  0.64475    
    ## gap_openness.tste_20_3_ct            0.0209958  0.0492852   0.426  0.67015    
    ## gap_extraversion.tste_20_4_ct       -0.0328628  0.0266756  -1.232  0.21811    
    ## gap_agreeableness.tste_20_4_ct       0.0916715  0.0383577   2.390  0.01694 *  
    ## gap_conscientiousness.tste_20_4_ct  -0.0342428  0.0436232  -0.785  0.43256    
    ## gap_openness.tste_20_4_ct            0.0189794  0.0443999   0.427  0.66909    
    ## gap_extraversion.tste_20_5_ct       -0.0022244  0.0282218  -0.079  0.93719    
    ## gap_agreeableness.tste_20_5_ct       0.0228620  0.0389616   0.587  0.55742    
    ## gap_conscientiousness.tste_20_5_ct  -0.1019252  0.0427349  -2.385  0.01717 *  
    ## gap_openness.tste_20_5_ct            0.0059708  0.0431860   0.138  0.89005    
    ## gap_extraversion.tste_20_6_ct       -0.0764346  0.0316624  -2.414  0.01586 *  
    ## gap_agreeableness.tste_20_6_ct      -0.0026006  0.0433896  -0.060  0.95221    
    ## gap_conscientiousness.tste_20_6_ct   0.0046769  0.0485243   0.096  0.92323    
    ## gap_openness.tste_20_6_ct            0.0453725  0.0504691   0.899  0.36875    
    ## gap_extraversion.tste_20_7_ct       -0.0769515  0.0337040  -2.283  0.02252 *  
    ## gap_agreeableness.tste_20_7_ct       0.0088954  0.0477393   0.186  0.85220    
    ## gap_conscientiousness.tste_20_7_ct  -0.0005452  0.0510871  -0.011  0.99149    
    ## gap_emotionstability.tste_20_7_ct   -0.0380863  0.0408592  -0.932  0.35138    
    ## gap_openness.tste_20_7_ct           -0.0641555  0.0523734  -1.225  0.22073    
    ## gap_extraversion.tste_20_8_ct       -0.0046582  0.0312331  -0.149  0.88145    
    ## gap_agreeableness.tste_20_8_ct      -0.0798830  0.0411884  -1.939  0.05258 .  
    ## gap_openness.tste_20_8_ct            0.0650949  0.0472082   1.379  0.16808    
    ## gap_extraversion.tste_20_9_ct        0.0302378  0.0289725   1.044  0.29676    
    ## gap_agreeableness.tste_20_9_ct      -0.0578312  0.0390797  -1.480  0.13908    
    ## gap_conscientiousness.tste_20_9_ct   0.0098033  0.0416243   0.236  0.81383    
    ## gap_openness.tste_20_9_ct           -0.0463382  0.0451854  -1.026  0.30524    
    ## gap_extraversion.tste_20_10_ct      -0.0091387  0.0302629  -0.302  0.76270    
    ## gap_agreeableness.tste_20_10_ct     -0.0441075  0.0413834  -1.066  0.28663    
    ## gap_conscientiousness.tste_20_10_ct -0.0093462  0.0469469  -0.199  0.84222    
    ## gap_emotionstability.tste_20_10_ct   0.0001344  0.0328116   0.004  0.99673    
    ## gap_openness.tste_20_10_ct           0.0018632  0.0489428   0.038  0.96964    
    ## gap_extraversion.tste_20_11_ct       0.0474046  0.0285864   1.658  0.09741 .  
    ## gap_agreeableness.tste_20_11_ct      0.0169655  0.0400335   0.424  0.67177    
    ## gap_conscientiousness.tste_20_11_ct -0.0186809  0.0461650  -0.405  0.68577    
    ## gap_openness.tste_20_11_ct          -0.0867989  0.0450511  -1.927  0.05416 .  
    ## gap_extraversion.tste_20_12_ct      -0.0083992  0.0304700  -0.276  0.78284    
    ## gap_agreeableness.tste_20_12_ct      0.0262427  0.0433732   0.605  0.54522    
    ## gap_emotionstability.tste_20_12_ct  -0.0937497  0.0329072  -2.849  0.00443 ** 
    ## gap_openness.tste_20_12_ct          -0.0316938  0.0484422  -0.654  0.51302    
    ## gap_extraversion.tste_20_13_ct      -0.0695282  0.0313312  -2.219  0.02659 *  
    ## gap_agreeableness.tste_20_13_ct      0.0171496  0.0459580   0.373  0.70907    
    ## gap_conscientiousness.tste_20_13_ct  0.0873177  0.0493662   1.769  0.07708 .  
    ## gap_openness.tste_20_13_ct           0.0190615  0.0504603   0.378  0.70565    
    ## gap_extraversion.tste_20_14_ct       0.0291194  0.0256377   1.136  0.25617    
    ## gap_agreeableness.tste_20_14_ct      0.0354780  0.0350568   1.012  0.31165    
    ## gap_conscientiousness.tste_20_14_ct -0.0491599  0.0406103  -1.211  0.22622    
    ## gap_openness.tste_20_14_ct          -0.0346717  0.0409577  -0.847  0.39736    
    ## gap_extraversion.tste_20_15_ct       0.0147616  0.0300416   0.491  0.62322    
    ## gap_agreeableness.tste_20_15_ct     -0.0057749  0.0422143  -0.137  0.89120    
    ## gap_openness.tste_20_15_ct          -0.0382576  0.0496691  -0.770  0.44124    
    ## gap_extraversion.tste_20_16_ct      -0.0509904  0.0282565  -1.805  0.07129 .  
    ## gap_agreeableness.tste_20_16_ct      0.0506591  0.0390854   1.296  0.19509    
    ## gap_openness.tste_20_16_ct          -0.0218501  0.0463961  -0.471  0.63773    
    ## gap_extraversion.tste_20_17_ct      -0.0100325  0.0268246  -0.374  0.70844    
    ## gap_agreeableness.tste_20_17_ct     -0.0305439  0.0354179  -0.862  0.38858    
    ## gap_conscientiousness.tste_20_17_ct  0.0084127  0.0427591   0.197  0.84405    
    ## gap_openness.tste_20_17_ct          -0.0456934  0.0407609  -1.121  0.26242    
    ## gap_extraversion.tste_20_18_ct       0.0079383  0.0263355   0.301  0.76312    
    ## gap_agreeableness.tste_20_18_ct      0.0532446  0.0364688   1.460  0.14444    
    ## gap_conscientiousness.tste_20_18_ct -0.0089244  0.0407028  -0.219  0.82647    
    ## gap_openness.tste_20_18_ct           0.0290647  0.0412537   0.705  0.48118    
    ## gap_extraversion.tste_20_19_ct      -0.0135359  0.0293420  -0.461  0.64462    
    ## gap_agreeableness.tste_20_19_ct      0.0338788  0.0405893   0.835  0.40400    
    ## gap_openness.tste_20_19_ct          -0.0553101  0.0468578  -1.180  0.23799    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.436 on 2021 degrees of freedom
    ## Multiple R-squared:  0.1353, Adjusted R-squared:  0.08566 
    ## F-statistic: 2.726 on 116 and 2021 DF,  p-value: < 2.2e-16

Model summaries (Lasso select all variables)
--------------------------------------------

### preference ~ tste + real

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 1:19)$model_lm_2) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8943 -0.6831  0.3091  1.1077  2.4512 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.505627  12.157901   4.977 6.99e-07 ***
    ## release                   -0.027424   0.006056  -4.529 6.27e-06 ***
    ## real_extraversion_ct       0.051778   0.019568   2.646   0.0082 ** 
    ## real_conscientiousness_ct  0.068918   0.028122   2.451   0.0143 *  
    ## real_emotionstability_ct  -0.014358   0.027949  -0.514   0.6075    
    ## real_openness_ct           0.116831   0.024041   4.860 1.26e-06 ***
    ## tste_3_1_ct                0.169652   0.033394   5.080 4.10e-07 ***
    ## tste_3_2_ct                0.234705   0.048520   4.837 1.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06065,    Adjusted R-squared:  0.05756 
    ## F-statistic: 19.65 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0474 -0.6842  0.3174  1.0903  2.4561 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               63.94075   12.12544   5.273 1.48e-07 ***
    ## release                   -0.02914    0.00604  -4.824 1.51e-06 ***
    ## real_extraversion_ct       0.05197    0.01956   2.656  0.00796 ** 
    ## real_conscientiousness_ct  0.06884    0.02811   2.449  0.01441 *  
    ## real_emotionstability_ct  -0.01863    0.02792  -0.667  0.50476    
    ## real_openness_ct           0.11487    0.02404   4.779 1.88e-06 ***
    ## tste_4_0_ct                0.22900    0.04732   4.839 1.40e-06 ***
    ## tste_4_3_ct               -0.17815    0.03320  -5.365 8.95e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06123,    Adjusted R-squared:  0.05814 
    ## F-statistic: 19.85 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0259 -0.6771  0.3095  1.0863  2.4242 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.327796  12.378359   4.227 2.46e-05 ***
    ## release                   -0.023351   0.006166  -3.787 0.000157 ***
    ## real_extraversion_ct       0.054450   0.019595   2.779 0.005505 ** 
    ## real_conscientiousness_ct  0.068539   0.028156   2.434 0.015004 *  
    ## real_emotionstability_ct  -0.017507   0.027993  -0.625 0.531759    
    ## real_openness_ct           0.113406   0.024112   4.703 2.72e-06 ***
    ## tste_5_0_ct                0.209258   0.045079   4.642 3.66e-06 ***
    ## tste_5_1_ct               -0.181398   0.047170  -3.846 0.000124 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05834,    Adjusted R-squared:  0.05524 
    ## F-statistic: 18.85 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9686 -0.6884  0.3096  1.1057  2.3698 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.773934  12.243624   4.800 1.69e-06 ***
    ## release                   -0.026562   0.006099  -4.355 1.39e-05 ***
    ## real_extraversion_ct       0.053543   0.019611   2.730  0.00638 ** 
    ## real_conscientiousness_ct  0.068922   0.028194   2.445  0.01458 *  
    ## real_emotionstability_ct  -0.014391   0.028024  -0.514  0.60764    
    ## real_openness_ct           0.116704   0.024099   4.843 1.37e-06 ***
    ## tste_6_2_ct                0.206689   0.038774   5.331 1.08e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05533,    Adjusted R-squared:  0.05267 
    ## F-statistic:  20.8 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9249 -0.6860  0.3124  1.0969  2.4481 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.395224  12.200114   5.114 3.43e-07 ***
    ## release                   -0.028366   0.006077  -4.668 3.23e-06 ***
    ## real_extraversion_ct       0.052702   0.019634   2.684  0.00733 ** 
    ## real_conscientiousness_ct  0.068335   0.028231   2.421  0.01558 *  
    ## real_emotionstability_ct  -0.015252   0.028059  -0.544  0.58679    
    ## real_openness_ct           0.114941   0.024128   4.764 2.03e-06 ***
    ## tste_7_3_ct               -0.166962   0.034983  -4.773 1.94e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05285,    Adjusted R-squared:  0.05019 
    ## F-statistic: 19.82 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9382 -0.6887  0.3067  1.0888  2.4638 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.846350  12.238524   4.318 1.65e-05 ***
    ## release                   -0.023609   0.006096  -3.873 0.000111 ***
    ## real_extraversion_ct       0.055980   0.019511   2.869 0.004156 ** 
    ## real_conscientiousness_ct  0.069495   0.028042   2.478 0.013279 *  
    ## real_emotionstability_ct  -0.015979   0.027857  -0.574 0.566282    
    ## real_openness_ct           0.112530   0.023968   4.695 2.83e-06 ***
    ## tste_9_2_ct               -0.325207   0.045091  -7.212 7.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06554,    Adjusted R-squared:  0.06291 
    ## F-statistic: 24.91 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0163 -0.6769  0.3079  1.1045  2.3228 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.758430  12.198335   5.309 1.22e-07 ***
    ## release                   -0.029543   0.006076  -4.862 1.25e-06 ***
    ## real_extraversion_ct       0.049788   0.019658   2.533 0.011391 *  
    ## real_conscientiousness_ct  0.068205   0.028283   2.411 0.015971 *  
    ## real_emotionstability_ct  -0.016560   0.028107  -0.589 0.555802    
    ## real_openness_ct           0.116062   0.024175   4.801 1.69e-06 ***
    ## tste_10_9_ct               0.176935   0.045950   3.851 0.000121 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2131 degrees of freedom
    ## Multiple R-squared:  0.04935,    Adjusted R-squared:  0.04667 
    ## F-statistic: 18.44 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8930 -0.6807  0.2927  1.0736  2.4339 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.258618  12.495387   3.942 8.34e-05 ***
    ## release                   -0.021822   0.006224  -3.506 0.000464 ***
    ## real_extraversion_ct       0.054686   0.019537   2.799 0.005170 ** 
    ## real_conscientiousness_ct  0.069317   0.028055   2.471 0.013560 *  
    ## real_emotionstability_ct  -0.014389   0.027893  -0.516 0.605999    
    ## real_openness_ct           0.115311   0.023997   4.805 1.65e-06 ***
    ## tste_11_4_ct              -0.178025   0.050780  -3.506 0.000465 ***
    ## tste_11_8_ct              -0.166704   0.033785  -4.934 8.67e-07 ***
    ## tste_11_9_ct               0.180687   0.048393   3.734 0.000194 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2129 degrees of freedom
    ## Multiple R-squared:  0.06579,    Adjusted R-squared:  0.06228 
    ## F-statistic: 18.74 on 8 and 2129 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8918 -0.6796  0.2993  1.0700  2.5528 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               50.104546  12.555011   3.991 6.81e-05 ***
    ## release                   -0.022244   0.006254  -3.557 0.000383 ***
    ## real_extraversion_ct       0.054906   0.019529   2.812 0.004975 ** 
    ## real_conscientiousness_ct  0.067191   0.028037   2.397 0.016637 *  
    ## real_emotionstability_ct  -0.016093   0.027864  -0.578 0.563630    
    ## real_openness_ct           0.112362   0.023976   4.686 2.96e-06 ***
    ## tste_12_8_ct               0.170374   0.039720   4.289 1.87e-05 ***
    ## tste_12_10_ct              0.174431   0.040071   4.353 1.41e-05 ***
    ## tste_12_11_ct             -0.195377   0.044302  -4.410 1.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2129 degrees of freedom
    ## Multiple R-squared:  0.06711,    Adjusted R-squared:  0.0636 
    ## F-statistic: 19.14 on 8 and 2129 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8699 -0.6844  0.3002  1.1001  2.3063 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.283800  12.770715   4.016 6.13e-05 ***
    ## release                   -0.022831   0.006361  -3.589 0.000339 ***
    ## real_extraversion_ct       0.052208   0.019611   2.662 0.007821 ** 
    ## real_conscientiousness_ct  0.068220   0.028203   2.419 0.015649 *  
    ## real_emotionstability_ct  -0.015524   0.028025  -0.554 0.579688    
    ## real_openness_ct           0.115016   0.024108   4.771 1.96e-06 ***
    ## tste_13_4_ct              -0.134887   0.049193  -2.742 0.006158 ** 
    ## tste_13_10_ct              0.191517   0.044746   4.280 1.95e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05525,    Adjusted R-squared:  0.05214 
    ## F-statistic: 17.79 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8143 -0.6859  0.3160  1.0978  2.4587 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.696225  12.517199   3.970 7.42e-05 ***
    ## release                   -0.022040   0.006235  -3.535 0.000416 ***
    ## real_extraversion_ct       0.056044   0.019613   2.857 0.004311 ** 
    ## real_conscientiousness_ct  0.066931   0.028177   2.375 0.017621 *  
    ## real_emotionstability_ct  -0.017999   0.027976  -0.643 0.520057    
    ## real_openness_ct           0.113343   0.024075   4.708 2.66e-06 ***
    ## tste_14_3_ct               0.138892   0.043616   3.184 0.001471 ** 
    ## tste_14_7_ct              -0.202717   0.053281  -3.805 0.000146 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05778,    Adjusted R-squared:  0.05468 
    ## F-statistic: 18.66 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9845 -0.6900  0.3015  1.0928  2.3458 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.454328  12.560411   4.097 4.35e-05 ***
    ## release                   -0.022916   0.006256  -3.663 0.000256 ***
    ## real_extraversion_ct       0.053388   0.019631   2.720 0.006591 ** 
    ## real_conscientiousness_ct  0.068575   0.028221   2.430 0.015186 *  
    ## real_emotionstability_ct  -0.021178   0.028030  -0.756 0.449999    
    ## real_openness_ct           0.112915   0.024122   4.681 3.03e-06 ***
    ## tste_15_2_ct               0.217396   0.044130   4.926 9.03e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.463 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05351,    Adjusted R-squared:  0.05084 
    ## F-statistic: 20.08 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0340 -0.7025  0.3029  1.0649  2.4420 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.385978  12.409350   3.738 0.000190 ***
    ## release                   -0.020391   0.006181  -3.299 0.000986 ***
    ## real_extraversion_ct       0.056742   0.019501   2.910 0.003656 ** 
    ## real_conscientiousness_ct  0.071225   0.028024   2.542 0.011106 *  
    ## real_emotionstability_ct  -0.015229   0.027842  -0.547 0.584442    
    ## real_openness_ct           0.112631   0.023967   4.699 2.77e-06 ***
    ## tste_17_0_ct               0.265363   0.044142   6.012 2.16e-09 ***
    ## tste_17_12_ct             -0.169770   0.038368  -4.425 1.01e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06732,    Adjusted R-squared:  0.06425 
    ## F-statistic: 21.96 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8666 -0.6899  0.3208  1.0845  2.5051 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               40.818391  12.715936   3.210  0.00135 ** 
    ## release                   -0.017618   0.006334  -2.782  0.00546 ** 
    ## real_extraversion_ct       0.050290   0.019523   2.576  0.01006 *  
    ## real_conscientiousness_ct  0.069359   0.028088   2.469  0.01361 *  
    ## real_emotionstability_ct  -0.014587   0.027909  -0.523  0.60126    
    ## real_openness_ct           0.114281   0.024005   4.761 2.06e-06 ***
    ## tste_18_16_ct             -0.301538   0.045030  -6.696 2.72e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06246,    Adjusted R-squared:  0.05982 
    ## F-statistic: 23.66 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8523 -0.6968  0.2963  1.1007  2.4621 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.925406  12.413300   4.264 2.10e-05 ***
    ## release                   -0.023649   0.006183  -3.825 0.000135 ***
    ## real_extraversion_ct       0.051710   0.019595   2.639 0.008376 ** 
    ## real_conscientiousness_ct  0.064761   0.028193   2.297 0.021713 *  
    ## real_emotionstability_ct  -0.017523   0.027998  -0.626 0.531482    
    ## real_openness_ct           0.115216   0.024089   4.783 1.85e-06 ***
    ## tste_19_12_ct              0.202279   0.037084   5.455 5.48e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.461 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05591,    Adjusted R-squared:  0.05325 
    ## F-statistic: 21.03 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0870 -0.6597  0.3126  1.0801  2.3131 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               55.207734  12.521514   4.409 1.09e-05 ***
    ## release                   -0.024785   0.006237  -3.974 7.30e-05 ***
    ## real_extraversion_ct       0.050958   0.019654   2.593  0.00959 ** 
    ## real_conscientiousness_ct  0.070250   0.028276   2.484  0.01305 *  
    ## real_emotionstability_ct  -0.017072   0.028091  -0.608  0.54342    
    ## real_openness_ct           0.116825   0.024169   4.834 1.44e-06 ***
    ## tste_20_1_ct               0.155547   0.038335   4.058 5.14e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05007,    Adjusted R-squared:  0.0474 
    ## F-statistic: 18.72 on 6 and 2131 DF,  p-value: < 2.2e-16

### preference ~ tste + real + real\*tste

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 20:38)$model_lm_2) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8943 -0.6831  0.3091  1.1077  2.4512 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.505627  12.157901   4.977 6.99e-07 ***
    ## release                   -0.027424   0.006056  -4.529 6.27e-06 ***
    ## real_extraversion_ct       0.051778   0.019568   2.646   0.0082 ** 
    ## real_conscientiousness_ct  0.068918   0.028122   2.451   0.0143 *  
    ## real_emotionstability_ct  -0.014358   0.027949  -0.514   0.6075    
    ## real_openness_ct           0.116831   0.024041   4.860 1.26e-06 ***
    ## tste_3_1_ct                0.169652   0.033394   5.080 4.10e-07 ***
    ## tste_3_2_ct                0.234705   0.048520   4.837 1.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06065,    Adjusted R-squared:  0.05756 
    ## F-statistic: 19.65 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0614 -0.6817  0.3196  1.0992  2.3180 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.800694  12.174556   5.487 4.58e-08 ***
    ## release                   -0.030560   0.006064  -5.039 5.06e-07 ***
    ## real_extraversion_ct       0.053446   0.019664   2.718  0.00662 ** 
    ## real_conscientiousness_ct  0.069166   0.028261   2.447  0.01447 *  
    ## real_emotionstability_ct  -0.018761   0.028070  -0.668  0.50398    
    ## real_openness_ct           0.112378   0.024159   4.652 3.49e-06 ***
    ## tste_4_3_ct               -0.138632   0.032353  -4.285 1.91e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05091,    Adjusted R-squared:  0.04824 
    ## F-statistic: 19.05 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0259 -0.6771  0.3095  1.0863  2.4242 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.327796  12.378359   4.227 2.46e-05 ***
    ## release                   -0.023351   0.006166  -3.787 0.000157 ***
    ## real_extraversion_ct       0.054450   0.019595   2.779 0.005505 ** 
    ## real_conscientiousness_ct  0.068539   0.028156   2.434 0.015004 *  
    ## real_emotionstability_ct  -0.017507   0.027993  -0.625 0.531759    
    ## real_openness_ct           0.113406   0.024112   4.703 2.72e-06 ***
    ## tste_5_0_ct                0.209258   0.045079   4.642 3.66e-06 ***
    ## tste_5_1_ct               -0.181398   0.047170  -3.846 0.000124 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05834,    Adjusted R-squared:  0.05524 
    ## F-statistic: 18.85 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9686 -0.6884  0.3096  1.1057  2.3698 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.773934  12.243624   4.800 1.69e-06 ***
    ## release                   -0.026562   0.006099  -4.355 1.39e-05 ***
    ## real_extraversion_ct       0.053543   0.019611   2.730  0.00638 ** 
    ## real_conscientiousness_ct  0.068922   0.028194   2.445  0.01458 *  
    ## real_emotionstability_ct  -0.014391   0.028024  -0.514  0.60764    
    ## real_openness_ct           0.116704   0.024099   4.843 1.37e-06 ***
    ## tste_6_2_ct                0.206689   0.038774   5.331 1.08e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05533,    Adjusted R-squared:  0.05267 
    ## F-statistic:  20.8 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9249 -0.6860  0.3124  1.0969  2.4481 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.395224  12.200114   5.114 3.43e-07 ***
    ## release                   -0.028366   0.006077  -4.668 3.23e-06 ***
    ## real_extraversion_ct       0.052702   0.019634   2.684  0.00733 ** 
    ## real_conscientiousness_ct  0.068335   0.028231   2.421  0.01558 *  
    ## real_emotionstability_ct  -0.015252   0.028059  -0.544  0.58679    
    ## real_openness_ct           0.114941   0.024128   4.764 2.03e-06 ***
    ## tste_7_3_ct               -0.166962   0.034983  -4.773 1.94e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05285,    Adjusted R-squared:  0.05019 
    ## F-statistic: 19.82 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9382 -0.6887  0.3067  1.0888  2.4638 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.846350  12.238524   4.318 1.65e-05 ***
    ## release                   -0.023609   0.006096  -3.873 0.000111 ***
    ## real_extraversion_ct       0.055980   0.019511   2.869 0.004156 ** 
    ## real_conscientiousness_ct  0.069495   0.028042   2.478 0.013279 *  
    ## real_emotionstability_ct  -0.015979   0.027857  -0.574 0.566282    
    ## real_openness_ct           0.112530   0.023968   4.695 2.83e-06 ***
    ## tste_9_2_ct               -0.325207   0.045091  -7.212 7.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06554,    Adjusted R-squared:  0.06291 
    ## F-statistic: 24.91 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0684 -0.6637  0.3308  1.1381  2.2721 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.906809  12.239033   5.467 5.12e-08 ***
    ## release                   -0.030613   0.006096  -5.022 5.55e-07 ***
    ## real_conscientiousness_ct  0.059320   0.028179   2.105   0.0354 *  
    ## real_emotionstability_ct  -0.043545   0.026640  -1.635   0.1023    
    ## real_openness_ct           0.127598   0.023726   5.378 8.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.473 on 2133 degrees of freedom
    ## Multiple R-squared:  0.03991,    Adjusted R-squared:  0.03811 
    ## F-statistic: 22.17 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0451 -0.6799  0.3112  1.0760  2.3773 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.823043  12.496654   4.227 2.47e-05 ***
    ## release                   -0.023598   0.006225  -3.791 0.000154 ***
    ## real_extraversion_ct       0.056254   0.019591   2.871 0.004127 ** 
    ## real_conscientiousness_ct  0.069995   0.028139   2.487 0.012943 *  
    ## real_emotionstability_ct  -0.018558   0.027955  -0.664 0.506852    
    ## real_openness_ct           0.113004   0.024062   4.696 2.82e-06 ***
    ## tste_11_4_ct              -0.168739   0.050873  -3.317 0.000926 ***
    ## tste_11_8_ct              -0.163660   0.033878  -4.831 1.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.459 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05967,    Adjusted R-squared:  0.05658 
    ## F-statistic: 19.31 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8863 -0.6672  0.3074  1.0921  2.4227 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               47.782149  12.596410   3.793 0.000153 ***
    ## release                   -0.021087   0.006274  -3.361 0.000791 ***
    ## real_extraversion_ct       0.053969   0.019610   2.752 0.005970 ** 
    ## real_conscientiousness_ct  0.068044   0.028154   2.417 0.015738 *  
    ## real_emotionstability_ct  -0.014002   0.027977  -0.501 0.616773    
    ## real_openness_ct           0.114438   0.024072   4.754 2.13e-06 ***
    ## tste_12_8_ct               0.172238   0.039885   4.318 1.64e-05 ***
    ## tste_12_11_ct             -0.184006   0.044411  -4.143 3.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05881,    Adjusted R-squared:  0.05571 
    ## F-statistic: 19.01 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9683 -0.6705  0.3152  1.1126  2.3366 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               61.520699  12.242941   5.025 5.45e-07 ***
    ## release                   -0.027930   0.006098  -4.580 4.92e-06 ***
    ## real_conscientiousness_ct  0.059372   0.028052   2.116   0.0344 *  
    ## real_emotionstability_ct  -0.041326   0.026525  -1.558   0.1194    
    ## real_openness_ct           0.127089   0.023620   5.381 8.24e-08 ***
    ## tste_13_10_ct              0.200965   0.044681   4.498 7.24e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04893,    Adjusted R-squared:  0.0467 
    ## F-statistic: 21.94 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8143 -0.6859  0.3160  1.0978  2.4587 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.696225  12.517199   3.970 7.42e-05 ***
    ## release                   -0.022040   0.006235  -3.535 0.000416 ***
    ## real_extraversion_ct       0.056044   0.019613   2.857 0.004311 ** 
    ## real_conscientiousness_ct  0.066931   0.028177   2.375 0.017621 *  
    ## real_emotionstability_ct  -0.017999   0.027976  -0.643 0.520057    
    ## real_openness_ct           0.113343   0.024075   4.708 2.66e-06 ***
    ## tste_14_3_ct               0.138892   0.043616   3.184 0.001471 ** 
    ## tste_14_7_ct              -0.202717   0.053281  -3.805 0.000146 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05778,    Adjusted R-squared:  0.05468 
    ## F-statistic: 18.66 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9917 -0.6697  0.3005  1.1062  2.3173 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.707866  12.578890   4.111 4.09e-05 ***
    ## release                   -0.023042   0.006266  -3.678 0.000241 ***
    ## real_conscientiousness_ct  0.058800   0.028033   2.097 0.036068 *  
    ## real_emotionstability_ct  -0.046261   0.026509  -1.745 0.081106 .  
    ## real_openness_ct           0.126881   0.023604   5.375 8.48e-08 ***
    ## tste_15_2_ct               0.212514   0.044160   4.812 1.60e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2132 degrees of freedom
    ## Multiple R-squared:  0.05022,    Adjusted R-squared:  0.048 
    ## F-statistic: 22.55 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0684 -0.6637  0.3308  1.1381  2.2721 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.906809  12.239033   5.467 5.12e-08 ***
    ## release                   -0.030613   0.006096  -5.022 5.55e-07 ***
    ## real_conscientiousness_ct  0.059320   0.028179   2.105   0.0354 *  
    ## real_emotionstability_ct  -0.043545   0.026640  -1.635   0.1023    
    ## real_openness_ct           0.127598   0.023726   5.378 8.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.473 on 2133 degrees of freedom
    ## Multiple R-squared:  0.03991,    Adjusted R-squared:  0.03811 
    ## F-statistic: 22.17 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0340 -0.7025  0.3029  1.0649  2.4420 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.385978  12.409350   3.738 0.000190 ***
    ## release                   -0.020391   0.006181  -3.299 0.000986 ***
    ## real_extraversion_ct       0.056742   0.019501   2.910 0.003656 ** 
    ## real_conscientiousness_ct  0.071225   0.028024   2.542 0.011106 *  
    ## real_emotionstability_ct  -0.015229   0.027842  -0.547 0.584442    
    ## real_openness_ct           0.112631   0.023967   4.699 2.77e-06 ***
    ## tste_17_0_ct               0.265363   0.044142   6.012 2.16e-09 ***
    ## tste_17_12_ct             -0.169770   0.038368  -4.425 1.01e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06732,    Adjusted R-squared:  0.06425 
    ## F-statistic: 21.96 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9029 -0.6861  0.3340  1.1026  2.5531 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                5.44808    0.03159 172.478  < 2e-16 ***
    ## real_conscientiousness_ct  0.06176    0.02793   2.211   0.0271 *  
    ## real_emotionstability_ct  -0.03968    0.02642  -1.502   0.1333    
    ## real_openness_ct           0.12728    0.02352   5.410 6.99e-08 ***
    ## tste_18_16_ct             -0.33927    0.04297  -7.895 4.60e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.461 on 2133 degrees of freedom
    ## Multiple R-squared:  0.05614,    Adjusted R-squared:  0.05437 
    ## F-statistic: 31.72 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8624 -0.6852  0.2982  1.1100  2.4339 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.976499  12.430635   4.262 2.12e-05 ***
    ## release                   -0.023674   0.006192  -3.824 0.000135 ***
    ## real_conscientiousness_ct  0.055323   0.028005   1.975 0.048341 *  
    ## real_emotionstability_ct  -0.041894   0.026468  -1.583 0.113609    
    ## real_openness_ct           0.128716   0.023573   5.460 5.30e-08 ***
    ## tste_19_12_ct              0.200214   0.037128   5.393 7.71e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.463 on 2132 degrees of freedom
    ## Multiple R-squared:  0.05283,    Adjusted R-squared:  0.05061 
    ## F-statistic: 23.78 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0959 -0.6604  0.3186  1.1093  2.2867 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               55.258573  12.538291   4.407 1.10e-05 ***
    ## release                   -0.024811   0.006245  -3.973 7.34e-05 ***
    ## real_conscientiousness_ct  0.060889   0.028082   2.168   0.0303 *  
    ## real_emotionstability_ct  -0.041103   0.026554  -1.548   0.1218    
    ## real_openness_ct           0.130109   0.023651   5.501 4.23e-08 ***
    ## tste_20_1_ct               0.153672   0.038379   4.004 6.44e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.468 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04707,    Adjusted R-squared:  0.04484 
    ## F-statistic: 21.06 on 5 and 2132 DF,  p-value: < 2.2e-16

### preference ~ tste + real + game

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 39:57)$model_lm_2) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8943 -0.6831  0.3091  1.1077  2.4512 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.505627  12.157901   4.977 6.99e-07 ***
    ## release                   -0.027424   0.006056  -4.529 6.27e-06 ***
    ## real_extraversion_ct       0.051778   0.019568   2.646   0.0082 ** 
    ## real_conscientiousness_ct  0.068918   0.028122   2.451   0.0143 *  
    ## real_emotionstability_ct  -0.014358   0.027949  -0.514   0.6075    
    ## real_openness_ct           0.116831   0.024041   4.860 1.26e-06 ***
    ## tste_3_1_ct                0.169652   0.033394   5.080 4.10e-07 ***
    ## tste_3_2_ct                0.234705   0.048520   4.837 1.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06065,    Adjusted R-squared:  0.05756 
    ## F-statistic: 19.65 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0474 -0.6842  0.3174  1.0903  2.4561 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               63.94075   12.12544   5.273 1.48e-07 ***
    ## release                   -0.02914    0.00604  -4.824 1.51e-06 ***
    ## real_extraversion_ct       0.05197    0.01956   2.656  0.00796 ** 
    ## real_conscientiousness_ct  0.06884    0.02811   2.449  0.01441 *  
    ## real_emotionstability_ct  -0.01863    0.02792  -0.667  0.50476    
    ## real_openness_ct           0.11487    0.02404   4.779 1.88e-06 ***
    ## tste_4_0_ct                0.22900    0.04732   4.839 1.40e-06 ***
    ## tste_4_3_ct               -0.17815    0.03320  -5.365 8.95e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06123,    Adjusted R-squared:  0.05814 
    ## F-statistic: 19.85 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0259 -0.6771  0.3095  1.0863  2.4242 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.327796  12.378359   4.227 2.46e-05 ***
    ## release                   -0.023351   0.006166  -3.787 0.000157 ***
    ## real_extraversion_ct       0.054450   0.019595   2.779 0.005505 ** 
    ## real_conscientiousness_ct  0.068539   0.028156   2.434 0.015004 *  
    ## real_emotionstability_ct  -0.017507   0.027993  -0.625 0.531759    
    ## real_openness_ct           0.113406   0.024112   4.703 2.72e-06 ***
    ## tste_5_0_ct                0.209258   0.045079   4.642 3.66e-06 ***
    ## tste_5_1_ct               -0.181398   0.047170  -3.846 0.000124 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05834,    Adjusted R-squared:  0.05524 
    ## F-statistic: 18.85 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9686 -0.6884  0.3096  1.1057  2.3698 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.773934  12.243624   4.800 1.69e-06 ***
    ## release                   -0.026562   0.006099  -4.355 1.39e-05 ***
    ## real_extraversion_ct       0.053543   0.019611   2.730  0.00638 ** 
    ## real_conscientiousness_ct  0.068922   0.028194   2.445  0.01458 *  
    ## real_emotionstability_ct  -0.014391   0.028024  -0.514  0.60764    
    ## real_openness_ct           0.116704   0.024099   4.843 1.37e-06 ***
    ## tste_6_2_ct                0.206689   0.038774   5.331 1.08e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05533,    Adjusted R-squared:  0.05267 
    ## F-statistic:  20.8 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9249 -0.6860  0.3124  1.0969  2.4481 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.395224  12.200114   5.114 3.43e-07 ***
    ## release                   -0.028366   0.006077  -4.668 3.23e-06 ***
    ## real_extraversion_ct       0.052702   0.019634   2.684  0.00733 ** 
    ## real_conscientiousness_ct  0.068335   0.028231   2.421  0.01558 *  
    ## real_emotionstability_ct  -0.015252   0.028059  -0.544  0.58679    
    ## real_openness_ct           0.114941   0.024128   4.764 2.03e-06 ***
    ## tste_7_3_ct               -0.166962   0.034983  -4.773 1.94e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05285,    Adjusted R-squared:  0.05019 
    ## F-statistic: 19.82 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9382 -0.6887  0.3067  1.0888  2.4638 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.846350  12.238524   4.318 1.65e-05 ***
    ## release                   -0.023609   0.006096  -3.873 0.000111 ***
    ## real_extraversion_ct       0.055980   0.019511   2.869 0.004156 ** 
    ## real_conscientiousness_ct  0.069495   0.028042   2.478 0.013279 *  
    ## real_emotionstability_ct  -0.015979   0.027857  -0.574 0.566282    
    ## real_openness_ct           0.112530   0.023968   4.695 2.83e-06 ***
    ## tste_9_2_ct               -0.325207   0.045091  -7.212 7.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06554,    Adjusted R-squared:  0.06291 
    ## F-statistic: 24.91 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0163 -0.6769  0.3079  1.1045  2.3228 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.758430  12.198335   5.309 1.22e-07 ***
    ## release                   -0.029543   0.006076  -4.862 1.25e-06 ***
    ## real_extraversion_ct       0.049788   0.019658   2.533 0.011391 *  
    ## real_conscientiousness_ct  0.068205   0.028283   2.411 0.015971 *  
    ## real_emotionstability_ct  -0.016560   0.028107  -0.589 0.555802    
    ## real_openness_ct           0.116062   0.024175   4.801 1.69e-06 ***
    ## tste_10_9_ct               0.176935   0.045950   3.851 0.000121 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2131 degrees of freedom
    ## Multiple R-squared:  0.04935,    Adjusted R-squared:  0.04667 
    ## F-statistic: 18.44 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0451 -0.6799  0.3112  1.0760  2.3773 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.823043  12.496654   4.227 2.47e-05 ***
    ## release                   -0.023598   0.006225  -3.791 0.000154 ***
    ## real_extraversion_ct       0.056254   0.019591   2.871 0.004127 ** 
    ## real_conscientiousness_ct  0.069995   0.028139   2.487 0.012943 *  
    ## real_emotionstability_ct  -0.018558   0.027955  -0.664 0.506852    
    ## real_openness_ct           0.113004   0.024062   4.696 2.82e-06 ***
    ## tste_11_4_ct              -0.168739   0.050873  -3.317 0.000926 ***
    ## tste_11_8_ct              -0.163660   0.033878  -4.831 1.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.459 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05967,    Adjusted R-squared:  0.05658 
    ## F-statistic: 19.31 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8863 -0.6672  0.3074  1.0921  2.4227 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               47.782149  12.596410   3.793 0.000153 ***
    ## release                   -0.021087   0.006274  -3.361 0.000791 ***
    ## real_extraversion_ct       0.053969   0.019610   2.752 0.005970 ** 
    ## real_conscientiousness_ct  0.068044   0.028154   2.417 0.015738 *  
    ## real_emotionstability_ct  -0.014002   0.027977  -0.501 0.616773    
    ## real_openness_ct           0.114438   0.024072   4.754 2.13e-06 ***
    ## tste_12_8_ct               0.172238   0.039885   4.318 1.64e-05 ***
    ## tste_12_11_ct             -0.184006   0.044411  -4.143 3.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05881,    Adjusted R-squared:  0.05571 
    ## F-statistic: 19.01 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8699 -0.6844  0.3002  1.1001  2.3063 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.283800  12.770715   4.016 6.13e-05 ***
    ## release                   -0.022831   0.006361  -3.589 0.000339 ***
    ## real_extraversion_ct       0.052208   0.019611   2.662 0.007821 ** 
    ## real_conscientiousness_ct  0.068220   0.028203   2.419 0.015649 *  
    ## real_emotionstability_ct  -0.015524   0.028025  -0.554 0.579688    
    ## real_openness_ct           0.115016   0.024108   4.771 1.96e-06 ***
    ## tste_13_4_ct              -0.134887   0.049193  -2.742 0.006158 ** 
    ## tste_13_10_ct              0.191517   0.044746   4.280 1.95e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05525,    Adjusted R-squared:  0.05214 
    ## F-statistic: 17.79 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8143 -0.6859  0.3160  1.0978  2.4587 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.696225  12.517199   3.970 7.42e-05 ***
    ## release                   -0.022040   0.006235  -3.535 0.000416 ***
    ## real_extraversion_ct       0.056044   0.019613   2.857 0.004311 ** 
    ## real_conscientiousness_ct  0.066931   0.028177   2.375 0.017621 *  
    ## real_emotionstability_ct  -0.017999   0.027976  -0.643 0.520057    
    ## real_openness_ct           0.113343   0.024075   4.708 2.66e-06 ***
    ## tste_14_3_ct               0.138892   0.043616   3.184 0.001471 ** 
    ## tste_14_7_ct              -0.202717   0.053281  -3.805 0.000146 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05778,    Adjusted R-squared:  0.05468 
    ## F-statistic: 18.66 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9845 -0.6900  0.3015  1.0928  2.3458 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.454328  12.560411   4.097 4.35e-05 ***
    ## release                   -0.022916   0.006256  -3.663 0.000256 ***
    ## real_extraversion_ct       0.053388   0.019631   2.720 0.006591 ** 
    ## real_conscientiousness_ct  0.068575   0.028221   2.430 0.015186 *  
    ## real_emotionstability_ct  -0.021178   0.028030  -0.756 0.449999    
    ## real_openness_ct           0.112915   0.024122   4.681 3.03e-06 ***
    ## tste_15_2_ct               0.217396   0.044130   4.926 9.03e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.463 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05351,    Adjusted R-squared:  0.05084 
    ## F-statistic: 20.08 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0340 -0.7025  0.3029  1.0649  2.4420 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.385978  12.409350   3.738 0.000190 ***
    ## release                   -0.020391   0.006181  -3.299 0.000986 ***
    ## real_extraversion_ct       0.056742   0.019501   2.910 0.003656 ** 
    ## real_conscientiousness_ct  0.071225   0.028024   2.542 0.011106 *  
    ## real_emotionstability_ct  -0.015229   0.027842  -0.547 0.584442    
    ## real_openness_ct           0.112631   0.023967   4.699 2.77e-06 ***
    ## tste_17_0_ct               0.265363   0.044142   6.012 2.16e-09 ***
    ## tste_17_12_ct             -0.169770   0.038368  -4.425 1.01e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06732,    Adjusted R-squared:  0.06425 
    ## F-statistic: 21.96 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8666 -0.6899  0.3208  1.0845  2.5051 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               40.818391  12.715936   3.210  0.00135 ** 
    ## release                   -0.017618   0.006334  -2.782  0.00546 ** 
    ## real_extraversion_ct       0.050290   0.019523   2.576  0.01006 *  
    ## real_conscientiousness_ct  0.069359   0.028088   2.469  0.01361 *  
    ## real_emotionstability_ct  -0.014587   0.027909  -0.523  0.60126    
    ## real_openness_ct           0.114281   0.024005   4.761 2.06e-06 ***
    ## tste_18_16_ct             -0.301538   0.045030  -6.696 2.72e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06246,    Adjusted R-squared:  0.05982 
    ## F-statistic: 23.66 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8523 -0.6968  0.2963  1.1007  2.4621 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.925406  12.413300   4.264 2.10e-05 ***
    ## release                   -0.023649   0.006183  -3.825 0.000135 ***
    ## real_extraversion_ct       0.051710   0.019595   2.639 0.008376 ** 
    ## real_conscientiousness_ct  0.064761   0.028193   2.297 0.021713 *  
    ## real_emotionstability_ct  -0.017523   0.027998  -0.626 0.531482    
    ## real_openness_ct           0.115216   0.024089   4.783 1.85e-06 ***
    ## tste_19_12_ct              0.202279   0.037084   5.455 5.48e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.461 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05591,    Adjusted R-squared:  0.05325 
    ## F-statistic: 21.03 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0870 -0.6597  0.3126  1.0801  2.3131 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               55.207734  12.521514   4.409 1.09e-05 ***
    ## release                   -0.024785   0.006237  -3.974 7.30e-05 ***
    ## real_extraversion_ct       0.050958   0.019654   2.593  0.00959 ** 
    ## real_conscientiousness_ct  0.070250   0.028276   2.484  0.01305 *  
    ## real_emotionstability_ct  -0.017072   0.028091  -0.608  0.54342    
    ## real_openness_ct           0.116825   0.024169   4.834 1.44e-06 ***
    ## tste_20_1_ct               0.155547   0.038335   4.058 5.14e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05007,    Adjusted R-squared:  0.0474 
    ## F-statistic: 18.72 on 6 and 2131 DF,  p-value: < 2.2e-16

### preference ~ tste + real + game + game\*tste

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 58:76)$model_lm_2) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8943 -0.6831  0.3091  1.1077  2.4512 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.505627  12.157901   4.977 6.99e-07 ***
    ## release                   -0.027424   0.006056  -4.529 6.27e-06 ***
    ## real_extraversion_ct       0.051778   0.019568   2.646   0.0082 ** 
    ## real_conscientiousness_ct  0.068918   0.028122   2.451   0.0143 *  
    ## real_emotionstability_ct  -0.014358   0.027949  -0.514   0.6075    
    ## real_openness_ct           0.116831   0.024041   4.860 1.26e-06 ***
    ## tste_3_1_ct                0.169652   0.033394   5.080 4.10e-07 ***
    ## tste_3_2_ct                0.234705   0.048520   4.837 1.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06065,    Adjusted R-squared:  0.05756 
    ## F-statistic: 19.65 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0614 -0.6817  0.3196  1.0992  2.3180 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.800694  12.174556   5.487 4.58e-08 ***
    ## release                   -0.030560   0.006064  -5.039 5.06e-07 ***
    ## real_extraversion_ct       0.053446   0.019664   2.718  0.00662 ** 
    ## real_conscientiousness_ct  0.069166   0.028261   2.447  0.01447 *  
    ## real_emotionstability_ct  -0.018761   0.028070  -0.668  0.50398    
    ## real_openness_ct           0.112378   0.024159   4.652 3.49e-06 ***
    ## tste_4_3_ct               -0.138632   0.032353  -4.285 1.91e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05091,    Adjusted R-squared:  0.04824 
    ## F-statistic: 19.05 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8978 -0.6901  0.3149  1.1049  2.4047 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.317481  12.319646   4.734 2.35e-06 ***
    ## release                   -0.026334   0.006136  -4.291 1.85e-05 ***
    ## real_extraversion_ct       0.050747   0.019635   2.585  0.00982 ** 
    ## real_conscientiousness_ct  0.068279   0.028247   2.417  0.01572 *  
    ## real_emotionstability_ct  -0.015211   0.028077  -0.542  0.58804    
    ## real_openness_ct           0.118363   0.024155   4.900 1.03e-06 ***
    ## tste_5_0_ct                0.204072   0.045204   4.514 6.69e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2131 degrees of freedom
    ## Multiple R-squared:  0.0518, Adjusted R-squared:  0.04913 
    ## F-statistic:  19.4 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9686 -0.6884  0.3096  1.1057  2.3698 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.773934  12.243624   4.800 1.69e-06 ***
    ## release                   -0.026562   0.006099  -4.355 1.39e-05 ***
    ## real_extraversion_ct       0.053543   0.019611   2.730  0.00638 ** 
    ## real_conscientiousness_ct  0.068922   0.028194   2.445  0.01458 *  
    ## real_emotionstability_ct  -0.014391   0.028024  -0.514  0.60764    
    ## real_openness_ct           0.116704   0.024099   4.843 1.37e-06 ***
    ## tste_6_2_ct                0.206689   0.038774   5.331 1.08e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05533,    Adjusted R-squared:  0.05267 
    ## F-statistic:  20.8 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9249 -0.6860  0.3124  1.0969  2.4481 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.395224  12.200114   5.114 3.43e-07 ***
    ## release                   -0.028366   0.006077  -4.668 3.23e-06 ***
    ## real_extraversion_ct       0.052702   0.019634   2.684  0.00733 ** 
    ## real_conscientiousness_ct  0.068335   0.028231   2.421  0.01558 *  
    ## real_emotionstability_ct  -0.015252   0.028059  -0.544  0.58679    
    ## real_openness_ct           0.114941   0.024128   4.764 2.03e-06 ***
    ## tste_7_3_ct               -0.166962   0.034983  -4.773 1.94e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05285,    Adjusted R-squared:  0.05019 
    ## F-statistic: 19.82 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0684 -0.6637  0.3308  1.1381  2.2721 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.906809  12.239033   5.467 5.12e-08 ***
    ## release                   -0.030613   0.006096  -5.022 5.55e-07 ***
    ## real_conscientiousness_ct  0.059320   0.028179   2.105   0.0354 *  
    ## real_emotionstability_ct  -0.043545   0.026640  -1.635   0.1023    
    ## real_openness_ct           0.127598   0.023726   5.378 8.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.473 on 2133 degrees of freedom
    ## Multiple R-squared:  0.03991,    Adjusted R-squared:  0.03811 
    ## F-statistic: 22.17 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9382 -0.6887  0.3067  1.0888  2.4638 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.846350  12.238524   4.318 1.65e-05 ***
    ## release                   -0.023609   0.006096  -3.873 0.000111 ***
    ## real_extraversion_ct       0.055980   0.019511   2.869 0.004156 ** 
    ## real_conscientiousness_ct  0.069495   0.028042   2.478 0.013279 *  
    ## real_emotionstability_ct  -0.015979   0.027857  -0.574 0.566282    
    ## real_openness_ct           0.112530   0.023968   4.695 2.83e-06 ***
    ## tste_9_2_ct               -0.325207   0.045091  -7.212 7.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06554,    Adjusted R-squared:  0.06291 
    ## F-statistic: 24.91 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0684 -0.6637  0.3308  1.1381  2.2721 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.906809  12.239033   5.467 5.12e-08 ***
    ## release                   -0.030613   0.006096  -5.022 5.55e-07 ***
    ## real_conscientiousness_ct  0.059320   0.028179   2.105   0.0354 *  
    ## real_emotionstability_ct  -0.043545   0.026640  -1.635   0.1023    
    ## real_openness_ct           0.127598   0.023726   5.378 8.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.473 on 2133 degrees of freedom
    ## Multiple R-squared:  0.03991,    Adjusted R-squared:  0.03811 
    ## F-statistic: 22.17 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0451 -0.6799  0.3112  1.0760  2.3773 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.823043  12.496654   4.227 2.47e-05 ***
    ## release                   -0.023598   0.006225  -3.791 0.000154 ***
    ## real_extraversion_ct       0.056254   0.019591   2.871 0.004127 ** 
    ## real_conscientiousness_ct  0.069995   0.028139   2.487 0.012943 *  
    ## real_emotionstability_ct  -0.018558   0.027955  -0.664 0.506852    
    ## real_openness_ct           0.113004   0.024062   4.696 2.82e-06 ***
    ## tste_11_4_ct              -0.168739   0.050873  -3.317 0.000926 ***
    ## tste_11_8_ct              -0.163660   0.033878  -4.831 1.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.459 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05967,    Adjusted R-squared:  0.05658 
    ## F-statistic: 19.31 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8863 -0.6672  0.3074  1.0921  2.4227 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               47.782149  12.596410   3.793 0.000153 ***
    ## release                   -0.021087   0.006274  -3.361 0.000791 ***
    ## real_extraversion_ct       0.053969   0.019610   2.752 0.005970 ** 
    ## real_conscientiousness_ct  0.068044   0.028154   2.417 0.015738 *  
    ## real_emotionstability_ct  -0.014002   0.027977  -0.501 0.616773    
    ## real_openness_ct           0.114438   0.024072   4.754 2.13e-06 ***
    ## tste_12_8_ct               0.172238   0.039885   4.318 1.64e-05 ***
    ## tste_12_11_ct             -0.184006   0.044411  -4.143 3.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05881,    Adjusted R-squared:  0.05571 
    ## F-statistic: 19.01 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9683 -0.6705  0.3152  1.1126  2.3366 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               61.520699  12.242941   5.025 5.45e-07 ***
    ## release                   -0.027930   0.006098  -4.580 4.92e-06 ***
    ## real_conscientiousness_ct  0.059372   0.028052   2.116   0.0344 *  
    ## real_emotionstability_ct  -0.041326   0.026525  -1.558   0.1194    
    ## real_openness_ct           0.127089   0.023620   5.381 8.24e-08 ***
    ## tste_13_10_ct              0.200965   0.044681   4.498 7.24e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04893,    Adjusted R-squared:  0.0467 
    ## F-statistic: 21.94 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8143 -0.6859  0.3160  1.0978  2.4587 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.696225  12.517199   3.970 7.42e-05 ***
    ## release                   -0.022040   0.006235  -3.535 0.000416 ***
    ## real_extraversion_ct       0.056044   0.019613   2.857 0.004311 ** 
    ## real_conscientiousness_ct  0.066931   0.028177   2.375 0.017621 *  
    ## real_emotionstability_ct  -0.017999   0.027976  -0.643 0.520057    
    ## real_openness_ct           0.113343   0.024075   4.708 2.66e-06 ***
    ## tste_14_3_ct               0.138892   0.043616   3.184 0.001471 ** 
    ## tste_14_7_ct              -0.202717   0.053281  -3.805 0.000146 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05778,    Adjusted R-squared:  0.05468 
    ## F-statistic: 18.66 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9917 -0.6697  0.3005  1.1062  2.3173 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.707866  12.578890   4.111 4.09e-05 ***
    ## release                   -0.023042   0.006266  -3.678 0.000241 ***
    ## real_conscientiousness_ct  0.058800   0.028033   2.097 0.036068 *  
    ## real_emotionstability_ct  -0.046261   0.026509  -1.745 0.081106 .  
    ## real_openness_ct           0.126881   0.023604   5.375 8.48e-08 ***
    ## tste_15_2_ct               0.212514   0.044160   4.812 1.60e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2132 degrees of freedom
    ## Multiple R-squared:  0.05022,    Adjusted R-squared:  0.048 
    ## F-statistic: 22.55 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0684 -0.6637  0.3308  1.1381  2.2721 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.906809  12.239033   5.467 5.12e-08 ***
    ## release                   -0.030613   0.006096  -5.022 5.55e-07 ***
    ## real_conscientiousness_ct  0.059320   0.028179   2.105   0.0354 *  
    ## real_emotionstability_ct  -0.043545   0.026640  -1.635   0.1023    
    ## real_openness_ct           0.127598   0.023726   5.378 8.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.473 on 2133 degrees of freedom
    ## Multiple R-squared:  0.03991,    Adjusted R-squared:  0.03811 
    ## F-statistic: 22.17 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0340 -0.7025  0.3029  1.0649  2.4420 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.385978  12.409350   3.738 0.000190 ***
    ## release                   -0.020391   0.006181  -3.299 0.000986 ***
    ## real_extraversion_ct       0.056742   0.019501   2.910 0.003656 ** 
    ## real_conscientiousness_ct  0.071225   0.028024   2.542 0.011106 *  
    ## real_emotionstability_ct  -0.015229   0.027842  -0.547 0.584442    
    ## real_openness_ct           0.112631   0.023967   4.699 2.77e-06 ***
    ## tste_17_0_ct               0.265363   0.044142   6.012 2.16e-09 ***
    ## tste_17_12_ct             -0.169770   0.038368  -4.425 1.01e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06732,    Adjusted R-squared:  0.06425 
    ## F-statistic: 21.96 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9029 -0.6861  0.3340  1.1026  2.5531 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                5.44808    0.03159 172.478  < 2e-16 ***
    ## real_conscientiousness_ct  0.06176    0.02793   2.211   0.0271 *  
    ## real_emotionstability_ct  -0.03968    0.02642  -1.502   0.1333    
    ## real_openness_ct           0.12728    0.02352   5.410 6.99e-08 ***
    ## tste_18_16_ct             -0.33927    0.04297  -7.895 4.60e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.461 on 2133 degrees of freedom
    ## Multiple R-squared:  0.05614,    Adjusted R-squared:  0.05437 
    ## F-statistic: 31.72 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8624 -0.6852  0.2982  1.1100  2.4339 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.976499  12.430635   4.262 2.12e-05 ***
    ## release                   -0.023674   0.006192  -3.824 0.000135 ***
    ## real_conscientiousness_ct  0.055323   0.028005   1.975 0.048341 *  
    ## real_emotionstability_ct  -0.041894   0.026468  -1.583 0.113609    
    ## real_openness_ct           0.128716   0.023573   5.460 5.30e-08 ***
    ## tste_19_12_ct              0.200214   0.037128   5.393 7.71e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.463 on 2132 degrees of freedom
    ## Multiple R-squared:  0.05283,    Adjusted R-squared:  0.05061 
    ## F-statistic: 23.78 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0959 -0.6604  0.3186  1.1093  2.2867 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               55.258573  12.538291   4.407 1.10e-05 ***
    ## release                   -0.024811   0.006245  -3.973 7.34e-05 ***
    ## real_conscientiousness_ct  0.060889   0.028082   2.168   0.0303 *  
    ## real_emotionstability_ct  -0.041103   0.026554  -1.548   0.1218    
    ## real_openness_ct           0.130109   0.023651   5.501 4.23e-08 ***
    ## tste_20_1_ct               0.153672   0.038379   4.004 6.44e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.468 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04707,    Adjusted R-squared:  0.04484 
    ## F-statistic: 21.06 on 5 and 2132 DF,  p-value: < 2.2e-16

### preference ~ tste + real + gap

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 77:95)$model_lm_2) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8943 -0.6831  0.3091  1.1077  2.4512 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.505627  12.157901   4.977 6.99e-07 ***
    ## release                   -0.027424   0.006056  -4.529 6.27e-06 ***
    ## real_extraversion_ct       0.051778   0.019568   2.646   0.0082 ** 
    ## real_conscientiousness_ct  0.068918   0.028122   2.451   0.0143 *  
    ## real_emotionstability_ct  -0.014358   0.027949  -0.514   0.6075    
    ## real_openness_ct           0.116831   0.024041   4.860 1.26e-06 ***
    ## tste_3_1_ct                0.169652   0.033394   5.080 4.10e-07 ***
    ## tste_3_2_ct                0.234705   0.048520   4.837 1.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06065,    Adjusted R-squared:  0.05756 
    ## F-statistic: 19.65 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0474 -0.6842  0.3174  1.0903  2.4561 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               63.94075   12.12544   5.273 1.48e-07 ***
    ## release                   -0.02914    0.00604  -4.824 1.51e-06 ***
    ## real_extraversion_ct       0.05197    0.01956   2.656  0.00796 ** 
    ## real_conscientiousness_ct  0.06884    0.02811   2.449  0.01441 *  
    ## real_emotionstability_ct  -0.01863    0.02792  -0.667  0.50476    
    ## real_openness_ct           0.11487    0.02404   4.779 1.88e-06 ***
    ## tste_4_0_ct                0.22900    0.04732   4.839 1.40e-06 ***
    ## tste_4_3_ct               -0.17815    0.03320  -5.365 8.95e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06123,    Adjusted R-squared:  0.05814 
    ## F-statistic: 19.85 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0259 -0.6771  0.3095  1.0863  2.4242 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.327796  12.378359   4.227 2.46e-05 ***
    ## release                   -0.023351   0.006166  -3.787 0.000157 ***
    ## real_extraversion_ct       0.054450   0.019595   2.779 0.005505 ** 
    ## real_conscientiousness_ct  0.068539   0.028156   2.434 0.015004 *  
    ## real_emotionstability_ct  -0.017507   0.027993  -0.625 0.531759    
    ## real_openness_ct           0.113406   0.024112   4.703 2.72e-06 ***
    ## tste_5_0_ct                0.209258   0.045079   4.642 3.66e-06 ***
    ## tste_5_1_ct               -0.181398   0.047170  -3.846 0.000124 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05834,    Adjusted R-squared:  0.05524 
    ## F-statistic: 18.85 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9686 -0.6884  0.3096  1.1057  2.3698 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.773934  12.243624   4.800 1.69e-06 ***
    ## release                   -0.026562   0.006099  -4.355 1.39e-05 ***
    ## real_extraversion_ct       0.053543   0.019611   2.730  0.00638 ** 
    ## real_conscientiousness_ct  0.068922   0.028194   2.445  0.01458 *  
    ## real_emotionstability_ct  -0.014391   0.028024  -0.514  0.60764    
    ## real_openness_ct           0.116704   0.024099   4.843 1.37e-06 ***
    ## tste_6_2_ct                0.206689   0.038774   5.331 1.08e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05533,    Adjusted R-squared:  0.05267 
    ## F-statistic:  20.8 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9249 -0.6860  0.3124  1.0969  2.4481 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.395224  12.200114   5.114 3.43e-07 ***
    ## release                   -0.028366   0.006077  -4.668 3.23e-06 ***
    ## real_extraversion_ct       0.052702   0.019634   2.684  0.00733 ** 
    ## real_conscientiousness_ct  0.068335   0.028231   2.421  0.01558 *  
    ## real_emotionstability_ct  -0.015252   0.028059  -0.544  0.58679    
    ## real_openness_ct           0.114941   0.024128   4.764 2.03e-06 ***
    ## tste_7_3_ct               -0.166962   0.034983  -4.773 1.94e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05285,    Adjusted R-squared:  0.05019 
    ## F-statistic: 19.82 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9382 -0.6887  0.3067  1.0888  2.4638 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.846350  12.238524   4.318 1.65e-05 ***
    ## release                   -0.023609   0.006096  -3.873 0.000111 ***
    ## real_extraversion_ct       0.055980   0.019511   2.869 0.004156 ** 
    ## real_conscientiousness_ct  0.069495   0.028042   2.478 0.013279 *  
    ## real_emotionstability_ct  -0.015979   0.027857  -0.574 0.566282    
    ## real_openness_ct           0.112530   0.023968   4.695 2.83e-06 ***
    ## tste_9_2_ct               -0.325207   0.045091  -7.212 7.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06554,    Adjusted R-squared:  0.06291 
    ## F-statistic: 24.91 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0163 -0.6769  0.3079  1.1045  2.3228 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               64.758430  12.198335   5.309 1.22e-07 ***
    ## release                   -0.029543   0.006076  -4.862 1.25e-06 ***
    ## real_extraversion_ct       0.049788   0.019658   2.533 0.011391 *  
    ## real_conscientiousness_ct  0.068205   0.028283   2.411 0.015971 *  
    ## real_emotionstability_ct  -0.016560   0.028107  -0.589 0.555802    
    ## real_openness_ct           0.116062   0.024175   4.801 1.69e-06 ***
    ## tste_10_9_ct               0.176935   0.045950   3.851 0.000121 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2131 degrees of freedom
    ## Multiple R-squared:  0.04935,    Adjusted R-squared:  0.04667 
    ## F-statistic: 18.44 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0451 -0.6799  0.3112  1.0760  2.3773 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.823043  12.496654   4.227 2.47e-05 ***
    ## release                   -0.023598   0.006225  -3.791 0.000154 ***
    ## real_extraversion_ct       0.056254   0.019591   2.871 0.004127 ** 
    ## real_conscientiousness_ct  0.069995   0.028139   2.487 0.012943 *  
    ## real_emotionstability_ct  -0.018558   0.027955  -0.664 0.506852    
    ## real_openness_ct           0.113004   0.024062   4.696 2.82e-06 ***
    ## tste_11_4_ct              -0.168739   0.050873  -3.317 0.000926 ***
    ## tste_11_8_ct              -0.163660   0.033878  -4.831 1.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.459 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05967,    Adjusted R-squared:  0.05658 
    ## F-statistic: 19.31 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8863 -0.6672  0.3074  1.0921  2.4227 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               47.782149  12.596410   3.793 0.000153 ***
    ## release                   -0.021087   0.006274  -3.361 0.000791 ***
    ## real_extraversion_ct       0.053969   0.019610   2.752 0.005970 ** 
    ## real_conscientiousness_ct  0.068044   0.028154   2.417 0.015738 *  
    ## real_emotionstability_ct  -0.014002   0.027977  -0.501 0.616773    
    ## real_openness_ct           0.114438   0.024072   4.754 2.13e-06 ***
    ## tste_12_8_ct               0.172238   0.039885   4.318 1.64e-05 ***
    ## tste_12_11_ct             -0.184006   0.044411  -4.143 3.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05881,    Adjusted R-squared:  0.05571 
    ## F-statistic: 19.01 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8699 -0.6844  0.3002  1.1001  2.3063 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.283800  12.770715   4.016 6.13e-05 ***
    ## release                   -0.022831   0.006361  -3.589 0.000339 ***
    ## real_extraversion_ct       0.052208   0.019611   2.662 0.007821 ** 
    ## real_conscientiousness_ct  0.068220   0.028203   2.419 0.015649 *  
    ## real_emotionstability_ct  -0.015524   0.028025  -0.554 0.579688    
    ## real_openness_ct           0.115016   0.024108   4.771 1.96e-06 ***
    ## tste_13_4_ct              -0.134887   0.049193  -2.742 0.006158 ** 
    ## tste_13_10_ct              0.191517   0.044746   4.280 1.95e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05525,    Adjusted R-squared:  0.05214 
    ## F-statistic: 17.79 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8143 -0.6859  0.3160  1.0978  2.4587 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.696225  12.517199   3.970 7.42e-05 ***
    ## release                   -0.022040   0.006235  -3.535 0.000416 ***
    ## real_extraversion_ct       0.056044   0.019613   2.857 0.004311 ** 
    ## real_conscientiousness_ct  0.066931   0.028177   2.375 0.017621 *  
    ## real_emotionstability_ct  -0.017999   0.027976  -0.643 0.520057    
    ## real_openness_ct           0.113343   0.024075   4.708 2.66e-06 ***
    ## tste_14_3_ct               0.138892   0.043616   3.184 0.001471 ** 
    ## tste_14_7_ct              -0.202717   0.053281  -3.805 0.000146 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05778,    Adjusted R-squared:  0.05468 
    ## F-statistic: 18.66 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9845 -0.6900  0.3015  1.0928  2.3458 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.454328  12.560411   4.097 4.35e-05 ***
    ## release                   -0.022916   0.006256  -3.663 0.000256 ***
    ## real_extraversion_ct       0.053388   0.019631   2.720 0.006591 ** 
    ## real_conscientiousness_ct  0.068575   0.028221   2.430 0.015186 *  
    ## real_emotionstability_ct  -0.021178   0.028030  -0.756 0.449999    
    ## real_openness_ct           0.112915   0.024122   4.681 3.03e-06 ***
    ## tste_15_2_ct               0.217396   0.044130   4.926 9.03e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.463 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05351,    Adjusted R-squared:  0.05084 
    ## F-statistic: 20.08 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0340 -0.7025  0.3029  1.0649  2.4420 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.385978  12.409350   3.738 0.000190 ***
    ## release                   -0.020391   0.006181  -3.299 0.000986 ***
    ## real_extraversion_ct       0.056742   0.019501   2.910 0.003656 ** 
    ## real_conscientiousness_ct  0.071225   0.028024   2.542 0.011106 *  
    ## real_emotionstability_ct  -0.015229   0.027842  -0.547 0.584442    
    ## real_openness_ct           0.112631   0.023967   4.699 2.77e-06 ***
    ## tste_17_0_ct               0.265363   0.044142   6.012 2.16e-09 ***
    ## tste_17_12_ct             -0.169770   0.038368  -4.425 1.01e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06732,    Adjusted R-squared:  0.06425 
    ## F-statistic: 21.96 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8666 -0.6899  0.3208  1.0845  2.5051 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               40.818391  12.715936   3.210  0.00135 ** 
    ## release                   -0.017618   0.006334  -2.782  0.00546 ** 
    ## real_extraversion_ct       0.050290   0.019523   2.576  0.01006 *  
    ## real_conscientiousness_ct  0.069359   0.028088   2.469  0.01361 *  
    ## real_emotionstability_ct  -0.014587   0.027909  -0.523  0.60126    
    ## real_openness_ct           0.114281   0.024005   4.761 2.06e-06 ***
    ## tste_18_16_ct             -0.301538   0.045030  -6.696 2.72e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.456 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06246,    Adjusted R-squared:  0.05982 
    ## F-statistic: 23.66 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8523 -0.6968  0.2963  1.1007  2.4621 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.925406  12.413300   4.264 2.10e-05 ***
    ## release                   -0.023649   0.006183  -3.825 0.000135 ***
    ## real_extraversion_ct       0.051710   0.019595   2.639 0.008376 ** 
    ## real_conscientiousness_ct  0.064761   0.028193   2.297 0.021713 *  
    ## real_emotionstability_ct  -0.017523   0.027998  -0.626 0.531482    
    ## real_openness_ct           0.115216   0.024089   4.783 1.85e-06 ***
    ## tste_19_12_ct              0.202279   0.037084   5.455 5.48e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.461 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05591,    Adjusted R-squared:  0.05325 
    ## F-statistic: 21.03 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0870 -0.6597  0.3126  1.0801  2.3131 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               55.207734  12.521514   4.409 1.09e-05 ***
    ## release                   -0.024785   0.006237  -3.974 7.30e-05 ***
    ## real_extraversion_ct       0.050958   0.019654   2.593  0.00959 ** 
    ## real_conscientiousness_ct  0.070250   0.028276   2.484  0.01305 *  
    ## real_emotionstability_ct  -0.017072   0.028091  -0.608  0.54342    
    ## real_openness_ct           0.116825   0.024169   4.834 1.44e-06 ***
    ## tste_20_1_ct               0.155547   0.038335   4.058 5.14e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.466 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05007,    Adjusted R-squared:  0.0474 
    ## F-statistic: 18.72 on 6 and 2131 DF,  p-value: < 2.2e-16

### preference ~ tste + real + gap + gap\*tste

### (link for the above position)

``` r
#Summary
for(model in slice(dfs, 96:114)$model_lm_2) print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9597 -0.6644  0.3457  1.1181  2.2955 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.995386  12.223941   5.481 4.74e-08 ***
    ## release                   -0.030657   0.006089  -5.035 5.18e-07 ***
    ## real_extraversion_ct       0.049454   0.019722   2.508   0.0122 *  
    ## real_conscientiousness_ct  0.068386   0.028375   2.410   0.0160 *  
    ## real_emotionstability_ct  -0.020252   0.028182  -0.719   0.4725    
    ## real_openness_ct           0.114677   0.024251   4.729 2.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04273,    Adjusted R-squared:  0.04049 
    ## F-statistic: 19.03 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8943 -0.6831  0.3091  1.1077  2.4512 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               60.505627  12.157901   4.977 6.99e-07 ***
    ## release                   -0.027424   0.006056  -4.529 6.27e-06 ***
    ## real_extraversion_ct       0.051778   0.019568   2.646   0.0082 ** 
    ## real_conscientiousness_ct  0.068918   0.028122   2.451   0.0143 *  
    ## real_emotionstability_ct  -0.014358   0.027949  -0.514   0.6075    
    ## real_openness_ct           0.116831   0.024041   4.860 1.26e-06 ***
    ## tste_3_1_ct                0.169652   0.033394   5.080 4.10e-07 ***
    ## tste_3_2_ct                0.234705   0.048520   4.837 1.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.458 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06065,    Adjusted R-squared:  0.05756 
    ## F-statistic: 19.65 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0614 -0.6817  0.3196  1.0992  2.3180 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.800694  12.174556   5.487 4.58e-08 ***
    ## release                   -0.030560   0.006064  -5.039 5.06e-07 ***
    ## real_extraversion_ct       0.053446   0.019664   2.718  0.00662 ** 
    ## real_conscientiousness_ct  0.069166   0.028261   2.447  0.01447 *  
    ## real_emotionstability_ct  -0.018761   0.028070  -0.668  0.50398    
    ## real_openness_ct           0.112378   0.024159   4.652 3.49e-06 ***
    ## tste_4_3_ct               -0.138632   0.032353  -4.285 1.91e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05091,    Adjusted R-squared:  0.04824 
    ## F-statistic: 19.05 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8978 -0.6901  0.3149  1.1049  2.4047 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.317481  12.319646   4.734 2.35e-06 ***
    ## release                   -0.026334   0.006136  -4.291 1.85e-05 ***
    ## real_extraversion_ct       0.050747   0.019635   2.585  0.00982 ** 
    ## real_conscientiousness_ct  0.068279   0.028247   2.417  0.01572 *  
    ## real_emotionstability_ct  -0.015211   0.028077  -0.542  0.58804    
    ## real_openness_ct           0.118363   0.024155   4.900 1.03e-06 ***
    ## tste_5_0_ct                0.204072   0.045204   4.514 6.69e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2131 degrees of freedom
    ## Multiple R-squared:  0.0518, Adjusted R-squared:  0.04913 
    ## F-statistic:  19.4 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9686 -0.6884  0.3096  1.1057  2.3698 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               58.773934  12.243624   4.800 1.69e-06 ***
    ## release                   -0.026562   0.006099  -4.355 1.39e-05 ***
    ## real_extraversion_ct       0.053543   0.019611   2.730  0.00638 ** 
    ## real_conscientiousness_ct  0.068922   0.028194   2.445  0.01458 *  
    ## real_emotionstability_ct  -0.014391   0.028024  -0.514  0.60764    
    ## real_openness_ct           0.116704   0.024099   4.843 1.37e-06 ***
    ## tste_6_2_ct                0.206689   0.038774   5.331 1.08e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.462 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05533,    Adjusted R-squared:  0.05267 
    ## F-statistic:  20.8 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9249 -0.6860  0.3124  1.0969  2.4481 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               62.395224  12.200114   5.114 3.43e-07 ***
    ## release                   -0.028366   0.006077  -4.668 3.23e-06 ***
    ## real_extraversion_ct       0.052702   0.019634   2.684  0.00733 ** 
    ## real_conscientiousness_ct  0.068335   0.028231   2.421  0.01558 *  
    ## real_emotionstability_ct  -0.015252   0.028059  -0.544  0.58679    
    ## real_openness_ct           0.114941   0.024128   4.764 2.03e-06 ***
    ## tste_7_3_ct               -0.166962   0.034983  -4.773 1.94e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.464 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05285,    Adjusted R-squared:  0.05019 
    ## F-statistic: 19.82 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9539 -0.6482  0.3253  1.1179  2.5100 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      64.949957  12.177302   5.334 1.06e-07 ***
    ## release                          -0.029639   0.006066  -4.886 1.10e-06 ***
    ## real_extraversion_ct              0.050219   0.019634   2.558   0.0106 *  
    ## real_conscientiousness_ct         0.066141   0.028251   2.341   0.0193 *  
    ## real_emotionstability_ct         -0.019036   0.028056  -0.678   0.4975    
    ## real_openness_ct                  0.115252   0.024142   4.774 1.93e-06 ***
    ## gap_emotionstability.tste_8_1_ct -0.160390   0.035558  -4.511 6.81e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2131 degrees of freedom
    ## Multiple R-squared:  0.05178,    Adjusted R-squared:  0.04911 
    ## F-statistic:  19.4 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9382 -0.6887  0.3067  1.0888  2.4638 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.846350  12.238524   4.318 1.65e-05 ***
    ## release                   -0.023609   0.006096  -3.873 0.000111 ***
    ## real_extraversion_ct       0.055980   0.019511   2.869 0.004156 ** 
    ## real_conscientiousness_ct  0.069495   0.028042   2.478 0.013279 *  
    ## real_emotionstability_ct  -0.015979   0.027857  -0.574 0.566282    
    ## real_openness_ct           0.112530   0.023968   4.695 2.83e-06 ***
    ## tste_9_2_ct               -0.325207   0.045091  -7.212 7.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.454 on 2131 degrees of freedom
    ## Multiple R-squared:  0.06554,    Adjusted R-squared:  0.06291 
    ## F-statistic: 24.91 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0684 -0.6637  0.3308  1.1381  2.2721 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.906809  12.239033   5.467 5.12e-08 ***
    ## release                   -0.030613   0.006096  -5.022 5.55e-07 ***
    ## real_conscientiousness_ct  0.059320   0.028179   2.105   0.0354 *  
    ## real_emotionstability_ct  -0.043545   0.026640  -1.635   0.1023    
    ## real_openness_ct           0.127598   0.023726   5.378 8.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.473 on 2133 degrees of freedom
    ## Multiple R-squared:  0.03991,    Adjusted R-squared:  0.03811 
    ## F-statistic: 22.17 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0451 -0.6799  0.3112  1.0760  2.3773 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.823043  12.496654   4.227 2.47e-05 ***
    ## release                   -0.023598   0.006225  -3.791 0.000154 ***
    ## real_extraversion_ct       0.056254   0.019591   2.871 0.004127 ** 
    ## real_conscientiousness_ct  0.069995   0.028139   2.487 0.012943 *  
    ## real_emotionstability_ct  -0.018558   0.027955  -0.664 0.506852    
    ## real_openness_ct           0.113004   0.024062   4.696 2.82e-06 ***
    ## tste_11_4_ct              -0.168739   0.050873  -3.317 0.000926 ***
    ## tste_11_8_ct              -0.163660   0.033878  -4.831 1.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.459 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05967,    Adjusted R-squared:  0.05658 
    ## F-statistic: 19.31 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8863 -0.6672  0.3074  1.0921  2.4227 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               47.782149  12.596410   3.793 0.000153 ***
    ## release                   -0.021087   0.006274  -3.361 0.000791 ***
    ## real_extraversion_ct       0.053969   0.019610   2.752 0.005970 ** 
    ## real_conscientiousness_ct  0.068044   0.028154   2.417 0.015738 *  
    ## real_emotionstability_ct  -0.014002   0.027977  -0.501 0.616773    
    ## real_openness_ct           0.114438   0.024072   4.754 2.13e-06 ***
    ## tste_12_8_ct               0.172238   0.039885   4.318 1.64e-05 ***
    ## tste_12_11_ct             -0.184006   0.044411  -4.143 3.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05881,    Adjusted R-squared:  0.05571 
    ## F-statistic: 19.01 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1435 -0.6717  0.3219  1.1147  2.4152 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   54.494992  12.384799   4.400 1.14e-05 ***
    ## release                       -0.024430   0.006169  -3.960 7.74e-05 ***
    ## real_conscientiousness_ct      0.059658   0.027983   2.132 0.033126 *  
    ## real_emotionstability_ct      -0.036350   0.026499  -1.372 0.170297    
    ## real_openness_ct               0.127990   0.023563   5.432 6.21e-08 ***
    ## tste_13_10_ct                  0.192830   0.044634   4.320 1.63e-05 ***
    ## gap_extraversion.tste_13_4_ct -0.082550   0.024190  -3.413 0.000655 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.463 on 2131 degrees of freedom
    ## Multiple R-squared:  0.0541, Adjusted R-squared:  0.05144 
    ## F-statistic: 20.31 on 6 and 2131 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8143 -0.6859  0.3160  1.0978  2.4587 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               49.696225  12.517199   3.970 7.42e-05 ***
    ## release                   -0.022040   0.006235  -3.535 0.000416 ***
    ## real_extraversion_ct       0.056044   0.019613   2.857 0.004311 ** 
    ## real_conscientiousness_ct  0.066931   0.028177   2.375 0.017621 *  
    ## real_emotionstability_ct  -0.017999   0.027976  -0.643 0.520057    
    ## real_openness_ct           0.113343   0.024075   4.708 2.66e-06 ***
    ## tste_14_3_ct               0.138892   0.043616   3.184 0.001471 ** 
    ## tste_14_7_ct              -0.202717   0.053281  -3.805 0.000146 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.46 on 2130 degrees of freedom
    ## Multiple R-squared:  0.05778,    Adjusted R-squared:  0.05468 
    ## F-statistic: 18.66 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9917 -0.6697  0.3005  1.1062  2.3173 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               51.707866  12.578890   4.111 4.09e-05 ***
    ## release                   -0.023042   0.006266  -3.678 0.000241 ***
    ## real_conscientiousness_ct  0.058800   0.028033   2.097 0.036068 *  
    ## real_emotionstability_ct  -0.046261   0.026509  -1.745 0.081106 .  
    ## real_openness_ct           0.126881   0.023604   5.375 8.48e-08 ***
    ## tste_15_2_ct               0.212514   0.044160   4.812 1.60e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.465 on 2132 degrees of freedom
    ## Multiple R-squared:  0.05022,    Adjusted R-squared:  0.048 
    ## F-statistic: 22.55 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0684 -0.6637  0.3308  1.1381  2.2721 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               66.906809  12.239033   5.467 5.12e-08 ***
    ## release                   -0.030613   0.006096  -5.022 5.55e-07 ***
    ## real_conscientiousness_ct  0.059320   0.028179   2.105   0.0354 *  
    ## real_emotionstability_ct  -0.043545   0.026640  -1.635   0.1023    
    ## real_openness_ct           0.127598   0.023726   5.378 8.36e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.473 on 2133 degrees of freedom
    ## Multiple R-squared:  0.03991,    Adjusted R-squared:  0.03811 
    ## F-statistic: 22.17 on 4 and 2133 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0340 -0.7025  0.3029  1.0649  2.4420 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               46.385978  12.409350   3.738 0.000190 ***
    ## release                   -0.020391   0.006181  -3.299 0.000986 ***
    ## real_extraversion_ct       0.056742   0.019501   2.910 0.003656 ** 
    ## real_conscientiousness_ct  0.071225   0.028024   2.542 0.011106 *  
    ## real_emotionstability_ct  -0.015229   0.027842  -0.547 0.584442    
    ## real_openness_ct           0.112631   0.023967   4.699 2.77e-06 ***
    ## tste_17_0_ct               0.265363   0.044142   6.012 2.16e-09 ***
    ## tste_17_12_ct             -0.169770   0.038368  -4.425 1.01e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2130 degrees of freedom
    ## Multiple R-squared:  0.06732,    Adjusted R-squared:  0.06425 
    ## F-statistic: 21.96 on 7 and 2130 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0476 -0.6881  0.3119  1.0887  2.6437 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        5.44482    0.03142 173.286  < 2e-16 ***
    ## real_conscientiousness_ct          0.06036    0.02778   2.173   0.0299 *  
    ## real_emotionstability_ct          -0.04041    0.02628  -1.538   0.1243    
    ## real_openness_ct                   0.12256    0.02342   5.234 1.82e-07 ***
    ## tste_18_16_ct                     -0.34324    0.04274  -8.030 1.59e-15 ***
    ## gap_emotionstability.tste_18_9_ct  0.14499    0.02925   4.957 7.74e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2132 degrees of freedom
    ## Multiple R-squared:  0.06689,    Adjusted R-squared:  0.0647 
    ## F-statistic: 30.57 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8624 -0.6852  0.2982  1.1100  2.4339 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               52.976499  12.430635   4.262 2.12e-05 ***
    ## release                   -0.023674   0.006192  -3.824 0.000135 ***
    ## real_conscientiousness_ct  0.055323   0.028005   1.975 0.048341 *  
    ## real_emotionstability_ct  -0.041894   0.026468  -1.583 0.113609    
    ## real_openness_ct           0.128716   0.023573   5.460 5.30e-08 ***
    ## tste_19_12_ct              0.200214   0.037128   5.393 7.71e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.463 on 2132 degrees of freedom
    ## Multiple R-squared:  0.05283,    Adjusted R-squared:  0.05061 
    ## F-statistic: 23.78 on 5 and 2132 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0959 -0.6604  0.3186  1.1093  2.2867 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               55.258573  12.538291   4.407 1.10e-05 ***
    ## release                   -0.024811   0.006245  -3.973 7.34e-05 ***
    ## real_conscientiousness_ct  0.060889   0.028082   2.168   0.0303 *  
    ## real_emotionstability_ct  -0.041103   0.026554  -1.548   0.1218    
    ## real_openness_ct           0.130109   0.023651   5.501 4.23e-08 ***
    ## tste_20_1_ct               0.153672   0.038379   4.004 6.44e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.468 on 2132 degrees of freedom
    ## Multiple R-squared:  0.04707,    Adjusted R-squared:  0.04484 
    ## F-statistic: 21.06 on 5 and 2132 DF,  p-value: < 2.2e-16

Model summaries (traditional genres as predictors)
--------------------------------------------------

### preference ~ traditional genres (not selected)

### (link for the above position)

``` r
summary(model_tGenre)
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = select(df_c, preference, 
    ##     starts_with("c_"), starts_with("tg_")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0201 -0.7313  0.2587  1.0658  3.1898 
    ## 
    ## Coefficients: (38 not defined because of singularities)
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 1.202e+02  2.363e+01   5.086 3.99e-07 ***
    ## c_age                      -8.797e-03  4.500e-03  -1.955  0.05074 .  
    ## c_education                 1.521e-02  2.619e-02   0.581  0.56143    
    ## c_income                    3.066e-02  1.663e-02   1.844  0.06527 .  
    ## c_race2                     9.587e-02  1.308e-01   0.733  0.46359    
    ## c_race4                    -2.700e-01  1.404e-01  -1.923  0.05459 .  
    ## c_race6                    -9.431e-01  3.406e-01  -2.769  0.00567 ** 
    ## c_race7                    -1.565e-01  1.337e-01  -1.171  0.24175    
    ## c_sex2                     -6.138e-02  6.800e-02  -0.903  0.36687    
    ## c_release                  -5.646e-02  1.165e-02  -4.845 1.36e-06 ***
    ## c_star                     -2.309e-01  9.760e-02  -2.365  0.01810 *  
    ## c_starGS                    2.019e-01  7.296e-02   2.767  0.00570 ** 
    ## tg_3D                      -7.923e-01  4.958e-01  -1.598  0.11019    
    ## tg_Action                  -2.825e-01  1.802e-01  -1.568  0.11710    
    ## tg_Adventure               -6.199e-01  4.515e-01  -1.373  0.16989    
    ## tg_Arcade                  -6.577e-01  5.250e-01  -1.253  0.21044    
    ## tg_Baseball                        NA         NA      NA       NA    
    ## tg_Basketball                      NA         NA      NA       NA    
    ## `tg_Beat-'Em-Up`                   NA         NA      NA       NA    
    ## tg_Billiards                       NA         NA      NA       NA    
    ## tg_Bowling                         NA         NA      NA       NA    
    ## tg_Boxing                          NA         NA      NA       NA    
    ## `tg_Card Game`             -2.453e+00  5.245e-01  -4.676 3.11e-06 ***
    ## tg_Compilation                     NA         NA      NA       NA    
    ## tg_Cricket                         NA         NA      NA       NA    
    ## `tg_Driving/Racing`        -6.683e-01  2.661e-01  -2.512  0.01209 *  
    ## tg_Edutainment             -7.585e-01  4.887e-01  -1.552  0.12081    
    ## tg_Fantasy                         NA         NA      NA       NA    
    ## tg_Fighting                -1.039e+00  4.923e-01  -2.110  0.03498 *  
    ## `tg_First-Person`          -3.326e-01  5.350e-01  -0.622  0.53429    
    ## tg_Fitness                 -1.218e+00  4.938e-01  -2.468  0.01368 *  
    ## `tg_Fixed-Screen`                  NA         NA      NA       NA    
    ## tg_Flight                          NA         NA      NA       NA    
    ## `tg_Football (American)`   -6.251e-02  6.958e-01  -0.090  0.92843    
    ## `tg_Free-to-Play`          -8.933e-04  2.769e-01  -0.003  0.99743    
    ## tg_Gambling                        NA         NA      NA       NA    
    ## `tg_Game Show`                     NA         NA      NA       NA    
    ## tg_Golf                            NA         NA      NA       NA    
    ## `tg_Hidden Object`                 NA         NA      NA       NA    
    ## tg_Historic                        NA         NA      NA       NA    
    ## tg_Hockey                          NA         NA      NA       NA    
    ## tg_Horror                          NA         NA      NA       NA    
    ## `tg_Hunting/Fishing`               NA         NA      NA       NA    
    ## `tg_Light-Gun`                     NA         NA      NA       NA    
    ## tg_MMO                     -1.144e+00  3.765e-01  -3.039  0.00240 ** 
    ## tg_MOBA                    -3.589e-01  4.981e-01  -0.721  0.47128    
    ## tg_Management              -1.008e-02  2.343e-01  -0.043  0.96568    
    ## `tg_Matching/Stacking`             NA         NA      NA       NA    
    ## tg_Military                        NA         NA      NA       NA    
    ## tg_Miscellaneous                   NA         NA      NA       NA    
    ## tg_Modern                          NA         NA      NA       NA    
    ## `tg_Music/Rhythm`          -1.011e+00  4.619e-01  -2.188  0.02875 *  
    ## `tg_On-Rails`                      NA         NA      NA       NA    
    ## `tg_Open-World`             9.906e-01  5.075e-01   1.952  0.05109 .  
    ## `tg_Party/Minigame`        -1.287e+00  4.847e-01  -2.656  0.00797 ** 
    ## tg_Pinball                         NA         NA      NA       NA    
    ## tg_Platformer              -7.392e-01  4.490e-01  -1.646  0.09982 .  
    ## tg_Puzzle                  -1.362e+00  4.707e-01  -2.894  0.00384 ** 
    ## `tg_Real-Time`             -3.403e-01  3.719e-01  -0.915  0.36027    
    ## tg_Roguelike                       NA         NA      NA       NA    
    ## `tg_Role-Playing`          -4.504e-01  4.071e-01  -1.106  0.26871    
    ## `tg_Sci-Fi`                        NA         NA      NA       NA    
    ## `tg_Shoot-'Em-Up`                  NA         NA      NA       NA    
    ## tg_Shooter                  5.908e-01  3.824e-01   1.545  0.12256    
    ## tg_Simulation              -4.806e-01  4.127e-01  -1.164  0.24437    
    ## `tg_Skateboarding/Skating` -8.764e-01  4.906e-01  -1.786  0.07418 .  
    ## `tg_Snowboarding/Skiing`           NA         NA      NA       NA    
    ## tg_Soccer                  -4.941e-01  6.996e-01  -0.706  0.48010    
    ## tg_Sports                          NA         NA      NA       NA    
    ## tg_Strategy                -9.089e-01  2.855e-01  -3.183  0.00148 ** 
    ## tg_Survival                 9.342e-01  5.726e-01   1.632  0.10291    
    ## tg_Tactical                -1.297e+00  4.307e-01  -3.011  0.00263 ** 
    ## `tg_Team-Based`            -4.330e-01  6.561e-01  -0.660  0.50931    
    ## tg_Tennis                          NA         NA      NA       NA    
    ## `tg_Text-Based`                    NA         NA      NA       NA    
    ## `tg_Third-Person`                  NA         NA      NA       NA    
    ## `tg_Trivia/Board Game`             NA         NA      NA       NA    
    ## `tg_Turn-Based`            -5.460e-01  4.365e-01  -1.251  0.21117    
    ## tg_VR                              NA         NA      NA       NA    
    ## `tg_Wakeboarding/Surfing`          NA         NA      NA       NA    
    ## tg_Wrestling                       NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.453 on 2095 degrees of freedom
    ## Multiple R-squared:  0.0823, Adjusted R-squared:  0.06391 
    ## F-statistic: 4.474 on 42 and 2095 DF,  p-value: < 2.2e-16

### preference ~ traditional genres (selected)

### (link for the above position)

``` r
summary(model_tGenre_selected)
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = df_c_selected)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8107 -0.5625  0.4577  1.2966  3.3548 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    59.356597  12.414866   4.781 1.86e-06 ***
    ## c_release      -0.026840   0.006184  -4.340 1.49e-05 ***
    ## `tg_Card Game` -1.655618   0.270763  -6.115 1.15e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.48 on 2135 degrees of freedom
    ## Multiple R-squared:  0.02977,    Adjusted R-squared:  0.02886 
    ## F-statistic: 32.76 on 2 and 2135 DF,  p-value: 9.74e-15
