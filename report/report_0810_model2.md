MAPSS Thesis II - model 2
================
Chih-Yu Chiang
August 10, 2017

-   [Setup](#setup)
-   [Variable](#variable)
-   [Model](#model)
-   [game personality ~ real personality + dissatisfaction + real personality\*dissatisfaction](#game-personality-real-personality-dissatisfaction-real-personalitydissatisfaction)
    -   [agreeableness](#agreeableness)
    -   [conscientiousness](#conscientiousness)
    -   [emotionstability](#emotionstability)
    -   [extraversion](#extraversion)
    -   [openness](#openness)
-   [Information criteria](#information-criteria)
    -   [BIC](#bic)
    -   [AIC](#aic)

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
#Main df, key=player-game
df <- bind_cols(core_cluster, core_tsteScore) %>%
  left_join(survey, by=c("core_id"), copy=FALSE)
```

Variable
--------

Compute and select variables to be used in models.

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
updateVars <- function(){
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
                gap_openness = game_openness - real_openness)
  
  
  #--Acquire player df, key=player
  df_player <<- distinct(df, respondent, .keep_all=TRUE)
  
  
  #--Select variables to be included in regression (model formation)
  #Sets of predictor variables from file
  predictors <- read.csv("../data/vars/predictors.csv", header=TRUE, na.strings="")
  
  #Get column name as model id
  modelId <- colnames(predictors)
  
  #predictor variable as strings for each model
  predictorString <- apply(predictors, MARGIN=2, function(x) paste(na.omit(x), collapse="+"))
  
  #Make the dfs into a data frame
  dfs <<- data.frame(predictorString, modelId, stringsAsFactors=FALSE) %>%
    mutate(df_x = map(predictorString, ~ model.matrix(as.formula(paste("preference ~ ", .x, sep="")), data=df)[, -1])) %>% #df with only predictor variables; [, -1] used to remove redundant intercept column
    mutate(df_yx = map(df_x, ~ bind_cols(select(df, preference), data.frame(.x)))) #df also with outcome variables
  
  #Set row names for reference
  row.names(dfs) <<- modelId
}
```

Model
-----

![Analysis Framework](img/framework_2.png)

``` r
#Update vars
updateVars()

#Player df with control marked
df_player_c <- mutate(df_player,
                      c_age = age,
                      c_education = education,
                      c_income = income,
                      c_race = race,
                      c_sex = sex)


#--Acquire corresponding df for each game personality
#Alphabetical order for personality response vars
#With all personality
dfs_ygame_all <- list(select(df_player_c, game_p = game_agreeableness, matches("^real.*ct$"), starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_conscientiousness, matches("^real.*ct$"), starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_emotionstability, matches("^real.*ct$"), starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_extraversion, matches("^real.*ct$"), starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_openness, matches("^real.*ct$"), starts_with("c_"), matches("^dissatis.*ct$")))

#With only the specific personality
dfs_ygame <- list(select(df_player_c, game_p = game_agreeableness, real_p_ct = real_agreeableness_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_conscientiousness, real_p_ct = real_conscientiousness_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_emotionstability, real_p_ct = real_emotionstability_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_extraversion, real_p_ct = real_extraversion_ct, starts_with("c_"), matches("^dissatis.*ct$")),
                  select(df_player_c, game_p = game_openness, real_p_ct = real_openness_ct, starts_with("c_"), matches("^dissatis.*ct$")))


#--Train models
#With all personality
models_ygame_all_lm <- map(dfs_ygame_all,
                       ~ lm(game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + dissatis_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct),
                            data=.x))

#With only the specific personality
models_ygame_lm <- map(dfs_ygame,
                       ~ lm(game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + dissatis_competence_ct) * real_p_ct,
                            data=.x))
```

game personality ~ real personality + dissatisfaction + real personality\*dissatisfaction
-----------------------------------------------------------------------------------------

### agreeableness

``` r
summary(models_ygame_lm[[1]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * real_p_ct, data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1590 -0.8223 -0.1279  0.5887  2.8899 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        3.047552   0.395126   7.713 5.78e-13 ***
    ## real_p_ct                          0.585024   0.062340   9.384  < 2e-16 ***
    ## c_age                              0.002784   0.010751   0.259    0.796    
    ## c_education                        0.057683   0.062542   0.922    0.357    
    ## c_income                           0.008981   0.039871   0.225    0.822    
    ## c_race2                            0.056980   0.305807   0.186    0.852    
    ## c_race4                            0.016377   0.329557   0.050    0.960    
    ## c_race6                           -1.292682   0.811729  -1.593    0.113    
    ## c_race7                            0.185890   0.315388   0.589    0.556    
    ## c_sex2                            -0.258401   0.160503  -1.610    0.109    
    ## dissatis_autonomy_ct              -0.078824   0.071225  -1.107    0.270    
    ## dissatis_relatedness_ct            0.053176   0.072341   0.735    0.463    
    ## dissatis_competence_ct             0.053204   0.070295   0.757    0.450    
    ## real_p_ct:dissatis_autonomy_ct    -0.068974   0.051003  -1.352    0.178    
    ## real_p_ct:dissatis_relatedness_ct  0.037383   0.053767   0.695    0.488    
    ## real_p_ct:dissatis_competence_ct   0.012679   0.051907   0.244    0.807    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.081 on 199 degrees of freedom
    ## Multiple R-squared:  0.4009, Adjusted R-squared:  0.3557 
    ## F-statistic: 8.877 on 15 and 199 DF,  p-value: 1.388e-15

``` r
summary(models_ygame_all_lm[[1]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + 
    ##     real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct), 
    ##     data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9087 -0.6720 -0.1221  0.5313  3.2861 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                                        3.139618   0.398053   7.887 2.71e-13 ***
    ## real_extraversion_ct                               0.064707   0.049679   1.302  0.19438    
    ## real_agreeableness_ct                              0.554534   0.065118   8.516 5.93e-15 ***
    ## real_conscientiousness_ct                         -0.062275   0.078625  -0.792  0.42936    
    ## real_emotionstability_ct                           0.076516   0.076722   0.997  0.31993    
    ## real_openness_ct                                  -0.077239   0.062683  -1.232  0.21945    
    ## c_age                                              0.001807   0.010921   0.165  0.86874    
    ## c_education                                        0.074472   0.063313   1.176  0.24102    
    ## c_income                                          -0.007760   0.040097  -0.194  0.84675    
    ## c_race2                                           -0.008721   0.308181  -0.028  0.97745    
    ## c_race4                                           -0.156459   0.328791  -0.476  0.63474    
    ## c_race6                                           -0.603070   0.869033  -0.694  0.48859    
    ## c_race7                                            0.394261   0.335909   1.174  0.24203    
    ## c_sex2                                            -0.275744   0.167477  -1.646  0.10139    
    ## dissatis_autonomy_ct                              -0.005411   0.073458  -0.074  0.94136    
    ## dissatis_relatedness_ct                           -0.040540   0.077413  -0.524  0.60113    
    ## dissatis_competence_ct                             0.023850   0.088936   0.268  0.78887    
    ## real_extraversion_ct:dissatis_autonomy_ct          0.064169   0.042768   1.500  0.13523    
    ## real_agreeableness_ct:dissatis_autonomy_ct        -0.028618   0.067105  -0.426  0.67027    
    ## real_conscientiousness_ct:dissatis_autonomy_ct     0.161231   0.082619   1.951  0.05253 .  
    ## real_emotionstability_ct:dissatis_autonomy_ct      0.149884   0.086050   1.742  0.08322 .  
    ## real_openness_ct:dissatis_autonomy_ct             -0.088892   0.056785  -1.565  0.11921    
    ## real_extraversion_ct:dissatis_relatedness_ct      -0.120399   0.044690  -2.694  0.00771 ** 
    ## real_agreeableness_ct:dissatis_relatedness_ct     -0.012560   0.060201  -0.209  0.83497    
    ## real_conscientiousness_ct:dissatis_relatedness_ct -0.153177   0.065261  -2.347  0.01999 *  
    ## real_emotionstability_ct:dissatis_relatedness_ct  -0.085167   0.067259  -1.266  0.20704    
    ## real_openness_ct:dissatis_relatedness_ct          -0.074721   0.056541  -1.322  0.18797    
    ## real_extraversion_ct:dissatis_competence_ct        0.043732   0.045149   0.969  0.33402    
    ## real_agreeableness_ct:dissatis_competence_ct      -0.029900   0.065037  -0.460  0.64625    
    ## real_conscientiousness_ct:dissatis_competence_ct  -0.013159   0.061994  -0.212  0.83214    
    ## real_emotionstability_ct:dissatis_competence_ct   -0.019422   0.065358  -0.297  0.76668    
    ## real_openness_ct:dissatis_competence_ct            0.128729   0.057440   2.241  0.02622 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.048 on 183 degrees of freedom
    ## Multiple R-squared:  0.4822, Adjusted R-squared:  0.3945 
    ## F-statistic: 5.497 on 31 and 183 DF,  p-value: 5.67e-14

### conscientiousness

``` r
summary(models_ygame_lm[[2]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * real_p_ct, data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1813 -0.4017 -0.0031  0.5163  2.5988 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        5.612653   0.313203  17.920   <2e-16 ***
    ## real_p_ct                          0.598478   0.055878  10.710   <2e-16 ***
    ## c_age                              0.004417   0.008578   0.515   0.6072    
    ## c_education                       -0.016613   0.049692  -0.334   0.7385    
    ## c_income                          -0.052558   0.031103  -1.690   0.0926 .  
    ## c_race2                           -0.141722   0.244363  -0.580   0.5626    
    ## c_race4                           -0.090288   0.261918  -0.345   0.7307    
    ## c_race6                            0.421154   0.629285   0.669   0.5041    
    ## c_race7                           -0.392868   0.251314  -1.563   0.1196    
    ## c_sex2                             0.045346   0.125493   0.361   0.7182    
    ## dissatis_autonomy_ct               0.046619   0.057746   0.807   0.4204    
    ## dissatis_relatedness_ct           -0.120388   0.057534  -2.092   0.0377 *  
    ## dissatis_competence_ct             0.018437   0.059663   0.309   0.7576    
    ## real_p_ct:dissatis_autonomy_ct     0.031220   0.038726   0.806   0.4211    
    ## real_p_ct:dissatis_relatedness_ct  0.068732   0.040202   1.710   0.0889 .  
    ## real_p_ct:dissatis_competence_ct  -0.084156   0.034667  -2.428   0.0161 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8548 on 199 degrees of freedom
    ## Multiple R-squared:  0.5201, Adjusted R-squared:  0.4839 
    ## F-statistic: 14.38 on 15 and 199 DF,  p-value: < 2.2e-16

``` r
summary(models_ygame_all_lm[[2]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + 
    ##     real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct), 
    ##     data = .x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.14554 -0.41837 -0.02954  0.46132  2.50329 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                                        5.547310   0.328151  16.905  < 2e-16 ***
    ## real_extraversion_ct                              -0.021385   0.040955  -0.522   0.6022    
    ## real_agreeableness_ct                             -0.054585   0.053683  -1.017   0.3106    
    ## real_conscientiousness_ct                          0.581657   0.064818   8.974  3.4e-16 ***
    ## real_emotionstability_ct                           0.012062   0.063249   0.191   0.8490    
    ## real_openness_ct                                   0.098377   0.051675   1.904   0.0585 .  
    ## c_age                                              0.003623   0.009003   0.402   0.6879    
    ## c_education                                       -0.020653   0.052195  -0.396   0.6928    
    ## c_income                                          -0.035075   0.033055  -1.061   0.2900    
    ## c_race2                                           -0.173569   0.254062  -0.683   0.4954    
    ## c_race4                                           -0.109558   0.271053  -0.404   0.6865    
    ## c_race6                                            0.581510   0.716423   0.812   0.4180    
    ## c_race7                                           -0.387914   0.276920  -1.401   0.1630    
    ## c_sex2                                             0.020000   0.138067   0.145   0.8850    
    ## dissatis_autonomy_ct                               0.036005   0.060558   0.595   0.5529    
    ## dissatis_relatedness_ct                           -0.065013   0.063819  -1.019   0.3097    
    ## dissatis_competence_ct                             0.012766   0.073318   0.174   0.8620    
    ## real_extraversion_ct:dissatis_autonomy_ct         -0.028213   0.035257  -0.800   0.4246    
    ## real_agreeableness_ct:dissatis_autonomy_ct        -0.021944   0.055321  -0.397   0.6921    
    ## real_conscientiousness_ct:dissatis_autonomy_ct     0.116141   0.068111   1.705   0.0899 .  
    ## real_emotionstability_ct:dissatis_autonomy_ct      0.111835   0.070939   1.577   0.1166    
    ## real_openness_ct:dissatis_autonomy_ct              0.085025   0.046813   1.816   0.0710 .  
    ## real_extraversion_ct:dissatis_relatedness_ct       0.015920   0.036842   0.432   0.6662    
    ## real_agreeableness_ct:dissatis_relatedness_ct      0.013235   0.049629   0.267   0.7900    
    ## real_conscientiousness_ct:dissatis_relatedness_ct  0.040118   0.053801   0.746   0.4568    
    ## real_emotionstability_ct:dissatis_relatedness_ct  -0.059387   0.055448  -1.071   0.2856    
    ## real_openness_ct:dissatis_relatedness_ct          -0.038148   0.046612  -0.818   0.4142    
    ## real_extraversion_ct:dissatis_competence_ct        0.008969   0.037221   0.241   0.8098    
    ## real_agreeableness_ct:dissatis_competence_ct       0.022781   0.053616   0.425   0.6714    
    ## real_conscientiousness_ct:dissatis_competence_ct  -0.103234   0.051107  -2.020   0.0448 *  
    ## real_emotionstability_ct:dissatis_competence_ct   -0.035931   0.053880  -0.667   0.5057    
    ## real_openness_ct:dissatis_competence_ct           -0.068481   0.047353  -1.446   0.1498    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8638 on 183 degrees of freedom
    ## Multiple R-squared:  0.5494, Adjusted R-squared:  0.4731 
    ## F-statistic: 7.198 on 31 and 183 DF,  p-value: < 2.2e-16

### emotionstability

``` r
summary(models_ygame_lm[[3]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * real_p_ct, data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9056 -0.6889 -0.1412  0.7387  2.9921 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        3.091808   0.432776   7.144 1.67e-11 ***
    ## real_p_ct                          0.493125   0.075104   6.566 4.40e-10 ***
    ## c_age                              0.002582   0.011739   0.220    0.826    
    ## c_education                       -0.017034   0.068560  -0.248    0.804    
    ## c_income                           0.006638   0.043491   0.153    0.879    
    ## c_race2                           -0.155568   0.335504  -0.464    0.643    
    ## c_race4                           -0.015312   0.361088  -0.042    0.966    
    ## c_race6                           -0.525832   0.905696  -0.581    0.562    
    ## c_race7                           -0.393858   0.355285  -1.109    0.269    
    ## c_sex2                            -0.203479   0.178804  -1.138    0.256    
    ## dissatis_autonomy_ct               0.007690   0.079995   0.096    0.924    
    ## dissatis_relatedness_ct            0.061086   0.081713   0.748    0.456    
    ## dissatis_competence_ct            -0.039401   0.090066  -0.437    0.662    
    ## real_p_ct:dissatis_autonomy_ct    -0.086858   0.053817  -1.614    0.108    
    ## real_p_ct:dissatis_relatedness_ct  0.057059   0.048689   1.172    0.243    
    ## real_p_ct:dissatis_competence_ct  -0.049624   0.045558  -1.089    0.277    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.182 on 199 degrees of freedom
    ## Multiple R-squared:  0.3043, Adjusted R-squared:  0.2519 
    ## F-statistic: 5.804 on 15 and 199 DF,  p-value: 7.004e-10

``` r
summary(models_ygame_all_lm[[3]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + 
    ##     real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct), 
    ##     data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5569 -0.6727 -0.1086  0.6496  2.9192 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                                        2.905176   0.404725   7.178 1.71e-11 ***
    ## real_extraversion_ct                               0.182567   0.050512   3.614 0.000389 ***
    ## real_agreeableness_ct                              0.129006   0.066210   1.948 0.052892 .  
    ## real_conscientiousness_ct                         -0.148231   0.079943  -1.854 0.065319 .  
    ## real_emotionstability_ct                           0.417017   0.078008   5.346 2.66e-07 ***
    ## real_openness_ct                                   0.047519   0.063734   0.746 0.456875    
    ## c_age                                              0.011968   0.011104   1.078 0.282546    
    ## c_education                                       -0.037278   0.064374  -0.579 0.563244    
    ## c_income                                           0.015768   0.040769   0.387 0.699381    
    ## c_race2                                           -0.163522   0.313347  -0.522 0.602401    
    ## c_race4                                           -0.439310   0.334302  -1.314 0.190454    
    ## c_race6                                           -0.562676   0.883599  -0.637 0.525051    
    ## c_race7                                           -0.366406   0.341539  -1.073 0.284770    
    ## c_sex2                                            -0.068638   0.170284  -0.403 0.687361    
    ## dissatis_autonomy_ct                               0.033890   0.074690   0.454 0.650546    
    ## dissatis_relatedness_ct                            0.006703   0.078711   0.085 0.932231    
    ## dissatis_competence_ct                             0.028279   0.090427   0.313 0.754844    
    ## real_extraversion_ct:dissatis_autonomy_ct          0.076145   0.043485   1.751 0.081609 .  
    ## real_agreeableness_ct:dissatis_autonomy_ct         0.272000   0.068230   3.987 9.68e-05 ***
    ## real_conscientiousness_ct:dissatis_autonomy_ct     0.067766   0.084004   0.807 0.420886    
    ## real_emotionstability_ct:dissatis_autonomy_ct     -0.158380   0.087492  -1.810 0.071902 .  
    ## real_openness_ct:dissatis_autonomy_ct              0.004262   0.057737   0.074 0.941241    
    ## real_extraversion_ct:dissatis_relatedness_ct      -0.061302   0.045439  -1.349 0.178970    
    ## real_agreeableness_ct:dissatis_relatedness_ct     -0.015800   0.061210  -0.258 0.796600    
    ## real_conscientiousness_ct:dissatis_relatedness_ct -0.178581   0.066355  -2.691 0.007777 ** 
    ## real_emotionstability_ct:dissatis_relatedness_ct   0.010912   0.068387   0.160 0.873397    
    ## real_openness_ct:dissatis_relatedness_ct          -0.044341   0.057489  -0.771 0.441526    
    ## real_extraversion_ct:dissatis_competence_ct        0.014514   0.045906   0.316 0.752242    
    ## real_agreeableness_ct:dissatis_competence_ct      -0.221905   0.066127  -3.356 0.000962 ***
    ## real_conscientiousness_ct:dissatis_competence_ct   0.167125   0.063033   2.651 0.008720 ** 
    ## real_emotionstability_ct:dissatis_competence_ct    0.126719   0.066453   1.907 0.058102 .  
    ## real_openness_ct:dissatis_competence_ct            0.094320   0.058403   1.615 0.108036    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.065 on 183 degrees of freedom
    ## Multiple R-squared:  0.4807, Adjusted R-squared:  0.3928 
    ## F-statistic: 5.465 on 31 and 183 DF,  p-value: 7.025e-14

### extraversion

``` r
summary(models_ygame_lm[[4]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * real_p_ct, data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6786 -0.8567  0.0450  0.8524  3.4301 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        4.263583   0.504885   8.445 6.32e-15 ***
    ## real_p_ct                          0.452652   0.060153   7.525 1.78e-12 ***
    ## c_age                              0.002222   0.013707   0.162   0.8714    
    ## c_education                       -0.037289   0.081080  -0.460   0.6461    
    ## c_income                           0.088158   0.049919   1.766   0.0789 .  
    ## c_race2                            0.163340   0.388805   0.420   0.6749    
    ## c_race4                           -0.211526   0.420080  -0.504   0.6151    
    ## c_race6                            0.995478   1.006975   0.989   0.3241    
    ## c_race7                            0.623957   0.399541   1.562   0.1200    
    ## c_sex2                            -0.039127   0.200641  -0.195   0.8456    
    ## dissatis_autonomy_ct               0.099370   0.091116   1.091   0.2768    
    ## dissatis_relatedness_ct           -0.147457   0.094744  -1.556   0.1212    
    ## dissatis_competence_ct            -0.043486   0.094261  -0.461   0.6451    
    ## real_p_ct:dissatis_autonomy_ct    -0.002382   0.046580  -0.051   0.9593    
    ## real_p_ct:dissatis_relatedness_ct -0.012435   0.047452  -0.262   0.7936    
    ## real_p_ct:dissatis_competence_ct  -0.049968   0.046117  -1.084   0.2799    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.367 on 199 degrees of freedom
    ## Multiple R-squared:  0.3346, Adjusted R-squared:  0.2844 
    ## F-statistic: 6.671 on 15 and 199 DF,  p-value: 1.523e-11

``` r
summary(models_ygame_all_lm[[4]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + 
    ##     real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct), 
    ##     data = .x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8671 -0.7013 -0.0073  0.6857  3.9600 
    ## 
    ## Coefficients:
    ##                                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                                        4.4379351  0.5015218   8.849 7.45e-16 ***
    ## real_extraversion_ct                               0.4734246  0.0625929   7.564 1.84e-12 ***
    ## real_agreeableness_ct                             -0.0872488  0.0820450  -1.063  0.28899    
    ## real_conscientiousness_ct                          0.0269683  0.0990632   0.272  0.78575    
    ## real_emotionstability_ct                           0.0538065  0.0966655   0.557  0.57846    
    ## real_openness_ct                                   0.0327727  0.0789765   0.415  0.67865    
    ## c_age                                             -0.0015664  0.0137596  -0.114  0.90949    
    ## c_education                                       -0.0162555  0.0797704  -0.204  0.83875    
    ## c_income                                           0.0488013  0.0505192   0.966  0.33532    
    ## c_race2                                            0.0231147  0.3882890   0.060  0.95260    
    ## c_race4                                           -0.1765107  0.4142564  -0.426  0.67054    
    ## c_race6                                           -0.1846434  1.0949273  -0.169  0.86627    
    ## c_race7                                            0.2140982  0.4232238   0.506  0.61355    
    ## c_sex2                                            -0.0600775  0.2110106  -0.285  0.77619    
    ## dissatis_autonomy_ct                               0.0967552  0.0925530   1.045  0.29722    
    ## dissatis_relatedness_ct                           -0.2130722  0.0975356  -2.185  0.03019 *  
    ## dissatis_competence_ct                            -0.0260520  0.1120539  -0.232  0.81641    
    ## real_extraversion_ct:dissatis_autonomy_ct         -0.0145168  0.0538847  -0.269  0.78792    
    ## real_agreeableness_ct:dissatis_autonomy_ct         0.1922183  0.0845485   2.273  0.02416 *  
    ## real_conscientiousness_ct:dissatis_autonomy_ct     0.0552029  0.1040950   0.530  0.59654    
    ## real_emotionstability_ct:dissatis_autonomy_ct     -0.1597816  0.1084172  -1.474  0.14226    
    ## real_openness_ct:dissatis_autonomy_ct             -0.0007667  0.0715457  -0.011  0.99146    
    ## real_extraversion_ct:dissatis_relatedness_ct       0.0642958  0.0563063   1.142  0.25499    
    ## real_agreeableness_ct:dissatis_relatedness_ct     -0.0886191  0.0758497  -1.168  0.24418    
    ## real_conscientiousness_ct:dissatis_relatedness_ct  0.0899691  0.0822252   1.094  0.27531    
    ## real_emotionstability_ct:dissatis_relatedness_ct   0.1966051  0.0847426   2.320  0.02144 *  
    ## real_openness_ct:dissatis_relatedness_ct           0.0993390  0.0712380   1.394  0.16487    
    ## real_extraversion_ct:dissatis_competence_ct       -0.0661873  0.0568854  -1.164  0.24613    
    ## real_agreeableness_ct:dissatis_competence_ct      -0.0595060  0.0819425  -0.726  0.46865    
    ## real_conscientiousness_ct:dissatis_competence_ct  -0.2462540  0.0781088  -3.153  0.00189 ** 
    ## real_emotionstability_ct:dissatis_competence_ct   -0.0974454  0.0823468  -1.183  0.23820    
    ## real_openness_ct:dissatis_competence_ct           -0.1109324  0.0723710  -1.533  0.12704    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.32 on 183 degrees of freedom
    ## Multiple R-squared:  0.4289, Adjusted R-squared:  0.3322 
    ## F-statistic: 4.434 on 31 and 183 DF,  p-value: 8.495e-11

### openness

``` r
summary(models_ygame_lm[[5]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * real_p_ct, data = .x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.54989 -0.52155  0.04135  0.48705  2.24184 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        5.910726   0.297334  19.879   <2e-16 ***
    ## real_p_ct                          0.509238   0.046215  11.019   <2e-16 ***
    ## c_age                              0.002794   0.008163   0.342   0.7325    
    ## c_education                       -0.075663   0.047167  -1.604   0.1103    
    ## c_income                          -0.040352   0.029823  -1.353   0.1776    
    ## c_race2                           -0.223774   0.230932  -0.969   0.3337    
    ## c_race4                           -0.091972   0.248372  -0.370   0.7116    
    ## c_race6                           -0.693446   0.598614  -1.158   0.2481    
    ## c_race7                           -0.149301   0.244714  -0.610   0.5425    
    ## c_sex2                             0.260784   0.120083   2.172   0.0311 *  
    ## dissatis_autonomy_ct               0.136546   0.054080   2.525   0.0124 *  
    ## dissatis_relatedness_ct           -0.130783   0.056100  -2.331   0.0207 *  
    ## dissatis_competence_ct            -0.041188   0.053707  -0.767   0.4441    
    ## real_p_ct:dissatis_autonomy_ct    -0.073234   0.040922  -1.790   0.0750 .  
    ## real_p_ct:dissatis_relatedness_ct  0.075588   0.039310   1.923   0.0559 .  
    ## real_p_ct:dissatis_competence_ct  -0.020217   0.038267  -0.528   0.5979    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8108 on 199 degrees of freedom
    ## Multiple R-squared:  0.5216, Adjusted R-squared:  0.4855 
    ## F-statistic: 14.46 on 15 and 199 DF,  p-value: < 2.2e-16

``` r
summary(models_ygame_all_lm[[5]])
```

    ## 
    ## Call:
    ## lm(formula = game_p ~ . + (dissatis_autonomy_ct + dissatis_relatedness_ct + 
    ##     dissatis_competence_ct) * (real_extraversion_ct + real_agreeableness_ct + 
    ##     real_conscientiousness_ct + real_emotionstability_ct + real_openness_ct), 
    ##     data = .x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.14636 -0.47003  0.04566  0.47253  1.99342 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                                        5.933962   0.298218  19.898   <2e-16 ***
    ## real_extraversion_ct                              -0.024183   0.037219  -0.650   0.5167    
    ## real_agreeableness_ct                             -0.084651   0.048786  -1.735   0.0844 .  
    ## real_conscientiousness_ct                          0.031629   0.058905   0.537   0.5920    
    ## real_emotionstability_ct                          -0.002364   0.057480  -0.041   0.9672    
    ## real_openness_ct                                   0.483054   0.046961  10.286   <2e-16 ***
    ## c_age                                              0.001124   0.008182   0.137   0.8909    
    ## c_education                                       -0.072272   0.047434  -1.524   0.1293    
    ## c_income                                          -0.052612   0.030040  -1.751   0.0816 .  
    ## c_race2                                           -0.241231   0.230886  -1.045   0.2975    
    ## c_race4                                            0.058818   0.246327   0.239   0.8115    
    ## c_race6                                           -0.840663   0.651072  -1.291   0.1983    
    ## c_race7                                           -0.250367   0.251660  -0.995   0.3211    
    ## c_sex2                                             0.264602   0.125472   2.109   0.0363 *  
    ## dissatis_autonomy_ct                               0.108022   0.055034   1.963   0.0512 .  
    ## dissatis_relatedness_ct                           -0.114595   0.057997  -1.976   0.0497 *  
    ## dissatis_competence_ct                            -0.048190   0.066630  -0.723   0.4705    
    ## real_extraversion_ct:dissatis_autonomy_ct         -0.029884   0.032041  -0.933   0.3522    
    ## real_agreeableness_ct:dissatis_autonomy_ct        -0.045107   0.050275  -0.897   0.3708    
    ## real_conscientiousness_ct:dissatis_autonomy_ct     0.090557   0.061898   1.463   0.1452    
    ## real_emotionstability_ct:dissatis_autonomy_ct      0.038937   0.064468   0.604   0.5466    
    ## real_openness_ct:dissatis_autonomy_ct             -0.060351   0.042543  -1.419   0.1577    
    ## real_extraversion_ct:dissatis_relatedness_ct       0.007496   0.033481   0.224   0.8231    
    ## real_agreeableness_ct:dissatis_relatedness_ct     -0.095002   0.045102  -2.106   0.0365 *  
    ## real_conscientiousness_ct:dissatis_relatedness_ct  0.031749   0.048893   0.649   0.5169    
    ## real_emotionstability_ct:dissatis_relatedness_ct   0.018002   0.050390   0.357   0.7213    
    ## real_openness_ct:dissatis_relatedness_ct           0.080245   0.042360   1.894   0.0598 .  
    ## real_extraversion_ct:dissatis_competence_ct       -0.018068   0.033826  -0.534   0.5939    
    ## real_agreeableness_ct:dissatis_competence_ct       0.069242   0.048725   1.421   0.1570    
    ## real_conscientiousness_ct:dissatis_competence_ct  -0.117471   0.046445  -2.529   0.0123 *  
    ## real_emotionstability_ct:dissatis_competence_ct   -0.032903   0.048965  -0.672   0.5025    
    ## real_openness_ct:dissatis_competence_ct           -0.034598   0.043034  -0.804   0.4225    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.785 on 183 degrees of freedom
    ## Multiple R-squared:  0.5876, Adjusted R-squared:  0.5178 
    ## F-statistic: 8.412 on 31 and 183 DF,  p-value: < 2.2e-16

Information criteria
--------------------

### BIC

``` r
BICs_ygame_lm <- unlist(map(models_ygame_lm, BIC))
BICs_ygame_all_lm <- unlist(map(models_ygame_all_lm, BIC))

print(BICs_ygame_lm)
```

    ## [1] 718.2218 617.3777 756.8900 819.0866 594.6374

``` r
print(BICs_ygame_all_lm)
```

    ## [1] 772.7945 689.7571 779.9422 872.1511 648.6269

### AIC

``` r
AICs_ygame_lm <- unlist(map(models_ygame_lm, AIC))
AICs_ygame_all_lm <- unlist(map(models_ygame_all_lm, AIC))

print(AICs_ygame_lm)
```

    ## [1] 660.9210 560.0769 699.5891 761.7857 537.3365

``` r
print(AICs_ygame_all_lm)
```

    ## [1] 661.5634 578.5261 668.7111 760.9201 537.3959
