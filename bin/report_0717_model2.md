MAPSS Thesis II - model2
================
Chih-Yu Chiang
July 17, 2017

``` r
knitr::opts_chunk$set(
    message = FALSE,
    warning = FALSE
)
```

Setup
-----

Data of game and player are read in and matched up.

-   Game release data, `release` (year), is read in as an interval variable.
-   Missing values are imputed with variable mean conveniently (`star_user` and `star_GS`).

``` r
#--Package
library(tidyverse)
library(modelr)
library(glmnet)
library(randomForest)
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

#Player df, key=player
df_player <- distinct(df, respondent, .keep_all = TRUE)
```

Variable
--------

Compute and select variables to be used in models.

-   Call the function to update the vars employed.
-   Final response variable utilizes only `preference_3`.

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
    rowwise() %>% 
    mutate(preference = mean(c(preference_1)))
  
  
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
  predictors <<- read.csv("../data/vars/predictors.csv", header=TRUE, na.strings="")
  
  #Get column name as model id
  modelId <<- colnames(predictors)
  
  #predictor variable as strings for each model
  predictorString <<- apply(predictors, MARGIN=2, function(x) paste(na.omit(x), collapse="+"))
  
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


#--Train models
#gap ~ real + c
model_ygap_1 <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), starts_with("real"), starts_with("c_")))

#gap ~ dissatis + c
model_ygap_2 <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), starts_with("c_"), starts_with("dissatis")))

#gap ~ real + satis + c
model_ygap_3 <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ .,
                 data=select(df_player_c, starts_with("gap"), starts_with("real"), starts_with("c_"), starts_with("combined")))

#gap ~ real + dissatis + real * dissatis + c
model_ygap_4 <- lm(cbind(gap_extraversion, gap_agreeableness, gap_conscientiousness, gap_emotionstability, gap_openness) ~ . +
                   (dissatis_autonomy + dissatis_relatedness + dissatis_competence) * (real_extraversion + real_agreeableness + real_conscientiousness + real_emotionstability + real_openness),
                 data=select(df_player_c, starts_with("gap"), starts_with("real"), starts_with("c_"), starts_with("dissatis")))
```

Difference between real and game personality
--------------------------------------------

``` r
#T test for each pair
#T test result dif: game - real
t.test(df_player$game_agreeableness, df_player$real_agreeableness, paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  df_player$game_agreeableness and df_player$real_agreeableness
    ## t = 5.4368, df = 214, p-value = 1.474e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2801813 0.5988885
    ## sample estimates:
    ## mean of the differences 
    ##               0.4395349

``` r
t.test(df_player$game_conscientiousness, df_player$real_conscientiousness, paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  df_player$game_conscientiousness and df_player$real_conscientiousness
    ## t = 2.4307, df = 214, p-value = 0.01589
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.03253805 0.31164800
    ## sample estimates:
    ## mean of the differences 
    ##                0.172093

``` r
t.test(df_player$game_extraversion, df_player$real_extraversion, paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  df_player$game_extraversion and df_player$real_extraversion
    ## t = 9.9484, df = 214, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.9062962 1.3541690
    ## sample estimates:
    ## mean of the differences 
    ##                1.130233

``` r
t.test(df_player$game_emotionstability, df_player$real_emotionstability, paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  df_player$game_emotionstability and df_player$real_emotionstability
    ## t = -2.4136, df = 214, p-value = 0.01664
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.43092873 -0.04348987
    ## sample estimates:
    ## mean of the differences 
    ##              -0.2372093

``` r
t.test(df_player$game_openness, df_player$real_openness, paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  df_player$game_openness and df_player$real_openness
    ## t = 5.2845, df = 214, p-value = 3.098e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2405931 0.5268488
    ## sample estimates:
    ## mean of the differences 
    ##               0.3837209

gap ~ real personality
----------------------

``` r
#Results of seperate models
summary(model_ygap_1)
```

    ## Response gap_extraversion :
    ## 
    ## Call:
    ## lm(formula = gap_extraversion ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6406 -0.8297  0.0622  0.8307  3.5394 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.679961   0.994016   2.696  0.00761 ** 
    ## real_extraversion      -0.550949   0.059056  -9.329  < 2e-16 ***
    ## real_agreeableness     -0.062387   0.081112  -0.769  0.44271    
    ## real_conscientiousness -0.046280   0.088203  -0.525  0.60037    
    ## real_emotionstability  -0.002693   0.088404  -0.030  0.97573    
    ## real_openness           0.094638   0.073109   1.294  0.19699    
    ## c_age                   0.002742   0.013698   0.200  0.84157    
    ## c_education            -0.048197   0.078470  -0.614  0.53977    
    ## c_income                0.093016   0.050094   1.857  0.06480 .  
    ## c_race2                 0.053872   0.388938   0.139  0.88998    
    ## c_race4                -0.254050   0.420158  -0.605  0.54609    
    ## c_race6                 1.323456   0.995925   1.329  0.18540    
    ## c_race7                 0.685305   0.399136   1.717  0.08752 .  
    ## c_sex2                 -0.063472   0.211369  -0.300  0.76427    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.377 on 201 degrees of freedom
    ## Multiple R-squared:  0.3584, Adjusted R-squared:  0.3169 
    ## F-statistic: 8.638 on 13 and 201 DF,  p-value: 7.059e-14
    ## 
    ## 
    ## Response gap_agreeableness :
    ## 
    ## Call:
    ## lm(formula = gap_agreeableness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2698 -0.7799 -0.1089  0.5463  3.0845 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.400733   0.770997   1.817   0.0707 .  
    ## real_extraversion       0.084576   0.045806   1.846   0.0663 .  
    ## real_agreeableness     -0.434035   0.062913  -6.899 6.67e-11 ***
    ## real_conscientiousness -0.056784   0.068414  -0.830   0.4075    
    ## real_emotionstability   0.071447   0.068569   1.042   0.2987    
    ## real_openness          -0.047382   0.056706  -0.836   0.4044    
    ## c_age                   0.005807   0.010625   0.547   0.5853    
    ## c_education             0.053890   0.060864   0.885   0.3770    
    ## c_income                0.001814   0.038855   0.047   0.9628    
    ## c_race2                 0.063287   0.301675   0.210   0.8340    
    ## c_race4                -0.097699   0.325891  -0.300   0.7646    
    ## c_race6                -1.338741   0.772477  -1.733   0.0846 .  
    ## c_race7                 0.201649   0.309585   0.651   0.5156    
    ## c_sex2                 -0.277782   0.163946  -1.694   0.0917 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.068 on 201 degrees of freedom
    ## Multiple R-squared:  0.2378, Adjusted R-squared:  0.1885 
    ## F-statistic: 4.823 on 13 and 201 DF,  p-value: 2.669e-07
    ## 
    ## 
    ## Response gap_conscientiousness :
    ## 
    ## Call:
    ## lm(formula = gap_conscientiousness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2357 -0.4499 -0.0342  0.4513  2.8889 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.3558877  0.6221254   3.787 0.000201 ***
    ## real_extraversion      -0.0293564  0.0369612  -0.794 0.427988    
    ## real_agreeableness     -0.0552196  0.0507655  -1.088 0.278014    
    ## real_conscientiousness -0.4369998  0.0552037  -7.916 1.62e-13 ***
    ## real_emotionstability   0.0119809  0.0553292   0.217 0.828787    
    ## real_openness           0.0998125  0.0457570   2.181 0.030317 *  
    ## c_age                   0.0051160  0.0085731   0.597 0.551344    
    ## c_education            -0.0269038  0.0491120  -0.548 0.584433    
    ## c_income               -0.0418163  0.0313522  -1.334 0.183792    
    ## c_race2                -0.1075456  0.2434246  -0.442 0.659107    
    ## c_race4                -0.0874544  0.2629647  -0.333 0.739805    
    ## c_race6                 0.4516767  0.6233201   0.725 0.469521    
    ## c_race7                -0.3016111  0.2498074  -1.207 0.228707    
    ## c_sex2                  0.0009074  0.1322896   0.007 0.994534    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8617 on 201 degrees of freedom
    ## Multiple R-squared:  0.3529, Adjusted R-squared:  0.3111 
    ## F-statistic: 8.432 on 13 and 201 DF,  p-value: 1.539e-13
    ## 
    ## 
    ## Response gap_emotionstability :
    ## 
    ## Call:
    ## lm(formula = gap_emotionstability ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9413 -0.8101 -0.0570  0.7296  2.6876 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             0.871013   0.849871   1.025  0.30665    
    ## real_extraversion       0.146945   0.050492   2.910  0.00402 ** 
    ## real_agreeableness      0.065235   0.069350   0.941  0.34800    
    ## real_conscientiousness -0.066281   0.075413  -0.879  0.38050    
    ## real_emotionstability  -0.521864   0.075584  -6.904 6.47e-11 ***
    ## real_openness           0.005412   0.062508   0.087  0.93109    
    ## c_age                   0.007973   0.011712   0.681  0.49679    
    ## c_education            -0.012082   0.067091  -0.180  0.85727    
    ## c_income                0.005271   0.042830   0.123  0.90217    
    ## c_race2                -0.094897   0.332537  -0.285  0.77565    
    ## c_race4                -0.095902   0.359230  -0.267  0.78977    
    ## c_race6                -0.046638   0.851503  -0.055  0.95637    
    ## c_race7                -0.412596   0.341256  -1.209  0.22806    
    ## c_sex2                 -0.109508   0.180718  -0.606  0.54522    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.177 on 201 degrees of freedom
    ## Multiple R-squared:  0.3733, Adjusted R-squared:  0.3328 
    ## F-statistic:  9.21 on 13 and 201 DF,  p-value: 8.321e-15
    ## 
    ## 
    ## Response gap_openness :
    ## 
    ## Call:
    ## lm(formula = gap_openness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_")))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.41817 -0.48001  0.06083  0.52534  2.07348 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.434778   0.602334   5.702 4.17e-08 ***
    ## real_extraversion      -0.023876   0.035785  -0.667    0.505    
    ## real_agreeableness     -0.067016   0.049150  -1.363    0.174    
    ## real_conscientiousness  0.034462   0.053448   0.645    0.520    
    ## real_emotionstability  -0.012551   0.053569  -0.234    0.815    
    ## real_openness          -0.486377   0.044301 -10.979  < 2e-16 ***
    ## c_age                   0.000240   0.008300   0.029    0.977    
    ## c_education            -0.070802   0.047550  -1.489    0.138    
    ## c_income               -0.034235   0.030355  -1.128    0.261    
    ## c_race2                -0.326043   0.235681  -1.383    0.168    
    ## c_race4                -0.071900   0.254599  -0.282    0.778    
    ## c_race6                -0.674929   0.603491  -1.118    0.265    
    ## c_race7                -0.008755   0.241860  -0.036    0.971    
    ## c_sex2                  0.218167   0.128081   1.703    0.090 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8343 on 201 degrees of freedom
    ## Multiple R-squared:  0.4233, Adjusted R-squared:  0.386 
    ## F-statistic: 11.35 on 13 and 201 DF,  p-value: < 2.2e-16

``` r
#MANOVA
Anova(model_ygap_1)
```

    ## 
    ## Type II MANOVA Tests: Pillai test statistic
    ##                        Df test stat approx F num Df den Df    Pr(>F)    
    ## real_extraversion       1   0.36817   22.958      5    197 < 2.2e-16 ***
    ## real_agreeableness      1   0.24291   12.642      5    197 1.172e-10 ***
    ## real_conscientiousness  1   0.27328   14.816      5    197 2.465e-12 ***
    ## real_emotionstability   1   0.25927   13.791      5    197 1.497e-11 ***
    ## real_openness           1   0.45775   33.261      5    197 < 2.2e-16 ***
    ## c_age                   1   0.00633    0.251      5    197    0.9389    
    ## c_education             1   0.02043    0.822      5    197    0.5355    
    ## c_income                1   0.04455    1.837      5    197    0.1073    
    ## c_race                  4   0.10487    1.077     20    800    0.3687    
    ## c_sex                   1   0.03052    1.240      5    197    0.2918    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

gap ~ dissatisfaction
=====================

``` r
#Results of seperate models
summary(model_ygap_2)
```

    ## Response gap_extraversion :
    ## 
    ## Call:
    ## lm(formula = gap_extraversion ~ c_age + c_education + c_income + 
    ##     c_race + c_sex + dissatis_autonomy + dissatis_relatedness + 
    ##     dissatis_competence, data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3372 -0.9674 -0.3709  0.9907  4.2562 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)          -0.04680    0.68946  -0.068   0.9459  
    ## c_age                 0.01629    0.01595   1.022   0.3082  
    ## c_education          -0.08927    0.09317  -0.958   0.3392  
    ## c_income              0.05810    0.05853   0.993   0.3220  
    ## c_race2              -0.09539    0.45498  -0.210   0.8341  
    ## c_race4              -0.60442    0.48982  -1.234   0.2186  
    ## c_race6               0.97520    1.17876   0.827   0.4090  
    ## c_race7               0.80711    0.46939   1.719   0.0871 .
    ## c_sex2                0.06331    0.23554   0.269   0.7884  
    ## dissatis_autonomy     0.17186    0.10491   1.638   0.1030  
    ## dissatis_relatedness -0.25849    0.10713  -2.413   0.0167 *
    ## dissatis_competence   0.23998    0.10231   2.346   0.0200 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.612 on 203 degrees of freedom
    ## Multiple R-squared:  0.1116, Adjusted R-squared:  0.06345 
    ## F-statistic: 2.318 on 11 and 203 DF,  p-value: 0.01063
    ## 
    ## 
    ## Response gap_agreeableness :
    ## 
    ## Call:
    ## lm(formula = gap_agreeableness ~ c_age + c_education + c_income + 
    ##     c_race + c_sex + dissatis_autonomy + dissatis_relatedness + 
    ##     dissatis_competence, data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7579 -0.6422 -0.1571  0.6517  3.3941 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)           0.233766   0.507407   0.461   0.6455  
    ## c_age                 0.008776   0.011737   0.748   0.4555  
    ## c_education           0.062911   0.068569   0.917   0.3600  
    ## c_income              0.009610   0.043072   0.223   0.8237  
    ## c_race2               0.093226   0.334839   0.278   0.7810  
    ## c_race4              -0.061179   0.360484  -0.170   0.8654  
    ## c_race6              -1.682834   0.867508  -1.940   0.0538 .
    ## c_race7               0.265084   0.345449   0.767   0.4438  
    ## c_sex2               -0.072478   0.173344  -0.418   0.6763  
    ## dissatis_autonomy    -0.053072   0.077210  -0.687   0.4926  
    ## dissatis_relatedness -0.003527   0.078841  -0.045   0.9644  
    ## dissatis_competence  -0.039643   0.075292  -0.527   0.5991  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.186 on 203 degrees of freedom
    ## Multiple R-squared:  0.04976,    Adjusted R-squared:  -0.00173 
    ## F-statistic: 0.9664 on 11 and 203 DF,  p-value: 0.4783
    ## 
    ## 
    ## Response gap_conscientiousness :
    ## 
    ## Call:
    ## lm(formula = gap_conscientiousness ~ c_age + c_education + c_income + 
    ##     c_race + c_sex + dissatis_autonomy + dissatis_relatedness + 
    ##     dissatis_competence, data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1972 -0.4777 -0.0429  0.4069  3.3992 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)           0.067393   0.425111   0.159  0.87420   
    ## c_age                -0.002685   0.009833  -0.273  0.78506   
    ## c_education          -0.026185   0.057448  -0.456  0.64902   
    ## c_income             -0.053909   0.036086  -1.494  0.13676   
    ## c_race2              -0.223759   0.280532  -0.798  0.42602   
    ## c_race4               0.094425   0.302018   0.313  0.75487   
    ## c_race6              -0.092866   0.726807  -0.128  0.89846   
    ## c_race7              -0.458604   0.289421  -1.585  0.11462   
    ## c_sex2                0.013214   0.145229   0.091  0.92759   
    ## dissatis_autonomy     0.015297   0.064688   0.236  0.81330   
    ## dissatis_relatedness -0.048109   0.066054  -0.728  0.46725   
    ## dissatis_competence   0.201268   0.063081   3.191  0.00164 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.994 on 203 degrees of freedom
    ## Multiple R-squared:  0.1303, Adjusted R-squared:  0.0832 
    ## F-statistic: 2.765 on 11 and 203 DF,  p-value: 0.0023
    ## 
    ## 
    ## Response gap_emotionstability :
    ## 
    ## Call:
    ## lm(formula = gap_emotionstability ~ c_age + c_education + c_income + 
    ##     c_race + c_sex + dissatis_autonomy + dissatis_relatedness + 
    ##     dissatis_competence, data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6561 -0.6377  0.0305  0.8493  3.0062 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.4389593  0.5691874   0.771   0.4415    
    ## c_age                 0.0001247  0.0131658   0.009   0.9925    
    ## c_education           0.0172273  0.0769181   0.224   0.8230    
    ## c_income              0.0463649  0.0483165   0.960   0.3384    
    ## c_race2              -0.1285707  0.3756080  -0.342   0.7325    
    ## c_race4              -0.0526551  0.4043756  -0.130   0.8965    
    ## c_race6               0.8640304  0.9731329   0.888   0.3757    
    ## c_race7              -0.4360285  0.3875095  -1.125   0.2618    
    ## c_sex2               -0.4159002  0.1944495  -2.139   0.0336 *  
    ## dissatis_autonomy     0.1044226  0.0866112   1.206   0.2294    
    ## dissatis_relatedness  0.0644280  0.0884405   0.728   0.4672    
    ## dissatis_competence  -0.4184832  0.0844597  -4.955 1.52e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.331 on 203 degrees of freedom
    ## Multiple R-squared:  0.1909, Adjusted R-squared:  0.147 
    ## F-statistic: 4.354 on 11 and 203 DF,  p-value: 7.472e-06
    ## 
    ## 
    ## Response gap_openness :
    ## 
    ## Call:
    ## lm(formula = gap_openness ~ c_age + c_education + c_income + 
    ##     c_race + c_sex + dissatis_autonomy + dissatis_relatedness + 
    ##     dissatis_competence, data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0515 -0.6272 -0.1357  0.4069  3.2137 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)          -0.328703   0.443681  -0.741   0.4596  
    ## c_age                 0.009808   0.010263   0.956   0.3404  
    ## c_education          -0.051143   0.059958  -0.853   0.3947  
    ## c_income             -0.012105   0.037663  -0.321   0.7482  
    ## c_race2              -0.160632   0.292786  -0.549   0.5839  
    ## c_race4              -0.221275   0.315211  -0.702   0.4835  
    ## c_race6              -1.323099   0.758557  -1.744   0.0826 .
    ## c_race7               0.075637   0.302063   0.250   0.8025  
    ## c_sex2                0.131491   0.151573   0.868   0.3867  
    ## dissatis_autonomy     0.100357   0.067513   1.486   0.1387  
    ## dissatis_relatedness -0.055557   0.068939  -0.806   0.4213  
    ## dissatis_competence   0.132775   0.065836   2.017   0.0450 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.037 on 203 degrees of freedom
    ## Multiple R-squared:  0.09939,    Adjusted R-squared:  0.05059 
    ## F-statistic: 2.037 on 11 and 203 DF,  p-value: 0.02668

``` r
#MANOVA
Anova(model_ygap_2)
```

    ## 
    ## Type II MANOVA Tests: Pillai test statistic
    ##                      Df test stat approx F num Df den Df    Pr(>F)    
    ## c_age                 1  0.009984   0.4014      5    199    0.8475    
    ## c_education           1  0.013356   0.5388      5    199    0.7468    
    ## c_income              1  0.020840   0.8471      5    199    0.5179    
    ## c_race                4  0.110438   1.1471     20    808    0.2951    
    ## c_sex                 1  0.023949   0.9766      5    199    0.4332    
    ## dissatis_autonomy     1  0.045020   1.8763      5    199    0.1000    
    ## dissatis_relatedness  1  0.029910   1.2271      5    199    0.2977    
    ## dissatis_competence   1  0.136377   6.2849      5    199 1.941e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

gap ~ real personality + dissatisfaction
----------------------------------------

``` r
#Results of seperate models
summary(model_ygap_3)
```

    ## Response gap_extraversion :
    ## 
    ## Call:
    ## lm(formula = gap_extraversion ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + combined_autonomy + 
    ##     combined_relatedness + combined_competence, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_"), 
    ##     starts_with("combined")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5353 -0.7985  0.0191  0.8515  3.3917 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.347563   1.078806   2.176   0.0307 *  
    ## real_extraversion      -0.547121   0.061910  -8.837 5.32e-16 ***
    ## real_agreeableness     -0.044156   0.082324  -0.536   0.5923    
    ## real_conscientiousness -0.088228   0.096017  -0.919   0.3593    
    ## real_emotionstability   0.005465   0.093419   0.059   0.9534    
    ## real_openness           0.061986   0.079032   0.784   0.4338    
    ## c_age                   0.002444   0.013835   0.177   0.8599    
    ## c_education            -0.045271   0.079630  -0.569   0.5703    
    ## c_income                0.090985   0.050406   1.805   0.0726 .  
    ## c_race2                 0.120250   0.390901   0.308   0.7587    
    ## c_race4                -0.300208   0.421974  -0.711   0.4777    
    ## c_race6                 1.213042   1.018765   1.191   0.2352    
    ## c_race7                 0.647935   0.403778   1.605   0.1102    
    ## c_sex2                 -0.082532   0.212089  -0.389   0.6976    
    ## combined_autonomy      -0.137286   0.129659  -1.059   0.2910    
    ## combined_relatedness    0.188135   0.121752   1.545   0.1239    
    ## combined_competence     0.056600   0.138915   0.407   0.6841    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.377 on 198 degrees of freedom
    ## Multiple R-squared:  0.368,  Adjusted R-squared:  0.3169 
    ## F-statistic: 7.205 on 16 and 198 DF,  p-value: 4.737e-13
    ## 
    ## 
    ## Response gap_agreeableness :
    ## 
    ## Call:
    ## lm(formula = gap_agreeableness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + combined_autonomy + 
    ##     combined_relatedness + combined_competence, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_"), 
    ##     starts_with("combined")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1534 -0.7512 -0.1055  0.5166  3.0544 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.2437083  0.8408709   1.479   0.1407    
    ## real_extraversion       0.0699667  0.0482553   1.450   0.1487    
    ## real_agreeableness     -0.4323847  0.0641675  -6.738 1.71e-10 ***
    ## real_conscientiousness -0.0750532  0.0748403  -1.003   0.3172    
    ## real_emotionstability   0.0925933  0.0728152   1.272   0.2050    
    ## real_openness          -0.0624176  0.0616011  -1.013   0.3122    
    ## c_age                   0.0046857  0.0107836   0.435   0.6644    
    ## c_education             0.0563891  0.0620672   0.909   0.3647    
    ## c_income               -0.0009307  0.0392889  -0.024   0.9811    
    ## c_race2                 0.0621445  0.3046864   0.204   0.8386    
    ## c_race4                -0.0729591  0.3289060  -0.222   0.8247    
    ## c_race6                -1.1830436  0.7940728  -1.490   0.1379    
    ## c_race7                 0.2215443  0.3147231   0.704   0.4823    
    ## c_sex2                 -0.2863349  0.1653117  -1.732   0.0848 .  
    ## combined_autonomy       0.0449622  0.1010623   0.445   0.6569    
    ## combined_relatedness   -0.0319556  0.0948989  -0.337   0.7367    
    ## combined_competence     0.0607621  0.1082767   0.561   0.5753    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.073 on 198 degrees of freedom
    ## Multiple R-squared:  0.2417, Adjusted R-squared:  0.1804 
    ## F-statistic: 3.944 on 16 and 198 DF,  p-value: 1.893e-06
    ## 
    ## 
    ## Response gap_conscientiousness :
    ## 
    ## Call:
    ## lm(formula = gap_conscientiousness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + combined_autonomy + 
    ##     combined_relatedness + combined_competence, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_"), 
    ##     starts_with("combined")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2613 -0.4446 -0.0557  0.4865  2.7952 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.159161   0.677603   3.186  0.00167 ** 
    ## real_extraversion      -0.025268   0.038886  -0.650  0.51658    
    ## real_agreeableness     -0.045692   0.051708  -0.884  0.37796    
    ## real_conscientiousness -0.447769   0.060309  -7.425 3.28e-12 ***
    ## real_emotionstability   0.008034   0.058677   0.137  0.89123    
    ## real_openness           0.089917   0.049640   1.811  0.07160 .  
    ## c_age                   0.005633   0.008690   0.648  0.51760    
    ## c_education            -0.021751   0.050016  -0.435  0.66413    
    ## c_income               -0.043460   0.031660  -1.373  0.17140    
    ## c_race2                -0.081429   0.245527  -0.332  0.74050    
    ## c_race4                -0.112990   0.265044  -0.426  0.67035    
    ## c_race6                 0.353587   0.639891   0.553  0.58118    
    ## c_race7                -0.337823   0.253615  -1.332  0.18438    
    ## c_sex2                 -0.004308   0.133214  -0.032  0.97423    
    ## combined_autonomy      -0.028161   0.081440  -0.346  0.72987    
    ## combined_relatedness    0.095367   0.076473   1.247  0.21385    
    ## combined_competence    -0.025655   0.087253  -0.294  0.76904    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8648 on 198 degrees of freedom
    ## Multiple R-squared:  0.358,  Adjusted R-squared:  0.3061 
    ## F-statistic: 6.899 on 16 and 198 DF,  p-value: 1.861e-12
    ## 
    ## 
    ## Response gap_emotionstability :
    ## 
    ## Call:
    ## lm(formula = gap_emotionstability ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + combined_autonomy + 
    ##     combined_relatedness + combined_competence, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_"), 
    ##     starts_with("combined")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7853 -0.7608 -0.0349  0.7668  2.7009 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.466907   0.919459   1.595  0.11222    
    ## real_extraversion       0.146821   0.052765   2.783  0.00592 ** 
    ## real_agreeableness      0.045123   0.070165   0.643  0.52091    
    ## real_conscientiousness -0.059636   0.081835  -0.729  0.46702    
    ## real_emotionstability  -0.510864   0.079621  -6.416 1.01e-09 ***
    ## real_openness           0.018575   0.067358   0.276  0.78302    
    ## c_age                   0.006122   0.011791   0.519  0.60422    
    ## c_education            -0.034710   0.067868  -0.511  0.60962    
    ## c_income                0.012701   0.042961   0.296  0.76781    
    ## c_race2                -0.120473   0.333163  -0.362  0.71803    
    ## c_race4                -0.065431   0.359646  -0.182  0.85582    
    ## c_race6                 0.112124   0.868287   0.129  0.89738    
    ## c_race7                -0.313635   0.344137  -0.911  0.36321    
    ## c_sex2                 -0.104712   0.180762  -0.579  0.56306    
    ## combined_autonomy      -0.110568   0.110508  -1.001  0.31827    
    ## combined_relatedness   -0.147374   0.103768  -1.420  0.15711    
    ## combined_competence     0.148890   0.118396   1.258  0.21003    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.173 on 198 degrees of freedom
    ## Multiple R-squared:  0.3865, Adjusted R-squared:  0.3369 
    ## F-statistic: 7.796 on 16 and 198 DF,  p-value: 3.48e-14
    ## 
    ## 
    ## Response gap_openness :
    ## 
    ## Call:
    ## lm(formula = gap_openness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + combined_autonomy + 
    ##     combined_relatedness + combined_competence, data = select(df_player_c, 
    ##     starts_with("gap"), starts_with("real"), starts_with("c_"), 
    ##     starts_with("combined")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3435 -0.5189  0.0400  0.5074  2.1538 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.2698757  0.6455526   5.065 9.31e-07 ***
    ## real_extraversion      -0.0109134  0.0370465  -0.295   0.7686    
    ## real_agreeableness     -0.0527753  0.0492626  -1.071   0.2853    
    ## real_conscientiousness  0.0090764  0.0574563   0.158   0.8746    
    ## real_emotionstability  -0.0184552  0.0559016  -0.330   0.7416    
    ## real_openness          -0.5051894  0.0472923 -10.682  < 2e-16 ***
    ## c_age                   0.0006086  0.0082787   0.074   0.9415    
    ## c_education            -0.0709839  0.0476502  -1.490   0.1379    
    ## c_income               -0.0338810  0.0301628  -1.123   0.2627    
    ## c_race2                -0.2670476  0.2339136  -1.142   0.2550    
    ## c_race4                -0.1278890  0.2525074  -0.506   0.6131    
    ## c_race6                -0.8673751  0.6096248  -1.423   0.1564    
    ## c_race7                -0.0501333  0.2416189  -0.207   0.8358    
    ## c_sex2                  0.2068904  0.1269129   1.630   0.1047    
    ## combined_autonomy      -0.1583135  0.0775875  -2.040   0.0426 *  
    ## combined_relatedness    0.1820530  0.0728557   2.499   0.0133 *  
    ## combined_competence     0.0188968  0.0831261   0.227   0.8204    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8239 on 198 degrees of freedom
    ## Multiple R-squared:  0.446,  Adjusted R-squared:  0.4012 
    ## F-statistic: 9.962 on 16 and 198 DF,  p-value: < 2.2e-16

``` r
#MANOVA
Anova(model_ygap_3)
```

    ## 
    ## Type II MANOVA Tests: Pillai test statistic
    ##                        Df test stat approx F num Df den Df    Pr(>F)    
    ## real_extraversion       1   0.34711  20.6277      5    194 < 2.2e-16 ***
    ## real_agreeableness      1   0.23161  11.6955      5    194 6.839e-10 ***
    ## real_conscientiousness  1   0.24786  12.7861      5    194 9.489e-11 ***
    ## real_emotionstability   1   0.24553  12.6267      5    194 1.264e-10 ***
    ## real_openness           1   0.43230  29.5458      5    194 < 2.2e-16 ***
    ## c_age                   1   0.00530   0.2067      5    194    0.9594    
    ## c_education             1   0.02539   1.0108      5    194    0.4125    
    ## c_income                1   0.04331   1.7565      5    194    0.1236    
    ## c_race                  4   0.09782   0.9877     20    788    0.4747    
    ## c_sex                   1   0.03098   1.2403      5    194    0.2918    
    ## combined_autonomy       1   0.04065   1.6442      5    194    0.1501    
    ## combined_relatedness    1   0.03902   1.5755      5    194    0.1687    
    ## combined_competence     1   0.00992   0.3888      5    194    0.8561    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

gap ~ real personality + dissatisfaction + real \* dissatis
-----------------------------------------------------------

``` r
#Results of seperate models
summary(model_ygap_4)
```

    ## Response gap_extraversion :
    ## 
    ## Call:
    ## lm(formula = gap_extraversion ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence + (dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence) * (real_extraversion + 
    ##     real_agreeableness + real_conscientiousness + real_emotionstability + 
    ##     real_openness), data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("real"), starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8671 -0.7013 -0.0073  0.6857  3.9600 
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                 -0.1602509  2.6830709  -0.060
    ## real_extraversion                           -0.4348752  0.1546082  -2.813
    ## real_agreeableness                          -0.3648570  0.2094768  -1.742
    ## real_conscientiousness                       0.3743902  0.2970899   1.260
    ## real_emotionstability                        0.4226794  0.2695517   1.568
    ## real_openness                                0.1218830  0.2338512   0.521
    ## c_age                                       -0.0015664  0.0137596  -0.114
    ## c_education                                 -0.0162555  0.0797704  -0.204
    ## c_income                                     0.0488013  0.0505192   0.966
    ## c_race2                                      0.0231147  0.3882890   0.060
    ## c_race4                                     -0.1765107  0.4142564  -0.426
    ## c_race6                                     -0.1846434  1.0949273  -0.169
    ## c_race7                                      0.2140982  0.4232238   0.506
    ## c_sex2                                      -0.0600775  0.2110106  -0.285
    ## dissatis_autonomy                           -0.1859812  1.0511892  -0.177
    ## dissatis_relatedness                        -1.7986914  0.9091520  -1.978
    ## dissatis_competence                          2.5623470  0.9001613   2.847
    ## real_extraversion:dissatis_autonomy         -0.0145168  0.0538847  -0.269
    ## real_agreeableness:dissatis_autonomy         0.1922183  0.0845485   2.273
    ## real_conscientiousness:dissatis_autonomy     0.0552029  0.1040950   0.530
    ## real_emotionstability:dissatis_autonomy     -0.1597816  0.1084172  -1.474
    ## real_openness:dissatis_autonomy             -0.0007667  0.0715457  -0.011
    ## real_extraversion:dissatis_relatedness       0.0642958  0.0563063   1.142
    ## real_agreeableness:dissatis_relatedness     -0.0886191  0.0758497  -1.168
    ## real_conscientiousness:dissatis_relatedness  0.0899691  0.0822252   1.094
    ## real_emotionstability:dissatis_relatedness   0.1966051  0.0847426   2.320
    ## real_openness:dissatis_relatedness           0.0993390  0.0712380   1.394
    ## real_extraversion:dissatis_competence       -0.0661873  0.0568854  -1.164
    ## real_agreeableness:dissatis_competence      -0.0595060  0.0819425  -0.726
    ## real_conscientiousness:dissatis_competence  -0.2462540  0.0781088  -3.153
    ## real_emotionstability:dissatis_competence   -0.0974454  0.0823468  -1.183
    ## real_openness:dissatis_competence           -0.1109324  0.0723710  -1.533
    ##                                             Pr(>|t|)   
    ## (Intercept)                                  0.95244   
    ## real_extraversion                            0.00545 **
    ## real_agreeableness                           0.08323 . 
    ## real_conscientiousness                       0.20920   
    ## real_emotionstability                        0.11859   
    ## real_openness                                0.60286   
    ## c_age                                        0.90949   
    ## c_education                                  0.83875   
    ## c_income                                     0.33532   
    ## c_race2                                      0.95260   
    ## c_race4                                      0.67054   
    ## c_race6                                      0.86627   
    ## c_race7                                      0.61355   
    ## c_sex2                                       0.77619   
    ## dissatis_autonomy                            0.85976   
    ## dissatis_relatedness                         0.04938 * 
    ## dissatis_competence                          0.00492 **
    ## real_extraversion:dissatis_autonomy          0.78792   
    ## real_agreeableness:dissatis_autonomy         0.02416 * 
    ## real_conscientiousness:dissatis_autonomy     0.59654   
    ## real_emotionstability:dissatis_autonomy      0.14226   
    ## real_openness:dissatis_autonomy              0.99146   
    ## real_extraversion:dissatis_relatedness       0.25499   
    ## real_agreeableness:dissatis_relatedness      0.24418   
    ## real_conscientiousness:dissatis_relatedness  0.27531   
    ## real_emotionstability:dissatis_relatedness   0.02144 * 
    ## real_openness:dissatis_relatedness           0.16487   
    ## real_extraversion:dissatis_competence        0.24613   
    ## real_agreeableness:dissatis_competence       0.46865   
    ## real_conscientiousness:dissatis_competence   0.00189 **
    ## real_emotionstability:dissatis_competence    0.23820   
    ## real_openness:dissatis_competence            0.12704   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.32 on 183 degrees of freedom
    ## Multiple R-squared:  0.463,  Adjusted R-squared:  0.372 
    ## F-statistic: 5.089 on 31 and 183 DF,  p-value: 8.987e-13
    ## 
    ## 
    ## Response gap_agreeableness :
    ## 
    ## Call:
    ## lm(formula = gap_agreeableness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence + (dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence) * (real_extraversion + 
    ##     real_agreeableness + real_conscientiousness + real_emotionstability + 
    ##     real_openness), data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("real"), starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9087 -0.6720 -0.1221  0.5313  3.2861 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  2.248741   2.129527   1.056
    ## real_extraversion                            0.016478   0.122711   0.134
    ## real_agreeableness                          -0.206356   0.166260  -1.241
    ## real_conscientiousness                      -0.196353   0.235797  -0.833
    ## real_emotionstability                       -0.183368   0.213941  -0.857
    ## real_openness                                0.041964   0.185605   0.226
    ## c_age                                        0.001807   0.010921   0.165
    ## c_education                                  0.074472   0.063313   1.176
    ## c_income                                    -0.007760   0.040097  -0.194
    ## c_race2                                     -0.008721   0.308181  -0.028
    ## c_race4                                     -0.156459   0.328791  -0.476
    ## c_race6                                     -0.603070   0.869033  -0.694
    ## c_race7                                      0.394261   0.335909   1.174
    ## c_sex2                                      -0.275744   0.167477  -1.646
    ## dissatis_autonomy                           -1.014889   0.834319  -1.216
    ## dissatis_relatedness                         1.889936   0.721585   2.619
    ## dissatis_competence                         -0.583375   0.714449  -0.817
    ## real_extraversion:dissatis_autonomy          0.064169   0.042768   1.500
    ## real_agreeableness:dissatis_autonomy        -0.028618   0.067105  -0.426
    ## real_conscientiousness:dissatis_autonomy     0.161231   0.082619   1.951
    ## real_emotionstability:dissatis_autonomy      0.149884   0.086050   1.742
    ## real_openness:dissatis_autonomy             -0.088892   0.056785  -1.565
    ## real_extraversion:dissatis_relatedness      -0.120399   0.044690  -2.694
    ## real_agreeableness:dissatis_relatedness     -0.012560   0.060201  -0.209
    ## real_conscientiousness:dissatis_relatedness -0.153177   0.065261  -2.347
    ## real_emotionstability:dissatis_relatedness  -0.085167   0.067259  -1.266
    ## real_openness:dissatis_relatedness          -0.074721   0.056541  -1.322
    ## real_extraversion:dissatis_competence        0.043732   0.045149   0.969
    ## real_agreeableness:dissatis_competence      -0.029900   0.065037  -0.460
    ## real_conscientiousness:dissatis_competence  -0.013159   0.061994  -0.212
    ## real_emotionstability:dissatis_competence   -0.019422   0.065358  -0.297
    ## real_openness:dissatis_competence            0.128729   0.057440   2.241
    ##                                             Pr(>|t|)   
    ## (Intercept)                                  0.29237   
    ## real_extraversion                            0.89333   
    ## real_agreeableness                           0.21613   
    ## real_conscientiousness                       0.40609   
    ## real_emotionstability                        0.39251   
    ## real_openness                                0.82138   
    ## c_age                                        0.86874   
    ## c_education                                  0.24102   
    ## c_income                                     0.84675   
    ## c_race2                                      0.97745   
    ## c_race4                                      0.63474   
    ## c_race6                                      0.48859   
    ## c_race7                                      0.24203   
    ## c_sex2                                       0.10139   
    ## dissatis_autonomy                            0.22539   
    ## dissatis_relatedness                         0.00955 **
    ## dissatis_competence                          0.41525   
    ## real_extraversion:dissatis_autonomy          0.13523   
    ## real_agreeableness:dissatis_autonomy         0.67027   
    ## real_conscientiousness:dissatis_autonomy     0.05253 . 
    ## real_emotionstability:dissatis_autonomy      0.08322 . 
    ## real_openness:dissatis_autonomy              0.11921   
    ## real_extraversion:dissatis_relatedness       0.00771 **
    ## real_agreeableness:dissatis_relatedness      0.83497   
    ## real_conscientiousness:dissatis_relatedness  0.01999 * 
    ## real_emotionstability:dissatis_relatedness   0.20704   
    ## real_openness:dissatis_relatedness           0.18797   
    ## real_extraversion:dissatis_competence        0.33402   
    ## real_agreeableness:dissatis_competence       0.64625   
    ## real_conscientiousness:dissatis_competence   0.83214   
    ## real_emotionstability:dissatis_competence    0.76668   
    ## real_openness:dissatis_competence            0.02622 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.048 on 183 degrees of freedom
    ## Multiple R-squared:  0.3319, Adjusted R-squared:  0.2187 
    ## F-statistic: 2.933 on 31 and 183 DF,  p-value: 4.049e-06
    ## 
    ## 
    ## Response gap_conscientiousness :
    ## 
    ## Call:
    ## lm(formula = gap_conscientiousness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence + (dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence) * (real_extraversion + 
    ##     real_agreeableness + real_conscientiousness + real_emotionstability + 
    ##     real_openness), data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("real"), starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.14554 -0.41837 -0.02954  0.46132  2.50329 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  3.606120   1.755564   2.054
    ## real_extraversion                            0.010519   0.101162   0.104
    ## real_agreeableness                          -0.083665   0.137063  -0.610
    ## real_conscientiousness                      -0.626415   0.194389  -3.222
    ## real_emotionstability                       -0.123483   0.176371  -0.700
    ## real_openness                                0.110122   0.153011   0.720
    ## c_age                                        0.003623   0.009003   0.402
    ## c_education                                 -0.020653   0.052195  -0.396
    ## c_income                                    -0.035075   0.033055  -1.061
    ## c_race2                                     -0.173569   0.254062  -0.683
    ## c_race4                                     -0.109558   0.271053  -0.404
    ## c_race6                                      0.581510   0.716423   0.812
    ## c_race7                                     -0.387914   0.276920  -1.401
    ## c_sex2                                       0.020000   0.138067   0.145
    ## dissatis_autonomy                           -1.206071   0.687805  -1.754
    ## dissatis_relatedness                         0.014154   0.594868   0.024
    ## dissatis_competence                          0.927580   0.588986   1.575
    ## real_extraversion:dissatis_autonomy         -0.028213   0.035257  -0.800
    ## real_agreeableness:dissatis_autonomy        -0.021944   0.055321  -0.397
    ## real_conscientiousness:dissatis_autonomy     0.116141   0.068111   1.705
    ## real_emotionstability:dissatis_autonomy      0.111835   0.070939   1.577
    ## real_openness:dissatis_autonomy              0.085025   0.046813   1.816
    ## real_extraversion:dissatis_relatedness       0.015920   0.036842   0.432
    ## real_agreeableness:dissatis_relatedness      0.013235   0.049629   0.267
    ## real_conscientiousness:dissatis_relatedness  0.040118   0.053801   0.746
    ## real_emotionstability:dissatis_relatedness  -0.059387   0.055448  -1.071
    ## real_openness:dissatis_relatedness          -0.038148   0.046612  -0.818
    ## real_extraversion:dissatis_competence        0.008969   0.037221   0.241
    ## real_agreeableness:dissatis_competence       0.022781   0.053616   0.425
    ## real_conscientiousness:dissatis_competence  -0.103234   0.051107  -2.020
    ## real_emotionstability:dissatis_competence   -0.035931   0.053880  -0.667
    ## real_openness:dissatis_competence           -0.068481   0.047353  -1.446
    ##                                             Pr(>|t|)   
    ## (Intercept)                                   0.0414 * 
    ## real_extraversion                             0.9173   
    ## real_agreeableness                            0.5423   
    ## real_conscientiousness                        0.0015 **
    ## real_emotionstability                         0.4847   
    ## real_openness                                 0.4726   
    ## c_age                                         0.6879   
    ## c_education                                   0.6928   
    ## c_income                                      0.2900   
    ## c_race2                                       0.4954   
    ## c_race4                                       0.6865   
    ## c_race6                                       0.4180   
    ## c_race7                                       0.1630   
    ## c_sex2                                        0.8850   
    ## dissatis_autonomy                             0.0812 . 
    ## dissatis_relatedness                          0.9810   
    ## dissatis_competence                           0.1170   
    ## real_extraversion:dissatis_autonomy           0.4246   
    ## real_agreeableness:dissatis_autonomy          0.6921   
    ## real_conscientiousness:dissatis_autonomy      0.0899 . 
    ## real_emotionstability:dissatis_autonomy       0.1166   
    ## real_openness:dissatis_autonomy               0.0710 . 
    ## real_extraversion:dissatis_relatedness        0.6662   
    ## real_agreeableness:dissatis_relatedness       0.7900   
    ## real_conscientiousness:dissatis_relatedness   0.4568   
    ## real_emotionstability:dissatis_relatedness    0.2856   
    ## real_openness:dissatis_relatedness            0.4142   
    ## real_extraversion:dissatis_competence         0.8098   
    ## real_agreeableness:dissatis_competence        0.6714   
    ## real_conscientiousness:dissatis_competence    0.0448 * 
    ## real_emotionstability:dissatis_competence     0.5057   
    ## real_openness:dissatis_competence             0.1498   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8638 on 183 degrees of freedom
    ## Multiple R-squared:  0.408,  Adjusted R-squared:  0.3077 
    ## F-statistic: 4.068 on 31 and 183 DF,  p-value: 1.133e-09
    ## 
    ## 
    ## Response gap_emotionstability :
    ## 
    ## Call:
    ## lm(formula = gap_emotionstability ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence + (dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence) * (real_extraversion + 
    ##     real_agreeableness + real_conscientiousness + real_emotionstability + 
    ##     real_openness), data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("real"), starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5569 -0.6727 -0.1086  0.6496  2.9192 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  4.317355   2.165222   1.994
    ## real_extraversion                            0.021054   0.124768   0.169
    ## real_agreeableness                          -0.118756   0.169046  -0.703
    ## real_conscientiousness                      -0.450872   0.239750  -1.881
    ## real_emotionstability                       -0.435342   0.217527  -2.001
    ## real_openness                               -0.152964   0.188716  -0.811
    ## c_age                                        0.011968   0.011104   1.078
    ## c_education                                 -0.037278   0.064374  -0.579
    ## c_income                                     0.015768   0.040769   0.387
    ## c_race2                                     -0.163522   0.313347  -0.522
    ## c_race4                                     -0.439310   0.334302  -1.314
    ## c_race6                                     -0.562676   0.883599  -0.637
    ## c_race7                                     -0.366406   0.341539  -1.073
    ## c_sex2                                      -0.068638   0.170284  -0.403
    ## dissatis_autonomy                           -0.899456   0.848303  -1.060
    ## dissatis_relatedness                         1.407607   0.733680   1.919
    ## dissatis_competence                         -1.155251   0.726425  -1.590
    ## real_extraversion:dissatis_autonomy          0.076145   0.043485   1.751
    ## real_agreeableness:dissatis_autonomy         0.272000   0.068230   3.987
    ## real_conscientiousness:dissatis_autonomy     0.067766   0.084004   0.807
    ## real_emotionstability:dissatis_autonomy     -0.158380   0.087492  -1.810
    ## real_openness:dissatis_autonomy              0.004262   0.057737   0.074
    ## real_extraversion:dissatis_relatedness      -0.061302   0.045439  -1.349
    ## real_agreeableness:dissatis_relatedness     -0.015800   0.061210  -0.258
    ## real_conscientiousness:dissatis_relatedness -0.178581   0.066355  -2.691
    ## real_emotionstability:dissatis_relatedness   0.010912   0.068387   0.160
    ## real_openness:dissatis_relatedness          -0.044341   0.057489  -0.771
    ## real_extraversion:dissatis_competence        0.014514   0.045906   0.316
    ## real_agreeableness:dissatis_competence      -0.221905   0.066127  -3.356
    ## real_conscientiousness:dissatis_competence   0.167125   0.063033   2.651
    ## real_emotionstability:dissatis_competence    0.126719   0.066453   1.907
    ## real_openness:dissatis_competence            0.094320   0.058403   1.615
    ##                                             Pr(>|t|)    
    ## (Intercept)                                 0.047642 *  
    ## real_extraversion                           0.866181    
    ## real_agreeableness                          0.483255    
    ## real_conscientiousness                      0.061616 .  
    ## real_emotionstability                       0.046835 *  
    ## real_openness                               0.418677    
    ## c_age                                       0.282546    
    ## c_education                                 0.563244    
    ## c_income                                    0.699381    
    ## c_race2                                     0.602401    
    ## c_race4                                     0.190454    
    ## c_race6                                     0.525051    
    ## c_race7                                     0.284770    
    ## c_sex2                                      0.687361    
    ## dissatis_autonomy                           0.290405    
    ## dissatis_relatedness                        0.056597 .  
    ## dissatis_competence                         0.113488    
    ## real_extraversion:dissatis_autonomy         0.081609 .  
    ## real_agreeableness:dissatis_autonomy        9.68e-05 ***
    ## real_conscientiousness:dissatis_autonomy    0.420886    
    ## real_emotionstability:dissatis_autonomy     0.071902 .  
    ## real_openness:dissatis_autonomy             0.941241    
    ## real_extraversion:dissatis_relatedness      0.178970    
    ## real_agreeableness:dissatis_relatedness     0.796600    
    ## real_conscientiousness:dissatis_relatedness 0.007777 ** 
    ## real_emotionstability:dissatis_relatedness  0.873397    
    ## real_openness:dissatis_relatedness          0.441526    
    ## real_extraversion:dissatis_competence       0.752242    
    ## real_agreeableness:dissatis_competence      0.000962 ***
    ## real_conscientiousness:dissatis_competence  0.008720 ** 
    ## real_emotionstability:dissatis_competence   0.058102 .  
    ## real_openness:dissatis_competence           0.108036    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.065 on 183 degrees of freedom
    ## Multiple R-squared:  0.5326, Adjusted R-squared:  0.4535 
    ## F-statistic: 6.728 on 31 and 183 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Response gap_openness :
    ## 
    ## Call:
    ## lm(formula = gap_openness ~ real_extraversion + real_agreeableness + 
    ##     real_conscientiousness + real_emotionstability + real_openness + 
    ##     c_age + c_education + c_income + c_race + c_sex + dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence + (dissatis_autonomy + 
    ##     dissatis_relatedness + dissatis_competence) * (real_extraversion + 
    ##     real_agreeableness + real_conscientiousness + real_emotionstability + 
    ##     real_openness), data = select(df_player_c, starts_with("gap"), 
    ##     starts_with("real"), starts_with("c_"), starts_with("dissatis")))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.14636 -0.47003  0.04566  0.47253  1.99342 
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  2.449247   1.595422   1.535
    ## real_extraversion                            0.125465   0.091934   1.365
    ## real_agreeableness                           0.121339   0.124560   0.974
    ## real_conscientiousness                      -0.011345   0.176657  -0.064
    ## real_emotionstability                       -0.090308   0.160282  -0.563
    ## real_openness                               -0.401414   0.139054  -2.887
    ## c_age                                        0.001124   0.008182   0.137
    ## c_education                                 -0.072272   0.047434  -1.524
    ## c_income                                    -0.052612   0.030040  -1.751
    ## c_race2                                     -0.241231   0.230886  -1.045
    ## c_race4                                      0.058818   0.246327   0.239
    ## c_race6                                     -0.840663   0.651072  -1.291
    ## c_race7                                     -0.250367   0.251660  -0.995
    ## c_sex2                                       0.264602   0.125472   2.109
    ## dissatis_autonomy                            0.057544   0.625064   0.092
    ## dissatis_relatedness                        -0.508335   0.540605  -0.940
    ## dissatis_competence                          0.719454   0.535259   1.344
    ## real_extraversion:dissatis_autonomy         -0.029884   0.032041  -0.933
    ## real_agreeableness:dissatis_autonomy        -0.045107   0.050275  -0.897
    ## real_conscientiousness:dissatis_autonomy     0.090557   0.061898   1.463
    ## real_emotionstability:dissatis_autonomy      0.038937   0.064468   0.604
    ## real_openness:dissatis_autonomy             -0.060351   0.042543  -1.419
    ## real_extraversion:dissatis_relatedness       0.007496   0.033481   0.224
    ## real_agreeableness:dissatis_relatedness     -0.095002   0.045102  -2.106
    ## real_conscientiousness:dissatis_relatedness  0.031749   0.048893   0.649
    ## real_emotionstability:dissatis_relatedness   0.018002   0.050390   0.357
    ## real_openness:dissatis_relatedness           0.080245   0.042360   1.894
    ## real_extraversion:dissatis_competence       -0.018068   0.033826  -0.534
    ## real_agreeableness:dissatis_competence       0.069242   0.048725   1.421
    ## real_conscientiousness:dissatis_competence  -0.117471   0.046445  -2.529
    ## real_emotionstability:dissatis_competence   -0.032903   0.048965  -0.672
    ## real_openness:dissatis_competence           -0.034598   0.043034  -0.804
    ##                                             Pr(>|t|)   
    ## (Intercept)                                  0.12647   
    ## real_extraversion                            0.17401   
    ## real_agreeableness                           0.33127   
    ## real_conscientiousness                       0.94887   
    ## real_emotionstability                        0.57383   
    ## real_openness                                0.00436 **
    ## c_age                                        0.89091   
    ## c_education                                  0.12932   
    ## c_income                                     0.08155 . 
    ## c_race2                                      0.29749   
    ## c_race4                                      0.81154   
    ## c_race6                                      0.19826   
    ## c_race7                                      0.32112   
    ## c_sex2                                       0.03632 * 
    ## dissatis_autonomy                            0.92675   
    ## dissatis_relatedness                         0.34830   
    ## dissatis_competence                          0.18057   
    ## real_extraversion:dissatis_autonomy          0.35221   
    ## real_agreeableness:dissatis_autonomy         0.37078   
    ## real_conscientiousness:dissatis_autonomy     0.14518   
    ## real_emotionstability:dissatis_autonomy      0.54661   
    ## real_openness:dissatis_autonomy              0.15772   
    ## real_extraversion:dissatis_relatedness       0.82310   
    ## real_agreeableness:dissatis_relatedness      0.03653 * 
    ## real_conscientiousness:dissatis_relatedness  0.51693   
    ## real_emotionstability:dissatis_relatedness   0.72131   
    ## real_openness:dissatis_relatedness           0.05975 . 
    ## real_extraversion:dissatis_competence        0.59389   
    ## real_agreeableness:dissatis_competence       0.15700   
    ## real_conscientiousness:dissatis_competence   0.01228 * 
    ## real_emotionstability:dissatis_competence    0.50246   
    ## real_openness:dissatis_competence            0.42245   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.785 on 183 degrees of freedom
    ## Multiple R-squared:  0.5352, Adjusted R-squared:  0.4564 
    ## F-statistic: 6.797 on 31 and 183 DF,  p-value: < 2.2e-16

``` r
#MANOVA
Anova(model_ygap_4)
```

    ## 
    ## Type II MANOVA Tests: Pillai test statistic
    ##                                             Df test stat approx F num Df
    ## real_extraversion                            1   0.39204  23.0854      5
    ## real_agreeableness                           1   0.26941  13.2014      5
    ## real_conscientiousness                       1   0.23605  11.0615      5
    ## real_emotionstability                        1   0.29299  14.8361      5
    ## real_openness                                1   0.46408  31.0013      5
    ## c_age                                        1   0.01030   0.3727      5
    ## c_education                                  1   0.03178   1.1750      5
    ## c_income                                     1   0.03708   1.3786      5
    ## c_race                                       4   0.08574   0.7973     20
    ## c_sex                                        1   0.04086   1.5249      5
    ## dissatis_autonomy                            1   0.03992   1.4886      5
    ## dissatis_relatedness                         1   0.04320   1.6166      5
    ## dissatis_competence                          1   0.00226   0.0810      5
    ## real_extraversion:dissatis_autonomy          1   0.02488   0.9134      5
    ## real_agreeableness:dissatis_autonomy         1   0.13727   5.6961      5
    ## real_conscientiousness:dissatis_autonomy     1   0.05334   2.0173      5
    ## real_emotionstability:dissatis_autonomy      1   0.08947   3.5177      5
    ## real_openness:dissatis_autonomy              1   0.04723   1.7745      5
    ## real_extraversion:dissatis_relatedness       1   0.06040   2.3015      5
    ## real_agreeableness:dissatis_relatedness      1   0.02826   1.0410      5
    ## real_conscientiousness:dissatis_relatedness  1   0.06405   2.4499      5
    ## real_emotionstability:dissatis_relatedness   1   0.06352   2.4285      5
    ## real_openness:dissatis_relatedness           1   0.04650   1.7459      5
    ## real_extraversion:dissatis_competence        1   0.01912   0.6977      5
    ## real_agreeableness:dissatis_competence       1   0.07096   2.7343      5
    ## real_conscientiousness:dissatis_competence   1   0.10170   4.0530      5
    ## real_emotionstability:dissatis_competence    1   0.03144   1.1619      5
    ## real_openness:dissatis_competence            1   0.05934   2.2584      5
    ##                                             den Df    Pr(>F)    
    ## real_extraversion                              179 < 2.2e-16 ***
    ## real_agreeableness                             179 5.984e-11 ***
    ## real_conscientiousness                         179 2.692e-09 ***
    ## real_emotionstability                          179 3.581e-12 ***
    ## real_openness                                  179 < 2.2e-16 ***
    ## c_age                                          179  0.866898    
    ## c_education                                    179  0.323226    
    ## c_income                                       179  0.234370    
    ## c_race                                         728  0.718618    
    ## c_sex                                          179  0.184177    
    ## dissatis_autonomy                              179  0.195662    
    ## dissatis_relatedness                           179  0.157812    
    ## dissatis_competence                            179  0.995104    
    ## real_extraversion:dissatis_autonomy            179  0.473574    
    ## real_agreeableness:dissatis_autonomy           179 6.680e-05 ***
    ## real_conscientiousness:dissatis_autonomy       179  0.078294 .  
    ## real_emotionstability:dissatis_autonomy        179  0.004662 ** 
    ## real_openness:dissatis_autonomy                179  0.120249    
    ## real_extraversion:dissatis_relatedness         179  0.046727 *  
    ## real_agreeableness:dissatis_relatedness        179  0.395093    
    ## real_conscientiousness:dissatis_relatedness    179  0.035524 *  
    ## real_emotionstability:dissatis_relatedness     179  0.036963 *  
    ## real_openness:dissatis_relatedness             179  0.126378    
    ## real_extraversion:dissatis_competence          179  0.625839    
    ## real_agreeableness:dissatis_competence         179  0.020857 *  
    ## real_conscientiousness:dissatis_competence     179  0.001649 ** 
    ## real_emotionstability:dissatis_competence      179  0.329763    
    ## real_openness:dissatis_competence              179  0.050568 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
