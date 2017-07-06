MAPSS Thesis II - Game Characteristics and Player Personality
================
Chih-Yu Chiang
July 5, 2017

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
library(corrgram)
library(modelr)
library(glmnet)
library(randomForest)
library(e1071)
set.seed(1)


#--Read in data
core_cluster <- read_csv("../data/core_cluster.csv", col_names=TRUE) %>%
  mutate(group_survey = factor(group_survey),
         group_review = factor(group_review),
         core_id = factor(core_id)) %>%
  select(-X1)
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
df <- left_join(survey, core_cluster, by=c("core_id"), copy=FALSE)
```

![Analysis Framework](img/framework.png)

Variable
--------

Compute and select variables to be used in models.

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
<td>categorical 1-7</td>
</tr>
<tr class="even">
<td><code>group_review</code></td>
<td>group identity from review</td>
<td>categorical 1-7</td>
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

-   Final response variable utilizes only `preference_3`.

``` r
#--Create response variable
df <- df %>%
  rowwise() %>% 
  mutate(preference = mean(c(preference_3)))


#--Compute personalty gap
df <- mutate(df,
             gap_extraversion = game_extraversion - real_extraversion,
             gap_agreeableness = game_agreeableness - real_agreeableness,
             gap_conscientiousness = game_conscientiousness - real_conscientiousness,
             gap_emotionstability = game_emotionstability - real_emotionstability,
             gap_openness = game_openness - real_openness)


#--Select variables to be included in regression (model formation)
#predictor variables
predictors <- paste(read.csv("../data/predictors.csv", header=FALSE)[,1], collapse="+")

#df with only predictor variables
df_x <- model.matrix(as.formula(paste("preference ~ ", predictors, sep="")),
                     data=df) %>% #Define model formation and create dummies
  .[, -1] #Remove redundant interacept column

#df also with outcome variables
df_yx <- bind_cols(select(df, preference), data.frame(df_x))
```

Models
------

Models applying the variables selected in the previous section.

-   Predictor variables being used are edited through 'predictors.csv'

``` r
#--Regression_simple linear
model_lm <- lm(preference ~ ., data=df_yx)
summary(model_lm)
```

    ## 
    ## Call:
    ## lm(formula = preference ~ ., data = df_yx)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0397 -0.6836  0.2651  1.0141  3.1886 
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error
    ## (Intercept)                                     40.351083  20.817068
    ## star_user                                        0.026853   0.054387
    ## education                                        0.005403   0.026146
    ## income                                          -0.006006   0.016784
    ## release                                         -0.024263   0.006950
    ## real_extraversion                                0.814264   0.932926
    ## real_agreeableness                               1.239082   1.272043
    ## real_conscientiousness                           2.982567   1.512939
    ## real_emotionstability                            1.057145   1.394499
    ## real_openness                                   -1.154781   1.260103
    ## distance_survey_median_1                         4.977043   2.977450
    ## distance_survey_median_2                         2.945795   3.313388
    ## distance_survey_median_3                        -0.106674   2.770052
    ## distance_survey_median_4                         2.480897   2.502475
    ## distance_survey_median_5                         2.724114   3.355719
    ## distance_survey_median_6                        -0.201554   2.241640
    ## distance_survey_median_7                        -0.908753   2.452411
    ## satis_autonomy                                   6.721478   2.179847
    ## satis_relatedness                               -4.811202   1.927026
    ## satis_competence                                -2.785237   2.098511
    ## real_extraversion.distance_survey_median_1      -0.161793   0.172891
    ## real_agreeableness.distance_survey_median_1     -0.516165   0.236948
    ## real_conscientiousness.distance_survey_median_1 -0.475268   0.277029
    ## real_emotionstability.distance_survey_median_1  -0.411359   0.249954
    ## real_openness.distance_survey_median_1          -0.178889   0.234306
    ## real_extraversion.distance_survey_median_2      -0.230142   0.198134
    ## real_agreeableness.distance_survey_median_2      0.083174   0.267912
    ## real_conscientiousness.distance_survey_median_2 -0.671752   0.320915
    ## real_emotionstability.distance_survey_median_2  -0.475499   0.300642
    ## real_openness.distance_survey_median_2           0.251065   0.267777
    ## real_extraversion.distance_survey_median_3       0.068474   0.170279
    ## real_agreeableness.distance_survey_median_3     -0.110227   0.229932
    ## real_conscientiousness.distance_survey_median_3 -0.513815   0.271533
    ## real_emotionstability.distance_survey_median_3  -0.106607   0.253938
    ## real_openness.distance_survey_median_3           0.345132   0.225456
    ## real_extraversion.distance_survey_median_4      -0.175621   0.156294
    ## real_agreeableness.distance_survey_median_4     -0.278401   0.213410
    ## real_conscientiousness.distance_survey_median_4 -0.397264   0.239191
    ## real_emotionstability.distance_survey_median_4  -0.010221   0.229676
    ## real_openness.distance_survey_median_4           0.334871   0.201058
    ## real_extraversion.distance_survey_median_5      -0.074011   0.206535
    ## real_agreeableness.distance_survey_median_5     -0.384690   0.277864
    ## real_conscientiousness.distance_survey_median_5 -0.741117   0.327088
    ## real_emotionstability.distance_survey_median_5  -0.270543   0.301074
    ## real_openness.distance_survey_median_5           0.054047   0.275084
    ## real_extraversion.distance_survey_median_6       0.028497   0.137143
    ## real_agreeableness.distance_survey_median_6     -0.078690   0.184206
    ## real_conscientiousness.distance_survey_median_6 -0.140251   0.216849
    ## real_emotionstability.distance_survey_median_6   0.313551   0.208212
    ## real_openness.distance_survey_median_6           0.442823   0.179843
    ## real_extraversion.distance_survey_median_7      -0.235843   0.141835
    ## real_agreeableness.distance_survey_median_7      0.054702   0.197045
    ## real_conscientiousness.distance_survey_median_7 -0.017221   0.239497
    ## real_emotionstability.distance_survey_median_7  -0.096744   0.216217
    ## real_openness.distance_survey_median_7          -0.016483   0.192794
    ## distance_survey_median_1.satis_autonomy         -0.887364   0.389618
    ## distance_survey_median_2.satis_autonomy         -1.386969   0.460430
    ## distance_survey_median_3.satis_autonomy         -1.061902   0.375627
    ## distance_survey_median_4.satis_autonomy         -1.088082   0.361178
    ## distance_survey_median_5.satis_autonomy         -1.229970   0.472965
    ## distance_survey_median_6.satis_autonomy         -0.373551   0.292560
    ## distance_survey_median_7.satis_autonomy         -0.556664   0.322268
    ## distance_survey_median_1.satis_relatedness       0.943819   0.351541
    ## distance_survey_median_2.satis_relatedness       0.990044   0.407398
    ## distance_survey_median_3.satis_relatedness       0.809158   0.354708
    ## distance_survey_median_4.satis_relatedness       0.635332   0.305673
    ## distance_survey_median_5.satis_relatedness       0.971645   0.415287
    ## distance_survey_median_6.satis_relatedness      -0.067544   0.279241
    ## distance_survey_median_7.satis_relatedness       0.537966   0.293779
    ## distance_survey_median_1.satis_competence        0.343273   0.376896
    ## distance_survey_median_2.satis_competence        0.604373   0.446198
    ## distance_survey_median_3.satis_competence        0.426429   0.378150
    ## distance_survey_median_4.satis_competence        0.305683   0.342829
    ## distance_survey_median_5.satis_competence        0.733143   0.451151
    ## distance_survey_median_6.satis_competence        0.022521   0.293520
    ## distance_survey_median_7.satis_competence        0.386659   0.318584
    ##                                                 t value Pr(>|t|)    
    ## (Intercept)                                       1.938 0.052715 .  
    ## star_user                                         0.494 0.621540    
    ## education                                         0.207 0.836315    
    ## income                                           -0.358 0.720486    
    ## release                                          -3.491 0.000491 ***
    ## real_extraversion                                 0.873 0.382870    
    ## real_agreeableness                                0.974 0.330127    
    ## real_conscientiousness                            1.971 0.048815 *  
    ## real_emotionstability                             0.758 0.448488    
    ## real_openness                                    -0.916 0.359555    
    ## distance_survey_median_1                          1.672 0.094759 .  
    ## distance_survey_median_2                          0.889 0.374075    
    ## distance_survey_median_3                         -0.039 0.969285    
    ## distance_survey_median_4                          0.991 0.321618    
    ## distance_survey_median_5                          0.812 0.417010    
    ## distance_survey_median_6                         -0.090 0.928364    
    ## distance_survey_median_7                         -0.371 0.711007    
    ## satis_autonomy                                    3.083 0.002073 ** 
    ## satis_relatedness                                -2.497 0.012613 *  
    ## satis_competence                                 -1.327 0.184575    
    ## real_extraversion.distance_survey_median_1       -0.936 0.349479    
    ## real_agreeableness.distance_survey_median_1      -2.178 0.029490 *  
    ## real_conscientiousness.distance_survey_median_1  -1.716 0.086387 .  
    ## real_emotionstability.distance_survey_median_1   -1.646 0.099970 .  
    ## real_openness.distance_survey_median_1           -0.763 0.445261    
    ## real_extraversion.distance_survey_median_2       -1.162 0.245553    
    ## real_agreeableness.distance_survey_median_2       0.310 0.756248    
    ## real_conscientiousness.distance_survey_median_2  -2.093 0.036450 *  
    ## real_emotionstability.distance_survey_median_2   -1.582 0.113892    
    ## real_openness.distance_survey_median_2            0.938 0.348564    
    ## real_extraversion.distance_survey_median_3        0.402 0.687632    
    ## real_agreeableness.distance_survey_median_3      -0.479 0.631712    
    ## real_conscientiousness.distance_survey_median_3  -1.892 0.058594 .  
    ## real_emotionstability.distance_survey_median_3   -0.420 0.674666    
    ## real_openness.distance_survey_median_3            1.531 0.125967    
    ## real_extraversion.distance_survey_median_4       -1.124 0.261287    
    ## real_agreeableness.distance_survey_median_4      -1.305 0.192196    
    ## real_conscientiousness.distance_survey_median_4  -1.661 0.096892 .  
    ## real_emotionstability.distance_survey_median_4   -0.045 0.964509    
    ## real_openness.distance_survey_median_4            1.666 0.095957 .  
    ## real_extraversion.distance_survey_median_5       -0.358 0.720121    
    ## real_agreeableness.distance_survey_median_5      -1.384 0.166369    
    ## real_conscientiousness.distance_survey_median_5  -2.266 0.023567 *  
    ## real_emotionstability.distance_survey_median_5   -0.899 0.368975    
    ## real_openness.distance_survey_median_5            0.196 0.844257    
    ## real_extraversion.distance_survey_median_6        0.208 0.835413    
    ## real_agreeableness.distance_survey_median_6      -0.427 0.669289    
    ## real_conscientiousness.distance_survey_median_6  -0.647 0.517855    
    ## real_emotionstability.distance_survey_median_6    1.506 0.132241    
    ## real_openness.distance_survey_median_6            2.462 0.013887 *  
    ## real_extraversion.distance_survey_median_7       -1.663 0.096506 .  
    ## real_agreeableness.distance_survey_median_7       0.278 0.781339    
    ## real_conscientiousness.distance_survey_median_7  -0.072 0.942684    
    ## real_emotionstability.distance_survey_median_7   -0.447 0.654605    
    ## real_openness.distance_survey_median_7           -0.085 0.931877    
    ## distance_survey_median_1.satis_autonomy          -2.278 0.022857 *  
    ## distance_survey_median_2.satis_autonomy          -3.012 0.002624 ** 
    ## distance_survey_median_3.satis_autonomy          -2.827 0.004744 ** 
    ## distance_survey_median_4.satis_autonomy          -3.013 0.002622 ** 
    ## distance_survey_median_5.satis_autonomy          -2.601 0.009374 ** 
    ## distance_survey_median_6.satis_autonomy          -1.277 0.201804    
    ## distance_survey_median_7.satis_autonomy          -1.727 0.084258 .  
    ## distance_survey_median_1.satis_relatedness        2.685 0.007315 ** 
    ## distance_survey_median_2.satis_relatedness        2.430 0.015177 *  
    ## distance_survey_median_3.satis_relatedness        2.281 0.022639 *  
    ## distance_survey_median_4.satis_relatedness        2.078 0.037790 *  
    ## distance_survey_median_5.satis_relatedness        2.340 0.019394 *  
    ## distance_survey_median_6.satis_relatedness       -0.242 0.808893    
    ## distance_survey_median_7.satis_relatedness        1.831 0.067216 .  
    ## distance_survey_median_1.satis_competence         0.911 0.362513    
    ## distance_survey_median_2.satis_competence         1.354 0.175727    
    ## distance_survey_median_3.satis_competence         1.128 0.259591    
    ## distance_survey_median_4.satis_competence         0.892 0.372685    
    ## distance_survey_median_5.satis_competence         1.625 0.104305    
    ## distance_survey_median_6.satis_competence         0.077 0.938848    
    ## distance_survey_median_7.satis_competence         1.214 0.225009    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.443 on 2062 degrees of freedom
    ## Multiple R-squared:  0.1097, Adjusted R-squared:  0.07734 
    ## F-statistic: 3.389 on 75 and 2062 DF,  p-value: < 2.2e-16
