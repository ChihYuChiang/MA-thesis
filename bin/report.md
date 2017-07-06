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
    ## -5.0583 -0.6868  0.2737  1.0325  3.3076 
    ## 
    ## Coefficients:
    ##                                                  Estimate Std. Error
    ## (Intercept)                                     39.505774  20.803633
    ## star_user                                        0.026380   0.054343
    ## education                                        0.008020   0.026154
    ## income                                          -0.007082   0.016778
    ## release                                         -0.023976   0.006945
    ## sex2                                            -0.150880   0.071659
    ## real_extraversion                                0.805741   0.932159
    ## real_agreeableness                               1.261912   1.271032
    ## real_conscientiousness                           3.101851   1.512743
    ## real_emotionstability                            1.114126   1.393602
    ## real_openness                                   -1.151492   1.259056
    ## distance_survey_median_1                         5.039080   2.975120
    ## distance_survey_median_2                         2.946458   3.310633
    ## distance_survey_median_3                        -0.032897   2.767971
    ## distance_survey_median_4                         2.606824   2.501109
    ## distance_survey_median_5                         2.711597   3.352934
    ## distance_survey_median_6                        -0.141274   2.239959
    ## distance_survey_median_7                        -0.982010   2.450619
    ## satis_autonomy                                   6.717965   2.178035
    ## satis_relatedness                               -4.823087   1.925432
    ## satis_competence                                -2.880620   2.097255
    ## real_extraversion.distance_survey_median_1      -0.154998   0.172777
    ## real_agreeableness.distance_survey_median_1     -0.524500   0.236785
    ## real_conscientiousness.distance_survey_median_1 -0.495529   0.276966
    ## real_emotionstability.distance_survey_median_1  -0.413795   0.249749
    ## real_openness.distance_survey_median_1          -0.175181   0.234118
    ## real_extraversion.distance_survey_median_2      -0.230410   0.197969
    ## real_agreeableness.distance_survey_median_2      0.080856   0.267692
    ## real_conscientiousness.distance_survey_median_2 -0.686491   0.320725
    ## real_emotionstability.distance_survey_median_2  -0.486532   0.300438
    ## real_openness.distance_survey_median_2           0.248208   0.267558
    ## real_extraversion.distance_survey_median_3       0.067592   0.170138
    ## real_agreeableness.distance_survey_median_3     -0.120688   0.229795
    ## real_conscientiousness.distance_survey_median_3 -0.536834   0.271527
    ## real_emotionstability.distance_survey_median_3  -0.117834   0.253783
    ## real_openness.distance_survey_median_3           0.343994   0.225269
    ## real_extraversion.distance_survey_median_4      -0.174750   0.156164
    ## real_agreeableness.distance_survey_median_4     -0.277825   0.213233
    ## real_conscientiousness.distance_survey_median_4 -0.421161   0.239261
    ## real_emotionstability.distance_survey_median_4  -0.020653   0.229539
    ## real_openness.distance_survey_median_4           0.333880   0.200891
    ## real_extraversion.distance_survey_median_5      -0.070634   0.206369
    ## real_agreeableness.distance_survey_median_5     -0.398053   0.277705
    ## real_conscientiousness.distance_survey_median_5 -0.752626   0.326862
    ## real_emotionstability.distance_survey_median_5  -0.272401   0.300825
    ## real_openness.distance_survey_median_5           0.056634   0.274858
    ## real_extraversion.distance_survey_median_6       0.017837   0.137123
    ## real_agreeableness.distance_survey_median_6     -0.078816   0.184053
    ## real_conscientiousness.distance_survey_median_6 -0.159223   0.216856
    ## real_emotionstability.distance_survey_median_6   0.306463   0.208066
    ## real_openness.distance_survey_median_6           0.447697   0.179708
    ## real_extraversion.distance_survey_median_7      -0.229021   0.141755
    ## real_agreeableness.distance_survey_median_7      0.053781   0.196881
    ## real_conscientiousness.distance_survey_median_7 -0.019522   0.239300
    ## real_emotionstability.distance_survey_median_7  -0.092932   0.216045
    ## real_openness.distance_survey_median_7          -0.021028   0.192646
    ## distance_survey_median_1.satis_autonomy         -0.890339   0.389296
    ## distance_survey_median_2.satis_autonomy         -1.390498   0.460050
    ## distance_survey_median_3.satis_autonomy         -1.066138   0.375320
    ## distance_survey_median_4.satis_autonomy         -1.072098   0.360957
    ## distance_survey_median_5.satis_autonomy         -1.225015   0.472577
    ## distance_survey_median_6.satis_autonomy         -0.371007   0.292319
    ## distance_survey_median_7.satis_autonomy         -0.568311   0.322048
    ## distance_survey_median_1.satis_relatedness       0.946892   0.351251
    ## distance_survey_median_2.satis_relatedness       0.996882   0.407072
    ## distance_survey_median_3.satis_relatedness       0.826226   0.354506
    ## distance_survey_median_4.satis_relatedness       0.610028   0.305656
    ## distance_survey_median_5.satis_relatedness       0.973976   0.414943
    ## distance_survey_median_6.satis_relatedness      -0.075513   0.279034
    ## distance_survey_median_7.satis_relatedness       0.553008   0.293621
    ## distance_survey_median_1.satis_competence        0.348739   0.376592
    ## distance_survey_median_2.satis_competence        0.625805   0.445943
    ## distance_survey_median_3.satis_competence        0.436924   0.377869
    ## distance_survey_median_4.satis_competence        0.324006   0.342654
    ## distance_survey_median_5.satis_competence        0.746395   0.450820
    ## distance_survey_median_6.satis_competence        0.045324   0.293476
    ## distance_survey_median_7.satis_competence        0.397762   0.318363
    ##                                                 t value Pr(>|t|)    
    ## (Intercept)                                       1.899 0.057706 .  
    ## star_user                                         0.485 0.627420    
    ## education                                         0.307 0.759132    
    ## income                                           -0.422 0.672971    
    ## release                                          -3.452 0.000568 ***
    ## sex2                                             -2.106 0.035367 *  
    ## real_extraversion                                 0.864 0.387479    
    ## real_agreeableness                                0.993 0.320912    
    ## real_conscientiousness                            2.050 0.040444 *  
    ## real_emotionstability                             0.799 0.424117    
    ## real_openness                                    -0.915 0.360526    
    ## distance_survey_median_1                          1.694 0.090466 .  
    ## distance_survey_median_2                          0.890 0.373571    
    ## distance_survey_median_3                         -0.012 0.990518    
    ## distance_survey_median_4                          1.042 0.297410    
    ## distance_survey_median_5                          0.809 0.418767    
    ## distance_survey_median_6                         -0.063 0.949717    
    ## distance_survey_median_7                         -0.401 0.688668    
    ## satis_autonomy                                    3.084 0.002067 ** 
    ## satis_relatedness                                -2.505 0.012324 *  
    ## satis_competence                                 -1.374 0.169740    
    ## real_extraversion.distance_survey_median_1       -0.897 0.369773    
    ## real_agreeableness.distance_survey_median_1      -2.215 0.026862 *  
    ## real_conscientiousness.distance_survey_median_1  -1.789 0.073740 .  
    ## real_emotionstability.distance_survey_median_1   -1.657 0.097703 .  
    ## real_openness.distance_survey_median_1           -0.748 0.454388    
    ## real_extraversion.distance_survey_median_2       -1.164 0.244611    
    ## real_agreeableness.distance_survey_median_2       0.302 0.762646    
    ## real_conscientiousness.distance_survey_median_2  -2.140 0.032437 *  
    ## real_emotionstability.distance_survey_median_2   -1.619 0.105513    
    ## real_openness.distance_survey_median_2            0.928 0.353682    
    ## real_extraversion.distance_survey_median_3        0.397 0.691204    
    ## real_agreeableness.distance_survey_median_3      -0.525 0.599500    
    ## real_conscientiousness.distance_survey_median_3  -1.977 0.048164 *  
    ## real_emotionstability.distance_survey_median_3   -0.464 0.642476    
    ## real_openness.distance_survey_median_3            1.527 0.126905    
    ## real_extraversion.distance_survey_median_4       -1.119 0.263263    
    ## real_agreeableness.distance_survey_median_4      -1.303 0.192748    
    ## real_conscientiousness.distance_survey_median_4  -1.760 0.078513 .  
    ## real_emotionstability.distance_survey_median_4   -0.090 0.928314    
    ## real_openness.distance_survey_median_4            1.662 0.096666 .  
    ## real_extraversion.distance_survey_median_5       -0.342 0.732182    
    ## real_agreeableness.distance_survey_median_5      -1.433 0.151905    
    ## real_conscientiousness.distance_survey_median_5  -2.303 0.021402 *  
    ## real_emotionstability.distance_survey_median_5   -0.906 0.365299    
    ## real_openness.distance_survey_median_5            0.206 0.836772    
    ## real_extraversion.distance_survey_median_6        0.130 0.896516    
    ## real_agreeableness.distance_survey_median_6      -0.428 0.668533    
    ## real_conscientiousness.distance_survey_median_6  -0.734 0.462890    
    ## real_emotionstability.distance_survey_median_6    1.473 0.140928    
    ## real_openness.distance_survey_median_6            2.491 0.012808 *  
    ## real_extraversion.distance_survey_median_7       -1.616 0.106331    
    ## real_agreeableness.distance_survey_median_7       0.273 0.784755    
    ## real_conscientiousness.distance_survey_median_7  -0.082 0.934988    
    ## real_emotionstability.distance_survey_median_7   -0.430 0.667130    
    ## real_openness.distance_survey_median_7           -0.109 0.913090    
    ## distance_survey_median_1.satis_autonomy          -2.287 0.022294 *  
    ## distance_survey_median_2.satis_autonomy          -3.022 0.002538 ** 
    ## distance_survey_median_3.satis_autonomy          -2.841 0.004547 ** 
    ## distance_survey_median_4.satis_autonomy          -2.970 0.003011 ** 
    ## distance_survey_median_5.satis_autonomy          -2.592 0.009604 ** 
    ## distance_survey_median_6.satis_autonomy          -1.269 0.204518    
    ## distance_survey_median_7.satis_autonomy          -1.765 0.077766 .  
    ## distance_survey_median_1.satis_relatedness        2.696 0.007080 ** 
    ## distance_survey_median_2.satis_relatedness        2.449 0.014412 *  
    ## distance_survey_median_3.satis_relatedness        2.331 0.019868 *  
    ## distance_survey_median_4.satis_relatedness        1.996 0.046087 *  
    ## distance_survey_median_5.satis_relatedness        2.347 0.019007 *  
    ## distance_survey_median_6.satis_relatedness       -0.271 0.786708    
    ## distance_survey_median_7.satis_relatedness        1.883 0.059786 .  
    ## distance_survey_median_1.satis_competence         0.926 0.354533    
    ## distance_survey_median_2.satis_competence         1.403 0.160669    
    ## distance_survey_median_3.satis_competence         1.156 0.247699    
    ## distance_survey_median_4.satis_competence         0.946 0.344476    
    ## distance_survey_median_5.satis_competence         1.656 0.097947 .  
    ## distance_survey_median_6.satis_competence         0.154 0.877279    
    ## distance_survey_median_7.satis_competence         1.249 0.211661    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.441 on 2061 degrees of freedom
    ## Multiple R-squared:  0.1116, Adjusted R-squared:  0.07888 
    ## F-statistic: 3.408 on 76 and 2061 DF,  p-value: < 2.2e-16
