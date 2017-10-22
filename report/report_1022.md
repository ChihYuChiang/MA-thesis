MAPSS Thesis III
================
Chih-Yu Chiang
October 22, 2017

-   [Setup](#setup)
-   [Variable](#variable)
-   [Gap difference between gender](#gap-difference-between-gender)
-   [Gap difference between the players choose MMO/MOBA and do not choose MMO/MOBA](#gap-difference-between-the-players-choose-mmomoba-and-do-not-choose-mmomoba)
-   [Correlation between general liking of video games and gap](#correlation-between-general-liking-of-video-games-and-gap)
-   [Correlation matrix between individual preference and tste vars](#correlation-matrix-between-individual-preference-and-tste-vars)

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
colnames(core_tGenre)[2:length(colnames(core_tGenre))] <- #Give genre columns identification
  unlist(lapply(X=colnames(core_tGenre)[2:length(colnames(core_tGenre))], function(X) {paste("tg_", X, sep="")}))

#Player-related survey data
survey <- read_csv("../data/survey.csv", col_names=TRUE) %>%
  mutate(race = factor(race),
         sex = factor(sex),
         core_id = factor(core_id)) %>%
  select(-id)

#Satisfaction raw data
satisRaw <- read_csv("../data/raw_survey/raw_satisfaction.csv", col_names=TRUE)


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
<td>SDT combined (previous two) satisfaction in real life</td>
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
  

  #--Mean-center predictor variables (if haven't been produced)
  if(!ncol(select(df, matches("_ct$")))) {
    df <<- mutate_at(df, vars(starts_with("tste"),
                              starts_with("game"),
                              starts_with("real"),
                              starts_with("satis"),
                              starts_with("dissatis"),
                              starts_with("combined")), funs(ct = . - mean(.)))
  }


  #--Compute personalty gap
  df <<- mutate(df,
                gap_extraversion = game_extraversion - real_extraversion,
                gap_agreeableness = game_agreeableness - real_agreeableness,
                gap_conscientiousness = game_conscientiousness - real_conscientiousness,
                gap_emotionstability = game_emotionstability - real_emotionstability,
                gap_openness = game_openness - real_openness,
                gap_sum = gap_extraversion + gap_agreeableness + gap_conscientiousness + gap_emotionstability + gap_openness,
                gap_sum_abs = abs(gap_extraversion) + abs(gap_agreeableness) + abs(gap_conscientiousness) + abs(gap_emotionstability) + abs(gap_openness),
                game_sum = game_extraversion + game_agreeableness + game_conscientiousness + game_emotionstability + game_openness,
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

#Create dfs for each player
dfs_playerG <- select(df_player, respondent)
dfs_playerG$df <- map(dfs_playerG$respondent, ~ filter(df, respondent == .x))
```

Gap difference between gender
-----------------------------

-male: sex == 1, x -female: sex == 2, y

``` r
t.test(filter(df_player, sex == 1)$gap_agreeableness, filter(df_player, sex == 2)$gap_agreeableness, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df_player, sex == 1)$gap_agreeableness and filter(df_player, sex == 2)$gap_agreeableness
    ## t = 0.46407, df = 173.47, p-value = 0.6432
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2419557  0.3907112
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.4647887 0.3904110

``` r
t.test(filter(df_player, sex == 1)$gap_conscientiousness, filter(df_player, sex == 2)$gap_conscientiousness, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df_player, sex == 1)$gap_conscientiousness and filter(df_player, sex == 2)$gap_conscientiousness
    ## t = -0.20385, df = 155.51, p-value = 0.8387
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3186608  0.2590429
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.1619718 0.1917808

``` r
t.test(filter(df_player, sex == 1)$gap_emotionstability, filter(df_player, sex == 2)$gap_emotionstability, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df_player, sex == 1)$gap_emotionstability and filter(df_player, sex == 2)$gap_emotionstability
    ## t = 2.2913, df = 145.78, p-value = 0.02338
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.06467423 0.87628660
    ## sample estimates:
    ##   mean of x   mean of y 
    ## -0.07746479 -0.54794521

``` r
t.test(filter(df_player, sex == 1)$gap_extraversion, filter(df_player, sex == 2)$gap_extraversion, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df_player, sex == 1)$gap_extraversion and filter(df_player, sex == 2)$gap_extraversion
    ## t = -0.72614, df = 141.5, p-value = 0.469
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.6557197  0.3034141
    ## sample estimates:
    ## mean of x mean of y 
    ##  1.070423  1.246575

``` r
t.test(filter(df_player, sex == 1)$gap_openness, filter(df_player, sex == 2)$gap_openness, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df_player, sex == 1)$gap_openness and filter(df_player, sex == 2)$gap_openness
    ## t = -1.0836, df = 174.53, p-value = 0.28
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4382111  0.1275802
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.3309859 0.4863014

``` r
t.test(filter(df_player, sex == 1)$gap_sum, filter(df_player, sex == 2)$gap_sum, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df_player, sex == 1)$gap_sum and filter(df_player, sex == 2)$gap_sum
    ## t = 0.46503, df = 161.56, p-value = 0.6425
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.5960005  0.9631624
    ## sample estimates:
    ## mean of x mean of y 
    ##  1.950704  1.767123

``` r
t.test(filter(df_player, sex == 1)$gap_sum_abs, filter(df_player, sex == 2)$gap_sum_abs, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df_player, sex == 1)$gap_sum_abs and filter(df_player, sex == 2)$gap_sum_abs
    ## t = -0.16469, df = 155.06, p-value = 0.8694
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.0204035  0.8633516
    ## sample estimates:
    ## mean of x mean of y 
    ##  4.633803  4.712329

Gap difference between the players choose MMO/MOBA and do not choose MMO/MOBA
-----------------------------------------------------------------------------

-choose MMO/MOBA: df\_1, x -do not choose MMO/MOBA: df\_2, y

``` r
df_1 <- filter(df, (tg_MOBA == 1 | tg_MMO == 1))
df_player_1 <- distinct(df_1, respondent, .keep_all=TRUE)
df_2 <- filter(df, (tg_MOBA == 0 & tg_MMO == 0))
df_player_2 <- distinct(df_2, respondent, .keep_all=TRUE)

t.test(df_1$gap_agreeableness, df_2$gap_agreeableness, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df_1$gap_agreeableness and df_2$gap_agreeableness
    ## t = 0.07608, df = 111.5, p-value = 0.9395
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2078022  0.2243969
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.4450000 0.4367026

``` r
t.test(df_1$gap_conscientiousness, df_2$gap_conscientiousness, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df_1$gap_conscientiousness and df_2$gap_conscientiousness
    ## t = -1.512, df = 118.47, p-value = 0.1332
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.27507750  0.03688319
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.0600000 0.1790972

``` r
t.test(df_1$gap_emotionstability, df_2$gap_emotionstability, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df_1$gap_emotionstability and df_2$gap_emotionstability
    ## t = -0.073369, df = 115.09, p-value = 0.9416
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2446699  0.2271919
    ## sample estimates:
    ## mean of x mean of y 
    ## -0.245000 -0.236261

``` r
t.test(df_1$gap_extraversion, df_2$gap_extraversion, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df_1$gap_extraversion and df_2$gap_extraversion
    ## t = -0.95618, df = 110.38, p-value = 0.3411
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4700131  0.1640661
    ## sample estimates:
    ## mean of x mean of y 
    ##  0.980000  1.132974

``` r
t.test(df_1$gap_openness, df_2$gap_openness, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df_1$gap_openness and df_2$gap_openness
    ## t = -0.54811, df = 115.72, p-value = 0.5847
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2186402  0.1238610
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.3400000 0.3873896

``` r
t.test(df_1$gap_sum, df_2$gap_sum, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df_1$gap_sum and df_2$gap_sum
    ## t = -1.3238, df = 114.12, p-value = 0.1882
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.7985977  0.1587940
    ## sample estimates:
    ## mean of x mean of y 
    ##  1.580000  1.899902

``` r
t.test(df_1$gap_sum_abs, df_2$gap_sum_abs, paired=FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df_1$gap_sum_abs and df_2$gap_sum_abs
    ## t = -2.6488, df = 115.77, p-value = 0.009205
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.2653794 -0.1826284
    ## sample estimates:
    ## mean of x mean of y 
    ##  3.960000  4.684004

Correlation between general liking of video games and gap
---------------------------------------------------------

How much do you like this game?

``` r
df_playerG <- group_by(df, respondent) %>%
  summarize(max = max(preference_1), avg = mean(preference_1))
cor(df_playerG$max, select(df_player, matches("^gap.*$")))
```

    ##      gap_extraversion gap_agreeableness gap_conscientiousness gap_emotionstability gap_openness     gap_sum
    ## [1,]     -0.008223057       -0.01138023           -0.07567861         -0.009443856 -0.005442112 -0.04402099
    ##       gap_sum_abs
    ## [1,] -0.009613859

``` r
cor(df_playerG$avg, select(df_player, matches("^gap.*$")))
```

    ##      gap_extraversion gap_agreeableness gap_conscientiousness gap_emotionstability gap_openness   gap_sum  gap_sum_abs
    ## [1,]       0.04877671        0.06701409           -0.02177844           -0.0332849  -0.03526536 0.0184909 -0.004606698

Correlation matrix between individual preference and tste vars
--------------------------------------------------------------

How much does it fit your taste?

-   For each individual, acquire correlation between preference and each tste var
-   20 tste vars
-   215 individuals
-   10 observations per individual
-   The individuals are ordered by cluster based on the correlation

``` r
#--Acquire the correlations
df_cor <- data.frame()
for (i in seq(1:nrow(dfs_playerG))) {
  mx_temp <- cor(dfs_playerG$df[[i]]$preference, select(dfs_playerG$df[[i]], starts_with("tste_20"))[, c(1:20)]) %>%
    data.frame()
  df_cor <- bind_rows(df_cor, mx_temp)
}
df_cor <- na.omit(df_cor)


#--Decide number of clusters
betweenOnTotals <- array()
for (i in c(1:50)) {
  model_km <- kmeans(df_cor, centers=i, nstart=100, iter.max=1000)
  betweenOnTotals[i] <- model_km$betweenss / model_km$totss
}


#--k-mean for better presenting the plot
centerN <- 5
model_km <- kmeans(df_cor, centers=centerN, nstart=100, iter.max=1000)
df_cor$cluster <- model_km$cluster
df_cor_arranged <- arrange(df_cor, cluster)


#--Main plot
#Expand the combination between number of individuals and number of vars and lookup the corresponding z value
df_cor_expand <- expand.grid(y=c(1:nrow(df_cor_arranged)), x=c(1:(ncol(df_cor_arranged) - 1))) %>%
  rowwise() %>%
  mutate(z=df_cor_arranged[y, x]) %>%
  ungroup()

ggplot(data=df_cor_expand, aes(x=x, y=y, fill=z)) +
  geom_tile() + 
  geom_hline(data=data.frame(y=cumsum(model_km$size)[-centerN]), aes(yintercept=y), color="orange", size=0.5, linetype=2, alpha=1) +
  labs(x="Tste var (from triplet embedding)", y="Individual", fill="Corelation",
       title="Correlation between individual preference and tste vars",
       subtitle="(The individuals are ordered by 5 clusters based on the correlations)") +
  theme_minimal()
```

![](report_1022_files/figure-markdown_github-ascii_identifiers/corMatrix-1.png)

``` r
#--Supplement plot
ggplot(data.frame(x=c(1:length(betweenOnTotals)), y=betweenOnTotals), aes(x=x, y=y)) +
  geom_line() +
  labs(x="Number of clusters", y="Variation explained between cluster",
       title="Variation explained with different numbers of clusters") +
  scale_y_continuous(labels=scales::percent)
```

![](report_1022_files/figure-markdown_github-ascii_identifiers/corMatrix-2.png)
