dfs_playerG <- select(df_player, respondent)
dfs_playerG$df <- map(dfs_playerG$respondent, ~ filter(df, respondent == .x))





df_cor <- data.frame()
for(i in seq(1:nrow(dfs_playerG))) {
  mx_temp <- cor(dfs_playerG$df[[i]]$preference, select(dfs_playerG$df[[i]], starts_with("tste_20"))[, c(1:20)]) %>%
    data.frame()
  df_cor <- bind_rows(df_cor, mx_temp)
}
df_cor <- na.omit(df_cor)

betweenOnTotals <- array()
for(i in c(1:20)) {
  model_km <- kmeans(df_cor, centers=i, nstart=100, iter.max=1000)
  betweenOnTotals[i] <- model_km$betweenss / model_km$totss
}

BOTs <- array()
for(centerN in c(1:20)) {
  betweenOnTotals <- array()
  for(i in c(1:100)) {
    df_cor_ran <- matrix(runif(dim(df_cor)[1] * dim(df_cor)[2], min=-1, max=1), nrow=dim(df_cor)[1], ncol=dim(df_cor)[2]) %>%
      as.data.frame()
    model_km <- kmeans(df_cor_ran, centers=centerN, nstart=100, iter.max=1000)
    betweenOnTotals <- model_km$betweenss / model_km$totss
  }
  BOTs[centerN] <- mean(betweenOnTotals)
}


ggplot() +
  geom_line(data=data.frame(x=c(1:length(betweenOnTotals)), y=betweenOnTotals), aes(x=x, y=y)) +
  geom_line(data=data.frame(x=c(1:length(BOTs)), y=BOTs), aes(x=x, y=y)) +
  labs(x="Number of clusters", y="Variation explained between cluster",
       title="Variation explained with different numbers of clusters") +
  scale_y_continuous(labels=scales::percent)


corPlot <- function(df_cor) {
  centerN <- 5
  model_km <- kmeans(df_cor, centers=centerN, nstart=100, iter.max=1000)
  df_cor$cluster <- model_km$cluster
  df_cor_arranged <- arrange(df_cor, cluster)
  
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
}

corPlot(df_cor)

for(i in c(1:5)) {
  df_cor_ran <- matrix(runif(dim(df_cor)[1] * dim(df_cor)[2], min=-1, max=1), nrow=dim(df_cor)[1], ncol=dim(df_cor)[2]) %>%
    as.data.frame()
  plot(corPlot(df_cor_ran))
}




t.test(filter(df_player, sex == 1)$gap_agreeableness, filter(df_player, sex == 2)$gap_agreeableness, paired=FALSE)
t.test(filter(df_player, sex == 1)$gap_conscientiousness, filter(df_player, sex == 2)$gap_conscientiousness, paired=FALSE)
t.test(filter(df_player, sex == 1)$gap_emotionstability, filter(df_player, sex == 2)$gap_emotionstability, paired=FALSE)
t.test(filter(df_player, sex == 1)$gap_extraversion, filter(df_player, sex == 2)$gap_extraversion, paired=FALSE)
t.test(filter(df_player, sex == 1)$gap_openness, filter(df_player, sex == 2)$gap_openness, paired=FALSE)
t.test(filter(df_player, sex == 1)$gap_sum, filter(df_player, sex == 2)$gap_sum, paired=FALSE)
t.test(filter(df_player, sex == 1)$gap_sum_abs, filter(df_player, sex == 2)$gap_sum_abs, paired=FALSE)


df_1 <- filter(df, (tg_MOBA == 1 | tg_MMO == 1))
df_player_1 <- distinct(df_1, respondent, .keep_all=TRUE)
df_2 <- filter(df, (tg_MOBA == 0 & tg_MMO == 0))
df_player_2 <- distinct(df_2, respondent, .keep_all=TRUE)

t.test(df_1$gap_agreeableness, df_2$gap_agreeableness, paired=FALSE)
t.test(df_1$gap_conscientiousness, df_2$gap_conscientiousness, paired=FALSE)
t.test(df_1$gap_emotionstability, df_2$gap_emotionstability, paired=FALSE)
t.test(df_1$gap_extraversion, df_2$gap_extraversion, paired=FALSE)
t.test(df_1$gap_openness, df_2$gap_openness, paired=FALSE)
t.test(df_1$gap_sum, df_2$gap_sum, paired=FALSE)
t.test(df_1$gap_sum_abs, df_2$gap_sum_abs, paired=FALSE)


df_playerG <- group_by(df, respondent) %>%
  summarize(max = max(preference), avg = mean(preference))
cor(df_playerG$max, select(df_player, matches("^gap.*$")))
cor(df_playerG$avg, select(df_player, matches("^gap.*$")))
