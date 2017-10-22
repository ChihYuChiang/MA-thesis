dfs_playerG <- select(df_player, respondent)
dfs_playerG$df <- map(dfs_playerG$respondent, ~ filter(df, respondent == .x))

df_cor <- data.frame()
for (i in seq(1:nrow(dfs_playerG))) {
  mx_temp <- cor(dfs_playerG$df[[i]]$preference, select(dfs_playerG$df[[i]], starts_with("tste_20"))[, c(1:20)]) %>%
    data.frame()
  df_cor <- bind_rows(df_cor, mx_temp)
}

ggplot(data=df_cor_expand, aes(x=x, y=y, fill=z)) +
  geom_tile()

df_cor_expand <- expand.grid(x=c(1:nrow(df_cor)), y=c(1:ncol(df_cor))) %>%
  rowwise() %>%
  mutate(z=df_cor[x, y]) %>%
  ungroup()


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
