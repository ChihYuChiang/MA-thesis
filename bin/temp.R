#--gap_sum ~ real_sum + satisfaction
model <- lm(gap_sum ~ ., data=select(df_player, gap_sum, real_sum, matches("^combined.+ct$")))
model_abs <- lm(gap_sum_abs ~ ., data=select(df_player, gap_sum_abs, real_sum, matches("^combined.+ct$")))

summary(model)
summary(model_abs)

cor(select(df, satis_autonomy, dissatis_autonomy, combined_autonomy))


#--gap_sum ~ satisfaction
model <- lm(gap_sum ~ ., data=select(df_player, gap_sum, matches("^combined.+ct$")))
model_abs <- lm(gap_sum_abs ~ ., data=select(df_player, gap_sum_abs, matches("^combined.+ct$")))

summary(model)
summary(model_abs)


#--gap_sum ~ real_sum + satisfaction_sum
model <- lm(gap_sum ~ ., data=select(df_player, gap_sum, real_sum, combined_sum))
model_abs <- lm(gap_sum_abs ~ ., data=select(df_player, gap_sum_abs, real_sum, combined_sum))

summary(model)
summary(model_abs)


#--gap_sum ~ satisfaction_sum
model <- lm(gap_sum ~ ., data=select(df_player, gap_sum, combined_sum))
model_abs <- lm(gap_sum_abs ~ ., data=select(df_player, gap_sum_abs, combined_sum))

summary(model)
summary(model_abs)


#--gap_sum ~ real_sum + satisfaction (gap_sum > 0)
model <- lm(gap_sum ~ ., data=select(filter(df_player, gap_sum > 0), gap_sum, real_sum, matches("^combined.+ct$")))
model_abs <- lm(gap_sum_abs ~ ., data=select(filter(df_player, gap_sum > 0), gap_sum_abs, real_sum, matches("^combined.+ct$")))

summary(model)
summary(model_abs)


#--gap_sum ~ satisfaction (gap_sum > 0)
model <- lm(gap_sum ~ ., data=select(filter(df_player, gap_sum > 0), gap_sum, matches("^combined.+ct$")))
model_abs <- lm(gap_sum_abs ~ ., data=select(filter(df_player, gap_sum > 0), gap_sum_abs, matches("^combined.+ct$")))

summary(model)
summary(model_abs)


#--gap_sum ~ real_sum + satisfaction (gap_sum > 0) + cov
model <- lm(gap_sum ~ ., data=select(filter(df_player_c, gap_sum > 0), gap_sum, real_sum, matches("^combined.+ct$"), starts_with("c_")))
model_abs <- lm(gap_sum_abs ~ ., data=select(filter(df_player_c, gap_sum > 0), gap_sum_abs, real_sum, matches("^combined.+ct$"), starts_with("c_")))

summary(model)
summary(model_abs)


#--gap_sum ~ satisfaction (gap_sum > 0) + cov
model <- lm(gap_sum ~ ., data=select(filter(df_player_c, gap_sum > 0), gap_sum, matches("^combined.+ct$"), starts_with("c_")))
model_abs <- lm(gap_sum_abs ~ ., data=select(filter(df_player_c, gap_sum > 0), gap_sum_abs, matches("^combined.+ct$"), starts_with("c_")))

summary(model)
summary(model_abs)


#--gap_sum ~ preference
model <- lm(gap_sum ~ ., data=select(df, preference, gap_sum))
model_abs <- lm(gap_sum_abs ~ ., data=select(df, preference, gap_sum_abs))

summary(model)
summary(model_abs)


#--game_sum ~ preference
model <- lm(game_sum ~ ., data=select(df, preference, game_sum))

summary(model)
