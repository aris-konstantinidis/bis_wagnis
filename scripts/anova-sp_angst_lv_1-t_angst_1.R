# anova
options(scipen=999)

data = read.csv('cluster.csv')

# high groups (exp vs control)
high = data[data$cluster_group == "high" & ( data$gruppe_3 == "Kontroll" | data$gruppe_3 == "Experimental" ), ]
high = subset(high, select = -c(X, t_angst_1, cluster_group))

high <- high %>%
  gather(key = "time", value = "sp_angst_lv", sp_angst_lv_1, sp_angst_lv_3) %>%
  convert_as_factor(id_nummer, gruppe_3)

bxp <- ggboxplot(
  high, x = "time", y = "sp_angst_lv",
  color = "gruppe_3", palette = "jco"
)
bxp

descr = high %>%
  group_by(time, gruppe_3) %>%
  get_summary_stats(sp_angst_lv, type = "mean_sd")

res.aov <- anova_test(
  data = high, dv = sp_angst_lv, wid = id_nummer,
  between = gruppe_3, within = time
)
anova = get_anova_table(res.aov)

ggplot(high, aes(x=time, y=sp_angst_lv, group = gruppe_3, colour = gruppe_3)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white")







# low groups (exp vs control)
low = data[data$cluster_group == "low" & ( data$gruppe_3 == "Kontroll" | data$gruppe_3 == "Experimental" ), ]
low = subset(low, select = -c(X, t_angst_1, cluster_group))

low <- low %>%
  gather(key = "time", value = "sp_angst_lv", sp_angst_lv_1, sp_angst_lv_3) %>%
  convert_as_factor(id_nummer, gruppe_3)

bxp <- ggboxplot(
  low, x = "time", y = "sp_angst_lv",
  color = "gruppe_3", palette = "jco"
)
bxp

descr = low %>%
  group_by(time, gruppe_3) %>%
  get_summary_stats(sp_angst_lv, type = "mean_sd")

res.aov <- anova_test(
  data = low, dv = sp_angst_lv, wid = id_nummer,
  between = gruppe_3, within = time
)
anova = get_anova_table(res.aov)

ggplot(low, aes(x=time, y=sp_angst_lv, group = gruppe_3, colour = gruppe_3)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white")



