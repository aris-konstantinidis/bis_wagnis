# anova
options(scipen=999)

data = read.csv('cluster-kswg.csv')

# high groups (exp vs control)
high = data[data$cluster_group == "high" & ( data$gruppe_3 == "Kontroll" | data$gruppe_3 == "Experimental" ), ]
high = subset(high, select = -c(X, sp_angst_lv_1, t_angst_1, cluster_group))

high <- high %>%
  gather(key = "time", value = "kswg", kswg_1, kswg_3) %>%
  convert_as_factor(id_nummer, gruppe_3)

bxp <- ggboxplot(
  high, x = "time", y = "kswg",
  color = "gruppe_3", palette = "jco"
)
bxp

descr = high %>%
  group_by(time, gruppe_3) %>%
  get_summary_stats(kswg, type = "mean_sd")

res.aov <- anova_test(
  data = high, dv = kswg, wid = id_nummer,
  between = gruppe_3, within = time
)
anova = get_anova_table(res.aov)





###################################################################



# low groups (exp vs control)
low = data[data$cluster_group == "low" & ( data$gruppe_3 == "Kontroll" | data$gruppe_3 == "Experimental" ), ]
low = subset(low, select = -c(X, sp_angst_lv_1, t_angst_1, cluster_group))

low <- low %>%
  gather(key = "time", value = "kswg", kswg_1, kswg_3) %>%
  convert_as_factor(id_nummer, gruppe_3)

bxp <- ggboxplot(
  low, x = "time", y = "kswg",
  color = "gruppe_3", palette = "jco"
)
bxp

descr = low %>%
  group_by(time, gruppe_3) %>%
  get_summary_stats(kswg, type = "mean_sd")

res.aov <- anova_test(
  data = low, dv = kswg, wid = id_nummer,
  between = gruppe_3, within = time
)
anova = get_anova_table(res.aov)

