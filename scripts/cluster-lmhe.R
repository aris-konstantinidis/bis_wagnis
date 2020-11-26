library(foreign)
data = read.spss("data.sav", use.value.labels = TRUE, to.data.frame = TRUE)

# find subset of all classes including wagnis or control and get all variables of interest
data = data[data$gruppe_3 == "Spiel-Wagnis" | data$gruppe_3 == "Wagnis-Spiel" | data$gruppe_3 == "Wagnis-Leistung" | data$gruppe_3 == "Leistung-Wagnis" | data$gruppe_3 == "Kontroll", ]

# hoffnung auf erfolg in abhaengigkeit des clusters angst vor Leistungsversagen vs generelle Ängstlichkeit
# note that id's are not integers - id_nummer is decimal if at least NA in row
subset = subset(data, select = c(sp_angst_lv_1, t_angst_1, lmhe_1, lmhe_3, id_nummer, gruppe_3))
subset = subset[complete.cases(subset), ]


ca = subset(subset, select = c(sp_angst_lv_1, t_angst_1))

# find optimal k
k.max = 15
wss = sapply(1:k.max, function(k) { # within sum of squares
  kmeans(c(ca$sp_angst_lv_1, ca$t_angst_1), k, nstart=50, iter.max = 15)$tot.withinss
})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares"
) # elbow method, optimal k between 2-3
# kmeans clustering 
results = kmeans(ca, 2)
results
# plot

plot(ca[c("sp_angst_lv_1", "t_angst_1")], col = results$cluster)

# goal is to assign a cluster group to each subject
rvec = results$cluster
rvec
subset$cluster_group = rvec

sub = subset
sub[1,]
high_group = sub[sub[, "id_nummer"] == 137.0,]$cluster_group
high_group

sub$gruppe_3 <- as.character(sub$gruppe_3)
for (i in 1:nrow(sub)) {
  if (sub$cluster_group[i] == high_group) { # assign cluster group
    sub$cluster_group[i] = 'high'
  } else {
    sub$cluster_group[i] = 'low'
  }
  if (sub$gruppe_3[i] != "Kontroll") {
    sub$gruppe_3[i] = "Experimental"
  }
}

# find number of subjects in each group
nkh = 0
nkl = 0
neh = 0
nel = 0
for (i in 1:nrow(sub)) {
  if (sub$gruppe_3[i] == 'Kontroll' & sub$cluster_group[i] == "high") {
    nkh = nkh + 1
  } else if (sub$gruppe_3[i] == 'Kontroll' & sub$cluster_group[i] == "low") {
    nkl = nkl + 1
  } else if (sub$gruppe_3[i] == 'Experimental' & sub$cluster_group[i] == "high") {
    neh = neh + 1
  } else if (sub$gruppe_3[i] == 'Experimental' & sub$cluster_group[i] == "low") {
    nel = nel + 1
  }
}
write.csv(sub, './cluster-lmhe.csv')
