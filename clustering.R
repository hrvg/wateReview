library("clValid")
library("factoextra")
library("cowplot")

df <- filtered_data

distance <- get_dist(df)

set.seed(1789)

kmax <- nrow(df) %/% 2
p_dist <- fviz_dist(distance, gradient = list(low = "steelblue",  high = "white"))
p_wss <- fviz_nbclust(df, kmeans, method = "wss", k.max = kmax)
p_sil <- fviz_nbclust(df, kmeans, method = "silhouette", k.max = kmax)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = nrow(df) - 1, B = 50)
p_gap <- fviz_gap_stat(gap_stat)

print(p_dist)

lay <-  rbind(c(0, 1), c(2, 3))
gridExtra::grid.arrange(grobs = list(p_wss, p_sil, p_gap), layout_matrix = lay)

# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df, nClust = 2:kmax, 
              clMethods = clmethods, validation = "internal")

# Summary
summary(intern)

stab <- clValid(df, nClust = 2:kmax, clMethods = clmethods, 
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)

# APN: average proportion of observations not placed in the same cluster under both cases; how robust are the cluster under cross-validation
# AD: average distance between observations placed in the same cluster under both cases; how the observations wiggle in the cluster
# ADM: average distance between cluster centers for observations placed in the same cluster under both cases; how the cluster center wiggle
# FOM: figure of merit: average intra-cluster variance of the deleted column,

# plots to compare
optik <- 2

k2 <- kmeans(df, centers = optik, nstart = 25) 
pkm <- fviz_cluster(k2, geom = "text",  data = df) + theme_cowplot()

res.hc <- eclust(df, "hclust", k = 3, graph = FALSE) 
phc <- fviz_dend(res.hc, rect = TRUE, show_labels = TRUE)

gridExtra::grid.arrange(grobs = list(pkm, phc), nrow = 1)
