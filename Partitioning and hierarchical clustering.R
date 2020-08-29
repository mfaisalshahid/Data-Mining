install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggrepel")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggforce")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(cluster)
library(factoextra)
require(ggplot2)
require(ggforce)

game_data = read.csv("epl2020.csv")

table_teams <- game_data %>%
  group_by(teamId) %>%
  arrange(teamId) %>%
  summarise(P = n(),
            W = sum(wins),
            D = sum(draws),
            L = sum(loses),
            F = sum(scored),
            A = sum(missed),
            GD = F-A,
            Points = sum(pts)) %>%            
  arrange(desc(Points))
table_teams

temp_index <- table_teams$teamId
table_teams <- subset(table_teams, select = -c(teamId))
rownames(table_teams) <- temp_index

kmeans_1 <- kmeans(x = table_teams,centers = 2)
kmeans_1

scale_M <- scale(table_teams)

set.seed(823)
factoextra::fviz_nbclust(
  x = scale_M,
  FUNcluster = kmeans,
  method = "wss"
)

set.seed(823)
factoextra::fviz_nbclust(
  x = scale_M,
  FUNcluster = kmeans,
  method = "silhouette"
)

kmeans_2 <- kmeans(x = scale_M,centers = 2)
kmeans_2

v <- c(3,-3,-3,3,-3,3,-3,-3)
initial_centers <- matrix(
  data = c(v,-v),
  nrow = 2,
  byrow = TRUE
)
rownames(initial_centers) <- c(
  "Top Half",
  "Bottom Half"
)
colnames(initial_centers) <- c("p","W","D","L","F","A","GD","Points")
initial_centers

kmeans_3 <- kmeans(x = scale_M, centers = initial_centers)
kmeans_3

plot_var <- data.frame(
  prcomp(
    x = scale_M,
    center = FALSE,
    scale. = FALSE
  )$x[,1:2],
  Name = temp_index,
  Points = table_teams$Points,
  Cluster = as.character(kmeans_3$cluster),
  stringsAsFactors = FALSE
)

ggplot(plot_var) +
  aes(x = PC2,y = PC1,size = Points,color = Cluster,fill = Cluster,label = Name,group = Cluster) +
  geom_point() +
  ggrepel::geom_text_repel(color = "black",size = 3) +
  ggtitle("Scatter plot of Priemer League Teams with respect to Ranks by points") +
  theme_bw() +
  theme(legend.position = "none")

clara_var <- clara(scale_M, 2, samples = 10, pamLike = TRUE)
clara_var

factoextra::fviz_cluster(object = clara_var, data = scale_M)

fanny_var <- fanny(scale_M, 2)
fanny_var

factoextra::fviz_cluster(object = fanny_var, data = scale_M)

pam_var <- pam(scale_M, 2)
pam_var

factoextra::fviz_cluster(object = pam_var, data = scale_M)


dist_M <- dist(scale_M)
heatmap( x = as.matrix(dist_M), col = viridis::viridis(256))
factoextra::fviz_dist(dist_M)

hclust_M <- hclust(dist_M)
plot(hclust_M)

v_dist <- c("canberra","manhattan","euclidean")
list_dist <- lapply(
  X = v_dist,
  FUN = function(distance_method) dist(
    x = scale_M,
    method = distance_method
  )
)
names(list_dist) <- v_dist
v_hclust <- c("ward.D","single")

list_hclust <- list()
for(j in v_dist) for(k in v_hclust) list_hclust[[j]][[k]] <- hclust(
  d = list_dist[[j]],
  method = k
)
par(
  mfrow = c(length(v_dist),length(v_hclust)),
  mar = c(0,0,0,0),
  mai = c(0,0,0,0),
  oma = c(0,0,0,0)
)
for(j in v_dist) for(k in v_hclust) plot(
  x = list_hclust[[j]][[k]],
  labels = FALSE,
  axes = FALSE,
  main = paste("\n",j,"\n",k)
)

plot(
  x = list_hclust[["canberra"]][["ward.D"]],
  main = "Canberrra Ward's D",
  sub = ""
)

plot(
  x = list_hclust[["canberra"]][["single"]],
  main = "Canberrra Single",
  sub = ""
)

agnes_M <- cluster::agnes(scale_M)
plot(agnes_M)
as.hclust(x = agnes_M)
print(agnes_M)
as.dendrogram(object = agnes_M)
cutree(tree = agnes_M, k = 2)

diana_M <- cluster::diana(scale_M, metric = "manhattan")
plot(diana_M)
as.hclust(x = diana_M)
print(diana_M)
as.dendrogram(object = diana_M)
cutree(tree = diana_M, k = 2)

agnes_M <- cluster::agnes(scale_M, metric = "euclidean", method = "single")
plot(agnes_M)

d <- dist(scale_M, method = "canberra")

hc5 <- hclust(d, method = "ward.D")

cutree_M <- cutree(hc5, k = 2)

fviz_cluster(list(data = scale_M, cluster = cutree_M))