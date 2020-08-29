install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggrepel")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggforce")
install.packages("fastDummies")
install.packages("ggfortify")
install.packages("Rtsne")
install.packages("NMF")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(cluster)
library(factoextra)
library(fastDummies)
library(ggfortify)
library(Rtsne)
library(NMF)
require(ggplot2)
require(ggforce)

game_data = read.csv("epl2020_q1.csv")

filtered_data = select (game_data,c(h_a,result,teamId,pts,wins,draws,loses))

results <- fastDummies::dummy_cols(filtered_data, select_columns = c("h_a", "result","teamId"))

v_keep = c("wins","loses","draws","h_a_a","h_a_h","result_d","result_w","result_l","teamId_Liverpool","teamId_Man_City")

M = results[,v_keep]

scale_M = scale(M)

#--------------------------------------------------------------------------------------------------------------------------
  
prcomp_new <- prcomp(
    x = scale_M,
    center = TRUE,
    scale. = TRUE,
    rank = 2
  )
summary(prcomp_new)

str(prcomp_new)

head(prcomp_new$x)

plot(prcomp_new)

biplot(prcomp_new)

plot(
  prcomp_new$x[,1:2],
  col = factor(filtered_data$pts),
  pch = as.character(y),
  main = "Scatterplot of EPL PCA two dimensions"
)

#-----------------------------------------------------------------------------------------
  
y <- filtered_data$pts

set.seed(823)
Rtsne_1 <- Rtsne::Rtsne(
  X = table_pred,
  dims = 3,
  PCA = FALSE,
  max_iter = 2000,
  perplexity = 2
)

plot(
  Rtsne_1$Y,
  pch = as.character(y),
  col = factor(filtered_data$pts),
  main = "Scatterplot of EPL T-SNE two dimensions"
)

prcomp_new <- prcomp(
  x = scale_M,
  center = TRUE,
  scale. = TRUE,
  rank = 2
)

plot(
  prcomp_new$x[,1:2],
  col = factor(filtered_data$pts),
  pch = as.character(y),
  main = "Scatterplot of EPL PCA two dimensions"
)

#-------------------------------------------------------------------
  
nmf_df = scale_M

nmf_df[nmf_df<0] = 0

nmf_var <- NMF::nmf(
  x = as.matrix(nmf_df),
  rank = 2
)

nmf_var

basis_var <- NMF::basis(nmf_var)

coef_var <- NMF::coef(nmf_var)

dim(nmf_df)

dim(basis_var)

dim(coef_var)

colnames(basis_var) <- c("topic_1","topic_2")
rownames(coef_var) <- c("topic_1","topic_2")

round(head(coef_var[,1:2]),3)

ggplot(basis_var, aes(x=topic_1, y=topic_2)) + geom_point(aes(color=factor(filtered_data$pts)))
