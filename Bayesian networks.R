install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("bnlearn")
install.packages("gRain")
install.packages("Rcpp") 
install.packages("forecast")
library(Rcpp)
library(forecast)
library(tidyverse)
require(ggplot2)
library(dplyr)
require(bnlearn)
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
BiocManager::install("RBGL")

game_data = read.csv("epl2020.csv")

filtered_data = select (game_data,c(h_a,result,teamId,Referee.x,matchDay,wins,draws,loses,scored,missed,pts))

filtered_data[6:11] <- lapply(filtered_data[6:11], as.numeric)

summary(filtered_data)

model_hc <- hc(filtered_data)

model_iamb <- iamb(filtered_data)

model_h2pc <- h2pc(filtered_data)

par(mar=c(1,1,1,1))
par(cex=6)
graphviz.plot(model_hc)

arc.strength_gd <- arc.strength(
  x = model_hc,
  data = filtered_data
)
par(mar=c(1,1,1,1))
par(cex=6)
strength.plot(
  x = model_hc,
  strength = arc.strength_gd
)

bn_gd <- bn.fit(
  x = model_hc,
  data = filtered_data
)

v_models <- c(
  "iamb","hc","h2pc"
)

list_cv <- list()
for(j in v_models) try({ 
  list_cv[[j]] <- bn.cv(
    data = filtered_data,
    bn = j,
    k = 2,
    runs = 2
  )
},silent = TRUE)
list_cv

list_mean <- list()
for(j in names(list_cv)){
  for(k in 1:length(list_cv[[j]])){
    list_mean[[j]][[k]] <- rep(NA,length(list_cv[[j]][[k]]))
    for(l in 1:length(list_cv[[j]][[k]])){
      list_mean[[j]][[k]][l] <- list_cv[[j]][[k]][[l]]$loss
    }
  }
  list_mean[[j]] <- unlist(list_mean[[j]])
}
sort(base::sapply(X = list_mean,FUN = mean))

#-----------------------------------------------------------------------

score(model_hc, filtered_data, type = "loglik-cg")

score(model_h2pc, filtered_data, type = "loglik-cg")

score(model_hc, filtered_data, type = "aic-cg")

score(model_h2pc, filtered_data, type = "aic-cg")

score(model_hc, filtered_data, type = "bic-cg")

score(model_h2pc, filtered_data, type = "bic-cg")

hc_score = score(model_hc, filtered_data, type = "loglik-cg")

h2pc_score = score(model_h2pc, filtered_data, type = "loglik-cg")

score_table <- data.frame(hc_score,h2pc_score)

score_table[order(score_table, decreasing = TRUE)]

#------------------------------------------------------------------------
dev.off()
v_nodes <- nodes(model_hc)
names(v_nodes) <- v_nodes
strength_alarm <- arc.strength(
  x = model_hc,
  data = filtered_data
)
n_nodes <- nnodes(model_hc)

v_fillcolor <- viridis::viridis(n_nodes)
names(v_fillcolor) <- v_nodes

v_shape <- c(
  rep("circle",floor(n_nodes/3)),
  rep("ellipse",floor(n_nodes/3)),
  rep("box",n_nodes - 2*floor(n_nodes/3))
)
names(v_shape) <- v_nodes

v_edges <- paste0(strength_alarm[,"from"],"~",strength_alarm[,"to"])
names(v_edges) <- v_edges

v_edgecolor <- v_fillcolor[strength_alarm[,"from"]]
names(v_edgecolor) <- v_edges

graphNEL_alarm <- as.graphNEL(model_hc)

Rgraphviz::plot(
  x = graphNEL_alarm,
  y = "dot",
  attrs = list(), 
  nodeAttrs = list(
    fillcolor = v_fillcolor,
    shape = v_shape
  ), 
  edgeAttrs = list(
    label = v_edges,
    weight = graph::edgeWeights(graphNEL_alarm),
    color = v_edgecolor,
    fontcolor = v_edgecolor
  )
)

#---------------------------------------------------------------------

model_fit = bn.fit(model_hc, filtered_data)
model_fit$pts	

pred = predict(model_fit, "pts",filtered_data) 
pred

accuracy(object = pred, x = filtered_data$pts)