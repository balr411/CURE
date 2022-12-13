## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library("ggplot2")
library("tictoc")
library("CURE")

## -----------------------------------------------------------------------------
head(jain)

## -----------------------------------------------------------------------------
length(unique(jain$true.clus))

## -----------------------------------------------------------------------------
par_df <- jain
names(par_df) <- c("x", "y", "Cluster")
par_df$Cluster <- as.factor(par_df$Cluster)
ggplot(par_df, aes(x = x, y = y, color = Cluster)) + geom_point() +
  labs(x = "X", y = "Y") +
  theme(axis.text = element_text(size = 14), axis.title.x = element_text(size  = 14),
        axis.title.y = element_text(size = 14), legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) 

