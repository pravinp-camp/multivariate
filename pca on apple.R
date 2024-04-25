install.packages("corr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factorextra")
library(factoextra)
df=read.csv("C:/Users/pravin/Documents/phtots/apple_quality.csv")
df
str(df)
colSums(is.na(df))
df=na.omit(df)
colSums(is.na(df))
df$Acidity=as.numeric(df$Acidity)
df=df[,2:8]
data_normalized =scale(df) 
head(data_normalized)
corr_matrix <- cor(data_normalized)
Corr_matrix
ggcorrplot(corr_matrix)
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:4]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1:3)
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
