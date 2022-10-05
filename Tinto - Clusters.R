setwd("C:/Users/andre/Desktop/Mestrado/Análise Multivariada/Projeto-Meu/Vinhos")


library(caret) 
library(naivebayes)
library(e1071)
library(caTools)
library(ggplot2)
library(lattice)

library(digest)
library(cluster)

vinho_tinto <- read.csv(file = 'winequality-red.csv', sep=';')

variaveis_explicativas = vinho_tinto[,0:11]
variavel_explicada = vinho_tinto[,12]


a<-t(apply(vinho_tinto,2,summary))
print(a)



library(ggcorrplot)
correlacao = cor(vinho_tinto)
ggcorrplot(correlacao, type = "lower", outline.color = "white", ggtheme = ggplot2::theme_gray, colors = c("#FF007F", "#8888FF", "#00FF00"), lab = TRUE)



print(correlacao)
barplot(table(vinho_tinto$quality),col = "yellow",border = "blue")


boxplot(vinho_tinto)


#-------------------------------------------------

data_standardizada<- scale(variaveis_explicativas)
y <- data_standardizada
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada


################################################################
#K-Means e K-Medoids para variáveis Normais (Sem Standardização)
#e Usando Features que melhor justificam a Qualidade
################################################################

library(factoextra)
# install.packages("NbClust")
library("NbClust")
#Descobrir o melhor numero de clusters para o Kmeans
# nb <- NbClust(data_standardizada, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
# fviz_nbclust(nb)

#Tendo em conta o numero obtido em cima que foi 3, lançar o kmeans, com 20 pontos de partida diferentes para encontrar a melhor partição dos dados.
res.km <- eclust(data_standardizada, "kmeans", nstart = 500, k=2)
fviz_silhouette(res.km)
Estatisticas_kmeans_Normais = res.km
table(res.km$cluster, variavel_explicada)




#### Kmedoids supostamente que leva à melhor Silhouette
# number_pam = fviz_nbclust(data_standardizada, pam, method = c("silhouette", "wss", "gap_stat"))
# number_pam$data #Mostra os valores da silhueta média para o k-numero de clusters

res2.km <- eclust(data_standardizada, "pam", k=2)
fviz_silhouette(res2.km)
Estatisticas_kmedoids_Normais = res2.km
table(res2.km$cluster, variavel_explicada)


#-------------------------------------------------------------------------------
library("mvtnorm")
library("robustbase")
library("rrcov")


#Crit Distances = 0.99999 para não aparecerem muitos outliers
pc.ROBPCA <- PcaHubert(variaveis_explicativas, kmax = ncol(data_standardizada), k = 2, crit.pca.distances = 0.99999, scale = TRUE)
summary(pc.ROBPCA)

robust_pca = predict(pc.ROBPCA)

# nb <- NbClust(robust_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
# fviz_nbclust(nb)

#Tendo em conta o numero obtido em cima que foi 3, lançar o kmeans, com 20 pontos de partida diferentes para encontrar a melhor partição dos dados.
res4.km <- eclust(robust_pca, "kmeans", nstart = 500, k=4)
fviz_silhouette(res4.km)
Estatisticas_kmeans_PCA = res4.km
table(res4.km$cluster, variavel_explicada)

pc.ROBPCA$loadings


####
#K means pca robusta, verifica-se que o ideal seria escolher 2 ou 4, no entanto decidimos reter as 4 porque parece haver uma melhor
# distinção na qualidade entre os variados clusters. Cluster 1 tem praticamente só bons vinhos, 2 bons vinhos tambem, 3 e 4 maus vinhos.
####




#### Kmedoids supostamente que leva à melhor Silhouette
# number_pam = fviz_nbclust(robust_pca, pam, method = c("silhouette", "wss", "gap_stat"))
# number_pam$data
res5.km <- eclust(robust_pca, "pam", k=4)
fviz_silhouette(res5.km)
Estatisticas_kmedoids_PCA = res5.km
table(res5.km$cluster, variavel_explicada)



#######################################################################################################
#Resolver o Problema de Supervided Learning Usando os Clusters
#######################################################################################################
#-----------------------------------------------------------------------------------------------------------------
#Random Forest
#-----------------------------------------------------------------------------------------------------------------

set.seed(1234)
pc.ROBPCA <- PcaHubert(variaveis_explicativas, kmax = ncol(data_standardizada), k = 4, crit.pca.distances = 0.99999, scale = TRUE)
y <- predict(pc.ROBPCA)
standard.df <- as.data.frame(y) 
standard.df$Cluster = res5.km$cluster



#data partition
set.seed(1234)
standard.df$Cluster = as.factor(standard.df$Cluster) 


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]


train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Cluster~., data=train_cl, trControl=train_control, method="rf")


rf_pred <- predict(model, test_cl, type = "raw")



#confusion matrix RF

cmx <- confusionMatrix(rf_pred, test_cl$Cluster)


library(tidyr)
library(MLmetrics)
cat("Accuracy RF PCA Robusta Red Wine =" , cmx$overall[1])
cat("F1-Score PCA Robusta Clusters Red Wine =" , F1_Score(rf_pred, test_cl$Cluster, positive = NULL))







