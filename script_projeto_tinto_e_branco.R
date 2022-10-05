setwd("C:/Users/andre/Desktop/Mestrado/Análise Multivariada/Projeto-Meu/Vinhos")


vinho_tinto <- read.csv(file = 'winequality-red.csv', sep=';')
vinho_branco <- read.csv(file = 'winequality-white.csv', sep=';')

vec_tinto <- c(1:nrow(vinho_tinto))*0
vinho_tinto <- cbind(vinho_tinto, Tinto_ou_Branco = vec_tinto)

#Tinto = 0 //// Branco = 1

vec_branco <- (c(1:nrow(vinho_branco))*0+1)
vinho_branco <- cbind(vinho_branco, Tinto_ou_Branco = vec_branco)


total <- rbind(as.data.frame(vinho_tinto), as.data.frame(vinho_branco))
total = total[,-12]


library(ggcorrplot)
correlacao = cor(total)
ggcorrplot(correlacao, type = "lower", outline.color = "white", ggtheme = ggplot2::theme_gray, colors = c("#FF007F", "#8888FF", "#00FF00"), lab = TRUE)







variaveis_explicativas = total[,0:11]
variavel_explicada = total[,12]



#PCA sem Standardização. Não é boa maneira de abordar o problema, a 1ª PC explica logo 94%, o que é demais

# data.cpca <- prcomp(variaveis_explicativas, scale. = F, retx=TRUE)
# summary(data.cpca)
# #Mudar a label do Eixo dos Y, não é variâ
# screeplot(data.cpca, main="PCA based on the Standardized Variables", type="line",cex=0.8)
# abline(h=mean(data.cpca$sdev^2),col="green")
# 
# 

#Começar a PCA
data_standardizada<- scale(variaveis_explicativas)

data_standard.cpca <- prcomp(data_standardizada, scale. = F, retx=TRUE)
summary(data_standard.cpca)
#Mudar a label do Eixo dos Y, não é variâ
screeplot(data_standard.cpca, main="PCA based on the Standardized Variables", type="line",cex=0.8)
abline(h=mean(data_standard.cpca$sdev^2),col="green")


#ROBUSTA

#Crit Distances = 0.99999 para não aparecerem muitos outliers
pc.ROBPCA <- PcaHubert(data_standardizada, kmax = ncol(data_standardizada), k = 5, crit.pca.distances = 0.75)
summary(pc.ROBPCA)

#Mostrar o Scree Plot
screeplot(pc.ROBPCA)
abline(h=1 ,col="red")


#Mostrar um plot em que se vê por alto quem são os outliers e os inliers
plot(pc.ROBPCA,pch=19,lwd=2, col = variavel_explicada + 2)
plot(cbind(slot(pc.ROBPCA ,"sd"),slot(pc.ROBPCA ,"od")),pch=19,xlab="Score distance",ylab="Orthogonal Distance", col = ifelse(slot(pc.ROBPCA,"sd") < slot(pc.ROBPCA,"cutoff.sd") & slot(pc.ROBPCA,"od") <slot(pc.ROBPCA,"cutoff.od"),'black',as.numeric(variavel_explicada)+2))
abline(v=slot(pc.ROBPCA ,"cutoff.sd"),col="red")
abline(h=slot(pc.ROBPCA ,"cutoff.od"),col="red")
legend('topright', legend = c("Tinto","Branco"), col=2:3, cex = 0.8, pch = 19)
title("ROBPCA, k=5",cex=0.8)





print(loadings<-data_standard.cpca$r)
round(loadings[,1:6],3)
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Tinto_Ou_Branco = variavel_explicada


#Distinção entre Vinhos Tintos e Brancos através da PC1
plot(data_standard.cpca$x[,1], col = variavel_explicada + 1, pch = 19, ylab ="PC1", xlab = "Wine Number")
legend('topright', legend = c("Red","White"), col=1:2, cex = 0.8, pch = 19)
title("Green Wine PCA - PC1",cex=0.8)









