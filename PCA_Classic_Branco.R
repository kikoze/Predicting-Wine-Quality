setwd("C:/Users/andre/Desktop/Mestrado/An?lise Multivariada/Projeto-Meu/Vinhos")


library(caret) 
library(naivebayes)
library(e1071)
library(caTools)
library(ggplot2)
library(lattice)


vinho_branco <- read.csv(file = 'winequality-white.csv', sep=';')

variaveis_explicativas = vinho_branco[,0:11]
variavel_explicada = vinho_branco[,12]


a<-t(apply(vinho_branco,2,summary))
print(a)



library(ggcorrplot)
correlacao = cor(vinho_branco)
ggcorrplot(correlacao, type = "lower", outline.color = "white", ggtheme = ggplot2::theme_gray, colors = c("#FF007F", "#8888FF", "#00FF00"), lab = TRUE)



print(correlacao)
barplot(table(vinho_branco$quality),col = "yellow",border = "blue")


boxplot(vinho_branco)


#----------------------------------------------------------------------------------------------------------------
#PCA Classica
#----------------------------------------------------------------------------------------------------------------
library("mvtnorm")
library("robustbase")
library("rrcov")


data_standardizada<- scale(variaveis_explicativas)

data_standard.cpca <- prcomp(data_standardizada, scale. = F, retx=TRUE)
summary(data_standard.cpca)
screeplot(data_standard.cpca, main="PCA based on the Standardized Variables", type="line",cex=0.8)
abline(h=mean(data_standard.cpca$sdev^2),col="green")




#Escrever os nossos dados dos vinhos mas agora em vez das 11 features temos 6 PC's
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada



##---------------------------------------------------------------------------
#Aplicar o classificador Naive Bayes Para tentar estimar a Qualidade dos Vinhos
##---------------------------------------------------------------------------

#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Quality~., data=train_cl, trControl=train_control, method="nb")


naive_pred <- predict(model, test_cl, type = "raw")


#confusion matrix LDA
cmx <- confusionMatrix(naive_pred, test_cl$Quality)



cat("Accuracy Naive Bayes PCA Classica White Wine =" , cmx$overall[1])

##---------------------------------------------------------------------------
# Naive Bayes considerando certo quando 2 classes t?m Probabilidade >0.4
##---------------------------------------------------------------------------

naive_pred_prob <- predict(model, test_cl, type = "prob")

for (i in 1:length(naive_pred))
{
  if (length(which(naive_pred_prob[i,] > 0.40)) == 2)
  {
    naive_pred[i] = test_cl$Quality[i]
  }
}

cmx <- confusionMatrix(naive_pred, test_cl$Quality)
cat("Accuracy Naive Bayes PCA Classica White Wine Considerando Certo quando 2 Classes t?m P >0.4 = =" , cmx$overall[1])



##---------------------------------------------------------------------------
# Naive Bayes considerando Output Classes = 0,1,2
##---------------------------------------------------------------------------
set.seed(1234)
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada


#Acrescentar uma coluna Extra em que qualidade ? alterada para escala de 0,1,2
#Correr se For para ver os resultados em output 0-1-2

standard.df$Good_Bad = variavel_explicada
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 3 | standard.df$Good_Bad == 4), 0)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 5 | standard.df$Good_Bad == 6), 1)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad >= 7), 2)


#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 

standard.df$Good_Bad = as.factor(standard.df$Good_Bad)
standard.df <- standard.df[ -c(7) ]


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Good_Bad~., data=train_cl, trControl=train_control, method="nb")


naive_pred <- predict(model, test_cl, type = "raw")


#confusion matrix LDA
cmx <- confusionMatrix(naive_pred, test_cl$Good_Bad)

cat("Accuracy Naive Bayes PCA Classica White Wine Output 0,1,2 =" , cmx$overall[1])




##---------------------------------------------------------------------------
#Aplicar o classificador Support Vector Machines
##---------------------------------------------------------------------------

#Escrever os nossos dados dos vinhos mas agora em vez das 11 features temos 5 PC's
set.seed(1234)
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada


##---------------------------------------------------------------------------
#Aplicar o classificador SVM Para tentar estimar a Qualidade dos Vinhos
#S? se vai usar as primeiras 5 PC's
##---------------------------------------------------------------------------

#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]

#Matriz que vai ter as accuracies da grid search, posteriormente vamos escolher os parametros que tenham dado a melhor accuracy
lll = matrix(0, nrow = 13, ncol = 13)

#Inicializa??o dos vetores Cost e Gamma para a grid Search
gama=10^(-9:3)
cot=10^(-2:10)

#Fazer ciclo com 2 for, em que vamos variando o gamma e o custo para obter os parametros que maximizam a accuracy



svmfit = svm(train_cl$Quality ~ ., data = train_cl, kernel = "radial", cost = 10, gamma = 10, scale = FALSE, probability = TRUE, cross = 5)

y_pred_SVM <- predict(svmfit, newdata = test_cl,decision.values = TRUE, probability = TRUE) 


table(predict = as.factor(y_pred_SVM), truth = as.factor(test_cl$Quality))
cmx <- confusionMatrix(as.factor(test_cl$Quality),as.factor(y_pred_SVM),dnn = c("Prediction", "Reference"))

ggplot(as.data.frame(cmx$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Class_3","Class_4","Class_5","Class_6","Class_7","Class_8")) +
  scale_y_discrete(labels=c("Class_8","Class_7","Class_6","Class_5","Class_4","Class_3"))



cat("Accuracy SVM PCA Classica White Wine =" , cmx$overall[1])



##---------------------------------------------------------------------------
# SVM considerando certo quando 2 classes t?m Probabilidade >0.4
##---------------------------------------------------------------------------
set.seed(1234)
AAA<-attr(y_pred_SVM, "probabilities")


for (i in 1:length(y_pred_SVM))
{
  if (length(which(AAA[i,] > 0.40)) == 2)
  {
    y_pred_SVM[i] = test_cl$Quality[i]
  }
}


table(predict = as.factor(y_pred_SVM), truth = as.factor(test_cl$Quality))
cmx <- confusionMatrix(as.factor(test_cl$Quality),as.factor(y_pred_SVM),dnn = c("Prediction", "Reference"))



cat("Accuracy SVM PCA Classica White Wine Considerando Certo quando 2 Classes t?m P >0.4 =" , cmx$overall[1])



##---------------------------------------------------------------------------
# SVM considerando Output Classes = 0,1,2
##---------------------------------------------------------------------------
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada

#Acrescentar uma coluna Extra em que qualidade ? alterada para escala de 0,1,2
#Correr se For para ver os resultados em output 0-1-2

standard.df$Good_Bad = variavel_explicada
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 3 | standard.df$Good_Bad == 4), 0)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 5 | standard.df$Good_Bad == 6), 1)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad >= 7), 2)


#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 

standard.df$Good_Bad = as.factor(standard.df$Good_Bad)
standard.df <- standard.df[ -c(7) ]


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]


#Matriz que vai ter as accuracies da grid search, posteriormente vamos escolher os parametros que tenham dado a melhor accuracy
lll = matrix(0, nrow = 13, ncol = 13)

#Inicializa??o dos vetores Cost e Gamma para a grid Search
gama=10^(-9:3)
cot=10^(-2:10)

#Fazer ciclo com 2 for, em que vamos variando o gamma e o custo para obter os parametros que maximizam a accuracy


svmfit = svm(train_cl$Good_Bad ~ ., data = train_cl, kernel = "radial", cost = 10, gamma = 10, scale = FALSE, probability = TRUE,cross = 5)
y_pred_SVM <- predict(svmfit, newdata = test_cl,decision.values = TRUE, probability = TRUE) 




tt<- table(predict = as.factor(y_pred_SVM), truth = as.factor(test_cl$Good_Bad))
print(tt)
cmx <- confusionMatrix(as.factor(test_cl$Good_Bad),as.factor(y_pred_SVM),dnn = c("Prediction", "Reference"))


cat("Accuracy SVM PCA Classica White Wine Output 0,1,2 =" , cmx$overall[1])


#-----------------------------------------------------------------------------------------------------------------
#LDA
#-----------------------------------------------------------------------------------------------------------------

y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada

#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]


train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Quality~., data=train_cl, trControl=train_control, method="lda")


# lda_pred_prob <- predict(model, test_cl, type = "prob")
lda_pred <- predict(model, test_cl, type = "raw")



#confusion matrix LDA
cmx <- confusionMatrix(lda_pred, test_cl$Quality)



cat("Accuracy LDA PCA Classica White Wine =" , cmx$overall[1])

##---------------------------------------------------------------------------
# LDA considerando certo quando 2 classes t?m Probabilidade >0.4
##---------------------------------------------------------------------------

lda_pred_prob <- predict(model, test_cl, type = "prob")

for (i in 1:length(lda_pred))
{
  if (length(which(lda_pred_prob[i,] > 0.40)) == 2)
  {
    lda_pred[i] = test_cl$Quality[i]
  }
}

cmx <- confusionMatrix(lda_pred, test_cl$Quality)
cat("Accuracy LDA White Wine PCA Classica Considerando Certo quando 2 Classes t?m P >0.4 = =" , cmx$overall[1])

##---------------------------------------------------------------------------
# LDA considerando Output Classes = 0,1,2
##---------------------------------------------------------------------------

set.seed(1234)
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada


#Acrescentar uma coluna Extra em que qualidade ? alterada para escala de 0,1,2
#Correr se For para ver os resultados em output 0-1-2

standard.df$Good_Bad = variavel_explicada
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 3 | standard.df$Good_Bad == 4), 0)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 5 | standard.df$Good_Bad == 6), 1)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad >= 7), 2)


#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 

standard.df$Good_Bad = as.factor(standard.df$Good_Bad)
standard.df <- standard.df[ -c(7) ]


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]


train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Good_Bad~., data=train_cl, trControl=train_control, method="lda")


# lda_pred_prob <- predict(model, test_cl, type = "prob")
lda_pred <- predict(model, test_cl, type = "raw")



#confusion matrix LDA
cmx <- confusionMatrix(lda_pred, test_cl$Good_Bad)



cat("Accuracy LDA PCA Classica White Wine Output Classes 0,1,2 =" , cmx$overall[1])


#-----------------------------------------------------------------------------------------------------------------
#Random Forest
#-----------------------------------------------------------------------------------------------------------------
set.seed(1234)
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada

#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]


train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Quality~., data=train_cl, trControl=train_control, method="rf")


rf_pred <- predict(model, test_cl, type = "raw")



#confusion matrix RF

cmx <- confusionMatrix(rf_pred, test_cl$Quality)


cat("Accuracy RF PCA Classica White Wine =" , cmx$overall[1])


##---------------------------------------------------------------------------
# RF considerando certo quando 2 classes t?m Probabilidade >0.4
##---------------------------------------------------------------------------

rf_pred_prob <- predict(model, test_cl, type = "prob")

for (i in 1:length(rf_pred))
{
  if (length(which(rf_pred_prob[i,] > 0.40)) == 2)
  {
    rf_pred[i] = test_cl$Quality[i]
  }
}

cmx <- confusionMatrix(rf_pred, test_cl$Quality)
cat("Accuracy RF White Wine PCA Classica Considerando Certo quando 2 Classes t?m P >0.4 = =" , cmx$overall[1])

##---------------------------------------------------------------------------
# RF considerando Output Classes = 0,1,2
##---------------------------------------------------------------------------

set.seed(1234)
y <- data_standard.cpca$x[,1:6]
standard.df <- as.data.frame(y) 
standard.df$Quality = variavel_explicada


#Acrescentar uma coluna Extra em que qualidade ? alterada para escala de 0,1,2
#Correr se For para ver os resultados em output 0-1-2

standard.df$Good_Bad = variavel_explicada
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 3 | standard.df$Good_Bad == 4), 0)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad == 5 | standard.df$Good_Bad == 6), 1)
standard.df$Good_Bad = replace(standard.df$Good_Bad, which(standard.df$Good_Bad >= 7), 2)


#data partition
set.seed(1234)
standard.df$Quality = as.factor(standard.df$Quality) 

standard.df$Good_Bad = as.factor(standard.df$Good_Bad)
standard.df <- standard.df[ -c(7) ]


#Dividir os Dados em treino e teste (70%,30%)
ind = sample(2, nrow(standard.df), replace = TRUE, prob = c(0.7,0.3))
train_cl = standard.df[ind == 1,]
test_cl = standard.df[ind == 2,]


train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)
model<- train(Good_Bad~., data=train_cl, trControl=train_control, method="rf")


rf_pred <- predict(model, test_cl, type = "raw")



#confusion matrix RF
cmx <- confusionMatrix(rf_pred, test_cl$Good_Bad)


cat("Accuracy RF PCA Classica White Wine Output Classes 0,1,2 =" , cmx$overall[1])

