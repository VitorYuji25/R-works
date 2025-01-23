#nesta aula vamos construir um modelo de floresta aleatoria que seja capaz de prever se uma pessoa continuara ou deixara de ser cliente de um banco banco

dados <- read.table("churn.txt", sep = ",", header = TRUE)
str(dados)

dados <- dados[,-c(1,2,3)] #excluindo as variaveis que nao trazem informacoes relevantes para a variavel resposta

unique(dados$Geography)

#manipulando as variaveis: transformando as variaveis que foram importadas com classes erradas
dados$Geography <- as.factor(dados$Geography)
dados$Gender <- as.factor(dados$Gender)
dados$HasCrCard <- as.factor(dados$HasCrCard)
dados$IsActiveMember <- as.factor(dados$IsActiveMember)
dados$Exited <- as.factor(dados$Exited)

#dados[,c(2,3,8,9,11)] <- lapply(dados[,c(2,3,8,9,11)], factor)

#treino, teste e avaliacao do modelo
n <- round(0.8*nrow(dados))
dados <- dados[sample(nrow(dados)),]
treinamento <- dados[1:n,]
teste <- dados[-(1:n),]

prop.table(table(treinamento$Exited))
prop.table(table(teste$Exited))

library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)

ggplot(data = treinamento, aes(x = Exited))+
  geom_bar()

ggplot(data = treinamento, aes(x = Gender, fill = Exited))+
  geom_bar()

ggplot(data = treinamento, aes(x = Age))+
  geom_histogram()+
  facet_wrap(~Exited)+
  theme_minimal()

ggplot(data = treinamento, aes(x = Age,fill = Exited))+
  geom_density(alpha = 0.5)


arvore.churn <- rpart(formula = Exited ~., data = treinamento, method = "class")
rpart.plot(arvore.churn, extra = 101)

previsao <- predict(arvore.churn, newdata = teste, type = "class")

previsao
mean(previsao == teste$Exited)

floresta <- randomForest(formula = Exited ~., data = treinamento, importance = TRUE)
floresta
previsao.floresta <- predict(floresta, newdata = teste, type = "class")
mean(previsao.floresta == teste$Exited)
varImpPlot(floresta) #importancia das variaveis para o modelo

#construindo uma floresta sem algumas variaveis do primeiro modelo
floresta2 <- randomForest(formula = Exited ~.-Gender - HasCrCard, data = treinamento, importance = TRUE)
previsao.floresta2 <- predict(floresta2, newdata = teste, type = "class")
mean(previsao.floresta2 == teste$Exited)
