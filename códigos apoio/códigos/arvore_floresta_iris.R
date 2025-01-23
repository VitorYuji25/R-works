#uma arvore de decisao para classificar as especies da flor iris
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)

set.seed(123)
data(iris)
iris <- iris[sample(nrow(iris)),]
n <- round(0.8*nrow(iris))

treino <- iris[1:n,]
teste <- iris[-(1:n),]

arvore.iris <- rpart(formula = Species~., data = treino)

rpart.plot(arvore.iris, extra = 101)
mean(predict(arvore.iris, newdata = teste, type = 'class') == teste$Species)

#uma floresta aleatoria para classificar as especies da flor iris

floresta.iris <- randomForest(formula = Species ~ ., data = treino, ntree = 200)

previsao.floresta <- predict(floresta.iris, newdata = teste, type = "class")
previsao.floresta
mean(previsao.floresta == teste$Species)
