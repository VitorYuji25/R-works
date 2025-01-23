#nesta aula, vamos usar o modelo KNN para classificar se uma pessoa tem um tumor maligno ou benigno a partir de informações de celulas que foram retiradas do tumor
dados <- read.csv("cancer.csv", header = TRUE)
str(dados)
dados$diagnosis <- as.factor(dados$diagnosis)

dados <- dados[sample(nrow(dados)),]

n <- round(0.8*nrow(dados))
n

treinamento <- dados[1:n,]
teste <- dados[-(1:n),]

ggplot(data = treinamento, aes(x = diagnosis))+
  geom_bar()

colnames(dados)

ggplot(data = treinamento, aes(y = radius_mean))+
  geom_boxplot()+
  facet_wrap(~diagnosis)

ggplot(data = treinamento, aes(x = concavity_mean, y = texture_mean, color = diagnosis))+
  geom_point()

cor(treinamento[,-1])

library(class)
?knn

#determinando o treinamento, o teste e escalonando os dados
treinamento.X <- scale(treinamento[,-1])
teste.X <- scale(teste[,-1])
treinamento.Y <- treinamento[,1]

#usando a funcao knn para construir o modelo com k=3 vizinhos
modelo.knn.cancer <- knn(train = treinamento.X, test = teste.X, cl = treinamento.Y, k = 3)

mean(modelo.knn.cancer == teste$diagnosis)

#matriz de confusao para determinar os falsos positivos e os falsos negativos.
table(modelo.knn.cancer, teste$diagnosis)
