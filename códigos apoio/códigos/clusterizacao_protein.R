library(factoextra)
library(ggplot2)

#o conjunto de dados da aula de hoje contem informacoes sobre habitos alimentares de paises europeus; a ideia é agrupar os paises que possuem habitos alimentares semelhantes e entender o porque destas semelhancas; usaremos a tecnica de clusterizacao de Aglomerados hierarquicos e depois o Kmeans.



dados <- read.table("protein.txt", header = TRUE, sep = "\t")
str(dados)

dados_padronizados <- scale(dados[,-1])

matriz_distancias <- dist(dados_padronizados)

modelo <- hclust(matriz_distancias, method = "ward.D2")

plot(modelo)

# plotando o modelo com a funcao fviz_dend do pacote factoextra

fviz_dend(modelo, k =5)

#analisando cada aglomerado
aglomerados <- cutree(modelo, k = 5)
dados$aglomerados <- factor(aglomerados)

dados$Country[aglomerados == 1]
summary(dados[aglomerados == 1,])

ggplot(dados, aes(x = aglomerados, y = Fish))+
  geom_boxplot()+
  labs(title = "consumo de peixe em cada aglomerado")

ggplot(dados, aes(x = aglomerados, y = Eggs))+
  geom_boxplot()+
  labs(title = "consumo de ovos em cada aglomerado")

ggplot(dados, aes(x = aglomerados, y = RedMeat))+
  geom_boxplot()+
  labs(title = "consumo de carne vermelha em cada aglomerado")

ggplot(dados, aes(x = aglomerados, y = Cereals))+
  geom_boxplot()+
  labs(title = "consumo de cereais em cada aglomerado")

#vamos clusterizar agora com o K-means usando 6 clusters

modelo_kmeans <- kmeans(dados_padronizados, centers = 6)
modelo_kmeans$cluster

dados$Country[modelo_kmeans$cluster == 1]
dados$Country[modelo_kmeans$cluster == 2]
dados$Country[modelo_kmeans$cluster == 3]
dados$Country[modelo_kmeans$cluster == 4]
dados$Country[modelo_kmeans$cluster == 5]
dados$Country[modelo_kmeans$cluster == 6]
