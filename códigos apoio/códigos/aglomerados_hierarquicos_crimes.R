library(rvest)
library(dplyr)
library(ggplot2)
library(factoextra)

#vamos, nesta aula, analisar um conjunto de dados que apresenta indices de crimes violentos por estado nos EUA; depois de raspar estes dados da wikipedia, vamos clusterizar os estados usando o modelo de Aglomerados Hierarquicos; clusterizar é agrupar aqueles estados que são semelhantes em relacao aos indices analisados.

#1. raspagem dos dados
url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_violent_crime_rate"

html <- read_html(url)

dados <- html |> 
  html_element("table") |>
  html_table() |>
  as.data.frame()

#2. manipulando os dados: (A) excluindo a primeira linha que contem informacoes sobre todo o pais (so queremos analisar os estados); (B) excluindo a segunda coluna que contem o indice de crimes violentos, ela é a soma das outras colunas; (C) padronizando as taxas (todo mundo na mesma escala), so variaveis numericas; (D) criando a matriz de distancias.

str(dados)
dados <- dados[-1,] #(A)
nomes <- dados$Location
dados <- dados[,-2] #(B)
dados <- scale(dados[,-1]) #(C)

matriz_distancias <- dist(dados) #(D)

#a seguir, clusterizando os dados com a tecnica dos Aglomerados Hierarquicos (Hierarchical Clustering)
modelo <- hclust(matriz_distancias, method = "complete")

plot(modelo) #plotando o dendograma do modelo

#cortando o dendograma de forma a gerar 5 aglomerados
aglomerados <- cutree(modelo, k = 5)

nomes[aglomerados == 5] #estados que estão no aglomerado 5

#é possivel visualizar os dados usando a funcao fviz_dend() do pacote factoextra

fviz_dend(modelo, k = 5)
