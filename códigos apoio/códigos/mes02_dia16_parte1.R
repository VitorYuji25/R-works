#na primeira parte da aula, construiremos um cross validation para determinarmos o melhor valor de K para o modelo KNN
#vamos utilizar o conjunto da iris

data(iris) #resetando o conjunto da iris

set.seed(02161) #configurando uma semente para reproduzir os resultados; minha versao do R: 4.3.1

dados <- iris[sample(nrow(iris)),] #embaralhando as linhas

N <- round(nrow(dados)*0.8) #determinando o ponto de corte de 80% para separar treino e teste
N

treino <- dados[1:N,] #conjunto de treinamento
teste <- dados[-(1:N),] #conjunto de teste
nrow(teste)

#a seguir, vamos dividir o treino em 10 pastas; cada pasta terá 12 observações

#ideia: uma pasta será o teste e as outras o treinamento, para cada k variando de 1 até 10, avaliamos a taxa de acerto do modelo; repetimos isto para cada uma das 10 pastas e calculamos a média da taxa de acerto ao final; escolheremos o K que tiver a maior média.
passo_intervalo <- N/10
intervalos <- seq(from = 1, to = N, by = passo_intervalo)
intervalos <- c(intervalos, N+1)
intervalos

library(class)
#cv = cross validation
acertos <- rep(0, times = 10)
for(j in 1:(length(intervalos)-1)){
  indices <- intervalos[j]:(intervalos[j+1]-1)
  teste_cv <- treino[indices,]
  treino_cv <- treino[-indices,]
  acertos_atual <- c()
  for (k in 1:10) {
    modelo.knn <- knn(train = treino_cv[,-5], test = teste_cv[,-5], cl = treino_cv$Species, k)
    acertos_atual[k] <- mean(modelo.knn == teste_cv$Species)
  }
  acertos <- acertos + acertos_atual
}
media_acertos <- acertos/10
media_acertos

#grafico do numero de vizinhos k versus media de acerto
plot(x = 1:10, y = media_acertos, type = "l")

#melhor K: 7

#ao determinarmos o valor de k, construimos o modelo com este K e podemos agora usar o teste para avaliarmos este modelo.
modelo_final <- knn(train = treino[,-5], test = teste[,-5], cl = treino$Species, k = 7)

mean(modelo_final == teste$Species) #taxa de acerto do modelo final.



