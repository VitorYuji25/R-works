# condicionais em r
# vamos aplicar o uso de condicionais em r para o seguinte problema:
# lanca-se um dado duas vezes; se (if) os resultados forem iguais, retorna-se "os resultados foram iguais; caso contrário se (else if) o dado 1 for menor do que o dado2, enta0 retorna-se "o primeiro lancamento foi menor do que o segundo"; caso contrário, retorna-se "o segundo lancamento foi menor do que o primeiro"

dados <- sample(x = 1:6, size = 2, replace = TRUE)
dados

if(dados[1] == dados[2]){
  print("os resultados foram iguais")
}else if(dados[1] < dados[2]){
  print("o primeiro lancamento foi menor do que o segundo")
}else{
  print("o segundo é menor")
}


#vamos, agora, criar um modelo capaz de classificar a especie de uma flor a partir das medidas do tamanho e da largura da pétala.

iris #imprimindo o conjunto
str(iris) #analisando a estrutura do conjunto
iris$Species #imprimindo a variável espécie pelo nome
iris[,5] #imprimindo a variável espécie pela posicao

# os dados do conjunto iris estao muito bem organizados, vamos embaralhar as linhas para, em seguida, dividir o conjunto em treino e teste

set.seed(2401) # usando uma semente para que voce possa obter os mesmo resultados que eu obtive; versao r que usei: 4.3.1
amostra <- sample(x = 1:150, size = 150, replace = FALSE)
amostra

iris <- iris[amostra,]

# uma vez que embaralhamos, vamos agora dividir o conjunto em 80%-20%; 80% para treinar o modelo e o restante para o teste.

n <- round(nrow(iris)*0.8) #80% do numero de observacoes
n


table(iris$Species) #quantidade de cada especie no conjunto todo

treinamento <- iris[1:n,] # parte do treino
teste <- iris[(n+1):nrow(iris),] # outra forma de escolher o teste iris[-(1:n),]

table(treinamento$Species) #quantidade de cada especie no conjunto de treino
barplot(table(treinamento$Species))
unique(treinamento$Species)

# separando as espécies do treino, cada especie estará em um data frame para que possamos analisa-las separadamente

setosa <- treinamento[treinamento$Species == "setosa",]
setosa

versicolor <- treinamento[treinamento$Species == "versicolor",]

virginica <- treinamento[treinamento$Species == "virginica",]

# analisando o histograma do tamanho da petala de cada especie
par(mfrow = c(1,3))
hist(setosa$Petal.Length)
hist(versicolor$Petal.Length)
hist(virginica$Petal.Length)


#criando um gráfico de pontos do tamanho versus largura da petala e pintando cada especie de uma cor
plot(x = treinamento$Petal.Length, y = treinamento$Petal.Width, type = "n", pch = 16)
points(x = setosa$Petal.Length, y = setosa$Petal.Width, pch = 16, col = "red")

points(x = versicolor$Petal.Length, y = versicolor$Petal.Width, pch = 16, col = "green")

points(x = virginica$Petal.Length, y = virginica$Petal.Width, pch = 16, col = "blue")
abline(h = 1.75)

# a partir da analise grafica nos decidimos que: se tamanho da petala menor do que 2.5, entao classificaremos a flor como setosa; caso contrario, se a largura da petala for menor do que 1.75, entao classificaremos a flor como versicolor e caso contrario, classificaremos a flor como virginica.

respostas <- c()
for(j in 1:nrow(teste)){
  if(teste$Petal.Length[j] < 2.5){
    respostas[j] <- "setosa"
  }else{
    if(teste$Petal.Width[j] < 1.75){
      respostas[j] <- "versicolor"
    }else{
      respostas[j] <- "virginica"
    }
  }
}
mean(respostas == teste$Species) #comparando as previsoes com as verdadeiras classificacoes; acerto de 96%
