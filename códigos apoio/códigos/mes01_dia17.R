#########################
# revisao da aula passada
#########################

# vetores
x <- c(1,65,32,45) # criando vetores
x <- c(x,12) # adicionando elementos a um vetor
print(x) # imprimindo um vetor
x <- c(x, x) # concatenando um vetor com ele mesmo
x <- c(x, "a") # vetores admitem elementos da mesma classe apenas
x # todos os elementos numericos foram convertidos para character

y <- 14:123 # um vetor sequencial com passo 1 indo de 14 ate 123
y
y[67] # acessando o elemento da posicao 67 do vetor y

y[c(67,81)] # acessando os elementos da posicoes 67 e 81 do vetor y

################################
# fim da revisao da aula passada
################################

# operando vetores

y/3 # dividindo o vetor y por 3

z <- c(10,45,12)
z/2 # dividindo o vetor z por 2

z/c(2,3,5,12) # divisao entre vetores e o conceito de reciclagem

w <- c(12,34,1,0,2)

z[c(TRUE,TRUE, FALSE, TRUE)] # filtrando um vetor a partir de um vetor logico

?seq # help do R: perguntando sobre a funcao seq()

seq(from = 12, to = 1234, length.out = 100)

?sample # help do R: perguntando sobre a funcao sample()

dados <- sample(x = 1:6, size = 100000, replace = TRUE) # lancando um dado 100 mil vezes
dados
mean(dados) # calculando a média dos valores obtidos; observe que este valor, teoricamente, deve estar perto de 3.5


mean(dados == 4) # proporcao de vezes que o valor 4 foi obtido

# experimento: vamos lançar dois dados uma quantidade n de vezes e depois verificar a proporcao de vezes em que a soma dos dois dados foi menor ou igual a 4.
n <- 1000000
dados1 <- sample(x = 1:6, size = n, replace = TRUE)
dados2 <- sample(x = 1:6, size = n, replace = TRUE)

soma <- dados1 + dados2
mean(soma <= 4)

# vamos repetir o experimento, só que agora vamos replica-lo 500 vezes, isto é, 1) lancamos dois dados 10 mil vezes e calculamos a proporcao de vezes em que a soma foi <= 4; 2) repetimos isto 500 vezes sendo que, para cada vez, a proporcao é armazenada no vetor medias.
medias <- c()
for (j in 1:500) {
  dados1 <- sample(x = 1:6, size = 10000, replace = TRUE)
  dados2 <- sample(x = 1:6, size = 10000, replace = TRUE)
  soma <- dados1 + dados2
  medias <- c(medias, mean(soma <= 4))
}

hist(medias) # plotando o histograma das medias (proporcoes)
