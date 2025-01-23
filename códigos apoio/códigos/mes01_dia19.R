# lancando um dado 10 vezes
dados <- sample(x = 1:6, size = 10, replace = TRUE)
dados

# criando funcoes
# vamos agora criar uma funcao em que entramos com o numero de lancamentos do dado, n, e a funcao retorna os resultados obtidos nos n lancamentos
joga_dado <- function(n){
  dados <- sample(x = 1:6, size = n, replace = TRUE)
  return(dados)
}
joga_dado(10)

# agora vamos criar uma funcao com dois argumentos; o argumento k detremina que faremos um sorteio dentro do conjunto 1,2,3,...k; o argumento n determina quantos sorteios serão feitos.
sorteio <- function(k,n){
  w <- sample(x = 1:k, size = n, replace = TRUE)
  return(w)
}

sorteio(n = 10,k = 100)

# agora criaremos um funcao que lanca um dado n vezes e retorna a proporcao em que o numero 3 foi obtido nos n lancamentos.
prop3 <- function(n){
  dados <- sample(x = 1:6, size = n, replace = TRUE)
        return(mean(dados == 3))
}
prop3(100000)



which(c(TRUE, FALSE, TRUE, FALSE, TRUE)) # a funcao which aplicado em um vetor logico retorna as posicoes dos elementos TRUE deste vetor logico



y <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
cumsum(y)/1:5



# a seguir, vamos lancar um dado 1000 vezes e analisar como se comporta a proporcao de vezes em que o 3 foi obtido ao longo do tempo, isto é, proporcao de 3 obtida com 1 lancamento, proporcao de 3 obtida com 2 lancamentos, proporcao de 3 obtida com 3 lancamentos, ..., proporcao de 3 obtida com 1000 lancamentos.

x <- joga_dado(1000) == 3
x # este vetor contem em quais lancamentos o 3 foi obtido
proporcao3 <- cumsum(x)/1:1000 # aqui a proporcao de 3 ao longo do tempo; certifique-se de ter entendido a funcao cumsum()

# grafico de linha do tempo versus proporcao de 3; para onde a curva está convergindo?
plot(x = 1:1000, y = proporcao3, type = "l")
abline(h = 1/6, col = "red")


# como funciona o while
# para introduzirmos o while, simularemos o seguinte experimento: vamos jogar na mega sena uma vez por semana e vamos jogar até ganharmos algum premio (acertar 4, 5 ou 6 numeros); quantas vezes iremos jogar até ganharmos pela primeira vez

j <- 0 # contador do numero de semanas
acertos <- 0 # variavel que guardara a quantidade de numeros que acertamos em um sorteio; ela será usada como criterio de parada do while porque se acertarmos menos do que 4 numeros continuamos a jogar.
while(acertos < 4){
  j <- j + 1
  bilhete <- sample(x = 1:60, size = 6, replace = FALSE)
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
  acertos <- sum(bilhete %in% sorteio)
}
j/50 # estimativa em anos; j era semanas; aproximando que 1 ano tem mais ou menos 50 semanas.


# conjuntos de dados no R
iris # este conjunto já está na biblioteca base do R; você pode obter mais informacoes sobre ele com o help do R: ?iris

iris[3,] # acessando a linha 3 do conjunto
iris[,4] # acessando a coluna 4 do conjunto
iris[10,5] # acessando a linha 10 e a coluna 5

# existem duas formas de acessar uma variável (coluna): pela posicao ou pelo nome; a ultima é mais utilizada
# a seguir as duas formas de se extrair as informações sobre a largura da sépala.

iris[,2]
iris$Petal.Width

iris$Species == "setosa" # verificando quais observações são setosa

iris$Sepal.Width[iris$Species == "setosa"] # extraindo a largura da sépala somente das setosas

setosa <- iris[iris$Species == "setosa",] # criando um conjunto de dados somente com as informacoes da setosa
