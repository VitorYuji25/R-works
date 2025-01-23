# Grupo:
# - Maria Fernanda Nunes Gouveia (11921BCC042)
# - Vitor Yuji F. Matsushita (11921BCC021)

# Exercício 1. Crie os seguintes vetores:
#(a)(10, 11, 12,..., 30)
#(b)(30, 29, 28,..., 10)
#(c)(10, 11, 12,..., 30, 29, 28,..., 10)

a <- c(10:30)
a

b <- c(30:10)
b

c <- c(10:30, 29:10)
c

# Exercício 2

help(rep)
help(seq)

# (a)Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,..., 2, 4, 6, 8), em que há dez ocorrências do número 2.

a <- rep(seq(2, 8, by = 2), times = 10)
a

# (b)Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,..., 2, 4, 6, 8, 2), em que há onze ocorrências do número 2 e dez ocorrências dos números 4, 6 e 8

b <- c(rep(seq(2, 8, by = 2), times = 10), rep(2, times = 1))
b

# (c)Considere o vetor (3, 7, 1)...

vetor_original <- c(3, 7, 1)
vetor_original

# (1) Criar um novo vetor que repita os valores do vetor original três vezes
vetor_repete_tres_vezes <- rep(vetor_original, times = 3)
vetor_repete_tres_vezes

# (2) Criar outro vetor onde o valor 1 repete 4 vezes, valor 2 repete 2 vezes e valor 3 repete 3 vezes mantendo a ordem original
vetor_repete_423 <- c(rep(vetor_original[1], times = 4), rep(vetor_original[2], times = 2), rep(vetor_original[3], times = 3))
vetor_repete_423

# Exercício 3. Utilize a estrutura de vetores do R para realizar as seguintes somas:
#a)
n <- seq(20,30)
exp <- n**2 + 4*n
resultado <- sum(exp)
resultado
##Resultado = 8085

#b)
n2<- seq(10,20)
exp2<- ((3**n2/n2) + (2**n2/n2**2))
resultado2 <- sum(exp2)
resultado2
##Resultado = 268814233


#Exercício 4. Numa urna há bolas idênticas numeradas de 1 até 100. Serão extraídas 40 bolas com reposição
#desta urna. Simule este experimento e guarde o resultado dos sorteios em um vetor.
urna <- seq(1,100)
resultados <- sample(urna, size = 40, replace = TRUE)
sorteios <- c(resultados)
sorteios
##sorteio = [1] 43 63 51 93 55 23 87 32
#[9] 94 49 81 60 27 36 71 89
#[17] 32 28 20 25 70  4 54 67
#[25] 60 84 16 47 41 83 55 27
#[33] 70 24  2 62 66 61 50 53
#(a) Quantas bolas pares foram sorteadas?
pares <- sum(sorteios%%2 == 0)
pares
#[1] 19
#(b) Quantas bolas maiores do que 70 foram sorteadas?
maiores70<- sum(sorteios>70)
maiores70
#[1] 8
#(c) Em quais retiradas (posições) foram sorteadas as bolas ímpares?
posicoesImpares <- which(sorteios%%2 != 0)
posicoesImpares
#[1]  1  2  3  4  5  6  7 10
#[9] 11 13 15 16 20 24 28 29
#[17] 30 31 32 38 40

#Exercício 5. Crie um função no R que irá simular sucessivos lançamentos de um dado até que o número 4
#seja obtido pela segunda vez. A função deverá retornar o número de lançamentos que foram necessários até
#o 4 ser obtido pela segunda vez. Assim, se os sorteios foram 3, 6, 6, 5, 4, 2, 4 a função deverá retornar 7.
simula_lancamentos <- function() {
  n <- 1:6
  lancamentos <- 0
  ocorrencias <- 0
  
  while (ocorrencias < 2) {
    val <- sample(n, 1)
    lancamentos <- lancamentos + 1
    if (val == 4) {
      ocorrencias <- ocorrencias + 1
    }
  }
  return(lancamentos)
}

resultados <- simula_lancamentos()
resultados

#Exercício 6. Utilize a função do exercício anterior para replicar o experimento dez mil vezes. Para cada replicação, guarde o número de lançamentos #num vetor chamado quantidades. Por fim, calcule a média de quantidades. Interprete o resultado obtido

quantidades <- c()
for (i in 1:10000) {
  quantidades <- c(quantidades,simula_lancamentos())
}

media_lancamentos <- mean(quantidades)
media_lancamentos
cat("em media precisamos fazer", media_lancamentos, "lançamentos para obter  o número de lançamentos que foram necessários até o 4 ser obtido pela segunda vez")

#Exercicio 7.

fibonacci <- function(n) {
  if (n <= 0) return(NULL)
  if (n == 1) return(1)
  if (n == 2) return(c(1, 1))
  
  fib <- numeric(n)
  fib[1:2] <- 1
  for (i in 3:n) {
    fib[i] <- fib[i - 1] + fib[i - 2]
  }
  return(fib)
}

fibonacci(10)

#Exercicio 8

sorteio_amigo_oculto <- function(nomes) {
  sorteados <- sample(nomes)
  
  if (any(sorteados == nomes)) {
    return(0)
  } else {
    return(1)
  }
}

nomes_amigo_oculto <- c("Michael Scott", "Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton")

resultados <- c()

for (i in 1:100000) {
  resultados <- c(resultados,sorteio_amigo_oculto(nomes_amigo_oculto))
}

proporcao_erros <- mean(resultados)
proporcao_erros

#Exercicio 9

jogar_craps <- function() {
  
  dados <- sample(1:6, 2, replace = TRUE)
  soma <- sum(dados)
  
  #resultado da partida
  if (soma == 7 | soma == 11) {
    return(1)
  } else if (soma == 2 | soma == 3 | soma == 12) {
    return(0)
  } else {
    ponto <- soma
    repeat {
      # joga dados de novo
      dados <- sample(1:6, 2, replace = TRUE)
      nova_soma <- sum(dados)
      
      # pra ver se ganhou ou perdeu
      if (nova_soma == 7) {
        return(0)
      } else if (nova_soma == ponto) {
        return(1)
      }
    }
  }
}

resultados <-c()

for (i in 1:100000) {
  resultados <- c(resultados,jogar_craps())
}
proporcao_vitorias <- mean(resultados)
proporcao_vitorias

##Exercicio 10
N<-100
reta<-seq(0:N)
Luke_point<-sample(N,size = 1,replace = FALSE)#L
L<-Luke_point
coroa<-0
cara<-1

lanca_moeda<-function(){
  while(L>=1 && L<N){
    lancamento<-sample(moeda<-seq(0:1),size = 1,replace = TRUE)
    if(lancamento == 0){
      L<-L-1
    }
    else{
      L<-L+1
    }
    if(L == N){
      return(sprintf("Chegou em casa"))
    }
  }
  return(sprintf("Caiu no precipicio"))
}

result<-lanca_moeda()
sprintf(result)

##a)
N<-20
reta<-seq(0:N)
Luke_point<-sample(N,size = 1,replace = FALSE)#L
coroa<-0
cara<-1

lanca_moeda<-function(L){
  while(L>=1 && L<N){
    lancamento<-sample(moeda<-seq(0:1),size = 1,replace = TRUE)
    if(lancamento == 0){
      L<-L-1
    }
    else{
      L<-L+1
    }
    if(L == N){
      return(1)
    }
  }
  return(0)
}

result<-lanca_moeda(Luke_point)
result

##b)
N<-20
reta<-seq(0:N)
Luke_point<-0
coroa<-0
cara<-1
i<-0
vet<-c()

lanca_moeda<-function(L){
  while(L>=1 && L<N){
    lancamento<-sample(moeda<-seq(0:1),size = 1,replace = TRUE)
    if(lancamento == 0){
      L<-L-1
    }
    else{
      L<-L+1
    }
    if(L == N){
      return(1)
    }
  }
  return(0)
}

for(i in 1:10000){
  Luke_point<-sample(N,size = 1,replace = FALSE)#L
  result<-(lanca_moeda(Luke_point))
  vet<-c(vet,result)
}
mean(vet)
vet

##c)
library(ggplot2)

N<-20
reta<-seq(0:N)
Luke_point<-1
coroa<-0
cara<-1
i<-0
j<-1
vet<-c()

lanca_moeda<-function(L){
  while(L>=1 && L<=N){
    lancamento<-sample(moeda<-seq(0:1),size = 1,replace = TRUE)
    if(lancamento == 0){
      L<-L-1
    }
    else{
      L<-L+1
    }
    if(L == N){
      return(1)
    }
  }
  return(0)
}

for(i in 1:500){
  for(j in 19){
    Luke_point<- j ##L
    result<-(lanca_moeda(Luke_point))
    vet<-c(vet,result)
  }
}
proporcao <- data.frame(mean(vet))
proporcao
plot(proporcao)
vet
