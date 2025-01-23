# para fazer comentários no R, comece uma linha com #
# para executar uma linha do script, aperte run ou utilize o atalho ctrl+enter.

# operaçoes básicas

1 + 2 # soma
1 - 2 # subtraçao
3*3 # produto
3/2 # divisao
3**2 # potenciacao

# criando variáveis, atribuindo valores
a <- 1 + 2
1 + 3 -> b
b # imprimindo o valor de b
print(b) # imprimindo o valor de b a partir da função print()
class(b) # analisando a classe da variável b
# para nomear uma variável, existem algumas regras: não comece com número ou caractere especial, comece sempre com uma letra; depois da primeira letra, números e caracteres especiais são permitidos; espaços não são permitidos.

# variáveis do tipo character (string)
w <- "dados" #criando uma string (no R este tipo de variável pertence a uma classe chama character)
w
class(w)

# O R é case sensitive, isto é, ele diferencia maiúsculas de minúsculas.

# variáveis lógicas
x <- TRUE
y <- FALSE
class(x)

# variáveis lógicas podem ser vistas como lógicas; TRUE assume o valor 1 e FALSE assume o valor 0 (zero).
TRUE + TRUE + FALSE


# operacaoes lógicas

a < 1
a == 1
a == 3
a >= 3



# vetores

# criando vetores no R

v1 <- c(1,10,100,900) #para criar vetores você deve utilizar a função c(); esta função recebe este nome porque sua função é combinar/concatenar elementos.
v1[3] #acessando o elemento da posição 1 do vetor v1


# lembre-se que em R começamos a contar do 1 e não do zero.

v2 <- c(1,4)
v1[v2] # acessando os elementos das posições 1 e 4 de v1
v1[c(1,4)] # o filtro acima poderia ter sido feito diretamente desta forma

v3 <- c(1, 3, "pedro")
v3 # observe que vetores só admitem elementos da mesma classe

v4 <- 5:230 # criando um vetor sequencial de 5 até 230


v5 <- sample(60) # a função sample aplicada a um número inteiro N realiza uma permutação dos números 1,2,3,...,N.
v6 <- v5 < 30 # o vetor v6 é lógico e uma entrada sua será TRUE se o elemento desta posição em v5 for menor do que 30.
v6
sum(v6) # somando os elementos de v6: a soma representará a quantidade de elementos de v5 que eram menores do que 30.

v5[!v6] #a operação ! é a negação de um elementos lógico, isto é, !TRUE será FALSE e !FALSE será TRUE

print(!TRUE)
print(!FALSE)
