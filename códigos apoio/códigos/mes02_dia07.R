#nesta aula aprendemos sobre correlacao e como encontrar a melhor reta de regressao linear
library(ggplot2)

cor(iris$Sepal.Length, iris$Sepal.Width) #correlacao entre comprimento da sepala e largura da sepala do conjunto da iris

ggplot(data = iris, aes(x= Petal.Length, y = Petal.Width))+
  geom_point()

cor(iris$Petal.Length, iris$Petal.Width)

cor(iris[,1:4]) #correlacao entre todas as variaves numericas do conjunto da iris

#a funcao lm() nos permite determinar a melhor reta de regressao; a seguir queremos escrever o comprimento da petala (y) em funcao da largura da petala (x) a partir de uma reta, isto e, y = mx + n; a funcao lm() nos diz quem é m e quem é n.
lm(iris$Petal.Length ~iris$Petal.Width)
y = m*x + n = 2.230*x + 1.084
# tamanho.petala = 2.230*largura + 1.084

plot(x = iris$Petal.Width, y = iris$Petal.Length, pch = 16)

min(iris$Petal.Width)
max(iris$Petal.Width)

x <- seq(from = 0.1, by = 0.01, to = 2.5)

reta <- function(x){
  return(2.230*x + 1.084)
}
y <- reta(x)
y
lines(x,y, col = "red")
