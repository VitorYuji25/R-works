dados <- read.csv(file = "titanic.csv", header = TRUE)

str(dados)

dados <- dados[,-1]

dados$Survived <- as.factor(dados$Survived)

library(ggplot2)

ggplot(data = dados, aes(x = Survived))+
  geom_bar()+
  facet_wrap(~Sex)
dados$Pclass <- as.factor(dados$Pclass)
ggplot(data = dados, aes(x = Pclass, fill = Survived))+
  geom_bar()+
  facet_wrap(~Sex)

dados$Age
sum(is.na(dados$Age))/nrow(dados)

dados$Name

ggplot(data = dados, aes(x = Age, fill = Survived))+
  geom_histogram()+
  facet_wrap(~Sex)+
  labs(title = "histograma para a idade",
       x = "idade",
       y = "frequencia")
