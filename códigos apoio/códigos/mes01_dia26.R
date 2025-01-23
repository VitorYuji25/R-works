# vamos importar o arquivo pinguim.txt para o r usando a funcao read.table; voce pode conhecer os argumentos desta funcao a partir do comando ?read.table
dados <- read.table(file = "pinguim.txt",
                    header = TRUE,
                  sep = ",")

#analisando a estrutura do conjunto
str(dados)
dados$species
tabela <- table(dados$species) #tabela de frequencia para a quantidade de observacoes em cada especie
barplot(tabela) # grafico de barras da tabela de frequencia das especies

summary(dados) # funcao que retorna um resumo estatistico das variaveis do conjunto

table(dados$sex) #tabela de frequencia para a quantidade de observacoes de cada sexo; observamos que há dados faltantes do tipo NA e algumas entradas invalidas que possuem "."como valor

faltantes<- which(dados$sex == ".") # detectando as entradas que possuem "." na coluna sex

dados <- dados[-faltantes,] # retirando as entradas faltantes

is.na(dados$sex) # is.na() verifica quais entradas de uma variavel possuem NA (not available; faltante, nao disponivel)
summary(dados)

dados <- dados[-which(is.na(dados$sex)),] # retirando as entradas que tinham NA na coluna sexo

summary(dados) # nao tem mais dado faltante!

dados$sex <- as.factor(dados$sex) # transformando a variavel sexo em categorica 

dados$species <- as.factor(dados$species) # transformando a variavel species em categorica 

dados$island <- as.factor(dados$island) # transformando a variavel ilha em categorica 

summary(dados) # o resumo dos dados agora retorna a variavel sexo como categoria (factor); antes ela era do tipo character (string); o mesmo para ilha e especie

install.packages(ggplot2) # instalando o pacote ggplot2, um excelente pacote para visualizacao de dados
library(ggplot2) # lendo o pacote

# a funcao ggplot é formado basicamente por três partes: data, mapping (mapeamento estetico) e geometria; data para entrar com os dados, mapping que associara as variaveis do conjunto a parte estetica do grafico e geometria fala da geometria do grafico (ponto, linha, histograma, boxplot, barra, etc)

ggplot(data = dados, mapping = aes(x = island, fill = species))+
  geom_bar()+
  theme_minimal()

ggplot(data = dados, mapping = aes(x = culmen_length_mm, y = flipper_length_mm, color = species))+
  geom_jitter(size = 2, alpha = 0.5)

ggplot(data = dados, 
       mapping = aes(y = flipper_length_mm, x = species))+
  geom_boxplot()
summary(dados)
