# nesta aula, construiremos um modelo de classificação para as espécies dos pinguins
# o modelo que consideraremos é chamado de KNN: k nearest neighbours
# ele recebe este nome porque a classificação de uma observação que o modelo não conhece se dá a partir dos k vizinhos mais proximos desta observação; a partir destes k vizinhos mais proximos (do treinamento), faz-se uma votação e a espécie mais votada é a espécie do pinguim que estamos avaliando.
# consideraremos um vizinho, isto é, k=1; tente criar um modelo com um valor de k > 1;

library(ggplot2)

dados <- read.table(file = "pinguim.txt",
                    header = TRUE,
                    sep = ',') # importando os dados

head(dados) # imprimindo as primeiras 6 observacoes do conjunto
str(dados) # a estrtura do conjunto
unique(dados$sex) # os valores unicos da variavel sexo; identificamos valores inconsistentes; vamos remove-los.

dados <- dados[-which(dados$sex == "."),] # removendo o "."

dados <- dados[-which(is.na(dados$sex)),] # removendo os valores NA

summary(dados) # resumo estatístico dos dados; nenhum NA sobrou
unique(dados$island) # valores unicos da variavel ilha

# transformando as variaves especies, ilha e sexo em variaveis categoricas:
dados$species <- as.factor(dados$species)
dados$island <- as.factor(dados$island)
dados$sex <- as.factor(dados$sex)

dados <- dados[sample(nrow(dados)),] # embaralhando o conjunto de dados

n <- round(nrow(dados)*0.8) #determinando 80% do conjunto para separar o treinamento do modelo


treinamento <- dados[1:n,] # separando o treinamento
teste <- dados[-(1:n),] # separando o teste

# a seguir, algumas análises gráficas utilizando a biblioteca ggplot2 para entender o conjunto de treinamento

# frequencia de cada especie; em cada especie uma analise da distribuicao entre machos e femeas
ggplot(data = treinamento, aes(x = species, fill = sex))+
  geom_bar()+
  labs(x = "espécies",
       y = "frequência",
       title = "frequência de cada uma das espécies",
       fill = "sexo")+
  scale_fill_manual(values = c("darkorange", "darkorchid"),
                    labels = c("fêmea", "macho"))

# guardando o grafico em um objeto, neste gráfico o boxplot do peso em relacao aos machos e femeas 
grafico_base <- ggplot(data = treinamento, aes(x = sex, y = body_mass_g))+
  geom_boxplot()
grafico_base

# mesmo grafico anterior, mas agora a analise é feita por especie
grafico_base +
  facet_wrap(~species)

# grafico de dispersao do tamanho do bico versus profundidade do bico; cada especie representada por uma cor
ggplot(data = treinamento, aes(x = culmen_length_mm, y = culmen_depth_mm, color = species))+
  geom_point(size = 1.5)+
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))

# mesmo grafico anterior mas agora analisando a distribuicao em cada ilha
ggplot(data = treinamento, aes(x = culmen_length_mm, y = culmen_depth_mm, color = species))+
  geom_point(size = 1.5)+
  facet_wrap(~island)+
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))

# vamos criar nosso modelo KNN com K=1.
respostas <- c() # vetor que guardara as respostas do modelo para cada observacao do teste

# Itera sobre cada linha do conjunto de teste
for(j in 1:nrow(teste)){
  # Cria um vetor para armazenar as distâncias entre a observação atual do teste e as observações do conjunto de treinamento
  distancias <- c()
  
# Itera sobre cada linha do conjunto de treinamento
  for(k in 1:nrow(treinamento)){
    # Calcula a distância euclidiana entre a observação atual do teste e a observação atual do conjunto de treinamento
    distancias[k] <- sqrt(sum((teste[j,c(3,4)] - treinamento[k,c(3,4)])**2))
  }
  
  # Atribui à resposta da observação atual do teste a classe da observação do conjunto de treinamento mais próxima, usando o índice obtido pela ordenação das distâncias
  respostas[j] <- as.character(treinamento$species[order(distancias)[1]])
}

respostas

mean(respostas == teste$species) # taxa de acerto
