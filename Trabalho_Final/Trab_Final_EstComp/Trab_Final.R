#Grupo:Vitor Yuji F. Matsushita (11921BCC021)
#      Maria Fernanda Nunes Gouveia (11921BCC042)

# Exercício 1. O conjunto de dados SBI.csv contém informações de mais de 2
# mil crianças que compareceram aos serviços de emergˆencia de um hospital com
# febre e que foram submetidas a um teste para detecção de infecção bacteriana
# grave
#a)
# A variável sbi possui 4 categorias: Not Applicable, UTI, Pneum e Bact. Not
# Applicable significa que o teste deu negativo; j´a as outras categorias indicam a
# existˆencia de infecção bacteriana grave.

# (a)
#Adiciona um novo conjunto de dados (Uma nova coluna Infection)
dados<- read.csv("SBI.csv",sep = ",")
dados

dados$infection <- ifelse(dados$sbi != "Not Applicable","yes","no")
dados$infection<- as.factor(dados$infection)

head(dados)

#Retirar os dados das colunas X, id, sbi
# b)
dados <- dados[,-c(1,2,8)]
head(dados)


# c)
#Separar em dois novos conjuntos, em um treino e teste.
set.seed(321)
indice_treino<- sample(1:nrow(dados),0.8*nrow(dados))
treinamento<- dados[indice_treino,]
teste<- dados[-indice_treino]

cat("Tamanho do conjunto de treinamento:", nrow(treinamento), "\n")
cat("Tamanho do conjunto de teste:", nrow(teste), "\n")

# d)
#Criar um arvore de decisão classificar a variavel infection com as outras colunas.
# Carregue o conjunto de dados e divida-o em conjuntos de treinamento e teste
# Supondo que o conjunto de dados se chama "dados" e a variável alvo se chama "infection"
library(rpart)
library(rpart.plot)

# Crie a árvore
arvore <- rpart(infection ~ ., data = treinamento)

# Plote a árvore de decisão
library(rpart.plot)
rpart.plot(arvore)
text(arvore)

# Faca previsões no conjunto de teste
predicao <- predict(arvore, newdata = teste, type = "class")

# Calcule a acurácia do modelo
acuracia <- sum(predicao == teste$infection) / nrow(teste_new)
print(paste("Acurácia do modelo: ", acuracia))

# Crie uma matriz de confusão
matriz_confusao <- table(Predicted = predicao, Actual = teste$infection)
print(matriz_confusao)

# e)
#Modelo floresta Aletória
library(randomForest)
modelo_floresta <- randomForest(formula = infection ~ ., data = treino)

previsoes <- predict(modelo_floresta, newdata = test_data, type = "class")

acuracia <- sum(previsoes == test_data$infection) / nrow(test_data)
cat("Acurácia do modelo de floresta aleatória:", acuracia, "\n")

library(caret)
conf_matrix <- confusionMatrix(previsoes, test_data$infection)
print(conf_matrix$table)

# 2)
#a) Plote grafico flipper_length_mm vesus body_mass_g
library(ggplot2)

dados2 <- read.csv("pinguim.csv")
dados2
ggplot(dados2,aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(alpha = 0.6) + labs(x = "Comprimento da Asa (mm)", y = "Massa Corporal)",title = "Comprimento da Asa vs. Massa Corporal para Pinguins") + theme_minimal()

# b)
#utilize a função cor(), calcular correlação entre as váriaveis flipper_length_mm e body_mass_g
correlacao <- cor(dados2$flipper_length_mm, dados2$body_mass_g)
cat("Correlação entre flipper_length_mm e body_mass_g:", correlacao, "\n")

#c)
#A partir das duas respostas anteriores.
# Em geral, quanto maior o comprimento da asa, maior a massa corporal dos pinguins. No entanto, a relação não é extremamente forte, mas ainda é significativa.
# o coeficiente de correlação entre essas duas variáveis é positivo e está entre 0,5 e 0,7

#d)
#Utilize a função lm() para determinar a reta do modelo de regressão entre flipper_length_mm e body_mass_g
modelo <- lm(body_mass_g ~ flipper_length_mm, data = dados2)

cat("Coeficiente de inclinação (slope):", coef(modelo)[2], "\n")
cat("Coeficiente de interceptação (intercept):", coef(modelo)[1], "\n")

library(ggplot2)
ggplot(dados2, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Comprimento da Asa (mm)", y = "Massa Corporal (gramas)",
       title = "Regressão Linear: Comprimento da Asa vs. Massa Corporal") +
  theme_minimal()

# e)
#explique o coeficiente angular da reta encontrada em d
# o coeficiente angular indica quanto a massa corporal tende a aumentar (ou diminuir) quando o comprimento da asa aumenta em uma unidade. Se o coeficiente angular for positivo, significa que há uma relação positiva entre as duas variáveis: à medida que o comprimento da asa aumenta, a massa corporal também tende a aumentar. Se o coeficiente angular for negativo, a relação é inversa: à medida que o comprimento da asa aumenta, a massa corporal tende a diminuir.

#f)
# Usando a formula de (Peso = a . Asa + b)
# Podemos estimar o peso médio de um pinguim com a asa 204 mm substituindo a asa = 204
# Para estimar o peso médio de um pinguim com uma asa de 168 mm, também podemos usar o mesmo modelo, substituindo ( Asa = 168 ).

#g)
#Divida o conjunto de 3 outros conjunto de acordo com cada ilha
ilha_Torgersen <- subset(dados2, island == "Torgersen")
ilha_Biscoe <- subset(dados2, island == "Biscoe")
ilha_Dream <- subset(dados2, island == "Dream")

cat("Número de pinguins na ilha Torgersen:", nrow(ilha_Torgersen), "\n")
cat("Número de pinguins na ilha Biscoe:", nrow(ilha_Biscoe), "\n")
cat("Número de pinguins na ilha Dream:", nrow(ilha_Dream), "\n")

#h)
#Conjunto que contem apenas os pinguins ilha Biscoe, conjunto que contem apenas as femeas
pinguins_femeas_biscoe <- subset(ilha_Biscoe, sex == "FEMALE")
nrow(pinguins_femeas_biscoe)

library(cluster)
modelo <- hclust(dist(pinguins_femeas_biscoe[, c("flipper_length_mm", "body_mass_g")]), method = "ward.D2")


plot(modelo, main = "Dendrograma de Pinguins Fêmeas da Ilha Biscoe",
     xlab = "Índice do Pinguim", ylab = "Distância")

#i)
#Eu cortarai na altura da distancia de 2000, pois apartir dele temos 2 subgrupos grandes de maiores semelhanças, com distancias de mesmo indices mais proxímos.
pinguins_femeas_biscoe$species ##Temos pinguins das Especies Adelie e Gentoo
pinguis_adelie<- subset(pinguins_femeas_biscoe,species == "Adelie")
nrow(pinguis_adelie)
pinguis_gentoo<- subset(pinguins_femeas_biscoe,species == "Gentoo")
nrow(pinguis_gentoo)
prop <- (nrow(pinguis_adelie)/nrow(pinguis_gentoo))
prop ##Temos uma proporção de 0.3793

#j)
#Construir o modelo k-means com k= 2, k-means com k= 3
kmeans_Torgersen_3 <- kmeans(ilha_Torgersen[, c("flipper_length_mm", "body_mass_g")], centers = 3)
kmeans_Biscoe_3 <- kmeans(ilha_Biscoe[, c("flipper_length_mm", "body_mass_g")], centers = 3)
kmeans_Dream_3 <- kmeans(ilha_Dream[, c("flipper_length_mm", "body_mass_g")], centers = 3)

kmeans_Biscoe_3
kmeans_Dream_3
kmeans_Torgersen_3

# k)

# Plote o gráfico de dispersão dos dados com cada ponto colorido de acordo com seu cluster

plot(dados2$flipper_length_mm, dados2$body_mass_g, col = kmeans_Biscoe_3$cluster,
     main = "K-means Clustering (k=3)", xlab = "Flipper Length (mm)", ylab = "Body Mass (g)")

plot(dados2$flipper_length_mm, dados2$body_mass_g, col = kmeans_Dream_3$cluster,
     main = "K-means Clustering (k=3)", xlab = "Flipper Length (mm)", ylab = "Body Mass (g)")

plot(dados2$flipper_length_mm, dados2$body_mass_g, col = kmeans_Torgersen_3$cluster,
     main = "K-means Clustering (k=3)", xlab = "Flipper Length (mm)", ylab = "Body Mass (g)")

# acrescente a este grafico od centroides  de cada aglomerado.

# 1 - Extrair os centroides de cada cluster
centroids_Biscoe <- kmeans_Biscoe_3$centers
centroids_Dream <- kmeans_Dream_3$centers
centroids_Torgersen <- kmeans_Torgersen_3$centers


# 2 - Adicionar os centroides ao grafico
##Biscoe
points(centroids_Biscoe[,1], centroids_Biscoe[,2], col = 1:3, pch = 8, cex = 2)

legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Centroids"),
       col = c(1, 2, 3, "black"), pch = c(1, 1, 1, 8), inset = 0.05)
##Dream
points(centroids_Dream[,1], centroids_Dream[,2], col = 1:3, pch = 8, cex = 2)

legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Centroids"),
       col = c(1, 2, 3, "black"), pch = c(1, 1, 1, 8), inset = 0.05)
##Torgersen
points(centroids_Torgersen[,1], centroids_Torgersen[,2], col = 1:3, pch = 8, cex = 2)

legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Centroids"),
       col = c(1, 2, 3, "black"), pch = c(1, 1, 1, 8), inset = 0.05)

# 3)
dados <- read.table("olive.txt", header = TRUE, sep = ",")

dados_padronizados <- scale(dados[, -1])

matriz_distancia <- dist(dados_padronizados)

# a)
hc <- hclust(matriz_distancia, method = "ward.D2")

plot(hc, hang = -1, cex = 0.6, main = "Dendrograma - Hierarchical Clustering")

# b)
clusters <- cutree(hc, k = 5)
proporcoes_por_cluster <- lapply(1:5, function(i) {
  regioes_cluster <- dados$region[clusters == i]
  prop.table(table(regioes_cluster))
})

for (i in 1:5) {
  print(proporcoes_por_cluster[[i]])
}

# c)
set.seed(123)
kmeans_clusters <- kmeans(dados_padronizados, centers = 5)

proporcoes_por_cluster_kmeans <- lapply(1:5, function(i) {
  regioes_cluster <- dados$region[kmeans_clusters$cluster == i]
  prop.table(table(regioes_cluster))
})

for (i in 1:5) {
  print(proporcoes_por_cluster_kmeans[[i]])
}

# O K-means dividiu os dados em 5 grupos diferentes, cada um com uma quantidade diferente de azeites, mostrando que alguns grupos têm mais azeites que outros.
#alguns grupos têm valores médios mais altos para certos ácidos graxos, enquanto outros têm valores médios mais baixos.
# sabemos que grupo cada azeite pertence, o que nos ajuda a ver quais azeites são mais parecidos entre si.
# A soma das distâncias dentro de cada grupo nos diz o quão "apertado" e uniforme é o grupo. Quanto menor essa soma, mais "apertado" é o grupo.
# A quantidade de variância dos dados que é explicada pelos grupos nos ajuda a saber o quão bem os grupos representam os dados. Mais variância explicada significa grupos mais úteis.

# 4)
urna1_bolas <- c("preta", "preta", "preta", "preta", "preta", "preta", "branca", "branca", "branca", "vermelha", "vermelha", "vermelha",  "vermelha")
urna2_bolas <- c("preta", "preta", "preta", "branca", "branca", "branca", "branca", "branca", "vermelha", "vermelha")
urna3_bolas <- c("preta", "preta", "preta", "preta", "branca", "branca", "vermelha", "vermelha")

prob_urna1 <- 1/6
prob_urna2 <- 3/6
prob_urna3 <- 2/6

prob_vermelha_urna1 <- sum(urna1_bolas == "vermelha") / length(urna1_bolas)
prob_vermelha_urna2 <- sum(urna2_bolas == "vermelha") / length(urna2_bolas)
prob_vermelha_urna3 <- sum(urna3_bolas == "vermelha") / length(urna3_bolas)


prob_vermelha_total <- prob_urna1 * prob_vermelha_urna1 + prob_urna2 * prob_vermelha_urna2 + prob_urna3 * prob_vermelha_urna3

#probabilidade da bola retirada ser vermelha
print(prob_vermelha_total)


# 5)
sorteio <- function() {
  numeros <- 1:30
  contador <- 0
  
  while(length(numeros) > 0) {
    numero_sorteado <- sample(numeros, 1)
    numeros <- numeros[numeros != numero_sorteado]
    contador <- contador + 1
  }
  
  return(contador)
}

num_simulacoes <- 10000

resultados_simulacoes <- numeric(num_simulacoes)

for (i in 1:num_simulacoes) {
  resultados_simulacoes[i] <- sorteio()
}

mean(resultados_simulacoes)

#6)
#a)
lancamentos_ate4 <- function() {
  cont <- 0
  cont4 <- 0
  
  while (cont4 < 3) {
    lanca <- sample(1:6, 1)
    cont <- cont + 1
    
    if (lanca == 4) {
      cont4 <- cont4 + 1
    }
  }
  
  return(cont)
}

num_simulacoes <- 10000
results <- replicate(num_simulacoes, lancamentos_ate4())
val_esperado <- mean(results)
cat("Estimativa de valor para X:", val_esperado)

#b)

lancamentos_ate4 <- function() {
  cont <- 0
  cont4 <- 0
  
  while (cont4 < 3) {
    lanca <- sample(1:6, 1)
    cont <- cont + 1
    
    if (lanca == 4) {
      cont4 <- cont4 + 1
    }
  }
  
  return(cont)
}

num_simulacoes <- 10000
resultado <- replicate(num_simulacoes, lancamentos_ate4())

# Calcula a probabilidade de ser menor que 10
prop_menor10 <- sum(resultado < 10) / num_simulacoes
cat("Estimativa da probabilidade de resultado ser menor que 10:", prop_menor10)


