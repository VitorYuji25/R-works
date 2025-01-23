# Grupo:
# - Maria Fernanda Nunes Gouveia (11921BCC042)
# - Vitor Yuji F. Matsushita (11921BCC021)

# Exercício 1. Considere o seguinte jogo: Steven e Garnit escolherão, cada um, uma sequência de tamanho 3
# em que cada entrada da sequência é cara ou coroa [...]

Steven <- c(0, 1, 0)
Garnit <- c(0, 0, 1)
Steven_ganhou <- 0
Garnit_ganhou <- 0
ocorrencias <- 0

while (ocorrencias < 10000) {
  resultados <- sample(0:1, size = 5, replace = TRUE)
  
  for (i in 1:(length(resultados) - length(Steven) + 1)) {
    if (all(resultados[i:(i + length(Steven) - 1)] == Steven)) {
      Steven_ganhou <- Steven_ganhou + 1
      break
    } else if (all(resultados[i:(i + length(Garnit) - 1)] == Garnit)) {
      Garnit_ganhou <- Garnit_ganhou + 1
      break
    }
  }
  
  ocorrencias <- ocorrencias + 1
}

media_vitorias_Garnit <- Garnit_ganhou / 10000
media_vitorias_Garnit
# ao simular as medias tivemos 0.3498 como resultado, indicando que
#Garnit vence em media cerca de 34.98% dos jogos em 10 mil partidas simuladas,
#enquanto Steven fica com o restante.


# Exercício 2. Harold Frederick Shipman (Nottingham, 14 de janeiro de 1946 — Wakefield, 13 de janeiro
#                                        de 2004), conhecido como “Doutor Morte”, foi um médico e assassino em série britânico condenado pela
# morte de muitos pacientes entre as décadas de 1970 e 1990. Dr. Shipman é, talvez, o assassino em série mais
# prolífico da História Moderna. O arquivo dados.txt contém informações sobre o sexo, a idade, o local da
# morte (casa do paciente; hospital; casa de repouso) e o ano da morte das vítimas de Shipman. Antes de
# responder as questões abaixo, abra o arquivo dados.txt e compreenda sua estrutura. Importe o arquivo
# para o R e utilize-o para responder os seguintes itens.

# (a) Escolha um gráfico apropriado para representar as frequências das categorias da variável sexo. Comente
# os resultados encontrados.

library(ggplot2)

dados <- read.csv("dados.txt", sep = ";")
# Gráfico de barras para representar as frequências das categorias da variável "sexo"
ggplot(dados, aes(x = Genero, fill = Genero)) +
  geom_bar() +
  labs(title = "Frequência das categorias da variável sexo",
       x = "Sexo",
       y = "Frequência") +
  scale_fill_manual(values = c("Women" = "#FF9999", "Men" = "#66CCFF")) +
  theme_minimal()
#podemos concluir desse grafico que a maioria das vitimas eram mulheres, podendo entender possiveis padroes do assassino.

# (b) Apresente o histograma da variável idade em 8 (argumento bins na geometria do histograma) intervalos.
# Comente os resultados obtidos. Analise este gráfico para cada gênero.

ggplot(dados, aes(x = Idade, fill = Genero)) +
  geom_histogram(bins = 8, position = "identity", alpha = 0.6) +
  labs(title = "Histograma da Idade por Gênero",
       x = "Idade",
       y = "Frequência") +
  scale_fill_manual(values = c("Women" = "#FF9999", "Men" = "#66CCFF")) +
  theme_minimal()

#nesse grafico vemos que a maioria das mulheres vitimas tem idades variadas,
#com algumas mais jovens e outras mais velhas, mostrando uma distribuição uniforme ao longo das idades.
# no caso dos homens, eles tendem a ser mais velhos, com menos casos de homens mais jovens e
#uma quantidade maior de homens mais velhos.

# (c) Apresente o boxplot da variável idade. Comente os resultados obtidos.
ggplot(dados, aes(x = Genero, y = Idade, fill = Genero)) +
  geom_boxplot() +
  labs(title = "Boxplot da Idade por Gênero",
       x = "Gênero",
       y = "Idade") +
  scale_fill_manual(values = c("Women" = "#FF9999", "Men" = "#66CCFF")) +
  theme_minimal()

#conseguimos analisar alguns pontos no boxplot:

#os pontos fora das caixas mostram valores que estão muito acima ou abaixo do intervalo interquartil.
#podemos observar esses pontos tanto para mulheres quanto pra homens,
#o que pode indicar que houve casos de vitimas muito mais jovens que o padrao.

#a linha dentro da caixa mostra a idade mediana das vitimas, enquanto a caixa em si mostra onde a
#maioria das idades está concentrada. entende-se que existe uma diferença nas idades das vitimias entre homens e mulheres,
# ou seja, as idades das vítimas tendem a variar dependendo do gênero.

# (d) Apresente um gráfico para representar o local da morte. Comente os resultados obtidos.
# Gráfico de barras para representar o local da morte
ggplot(dados, aes(x = LocalDaMorte, fill = LocalDaMorte)) +
  geom_bar() +
  labs(title = "Local da Morte das Vítimas de Shipman",
       x = "Local da Morte",
       y = "Frequência") +
  scale_fill_manual(values = c("#FF9999", "#66CCFF", "#99FF99")) +
  theme_minimal()

#a maioria das mortes aconteceu na propria casa da vitima,
#indicando que Shipman tinha uma tendencia a cometer os assassinatos nesse tipo de local,
#enquanto a casa de repouso foi a que menos teve mortes.

# (e) Analise graficamente o ano da morte das vítimas de Harold Shipman.

# Gráfico de barras do ano da morte
ggplot(dados, aes(x = factor(AnoDaMorte))) +
  geom_bar(fill = "#99CCFF") +
  labs(title = "Ano da Morte das Vítimas de Harold Shipman",
       x = "Ano",
       y = "Frequência") +
  theme_minimal()

#escolhemos o grafico de barras pra analisar o ano da morte das vitimas.

#analisando o grafico, conseguimos ver a quantidade de mortes pra cada ano separadamente e enxergar mais facil quais anos tiveram mais ou menos mortes.
#nesse caso, o ano de 1997 foi o que ocorreu mais mortes, porem o ano de 1995 e 1996 ficaram empatados em segundo lugar,
#enquanto o que teve menos mortes foi 1975/1992,
#1979 a 1983 tambem seguem como anos com tendencia menor a acontecer assassinatos.

# (f) Com base nas informações obtidas nos itens anteriores, escreva um parágrafo sobre o padrão e o perfil
# das vítimas de Harold Shipman.

#as vítimas de Harold Shipman eram principalmente mulheres, e elas tinham idades variadas,
#algumas mais jovens e outras mais velhas. Já os homens eram geralmente mais velhos.
#Shipman tendia a cometer os assassinatos na casa das vítimas.
#Ao longo dos anos, houve mais mortes entre 1995 e 1997, enquanto houve menos entre 1975 e 1992, com uma tendência menor de assassinatos entre 1979 e 1983.


# Exercício 3. treino_baleias.txt e teste_baleias.txt [...]
# (a) Crie um conjunto para cada espécie de baleia; cada data frame criado deverá conter apenas baleias de
# uma espécie.
treino <- read.csv("treino_baleias.txt", header = TRUE)
teste <- read.csv("teste_baleias.txt", header = TRUE)

cachalote <- subset(treino, especie == "Cachalote")
baleia_azul <- subset(treino, especie == "Baleia Azul")
baleia_fin <- subset(treino, especie == "Baleia Fin")
jubarte <- subset(treino, especie == "Jubarte")

print(cachalote)
print(baleia_azul)
print(baleia_fin)
print(jubarte)

# (b) Calcule a média, a variância, o desvio padrão e o coeficiente de variação para a variável peso para cada
# espécie de baleia. Comente os resultados obtidos.

# funcao pra calcular o coeficiente de variação
coeficiente_variacao <- function(x) {
  desvio_padrao <- sd(x)
  media <- mean(x)
  coef_var <- desvio_padrao / media * 100
  return(coef_var)
}

media_peso <- aggregate(peso ~ especie, treino, mean)
variancia_peso <- aggregate(peso ~ especie, treino, var)
desvio_padrao_peso <- aggregate(peso ~ especie, treino, sd)
coef_variacao_peso <- aggregate(peso ~ especie, treino, coeficiente_variacao)

# renomeando as colunas
colnames(media_peso) <- c("Especie", "Media_Peso")
colnames(variancia_peso) <- c("Especie", "Variancia_Peso")
colnames(desvio_padrao_peso) <- c("Especie", "Desvio_Padrao_Peso")
colnames(coef_variacao_peso) <- c("Especie", "Coeficiente_Variacao_Peso")

# p/ combinar os resultados em um data frame só
resultados <- merge(media_peso, variancia_peso, by = "Especie")
resultados <- merge(resultados, desvio_padrao_peso, by = "Especie")
resultados <- merge(resultados, coef_variacao_peso, by = "Especie")

print(resultados)

# os resultados que encontramos ajudam a entender como o peso de baleias de especies diferentes
#variam em relação a media. como por exemplo, as baleias azuis têm menos variação em peso,
# enquanto cachalotes têm uma variação maior.

# (c) Apresente o histograma da variável peso para a espécie de baleia azul. Comente os resultados obtidos.
# Filtrar os dados apenas para a espécie de baleia azul
baleia_azul <- treino[treino$especie == "Baleia Azul", ]

hist(baleia_azul$peso,
     main = "Histograma da variável Peso para a espécie de Baleia Azul",
     xlab = "Peso (quilos)",
     ylab = "Frequência",
     col = "skyblue",
     border = "black")

# O histograma mostra a distribuição dos pesos das baleias azuis.
# Podemos observar que a maioria das baleias tem pesos concentrados em torno de 20000 kg
# enquanto poucas baleias têm pesos muito mais altos ou mais baixos.
# Isso sugere que existe uma média/valor central para o peso das baleias azuis, com uma variação em torno desse valor.

# (d) Apresente numa mesma janela os boxplots para cada espécie para a variável comprimento. Comente
# os resultados obtidos.

boxplot(comprimento ~ especie,
        data = treino,
        main = "Boxplot do Comprimento para as Espécies de Baleia",
        xlab = "Espécie de Baleia",
        ylab = "Comprimento (metros)",
        col = c("skyblue", "lightgreen", "pink", "orange"))

# ada caixa no boxplot representa a distribuição dos comprimentos, sendo a linha no meio a mediana.
# conseguimos ver que a mediana varia de acordo com cada especie,
#a jubarte tem media entre 15 e 20m, a cachalote 20m, a baleia fin 25m e a baleia azul 30m
# as caixas tambem representam onde a maioria dos comprimentos são concentrados, enquanto
# as "linhas" se estendem pra indicar a variação dos dados.
# podemos observar que cada espécie de baleia tem uma distribuição diferente

# (e) Apresente um gráfico de dispersão de comprimento versus profundidade_maxima. Cada espécie deve
# ser registrada por uma cor diferente.
plot(profundidade_maxima ~ comprimento,
     data = treino,
     col = as.numeric(factor(treino$especie)),
     pch = 16,
     main = "Dispersão de Comprimento vs Profundidade Máxima por Espécie",
     xlab = "Comprimento (metros)",
     ylab = "Profundidade Máxima (metros)")
grid()

# (f) Com base em todas as informações anteriores, construa um modelo de árvore de decisão a partir de
# estruturas condicionais e de repetição para prever a espécie de uma baleia com base nas variáveis
# numéricas do estudo. Justifique as escolhas das variáveis e dos pontos de corte escolhidos. Por fim,
# utilize o conjunto do arquivo teste_baleias.txt para calcular a taxa de acerto. Comente o resultado
# obtido.

library("rpart")
library("ROCR")

print(treino)


library(rpart)
library(rpart.plot)

modelo_arvore <- rpart( especie ~ peso + comprimento + profundidade_maxima + volume_cranio ,data = treino)
rpart.plot(modelo_arvore, type = 4, extra = 101)

previsoes <- predict(modelo_arvore, newdata = treino, type = "class")
taxa_acerto <- mean(previsoes == treino$especie)
taxa_acerto

# (g) Utilize gráficos de dispersão para registrar por linhas horizontais e verticais os pontos de cortes escolhidos em sua árvore de decisão. As espécies de baleias devem ser registradas por diferentes cores.
# Calcular estatísticas para a variável peso para cada espécie de baleia

print(treino)

grafico <- ggplot(data = treino,aes(x = profundidade_maxima,y = peso,color = previsoes)) + geom_point() + geom_hline(yintercept = 8481,linetype = "dashed") + geom_vline(xintercept = 168,linetype = "dashed") + labs(title = "Grafico de Dispersão" , X = treino$profundidade_maxima, Y = treino$peso, color = "especie predita") + theme_minimal()

grafico


# (h) Crie um modelo de classificação KNN para classificar as baleias. Utilize 𝐾 = 1 e depois 𝐾 = 3.
# Compare os resultados dos dois modelos KNN.

library(class)

dados_modelo <- c("comprimento", "peso", "profundidade_maxima", "volume_cranio")

resposta <- "especie"
resposta

#modelo knn com k = 1

modelok1 <- knn(train = treino[, dados_modelo],
                test = teste[, dados_modelo],
                cl = treino[, resposta],
                k = 1)
#modelo com knn com  = 3
modelok3 <- knn(train = treino[, dados_modelo],
                test = teste[, dados_modelo],
                cl= treino[, resposta],
                k = 3)
#comparando os resultados dos dois modelos
resultado <- data.frame(Real = teste$especie, k1 = modelok1, k3 = modelok3)
resultado


# Exercício 4. cogumelos.csv

library(ggplot2)

cogumelos <- read.csv("cogumelos.csv")

# embaralhar os dados
cogumelos <- cogumelos[sample(nrow(cogumelos)), ]

# separar em treinamento (80%) e teste (20%)
n <- nrow(cogumelos)
i <- round(0.8 * n)

dados_treino <- cogumelos[1:i, ]
dados_teste <- cogumelos[(i + 1):n, ]

# Quantidade de cogumelos comestíveis e venenosos no conjunto de treinamento

ggplot(dados_treino, aes(x = class, fill = class)) +
  geom_bar() +
  labs(title = "Quantidade de cogumelos comestíveis e venenosos",
       x = "Classe",
       y = "Frequência") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# Distribuição da forma do cogumelo entre as espécies comestíveis e venenosas
ggplot(dados_treino, aes(x = class, fill = cap.shape)) +
  geom_bar(position = "fill") +
  labs(title = "Distribuição da forma do cogumelo",
       x = "Classe",
       y = "Proporção",
       fill = "Forma") +
  theme_minimal()

# Distribuição das cores do chapéu entre as espécies comestíveis e venenosas
ggplot(dados_treino, aes(x = class, fill = cap.color)) +
  geom_bar(position = "fill") +
  labs(title = "Distribuição das cores do chapéu",
       x = "Classe",
       y = "Proporção",
       fill = "Cor") +
  theme_minimal()

# Relação entre odor e classe do cogumelo
ggplot(dados_treino, aes(x = odor, fill = class)) +
  geom_bar() +
  labs(title = "Relação entre odor e classe do cogumelo",
       x = "Odor",
       y = "Frequência",
       fill = "Classe") +
  theme_minimal()

# Modelo de árvore de decisão
library(rpart)
library(rpart.plot)

modelo_arvore <- rpart(class ~ ., data = dados_treino, method = "class")
rpart.plot(modelo_arvore, type = 4, extra = 101)

# Avaliar a taxa de acerto do modelo com os dados de teste
previsoes <- predict(modelo_arvore, newdata = dados_teste, type = "class")
taxa_acerto <- mean(previsoes == dados_teste$class)
taxa_acerto

# analise: é um modelo efetivo por ser uma porcentagem de acerto tao alta

