#o conjunto em marketing.txt, traz informacoes sobre o investimento de publicidade de um produto em diferentes plataformas e o numero de vendas (em milhares de unidades) deste produto para 200 empresas. 

#a ideia é construir um modelo que seja capaz de retornar o numero de vendas do produto em funcao dos investimentos no youtube, facebook, jornal.

marketing <- read.table("marketing.txt", sep = ";", header = TRUE)
str(marketing)

#embaralhar o conjunto, separar treino e teste e construir o modelo de regressão linear a partir do treinamento.
set.seed(123)
marketing <- marketing[sample(nrow(marketing)),]
N <- round(0.8*nrow(marketing))

treino <- marketing[1:N,]
teste <- marketing[-(1:N),]

str(treino)
cor(treino) #correlacao entre as variaveis do conjunto

modelo1 <- lm(data = treino, formula = sales ~ youtube)
modelo1 #este modelo apenas descreve as vendas a partir do investimento em youtube: vendas = 0.04724*youtube + 8.48286

summary(modelo1) #analisando a saída do modelo; observe que tanto o intercepto quanto o coeficiente do youtube foram testados em um teste de hipotese e deram diferentes de zero (rejeitou-se a hipotese nula); neste teste de hipotese, para cada constante, testou-se: H0: coeficiente eh zero versus HA: coeficiente nao eh zero; H0 siginifica hipotese nula e HA hipotese alternativa; rejeitamos H0 quando a coluna do p-valor (ou seja, Pr(>|t|)) for menos do que 0.05.

modelo2 <- lm(data = treino, formula = sales ~ .)
summary(modelo2) #neste modelo, iremos escrever as vendas em funcao de todas as outras variaveis

# sales = 3.65 + 0.05*youtube+ 0.19*facebook; repare que não colocamos o termo relacionado a newspaper porque o p-valor desta constante não foi menor do que 0.05 (foi igual a 0.414 que eh maior do que 0.05), portanto nao se rejeitou a hipotese nula (H0: o coeficiente eh zero); se ele é zero, entao zero vezes qualquer coisa continua zero e por isso ele nao entra no modelo2.

#assim, vamos fazer um modelo apenas considerando as vendas em funcao de youtube e facebook

modelo3 <- lm(data = treino, formula = sales ~ youtube + facebook)
summary(modelo3) #vendas = 3.5111 + 0.046*youtube + 0.185*facebook

#vamos analisar os erros do modelo; eles precisam ter distribuição normal
modelo3$residuals
hist(modelo3$residuals)

#teste de normalidade
#hipotese nula: os erros seguem uma distribuicao normal
#hipotese alternativa: os erros não seguem uma distribuição normal

shapiro.test(modelo3$residuals) #este teste de hipoteses é assim: H0: os erros tem distribuicao normal versus HA: os erros nao tem distribuicao normal; como o p-valor foi menor do que 0.05, rejeitamos a hipotese nula; entao o modelo nao satisfaz um de seus pressupostos. 
hist(x)
qqnorm(x)
qqline(x, col = "red")

qqnorm(modelo3$residuals)
qqline(modelo3$residuals)


#vamos tentar analisar um outro modelo agora, acrescentando uma coluna extra que é a raiz quadrado do youtube:
treino$sqrt_youtube <- sqrt(treino$youtube)

modelo4 <- lm(data = treino,
              formula = sales ~ facebook + sqrt_youtube)
summary(modelo4) #para este modelo, todas as constantes foram diferentes de zero e os residuos (erros) sao normais (p-valor do teste abaixo foi maior do que 0.05 e nao se rejeita a hipotese nula)
shapiro.test(modelo4$residuals)

teste$sqrt_youtube <- sqrt(teste$youtube)
previsao <- predict(modelo4, newdata = teste, interval = "prediction")
previsao #fazendo previsoes com o modelo a partir do teste; temos a previsao pontual e tambem temos um intervalo de confianca para o valor estimado; este intervalo eh da forma [lwr, upr].



comparacao <- data.frame(inferior = previsao[,2], superior = previsao[,3], teste$sales)
comparacao

#rpart
#rpart.plot
