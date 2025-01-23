library(jpeg) #biblioteca para importar a imagem para o R

imagem <- readJPEG("vangogh.jpg") #lendo a imagem
imagem
str(imagem) #conhencendo a estrutura da imagem

#vamos construir um data frame em que cada observação conterá informações sobre um pixel: vermelho, verde, azul e a posicao x e a posicao y do pixel na imagem para plotarmos, depois, a imagem clusterizada com ggplot

R <- as.vector(imagem[,,1]) #extraindo o vermelho
G <- as.vector(imagem[,,2]) #extraindo o verde
B <- as.vector(imagem[,,3]) #extraindo o azul

x <- rep(1:660, each = 800) #a partir da estrutura da imagem, construindo as coordenadas do eixo x dos pixels
y <- rep(800:1, times = 660) #a partir da estrutura da imagem, construindo as coordenadas do eixo y dos pixels

dados <- data.frame(x,y,R,G,B) #criando o data frame
dados[2,] #informacoes sobre o pixel da segunda linha


modelo <- kmeans(dados[,3:5], centers = 10) #clusterizando os pixels em 10 clusters com o Kmeans.
modelo$centers #retorna o centroide de cada cluster.
dados$aglomerados <- factor(modelo$cluster) #transformando para fator e adiocionando o aglomerado de cada pixel ao data frame para pintarmos cada cluster de acordo com seu aglomerado com o ggplot

#repintando a imagem com 10 cores.
ggplot(data = dados, aes(x,y, col = aglomerados))+
  geom_point()+
  scale_color_manual(values = rgb(modelo$centers))+
  coord_fixed()+
  theme_void()+
  theme(legend.position = "none")


