library(rvest)
library(dplyr)
library(ggplot2)

#vamos raspar os dados de crimes violentos por estado dos EUA na wikipedia; manipular os dados e plotar um mapa que apresentara a taxa de homicidio de cada estado a partir de uma escala de cor.

#1. raspando os dados
url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_violent_crime_rate#Rate_by_crime"

html <- read_html(url)

dados <- html |>
  html_element("table") |>
  html_table() |>
  as.data.frame()

#2. manipulando os dados
#mudando o nome de uma coluna que tinha um espaço em sua string
str(dados)
colnames(dados)[c(2,6)] <- c("violent.crime", "assault")

#o mapa de estados dos estados unidos pode ser acessado em state dentro da funcao map_data
?map_data
estados <- map_data("state")
estados

#mudando o nome dos estados para letras minusculas; isto é importante porque vamos juntar os conjuntos "dados"e "estados" pela "regio" e para fazer isto o nome dos estados precisam estar escritos da mesma forma

dados$region <- tolower(dados$Location)
dados <- dados[,-1]

dados_gerais <- left_join(estados, dados, by = "region")
str(dados_gerais)


ggplot(data = dados_gerais)+
  geom_polygon(aes(x = long, y = lat, group = group, fill = Homicide))+
  scale_fill_gradient(high = "#132B43",
                      low= "#56B1F7")+
  labs(title = "taxa de homicídio por estado")+
  theme_minimal()+
  theme(plot.title = element_text(family = "mono", hjust = 0.1, vjust = -6, face = "bold", size = 16),
        legend.title = element_text(family = "mono", vjust = 0.8),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

