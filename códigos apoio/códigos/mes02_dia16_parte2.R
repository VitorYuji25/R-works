#para esta parte da aula, vamos aprender um pouco sobre web scraping (raspagem de dados) com R.

library(rvest)
library(dplyr)

url <- "https://www.bbc.com/portuguese/articles/c80nj6re46ro"

html <- read_html(url) #lendo a url
html

html |>
  html_elements("h1") |>
  html_text2() #selecionando os elementos do tipo h1

html |>
  html_elements("a") |>
  html_text2() #imprimindo o texto dos elementos a


####
url_imdb <- "https://www.imdb.com/chart/top/"

html <- read_html(url_imdb)

html |>
  html_elements("ul.ipc-metadata-list") |>
  html_elements("li") |>
  html_text2()
html #raspando a tabela dos melhores filmes de acordo com o imdb
