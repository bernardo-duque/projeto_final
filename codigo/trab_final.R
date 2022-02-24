###############################################################################
###############################################################################
######## SCRIPT PARA ANALISAR OS DISCURSOS DA CPI DA COVID             ########
########                                                               ########  
######## 1 - Importação e limpeza das bases                            ########
########                                                               ########
########   1.1 - Limpando as strings e tratando base dos discursos     ########
########       e dos candidatos                                        ########        
########   1.2 -                         ########
########   1.3 -   ########
########   1.4 -                  ########
########   1.5 -                               ########
########                                                               ######## 
######## 2- Gráficos                                         ######## 
########   2.1 -    ########
########   2.2 -          ########
########   2.3 -                                                ######## 
########                                                               ######## 
###############################################################################


setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//Input")

pacman::p_load(sf, tidyverse, geobr, ggplot2, terra, spData, stringr, caret,
               stopwords, tictoc, foreach, SnowballC, hunspell, quanteda, readxl,writexl,
               tidytext)

##### 1. Importando e limpando as bases relacionadas aos candidatos #####

## Importando as bases

discurso <- read.csv("discursos.csv", encoding="UTF-8") # encoding veio esquisito

candidatos <- read.csv("candidatos.csv")

casos <- read.csv("caso.csv")

##### 1.1 Limpando as strings e juntando a base dos discursos e dos candidatos #####

## Criando funcao para padronizar strings (texto maisculo e sem acento)

arrumar <- function(vec){
  vec <- toupper(vec)
  vec <- gsub("Á","A",vec)
  vec <- gsub("Ã","A",vec)
  vec <- gsub("À","A",vec)
  vec <- gsub("Â","A",vec)
  vec <- gsub("É","E",vec)
  vec <- gsub("È","E",vec)
  vec <- gsub("Ê","E",vec)
  vec <- gsub("Í","I",vec)
  vec <- gsub("Ì","I",vec)
  vec <- gsub("Ó","O",vec)
  vec <- gsub("Õ","O",vec)
  vec <- gsub("Ò","O",vec)
  vec <- gsub("Ô","O",vec)
  vec <- gsub("Ú","U",vec)
  vec <- gsub("Ù","U",vec)
  vec <- gsub("Ç","C",vec)
  return(vec)
}

# usando a funcao nas colunas relevantes 

discurso <- discurso %>%
  mutate(texto_discurso = arrumar(texto_discurso),
         nome_discursante = arrumar(nome_discursante),
         categoria_discursante = ifelse(categoria_discursante == "Senador/a","senador",categoria_discursante))

candidatos <- candidatos %>%
  mutate(nome = arrumar(nome),
         nome_urna = arrumar(nome_urna))

## Filtrando a base de candidatos pelos deputados que participaram da cpi

# desconsiderando depoentes 

discurso <- discurso %>%
  filter(categoria_discursante == "senador")

# pegando o vetor de nomes 

deputados <- unique(discurso$nome_discursante)

# pegando as informacoes dos discursantes para os mandatos validos hoje

candidato_filt <- candidatos %>%
  filter(nome_urna %in% deputados,
         cargo %in% c("senador","1Âº suplente senador","2Âº suplente senador"),
         ano >= 2014) %>% 
  select(nome_urna,ano,sigla_uf,sigla_partido,numero_partido,ocupacao,idade,instrucao,estado_civil,raca)

# selecionando variaveis e dando join

discurso <- discurso %>%
  select(sequencial_sessao:sinalizacao_fora_microfone) %>%
  left_join(candidato_filt, by = c("nome_discursante"="nome_urna"))

## Criando amostra para treinar o modelo de classificação de ML

# setando seed para permitir reproducibilidade

set.seed(23022022)

# criando a amostra e gerando xlsx 

discurso_manual <- sample_n(discurso,size = 200,replace = F)

write_xlsx(discurso_manual, path = "discurso_manual.xlsx")


####----1.2) Tokenizando e removendo stop words----

# removendo stop words

stop_vf <- stopwords(language = "pt",source = "stopwords-iso") 

stop_vf <- as.data.frame(stop_vf)

colnames(stop_vf) <- "palavras"

# quebrando as palavras por linha

discurso_token <- discurso %>%
  unnest_tokens(output = "palavras",input = texto_discurso, token = "words") 

discurso_token <- discurso_token %>% 
  anti_join(stop_vf) #retira as stop words

# retirando nomes de pessoas

nomes <- read.csv("nomes.csv")

nomes <- unique(nomes$group_name) %>%
  toupper() %>%
  as.data.frame()

names(nomes) <- "palavras"

discurso_token <- discurso_token %>% 
  anti_join(nomes) 

discurso_token %>% count(palavras,sort=T) #vendo casos de palavras que possuem frequencia muito alta, mas que nao provem informacao util

####----1.2.5) Correção de typos e stemming----

# pegando apenas os radicais das palavras, para diminuir o numero de variaveis

# discurso_token_stem <- discurso_token %>%
#   mutate(palavras = wordStem(palavras,language = "portuguese" ))
  

####----1.3) Vetorizando os discursos----

#transformando as palavras em colunas

discurso_token_col <- discurso_token %>% 
  mutate(ocupacao_df = ocupacao, idade_df = idade,
         instrucao_df = instrucao, raca_df = raca,
         row = row_number(), n=1)%>% # trocando os nomes das colunas que tem palavras contidas no discurso
  select(-c(ocupacao,idade,instrucao,raca)) 

discurso_token_col <- discurso_token_col %>%
  pivot_wider(names_from = palavras, values_from = n,values_fn = list)

col <- length(discurso_token)
discurso_token[, 17:col][is.na(discurso_token[, 17:col])] <- 0 #transformando os NAs em 0

#save(discurso_token,file="checkpoint_3 - 05.24.21.RData")  #salvando para nao ter que rodar novamente a parte de cima
#load("checkpoint_3 - 05.24.21.RData")
col <- length(discurso_token)

##### 1.2 - Estatísticas Descritivas dos Participantes #####

##### 1.3 - Dados Covid #####