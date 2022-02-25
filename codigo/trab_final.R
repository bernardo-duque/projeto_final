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
########   2.1 -   dsds ########
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
  mutate(id_linha = row_number(), n=1) %>%
  filter(categoria_discursante == "senador") 

# setando seed para permitir reproducibilidade

set.seed(23022022)

# criando amostra para treinar o modelo de ML e gerando xlsx 

discurso_manual <- sample_n(discurso,size = 500,replace = F) %>%
  select(id_linha,nome_discursante,texto_discurso)

write_xlsx(discurso_manual, path = "discurso_manual.xlsx")


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
  select(id_linha,sequencial_sessao:sinalizacao_fora_microfone,n) %>%
  left_join(candidato_filt, by = c("nome_discursante"="nome_urna"))

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

#discurso_token %>% count(palavras,sort=T)

####----1.2.5) Correção de typos e stemming----

# pegando apenas os radicais das palavras, para perder menos informacao

discurso_token <- discurso_token %>%
  mutate(palavras = wordStem(palavras,language = "portuguese" ))

####----1.3) Retirando palavras com baixa frequencia----

check <- discurso_token %>% 
  count(palavras) %>%
  arrange(n)

# vendo as palavras mais frequentes

quantile(check$n,probs = seq(0,1,0.01))

# 51% aparece 2 vezes ou menos, filtrando para esse numero (questao de memoria de processamento)
# pegando apenas os radicais que aparecem nos ultimos 21% (a partir de 21 mencoes)

check <- discurso_token %>% 
  count(palavras) %>%
  arrange(n) %>%
  filter(n<21) %>%
  select(palavras)

discurso_token <- discurso_token %>%
  anti_join(check)  #retirando as palavras com baixa frequencia

length(unique(discurso_token$palavras))

# # retirando linhas repetidas com a agregacao por radicais
# 
# discurso_token <- discurso_token[duplicated(discurso_token)==F,]
  

####----1.4) Vetorizando os discursos----

# trocando os nomes das colunas que tem palavras contidas no discurso

discurso_token <- discurso_token %>% 
  mutate(ocupacao_df = ocupacao, idade_df = idade,
         instrucao_df = instrucao, raca_df = raca)%>% 
  select(-c(ocupacao,idade,instrucao,raca)) 

# salvando a base de discurso e removendo o restante do workspace por uma questão de memória

#save(discurso)
rm(candidato_filt, candidatos,discurso_manual, discurso, check)

# vetorizando os discursos

discurso_token <- discurso_token %>%
  pivot_wider(names_from = palavras, values_from = n, values_fn = sum,values_fill = 0)

col <- length(discurso_token)


#### 2 - Machine Learning #####

# lendo a amostra preenchida

resul <- read_xlsx("discurso_manual_preenchido.csv")

codigos <- resul$id_linha 

####----2.2) Definindo train e test datasets----

set.seed(24022022)

classes <- c("favor","contra","neutro") # discurso favoravel, contrario ou neutro em relacao à conduta da pandemia

discurso_modelo <- foreach(class=classes,.packages = "caret",.verbose = TRUE,.errorhandling = "pass") %do% {
  
  resul_aux <- resul[,c("id_linha",class)]
  
  amostra <- discurso_token %>%
    filter(id_linha %in% codigos) %>%
    left_join(resul_aux)
  
  ## Ordena validação
  
  amostra <- amostra[order(amostra$id_linha),]
  
  n_obs <- nrow(amostra)
  
  
  # Misturando os indices para ficar aleatorio
  amostra_aleatoria <- sample(n_obs)
  
  # Ordenando amostra pelos indices aleatorios
  amostra <- amostra[amostra_aleatoria,]
  
  # Pegando numero de colunas para dividir em 65/35 a amostra em train e teste
  divisao <- round(n_obs * 0.65)
  
  # Criando train
  treino <- amostra[1:divisao,]
  
  # Criando teste
  teste <- amostra[(divisao+1):n_obs, ]
  
  list(treino,teste)
  
}

####----2.3) Métricas de validação----


modelos <- list()

tictoc::tic("fitando o modelo")
modelos <- foreach(class=classes,.packages = "caret",.verbose = TRUE,.errorhandling = "remove") %do% {
  
  # bota a variavel dependente como coluna 1 e restante das colunas apenas as variaveis que vao entrar no modelo
  
  i <- which(classes==class)
  treino_modelo <- dd_modelo[[i]][[1]] 
  a <- which(colnames(treino_modelo)==class)
  treino_modelo <- treino_modelo[,c(a,17:col)]
  colnames(treino_modelo)[1] <- "y"
  
  # criando folds para  cross-validation ser igual para os modelos de random forest e xgbnet
  
  myFolds <- createFolds(y = treino_modelo$y,k=5)
  myControl <- trainControl(
    method = "cv", 
    number = 5, 
    index = myFolds,
    verboseIter = TRUE
  )
  
  # treina o modelo de random forest (ranger) testando 10 hiperparâmetros diferentes em 5 folds da cross-validation
  
  model_rf <- train(
    as.factor(y) ~.,
    tuneLength = 10,
    data = treino_modelo, 
    method = "ranger",
    importance  = "impurity",
    trControl = myControl,
    verbose=TRUE
  )
  
  model_xgb <- train(
    as.factor(y) ~.,
    data = treino_modelo, 
    method = "xgbTree",
    trControl = myControl,
    verbose=TRUE
  )
  
  list(model_rf,model_xgb,class)
}
toc() #1h26min

##### 3 - Estatísticas Descritivas dos Participantes #####

##### 1.3 - Dados Covid #####