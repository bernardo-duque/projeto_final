###############################################################################
###############################################################################
######## SCRIPT PARA ANALISAR OS DISCURSOS DA CPI DA COVID             ########
########                                                               ########  
######## 1 - Importação e limpeza das bases                            ########
########                                                               ########
########   1.1 - Limpando as strings e juntando a base dos discursos   ########
########         e dos candidatos                                      ########
########   1.2 - Tokenizando e removendo stop words                    ########
########   1.3 - Retirando palavras com baixa frequencia               ########
########   1.4 - Vetorizando os discursos                              ########
########                                                               ######## 
######## 2- Machine Learning                                           ######## 
########   2.1 - Definindo train e test datasets                       ########
########   2.2 - Métricas de validação                                 ########                                  
########                                                               ######## 
###############################################################################


setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//input")

pacman::p_load(sf, tidyverse, geobr, ggplot2, terra, spData, stringr, caret,
               stopwords, tictoc, foreach, SnowballC, hunspell, quanteda, readxl,writexl,
               tidytext, RColorBrewer, tm, wordcloud, ranger, xgboost, grDevices, reshape2,
               highcharter, webshot, htmlwidgets, kableExtra, magick, htmltools)

##### 1. Importando e limpando as bases relacionadas aos candidatos #####

## Importando as bases

discurso <- read.csv("discursos.csv", encoding="UTF-8") # encoding veio esquisito

candidatos <- read.csv("candidatos.csv")

casos <- read.csv("caso.csv")

####----1.1 Limpando as strings e juntando a base dos discursos e dos candidatos #####

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

discurso_manual <- sample_n(discurso,size = 1000,replace = F) %>%
  select(id_linha,nome_discursante,texto_discurso)

#write_xlsx(discurso_manual, path = "discurso_manual.xlsx")


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

stop_vf <- stopwords::stopwords(language = "pt",source = "stopwords-iso") 

stop_vf <- as.data.frame(stop_vf)

colnames(stop_vf) <- "palavras"

# quebrando as palavras por linha

discurso_ml <- discurso %>%
  unnest_tokens(output = "palavras",input = texto_discurso, token = "words") 

discurso_ml <- discurso_ml %>% 
  anti_join(stop_vf) #retira as stop words

# criando base com palavras agrupadas por frequencia para wordcloud

discurso_token <- discurso_ml %>%
  mutate(n=1) %>%
  group_by(palavras) %>%
  summarise(n=sum(n))

setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//output")

grDevices::png(filename = "wordcloud_cpi.png",width = 300,height = 300)

wordcloud::wordcloud(discurso_token$palavras, discurso_token$n, max.words = 50, min.freq = 1, 
          random.order=FALSE, colors=brewer.pal(6,"Dark2"), random.color=TRUE)

dev.off()

# salvando para gerar no rmd
save(discurso_token,file = "wc_1.rda")

setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//input")

## Stemming 

# pegando apenas os radicais das palavras, para perder menos informacao

discurso_ml <- discurso_ml %>%
  mutate(palavras = wordStem(palavras,language = "portuguese" ))

# repetindo wordcloud com stems

discurso_token <- discurso_ml # vai ser usado na frente nas estatisticas descritivas

palavras <- discurso_ml %>%
  mutate(n=1) %>%
  group_by(palavras) %>%
  summarise(n=sum(n))

setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//output")

grDevices::png(filename = "wordcloud_cpi_stem.png",width = 300,height = 300)

wordcloud::wordcloud(palavras$palavras, palavras$n, max.words = 50, min.freq = 1, 
          random.order=FALSE, colors=brewer.pal(6,"Dark2"), random.color=TRUE)

dev.off()

# salvando para gerar no rmd
save(palavras,file = "wc_2.rda")

rm(palavras)

setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//input")

####----1.3) Retirando palavras com baixa frequencia----

check <- discurso_ml %>% 
  count(palavras) %>%
  arrange(n)

# vendo as palavras mais frequentes

quantile(check$n,probs = seq(0,1,0.01))

# pegando apenas os radicais que aparecem nos ultimos 21% (a partir de 21 mencoes)

check <- discurso_ml %>% 
  count(palavras) %>%
  arrange(n) %>%
  filter(n<21) %>%
  select(palavras)

discurso_ml <- discurso_ml %>%
  anti_join(check)  #retirando as palavras com baixa frequencia

length(unique(discurso_ml$palavras))

# # retirando linhas repetidas com a agregacao por radicais
# 
# discurso_ml <- discurso_ml[duplicated(discurso_ml)==F,]
  

####----1.4) Vetorizando os discursos----

# trocando os nomes das colunas que tem palavras contidas no discurso

discurso_ml <- discurso_ml %>% 
  mutate(ocupacao_df = ocupacao, idade_df = idade,
         instrucao_df = instrucao, raca_df = raca)%>% 
  select(-c(ocupacao,idade,instrucao,raca)) 

# salvando a base de discurso e removendo o restante do workspace por uma questão de memória

#save(discurso)
rm(candidato_filt, candidatos,discurso_manual, discurso, check)

# vetorizando os discursos

discurso_ml <- discurso_ml %>%
  pivot_wider(names_from = palavras, values_from = n, values_fn = sum,values_fill = 0)

col <- length(discurso_ml)


#### 2 - Machine Learning #####

# lendo a amostra preenchida

resul <- read_xlsx("discurso_manual_preenchido.xlsx")

# pegando os codigos dos discursos

codigos <- resul %>%
  select(id_linha) %>%
  pull

####----2.1) Definindo train e test datasets----

set.seed(24022022)

# pegando o numero da coluna anterior às palavras dos discursos

id_col <- which(colnames(discurso_ml)=="raca_df")

# definindo a variavel a ser predita

class <- "classificacao_df"

# pegando apenas o codigo e a variavel

resul_aux <- resul[,c("id_linha",class)]
  
# criando amostra

amostra <- discurso_ml %>%
  filter(id_linha %in% codigos) %>%
  left_join(resul_aux)
  
# ordena classificação manual
  
amostra <- amostra[order(amostra$id_linha),]
  
n_obs <- nrow(amostra)
  
# misturando os indices para ficar aleatorio

amostra_aleatoria <- sample(n_obs)
  
# ordenando amostra pelos indices aleatorios

amostra <- amostra[amostra_aleatoria,]
  
# pegando numero de colunas para dividir em 65/35 a amostra em train e teste

divisao <- round(n_obs * 0.65)
  

# Criando train

treino <- amostra[1:divisao,]
  
# Criando teste

teste <- amostra[(divisao+1):n_obs, ]
  

####----2.2) Métricas de validação----####


# modelos <- list()

tictoc::tic("fitando o modelo")
  
# bota a variavel dependente como coluna 1 e restante das colunas apenas as variaveis que vao entrar no modelo
  
i <- 1
treino_modelo <- treino
a <- which(colnames(treino_modelo)==class)
treino_modelo <- treino_modelo[,c(a,(id_col + 1):col)]
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
  
toc() #5min
  
# faz o predict do modelo na amostra de teste e cria as matrizes de confusao
  
p_rf <- predict(model_rf,teste[,(id_col + 1):col])
p_xgb <- predict(model_xgb,teste[,(id_col + 1):col])
  
teste[[class]] <- as.factor(teste[[class]])
  
matriz_rf <- confusionMatrix(p_rf,teste[[class]],mode="prec_recall")
matriz_rf <- as.data.frame(matriz_rf$byClass)
  
matriz_xgb <- confusionMatrix(p_xgb,teste[[class]],mode="prec_recall")
matriz_xgb <- as.data.frame(matriz_xgb$byClass)
  
# pegando o modelo com maior F1
  
print(matriz_rf[7])
print(matriz_xgb[7])
  
# modelo xgb esta melhor especificado
  
modelo <- model_xgb
matriz <- round(matriz_xgb,3)
  
rm(model_xgb,model_rf,matriz_rf,matriz_xgb,resul_aux, amostra,myControl,myFolds,resul,teste,treino,treino_modelo)

# predizendo o modelo
  
discurso_ml$classificacao_df <- predict(modelo,discurso_ml[,(id_col + 1):col])

discurso <- discurso_ml %>% 
  select(id_linha:raca_df,classificacao_df)

rm(discurso_ml)

# exportando a base e salvando a confusion matrix

setwd("C://Users//duque//Desktop//PUC//Mestrado//Verão 1//Estatística//Data Science//Trabalho Final//output")

write.csv(discurso,"discurso_cpi_classificado.csv")

matriz <- matriz %>% 
  select(Precision:Prevalence)

save(matriz, file = "matriz.rda")

## Formatando tabela da matriz de confusão

matriz_form <- matriz %>% 
  kable() %>%
  kable_classic(full_width = T, html_font = "Cambria",font_size=12) %>%
  column_spec(1, bold = T) %>%
  row_spec(0,bold=T) 

save_kable(matriz_form,file = "matriz_form.png")
  
##### 3 - Estatísticas Descritivas #####

# transformando a classificacao em dummies

discurso <- discurso %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = classificacao_df,values_from = n, values_fill = 0)

# criando as estatisticas para senadores e para o total

estatisticas_senadores <- discurso %>%
  group_by(nome_discursante, sigla_partido.x,sigla_uf) %>%
  summarise(total = n(),
            favor = sum(`A favor`),
            contra = sum(Contra),
            favor_perc = round(favor/(favor + contra),2),
            contra_perc = round(contra/(favor + contra),2))

estatisticas_gerais <- discurso %>%
  summarise(total = n(),
            favor = sum(`A favor`),
            contra = sum(Contra),
            favor_perc = round(favor/(favor + contra),2),
            contra_perc = round(contra/(favor + contra),2),
            neutro_perc = round((favor+contra)/total,2))

save(estatisticas_gerais,file="estatisticas_gerais.rda")
save(estatisticas_senadores,file = "estatisticas_senadores.rda")

####---- 3.1 Número de discursos totais gerais ----####

# Números absolutos

grDevices::png(filename = "absolutos_gerais.png",width = 600,height = 600)

estatisticas_gerais_t <- estatisticas_gerais %>%
  pivot_longer(cols = total:neutro_perc,values_to = "n",names_to = "valores") 

a <- estatisticas_gerais_t %>%
  filter(valores %in% c("total","favor","contra")) %>%
  mutate(value = ifelse(valores=="total","Total",
                        ifelse(valores == "favor","A Favor","Contra")))

a %>%
  ggplot(aes(x = value,y = n),
         alpha = 0.9, col="white") +
  geom_bar(stat='identity',fill = "darkblue",width = 0.5) +
  labs(title = "Total de Discursos",x="",y="") +
  theme(plot.title = element_text(face = "bold"), legend.title = element_blank()) + 
  geom_text(aes(label = n), vjust = 2, size = 3, color = "#ffffff") + 
  theme_bw()

dev.off()

# Proporção

grDevices::png(filename = "proporcao_geral.png",width = 600,height = 600)

a <- estatisticas_gerais_t%>%
  mutate(total="discurso", posicao = ifelse(valores == "favor_perc","Favor",
                                            ifelse(valores == "contra_perc","Contra",valores))) %>%
  filter(posicao %in% c("Favor","Contra"))

a %>%
  ggplot(aes(fill=posicao, y=n, x=total)) + 
  geom_bar(position="fill", stat="identity", width = 0.5) +
  scale_fill_manual(values = c("#cc0000","darkgreen")) +
  labs(title = "Proporção de Discursos Favoráveis e Contra",x="",y="") +
  theme(plot.title = element_text(face = "bold"), legend.title = element_blank()) + 
  geom_text(aes(label = n), vjust = 2, size = 3, color = "#ffffff") + 
  theme_bw() + coord_flip()

dev.off()

####---- 3.2 Numeros de discursos totais por senador ----####

## Criando as tabelas  

# mais discursos
  
grDevices::png(filename = "mais_discursos.png",width = 600,height = 600)

a <- estatisticas_senadores %>%
  arrange(desc(total))

a <- a[1:5,]
a$nome_discursante <- c("O.Aziz","R.Calheiros","Randolfe","M.Rogerio","Elizane")

a %>%
  ggplot(aes(x = nome_discursante,y = total),
         alpha = 0.9, col="white") +
  geom_bar(stat='identity',fill = "darkblue",width = 0.5) +
  labs(title = "Total de Discursos por Senador",subtitle = "Top 5 senadores",x="",y="") +
  theme(plot.title = element_text(face = "bold"), legend.title = element_blank()) + 
  geom_text(aes(label = total), vjust = 2, size = 3, color = "#ffffff")

dev.off()

# mais discursos favoraveis 

grDevices::png(filename = "mais_favor.png",width = 1000,height = 800)

a <- estatisticas_senadores %>%
  arrange(desc(favor))

a <- a[1:5,]
a$nome_discursante <- c("E.Girão","M.Rogério","O.Aziz","F.Bezerra","Randolfe")

a %>%
  ggplot(aes(x = nome_discursante,y = favor),
         alpha = 0.9, col="white") +
  geom_bar(stat='identity',fill = "darkgreen",width = 0.5) +
  labs(title = "Total de Discursos a Favor do Governo por Senador",subtitle = "Top 5 senadores",x="",y="") +
  theme(plot.title = element_text(face = "bold"), legend.title = element_blank()) + 
  geom_text(aes(label = favor), vjust = 2, size = 3, color = "#ffffff")

dev.off()

grDevices::png(filename = "mais_contra.png",width = 1000,height = 800)

# mais discursos contra 

a <- estatisticas_senadores %>%
  arrange(desc(contra))

a <- a[1:5,]
a$nome_discursante <- c("R.Calheiros","O.Aziz","Randolfe","Humberto","R.Carvalho")

a %>%
  ggplot(aes(x = nome_discursante,y = contra),
         alpha = 0.9, col="white") +
  geom_bar(stat='identity',fill = "#cc0000",width = 0.5) +
  labs(title = "Total de Discursos Contra o Governo por Senador",subtitle = "Top 5 senadores",x="",y="") +
  theme(plot.title = element_text(face = "bold"), legend.title = element_blank()) + 
  geom_text(aes(label = contra), vjust = 2, size = 3, color = "#ffffff")

dev.off()

####---- 3.2 Distribuição por estado, genero e partido ----####

## estatisticas senadores

senadores <- discurso %>%
  group_by(nome_discursante,sigla_partido.x,sigla_uf_partido,genero_discursante,idade_df,raca_df) %>%
  summarise(across(Neutro:`A favor`,sum))

# salva a base dos senadores 

write.csv(senadores,file = "senadores.csv")

idade <- c(round(base::mean(senadores$idade_df,na.rm=T),1),"","","", "")

genero <- senadores %>%
  mutate(row=row_number(),n = 1,genero_discursante = ifelse(is.na(genero_discursante),"N/A",genero_discursante)) %>%
  select(row,everything())
genero <- genero %>%
  pivot_wider(id_cols = row,values_from = n,names_from = genero_discursante,values_fn = sum) %>%
  mutate(Total = Masculino + Feminino) %>%
  select(-row)

raca <- senadores %>%
  mutate(row=row_number(),n = 1,raca_df = ifelse(is.na(raca_df),"N/A",raca_df)) %>%
  select(row,everything())
raca <- raca %>%
  pivot_wider(id_cols = row,values_from = n,names_from = raca_df,values_fn = sum) %>%
  mutate(Total = branca + parda + preta + `N/A`) %>%
  select(-row)

genero_col <- colnames(genero) %>%
  rbind(genero)
genero_col <- t(genero_col) %>%
  rbind(c("","")) %>%
  rbind(c("",""))

raca_col <- colnames(raca)%>%
  rbind(raca)
raca_col <- t(raca_col)

idade <- as.data.frame(idade)

tabela <- cbind(raca_col,genero_col,idade)
colnames(tabela) <- c("Cor ou Raça","N senadores","Gênero","N de senadores","Média")
rownames(tabela) <- NULL

save(tabela,file = "tabela.rda")

# formatando

tabela <- tabela %>% 
  kable(caption = "Perfil dos Senadores") %>%
  kable_classic(html_font = "Cambria",font_size=10) %>%
  row_spec(0,bold=T) %>%
  add_header_above(c("Cor ou Raça" = 2, "Gênero" = 2, "Idade" = 1)) %>%
  kable_styling(latex_options = c("hold_position"))

save_kable(tabela,file = "tabela.png")

## mapa

estados <- read_country(year = 2020)

disc_est <- senadores %>%
  group_by(sigla_uf_partido) %>%
  summarise(across(Neutro:`A favor`,sum))

estados <- estados %>%
  left_join(disc_est,by = c("abbrev_state"="sigla_uf_partido")) %>%
  mutate(Neutro = ifelse(is.na(Neutro)==T,0,Neutro),
         Contra = ifelse(is.na(Contra)==T,0,Contra),
         `A favor` = ifelse(is.na(`A favor`)==T,0,`A favor`))

# mapa discursos

grDevices::png(filename = "mapa_discurso.png",width = 1000,height = 800)

estados %>%
  ggplot() +
  geom_sf(aes(fill=Contra), alpha = 0.9, col="white") +
  scale_fill_gradient(name = "N", labels = scales::comma,low = "pink1",high = "#cc0000",na.value = "grey") + 
  labs(title = "Número de Discursos Contra o Governo Federal") +
  theme(plot.title = element_text(face = "bold"))

dev.off()

## Casos de Covid

# filtrando os casos para o acumulado até o primeiro dia da CPI

casos <- casos %>%
  filter(date == "2021-04-27", place_type=="state") %>%
  select(state,confirmed_per_100k_inhabitants,death_rate)

estados <- estados %>%
  left_join(casos,by = c("abbrev_state"="state"))

# mapa casos

grDevices::png(filename = "mapa_casos.png",width = 1000,height = 800)

estados %>%
  ggplot() +
  geom_sf(aes(fill=confirmed_per_100k_inhabitants), alpha = 0.9, col="white") +
  scale_fill_gradient(name = "100mil habitantes", labels = scales::comma,low = "lightyellow",high = "gold",na.value = "grey") + 
  labs(title = "Número de Casos de Covid Confirmados até o Ínicio da CPI por 100k") +
  theme(plot.title = element_text(face = "bold"))

dev.off()

# mapa mortes

grDevices::png(filename = "mapa_mortes.png",width = 1000,height = 800)

estados %>%
  ggplot() +
  geom_sf(aes(fill=death_rate), alpha = 0.9, col="white") +
  scale_fill_gradient(name = "Taxa de Mortalidade", labels = scales::comma,low = "plum",high = "purple4",na.value = "grey") + 
  labs(title = "Taxa de Mortalidade por Estado até o Ínicio da CPI") +
  theme(plot.title = element_text(face = "bold"))

dev.off()

save(estados, file = "estados_sen_cov.rda")
