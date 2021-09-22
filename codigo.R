# Dados de transportes 
# Data criacao 21/9/2021
# Tabela de dados obtida do site 
# 

######################################################
# 1) Carregar bibliotecas

library(tidyverse)
library(magrittr)
#library(dplyr)
library(readr)
library(rjson)
library(RJSONIO)

# Library para importar dados SQL
library(DBI)
library(RMySQL)
library(pool)
library(sqldf)
library(RMariaDB)

# Carragamento de banco de dados

# Settings
db_user <-'admin'
db_password <-'password'
db_name <-'cdnaep'
#db_table <- 'your_data_table'
db_host <-'127.0.0.1' # for local access
db_port <-3306

# 3. Read data from db
# drv=RMariaDB::MariaDB(),
mydb <-  dbConnect(drv =RMariaDB::MariaDB(),user =db_user, 
                   password = db_password ,
                   dbname = 'cdnaep', host = db_host, port = db_port)

dbListTables(mydb)

s <- paste0("SELECT * from"," frota_veicular")
rs<-NULL
rs <- dbSendQuery(mydb, s)

dados<- NULL
dados <-  dbFetch(rs, n = -1)
dados
#dbHasCompleted(rs)
#dbClearResult(rs)


dados %<>% filter(local %in% c('Salvador')) %>% select(-local) %>% 
            gather(key = classe, value = numero,-ano,-id) 
dados %<>% select(-id)
# Temas Subtemas Perguntas

##  Perguntas e titulos 
T_ST_P_No_TRASNPMOB <- read_csv("data/TEMA_SUBTEMA_P_No - TRASNPORTEMOBILIDADE.csv")


## Arquivo de saida 

SAIDA_POVOAMENTO <- T_ST_P_No_TRASNPMOB %>% 
  select(TEMA,SUBTEMA,PERGUNTA,CODIGO)
SAIDA_POVOAMENTO <- as.data.frame(SAIDA_POVOAMENTO)

classes <- NULL
classes <- levels(as.factor(dados$classe))

# Cores secundarias paleta pantone -
corsec_recossa_azul <- c('#175676','#62acd1','#8bc6d2',
                         '#20cfef','#a094e1','#4a82a8',
                         '#7faed2')

for ( i in 1:length(classes)) {
  
  objeto_0 <- dados %>%
    filter(classe %in% c(classes[i])) %>%
    select(ano,numero) %>% filter(ano>2000) %>%
    arrange(ano) %>%
    mutate(ano = as.character(ano)) %>% list()               
  
  exportJson0 <- toJSON(objeto_0)
  
  
  titulo<-T_ST_P_No_TRASNPMOB$TITULO[i]
  subtexto<-"IBGE"
  link <-"https://www.ibge.gov.br/"
  
  data_axis <- paste('[',gsub(' ',',',
                              paste(paste(as.vector(objeto_0[[1]]$ano)),
                                    collapse = ' ')),']',sep = '')
  
  data_serie <- paste('[',gsub(' ',',',
                               paste(paste(as.vector(objeto_0[[1]]$numero)),
                                     collapse = ' ')),']',sep = '')
  
  texto<-paste('{"title":{"text":"',titulo,
               '","subtext":"',subtexto,
               '","sublink":"',link,'"},',
               '"tooltip":{"trigger":"axis"},',
               '"toolbox":{"left":"center","orient":"horizontal","itemSize":20,"top":45,"show":true,',
               '"feature":{"dataZoom":{"yAxisIndex":"none"},',
               '"dataView":{"readOnly":false},"magicType":{"type":["line","bar"]},',
               '"restore":{},"saveAsImage":{}}},"xAxis":{"type":"category",',
               '"data":',data_axis,'},',
               '"yAxis":{"type":"value","axisLabel":{"formatter":"{value}"}},',
               '"series":[{"data":',data_serie,',',
               '"type":"bar","color":"',corsec_recossa_azul[i],'","showBackground":true,',
               '"backgroundStyle":{"color":"rgba(180, 180, 180, 0.2)"},',
               '"itemStyle":{"borderRadius":10,"borderColor":"',corsec_recossa_azul[i],'","borderWidth":2}}]}',sep='')
  
  SAIDA_POVOAMENTO$CODIGO[i] <- texto   
  texto<-noquote(texto)
  
  
  write(exportJson0,file = paste('data/',gsub('.js','',T_ST_P_No_TRASNPMOB$`NOME ARQUIVO JS`[i]),
                                 '.json',sep =''))
  write(texto,file = paste('data/',T_ST_P_No_TRASNPMOB$`NOME ARQUIVO JS`[i],
                           sep =''))
  
}

# Arquivo dedicado a rotina de atualizacao global. 

write_csv2(SAIDA_POVOAMENTO,file ='data/POVOAMENTO.csv',quote='all',escape='none')

