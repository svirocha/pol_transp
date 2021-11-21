#########################################
###############TCC José Luiz#############
################18/11/21#################
#########################################

# Preparar ambiente
rm(list=ls()) #limpa tudo que estiver no global environment
# para limpar um objeto específico, use rm(nomeobjeto)
cat("\014")  # limpa console - também pode usar ctrl + L

# Instalar e carregar pacotes
if(require(plyr) == F) install.packages('plyr'); require(plyr)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(qdap) == F) install.packages('qdap'); require(qdap)
if(require(rio) == F) install.packages('rio'); require(rio)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(electionsBR) == F) install.packages('electionsBR'); require(electionsBR)
if(require(stringi) == F) install.packages('stringi'); require(stringi)
if(require(foreign) == F) install.packages('foreign'); require(foreign)
if(require(stringr) == F) install.packages('stringr'); require(stringr)
if(require(arsenal) == F) install.packages('arsenal'); require(arsenal)
if(require(magrittr) == F) install.packages('magrittr'); require(magrittr)
require(readxl)
require(readr)

#  Vamos precisar baixar dados da MUNIC para 2015 (já que 2016 não teve)
#  Dados do TSE para 2012 (abrange mandato 2013 a 2016)
#  PIB 2015 e 2016 (IBGE)

############################ BANCO CÓDIGOS#####################################

# Importar banco de códigos (do Prof. Dalson Figueiredo - UFPE) dos municípios para facilitar o merge 
codigos <- read.csv("C:/Users/Maria/Downloads/Base_ligando_diversos_codigos_institucio.txt")

# Visualizar primeiras observações do banco
head(codigos)

# Selecionar colunas que vamos precisar do banco de códigos

# Há duas formas de selecionar as colunas:

# Pela posição da coluna no banco (a matriz) - se fosse linha, fica antes da vírgula: [1:3,]
#codigos <- codigos[ ,c(1:3)]

# Ou pelo nome das variáveis - mais fácil para quem replica entender o que você fez depois
codigos <- select(codigos, "id_munic_7", "id_munic_6", "id_TSE", "municipio")

#################################COMISSIONADOS#################################

# Importar banco MUNIC - adm pública
#https://www.ibge.gov.br/estatisticas/sociais/educacao/10586-pesquisa-de-informacoes-basicas-municipais.html?edicao=18195&t=downloads
adm2015 <- read_excel("C:/Users/Maria/Downloads/adm2015.xls")

# checar banco
str(adm2015) #str() mostra estrutura
head(adm2015) #head() mostra primeiras observações. head(data,5) mostra só as 5 primeiras (vc pode escolher quantas quiser)
dim(adm2015) #dim() mostra total de obs e variáveis
names(adm2015) #names() mostra o nome de cada variáveis e ajuda a ver em que posição elas estão - útil para subset, rename etc
#o que eu mais costumo usar são as de name e head, mas dim e str ajudam muito a checar se o merge deu certo também

# Selecionar colunas
names(adm2015) #checar nomes das variáveis
adm2015 <- select(adm2015, "A1", "A2", "A3", "A5")

# Renomear colunas
names(adm2015)[1]<- "id_munic_7"
names(adm2015)[2]<- "total_serv"
names(adm2015)[3]<- "estat"
names(adm2015)[4]<- "comis"

# Checar
names(adm2015)
head(adm2015)

# Criar variável de proporção de comissionados
adm2015$comis <- as.numeric(adm2015$comis) #transformar de character para numeric
adm2015$total_serv <- as.numeric(adm2015$total_serv)#idem
adm2015$prop_comis <- adm2015$comis/adm2015$total_serv #criar nova variável que cria prop comiss/total
head(adm2015,5)

##############################POP & REGIÃO - 2015##############################

# Importar banco
#https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?edicao=18021&t=downloads
pop2015 <- read_excel("C:/Users/Maria/Downloads/pop2015.xls")
head(pop2015)


# Selecionar colunas
names(pop2015)
head(pop2015)
pop2015 <- select(pop2015, "A1", "A199", "A203", "A204")

# Renomear variáveis
head(pop2015)

names(pop2015)[1:4] <- c("id_munic_7","regiao","classepop","pop") #essa é uma outra forma de renomear, 
                                                                  #que fica mais elegante quando precisa renomear várias colunas

# Cortar região e classe pop e fazer duas variáveis - com número e string
pop2015$regiao1 <- stringi::stri_extract_last_words(pop2015$regiao) #retirar número, deixar só nome regiao
pop2015$classepop1 <- substring(pop2015$classepop, 1, 1) #extrai o primeiro caractere da string

head(pop2015,5) #checar banco

########################MERGIR BANCOS INTERMEDIÁRIOS###########################
df <- merge(codigos, adm2015, by.x = "id_munic_7",by.y = "id_munic_7")
df <- merge(df, pop2015, by.x = "id_munic_7",by.y = "id_munic_7")

#################################PIB 2015######################################

# Importar banco
pib2015 <- read_excel("C:/Users/Maria/Downloads/PIB dos Municípios - base de dados 2010-2016.xls")

# Checar nomes das colunas
names(pib2015)

# Aqui vou usar pipe (%>%) para selecionar e filtrar em um código só
pib2015 <- pib2015 %>% 
  select('Ano' , 'Código do Município', 'Produto Interno Bruto per capita\n(R$ 1,00)') %>%
  filter(Ano == "2015")

# Renomear colunas
names(pib2015)[1:3] <- c("ano","id_munic_7","pibcapita")

#########################REPETIR PROCESSO PARA PIB 2016########################

pib2016 <- read_excel("C:/Users/Maria/Downloads/PIB dos Municípios - base de dados 2010-2016.xls")

pib2016 <- pib2016 %>%
  select("Ano" , "Código do Município", "Produto Interno Bruto per capita\n(R$ 1,00)") %>%
  filter(Ano == "2016")

names(pib2016)[1:3] <- c("ano","id_munic_7","pibcapita")

head(pib2016)

################################LAI MUNIC#######################################
#NÃO PRECISA RODAR, É APENAS PARA REGISTRO

# Importar banco lai
lai <- read_excel("C:/Users/Maria/Downloads/lai.xlsx")

# Selecionar colunas
names(lai)
lai <- lai[ ,c(1,27,31)]

# Renomear variáveis lai
names(lai)[1]<- "id_munic_7"
names(lai)[2]<- "atendidas"
names(lai)[3]<- "total_pedidos"

# Criar variável de proporção de pedidos LAI atendidos
lai$atendidas <- as.numeric(lai$atendidas) #transformar de character para numeric
lai$total_pedidos <- as.numeric(lai$total_pedidos)#idem
lai$conformidadelai <- lai$atendidas/lai$total_pedidos #criar nova variável que cria prop comiss/tota

# Checar quantidade de NAs
sum(is.na(lai$conformidadelai)) #3293 - MUITOS! Vai ser necessário mudar regressão para uma que lide com muitos NAs

# Remover do ambiente do R
rm(lai)

#######################EBT - ESCALA BRASIL TRANSPARENTE########################

#https://mbt.cgu.gov.br/publico/avaliacao/escala_brasil_transparente/200000001
# 1ed - Avaliação: de 12/01/2015 a 04/05/2015
# 2ed - Avaliação: de 27/07/2015 a 09/10/2015
# 3ed - Avaliação: de 27/06/2016 a 16/01/2017

# Importar banco
ebt <- read_delim("C:/Users/Maria/Downloads/EBT.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
names(ebt)

# Selecionar colunas desejadas

ebt <- select(ebt, "rodada" , "cod_ibge", "pontos_transp_passiva")

# Remover observações duplicadas
ebt = ebt[!duplicated(ebt$cod_ibge),]

## 1 ed

# Filtrar obs para 1ed
ebt1 <- filter(ebt, rodada == "1") #filtrar primeira rodada
ebt1 <- ebt1[c(1:492),] #filtrar somente municípios (porque no original aparece ufs)

## 2 ed

# Filtrar obs para 2ed
ebt2 <- filter(ebt, rodada == "2") #filtrar segunda rodada

## 3 ed

# Filtrar obs para 3ed
ebt3 <- filter(ebt, rodada == "3") #filtrar terceira rodada

# Concatenar ebt 1 e 2 (feitas em 2015) para mergir com o pib2015
ebt2015 <- rbind(ebt1,ebt2)

# Agora vamos mergir pib2015 com ebt2015 e pib2016 com ebt 3 (feita em 2016)
ebt_pib2015 <- merge(ebt2015, pib2015, by.x = "cod_ibge",by.y = "id_munic_7")
ebt_pib2016 <- merge(ebt3, pib2016, by.x = "cod_ibge",by.y = "id_munic_7")

# Concatenar os dois bancos
data <- rbind(ebt_pib2015,ebt_pib2016)

# Rodar mais uma vez exclusão de observações duplicadas
data = data[!duplicated(data$cod_ibge),]

# Megir df e data
df <- merge(data, df, by.x = "cod_ibge",by.y = "id_munic_7")

head(df)
dim(df)

# Retirar objetos desnecessários
rm(adm2015,data,ebt,ebt_pib2015,ebt_pib2016,ebt1,ebt2,ebt2015,ebt3,
   pib2015,pib2016,pop2015)

###############################REELEIÇÃO#######################################

# Para reeleição::Importar bancos do TSE (baixados no site)
#https://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1

# Importar banco
reeleito <- read.csv("C:/Users/Maria/Downloads/consulta_cand_2012_BRASIL.csv", sep=";")
names(reeleito)

# Selecionar colunas e filtrar obs para prefeitos eleitos
reeleito <- reeleito %>%
  select("ANO_ELEICAO", "NM_TIPO_ELEICAO", "SG_UE", "DS_CARGO", "DS_SIT_TOT_TURNO", "ST_REELEICAO") %>%
  filter(DS_SIT_TOT_TURNO %in% 
           c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA") & DS_CARGO == "PREFEITO")

# Renomear colunas
names(reeleito)[1:6] <- c("ano_eleicao","tipo_eleicao","id_TSE",
                          "cargo","resultado_eleicao","reeleicao")

# Criar nova variável que transforma S, N em 1 e 0 e 'não divulgável' em vazio

table(reeleito$reeleicao) #tabular variável para checar categorias

reeleito$reeleicao_dummy <- #criar variável usando recode
  dplyr::recode(
    reeleito$reeleicao, 
      "S" = "1",
      "N" = "0", 
      "Não divulgável" = "NA")

# Checar se funcionou
table(reeleito$reeleicao_dummy)

##############################MARGEM DE VITÓRIA################################

# Baixar dados com electionsBR 
results2012 <- electionsBR::vote_mun_zone_local(2012, uf = "all", ascii = FALSE, encoding = 
                                     "latin1", export = TRUE) 

# Checar nomes
names(results2012)

# Selecionar colunas
NEP_MARGEM <- select(results2012, "ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "CODIGO_MUNICIPIO", 
                     "NOME_MUNICIPIO", "NUMERO_ZONA", "CODIGO_CARGO", "DESCRICAO_CARGO", "NOME_CANDIDATO", 
                     "NUMERO_PARTIDO", "SIGLA_PARTIDO", "TOTAL_VOTOS")

# Selecionar primeiro turno e resultado apenas para prefeito
NEP_MARGEM <- NEP_MARGEM %>% filter(NEP_MARGEM$NUM_TURNO == 1,
                                    NEP_MARGEM$CODIGO_CARGO == 11)

# Checar os dados
head(NEP_MARGEM)
table(NEP_MARGEM$NUM_TURNO)
table(NEP_MARGEM$CODIGO_CARGO)

# Group by cidade, zonas eleitorais
NEP_MARGEM2 <- NEP_MARGEM %>% group_by(CODIGO_MUNICIPIO, NOME_MUNICIPIO, 
                                       NUMERO_PARTIDO, SIGLA_UF) %>% 
  summarise(TOTAL_VOTOS=sum(TOTAL_VOTOS))

#Criar uma coluna com a proporção de votos de cada candidato a Prefeito 
NEP_MARGEM3 <- NEP_MARGEM2 %>% group_by(CODIGO_MUNICIPIO, NOME_MUNICIPIO, 
                                        SIGLA_UF) %>% 
  mutate(PROP_VOTOS = TOTAL_VOTOS/sum(TOTAL_VOTOS))

# Selecionar apenas os dois primeiros lugares (com mais votos) para cade município
# Função "top_n" já considera grupos previamente criados 
AUX_TAB <- NEP_MARGEM3 %>% top_n(2,PROP_VOTOS)

# Calcular a diferença entre os dois mais votados
# ATENÇÃO: Número 3 como default quando houve apenas um candidato competindo 
# Isso será descartado no próximo passo do script
AUX_TAB1 <- AUX_TAB %>% group_by(CODIGO_MUNICIPIO, NOME_MUNICIPIO, SIGLA_UF) %>% 
  summarise(MARG= abs(lead(PROP_VOTOS, default = 3)-PROP_VOTOS))

# Padrão gera duas diferenças, então é necessário selecionar a menor delas, que corresponde à margem de vitória
AUX_TAB2 <- AUX_TAB1 %>% group_by(CODIGO_MUNICIPIO, NOME_MUNICIPIO, 
                                  SIGLA_UF) %>% summarise(MARG= min(MARG))

# Remover acentos, pontuação, letras maiúsculas e espaços extras para os nomes das cidades
AUX_TAB2$NOME_MUNICIPIO <- stri_trans_general(AUX_TAB2$NOME_MUNICIPIO, "Latin-ASCII") #remove accents
AUX_TAB2$NOME_MUNICIPIO <- stri_replace_all(AUX_TAB2$NOME_MUNICIPIO, "", regex = "[[:punct:]]") #remove punctuation
AUX_TAB2$NOME_MUNICIPIO <- stri_replace_all(AUX_TAB2$NOME_MUNICIPIO, "", regex = "\\.") #remove extra space
AUX_TAB2$NOME_MUNICIPIO <- stri_trans_tolower(AUX_TAB2$NOME_MUNICIPIO) #transform capital letter in lower case
AUX_TAB2$NOME_MUNICIPIO <- stri_sub(AUX_TAB2$NOME_MUNICIPIO)

head(AUX_TAB2) #check data
save(AUX_TAB2, file = "margemvit2012.RData") # salvar banco Rdata
write.csv(AUX_TAB2,'C:/Users/Maria/Downloads/margemvit2012.csv', row.names = FALSE) #salvar banco em csv

# Retirar objetos que não vamos mais usar
rm(AUX_TAB, AUX_TAB1, NEP_MARGEM, NEP_MARGEM2, NEP_MARGEM3)

################################MERGES FINAIS##################################

## Mergir margem de vitória e reeleicao
codreel <- merge(reeleito, codigos, by.x = "id_TSE", by.y = "id_TSE")

# Checar nome e mudar nome da coluna de AUX_TAB2 para executar left_join
names(AUX_TAB2)
names(AUX_TAB2)[1]<- "id_TSE" #mudar nome

AUX_TAB2 <- AUX_TAB2 %>%  mutate(id_TSE = as.numeric(id_TSE)) #mudar tipo da coluna para numérico

varpol <- dplyr::left_join(codreel, AUX_TAB2, by = "id_TSE") #executar left join

## Mergir varpol + df
df_tcc <- merge(df, varpol, by.x = "id_munic_6", by.y = "id_munic_6")

table(df_tcc$reeleicao)

# Checar merge - com library(arsenal) e função comparedf
summary(arsenal::comparedf(df,df_tcc)) #Mostra que há 37 obs em df_tcc que não estão em df
                                      #'arsenal::' imediatamente antes da função comparedf mostra que a função vem desse pacote

# Tentar ver quais são as observações a mais em newdf com anti_join (dplyr)
teste <- dplyr::anti_join(df, df_tcc, by = "id_munic_6") #devolve 23 obs (e não 37) e retira as 5 colunas de reeleito

# Remover duplicados - nada acontece
teste2 = df_tcc[!duplicated(df_tcc$id_munic_6),]
summary(arsenal::comparedf(df_tcc,teste2))
rm(teste2)

#############################SALVAR OS BANCOS##################################

# Salvar os bancos como csv para checar manualmente
#row.names=FALSE é usado para evitar coluna de index quando abrimos no excel
write.csv(df,'C:/Users/Maria/Downloads/dfneto.csv', row.names = FALSE)
write.csv(df_tcc,'C:/Users/Maria/Downloads/df_tcc.csv', row.names = FALSE)
write.csv(teste,'C:/Users/Maria/Downloads/antijoin.csv', row.names = FALSE)

# Salvar df_tcc como RData
save(df_tcc, file = "df_tcc.RData")

# Salvar resultados2012 como RData, caso precise depois (electionsBR demora a carregar)
save(results2012, file = "eleicao2012.RData")
