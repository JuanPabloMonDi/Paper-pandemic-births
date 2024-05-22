library(readxl)
library(tidyverse)
library(readr)

#Carregamos os dados de nascimentos
nascimentos <- read_delim("~/Taxa Migracao/nascimentos.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Establecemos que as colunas dos anos, sejam de valor numerico
nascimentos[c(2:9)]<-nascimentos[c(2:9)]%>% mutate_all(as.numeric)

#Separamos a coluna "microrregiao IBGE" em 2. 
#A primeira vai ser o código do municipio e a segunda o nome
nascimentos<-nascimentos%>%separate(col = "Microrregiao IBGE", #A coluna a separar
                                    into=c("CD_MICRO","NM_MICRO"), #As duas colunas que vao se crear
                                    sep=" ", #O carater que vai se usar como criterio para separar, nesse caso o espaco em branco
                                    extra = "merge") #Esse indica que só vai dividir a coluna em 2 partes (pois só temos duas colunas em into)

#nascimentos<-nascimentos%>%mutate_all(~replace_na(.,0))

#Agora, fazemos o mesmo procedimento para os obitos.
Obitos <- read_delim("~/Taxa Migracao/Obitos.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
Obitos[c(2:9)]<-Obitos[c(2:9)]%>% mutate_all(as.numeric)
Obitos<-Obitos%>%separate(col = "Microrregiao IBGE", 
                                    into=c("CD_MICRO","NM_MICRO"), 
                                    sep=" ",
                                    extra = "merge") 

#Agora, carregamos as estimativas
Estimativas <- read_excel("~/Taxa Migracao/micrroregião_estimativas.xlsx")

#Essas linhas sao para conferir algumas coisas importantes antes de seguir

#1. os nomes das bases de dados de nascimentos e obitos sao os mesmos que das estimativas
for (coluna in colnames(nascimentos)){print(coluna %in% colnames(Estimativas))}
for (coluna in colnames(Obitos)){print(coluna %in% colnames(Estimativas))}

#2. Os codigos das microregioes sao os mesmos nas 3 bases de dados
unique(Estimativas$CD_MICRO)%in%unique(nascimentos$CD_MICRO)
unique(Estimativas$CD_MICRO)%in%unique(Obitos$CD_MICRO)


#Calculamos as taxas liquidas de migracao
#Taxa liquida= (populacao ano atual-(Populacao ano anterior+nascimentos-obitos))/populacao ano atual

anos<-as.character(2013:2018) #Anos para os quais vao se calcular a taxa
TaxasLiquidas<-data.frame()
Saldos<-data.frame()
for (codigo in Estimativas$CD_MICRO){
  nascidos<-nascimentos[nascimentos[["CD_MICRO"]]==codigo,]
  mortes<- Obitos[Obitos$CD_MICRO==codigo,]
  pop<- Estimativas[Estimativas$CD_MICRO==codigo,]
  #Criamos a uma nova tabela para salvar as taxas dos anos no municipio
  TaxaMunicipio<-Estimativas[Estimativas$CD_MICRO==codigo,][c("CD_MICRO","NM_MICRO","SIGLA_UF")] 
  SaldoMunicipio<-TaxaMunicipio
  for (ano in anos){
    saldo<- ceiling(pop[[ano]]- #Populacao do ano 
      (pop[[as.character(as.numeric(ano)-1)]]+ #Populacao ano anterior
            nascidos[[as.character(as.numeric(ano)-1)]]- #Nascimentos ano anterior
            mortes[[as.character(as.numeric(ano)-1)]])) #Mortes ano anterior
    #Dividimos pela populacao desse ano
    taxa<-round(saldo/pop[[ano]],5) #aproximada a 5 cifras decimais
    
    #Acrescentamos o valor do ano na tabela do municipio
    TaxaMunicipio[ano]<-taxa
    SaldoMunicipio[ano]<-saldo
  }
  #Finalmente, acrescentamos as taxas calculadas para o municipio na tabela
  TaxasLiquidas<-rbind(TaxasLiquidas,TaxaMunicipio)
  Saldos<-rbind(Saldos,SaldoMunicipio)
  }


#As taxas liquidas de migracao sao
head(TaxasLiquidas,10)
head(Saldos)
#Um valor positivo indica que o ano seguinte teve mais populacao que o estimado com os nascimentos.

#Para finalizar, vamos salvar a tabela num arquivo excel

writexl::write_xlsx(TaxasLiquidas, "~/Taxa Migracao/TaxasMigracao.xlsx")
writexl::write_xlsx(Saldos, "~/Taxa Migracao/SaldosMigratario.xlsx")
