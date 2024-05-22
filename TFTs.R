### Load the required packages --------------------------- 

library(readr)
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)

### Load the data --------------------------------

#Load the data of percentage variation of births
data = read.csv("nascimentos_auto_arima_maio.csv", sep = ";")

#Load the estimated total fecundity Rates (TFR) 
TFE<-read_csv("wcde_data.csv",skip=8)

#At the end, we didn't consider Cuba in the study. Let's remove it from the database
TFE<-TFE[TFE$Area!="Cuba",]

### Create the dataset -------------------

#The idea is modify the categories of education level and age group

#Desired table has the following structure

#Country | Years of study | TFR

## Categorize the education level in years  ----------------- 

YearsStudy <- function(YearsStudy) {
  if (YearsStudy %in% c("No Education", "Incomplete Primary", "Primary")) {
    return("at most 7")
  } else if (YearsStudy %in% c("Lower Secondary", "Upper Secondary")) {
    return("8-11")
  } else if (YearsStudy %in% c("Post Secondary")) {
    return("12 or more")
  } else {
    return(NA) # In case that there is another value
  }
}

## Obtain variable information ----------------------
# This function was used before, it obtains the country, age group and years of study that the variable name is referring

info_variable <- function(variable){
  
  #1.Get the years of study.
  # There are 3 possibel categories code as "07", "8" and "12" in the name of the column
  if (substr(variable,2,2)=="8"){
    StudyYears<-substr(variable,2,2)
  }else{StudyYears <- substr(variable, 2, 3)}
  
  #2. Now we are going to get the country of the variable. 
  
  #First we create a dictionary to interpret the country codes
  CountryLetter <- c("B" = "Brazil", "CR" = "Costa Rica", "M" = "Mexico", "CB" = "Cuba", "CH"="Chile")
  
  # Get the position (index) of the character that refers to the country in the column name
  InitialCountryPosition <- ifelse(StudyYears == "8", 3,4)
  
  # Get the country codes
  country_code <- substr(variable, InitialCountryPosition, ifelse(substr(variable, InitialCountryPosition + 1, InitialCountryPosition + 1) %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), InitialCountryPosition, InitialCountryPosition + 1))
  
  # Interpret the country code using the dictionary
  country <- ifelse(country_code %in% names(CountryLetter), CountryLetter[country_code], "Otro")
  
  # 3. Get the age group, which in most of the cases are the last 4 characters of the column name
  #   the group 40+ will be identified in a different way
  
  Age2 <- substr(variable, nchar(variable) - 1, nchar(variable))
  Age1 <- substr(variable, nchar(variable) - 3, nchar(variable)-2)
  
  # Get the interpretation of each StudyYears code-abbreviation
  if (StudyYears=="07"){
    text<-"at most 7"}else{if(StudyYears=="8"){text<-"8-11"}else{text<-"12 or more"}}
  
  # return all the information obtained in a vector
  return(c(StudyYears = text, country = country, Age1 = Age1,Age2=Age2))
}


#For practical use, we will create functions to obtain the country, age and study independently
country<-function(Variable){info_variable(Variable)[2]}
study_years<-function(Variable){info_variable(Variable)[1]}
Age<-function(Variable){
  #Here, we take in count the only Age group that is not coded in 4 characters, that is "40+"
  Age2<-info_variable(Variable)[4]
  if (Age2=="40"){return("40+")}else{
    paste(info_variable(Variable)[3],"-",info_variable(Variable)[4])}
}




## Calculate TFR by country and study years -----------------

#The given rates on the file, are given for per thousand women. In order to have the rate per woman, we divide the rate by a thousand

TFE$Rate<-TFE$Rate/1000

# Modify the values of the Age column, so both tables will have the same values in that column

TFE<-TFE %>% mutate(Age=gsub("--"," - ",Age))

# Categories "40-44" and "45-49" are unified in the category 40+

TFE[TFE$Age%in%c("40 - 44","45 - 49"),]["Age"]<-"40+"
#Now, we sum the rates grouped by the values in the other columns. This is made to sum the rates of the 2 categories that were merged
TFE<-aggregate(Rate~ Area+Period+Age+ Education,data=TFE,FUN=sum)

#Categorize the years of study given the education level using the YearsStudy function

TFE["Years_Study"]<-(sapply(TFE$Education,YearsStudy))

#We calculated the Total Fecundity Rate grouped by country, age and years of study

Rates <- aggregate(Rate ~ Area+Age+ Years_Study, data = TFE, FUN = mean)

#Tomamos nossa dada de referencia, Julio de 2022

Corte<-data[data$Mes=="2022-07",]
Corte2<-data[data$Mes=="2021-07",]
#Vamos crear uma tabela sobre as variacoes por pais,
Variacao<-NULL
for (Coluna in colnames(Corte)[2:length(colnames(Corte))]){
  Ag<-Age(Coluna)
  Pa<-pais(Coluna)[[1]]
  if (Pa %in% c("Chile","Cuba")){ Var<-Corte2[[Coluna]]}else{
  Var<-Corte[[Coluna]]}
  Stu<-anos_estudo(Coluna)[[1]]
  Fila<-data.frame(Area=Pa,Age=Ag,Years_Study=Stu,Var=Var)
  Variacao<-rbind(Variacao,Fila)
}
#Mudamos o nome a 40+
Variacao[!(Variacao$Age %in% c("15 - 19","20 - 24","25 - 29","30 - 34","35 - 39")),]["Age"]<-"40+"

#Tiramos a Cuba e outros valores onde nao se tenha data
Variacao<-na.omit(Variacao)
#Apricamos o efeito da pandemia (Variaciao) em cada grupo
Rates<-merge(Rates,Variacao, by=c("Area","Age","Years_Study"))
Rates["WithEffect"]<-Rates$Rate*(1+Rates$Var/100)

#Calculamos a TFT sem efeito da pandemia
TFTSemQueda<-aggregate(Rate~Area+Years_Study,data=Rates,FUN=sum)
TFTSemQueda$Rate<-TFTSemQueda$Rate*5
#Calculamos a TFT com efeito da pandemia
TFTComQueda<-aggregate(WithEffect~Area+Years_Study,data=Rates,FUN=sum)
TFTComQueda$WithEffect<-TFTComQueda$WithEffect*5

#Fazemos uma tabela conjunta para fazer mais facil a comparacao
TFT<-merge(TFTSemQueda,TFTComQueda,by=c("Area","Years_Study"))
TFT<-TFT%>% arrange(Area,desc(Years_Study))
write_xlsx(TFT,"TFT.xlsx")


datos_long <- tidyr::pivot_longer(TFT, cols = c(Rate, WithEffect), names_to = "TFTType", values_to = "Rate")

datos_long$Years_Study <- factor(datos_long$Years_Study, levels = c("at most 7", "8-11", "12 or more"))
ggplot(datos_long, aes(x = Years_Study , y = Rate, 
                fill = TFTType))+   
  labs(title="Total Fecundity Rate by country")+xlab("Years of study")+
  geom_col(position = 'dodge', color = "gray50")+
  facet_grid( ~ Area, switch = "x")+
  scale_fill_manual(values = c("darkblue", "lightblue3"),
                    labels = c("Without effect", "With effect"),
                    name="TFR")+theme_bw()+
  geom_hline(yintercept = 2.1, linetype = "dashed", color = "black")
