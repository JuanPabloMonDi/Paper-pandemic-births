### Load the required packages --------------------------- 

library(readr)
library(dplyr)
library(readxl)
library(writexl)
library(zoo)
library(ggplot2)

### Load the data --------------------------------

#Load the data of percentage variation of births
data = read.csv("data/Variation_of_births.csv", sep = ";")

#Load the estimated total fecundity Rates (TFR) 
TFE<-read_csv("data/wcde_data.csv",skip=8)

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

#We take our reference motnh, July 2022

ReferenceDate<-data[data$Mes=="2022-07",]
ReferenceDate2<-data[data$Mes=="2021-07",]
year2022<-data[data$Mes>=as.yearmon("2022-1"),]
year2021<-data[data$Mes>=as.yearmon("2021-1") & data$Mes<as.yearmon("2022-1"),]

#Now, lets create a table with the variation of TFR by country
Variation<-NULL
for (Column in colnames(ReferenceDate)[2:length(colnames(ReferenceDate))]){
  Ag<-Age(Column)
  Pa<-country(Column)[[1]]
  if (Pa %in% c("Chile","Cuba")){ 
    Var<-ReferenceDate2[[Column]] #Variation of the specific month
    MeanVar<-colMeans(year2021[2:91])
    MeanVar<-MeanVar[[Column]] #Mean variation of 2021
  }else{
  Var<-ReferenceDate[[Column]]
  MeanVar<-colMeans(year2022[2:91])
  MeanVar<-MeanVar[[Column]]}
  Stu<-study_years(Column)[[1]]
  Fila<-data.frame(Area=Pa,Age=Ag,Years_Study=Stu,Var=Var, MeanVar=MeanVar)
  Variation<-rbind(Variation,Fila)
}


Variation[!(Variation$Age %in% c("15 - 19","20 - 24","25 - 29","30 - 34","35 - 39")),]["Age"]<-"40+"

#remove Cuba an outer countries that does not have any data.
Variation<-na.omit(Variation)

#Apply the pandemic on the TFR pf each group
Rates<-merge(Rates,Variation, by=c("Area","Age","Years_Study"))
Rates["WithEffect"]<-Rates$Rate*(1+Rates$Var/100)
Rates["WithEffectMean"]<-Rates$Rate*(1+Rates$MeanVar/100)

#Calculate the TFR without the effect of the pandemic
TFRProjected<-aggregate(Rate~Area+Years_Study,data=Rates,FUN=sum)
TFRProjected$Rate<-TFRProjected$Rate*5

#Calculate the TFR with the effect of the pandemic
TFRWithPandemic<-aggregate(WithEffect~Area+Years_Study,data=Rates,FUN=sum) #considering july
TFRWithPandemicMean<-aggregate(WithEffectMean~Area+Years_Study,data=Rates,FUN=sum) #considering the mean variation pf 2022

TFRWithPandemic$WithEffect<-TFRWithPandemic$WithEffect*5 #Each age group has a length of 5
TFRWithPandemicMean$WithEffectMean<-TFRWithPandemicMean$WithEffectMean*5
#Make a joint table of the TFR to make comparison easier.
TFR<-merge(TFRProjected,TFRWithPandemic,by=c("Area","Years_Study"))
TFR<-merge(TFR,TFRWithPandemicMean,by=c("Area","Years_Study"))
TFR<-TFR%>% arrange(Area,desc(Years_Study))
write_xlsx(TFR,"data/TFR.xlsx")

#Now, lets plot this variations to have a different view of the data

#With the effect of the variations of july
data_long <- tidyr::pivot_longer(TFR, cols = c(Rate, WithEffect), names_to = "TFR_Type", values_to = "Rate")

data_long$Years_Study <- factor(data_long$Years_Study, levels = c("at most 7", "8-11", "12 or more"))
ggplot(data_long, aes(x = Years_Study , y = Rate, 
                fill = TFR_Type))+theme_bw()+
  labs(title="Total Fecundity Rate by country")+xlab("Years of study")+
  geom_col(position = 'dodge', color = "gray50")+
  facet_grid( ~ Area, switch = "x")+
  scale_fill_manual(values = c("darkblue", "lightblue3"),
                    labels = c("Without effect", "With effect"),
                    name="TFR")+
  geom_hline(yintercept = 2.1, linetype = "dashed", color = "black")

#With the effect of the mean variations

data_long <- tidyr::pivot_longer(TFR, cols = c(Rate, WithEffectMean), names_to = "TFR_Type", values_to = "Rate")

data_long$Years_Study <- factor(data_long$Years_Study, levels = c("at most 7", "8-11", "12 or more"))
ggplot(data_long, aes(x = Years_Study , y = Rate, 
                      fill = TFR_Type))+theme_bw()+
  labs(title="Total Fecundity Rate by country")+xlab("Years of schooling")+
  geom_col(position = 'dodge', color = "gray50")+
  facet_grid( ~ Area, switch = "x")+
  scale_fill_manual(values = c("darkblue", "lightblue3"),
                    labels = c("Without effect", "With mean effect"),
                    name="TFR")+
  geom_hline(yintercept = 2.1, linetype = "dashed", color = "black")
