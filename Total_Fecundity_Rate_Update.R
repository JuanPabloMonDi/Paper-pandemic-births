### Load the required packages --------------------------- 

library(tidyr)
library(readr)
library(dplyr)
library(readxl)
library(writexl)
library(zoo)
library(ggplot2)

###  Necessary functions ----------------------

## Categorize the education level in years  ----------------- 

YearsStudy <- function(YearsStudy) {
  if (YearsStudy %in% c("No Education", "Incomplete Primary", "Primary")) {
    return("Less or until 7")
  } else if (YearsStudy %in% c("Lower Secondary", "Upper Secondary")) {
    return("8-11")
  } else if (YearsStudy %in% c("Post Secondary")) {
    return("12 or more")
  } else if (YearsStudy %in% c("Total")){
    return("Total")
  }else{
    return(NA) # In case that there is another value
  }
}


AgeGroup <- function(age) {
  if (age %in% c("15--19","20--24")) {
    return("15 - 24")
  } else if (age %in% c("25--29")) {
    return("25 - 29")
  } else if (age %in% c("30--34")) {
    return("30 - 34")
  } else if (age %in% c("35--39","40--44","45--49")){
    return("35+")
  } else if (age %in% c("All")){
    return("Total")
  }else{
    return(NA) # In case that there is another value
  }
}



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
    text<-"Less or until 7"}else{if(StudyYears=="8"){text<-"8-11"}else{if(StudyYears=="00"){text<-"Total"}else{text<-"12 or more"}}}
  
  # return all the information obtained in a vector
  return(c(StudyYears = text, country = country, Age1 = Age1,Age2=Age2))
}


#For practical uses in the plots, we will create functions to obtain the country, age and study independently
country<-function(Variable){info_variable(Variable)[2]}
study_years<-function(Variable){info_variable(Variable)[1]}
Age<-function(Variable){
  #Here, we take in count the only Age group that is not coded in 4 characters, that is "40+"
  Age2<-info_variable(Variable)[4]
  if (Age2=="35"){return("35+")}else{
    paste(info_variable(Variable)[3],"-",info_variable(Variable)[4])}
}

### Load the data --------------------------------

#Load the data of percentage variation of births
births = read.csv("data/Births_by_month.csv", sep = ";")

#Load the estimated population sizes of the Wittgenstein Center

pop<-read_csv("data/wicdf.csv",skip=8)
#pop2<-pop
#pop<-pop[pop$Education!="Total",]

pop$Population<-pop$Population*1000
pop$Education<-sapply(pop$Education,YearsStudy)
pop$Age<-sapply(pop$Age,AgeGroup)

pop<-pop%>% group_by(,Year,Area,Age,Sex,Education)%>%reframe(Population=sum(Population))

#Method 2 -------------------
#The previous method is the same as do a simple linear regression.
#We can verify by doing a regression for each country, age and academic level an comparing the values
Slopes2 <- pop%>%
  group_by(Area, Age, Sex, Education) %>%
  reframe(Slope = coef(lm(Population ~ Year))[2],
            population2020=coef(lm(Population~ Year))[1]+coef(lm(Population~ Year))[2]*2020,
            population2021=coef(lm(Population~ Year))[1]+coef(lm(Population~ Year))[2]*2021,
            population2022=coef(lm(Population~ Year))[1]+coef(lm(Population~ Year))[2]*2022,
            population2023=coef(lm(Population~ Year))[1]+coef(lm(Population~ Year))[2]*2023)

# Method 1 --------------
Slopes<-pop%>% group_by(Area, Age, Sex, Education) %>%
  reframe(Slope = (Population[Year == 2025] - Population[Year == 2020]) / (2025 - 2020), population2020= Population[Year==2020])

Slopes["population2021"]=Slopes$population2020+Slopes$Slope*1 #one year after 2020 (2021)
Slopes["population2022"]=Slopes$population2020+Slopes$Slope*2 #Two years after 2020 (2022)
Slopes["population2023"]=Slopes$population2020+Slopes$Slope*3 #Three years after 2020 (2023)


#Now, we will estimated the exposed population in the 2022. This is the same as the estimated values before
#Slopes["Estimated2022"]<-(Slopes$population2023+Slopes$population2021)/2

### Create the dataset -------------------


##

births <- births %>% separate(Mes, into=c("Year","Month"),sep="-")


pop%>% group_by(Year)%>% reframe(Population=sum(Population))

births <- births %>%
  pivot_longer(cols = -c(Year, Month), names_to = "Column_Name", values_to = "Number_of_Births")%>%
  mutate(Age = sapply(Column_Name,Age),
         Area = sapply(Column_Name,country),
         Education = sapply(Column_Name,study_years))%>%
  select(Year, Month, Area, Age, Education, Number_of_Births)

total_births<- births%>% group_by(Year,Area, Age,Education)%>% 
  summarize(births=sum(Number_of_Births))

#Now, calculating TFR

ASFR1<-total_births[total_births$Year==2022 & c(total_births$Area %in% c("Costa Rica","Brazil","Mexico")),]
ASFR1<-merge(ASFR1,Slopes, by.x=c("Area","Age","Education"))

#For Chile
ASFR2<-total_births[total_births$Year==2021 & c(total_births$Area %in% c("Chile")),]
ASFR2<-merge(ASFR2,Slopes, by=c("Area","Age","Education"))


ASFR1$ASFR<-ASFR1$births/ASFR1$population2022
ASFR2$ASFR<-ASFR2$births/ASFR2$population2021

ASFR<-rbind(ASFR1,ASFR2)
%>%select(Area,Age,Education,ASFR)


TFR<-aggregate(ASFR~Area+Education,data=ASFR,FUN=sum)
TFR$ASFR<-TFR$ASFR*5
