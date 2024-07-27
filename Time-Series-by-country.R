### Require packages #--------------------------------
rm( list = ls( ) )
graphics.off( )

library(seasonal)
library(openxlsx)
library(emf)
library(dplyr)
library(zoo)
library(scales)
library(ggpubr)
library(grid)
library(gridExtra)
library(forecast)
library("xlsx")
library("ggplot2")
library(readxl)

### Import data set #--------------------
Sys.setlocale("LC_TIME", "C")
data= read.csv("data/Variation_of_births2.csv", sep = ";")
data$Mes<-as.yearmon(data$Mes)


### Obtain the information of the variable -------------------------

# Given the name structure of each column of the dataset.
# We create a function to obtain the country, age group and years of study from the name of the column
# For example: X07B2529 is Brazil, with age group 25-29 and between 0 and 7 years od study 

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



### Create function to plot time series ----------------------------

#Here, we a create a function to do an auto ARIMA and plot the result given the column name
# this is used to reduce the length od the code repeating the same process for different variables

# Function to measure the accuracy of the estimated time serie. This could be integrated with the PlotTimeSeries Function, but we had unexpected problems with this.
AccTimeSeries<-function(Variable,data){
  
  #Variable: The name of the column/variable in the data set
  
  #data: The dataset where the data is going to be obtained

  
  #Select the columns that are for the same country of the Variable
  country_columns<-colnames(data)[sapply(colnames(data),country)%in% country(Variable)]
    #now, we make the arima process    
    train_data = ts(data[Variable], start = c(2012,1), end = c(2020,10), frequency = 12)
    data_ts = ts(data[Variable], start = c(2012,1), end = c(2022,12), frequency = 12)
    tedata = window(data_ts, start=c(2020,11), end=c(2022,12))
    
    cat("\r")
    flush.console()
    ModArima = auto.arima(train_data, trace = T, stepwise = F, approximation = F)
    
    prevArima=forecast(ModArima, h=36, level = 95) #The parameter "level" establish the confidence intervals. It can be a vector to plot multiple intervals
    Accuracy<-accuracy(prevArima)

    return(Accuracy)}

PlotTimeSeries<-function(Variable,data, legend=F,tittle=F, ylim=c(-80,80)){

  #Variable: The name of the column/variable in the data set
  
  #data: The dataset where the data is going to be obtained
  
  # legend: plot legend in graph
  
  # title: plot an automated title generated with the name, age group and years of study 
  
  #ylim: a vector of length 2 which values are used to adjust the range on the plot the graph. If NULL, it will only consider the variable in the function.
  #     If "country" it will consider all time series referring to the same country to adjust ylim on the plot
  #     If "all" it will consider all the countries in dataset. if "notCR" it wont consider neither Cuba or Costa Rica (this because their variations are very high)
  
  #if (Variable %in% unique(sapply(colnames(data),country))){
  #  data2<-data
  #  data2[["B1524"]]<-rowSums(data[sapply(colnames(data),Age)=="15 - 24"])
  #  }
  
  #Select the columns that are for the same country of the Variable
  country_columns<-colnames(data)[sapply(colnames(data),country)%in% country(Variable)]
  
  #Now we will adjust the range of the plot, it depends if it wants to be the same for all the countries or not
  if (class(ylim)=="numeric"){maxlim<-ylim[2]
  minlim<-ylim[1]}else{
  if (is.null(ylim) == T){
    data2<-data[c("Mes",Variable)]}else{
  if (ylim=="country"){
  data2<-data[c("Mes",country_columns)]}
  if(ylim =="notCR"){
  data2<-data[colnames(data)[sapply(colnames(data),country)!="Cuba"]]
  data2<-data2[colnames(data2)[sapply(colnames(data2),country)!="Costa Rica"]]}
    }
  if (is.null(ylim)==T){
    maxlim<-max(data2[data2$Mes>"Oct 2020",][2:ncol(data2)], na.rm = T)+20
    minlim<-min(data2[data2$Mes>"Oct 2020",][2:ncol(data2)], na.rm = T)-20
  }else{
  if (ylim%in%c("country","notCR")){
  maxlim<-max(data2[data2$Mes>"Oct 2020",][2:ncol(data2)], na.rm = T)+20
  minlim<-min(data2[data2$Mes>"Oct 2020",][2:ncol(data2)], na.rm = T)-20
  }else{if (ylim=="all"){
  maxlim<-max(data[data$Mes>"Oct 2020",][2:ncol(data)], na.rm = T)+20
  minlim<-min(data[data$Mes>"Oct 2020",][2:ncol(data)], na.rm = T)-20
  }}}}
  
  #Just in case there is no data available in the selected column
  if (nrow(data[!is.na(data[Variable]),][Variable])==0){return(ggplot() +
                                                                   labs(x = "", y = "", title = "")+
                                                                   annotate("text",x = 2021.665, y = (maxlim-minlim)/2, size = 10, label="No info",color = "black")+
                                                                   xlim(c(2020.83,2022.5))+ #Os limites horizontais do Graph
                                                                   ylim(c(minlim-10,maxlim+10)) #Os limites verticais do Graph
  )}else{

#now, we make the arima process    
train_data = ts(data[Variable], start = c(2012,1), end = c(2020,10), frequency = 12)
data_ts = ts(data[Variable], start = c(2012,1), end = c(2022,12), frequency = 12)
tedata = window(data_ts, start=c(2020,11), end=c(2022,12))


cat("\r")
flush.console()
ModArima = auto.arima(train_data, trace = T, stepwise = F, approximation = F)

cat("\n")

prevArima=forecast(ModArima, h=36, level = 95) #The parameter "level" establish the confidence intervals. It can be a vector to plot multiple intervals


#If true, we add the title and legend to the plot
titles<-info_variable(Variable)
if (tittle==T){
  titlee<-paste("Women between ",titles[[3]],"-",titles[[4]]," with ",titles[[1]],"years of schooling - ",titles[[2]])
}else{titlee=" "}
if (legend==T){
  legend_position="bottom"
}else{legend_position="none"}

line<-data.frame(x1=2020.85,x2=2021.7,y1=maxlim-20,y2=maxlim-20)

#Now, we will plot the time series, most of the parameters are just for aesthetic purposes.
#A simpler code could be plot(PrevArima)

Graph<-autoplot(prevArima,color="blue",series="Forecast", PI=F) + #plot time series
  #Settings for confidence interval
  geom_ribbon(aes( x=seq(2020.833, by = 0.0833333, length.out = 36),
                  ymin = lower, ymax = upper),
              data =data.frame(lower=as.numeric(prevArima$lower[,1]),upper=as.numeric(prevArima$upper[,1]))  ,
              fill = "black",
              alpha = 0.2,
              color="black",
              linetype="dotted"
              )+
  geom_line(aes(x=seq(2020.833, by = 0.0833333, length.out = 36),
                 y = lower),
             data =data.frame(lower=as.numeric(prevArima$lower[,1]),upper=as.numeric(prevArima$upper[,1])),
            size = 0.1, color = "black",linetype="longdash")+
  geom_line(aes(x=seq(2020.833, by = 0.0833333, length.out = 36),
                 y = upper),
             data =data.frame(lower=as.numeric(prevArima$lower[,1]),upper=as.numeric(prevArima$upper[,1])),
             size = 0.1, color = "black", linetype = "longdash")+
  theme_bw()+ #set_theme
  autolayer(tedata,series="Observed",colour = T)+ #plot observed data
  autolayer(prevArima$mean,series="Forecasted",colour=T)+ #plot predicted data
  scale_x_continuous(labels = function(x) format(as.yearmon(x, origin = "2012-10-01"), "%b-%Y"))+ #Set the x-axis to date format
  coord_cartesian(xlim = as.yearmon(c("2020-10", "2022-11")))+  #Horizontal limits in date format 
  ylim(c(min(minlim,-maxlim)),max(-minlim,maxlim))+ #Range of plot
  theme(legend.position = legend_position, #Legend position
        plot.title = element_text(size=10), #Title size
        axis.title.x=element_blank(), #Remove x axis lab
        axis.text.x=element_text(angle = 45, vjust = 1.2, hjust=1,size = 7.75), #Modify x axis values
        axis.ticks.y = element_blank(),  # Remove y axis values
        axis.title.y = element_blank(), # Remove y labs
        axis.ticks.x=element_blank())+ # Remove x ticks
  ggtitle(titlee)+  #Add title to the graph
  geom_line(data = data.frame(x = c(max(time(prevArima$x)), min(time(prevArima$mean))), y = c(prevArima$x[length(prevArima$x)], prevArima$mean[1])),
            aes(x = x, y = y), color = "blue")+ #Fill the hole between the last observation and the first prediction that is made by the autoplot function
  labs(x="Births in months", y="Variation",colour="Birth counts")+ #Add names for X and Y axis
  geom_vline(xintercept=2020.833,color="forestgreen",alpha=0.95)+ #Horizontal line to mark the beginning of pandemic
  annotate("text",x = 2021.25, y = maxlim-10, label = "Pandemic", color = "forestgreen",size=2.5,fontface="bold")+  #Add text "Pandemic" to the graph
  #Draw the arrow below the "pandemic" text
  geom_segment(aes(x = x1, xend = x2,
               y = y1, yend = y2),data = line, 
               color = "forestgreen",linewidth =0.7, #a3a3a3 other color
               arrow = arrow(length = unit(0.0725,"inch")),
               lineend = "round",
               linejoin = "round"
               )+
  scale_color_manual(values=c("blue","red"))#Set colors of the legend

 return(Graph)} #Add ModArima if you wnat to return the model as well
}

### Function to plot legend -----------------------------
# This function will help us to obtain the legend box of the plot, will be usefil at the moment  of the grid

get_only_legend <- function(plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(plot))  
  
  #  Mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend)  
}

### Quick resume of the variable information -------------------------------
datainfo<-data.frame("columns"=colnames(data)[2:ncol(data)])
datainfo$Education_level<-lapply(datainfo$columns,study_years)
datainfo$Age<-datainfo$columns%>%lapply(Age)
datainfo$country<-datainfo$columns%>%lapply(country)

#### Accuracy measures of each serie.--------------
datainfo$Accuracy<-datainfo$columns%>%lapply(AccTimeSeries,data=data)
datainfo[c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")]<-do.call(rbind,datainfo$Accuracy)
datainfo[1:4]<-lapply(datainfo[1:4],as.character)
datainfo<-select(datainfo,-c("Accuracy"))
datainfo<-datainfo2
writexl::write_xlsx(datainfo,"data/Accuracy.xlsx")


### Example: Single time series plots --------------------------------

#Here we do some examples for testing the code, and also

## X07CR1519 -  Women between 15 and 24 years old with 0 to 7 years of education in Costa Rica

Graph1<-PlotTimeSeries("X07CR1524",data,ylim=NULL)

## X07M2024 -  Women between 15 and 24 years old with 0 to 7 years of education in Mexico

Graph2<-PlotTimeSeries("X07M1524",data, ylim="country")

## X12B3039 - Women between 35 years old or more with more than 12 years of education in Brazil

Graph3<-PlotTimeSeries("X12B35",data, ylim="all")

## X8CH2529 - Women between 25 and 29 years old with 8 to 11 years of education in Chile

Graph4<-PlotTimeSeries("X8CH2529",data, ylim=c(-100,100))

## X12CR1519 - Women between 15 and 24 years old with at least 12 years of education in Costa Rica
Graph5<-PlotTimeSeries("X12CR1524",data, ylim=c(-60,60))

## X12B1519 - Women between 15 and 24 years old with at least 12 years of education in Costa Rica
Graph6<-PlotTimeSeries("X12B1524",data)



### Grid of variations by country ------------------------

#Finally, we will made a grid by age group and years of study for each country. These graphs are the one that are showed in the paper

#First, we generate the legend for the grid
legend<-get_only_legend(PlotTimeSeries("X12B35",data,legend = T))



####  Costa Rica-------------------------
grid1<-grid.arrange(
  #First line
  arrangeGrob(PlotTimeSeries("X07CR1524",data,ylim = c(-60,60)), left=Age("X07CR1524"), top=study_years("X07CR1524")),
  arrangeGrob(PlotTimeSeries("X8CR1524",data,ylim = c(-60,60)), top=study_years("X8CR1524")),
  arrangeGrob(PlotTimeSeries("X12CR1524",data,ylim = c(-60,60)), top=study_years("X12CR1524")),
  arrangeGrob(PlotTimeSeries("X00CR1524",data,ylim = c(-60,60)), top=study_years("X00CR1524")),
  #Second line
  arrangeGrob(PlotTimeSeries("X07CR2529",data,ylim = c(-60,60)), left=Age("X07CR2529")),
  arrangeGrob(PlotTimeSeries("X8CR2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CR2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CR2529",data,ylim = c(-60,60))),
  
  #Third line
  arrangeGrob(PlotTimeSeries("X07CR3034",data,ylim = c(-60,60)), left=Age("X07CR3034")),
  arrangeGrob(PlotTimeSeries("X8CR3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CR3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CR3034",data,ylim = c(-60,60))),
  
  #Fourth line
  arrangeGrob(PlotTimeSeries("X07CR35",data,ylim = c(-60,60)), left=Age("X07CR35")),
  arrangeGrob(PlotTimeSeries("X8CR35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CR35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CR35",data,ylim = c(-60,60))),
  
  ncol=4,
  left = textGrob("Age group",gp=gpar(fontsize=11,font=2),rot=90),
  top= textGrob("Costa Rica", gp=gpar(fontsize=13,font=2),rot=0),
  bottom=textGrob("Births in month",gp=gpar(fontsize=11,font=2))
 )
grid2<-grid.arrange(grid1,legend,ncol=1, heights=c(10,1))


#### Brazil----------------------

grid3<-grid.arrange(
  #First line
  arrangeGrob(PlotTimeSeries("X07B1524",data,ylim = c(-60,60)), left=Age("X07B1524"), top=study_years("X07B1524")),
  arrangeGrob(PlotTimeSeries("X8B1524",data,ylim = c(-60,60)), top=study_years("X8CR1524")),
  arrangeGrob(PlotTimeSeries("X12B1524",data,ylim = c(-60,60)), top=study_years("X12B1524")),
  arrangeGrob(PlotTimeSeries("X00B1524",data,ylim = c(-60,60)), top=study_years("X00B1524")),
  #Second line
  arrangeGrob(PlotTimeSeries("X07B2529",data,ylim = c(-60,60)), left=Age("X07B2529")),
  arrangeGrob(PlotTimeSeries("X8B2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12B2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00B2529",data,ylim = c(-60,60))),
  
  #Third line
  arrangeGrob(PlotTimeSeries("X07B3034",data,ylim = c(-60,60)), left=Age("X07B3034")),
  arrangeGrob(PlotTimeSeries("X8B3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12B3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00B3034",data,ylim = c(-60,60))),
  
  #Fourth line
  arrangeGrob(PlotTimeSeries("X07B35",data,ylim = c(-60,60)), left=Age("X07B35")),
  arrangeGrob(PlotTimeSeries("X8B35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12B35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00B35",data,ylim = c(-60,60))),
  
  ncol=4,
  left = textGrob("Age group",gp=gpar(fontsize=11,font=2),rot=90),
  top= textGrob("Brazil", gp=gpar(fontsize=13,font=2),rot=0),
  bottom=textGrob("Births in months",gp=gpar(fontsize=11,font=2))
)
grid4<-grid.arrange(grid3,legend,ncol=1, heights=c(10,1))


#Cuba was discarted from the paper due bad quality data
#### Cuba (discarted)-----------------------
grid5<-grid.arrange(
  #First line
  arrangeGrob(PlotTimeSeries("X07CB1524",data,ylim = c(-60,60)), left=Age("X07CB1524"), top=study_years("X07CB1524")),
  arrangeGrob(PlotTimeSeries("X8CB1524",data,ylim = c(-60,60)), top=study_years("X8CB1524")),
  arrangeGrob(PlotTimeSeries("X12CB1524",data,ylim = c(-60,60)), top=study_years("X12CB1524")),
  arrangeGrob(PlotTimeSeries("X00CB1524",data,ylim = c(-60,60)), top=study_years("X00CB1524")),
  #Second line
  arrangeGrob(PlotTimeSeries("X07CB2529",data,ylim = c(-60,60)), left=Age("X07CB2529")),
  arrangeGrob(PlotTimeSeries("X8CB2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CB2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CB2529",data,ylim = c(-60,60))),
  
  #Third line
  arrangeGrob(PlotTimeSeries("X07CB3034",data,ylim = c(-60,60)), left=Age("X07CB3034")),
  arrangeGrob(PlotTimeSeries("X8CB3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CB3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CB3034",data,ylim = c(-60,60))),
  
  #Fourth line
  arrangeGrob(PlotTimeSeries("X07CB35",data,ylim = c(-60,60)), left=Age("X07CB35")),
  arrangeGrob(PlotTimeSeries("X8CB35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CB35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CB35",data,ylim = c(-60,60))),
  
  ncol=4,
  left = textGrob("Age group",gp=gpar(fontsize=11,font=2),rot=90),
  top= textGrob("Cuba", gp=gpar(fontsize=13,font=2),rot=0),
  bottom=textGrob("Births in months",gp=gpar(fontsize=11,font=2))
)
grid6<-grid.arrange(grid5,legend,ncol=1, heights=c(10,1))

#### Mexico ------------------------------

grid7<-grid.arrange(
  #First line
  arrangeGrob(PlotTimeSeries("X07M1524",data,ylim = c(-60,60)), left=Age("X07M1524"), top=study_years("X07M1524")),
  arrangeGrob(PlotTimeSeries("X8M1524",data,ylim = c(-60,60)), top=study_years("X8M1524")),
  arrangeGrob(PlotTimeSeries("X12M1524",data,ylim = c(-60,60)), top=study_years("X12M1524")),
  arrangeGrob(PlotTimeSeries("X00M1524",data,ylim = c(-60,60)), top=study_years("X00M1524")),
  #Second line
  arrangeGrob(PlotTimeSeries("X07M2529",data,ylim = c(-60,60)), left=Age("X07M2529")),
  arrangeGrob(PlotTimeSeries("X8M2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12M2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00M2529",data,ylim = c(-60,60))),
  
  #Third line
  arrangeGrob(PlotTimeSeries("X07M3034",data,ylim = c(-60,60)), left=Age("X07M3034")),
  arrangeGrob(PlotTimeSeries("X8M3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12M3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00M3034",data,ylim = c(-60,60))),
  
  #Fourth line
  arrangeGrob(PlotTimeSeries("X07M35",data,ylim = c(-60,60)), left=Age("X07M35")),
  arrangeGrob(PlotTimeSeries("X8M35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12M35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00M35",data,ylim = c(-60,60))),
  
  ncol=4,
  left = textGrob("Age group",gp=gpar(fontsize=11,font=2),rot=90),
  top= textGrob("Mexico", gp=gpar(fontsize=13,font=2),rot=0),
  bottom=textGrob("Births in months",gp=gpar(fontsize=11,font=2))
)
grid8<-grid.arrange(grid7,legend,ncol=1, heights=c(10,1))


#### Chile ------------------------------------

grid9<-grid.arrange(
  #First line
  arrangeGrob(PlotTimeSeries("X07CH1524",data,ylim = c(-60,60)), left=Age("X07CH1524"), top=study_years("X07CH1524")),
  arrangeGrob(PlotTimeSeries("X8CH1524",data,ylim = c(-60,60)), top=study_years("X8CH1524")),
  arrangeGrob(PlotTimeSeries("X12CH1524",data,ylim = c(-60,60)), top=study_years("X12CH1524")),
  arrangeGrob(PlotTimeSeries("X00CH1524",data,ylim = c(-60,60)), top=study_years("X00CH1524")),
  #Second line
  arrangeGrob(PlotTimeSeries("X07CH2529",data,ylim = c(-60,60)), left=Age("X07CH2529")),
  arrangeGrob(PlotTimeSeries("X8CH2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CH2529",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CH2529",data,ylim = c(-60,60))),
  
  #Third line
  arrangeGrob(PlotTimeSeries("X07CH3034",data,ylim = c(-60,60)), left=Age("X07CH3034")),
  arrangeGrob(PlotTimeSeries("X8CH3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CH3034",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CH3034",data,ylim = c(-60,60))),
  
  #Fourth line
  arrangeGrob(PlotTimeSeries("X07CH35",data,ylim = c(-60,60)), left=Age("X07CH35")),
  arrangeGrob(PlotTimeSeries("X8CH35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X12CH35",data,ylim = c(-60,60))),
  arrangeGrob(PlotTimeSeries("X00CH35",data,ylim = c(-60,60))),
  
  ncol=4,
  left = textGrob("Age group",gp=gpar(fontsize=11,font=2),rot=90),
  top= textGrob("Chile", gp=gpar(fontsize=13,font=2),rot=0),
  bottom=textGrob("Births in months",gp=gpar(fontsize=11,font=2))
)
grid10<-grid.arrange(grid9,legend,ncol=1, heights=c(10,1))



#Save images

ggsave("CostaRica.png", grid2, width = 16, height = 12, units = "in", bg = "white")
ggsave("Brazil.png", grid4, width = 16, height = 12, units = "in", bg = "white")
ggsave("Mexico.png", grid8, width = 16, height = 12, units = "in", bg = "white")
ggsave("Chile.png", grid10, width = 16, height = 12, units = "in", bg = "white")

ggsave("CostaRica2.png", grid2, width = 12, height = 10, units = "in", bg = "white")
ggsave("Brazil2.png", grid4, width = 12, height = 10, units = "in", bg = "white")
ggsave("Mexico2.png", grid8, width = 12, height = 10, units = "in", bg = "white")
ggsave("Chile2.png", grid10, width = 12, height = 10, units = "in", bg = "white")













#Future work -----------------------
#Now we will estimate the post-pandemic period, i.e, since January 2023

data2023=read_excel("data/data-Brazil-2023.xlsx")
data2023$Mes<-as.yearmon(data2023$Mes)

PlotTimeSeries2023<-function(Variable,data, legend=F,tittle=F, ylim=c(-80,80)){
  
  #Variable: The name of the column/variable in the data set
  
  #data: The dataset where the data is going to be obtained
  
  # legend: plot legend in graph
  
  # title: plot an automated title generated with the name, age group and years of study 
  
  #ylim: a vector of length 2 which values are used to adjust the range on the plot the graph. If NULL, it will only consider the variable in the function.
  #     If "country" it will consider all time series referring to the same country to adjust ylim on the plot
  #     If "all" it will consider all the countries in dataset. if "notCR" it wont consider neither Cuba or Costa Rica (this because their variations are very high)
  
  
  #Select the columns that are for the same country of the Variable
  colnames(data)<-c("Mes",paste0("X",colnames(data[,2:ncol(data)])))
  country_columns<-colnames(data)[sapply(colnames(data),country)%in% country(Variable)]
  
  #Now we will adjust the range of the plot, it depends if it wants to be the same for all the countries or not
  if (class(ylim)=="numeric"){maxlim<-ylim[2]
  minlim<-ylim[1]}else{
    if (is.null(ylim) == T){
      data2<-data[c("Mes",Variable)]}else{
        if (ylim=="country"){
          data2<-data[c("Mes",country_columns)]}
        if(ylim =="notCR"){
          data2<-data[colnames(data)[sapply(colnames(data),country)!="Cuba"]]
          data2<-data2[colnames(data2)[sapply(colnames(data2),country)!="Costa Rica"]]}
      }
    if (is.null(ylim)==T){
      maxlim<-max(data2[data2$Mes>"Dez 2022",][2:ncol(data2)], na.rm = T)+20
      minlim<-min(data2[data2$Mes>"Dez 2022",][2:ncol(data2)], na.rm = T)-20
    }else{
      if (ylim%in%c("country","notCR")){
        maxlim<-max(data2[data2$Mes>"Dez 2022",][2:ncol(data2)], na.rm = T)+20
        minlim<-min(data2[data2$Mes>"Dez 2022",][2:ncol(data2)], na.rm = T)-20
      }else{if (ylim=="all"){
        maxlim<-max(data[data$Mes>"Dez 2022",][2:ncol(data)], na.rm = T)+20
        minlim<-min(data[data$Mes>"Dez 2022",][2:ncol(data)], na.rm = T)-20
      }}}}
  
  #Just in case there is no data available in the selected column
  if (nrow(data[!is.na(data[Variable]),][Variable])==0){return(ggplot() +
                                                                 labs(x = "", y = "", title = "")+
                                                                 annotate("text",x = 2021.665, y = (maxlim-minlim)/2, size = 10, label="No info",color = "black")+
                                                                 xlim(c(2020.83,2024.5))+ #Os limites horizontais do Graph
                                                                 ylim(c(minlim-10,maxlim+10)) #Os limites verticais do Graph
  )}else{
    
    #now, we make the arima process    
    train_data = ts(data[Variable], start = c(2012,1), end = c(2022,12), frequency = 12)
    data_ts = ts(data[Variable], start = c(2012,1), end = c(2023,12), frequency = 12)
    tedata = window(data_ts, start=c(2023,1), end=c(2023,12))
    
    ModArima = auto.arima(train_data, trace = T, stepwise = F, approximation = F)
    prevArima=forecast(ModArima, h=13, level = 95) #The parameter "level" establish the confidence intervals. It can be a vector to plot multiple intervals
    accuracy(prevArima)
    
    #If true, we add the title and legend to the plot
    titles<-info_variable(Variable)
    if (tittle==T){
      titlee<-paste("Women between ",titles[[3]],"-",titles[[4]]," with ",titles[[1]],"years of schooling - ",titles[[2]])
    }else{titlee=" "}
    if (legend==T){
      legend_position="bottom"
    }else{legend_position="none"}
    
    line<-data.frame(x1=2020.85,x2=2021.7,y1=maxlim-20,y2=maxlim-20)
    
    line2<-data.frame(x1=2023.025,x2=2023.89,y1=maxlim-20,y2=maxlim-20)
    
    #Now, we will plot the time series, most of the parameters are just for aesthetic purposes.
    #A simpler code could be plot(PrevArima)
    
    Graph<-autoplot(prevArima,color="blue",series="Forecast", PI=F) + #plot time series
      #Settings for confidence interval
      geom_ribbon(aes( x=seq(2023, by = 0.0833333, length.out = 13),
                       ymin = lower, ymax = upper),
                  data =data.frame(lower=as.numeric(prevArima$lower[,1]),upper=as.numeric(prevArima$upper[,1]))  ,
                  fill = "black",
                  alpha = 0.1,
                  color="black",
                  linetype="dotted")+
      theme_bw()+ #set_theme
      autolayer(tedata,series="Observed",colour = T)+ #plot observed data
      autolayer(prevArima$mean,series="Forecasted",colour=T)+ #plot predicted data
      scale_x_continuous(labels = function(x) format(as.yearmon(x, origin = "2012-10-01"), "%b-%Y"))+ #Set the x-axis to date format
      coord_cartesian(xlim = as.yearmon(c("2020-10", "2023-12")))+  #Horizontal limits in date format 
      ylim(c(min(minlim,-maxlim)),max(-minlim,maxlim))+ #Range of plot
      theme(legend.position = legend_position, #Legend position
            plot.title = element_text(size=10), #Title size
            axis.title.x=element_blank(), #Remove x axis lab
            axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=1,size = 5), #Modify x axis values
            axis.ticks.y = element_blank(),  # Remove y axis values
            axis.title.y = element_blank(), # Remove y labs
            axis.ticks.x=element_blank())+ # Remove x ticks
      ggtitle(titlee)+  #Add title to the graph
      geom_line(data = data.frame(x = c(max(time(prevArima$x)), min(time(prevArima$mean))), y = c(prevArima$x[length(prevArima$x)], prevArima$mean[1])),
                aes(x = x, y = y), color = "blue")+ #Fill the hole between the last observation and the first prediction that is made by the autoplot function
      labs(x="Births in months", y="Variation",colour="Birth counts")+ #Add names for X and Y axis
      geom_vline(xintercept=2020.833,color="#a3a3a3",alpha=0.95)+ #Horizontal line to mark the beginning of pandemic
      annotate("text",x = 2021.25, y = maxlim-10, label = "Pandemic", color = "#a3a3a3",size=2.5,fontface="bold")+  #Add text "Pandemic" to the graph
      #Draw the arrow below the "pandemic" text
      geom_segment(aes(x = x1, xend = x2,
                       y = y1, yend = y2),data = line, 
                   color = "#a3a3a3",linewidth =0.7,
                   arrow = arrow(length = unit(0.0725,"inch")),
                   lineend = "round",
                   linejoin = "round"
      )+
      geom_vline(xintercept=2023.01,color="#a3a3a3",alpha=0.95)+ #Horizontal line to mark the beginning of pandemic
      annotate("text",x = 2023.25, y = maxlim-10, label = "Post-Pandemic", color = "#a3a3a3",size=2.5,fontface="bold")+  #Add text "Post-pandemic" to the graph
      #Draw the arrow below the "post-pandemic" text
      geom_segment(aes(x = x1, xend = x2,
                       y = y1, yend = y2),data = line2, 
                   color = "#a3a3a3",linewidth =0.7,
                   arrow = arrow(length = unit(0.0725,"inch")),
                   lineend = "round",
                   linejoin = "round"
      )+
      scale_color_manual(values=c("blue","red"))#Set colors of the legend
    
    return(Graph)}
}


PlotTimeSeries2023("X12B2024",data2023)

