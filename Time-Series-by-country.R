### Require packages #--------------------------------
rm( list = ls( ) )
graphics.off( )

library(seasonal)
library(openxlsx)
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
data= read.csv("data/Variation_of_births.csv", sep = ";")
data2023=read_excel("data/data-Brazil-2023.xlsx")
data$Mes<-as.yearmon(data$Mes)
data2023$Mes<-as.yearmon(data2023$Mes)

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
    text<-"at most 7"}else{if(StudyYears=="8"){text<-"8-11"}else{text<-"12 or more"}}
  
  # return all the information obtained in a vector
  return(c(StudyYears = text, country = country, Age1 = Age1,Age2=Age2))
}


#For practical uses in the plots, we will create functions to obtain the country, age and study independently
country<-function(Variable){info_variable(Variable)[2]}
study_years<-function(Variable){info_variable(Variable)[1]}
Age<-function(Variable){
  #Here, we take in count the only Age group that is not coded in 4 characters, that is "40+"
  Age2<-info_variable(Variable)[4]
  if (Age2=="40"){return("40+")}else{
    paste(info_variable(Variable)[3],"-",info_variable(Variable)[4])}
}



### Create function to plot time series ----------------------------

#Here, we a create a function to do an auto ARIMA and plot the result given the column name
# this is used to reduce the length od the code repeating the same process for different variables


PlotTimeSeries<-function(Variable,data, legend=F,tittle=F, ylim=c(-80,80)){

  #Variable: The name of the column/variable in the data set
  
  #data: The dataset where the data is going to be obtained
  
  # legend: plot legend in graph
  
  # title: plot an automated title generated with the name, age group and years of study 
  
  #ylim: a vector of length 2 which values are used to adjust the range on the plot the graph. If NULL, it will only consider the variable in the function.
  #     If "country" it will consider all time series referring to the same country to adjust ylim on the plot
  #     If "all" it will consider all the countries in dataset. if "notCR" it wont consider neither Cuba or Costa Rica (this because their variations are very high)
  
  
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
    maxlim<-max(data2[data2$Mes>"Out 2020",][2:ncol(data2)], na.rm = T)+20
    minlim<-min(data2[data2$Mes>"Out 2020",][2:ncol(data2)], na.rm = T)-20
  }else{
  if (ylim%in%c("country","notCR")){
  maxlim<-max(data2[data2$Mes>"Out 2020",][2:ncol(data2)], na.rm = T)+20
  minlim<-min(data2[data2$Mes>"Out 2020",][2:ncol(data2)], na.rm = T)-20
  }else{if (ylim=="all"){
  maxlim<-max(data[data$Mes>"Out 2020",][2:ncol(data)], na.rm = T)+20
  minlim<-min(data[data$Mes>"Out 2020",][2:ncol(data)], na.rm = T)-20
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

ModArima = auto.arima(train_data, trace = T, stepwise = F, approximation = F)
prevArima=forecast(ModArima, h=36, level = 95) #The parameter "level" establish the confidence intervals. It can be a vector to plot multiple intervals
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

#Now, we will plot the time series, most of the parameters are just for aesthetic purposes.
#A simpler code could be plot(PrevArima)

Graph<-autoplot(prevArima,color="blue",series="Forecast", PI=F) + #plot time series
  #Settings for confidence interval
  geom_ribbon(aes( x=seq(2020.833, by = 0.0833333, length.out = 36),
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
  coord_cartesian(xlim = as.yearmon(c("2020-10", "2022-12")))+  #Horizontal limits in date format 
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
  scale_color_manual(values=c("blue","red"))#Set colors of the legend

 return(Graph)}
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


### Example: Single time series plots --------------------------------

#Here we do some examples for testing the code, and also

## X07CR1519 -  Women between 15 and 19 years old with 0 to 7 years of education in Costa Rica

Graph1<-PlotTimeSeries("X07CR1519",data,ylim=NULL)

## X07M2024 -  Women between 20 and 24 years old with 0 to 7 years of education in Mexico

Graph2<-PlotTimeSeries("X07M2024",data, ylim="country")

## X12B3039 - Women between 35 and 39 years old with more than 12 years of education in Brazil

Graph3<-PlotTimeSeries("X12B5039",data, ylim="all")

## X8CH2529 - Women between 25 and 29 years old with 8 to 11 years of education in Chile

Graph4<-PlotTimeSeries("X8CH2529",data, ylim=c(-100,100))

## X12CR1519 - Women between 15 and 19 years old with at least 12 years of education in Costa Rica
Graph5<-PlotTimeSeries("X12CR1519",data, ylim=c(-105,100))

## X12B1519 - Women between 15 and 19 years old with at least 12 years of education in Costa Rica
Graph6<-PlotTimeSeries("X12B1519",data)



### Grid of variations by country ------------------------

#Finally, we will made a grid by age group and years of study for each country. These graphs are the one that are showed in the paper

#First, we generate the legend for the grid
legend<-get_only_legend(PlotTimeSeries("X12B40",data,legend = T))



#grid for Costa Rica
grid1<-grid.arrange(
  arrangeGrob(PlotTimeSeries("X07CR1519",data,ylim = c(-105,100)), top=Age("X07CR1519"), left=study_years("X07CR1519")),
  arrangeGrob(PlotTimeSeries("X07CR2024",data,ylim = c(-105,100)), top=Age("X07CR2024")),
  arrangeGrob(PlotTimeSeries("X07CR2529",data,ylim = c(-105,100)), top=Age("X07CR2529")),
  arrangeGrob(PlotTimeSeries("X07CR3034",data,ylim = c(-105,100)), top=Age("X07CR3034")),
  arrangeGrob(PlotTimeSeries("X07CR3539",data,ylim = c(-105,100)), top=Age("X07CR3539")),
  arrangeGrob(PlotTimeSeries("X07CR40",data,ylim = c(-105,100)), top="40+"),
  arrangeGrob(PlotTimeSeries("X8CR1519",data,ylim = c(-105,100)), left=study_years("X8CR1519")),
  arrangeGrob(PlotTimeSeries("X8CR2024",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X8CR2529",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X8CR3034",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X8CR3539",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X8CR40",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X12CR1519",data,ylim = c(-105,100)), left=study_years("X12CR1519")),
  arrangeGrob(PlotTimeSeries("X12CR2024",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X12CR2529",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X12CR3034",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X12CR3539",data,ylim = c(-105,100))),
  arrangeGrob(PlotTimeSeries("X12CR40",data,ylim = c(-105,100))),
  ncol=6,
  top = textGrob("Costa Rica \n Age group",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of schooling", gp=gpar(fontsize=15,font=2),rot=90),
  bottom=textGrob("Births in month",gp=gpar(fontsize=15,font=2))
 )
grid2<-grid.arrange(grid1,legend,ncol=1, heights=c(10,1))

# Grid for Brazil

grid3<-grid.arrange(
  arrangeGrob(PlotTimeSeries("X07B1519",data), top=Age("X07B1519"), left=study_years("X07CR1519")),
  arrangeGrob(PlotTimeSeries("X07B2024",data), top=Age("X07B2024")),
  arrangeGrob(PlotTimeSeries("X07B2529",data), top=Age("X07B2529")),
  arrangeGrob(PlotTimeSeries("X07B3034",data), top=Age("X07B3034")),
  arrangeGrob(PlotTimeSeries("X07B3539",data), top=Age("X07B3539")),
  arrangeGrob(PlotTimeSeries("X07B40",data), top="40+"),
  arrangeGrob(PlotTimeSeries("X8B1519",data), left=study_years("X8B1519")),
  arrangeGrob(PlotTimeSeries("X8B2024",data)),
  arrangeGrob(PlotTimeSeries("X8B2529",data)),
  arrangeGrob(PlotTimeSeries("X8B3034",data)),
  arrangeGrob(PlotTimeSeries("X8B3539",data)),
  arrangeGrob(PlotTimeSeries("X8B40",data)),
  arrangeGrob(PlotTimeSeries("X12B1519",data), left=study_years("X12B1519")),
  arrangeGrob(PlotTimeSeries("X12B2024",data)),
  arrangeGrob(PlotTimeSeries("X12B2529",data)),
  arrangeGrob(PlotTimeSeries("X12B3034",data)),
  arrangeGrob(PlotTimeSeries("X12B3539",data)),
  arrangeGrob(PlotTimeSeries("X12B40",data)),
  ncol=6,
  top = textGrob("Brazil \n Age group",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of schooling", gp=gpar(fontsize=15,font=2),rot=90),
  bottom=textGrob("Births in month",gp=gpar(fontsize=15,font=2))
)
grid4<-grid.arrange(grid3,legend,ncol=1, heights=c(10,1))


#Cuba was discarted from the paper due bad quality data
#Grid for Cuba
#grid5<-grid.arrange(
#  arrangeGrob(PlotTimeSeries("X07CB1519",data), top=Age("X07CB1519"), left=study_years("X07CB1519")),
#  arrangeGrob(PlotTimeSeries("X07CB2024",data), top=Age("X07CB2024")),
#  arrangeGrob(PlotTimeSeries("X07CB2529",data), top=Age("X07CB2529")),
#  arrangeGrob(PlotTimeSeries("X07CB3034",data), top=Age("X07CB3034")),
#  arrangeGrob(PlotTimeSeries("X07CB3539",data), top=Age("X07CB3539")),
#  arrangeGrob(PlotTimeSeries("X07CB40",data), top="40+"),
#  arrangeGrob(PlotTimeSeries("X8CB1519",data), left=study_years("X8CB1519")),
#  arrangeGrob(PlotTimeSeries("X8CB2024",data)),
#  arrangeGrob(PlotTimeSeries("X8CB2529",data)),
#  arrangeGrob(PlotTimeSeries("X8CB3034",data)),
#  arrangeGrob(PlotTimeSeries("X8CB3539",data)),
#  arrangeGrob(PlotTimeSeries("X8CB40",data)),
#  arrangeGrob(PlotTimeSeries("X12CB1519",data), left=study_years("X12CB1519")),
#  arrangeGrob(PlotTimeSeries("X12CB2024",data)),
#  arrangeGrob(PlotTimeSeries("X12CB2529",data)),
#  arrangeGrob(PlotTimeSeries("X12CB3034",data)),
#  arrangeGrob(PlotTimeSeries("X12CB3539",data)),
#  arrangeGrob(PlotTimeSeries("X12CB40",data)),
#  ncol=6,
#  top = textGrob("Cuba \n Age group",gp=gpar(fontsize=15,font=2)),
#  left= textGrob("Years of schooling", gp=gpar(fontsize=15,font=2),rot=90),
#   bottom=textGrob("Time",gp=gpar(fontsize=15,font=2))
#)
#grid6<-grid.arrange(grid5,legend,ncol=1, heights=c(10,1))

#Grid for Mexico

grid7<-grid.arrange(
  arrangeGrob(PlotTimeSeries("X07M1519",data), top=Age("X07M1519"), left=study_years("X07M1519")),
  arrangeGrob(PlotTimeSeries("X07M2024",data), top=Age("X07M2024")),
  arrangeGrob(PlotTimeSeries("X07M2529",data), top=Age("X07M2529")),
  arrangeGrob(PlotTimeSeries("X07M3034",data), top=Age("X07M3034")),
  arrangeGrob(PlotTimeSeries("X07M3539",data), top=Age("X07M3539")),
  arrangeGrob(PlotTimeSeries("X07M40",data), top="40+"),
  arrangeGrob(PlotTimeSeries("X8M1519",data), left=study_years("X8M1519")),
  arrangeGrob(PlotTimeSeries("X8M2024",data)),
  arrangeGrob(PlotTimeSeries("X8M2529",data)),
  arrangeGrob(PlotTimeSeries("X8M3034",data)),
  arrangeGrob(PlotTimeSeries("X8M3539",data)),
  arrangeGrob(PlotTimeSeries("X8M40",data)),
  arrangeGrob(PlotTimeSeries("X12M1519",data), left=study_years("X12M1519")),
  arrangeGrob(PlotTimeSeries("X12M2024",data)),
  arrangeGrob(PlotTimeSeries("X12M2529",data)),
  arrangeGrob(PlotTimeSeries("X12M3034",data)),
  arrangeGrob(PlotTimeSeries("X12M3539",data)),
  arrangeGrob(PlotTimeSeries("X12M40",data)),
  ncol=6,
  top = textGrob("Mexico \n Age group",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of schooling", gp=gpar(fontsize=15,font=2),rot=90),
  bottom=textGrob("Births in month",gp=gpar(fontsize=15,font=2))
)
grid8<-grid.arrange(grid7,legend,ncol=1, heights=c(10,1))


#Grid for Chile

grid9<-grid.arrange(
  arrangeGrob(PlotTimeSeries("X07CH1519",data), top=Age("X07CH1519"), left=study_years("X07CH1519")),
  arrangeGrob(PlotTimeSeries("X07CH2024",data), top=Age("X07CH2024")),
  arrangeGrob(PlotTimeSeries("X07CH2529",data), top=Age("X07CH2529")),
  arrangeGrob(PlotTimeSeries("X07CH3034",data), top=Age("X07CH3034")),
  arrangeGrob(PlotTimeSeries("X07CH3539",data), top=Age("X07CH3539")),
  arrangeGrob(PlotTimeSeries("X07CH40",data), top="40+"),
  arrangeGrob(PlotTimeSeries("X8CH1519",data), left=study_years("X8CH1519")),
  arrangeGrob(PlotTimeSeries("X8CH2024",data)),
  arrangeGrob(PlotTimeSeries("X8CH2529",data)),
  arrangeGrob(PlotTimeSeries("X8CH3034",data)),
  arrangeGrob(PlotTimeSeries("X8CH3539",data)),
  arrangeGrob(PlotTimeSeries("X8CH40",data)),
  arrangeGrob(PlotTimeSeries("X12CH1519",data), left=study_years("X12CH1519")),
  arrangeGrob(PlotTimeSeries("X12CH2024",data)),
  arrangeGrob(PlotTimeSeries("X12CH2529",data)),
  arrangeGrob(PlotTimeSeries("X12CH3034",data)),
  arrangeGrob(PlotTimeSeries("X12CH3539",data)),
  arrangeGrob(PlotTimeSeries("X12CH40",data)),
  ncol=6,
  top = textGrob("Chile \n Age group",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of schooling", gp=gpar(fontsize=15,font=2),rot=90),
  bottom=textGrob("Births in month",gp=gpar(fontsize=15,font=2))
)
grid10<-grid.arrange(grid9,legend,ncol=1, heights=c(10,1))


















#Now we will estimate the post-pandemic period, i.e, since January 2023


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


PlotTimeSeries2023("12B2024",data2023)
