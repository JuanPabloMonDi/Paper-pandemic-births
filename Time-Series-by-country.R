### Require packages #--------------------------------
rm( list = ls( ) )
graphics.off( )

library(seasonal)
library(openxlsx)
library(zoo)
library(ggpubr)
library(grid)
library(gridExtra)
library(forecast)
library("xlsx")
library("ggplot2")

### Import dataset #--------------------
data= read.csv("Variation_of_births.csv", sep = ";")



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


PlotTimeSeries<-function(Variable,data, legend=F,tittle=F, ylim="notCR"){

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
    maxlim<-max(data2[data2$Mes>"2020-10",][2:ncol(data2)], na.rm = T)+20
    minlim<-min(data2[data2$Mes>"2020-10",][2:ncol(data2)], na.rm = T)-20
  }else{
  if (ylim%in%c("country","notCR")){
  maxlim<-max(data2[data2$Mes>"2020-10",][2:ncol(data2)], na.rm = T)+20
  minlim<-min(data2[data2$Mes>"2020-10",][2:ncol(data2)], na.rm = T)-20
  }else{if (ylim=="all"){
  maxlim<-max(data[data$Mes>"2020-10",][2:ncol(data)], na.rm = T)+20
  minlim<-min(data[data$Mes>"2020-10",][2:ncol(data)], na.rm = T)-20
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
prevArima=forecast(ModArima, h=36)
accuracy(prevArima)

#If true, we add the title and legend to the plot
titles<-info_variable(Variable)
if (tittle==T){
  titlee<-paste("Women between ",titles[[3]],"-",titles[[4]]," with ",titles[[1]],"years of studies - ",titles[[2]])
}else{titlee=" "}
if (legend==T){
  legend_position="bottom"
}else{legend_position="none"}

Graph<-autoplot(prevArima,color="blue",series="Forecast") + #plot time series
  autolayer(tedata,series="Observed",colour = T)+ #plot observed data
  autolayer(prevArima$mean,series="Forecast",colour=T)+ #plot predicted data
  xlim(c(2020.8,2022.5))+ #Horizontal limits
  ylim(c(min(minlim,-maxlim)),max(-minlim,maxlim))+ #Range of plot
  theme(plot.title = element_text(size=10), #Title size
        legend.position = legend_position, #Legend position
        axis.title.x=element_blank(), #Remove x axis lab
        axis.text.x=element_text(angle = 45, vjust = 0.5, hjust=1,size = 5), #Modify x axis values
        axis.ticks.y = element_blank(),  # Remove y axis values
axis.title.y = element_blank(), # Remove y labs
        axis.ticks.x=element_blank())+ # Remove x ticks
  ggtitle(titlee)+  #Add title to the graph
  geom_line(data = data.frame(x = c(max(time(prevArima$x)), min(time(prevArima$mean))), y = c(prevArima$x[length(prevArima$x)], prevArima$mean[1])),
            aes(x = x, y = y), color = "blue")+ #Fill the hole between the last observation and the first prediction that is made by the autoplot function
  labs(x="Year", y="Variation",colour="Values",legend.position = "top")+ #Add names for X and Y axis
  geom_vline(xintercept=2020.833,color="#a3a3a3",alpha=0.95)+ #Horizontal line to mark the beginning of pandemic
  annotate("text",x = 2021.2, y = maxlim, label = "Pandemic", color = "#a3a3a3",size=2.5,fontface="bold")+  #Add text "Pandemic" to the graph
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

## X07CR2024 -  Women between 15 and 19 years old with 0 to 7 years of education in Costa Rica

Graph1<-PlotTimeSeries("X07CR1519",data,ylim=NULL)

## X07CR2024 -  Women between 20 and 24 years old with 0 to 7 years of education in Mexico

Graph2<-PlotTimeSeries("X07M2024",data, ylim="country")

## X12CR2024 - Women between 30 and 39 years old with more than 12 years of education in Brazil

Graph3<-PlotTimeSeries("X12B3039",data, ylim="all")

## X12CR2024 - Women between 25 and 29 years old with 8 to 11 years of education in Chile

Graph4<-PlotTimeSeries("X8CH2529",data, ylim=c(-100,100))


### Grid of variations by country ------------------------

#Finally, we will made a grid by age group and years of study for each country. These graphs are the one that are showed in the paper

#First, we generate the legend for the grid
legend<-get_only_legend(PlotTimeSeries("X12B40",data,legend = T))



#grid for Costa Rica
grid1<-grid.arrange(
  arrangeGrob(PlotTimeSeries("X07CR1519",data,ylim = "country"), top=Age("X07CR1519"), left=study_years("X07CR1519")),
  arrangeGrob(PlotTimeSeries("X07CR2024",data,ylim = "country"), top=Age("X07CR2024")),
  arrangeGrob(PlotTimeSeries("X07CR2529",data,ylim = "country"), top=Age("X07CR2529")),
  arrangeGrob(PlotTimeSeries("X07CR3034",data,ylim = "country"), top=Age("X07CR3034")),
  arrangeGrob(PlotTimeSeries("X07CR3539",data,ylim = "country"), top=Age("X07CR3539")),
  arrangeGrob(PlotTimeSeries("X07CR40",data,ylim = "country"), top="40+"),
  arrangeGrob(PlotTimeSeries("X8CR1519",data,ylim = "country"), left=study_years("X8CR1519")),
  arrangeGrob(PlotTimeSeries("X8CR2024",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X8CR2529",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X8CR3034",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X8CR3539",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X8CR40",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X12CR1519",data,ylim = "country"), left=study_years("X12CR1519")),
  arrangeGrob(PlotTimeSeries("X12CR2024",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X12CR2529",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X12CR3034",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X12CR3539",data,ylim = "country")),
  arrangeGrob(PlotTimeSeries("X12CR40",data,ylim = "country")),
  ncol=6,
  top = textGrob("Costa Rica \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
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
  top = textGrob("Brazil \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
)
grid4<-grid.arrange(grid3,legend,ncol=1, heights=c(10,1))

#Grid for Cuba
grid5<-grid.arrange(
  arrangeGrob(PlotTimeSeries("X07CB1519",data), top=Age("X07CB1519"), left=study_years("X07CB1519")),
  arrangeGrob(PlotTimeSeries("X07CB2024",data), top=Age("X07CB2024")),
  arrangeGrob(PlotTimeSeries("X07CB2529",data), top=Age("X07CB2529")),
  arrangeGrob(PlotTimeSeries("X07CB3034",data), top=Age("X07CB3034")),
  arrangeGrob(PlotTimeSeries("X07CB3539",data), top=Age("X07CB3539")),
  arrangeGrob(PlotTimeSeries("X07CB40",data), top="40+"),
  arrangeGrob(PlotTimeSeries("X8CB1519",data), left=study_years("X8CB1519")),
  arrangeGrob(PlotTimeSeries("X8CB2024",data)),
  arrangeGrob(PlotTimeSeries("X8CB2529",data)),
  arrangeGrob(PlotTimeSeries("X8CB3034",data)),
  arrangeGrob(PlotTimeSeries("X8CB3539",data)),
  arrangeGrob(PlotTimeSeries("X8CB40",data)),
  arrangeGrob(PlotTimeSeries("X12CB1519",data), left=study_years("X12CB1519")),
  arrangeGrob(PlotTimeSeries("X12CB2024",data)),
  arrangeGrob(PlotTimeSeries("X12CB2529",data)),
  arrangeGrob(PlotTimeSeries("X12CB3034",data)),
  arrangeGrob(PlotTimeSeries("X12CB3539",data)),
  arrangeGrob(PlotTimeSeries("X12CB40",data)),
  ncol=6,
  top = textGrob("Cuba \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
)
grid6<-grid.arrange(grid5,legend,ncol=1, heights=c(10,1))

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
  top = textGrob("Mexico \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
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
  top = textGrob("Chile \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
)
grid10<-grid.arrange(grid9,legend,ncol=1, heights=c(10,1))
