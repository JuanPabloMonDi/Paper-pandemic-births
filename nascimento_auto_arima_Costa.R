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
data= read.csv("nascimentos_auto_arima_maio.csv", sep = ";")

### Given the name structure of each column of the dataset
# We create a function to obtain the country, age group and years of study from the name of the column
# For example: X07B2529 is Brazil, with age group 25-29 and between 0 and 7 years od study 
info_variable <- function(variable){
  # Extraer la información de la variable
  if (substr(variable,2,2)=="8"){
    anos_estudio<-substr(variable,2,2)
  }else{anos_estudio <- substr(variable, 2, 3)}
  
  # Crear un mapeo de códigos de país a nombres de país
  mapa_paises <- c("B" = "Brasil", "CR" = "Costa Rica", "M" = "Mexico", "CB" = "Cuba", "CH"="Chile")
  
  # Determinar la posición inicial para el código de país
  posicion_inicial <- ifelse(anos_estudio == "8", 3,4)
  
  # Extraer el código de país
  codigo_pais <- substr(variable, posicion_inicial, ifelse(substr(variable, posicion_inicial + 1, posicion_inicial + 1) %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), posicion_inicial, posicion_inicial + 1))
  
  # Obtener el nombre del país según el mapeo
  pais <- ifelse(codigo_pais %in% names(mapa_paises), mapa_paises[codigo_pais], "Otro")
  
  # Extraer la edad
  Idade2 <- substr(variable, nchar(variable) - 1, nchar(variable))
  Idade1 <- substr(variable, nchar(variable) - 3, nchar(variable)-2)
  
  if (anos_estudio=="07"){
    texto<-"at most 7"}else{if(anos_estudio=="8"){texto<-"8-11"}else{texto<-"12 or more"}}
  # Devolver la información como un vector
  return(c(anos_estudio = texto, pais = pais, Idade1 = Idade1,Idade2=Idade2))
}
pais<-function(Variable){info_variable(Variable)[2]}
anos_estudo<-function(Variable){info_variable(Variable)[1]}
Idade<-function(Variable){
  idade2<-info_variable(Variable)[4]
  if (idade2=="40"){return("40+")}else{
    paste(info_variable(Variable)[3],"-",info_variable(Variable)[4])}
}


Variable<-"X07CB40"

GraficoSerieTemporal<-function(Variable,data, legend=F,tittle=F){
  #Mudar por data[Variable] se falha
  columnas_pais=c()
  for (coluna in colnames(data)){
    if (pais(coluna)==pais(Variable)){
      columnas_pais=c(columnas_pais,coluna)
    }
  }
  data1<-data[c("Mes",columnas_pais)]
  data2<-data[colnames(data)[sapply(colnames(data),pais)!="Cuba"]]
  data2<-data2[colnames(data2)[sapply(colnames(data2),pais)!="Costa Rica"]]
  #maxlim<-max(data1[data1$Mes>"2020-10",][2:ncol(data1)], na.rm = T)
  #minlim<-min(data1[data1$Mes>"2020-10",][2:ncol(data1)], na.rm = T)
  maxlim<-max(data2[data2$Mes>"2020-10",][2:ncol(data2)], na.rm = T)+10
  minlim<-min(data2[data2$Mes>"2020-10",][2:ncol(data2)], na.rm = T)-10
  #maxlim<-max(data[data$Mes>"2020-10",][2:ncol(data)], na.rm = T)
  #minlim<-min(data[data$Mes>"2020-10",][2:ncol(data)], na.rm = T)
  
  
  if (nrow(data[!is.na(data[Variable]),][Variable])==0){return(ggplot() +
                                                                   labs(x = "", y = "", title = "")+
                                                                   annotate("text",x = 2021.665, y = (maxlim-minlim)/2, size = 10, label="No info",color = "black")+
                                                                   xlim(c(2020.83,2022.5))+ #Os limites horizontais do grafico
                                                                   ylim(c(minlim-10,maxlim+10)) #Os limites verticais do grafico
  )}else{
    #data[!is.na(data[Variable]),][Variable]
data_treino = ts(data[Variable], start = c(2012,1), end = c(2020,10), frequency = 12)
data_ts = ts(data[Variable], start = c(2012,1), end = c(2022,12), frequency = 12)
tedata = window(data_ts, start=c(2020,11), end=c(2022,12))

ModArima = auto.arima(data_treino, trace = T, stepwise = F, approximation = F)
prevArima=forecast(ModArima, h=36)
accuracy(prevArima)


titulos<-info_variable(Variable)
if (tittle==T){
  Titulo<-paste("Women between ",titulos[[3]],"-",titulos[[4]]," with ",titulos[[1]],"years of studies - ",titulos[[2]])
}else{Titulo=" "}
if (legend==T){
  posicionlegenda="bottom"
}else{posicionlegenda="none"}

Grafico<-autoplot(prevArima,color="blue",series="Forecast") + #Graficamos a serie temporal
  autolayer(tedata,series="Observed",colour = T)+ #Graficamos os data observados
  autolayer(prevArima$mean,series="Forecast",colour=T)+ #Graficamos as predicoes
  xlim(c(2020.8,2022.5))+ #Os limites horizontais do grafico
  ylim(c(min(minlim,-maxlim)),max(-minlim,maxlim))+ #Os limites verticais do grafico
  theme(plot.title = element_text(size=10), #Tamanho do titulo
        legend.position = posicionlegenda, #Posicion da legenda
        axis.title.x=element_blank(), #Tirar os valores de x
        axis.text.x=element_text(angle = 45, vjust = 0.5, hjust=1,size = 5), #Tirar os titulos de x
        axis.ticks.y = element_blank(), #Tirar os titulos de y
        axis.title.y = element_blank(), #Tirar os titulos de y
        axis.ticks.x=element_blank())+ #Tirar os valores de x 
  ggtitle(Titulo)+  #Estabelecemos o titulo do grafico
  geom_line(data = data.frame(x = c(max(time(prevArima$x)), min(time(prevArima$mean))), y = c(prevArima$x[length(prevArima$x)], prevArima$mean[1])),
            aes(x = x, y = y), color = "blue")+ #Reparar um buraco entre a ultima observacao e a primeira predicao que a funcao autoplot faz
  labs(x="Year", y="Births",colour="Values",legend.position = "top")+ #Nomes dos eixos X e Y
  geom_vline(xintercept=2020.833,color="#a3a3a3",alpha=0.95)+ #Linha horizontal da pandemia
  annotate("text",x = 2021.2, y = maxlim, label = "Pandemic", color = "#a3a3a3",size=2.5,fontface="bold")+  #Texto que diz Pandemic no grafico
  scale_color_manual(values=c("blue","red"))#Adjustamos os cores das legendas
 return(Grafico)}
}


#Essa funcao daqui, foi tirada do site, vai nos ajudar a obter uma lenda muito util para  grafico geral


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


#Comentarios: Fazer escala dependendo de cada pais

datainfo<-data.frame("Colunas"=colnames(data)[2:ncol(data)])
datainfo$Escolaridade<-lapply(datainfo$Colunas,anos_estudo)
datainfo$Idade<-datainfo$Colunas%>%lapply(Idade)
datainfo$Pais<-datainfo$Colunas%>%lapply(pais)

## X07CR2024 - mulheres de 0 a 7 anos de escolaridade entre 15 a 19 anos

Grafico1<-GraficoSerieTemporal("X07CR1519",data)

## X07CR2024 - mulheres de 0 a 7 anos de escolaridade entre 20 a 24 anos

Grafico2<-GraficoSerieTemporal("X07CR2024",data)

## X12CR2024 - mulheres de mais de 12 anos de escolaridade entre 20 a 24 anos

Grafico3<-GraficoSerieTemporal("X07CR2529",data)
Grafico4<-GraficoSerieTemporal("X07CR3034",data)
Grafico5<-GraficoSerieTemporal("X07CR3539",data)
Grafico6<-GraficoSerieTemporal("X07CR40",data)


# Create row and column titles
col.titles = unlist(unique(datainfo$Idade))
row.titles = unlist(unique(datainfo$Escolaridade))

legenda<-get_only_legend(GraficoSerieTemporal("X12B40",data,legend = T))
#Agora, vamos a grilha dos graficos, tenho certeza que isto pode ser mais simples mais nao sei como, tal vez um for
grid1<-grid.arrange(
  arrangeGrob(GraficoSerieTemporal("X07CR1519",data), top=Idade("X07CR1519"), left=anos_estudo("X07CR1519")),
  arrangeGrob(GraficoSerieTemporal("X07CR2024",data), top=Idade("X07CR2024")),
  arrangeGrob(GraficoSerieTemporal("X07CR2529",data), top=Idade("X07CR2529")),
  arrangeGrob(GraficoSerieTemporal("X07CR3034",data), top=Idade("X07CR3034")),
  arrangeGrob(GraficoSerieTemporal("X07CR3539",data), top=Idade("X07CR3539")),
  arrangeGrob(GraficoSerieTemporal("X07CR40",data), top="40+"),
  arrangeGrob(GraficoSerieTemporal("X8CR1519",data), left=anos_estudo("X8CR1519")),
  arrangeGrob(GraficoSerieTemporal("X8CR2024",data)),
  arrangeGrob(GraficoSerieTemporal("X8CR2529",data)),
  arrangeGrob(GraficoSerieTemporal("X8CR3034",data)),
  arrangeGrob(GraficoSerieTemporal("X8CR3539",data)),
  arrangeGrob(GraficoSerieTemporal("X8CR40",data)),
  arrangeGrob(GraficoSerieTemporal("X12CR1519",data), left=anos_estudo("X12CR1519")),
  arrangeGrob(GraficoSerieTemporal("X12CR2024",data)),
  arrangeGrob(GraficoSerieTemporal("X12CR2529",data)),
  arrangeGrob(GraficoSerieTemporal("X12CR3034",data)),
  arrangeGrob(GraficoSerieTemporal("X12CR3539",data)),
  arrangeGrob(GraficoSerieTemporal("X12CR40",data)),
  ncol=6,
  top = textGrob("Costa Rica \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
 )
grid2<-grid.arrange(grid1,legenda,ncol=1, heights=c(10,1))

#Agora, vamos a grilha dos graficos, tenho certeza que isto pode ser mais simples mais nao sei como, tal vez um for
grid3<-grid.arrange(
  arrangeGrob(GraficoSerieTemporal("X07B1519",data), top=Idade("X07B1519"), left=anos_estudo("X07CR1519")),
  arrangeGrob(GraficoSerieTemporal("X07B2024",data), top=Idade("X07B2024")),
  arrangeGrob(GraficoSerieTemporal("X07B2529",data), top=Idade("X07B2529")),
  arrangeGrob(GraficoSerieTemporal("X07B3034",data), top=Idade("X07B3034")),
  arrangeGrob(GraficoSerieTemporal("X07B3539",data), top=Idade("X07B3539")),
  arrangeGrob(GraficoSerieTemporal("X07B40",data), top="40+"),
  arrangeGrob(GraficoSerieTemporal("X8B1519",data), left=anos_estudo("X8B1519")),
  arrangeGrob(GraficoSerieTemporal("X8B2024",data)),
  arrangeGrob(GraficoSerieTemporal("X8B2529",data)),
  arrangeGrob(GraficoSerieTemporal("X8B3034",data)),
  arrangeGrob(GraficoSerieTemporal("X8B3539",data)),
  arrangeGrob(GraficoSerieTemporal("X8B40",data)),
  arrangeGrob(GraficoSerieTemporal("X12B1519",data), left=anos_estudo("X12B1519")),
  arrangeGrob(GraficoSerieTemporal("X12B2024",data)),
  arrangeGrob(GraficoSerieTemporal("X12B2529",data)),
  arrangeGrob(GraficoSerieTemporal("X12B3034",data)),
  arrangeGrob(GraficoSerieTemporal("X12B3539",data)),
  arrangeGrob(GraficoSerieTemporal("X12B40",data)),
  ncol=6,
  top = textGrob("Brasil \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
)
grid4<-grid.arrange(grid3,legenda,ncol=1, heights=c(10,1))


grid5<-grid.arrange(
  arrangeGrob(GraficoSerieTemporal("X07CB1519",data), top=Idade("X07CB1519"), left=anos_estudo("X07CB1519")),
  arrangeGrob(GraficoSerieTemporal("X07CB2024",data), top=Idade("X07CB2024")),
  arrangeGrob(GraficoSerieTemporal("X07CB2529",data), top=Idade("X07CB2529")),
  arrangeGrob(GraficoSerieTemporal("X07CB3034",data), top=Idade("X07CB3034")),
  arrangeGrob(GraficoSerieTemporal("X07CB3539",data), top=Idade("X07CB3539")),
  arrangeGrob(GraficoSerieTemporal("X07CB40",data), top="40+"),
  arrangeGrob(GraficoSerieTemporal("X8CB1519",data), left=anos_estudo("X8CB1519")),
  arrangeGrob(GraficoSerieTemporal("X8CB2024",data)),
  arrangeGrob(GraficoSerieTemporal("X8CB2529",data)),
  arrangeGrob(GraficoSerieTemporal("X8CB3034",data)),
  arrangeGrob(GraficoSerieTemporal("X8CB3539",data)),
  arrangeGrob(GraficoSerieTemporal("X8CB40",data)),
  arrangeGrob(GraficoSerieTemporal("X12CB1519",data), left=anos_estudo("X12CB1519")),
  arrangeGrob(GraficoSerieTemporal("X12CB2024",data)),
  arrangeGrob(GraficoSerieTemporal("X12CB2529",data)),
  arrangeGrob(GraficoSerieTemporal("X12CB3034",data)),
  arrangeGrob(GraficoSerieTemporal("X12CB3539",data)),
  arrangeGrob(GraficoSerieTemporal("X12CB40",data)),
  ncol=6,
  top = textGrob("Cuba \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
)
grid6<-grid.arrange(grid5,legenda,ncol=1, heights=c(10,1))

grid7<-grid.arrange(
  arrangeGrob(GraficoSerieTemporal("X07M1519",data), top=Idade("X07M1519"), left=anos_estudo("X07M1519")),
  arrangeGrob(GraficoSerieTemporal("X07M2024",data), top=Idade("X07M2024")),
  arrangeGrob(GraficoSerieTemporal("X07M2529",data), top=Idade("X07M2529")),
  arrangeGrob(GraficoSerieTemporal("X07M3034",data), top=Idade("X07M3034")),
  arrangeGrob(GraficoSerieTemporal("X07M3539",data), top=Idade("X07M3539")),
  arrangeGrob(GraficoSerieTemporal("X07M40",data), top="40+"),
  arrangeGrob(GraficoSerieTemporal("X8M1519",data), left=anos_estudo("X8M1519")),
  arrangeGrob(GraficoSerieTemporal("X8M2024",data)),
  arrangeGrob(GraficoSerieTemporal("X8M2529",data)),
  arrangeGrob(GraficoSerieTemporal("X8M3034",data)),
  arrangeGrob(GraficoSerieTemporal("X8M3539",data)),
  arrangeGrob(GraficoSerieTemporal("X8M40",data)),
  arrangeGrob(GraficoSerieTemporal("X12M1519",data), left=anos_estudo("X12M1519")),
  arrangeGrob(GraficoSerieTemporal("X12M2024",data)),
  arrangeGrob(GraficoSerieTemporal("X12M2529",data)),
  arrangeGrob(GraficoSerieTemporal("X12M3034",data)),
  arrangeGrob(GraficoSerieTemporal("X12M3539",data)),
  arrangeGrob(GraficoSerieTemporal("X12M40",data)),
  ncol=6,
  top = textGrob("Mexico \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
)
grid8<-grid.arrange(grid7,legenda,ncol=1, heights=c(10,1))


grid9<-grid.arrange(
  arrangeGrob(GraficoSerieTemporal("X07CH1519",data), top=Idade("X07CH1519"), left=anos_estudo("X07CH1519")),
  arrangeGrob(GraficoSerieTemporal("X07CH2024",data), top=Idade("X07CH2024")),
  arrangeGrob(GraficoSerieTemporal("X07CH2529",data), top=Idade("X07CH2529")),
  arrangeGrob(GraficoSerieTemporal("X07CH3034",data), top=Idade("X07CH3034")),
  arrangeGrob(GraficoSerieTemporal("X07CH3539",data), top=Idade("X07CH3539")),
  arrangeGrob(GraficoSerieTemporal("X07CH40",data), top="40+"),
  arrangeGrob(GraficoSerieTemporal("X8CH1519",data), left=anos_estudo("X8CH1519")),
  arrangeGrob(GraficoSerieTemporal("X8CH2024",data)),
  arrangeGrob(GraficoSerieTemporal("X8CH2529",data)),
  arrangeGrob(GraficoSerieTemporal("X8CH3034",data)),
  arrangeGrob(GraficoSerieTemporal("X8CH3539",data)),
  arrangeGrob(GraficoSerieTemporal("X8CH40",data)),
  arrangeGrob(GraficoSerieTemporal("X12CH1519",data), left=anos_estudo("X12CH1519")),
  arrangeGrob(GraficoSerieTemporal("X12CH2024",data)),
  arrangeGrob(GraficoSerieTemporal("X12CH2529",data)),
  arrangeGrob(GraficoSerieTemporal("X12CH3034",data)),
  arrangeGrob(GraficoSerieTemporal("X12CH3539",data)),
  arrangeGrob(GraficoSerieTemporal("X12CH40",data)),
  ncol=6,
  top = textGrob("Chile \n Age (years)",gp=gpar(fontsize=15,font=2)),
  left= textGrob("Years of study", gp=gpar(fontsize=15,font=2),rot=90)
)
grid10<-grid.arrange(grid9,legenda,ncol=1, heights=c(10,1))
