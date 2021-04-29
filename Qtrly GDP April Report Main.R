#load packages

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(forecast)

#set working directory
setwd("G:/YOUR DIrectory/GDP GGPLOTs")

#load data
df<-read_excel("Table 1.17.1.xls", sheet = "data")
class(df)
head(df)
str(df)


#Create quarterly data for plot

gdpts <- ts(df$GDP, start = 2017, frequency = 4)
gdpd <- data.frame(GDP = as.numeric(gdpts),
                   QTR = as.numeric(time(gdpts)))

#create feature to make negative values red
gdpd2 <- mutate(gdpd, growth = ifelse(GDP >= 0, "pos", "neg"))

a<-ggplot(gdpd2, aes(QTR, GDP, fill = growth)) +
  geom_rect(xmin=2020.00, xmax=2021.00, 
            ymin=-40, ymax=40, fill="grey95", alpha=0.12, col="grey90")+
  geom_bar(stat = "identity") + scale_fill_manual(values = c(pos = "blue", neg = "red"))+
  theme_minimal()+theme( panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         legend.position = "none",
                         plot.caption = element_text(hjust = 0,size = 9),plot.title = element_text(size = 15,face = "bold"),plot.subtitle = element_text(size = 8))+
  labs(fill = 'Quarterly Percent Change', title = "The Economic Monitor: Quarterly GDP",subtitle = "Seasonal Adjusted", caption="Source:Bureau of Economic Analysis")+
  xlab("") +ylab("")+scale_y_continuous(limits = c(-40, 40),labels = function(GDP) paste0(GDP, "%"),expand = c(0, 0))+
  scale_x_continuous(expand = c(0,0)) +
  annotate("text", x = 2020.00, y =35,colour = "black", label = "COVID-19 Recession",fontface = 'italic',size = 2.3)

  #annotate("text", x = as.Date("2020-09-01"), y = 2.6,colour = "blue", label = "Fed Inflation Target Rate",fontface = 'italic', size=2)+
  #annotate("segment", x = as.Date("2020-09-01"), xend = as.Date("2020-09-01"), y = 2.05, yend = 2.4,colour = "blue")


#load data
df2<-read_excel("GDPContribute.xls", sheet = "data")
class(df2)
head(df2)


b<-df2%>%ggplot( aes(x=Measure, y=`2020Q4`,fill = `2020Q4`>0))+
  geom_bar(position="dodge",stat="identity",show.legend = FALSE) +coord_flip()+
  geom_text(aes(label = `2020Q4`),size =3, vjust = 1,hjust=1,family = "sans",fontface ="bold")+
  scale_y_continuous(limits = c(-4.5,3), expand = c(0, 0))+labs(title = "Contributions to Percent Change in Real GDP",subtitle="Seasonally Adjusted")+theme_bw()+
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank(),plot.caption = element_text(hjust = 0),plot.title = element_text(size = 11),plot.subtitle = element_text(size = 8),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_manual(values = c("Red", "#0072B2"))+
  scale_x_discrete(
    limits=c("Government Spending","Imports", "Exports", "Inventory Investment", "Residential Fixed Investment", "Nonresidential Fixed Investment",   "Consumer Spending"), 
    labels=c("Government Spending","Imports", "Exports", "Inventory Investment", "Residential Fixed Investment", "Nonresidential Fixed Investment",   "Consumer Spending"))

#from cowplot make two plots in one output
plot_grid(a, b)

#save image
ggsave("QtrlyGDP.png")
