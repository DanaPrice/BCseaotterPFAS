rm(list = ls())
library(readxl)
library(ggplot2) 

Region<-c("Alaska", "British Columbia", "Washington", "California")
SumPFAS<-c("13.1", "11.12", "8.69", "117.1")
df<-data.frame(Region,SumPFAS)
View(df)
is.numeric(df$SumPFAS)
df$SumPFAS<-as.numeric(df$SumPFAS)
is.factor(df$Region)
df$Region<-as.factor(df$Region)
ggplot(df,aes(x=Region,y=SumPFAS)+geom_bar())

ggplot(df,aes(x=Region,y=SumPFAS)+geom_bar()+theme(text=element_text(size=8))+theme(axis.line=element_line(colour="black",size=0.5,linetype="solid")))

       
       
       
ggplot(df,aes(x=Region, y=SumPFAS)+geom_bar())


ggplot(df,aes(x=Region, y=SumPFAS)+geom_bar())
