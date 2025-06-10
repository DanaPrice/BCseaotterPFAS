library(ggplot2)
library(readxl)
PFAS <- read_excel("PFASdata.xlsx",sheet = "Processed")

# [PFAS] vs Year ----------------------------------------------------------

#[total PFAS in livers] vs year of death
PFASL<-subset(PFAS,Type=="L")
yearfit<-lm(PFASL$Year~log(PFASL$Total))
summary(yearfit) #P=0.2519, adj.r^2=0.0475 -> no relationship
ggplot(PFAS, aes(x=Year, y=Total))+geom_point()+theme(text = element_text(size=8))



# [PFAS liver] vs [PFAS muscle] -------------------------------------------


#[PFAS liver] vs [PFAS muscle]
shapiro.test(PFAS$TotalL) #P=0.1042 -> normally distributed
shapiro.test(PFAS$TotalSM) #P=0.01819 -> not normally distributed (small sample size, n=5)
is.factor(PFAS$Type)
as.factor(PFAS$Type)
wilcox.test(Total~Type,data=PFAS)#P=0.003135 -> significant difference



# PFAS by age -------------------------------------------------------------

PFASL<-subset(PFASL,Type == "L")#only use liver results

shapiro.test(PFASL$Total) #P=0.1042 -> normal distribution
subadult<-subset(PFASL,Age == "subadult")
shapiro.test(subadult$Total) #P=0.3049 -> normal distribution
adult<-subset(PFASL,Age == "adult")
shapiro.test(adult$Total) #P=0.1959 -> normal distribution
juvenile<-subset(PFASL,Gen.Age == "juvenile")
shapiro.test(juvenile$Total) #fails test by sample size, n=2 -> not normal distribution
#cannot use anova 
kruskal.test(PFASL$Total~PFASL$Gen.Age) #not sig p=0.22

PAHadsub<-subset(PAHE,Age == "subadult"|Age=="adult")
View(PAHadsub)
wilcox.test(Total~Age,data=PAHadsub) #P=0.8858 -> not significant

PFASL$Gen.Age <- factor(PFASL$Gen.Age,levels=c('juvenile','subadult','adult')) #reorder age groups so x-axis increases from L to R
ggplot(PFASL, aes(x=Gen.Age, y=Total))+geom_boxplot()+theme(text = element_text(size=8))+ theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"))



# carcass condition -------------------------------------------------------
PFASL<-subset(PFASL,Type == "L")#only use liver results

shapiro.test(PFASL$Total) #P=0.1042 -> normal distribution
mid<-subset(PFASL,CC == "3")
shapiro.test(mid$Total) #P=0.4643 -> normal distribution
moderate<-subset(PFASL,CC == "3.5")
shapiro.test(moderate$Total) #P=0.8943 -> normal distribution
adv<-subset(PFASL,CC == "4")
shapiro.test(adv$Total) #P=0.5242 -> normal distribution
is.factor(PFASL$CC)
as.factor(PFASL$CC)
oneway.test(PFASL$Total~PFASL$CC) #P=0.8667 -> not significant

# location ----------------------------------------------------------------

PFASL<-subset(PFAS,Type=="L")
PFASN<-subset(PFASL,NS=="North")
PFASS<-subset(PFASL,NS=="South")
shapiro.test(PFASN$Total) #P=0.4934 -> normal distribution
shapiro.test(PFASS$Total) #P=0.1055 -> normal distribution
is.factor(PFASL$NS)
as.factor(PFASL$NS)
t.test(Total~NS,data=PFASL) #P=0.01421
ggplot(PFASL, aes(x=NS, y=Total))+geom_boxplot() 


