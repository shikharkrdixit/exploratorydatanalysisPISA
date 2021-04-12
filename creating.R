setwd("D:/R/Visualization TDS")
#unzip("./archive.zip")

data <- read.csv(file = "Pisa mean perfromance scores 2013 - 2015 Data.csv",encoding = "UTF-8-BOM",na.string='..')
str(data)
head(data)
ncol(data)
nrow(data)
unique(data$Series.Code)
data[1,1]
library(tidyverse)
dataf <- data[1:1161,c(1,4,7)] %>%
  pivot_wider(names_from = Series.Code,values_from = X2015..YR2015.) %>%
  rename(CountryName=ï..Country.Name,Maths=LO.PISA.MAT,MathsF=LO.PISA.MAT.FE,MathsM=LO.PISA.MAT.MA,Reading=LO.PISA.REA,ReadingF=LO.PISA.REA.FE,ReadingM=LO.PISA.REA.MA,Science=LO.PISA.SCI,ScienceF=LO.PISA.SCI.FE,ScienceM=LO.PISA.SCI.MA)%>%
  drop_na()


#install.packages("ggmap")
library(ggmap)
#R.version
wrldmap <- map_data("world")
str(wrldmap)
mrgddata <- merge(wrldmap,dataf,by.x="region",by.y="CountryName")
mrgddata <- mrgddata[order(mrgddata$group,mrgddata$order),]
head(mrgddata)
ggplot(mrgddata) +
  aes(x=long,y=lat,group=group) + geom_polygon() + aes(fill=region) +
  theme_dark()


head(dataf)

library(ggplot2)
ggplot(dataf) +
  aes(x=reorder(CountryName,Maths),y=Maths) +
  geom_bar(stat = 'identity') +
  aes(fill=Maths) +
  coord_flip() +
  scale_fill_gradient(name="Score Level") +
  geom_hline(yintercept = mean(dataf$Maths)) +
  theme_grey() +
  labs(x="Country Name",y="Math Score",title = "Graph Relation b/w Math & Countries") +
  geom_hline(yintercept = mean(dataf$Maths),size=1,col="salmon3")

ggplot(dataf) +
  aes(x=reorder(CountryName,Reading),Reading) +
  geom_bar(stat = 'identity') +
  aes(fill=Reading) +
  coord_flip() +
  scale_fill_gradient(name="Score Level") +
  theme_classic() +
  labs(x="Country Name",y="Reading Score",title="Graoh Relation b/w Reading & Countries") +
  geom_hline(yintercept = mean(dataf$Reading),size=1,col="seashell4")
  
ggplot(dataf) +
  aes(x=reorder(CountryName,Science),y=Science) +
  geom_bar(stat='identity') +
  aes(fill=Science) +
  coord_flip() +
  scale_fill_gradient("Score Level") +
  labs(x="Country Name",y="Science Score",title="Graph Relation b/w Science & Countries") +
  theme_bw() +
  geom_hline(yintercept = mean(dataf$Science),size=1,col="sienna3")





dataf2 <- dataf[,c(1,3,4,6,7,9,10)] %>%
  pivot_longer(c(2,3,4,5,6,7),names_to = 'SubjectGender')

head(dataf2)

ggplot(dataf2)+
  aes(x=SubjectGender,y=value)+
  geom_point(pos="jitter",alpha=0.25) +
  geom_boxplot(alpha=0.75) +
  aes(fill=SubjectGender) +
  scale_fill_manual(values = c("wheat4","tomato","wheat","violet","seashell3","salmon")) +
  theme_linedraw() +
  labs(x="Subject and Gender",y="Values/Scores")+
  facet_wrap(.~SubjectGender,scales = "free_x",nrow=3,ncol=2)


?rcorr

#install.packages("Hmisc")
library(Hmisc)
dataf3 <- dataf[,c(1,3,4,6,7,9,10)]
res <- cor(dataf3[,-1]) #Pearson Method measures the strength of linear relation ship b/w two variables. VAlues is always betweeen -1 and 1.
res

#?cor
rcorr(as.matrix(dataf3[,-1]))$P
#WE cannot use cor.test since there are multiple columns and it'll be a pita to write all of them. Smaller the value better tge significant correlation.


#install.packages("corrplot")
library(corrplot)
corrplot(res,order="hclust",type = "upper",tl.col = "black",tl.cex =0.5)
#?cor
head(dataf)

datafg <- mutate(dataf[,1],MathDiff = ((dataf$MathsF-dataf$MathsM)/dataf$MathsM)*100,
                 ScienceDiff = (dataf$ScienceF-dataf$ScienceM)/dataf$ScienceM*100,
                 ReadingDiff = (dataf$ReadingF-dataf$ReadingM)/dataf$ReadingM*100,
                 Total=dataf$Maths+dataf$Science+dataf$Reading,
                 AverageDiff=(MathDiff+ScienceDiff+ReadingDiff)/3)
#head(datafg)


#Maths
ggplot(datafg)+
  aes(x=reorder(CountryName,MathDiff),y=MathDiff)+
  geom_bar(stat='identity') + aes(fill=MathDiff) +
  scale_fill_gradient("Math Difference") +
  geom_hline(yintercept = mean(datafg$MathDiff),size=1,col="brown") +
  labs(x="Country Names",y="Math Diff %",title="Math Diff v/s Country") + coord_flip() +
  theme_bw()

#Science
ggplot(datafg) +
  aes(x=reorder(CountryName,ScienceDiff),y=ScienceDiff) +
  geom_bar(stat='identity') + aes(fill=ScienceDiff) +
  scale_fill_gradient("Science Difference",) +
  geom_hline(yintercept = mean(datafg$ScienceDiff),size=1,col="purple") +
  labs(x="Country Names",y="Science Diff %",title="Science Diff v/s Country") +
  coord_flip() +
  theme_bw()

#Reading
ggplot(datafg) +
  aes(x=reorder(CountryName,ReadingDiff),y=ReadingDiff) +
  geom_bar(stat='identity') + aes(fill=ReadingDiff) +
  scale_fill_gradient("Reading Difference Palette") +
  geom_hline(yintercept = mean(datafg$ReadingDiff),size=1,col="tan2") +
  labs(x="Country Names",y="Reading Diff",title="Reading Diff v/s Country") +
  coord_flip() +
  theme_bw()


#Avverage
ggplot(datafg)+
  aes(x=reorder(CountryName,AverageDiff),y=AverageDiff) +
  geom_bar(stat='identity') + aes(fill=AverageDiff) +
  scale_fill_gradient("Average Diff") +
  geom_hline(yintercept = mean(datafg$AverageDiff),size=1,col="tomato3") +
  labs(x="Country Name",y="Average Diff",title="Average Diff v/s Country") + 
  coord_flip() + theme_bw()


#Total
ggplot(datafg) +
  aes(x=AverageDiff,y=Total) +
  geom_jitter() +
  geom_smooth(fill="springgreen4",alpha=0.50,col="snow4") + theme_light() +
  labs(x="Average Diff",y="Total",title="Average Diff v/s Total")
