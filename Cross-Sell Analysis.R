# Input load. Please do not change #
`dataset` = read.csv('', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE); # csv file extracted from PowerBI
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
# Calculating frequency of items in an order
freq<-as.data.frame(table(dataset$OrderID))
#length(which(freq$Freq==1))/length(freq$Freq) [1] 0.174813 - # We need to balance the sample later
colnames(freq)[1]<-"OrderID"

if (!require(plyr)) {
  install.packages("plyr")
  require(plyr)
}

dd <- join_all(list(dataset, freq), by='OrderID', type='left')
rm(dataset, freq)
dd$MoreThanOne<-ifelse(dd$Freq==1,0,1)
#levels(dd$CategoryName3)
dd$Clothing<-ifelse(dd$CategoryName3=="Clothing",1,0)
dd$FurryFootwear<-ifelse(dd$CategoryName3=="Furry Footwear",1,0)
dd$NovelityItems<-ifelse(dd$CategoryName3=="Novelty Items",1,0)
dd$PackagingMaterials<-ifelse(dd$CategoryName3=="Packaging Materials",1,0)
dd$TShirts<-ifelse(dd$CategoryName3=="T-Shirts",1,0)
dd$Toys<-ifelse(dd$CategoryName3=="Toys",1,0)
dd$USBNovelties<-ifelse(dd$CategoryName3=="USB Novelties",1,0)
#head(dd)
dd<-dd[,c(-2,-3)]
#head(dd)
dd<-aggregate(. ~ OrderID, FUN = sum, data=dd)
#head(dd)
dd$MoreThanOne<-ifelse(dd$MoreThanOne>0,1,0)
dd$Clothing<-ifelse(dd$Clothing>0,1,0)
dd$FurryFootwear<-ifelse(dd$FurryFootwear>0,1,0)
dd$NovelityItems<-ifelse(dd$NovelityItem>0,1,0)
dd$PackagingMaterials<-ifelse(dd$PackagingMaterials>0,1,0)
dd$TShirts<-ifelse(dd$TShirts>0,1,0)
dd$Toys<-ifelse(dd$Toys>0,1,0)
dd$USBNovelties<-ifelse(dd$USBNovelties>0,1,0)
dd<-dd[,-1]
#head(dd)

# Make all variables as factors
for (i in 1: length(colnames(dd))){
  dd[,i]<-as.factor(dd[,i])
}
rm(i)

# Balancing the sample
if (!require(DMwR)) {
  install.packages("DMwR")
  require(DMwR)
}
# plot(dd$MoreThanOne)
dd <- SMOTE(MoreThanOne ~ ., dd, perc.over = 300,perc.under = 200)
#plot(dd$MoreThanOne)
eq1<-glm(MoreThanOne~., data=dd, family = binomial)
#rm(dd)
table<-eq1$coefficients
rm(eq1)
table<-round(log(table),2)
table<-table[-1]
table<-as.data.frame(table)
table$Product<-c("Clothing", "Furry Footwear", "Novelity Items", "Packaging Materials", "T-Shirts", "Toys", "USB Novelties")
names(table)<-c("Coefficient", "Product")
table$Coefficient<-as.numeric(table$Coefficient)
table<-table[order(table$Coefficient),]
library(ggplot2)
theme_set(theme_bw())

ggplot(table, aes(x=Product, y=Coefficient)) + 
  geom_segment(aes(y = mean(table$Coefficient)-3*sd(table$Coefficient), 
                   x = Product, 
                   yend = Coefficient, 
                   xend = Product), 
               color = "green4",
               size = 2) +
  labs(title="Cross-sell Analysis", size=10, 
       subtitle="Probability Coefficient per Product") +
  ylim(mean(table$Coefficient)-3*sd(table$Coefficient), mean(table$Coefficient)+1.5*sd(table$Coefficient)) +
  geom_point(stat='identity', fill="green4", size=10) +
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size=22))+
  coord_flip()


