rm(list = ls()) 

library(ggplot2)


salesdata<-read.csv("SalesData.csv",na.strings = c("","?","??","???",'""'))#convert missing/ambigous values to NA
summary(salesdata)
str(salesdata)

##############################remove Order.ID
salesdata$Order.ID<-NULL


###################### Order.Date AND Ship.Date
same_format <- all(grepl("^\\d{2}-\\d{2}-\\d{4}$", salesdata$date_col)) ||
  all(grepl("^\\d{2}/\\d{2}/\\d{4}$", salesdata$date_col))# checks if the data is all in the same format
print(same_format)#returns true meaning dates are all in the same format
#find the time between shipment and ordering in number of days 
salesdata$timetoship<-as.integer(as.character( as.Date(salesdata$Ship.Date, format = "%m/%d/%Y")-as.Date(salesdata$Order.Date,format = "%m/%d/%Y")))
summary(salesdata$timetoship)#time to ship value range is from 0 to 50
salesdata$Order.Date<-NULL#drop column
salesdata$Ship.Date<-NULL#drop column



########################### Sales.Channel
table(salesdata$Sales.Channel)#values have offline, online and yes, assuming yes means online
summary(salesdata$Sales.Channel)
#salesdata$Sales.Channel<-salesdata[salesdata$Sales.Channel,]
salesdata$Sales.Channel<-ifelse(salesdata$Sales.Channel=="YES" | salesdata$Sales.Channel=="Online",1,0)
salesdata<-salesdata[!is.na(salesdata$Sales.Channel),] #drop rows with values
table(salesdata$Sales.Channel)
plot(table(salesdata$Sales.Channel))
summary(salesdata$Sales.Channel)
#drop 


############# Order Priority
table(salesdata$Order.Priority)#seems clean enough,convert each to a number using factors
salesdata$Order.Priority<-as.factor(salesdata$Order.Priority)
levels(salesdata$Order.Priority)
salesdata$Order.Priority<-as.numeric(salesdata$Order.Priority)#map priority values to corresponding factor number list positions
plot(table(salesdata$Order.Priority))



################ Region
plot(table(salesdata$Region))#not a lot of then, convert to factors and map to numbers
salesdata$Region<- as.numeric(as.factor(salesdata$Region))
summary(salesdata$Region)
salesdata<-salesdata[!is.na(salesdata$Region),]#drop rows with region as NA
summary(salesdata$Region)

sum(is.na(salesdata))


####################### ItemType
summary(salesdata$ItemType)
table(salesdata$ItemType)
salesdata$ItemType[is.na(salesdata$ItemType)]<-"None"#assign NA to level None
#none item type interpret as NA
salesdata$ItemType<-as.factor(salesdata$ItemType)
levels(salesdata$ItemType)
salesdata$ItemType<-as.numeric(salesdata$ItemType)#map item to item level




####################### Units.Sold  ###binned
summary(salesdata$Units.Sold)#all numeric
sd(salesdata$Units.Sold)
hist(salesdata$Units.Sold)
boxplot(salesdata$Units.Sold)
plot(density(salesdata$Units.Sold))
mean(salesdata$Units.Sold)-2*sd(salesdata$Units.Sold)#min value is not an outlier
mean(salesdata$Units.Sold)+2*sd(salesdata$Units.Sold)#max value is not an outlier
salesdata$Units.Sold<-as.numeric(as.character(cut(salesdata$Units.Sold,c(0,2000,4000,6000,8000,10000),right = FALSE, labels = c(1,2,3,4,5))))#right is referring to right intervals open or closed
summary(salesdata$Units.Sold)





####################### Units.Price #binned
summary(salesdata$Unit.Price)#all numeric

plot(density(salesdata$Unit.Price))
salesdata$Unit.Price<-as.numeric(as.character(cut(salesdata$Unit.Price,c(0,150,300,450,600,750),right = FALSE, labels = c(1,2,3,4,5))))
summary(salesdata$Unit.Price)
hist(salesdata$Units.Sold)
mean(salesdata$Unit.Price)-2*sd(salesdata$Unit.Price)#min value is not an outlier
mean(salesdata$Unit.Price)+2*sd(salesdata$Unit.Price)#max value is not an outlier
hist(salesdata$Unit.Price)

sum(is.na(salesdata))#no more NA values in the dataset 85% of rows remaining


########################### MIN MAX NORMALIZATION

#timetoship
newmin<-0
newmax<-10
timetoship_<- ((salesdata[,"timetoship"]-min(salesdata$timetoship))/ (max(salesdata$timetoship)-min(salesdata$timetoship))) * (newmax-newmin) + newmin  
salesdata$timetoship<-timetoship_

summary(salesdata)

##########################################################################################
##    CLUSTERING                               #######################################
########################################################################################


########################## ---- AGGLOMERATIVE HIERACHICAL

dist.matrix<-dist(salesdata)
hclust<-hclust(dist.matrix,method = "average")
plot(hclust)


cluster_3<-cutree(hclust,3)#cut tree at 3 clusters
cluster_5<-cutree(hclust,5)#cut tree at 5 clusters
cluster_6<-cutree(hclust,6)#cut tree at 6 clusters
View(as.data.frame(cluster_3)) #shows which cluster each row is apart of 

length(cluster_3)
#head(cluster_3,20)

View(hclust)


#plot(salesdata,col=cluster_3)
#plot(salesdata,col=cluster_5)
#plot(salesdata,col=cluster_6)



#salesdata <-cbind(salesdata,as.data.frame(cluster_3))
#salesdata <-cbind(salesdata,as.data.frame(cluster_5))
#salesdata <-cbind(salesdata,as.data.frame(cluster_6))
View(salesdata)


#centroids_3<-aggregate(salesdata,by=list(cluster_3),FUN=mean)[,-1]
#centroids_3<-tapply(t(salesdata), cluster_3, mean)

centroids_3<-aggregate(salesdata,by=list(cluster_3),FUN=mean)[,-1]
cluster_3_freq<-table(cluster_3)/length(cluster_3)
column_sums<-colSums((centroids_3-colMeans(salesdata))^2 )

#between_ss<-sum( (table(cluster_3)/length(cluster_3)) * colSums((centroids_3-colMeans(salesdata))^2 ))
centroids_3<-aggregate(salesdata,by=list(cluster_3),FUN=mean)[,-1]
cluster_3_freq<-table(cluster_3)/length(cluster_3)
column_sums<-colSums((centroids_3-colMeans(salesdata))^2 )
between_ss_3<-0
for(i in 1:nrow(cluster_3_freq)){
 prod<-cluster_3_freq[i]*column_sums  
 between_ss_3<-sum(val,prod)
}

centroids_5<-aggregate(salesdata,by=list(cluster_5),FUN=mean)[,-1]
cluster_5_freq<-table(cluster_5)/length(cluster_5)
column_sums<-colSums((centroids_5-colMeans(salesdata))^2 )
between_ss_5<-0
for(i in 1:nrow(cluster_5_freq)){
  prod<-cluster_5_freq[i]*column_sums  
  between_ss_5<-sum(val,prod)
}

centroids_6<-aggregate(salesdata,by=list(cluster_6),FUN=mean)[,-1]
cluster_6_freq<-table(cluster_6)/length(cluster_6)
column_sums<-colSums((centroids_6-colMeans(salesdata))^2 )
between_ss_6<-0
for(i in 1:nrow(cluster_6_freq)){
  prod<-cluster_6_freq[i]*column_sums  
  between_ss_6<-sum(val,prod)
}


tss1<-sum(dist(salesdata)^2)/nrow(salesdata)

variance_3<-between_ss_3/tss1
variance_5<-between_ss_5/tss1
variance_6<-between_ss_6/tss1

variance_3
variance_5
variance_6

length(cluster_3)
length(salesdata$Region)

#colnames(salesdata)


variance<-var/tss1
#table(cluster_3)



plot(salesdata,col=cluster_6$cluser)



  



####################### #### ----KMEANS

km.results_3<-kmeans(salesdata,3)
km.results_5<-kmeans(salesdata,5)
km.results_6<-kmeans(salesdata,6)


#within cluster sum of squares, measures compactness, of each cluster, smaller values are better
wcss_3<-sum(km.results_3$withinss)
wcss_5<-sum(km.results_5$withinss)
wcss_6<-sum(km.results_6$withinss)
#6 clustering has smallest value==> best compactness
#for the 6 clustering each cluster has the minimum within cluster sum of square(which measures similarity within the grouping) 

#within cluser sum of squares/ total sum of squares
#the 3 clustering is better quality by this measure
km.results_3$withinss/km.results_3$totss
km.results_5$withinss/km.results_5$totss
km.results_6$withinss/km.results_6$totss

sum(km.results_3$totss)
sum(km.results_5$totss)
sum(km.results_6$totss)

tss<-sum(dist(salesdata)^2)/nrow(salesdata)# same as the totss(constant among clusters)

#SSB<-sum(km.results_3$size*dist(km.results_3$centers,km.results_3$center)^2)



#tss

#Variance Explained ration=(TSS-WCSS)/TSS
km.results_3$betweenss/km.results_3$totss
km.results_5$betweenss/km.results_5$totss
km.results_6$betweenss/km.results_6$totss


VE_3<-(tss-wcss_3)/tss
VE_5<-(tss-wcss_5)/tss
VE_6<-(tss-wcss_6)/tss




VE_3
VE_5
VE_6
#We can say from this calculations that out of the kmeans generated clusters, the 6 clustering is the best quality

#salesdata$kmeans_3<- km.results_3$cluster
#salesdata$kmeans_5<- km.results_5$cluster
#salesdata$kmeans_6<- km.results_6$cluster

str(salesdata)

#plot(salesdata,col=km.results_6$cluster)#multi scatter plot 
##plot(salesdata,col=km.results_5$cluster)#
#plot(salesdata,col=km.results_3$cluster)#

#library(fpc)

plotcluster(salesdata,km.results_6$cluster)

