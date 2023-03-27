rm(list = ls()) 

library("arules")#load library to do association rules

################# ------DATA CLEANING ---- ############################

retail<-read.csv("OnlineRetail.csv",stringsAsFactors = FALSE)
str(retail)
View(retail)
summary(retail)

retail<-retail[retail$Country=="Netherlands",]#the subset of the dataset will we are concerned about is Country: Netherlands


#To determine the buying patterns of customers we need to know what items they usually buy together
#to do this we are using association rule mining
#For this We only need two columns from the dataset, the transaction ID(InvoiceNo) and the itemsID (StockCode)



View(retail)
#From Viewing dataset and sorting by InvoiceNo Column, we can see that some values are pre-pended with a 'C", these need to be removed
retail$InvoiceNo<-gsub("C","",retail$InvoiceNo)# substitutes C with an empty string in InvoiceNo column
summary(retail$InvoiceNo)
retail$InvoiceNoVals<-as.integer(retail$InvoiceNo)#convert strings to numbers

#drop post item codes
retail<-retail[retail$StockCode!="POST",]

colnames(retail)#column 9 and 2
retail<- retail[,c(9,2)]#StockCode and InvoiceNo

plot(table(retail$StockCode))
write.csv(retail,"subset_Netherlands.csv",row.names = FALSE)#export dataframe so we can reimport as transaction

################---- ASSOCIATION RULE MINING ----  ################

subset.netherlands<- read.transactions("subset_Netherlands.csv",format=c("single"),cols=c("InvoiceNoVals","StockCode"),rm.duplicates = FALSE,header=TRUE,sep = ",")
inspect(subset.netherlands[1:2])

#Exploring values for support

itemFrequencyPlot(subset.netherlands,topN=20)#checking out reasonable value for support, top 20 items are above 0.1,
itemFrequencyPlot(subset.netherlands,support=0.10)#Using support of 0.1

#in order to resolve the StockCode values to item descriptions, re-load the orginal dataset
retail1<-read.csv("OnlineRetail.csv",stringsAsFactors = FALSE)
View(retail1)
retail1[retail1$StockCode=="M","Description"][1]

rules<-apriori(subset.netherlands,parameter = list(confidence=0.6,support=0.1,minlen=2))
inspect(rules)


rulesdf<- as.data.frame(inspect(rules))
View(rulesdf)

rulesdf[,1]<-as.character(gsub("[^[:alnum:],]", "", rulesdf[,1]))#extract numeric character and commas
rulesdf[,3]<-as.character(gsub("[^[:alnum:],]", "", rulesdf[,3]))#extract numeric characters and commas

#this below loop extractst the description for each rule created and appends it to the dataframe
for(i in 1:nrow(rulesdf)) {#find the description for each 
  ante<-strsplit(rulesdf[i,1],",")[[1]]#split antecedent items
  conse<-strsplit(rulesdf[i,3],",")[[1]]#split consequenct items
  ante_final<-""
  conse_final<-""
  for( j in 1:length(ante) ){
        ante_final<-paste0(ante_final, retail1[retail1$StockCode==ante[j],"Description"][1],"," )  
   }
  #print(ante_final)
  
  for (k in 1:length(conse) ){
    conse_final<-paste0(conse_final, retail1[retail1$StockCode==conse[k],"Description"][1],"," )  
  }    
  rulesdf$lhs_description[i]<-ante_final
  rulesdf$rhs_description[i]<-conse_final
 #rulesdf$lhs_description[i]<- retail1[retail1$StockCode==rulesdf[i,1],"Description"][1]#gsub("C","",ORData[i,"InvoiceNo"])
 #rulesdf$rhs_description[i]<- retail1[retail1$StockCode==rulesdf[i,3],"Description"][1]#gsub("C","",ORData[i,"InvoiceNo"])
}

View(rulesdf)

