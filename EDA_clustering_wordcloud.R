# importing dataset

cereals_data <- read.csv("C:/Users/venuk//OneDrive//Desktop//EDA_project//cereals_data.csv")
View(cereals_data)


#filling na values

library(VIM)
cereals_data<-kNN(cereals_data)
summary(cereals_data)
cereals_data=cereals_data[,c(1:16)]
View(cereals_data)


#sample stats

library(psych)
numeric_data=cereals_data[,-c(1,2,3,13,14,15)]
describe(numeric_data)
summary(numeric_data)


#data visual

dotchart(cereals_data$rating,cereals_data$name,              #dotchart
          cex = 0.4,groups=cereals_data$calories,
          main="Cereals calories by rating",
          xlab="rating")

#boxplot 
boxplot(numeric_data,col="blue")
#correlation 
pairs.panels(numeric_data)    
#heatmap
matrix_data=as.matrix(numeric_data)
heatmap(matrix_data)
#corplot
cor_data=cor(numeric_data)
library(corrplot)
corrplot(cor_data,type="lower")



#treemap
count_mfr=table(cereals_data$mfr)
count_mfr
View(count_mfr)
percent=round(100*count_mfr/sum(count_mfr),1)
percent
character_data=as.character(percent)        #tree map takes only one numerical values so made it into sting type
character_data=paste(character_data,"%")
character_data

unique_datamfr=table(cereals_data$mfr)
unique_datamfr1=data.frame(unique_datamfr)
unique_datamfr1

pasted=paste(unique_datamfr1$Var1,character_data)
pasted

data_frame1=data.frame(unique_datamfr,pasted)
View(data_frame1)
data_frame1=data_frame1[,c(2,3)]
View(data_frame1)

#data_frame=data_frame[,c(1,2)]
##(data_frame1)
library(treemap)
treemap(data_frame1,index = c("pasted"),vSize = "Freq",title = "Number of Products by Manufacturer")


#type stacked bar plot

library(ggplot2)
ggplot(cereals_data,aes(x="type",fill=cereals_data$type))+geom_bar(position = "stack")


#(type and mfr)vs calories

data=sum(cereals_data$calories)
data                                  
categorical_data=cereals_data$type
cereals_type=tapply(cereals_data$calories,cereals_data$type,sum)
#cereals_type1=tapply(cereals_data$calories,cereals_data$mfr,sum)
cereals_type
#cereals_type1  
type_data=subset(cereals_data,cereals_data$type=="H",select=c(type,calories,mfr)) 
type_data                   






mfr_cal=tapply(cereals_data$calories,cereals_data$mfr,sum)
mfr_cal
par(mfrow=c(1,2))
barplot(cereals_type,xlab="'type",ylab="sum of calories",main="type vs calories",col=c("red","blue"))

barplot(mfr_cal,xlab = "manufacturer",ylab = " sum of calories",main="manufacturer vs cal ",density=c(3,100,120,30,50,40,45),angle = c(90,45,50,20,30,60,25),col="red")

#insights



# total sum of calories is 8230
# in that 7930 are c and 300 is H
# in (300  total)H type c33.33% calories from N,A,Q 
#in c type total sum of calories is 7930,the highest contribution is 31.5% from c mfr
#in total(8230) the highest contribution is c mfr with 30.3% and with least A mfr with 3.64%
#the highest cal product is Mueslix_Crispy_Blend(160 cal)from k mfr which is c Type,least is ALL-Bran_with_Extra_Fiber (50),type=c,k mfr

#__________________

##mfr and type vs protein



data1=sum(cereals_data$protein)   
data1                                 
categorical_data1=cereals_data$type
pro_type=tapply(cereals_data$protein,cereals_data$type,sum)
#pro_type1=tapply(cereals_data$protein,cereals_data$mfr,sum)
pro_type 
#pro_type1
type_data1=subset(cereals_data,cereals_data$type=="H",select=c(type,protein,mfr)) 
type_data1    

# the sum of protiens is 196
#h=12,c=184
#the high content protien food are k mfr with 31% and least is A mfr with 2%


#in terms of type H=low is N mfr with 25%,high is Q mfr with(42%)
# C= 8% low ,N mfr ,high is K mfr with 33%
# the sum of proteins in mfr "K" is approx 3 times the sum of proteins in mfr "P"


mfr_pro=tapply(cereals_data$protein,cereals_data$mfr,sum)
mfr_pro

library(RColorBrewer)
design=brewer.pal(7,"Set2")
barplot(pro_type,xlab="'type",ylab="sum of proteins",main="type vs proteins",col=c("orange","pink"))
barplot(mfr_pro,xlab = "manufacturer",ylab = "protien",main="manufacturer vs pro",col = design)
par(mfrow=c(1,2))
#__________________________________________________________________________


data2=sum(cereals_data$fat)                  # fat
data2                                 
categorical_data2=cereals_data$type
fat_type=tapply(cereals_data$fat,cereals_data$type,sum)
#fat_type1=tapply(cereals_data$fat,cereals_data$mfr,sum)
fat_type 
#fat_type1
type_data2=subset(cereals_data,cereals_data$type=="H",select=c(type,fat,mfr)) 
type_data2      



#in N mfr for H type the fat is zero
#the mfr G has more fat content in their products
#and least is N 
#in terms of type  less is mfr A, and High is Q
====
mfr_fat=tapply(cereals_data$fat,cereals_data$mfr,sum)
mfr_fat

library(RColorBrewer)
design=brewer.pal(7,"Accent")
barplot(pro_type,xlab="'type",ylab="sum of fat",main="type vs fat",col=c("yellow","green"))
barplot(mfr_fat,xlab = "manufacturer",ylab = "fat",main="manufacturer vs fat",col = design)
par(mfrow=c(1,2))

#_________________________________________________________________________________

                            ##sodium
data3=sum(cereals_data$sodium)        
data3                                
categorical_data3=cereals_data$type
sodium_type=tapply(cereals_data$sodium,cereals_data$type,sum)
#sodium_type1=tapply(cereals_data$sodium,cereals_data$mfr,sum)
sodium_type 
#sodium_type1
type_data3=subset(cereals_data,cereals_data$type=="H",select=c(type,sodium,mfr)) 
type_data3      


mfr_fat=tapply(cereals_data$sodium,cereals_data$mfr,sum)
mfr_fat

library(RColorBrewer)
design=brewer.pal(7,"Dark2")
barplot(fat_type,xlab="'type",ylab="sum of sodium",main="type vs sodium",col=c("yellow","green"))
barplot(mfr_fat,xlab = "manufacturer",ylab = "sodium",main="manufacturer vs sodium",col = design)
par(mfrow=c(1,2))

#___________________________________________________________________________________________________

                                            #fiber
data4=sum(cereals_data$fiber)        
data4                                
categorical_data4=cereals_data$type
fiber_type=tapply(cereals_data$fiber,cereals_data$type,sum)
#fiber_type1=tapply(cereals_data$fiber,cereals_data$mfr,sum)
fiber_type 
#fiber_type1
type_data3=subset(cereals_data,cereals_data$type=="H",select=c(type,fiber,mfr)) 
type_data3      


mfr_fiber=tapply(cereals_data$fiber,cereals_data$mfr,sum)
mfr_fiber

library(RColorBrewer)
design=brewer.pal(7,"Paired")
barplot(fiber_type,xlab="'type",ylab="sum of fiber",main="type vs fiber",col=c("blue","orange"))
barplot(mfr_fiber,xlab = "manufacturer",ylab = "fiber",main="manufacturer vs fiber",col = design)
par(mfrow=c(1,2))
#___________________________________________________________________________________________
                                         #rating




data5=sum(cereals_data$rating)        
data5                              
categorical_data5=cereals_data$type
rating_type=tapply(cereals_data$rating,cereals_data$type,sum)

rating_type 
type_data4=subset(cereals_data,cereals_data$type=="H",select=c(type,rating,mfr)) 

type_data4     


mfr_rating=tapply(cereals_data$rating,cereals_data$mfr,sum)
mfr_rating

library(RColorBrewer)
design=brewer.pal(7,"Set2")
barplot(rating_type,xlab="'type",ylab="sum of rating",main="type vs rating",col=c("blue","yellow"))
barplot(mfr_rating,xlab = "manufacturer",ylab = "rating",main="manufacturer vs rating",col = design)

#________________________________________________________________________________________________



                                      #carbo
data6=sum(cereals_data$carbo)          
data6                         
categorical_data6=cereals_data$type
carbo_type=tapply(cereals_data$carbo,cereals_data$type,sum)

carbo_type 
type_data5=subset(cereals_data,cereals_data$type=="H",select=c(type,carbo,mfr)) 

type_data5    


mfr_carbo=tapply(cereals_data$carbo,cereals_data$mfr,sum)
mfr_carbo

library(RColorBrewer)
design=brewer.pal(7,"Pastel1")
barplot(carbo_type,xlab="'type",ylab="sum of carbo",main="type vs carbo",col=c("orange","blue"))
barplot(mfr_carbo,xlab = "manufacturer",ylab = "carbo",main="manufacturer vs carbo",col = design)
par(mfrow=c(1,2))
#__________________________________________________________________________

                           #sugars

data7=sum(cereals_data$sugars)     
data7                         
categorical_data7=cereals_data$type
sugars_type=tapply(cereals_data$sugars,cereals_data$type,sum)
sugars_type 
type_data6=subset(cereals_data,cereals_data$type=="H",select=c(type,sugars,mfr)) 

type_data6   

mfr_sugars=tapply(cereals_data$sugars,cereals_data$mfr,sum)
mfr_sugars

library(RColorBrewer)
design=brewer.pal(7,"Pastel2")
barplot(sugars_type,xlab="'type",ylab="sum of sugars",main="type vs sugars",col=c("Pink","green"))
barplot(mfr_sugars,xlab = "manufacturer",ylab = "sugars",main="manufacturer vs sugars",col = design)


#_____________________________________________________________________________________________
                               #potass


data8=sum(cereals_data$potass)     
data8                         
categorical_data8=cereals_data$type
potass_type=tapply(cereals_data$potass,cereals_data$type,sum)
potass_type 
type_data7=subset(cereals_data,cereals_data$type=="H",select=c(type,potass,mfr)) 

type_data7  


mfr_potass=tapply(cereals_data$potass,cereals_data$mfr,sum)
mfr_potass

library(RColorBrewer)
design=brewer.pal(7,"Set1")
barplot(potass_type,xlab="'type",ylab="sum of potass",main="type vs potass",col=c("orange","green"))
barplot(mfr_sugars,xlab = "manufacturer",ylab = "potass",main="manufacturer vs potass",col = design)
#__________________________________________________________________________________________
                                           #vitamins

data9=sum(cereals_data$vitamins)   
data9                         
categorical_data9=cereals_data$type
vitamins_type=tapply(cereals_data$vitamins,cereals_data$type,sum)
vitamins_type 
type_data8=subset(cereals_data,cereals_data$type=="H",select=c(type,vitamins,mfr)) 

type_data8


mfr_vitamins=tapply(cereals_data$vitamins,cereals_data$mfr,sum)
mfr_vitamins

library(RColorBrewer)
design=brewer.pal(7,"Set2")
barplot(vitamins_type,xlab="'type",ylab="sum of vitamins",main="type vs vitamins",col=c("grey","blue"))
barplot(mfr_vitamins,xlab = "manufacturer",ylab = "vitamins",main="manufacturer vs vitamins",col = design)
#__________________________________________________________________________________________________

                                     ###clustering
clustering_data=read.csv("C:/Users/venuk//OneDrive//Desktop//EDA_project//cereals_data.csv")
View(clustering_data)
sum(is.na(clustering_data))  
cluster_naremoved=na.omit(clustering_data)

#removing na values
dim(cluster_naremoved)  
View(cluster_naremoved)
sum(is.na(cluster_naremoved))
trimmed_data=cluster_naremoved[,-c(1,2,3,13,14,15)]

View(trimmed_data)
cluster_scaled=scale(trimmed_data)  #scaling the data
View(cluster_scaled)

set.seed(100)               #sampling
ss=sample(1:74,15)
dim(trimmed_data)
numeric_data1=cluster_scaled[ss,]
head(numeric_data1)
library(cluster)
library(factoextra)

dist_data=dist(numeric_data1,method="euclidean")    #calculating distances
head(dist_data)
fviz_dist(dist_data)
fviz_nbclust(trimmed_data,kmeans,method="wss")+geom_vline(xintercept = 3,linetype=4,col="red")
set.seed(100)
result_data=kmeans(trimmed_data,3,nstart = 25)
result_data
fviz_cluster(result_data,data=trimmed_data)
str(result_data)

x=result_data$totss   #986434.5
y=result_data$betweenss  #633922.9
y/x    


aggregate(trimmed_data,by = list(cluster = result_data$cluster),mean)
result_data     #(16,14,47)
concat_data=cbind(numeric_data1,cluster=result_data$cluster)
View(concat_data)
table(cluster_naremoved$mfr,result_data$cluster)
str(result_data)

                               #wordcloud

setwd("C:/Users/venuk/OneDrive/Desktop/TM281118")
tm_data=readLines("Uncovering the Nutritional Landscape of Food.txt")
library(tm)
corpus_data=Corpus(VectorSource(tm_data))
data_op=tm_map(corpus_data,stripWhitespace)
data_op=tm_map(corpus_data,tolower)
data_op=tm_map(corpus_data,removeNumbers)
data_op=tm_map(corpus_data,removePunctuation)
data_op=tm_map(corpus_data,removeWords,stopwords("english"))

class(data_op)
tdm_data=TermDocumentMatrix(data_op)

TDM1=as.matrix(tdm_data)

v=sort(rowSums(TDM1),decreasing = T)
datafram_1=data.frame(word=names(v),freq=v)
library(wordcloud2)

View(datafram_1)

datafram_1=datafram_1[-c(2,5),]

wordcloud2(datafram_1,backgroundColor = "grey",shape="Star")

