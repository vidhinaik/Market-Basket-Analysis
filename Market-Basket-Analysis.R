setwd("V:/MIS/SEMESTER 2/IDS 572/Assignment 05")
#import from excel

View(BathSoapData)
set.seed(123)


BathSoapData[!complete.cases(BathSoapData),]
BathSoapData <- na.omit(BathSoapData)

BathSoapData$BrCd <- names(BathSoapData[22:30])[max.col(BathSoapData[22:31])]
BathSoapData$BrCd <- NULL

#Data cleaning and Data preparation for Clustering
str(BathSoapData)

#Few Demographic data variable have 'Not specified' value. 
#They do not represent any particular category. For the 
table(BathSoapData$SEC) #Equal distribution of Social Economic class(150 each category)

#Food eating habits
table(BathSoapData$FEH)
# 0 -> Not specified (69 value are 0)
#From data -> major population is non-vegetarian(i.e. 3-> 332)

#Native language 
table(BathSoapData$MT)
# 0 -> Not specified (69 value are 0)

#Sex
table(BathSoapData$SEX)
# 0 -> Not specified (68 value are 0)

table(BathSoapData$AGE)

#Education of the homemaker
table(BathSoapData$EDU)
# 0 -> Not specified (73 value are 0)

#Presence of child in household
table(BathSoapData$CHILD)
# 5 -> Not specified (68 value are 0)

#Question 01
#Part-01
BathSoapData$maxShareBrand <- apply(BathSoapData[,23:30],1,max)
View(BathSoapData)

BathSoapData_BrandLoyalty <- BathSoapData[,c(12:16,19,31,47)]
View(BathSoapData_BrandLoyalty)

#Data Normalization
#Normalize the variable values between 0-1 (Max-Min normalization)
#No of Brands: Higher number is bad for brand loyalty and lower number of brands is better.

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

BathSoapData_BrandLoyalty_norm <- as.data.frame(lapply(BathSoapData_BrandLoyalty, normalize))
View(BathSoapData_BrandLoyalty_norm)


#Check for the optimal value of K
mydata <- BathSoapData_BrandLoyalty_norm
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

#Elbow Point -> 4(Best value of K)
set.seed(123)
km10 = kmeans(BathSoapData_BrandLoyalty_norm, 4, nstart =100)
km10$withinss #10.84585 17.93006 21.20124 23.45212
km10$tot.withinss #73.42927
km10$betweenss #110.0281
km10 #between_SS / total_SS =  60.0 %

#Elbow Point -> 5(Best value of K)
set.seed(123)
km11 = kmeans(BathSoapData_BrandLoyalty_norm, 5, nstart =100)
km11$withinss #10.761737 16.889353  9.266674 20.417912  8.690840
km11$tot.withinss #66.02652
km11$betweenss #117.4308
km11 #between_SS / total_SS =  64.0 %

#Elbow Point -> 6(Best value of K)
set.seed(123)
km12 = kmeans(BathSoapData_BrandLoyalty_norm, 6, nstart =100)
km12$withinss #7.784544  9.266674  8.690840 14.325473 11.298881  9.470808
km12$tot.withinss #60.83722
km12$betweenss #122.6201
km12 #between_SS / total_SS =  66.8 %

#best model -> km12 (It has lower withinss distance and higher betweenss)

#Part 02
#Determine which selling propositions to select for clustering
#Analyse the distribution of all the selling proposition codes

hist(BathSoapData$`PropCat 5`)
plot(density(BathSoapData$`PropCat 5`))
hist(BathSoapData$`PropCat 6`)
plot(density(BathSoapData$`PropCat 6`))
hist(BathSoapData$`PropCat 7`)
plot(density(BathSoapData$`PropCat 7`))
hist(BathSoapData$`PropCat 8`)
plot(density(BathSoapData$`PropCat 8`))
hist(BathSoapData$`PropCat 9`)
plot(density(BathSoapData$`PropCat 9`))
hist(BathSoapData$`PropCat 10`)
plot(density(BathSoapData$`PropCat 10`))
hist(BathSoapData$`PropCat 11`)
plot(density(BathSoapData$`PropCat 11`))
hist(BathSoapData$`PropCat 12`)
plot(density(BathSoapData$`PropCat 12`))

#PropCat 5(Normal Distributed), PropCat 12(All values between 0.0 and 0.1)
#PropCat 11(Highly noised and maximum values are 0) 
#PropCat 10(All values between 0.0 and 0.1 and max is 0.37)
#PropCat 9(90% values are between 0.0 and 0.1)
#PropCat 8(This category can be taken)
#PropCat 7(Distribution is between 0.0 and 1.0)
#PropCat 6(Distribution is between 0.0 and 1.0)
BrandLoyalty_Part02 <- BathSoapData[,c(20:22,32:35,36:39)]
View(BrandLoyalty_Part02)
BrandLoyalty_Part02_norm <- as.data.frame(lapply(BrandLoyalty_Part02, normalize))
View(BrandLoyalty_Part02_norm)

#Check for the optimal value of K
mydata <- BrandLoyalty_Part02_norm
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

#Elbow Point -> 5(Best value of K)
set.seed(123)
km20 = kmeans(BrandLoyalty_Part02_norm, 5, nstart =100)
km20$withinss #10.510302 39.054844  7.863222 52.463688 19.072490
km20$tot.withinss #128.9645
km20$betweenss #180.2902
km20 #between_SS / total_SS =  58.3 %

#Elbow Point -> 6(Best value of K)
set.seed(123)
km21 = kmeans(BrandLoyalty_Part02_norm, 6, nstart =100)
km21$withinss #29.293664 21.339164  7.863222 17.598423 32.050746  9.567151
km21$tot.withinss #117.7124
km21$betweenss # 191.5424
km21 #between_SS / total_SS =  61.9 %

#best model -> km21 (It has lower withinss distance and higher betweenss)

#Part 03
#Variables that decide purchase behavior -> km1
#Variables that decide basis of Purchase -> km2

BrandLoyalty_Part03 <- BathSoapData[,c(12:16,19:22,31:35,36:39,47)]
View(BrandLoyalty_Part03)
BrandLoyalty_Part03_norm <- as.data.frame(lapply(BrandLoyalty_Part03, normalize))
View(BrandLoyalty_Part03_norm)

#Check for the optimal value of K
mydata <- BrandLoyalty_Part03_norm
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

#Elbow Point -> 5(Best value of K)
set.seed(123)
km30 = kmeans(BrandLoyalty_Part03_norm, 5, nstart =100)
km30$withinss #95.10459 15.37235 17.39150 54.67866 68.60645
km30$tot.withinss #251.1535
km30$betweenss #241.5585
km30 #between_SS / total_SS =  49.0 %

#Elbow Point -> 6(Best value of K)
set.seed(123)
km31 = kmeans(BrandLoyalty_Part03_norm, 6, nstart =100)
km31$withinss #17.39150 13.76163 14.45552 37.19142 91.42685 60.46742
km31$tot.withinss #234.6943
km31$betweenss #258.0177
km31 

#Elbow Point -> 7(Best value of K)
set.seed(123)
km32 = kmeans(BrandLoyalty_Part03_norm, 7, nstart =100)
km32$withinss #54.13046 12.40721 30.28405 57.70315 14.83926 34.84255 15.89515
km32$tot.withinss #220.1018
km32$betweenss #272.6103
km32 #between_SS / total_SS =  55.3 %)

#Question 02
#part 01
# The within cluster distance is the least for clustering based on purchase behavior
# But the Between cluster distance is highest in case of both purchase behaviour and basis of purchase combined. 
# To finalize the best fit model, Between_SS/Total_ss ratio is taken. 
# It is the highest for the Purchase beghaviour data. 
# The Between_SS/Total_ss ratio is 66.8% for km12 model.

#part 02
#Cluster variables
km12
km12$cluster
km12$centers
km12$size
km12$iter
km12$ifault

# To get all the data instances that belong to these clusters
Cluster01 = BathSoapData_BrandLoyalty[km12$cluster == 1, ]
Cluster02 = BathSoapData_BrandLoyalty[km12$cluster == 2, ]
Cluster03 = BathSoapData_BrandLoyalty[km12$cluster == 3, ]
Cluster04 = BathSoapData_BrandLoyalty[km12$cluster == 4, ]
Cluster05 = BathSoapData_BrandLoyalty[km12$cluster == 5, ]
Cluster06 = BathSoapData_BrandLoyalty[km12$cluster == 6, ]

#examine the characteristics of these clusters
View(Cluster01)
View(Cluster02)
View(Cluster03)
View(Cluster04)
View(Cluster05)
View(Cluster06)

km12$centers

#Analyse the demographics of the customers in a cluster
Cluster011 = BathSoapData[km12$cluster == 1, ]
Cluster021 = BathSoapData[km12$cluster == 2, ]
Cluster031 = BathSoapData[km12$cluster == 3, ]
Cluster041 = BathSoapData[km12$cluster == 4, ]
Cluster051 = BathSoapData[km12$cluster == 5, ]
Cluster061 = BathSoapData[km12$cluster == 6, ]

View(Cluster011)
View(Cluster021)
View(Cluster031)
View(Cluster041)
View(Cluster051)
View(Cluster061)

#Cluster 01: maximum share of a brand is very less. The consumers belong to this cluster are not loyal to any particular brand and the share of others brand as well as brand runs are too high.
#They normally prefer to switch within different brands. Gender of customers in this cluster is majority female.

#Cluster 02: The customers in this cluster are highly loyal to their brands. Average percentage of purchase is 85% in particular brand. They have lowest brand runs and number of brands.
#Maximum Customers in this cluster speak Marathi so they belong to Maharastra state.

#Cluster 03: The customers in this cluster are fairly loyal to their brands but they caa]n switch to other brands as well because they have good number in No of brands.
#The Total Volume purchased, No of transactions and Value is highest for these people. They can add value to business by good amount of purchase. 
#Customers in this cluster are majorly above 40 years age.

#Cluster 04: The customers in this cluster have highest number of brands used and individaul max share to particular brand is very low. They are not brand loyal custers. They have highest brand runs. Th
#They have highest amount in Avg Price. These customers purchase costly soaps and keep changing the brands.   
#Customers in this cluster have higher Affluence index.

#Cluster 05: These customers prefer to buy maximum from other brands. The Avg Price is also high for them. They have switched between very less number of brands.
#Customers in this cluster have more than 3 people in the house.

# Cluster 06: The customers in this cluster are fairly loyal to particular brand. They have tried multiple brands but max share to any particular brand is also high. 
# So, they belong to the category where a customer settles with a brand after trying multiple brands.
# Majority customers in this cluster is female.

