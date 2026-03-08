#Install and load in packages 
install.packages("DataExplorer")
library(DataExplorer)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("naniar")
library(naniar)
install.packages("VIM")
library(VIM)
install.packages("missMDA")
library("missMDA")
install.packages("dendextend")
library(dendextend)
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)
install.packages("tidyverse")
library(tidyverse)
install.packages("WeightedCluster")
library(WeightedCluster) 
install.packages("TraMineR")
library(TraMineR)
install.packages("viridis")
library(viridis)
install.packages("NbClust")
library(NbClust)

# STEP 1- Joining each consumption set with the MSOA codes 


#Read in the MSOA code dataset (7,201 MSOA's in England and Wales)
MSOA <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//MSOA Codes.csv")

#Read in the gas consumption datasets from 2010-2020 (removed 2017)
Gas2010 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2010.csv",fileEncoding="UTF-8-BOM")
Gas2011 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2011.csv",fileEncoding="UTF-8-BOM")
Gas2012 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2012.csv",fileEncoding="UTF-8-BOM")
Gas2013 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2013.csv",fileEncoding="UTF-8-BOM")
Gas2014 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2014.csv",fileEncoding="UTF-8-BOM")
Gas2015 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2015.csv",fileEncoding="UTF-8-BOM")
Gas2016 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2016.csv",fileEncoding="UTF-8-BOM")
Gas2018 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2018.csv",fileEncoding="UTF-8-BOM")
Gas2019 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2019.csv",fileEncoding="UTF-8-BOM")
Gas2020 <- read.csv("C://Users//sgcward//Documents//PhD//1st Paper//Gas Consumption//2020.csv",fileEncoding="UTF-8-BOM")


#Join the datasets together using dplyr
test<- dplyr::left_join(MSOA, Gas2010, by = "Code")
test1<- dplyr::left_join(test, Gas2011, by = "Code")
test2<- dplyr::left_join(test1, Gas2012, by = "Code")
test3<- dplyr::left_join(test2, Gas2013, by = "Code")
test4<- dplyr::left_join(test3, Gas2014, by = "Code")
test5<- dplyr::left_join(test4, Gas2015, by = "Code")
test6<- dplyr::left_join(test5, Gas2016, by = "Code")
test7<- dplyr::left_join(test6, Gas2018, by = "Code")
test8<- dplyr::left_join(test7, Gas2019, by = "Code")
test9<- dplyr::left_join(test8, Gas2020, by = "Code")

#correctly joined- rename set to something more suitable. 
consumption <- test9
View(consumption)

####################################################################################

#Step 2- Checking for missing values within the dataset 
#Create dataframe of rows with missing values
missing <- consumption[rowSums(is.na(consumption)) > 0,]
View(missing) #38 MSOA's with complete missing values, 7 MSOA's with some missing values

#Making a new dataframe of only complete gas consumption sequences
consumption <- consumption[complete.cases(consumption), ]         
head(consumption) #7156 MSOA's from possible 7,200 (99.4% of MSOA's in England and Wales have complete sequences)

#Exploring the appropriate number of breaks through 
test = list()

for (col in 2:ncol(consumption)) {
  test[[col]] = consumption[,col]
}

p <- test %>% unlist() %>% hist(breaks = 500) 

#Plot it
plot(p, main = "", xlab = "Consumption (kWh)", ylab = "Frequency") 
title(main = "Histogram of Gas Consumption")
View(consumption)


#Lowest consumption is 4,518
#Highest consumption is 34,513

#Large portions of the data is concentrated around 10,0000-15,000 KwH mark
#If used equal quantiles, the distance between breaks would be very small around the central parts of the data 
#This means areas could appear to be moving through many sequences overtime,
#when their actual reduction is small. 
#Opted for equal intervals rather than equal quantiles

#Working out the breaks using equal intervals 
# (34,513 - 4518) / 10 = 2,999.5 breaks


#Creating the breaks and using these values to assign the categories
#Specifying 
#First break is 7517.5 (4518 + 2999.5)
consumption$'2010' = cut(consumption$MedConsum2010, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))

View(consumption)

#Using the same breaks from 2010 for all the years to create the most change in sequences overtime

#Creatign sequence categories for 2011
consumption$'2011' = cut(consumption$MedConsum2011, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))

#Creatign sequence categories for 2012
consumption$'2012' = cut(consumption$MedConsum2012, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))


#Creatign sequence categories for 2013
consumption$'2013' = cut(consumption$MedConsum2013, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))


#Creatign sequence categories for 2014
consumption$'2014' = cut(consumption$MedConsum2014, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))


#Creatign sequence categories for 2015
consumption$'2015' = cut(consumption$MedConsum2015, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))


#Creatign sequence categories for 2016
consumption$'2016' = cut(consumption$MedConsum2016, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))



#Creatign sequence categories for 2018
consumption$'2018' = cut(consumption$MedConsum2018, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))


#Creatign sequence categories for 2019
consumption$'2019' = cut(consumption$MedConsum2019, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))


#Creatign sequence categories for 2020
consumption$'2020' = cut(consumption$MedConsum2020, br=c(0, 7517.5, 10517.0, 13516.5, 
                                                         16516.0, 19515.5, 22515.0, 25514.5, 
                                                         28514.0, 31513.5, 34513.0),
                         labels=c('1','2', '3', '4', '5', '6', '7', '8', '9', '10'))


View(consumption)




#Defining a vector which contains the legends of consumption categories and create sequence argument 
mvad.labels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
mvad.labels #10 categories from 1 - 10 to indicate consumption levels (10 is highest)
mvad.scode <- c("1 ", "2", "3", "4", "5", "6", "7", "8", "9", "10")
mvad.scode #Category codes

#xtstep = NULL to ensure x axis is correct on the plots
#column 13-23 due to those columns corresponding with consumption categories 
mvad.seq <- seqdef(consumption, 12:21, states = mvad.scode, labels = mvad.labels, xtstep = NULL)
head(mvad.seq) #Outlines the trajectories of MSOA's

#Want the viridis colour scale 
cpal(mvad.seq) <- viridis(10)

dev.off()

seqiplot(mvad.seq, border = NA, 
         title= "Index Plot (First 10 Sequences)",cex.legend=0.40, ncol=5, legend.prop=.2)


#Frequency Plot
seqfplot(mvad.seq, border = NA, 
         title= "Sequence Frequency Plot",cex.legend=0.40, ncol=5, legend.prop=.2)  


#State distribution plot
seqdplot(mvad.seq, border = NA, 
         title= "State Distribution Plot",cex.legend=0.40, ncol=5, legend.prop=.2)


#Individual sequences plot 
seqIplot(mvad.seq,border = NA, title="Individual Sequences",
         cex.legend=0.50, ncol=5, legend.prop=.2)

#Plot the entropy 
seqHtplot(mvad.seq, title = "Entropy index")

Turbulence <- seqST(mvad.seq)
hist(Turbulence, col = "cyan", main = "Sequence turbulence")

#Create the distance matrix
distance <- seqdist(mvad.seq, method = 'DHD', indel=1, sm="TRATE")

View(distance)

#########################################################################################
#########################################################################################

#Clustering method 
clusterward <- agnes(distance, diss = TRUE, method = "ward")

fviz_nbclust(distance, FUN = hcut, method = "wss") #4

fviz_nbclust(distance, FUN = hcut, method = "silhouette", k.max = 10) #4

#Dendogram of clusters
pltree(clusterward, cex = 0.6, hang = -1, main = "Dendrogram of Clustering Sequences")

clusterward$ac #0.99988

#########################################################################################
#########################################################################################

#4 clusters- # 4 clusters seems optimal based on elbow, dendrogram and silhouette scores
cl1.4 <- cutree(clusterward, k = 4)


cl1.4fac <- factor(cl1.4, labels = paste("Type", 1:4))

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.4fac)

#Make new labels for the cluster categories
cl1.4fac <- factor(cl1.4, labels = paste(c("Medium to Low", 
                                           "Low to Very Low", 
                                           "Very High to High", 
                                           "High to Medium"
                                           )))

#State distribution plot of the clusters with new labels
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.4fac)

#Frequency plot of the clusters with new labels 
seqfplot(mvad.seq, group = cl1.4fac, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2)

#Individual seqeuences plot with new labels
seqIplot(mvad.seq,group = cl1.4fac, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2)

cl1.4fac <- as.data.frame(cl1.4fac)
head(cl1.4fac) #Add this to the consumption dataset 
consumption <- cbind(consumption, cl1.4fac)
View(consumption)

#Write them out as part of a new dataset
write.csv(consumption,"C://Users//sgcward//Documents//PhD//1st Paper//New breaks Hierarchical//Feedback afrer holiday//consumption.csv", row.names = TRUE)

View(consumption)



















########################################################################################
########################################################################################

#3 clusters 
cl1.3 <- cutree(clusterward, k = 3)

#Assign a label for the clusters- type 1 to 6
cl1.3fac <- factor(cl1.3, labels = paste("Type", 1:3))

cl1.3fac <- as.data.frame(cl1.3fac)
View(cl1.3fac) #Add this to the consumption dataset 
consumption <- cbind(consumption, cl1.3fac)
View(consumption)

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.3fac)

########################################################################################

########################################################################################
#4 clusters 
cl1.4 <- cutree(clusterward, k = 4)

#Assign a label for the clusters- type 1 to 6
cl1.4fac <- factor(cl1.4, labels = paste("Type", 1:4))

cl1.4fac <- as.data.frame(cl1.4fac)
head(cl1.4fac) #Add this to the consumption dataset 
consumption <- cbind(consumption, cl1.4fac)
View(consumption)

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.4fac)
########################################################################################

########################################################################################
#5 clusters 
cl1.5 <- cutree(clusterward, k = 5)

#Assign a label for the clusters- type 1 to 6
cl1.5fac <- factor(cl1.5, labels = paste("Type", 1:5))

cl1.5fac <- as.data.frame(cl1.5fac)
head(cl1.5fac) #Add this to the consumption dataset 
consumption <- cbind(consumption, cl1.5fac)
View(consumption)

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.5fac)
########################################################################################

########################################################################################
#6 clusters 
cl1.6 <- cutree(clusterward, k = 6)

#Assign a label for the clusters- type 1 to 6
cl1.6fac <- factor(cl1.6, labels = paste("Type", 1:6))

cl1.6fac <- as.data.frame(cl1.6fac)
head(cl1.6fac) #Add this to the consumption dataset 
consumption <- cbind(consumption, cl1.6fac)
View(consumption)

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.6fac)
########################################################################################

########################################################################################
#7 clusters 
cl1.7 <- cutree(clusterward, k = 7)

#Assign a label for the clusters- type 1 to 6
cl1.7fac <- factor(cl1.7, labels = paste("Type", 1:7))

cl1.7fac <- as.data.frame(cl1.7fac)
head(cl1.7fac) #Add this to the consumption dataset 
consumption <- cbind(consumption, cl1.7fac)
View(consumption)
#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.7fac)
########################################################################################

########################################################################################
#8 clusters
#8 seems most appropriate- splits the high and low consumers down well 
cl1.8 <- cutree(clusterward, k = 8)

#Assign a label for the clusters- type 1 to 6
cl1.8fac <- factor(cl1.8, labels = paste("Type", 1:8))

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.8fac)

#Make new labels for the cluster categories
cl1.8fac <- factor(cl1.8, labels = paste(c("Average to Low", 
                                           "Fluctuating Low to Very low", 
                                           "Low to Very Low", 
                                           "Very High to High",
                                           "Average to Below Average",
                                           "High to Average",
                                           "Highest Consumers", 
                                           "Lowest Consumers")))

#State distribution plot of the clusters with new labels 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.8fac)

#Frequency plot of the clusters with new labels 
seqfplot(mvad.seq, group = cl1.8fac, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2)

#Individual seqeuences plot with new labels
seqIplot(mvad.seq,group = cl1.8fac, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2)

cl1.8fac <- as.data.frame(cl1.8fac)
head(cl1.8fac) #Add this to the consumption dataset 
consumption <- cbind(consumption, cl1.8fac)
View(consumption)




########################################################################################

#9 clusters 
cl1.9 <- cutree(clusterward, k = 9)

#Assign a label for the clusters- type 1 to 6
cl1.9fac <- factor(cl1.9, labels = paste("Type", 1:9))

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.9fac)


#####################################################################################

#10 clusters 
cl1.10 <- cutree(clusterward, k = 10)

#Assign a label for the clusters- type 1 to 6
cl1.10fac <- factor(cl1.10, labels = paste("Type", 1:10))

#State distribution plot of the clusters 
seqdplot(mvad.seq, border = NA,
         cex.legend=.8, ncol=5, legend.prop=.2, group=cl1.10fac)


#Add them all to dataset 
write.csv(consumption,"C://Users//sgcward//Documents//PhD//1st Paper//New breaks Hierarchical//Consumption.csv", row.names = TRUE)

View(consumption)

#################################################################################
#Cluster 2 has MSOA's at the extreme ends of the data- highest of the high and lowest of the low 
#Although WSS pltot suggested 3 clusters- using more e.g., 8 splits these 2 extremes into their own cluster 
#Could be better to do this and show areas that have very high consumption to target as a policy perspective? 
#Currently having 3 clusters won't show the spatial temporal variation?

#Next steps- explore the makeup of these clusters- census data, EPC/energy efficiency data
#################################################################################