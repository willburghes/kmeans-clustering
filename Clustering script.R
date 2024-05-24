# Load libraries

library(foreign)
library(fpc)
library(dplyr)
library(reshape2)
library(ggplot2)

# Set working directory

setwd("~/R Projects/Clustering Pew data") # < UPDATE TO YOUR WORKING DIRECTORY

# Import data from Pew Research Center SPSS file

WorkingData <- read.spss("ATP W111.sav")
WorkingData <- as.data.frame(WorkingData, stringsasfactors=FALSE)

# Subset the data around: ONLINEDATE, ONLINEDATE2, ONDPLTFRM, ONIMPACT, ONCHOICES, ONCOMMIT, ONDALG, ONEXPGEN

ClusteringData <- WorkingData[, c(30:39, 46:50)]
ClusteringData <- data.frame(lapply(ClusteringData, as.character), stringsAsFactors = FALSE)
ClusteringData[is.na(ClusteringData)] <- 0
ClusteringData[ClusteringData=="No, have not done this" | ClusteringData=="No, I have not used this" |  ClusteringData=="Refused" | ClusteringData=="Mostly negative effect" | ClusteringData=="Too few options for dating" | ClusteringData=="A lot easier" | ClusteringData=="Very negative" | ClusteringData=="No, they couldn’t" | ClusteringData=="No, but I used one more than 5 years ago"] <- 1
ClusteringData[ClusteringData=="Yes, have done this" | ClusteringData=="Yes, I have used this" | ClusteringData=="Neither positive nor negative effect" | ClusteringData=="The right amount of options for dating" | ClusteringData=="A little easier" | ClusteringData=="Somewhat negative" | ClusteringData=="Not sure" | ClusteringData=="No, but I used one 1 to 5 years ago"] <- 2
ClusteringData[ClusteringData=="Mostly positive effect" | ClusteringData=="Made no difference" | ClusteringData=="Somewhat positive" | ClusteringData=="Too many options for dating" | ClusteringData=="Yes, they could" | ClusteringData=="No, but I have used one within the past year"] <- 3
ClusteringData[ClusteringData=="A little harder" | ClusteringData=="Very positive" | ClusteringData=="Yes, I am currently using an online dating site or dating app"] <- 4
ClusteringData[ClusteringData=="A lot harder"] <- 5

# Convert all text strings to integers so that the k-means algorithm can calculate means

ClusteringData[sapply(ClusteringData, is.character)] <- lapply(ClusteringData[sapply(ClusteringData, is.character)], as.integer)

# Run k-means algorithm to create five clusters and assign them to the original data set

Clusters <- kmeansruns(ClusteringData, krange=5:5, criterion="ch", iter.max=100, runs=100, scaledata=FALSE, alpha=0.001, critout=FALSE, plot=FALSE)

WorkingData$Cluster <- Clusters$cluster

# Print summaries of each of the five clusters

summary(WorkingData[which(WorkingData$Cluster == 1), ])
summary(WorkingData[which(WorkingData$Cluster == 2), ])
summary(WorkingData[which(WorkingData$Cluster == 3), ])
summary(WorkingData[which(WorkingData$Cluster == 4), ])
summary(WorkingData[which(WorkingData$Cluster == 5), ])

# Count clusters

ClusterCounts <- as.data.frame(WorkingData %>% group_by(Cluster) %>% count(Cluster, sort = FALSE))

# Plot count of clusters

ggplot(data=ClusterCounts, aes(x=Cluster, y=n)) + 
  geom_col() + 
  geom_text(aes(label = scales::comma(n)), vjust= -1, size = 4) +
  ylim(0, 1600) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y = element_blank(), 
        axis.text.x = element_text(vjust = 5, size = 12), text = element_text(size = 12)) + 
  guides(y = "none")

# Examine income profiles of each cluster

ClusterIncome <- as.data.frame(WorkingData %>% group_by(Cluster) %>% count(F_INC_SDT1, sort = FALSE))
colnames(ClusterIncome) <- c("Cluster", "Income", "count")
ClusterIncome <- ClusterIncome %>% group_by(Cluster) %>% mutate(freq = (count / sum(count)))

# Format a table of income by cluster for export

ClusterIncomeTable <- acast(ClusterIncome, Cluster ~ Income, value.var = "count")

# Plot a bar chart of income by cluster

ggplot(data=ClusterIncome, aes(x=Cluster, y=freq, fill=Income)) + 
  geom_col() + 
  geom_text(aes(label = scales::percent(freq, accuracy = 0.1)), position = position_fill(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y = element_blank(), 
        axis.text.x = element_text(vjust = 5, size = 12), text = element_text(size = 12)) + 
  guides(y = "none")

# Export cluster data for further analysis in Google Sheets

write.csv(WorkingData, "PewClusters.csv")
