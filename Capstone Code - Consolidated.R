#load all necessary packages
library(dplyr); library(tidyr); library(randomForest); library(caTools); library(tibble); library(ggplot2); library(stringr); library(cluster); library(fpc)

#set the working directory to be the folder for my Capstone project
setwd("C:/Users/catherine/Desktop/March 2016/Capstone Project")

#read in destination  theme data for clustering
Theme3<- read.csv("themes plus ctid1.csv", stringsAsFactors = F)
Theme4<- read.csv("themes plus ctid2.csv", stringsAsFactors = F)
Themes2<- rbind(Theme3, Theme4)

#read in the data I will use for modelling
FR_cluster_data<- read.csv("FR aggregate data.csv")
colnames(FR_cluster_data)<- c("ctid","Age","Gender","Interest","Sessions")

#read in the data to match destination id with destination names
city_ids <- read.csv("ctid.csv", stringsAsFactors = FALSE)
city_ids <- select(city_ids, -4)

#add destination names and tidy up the data
FR_cluster_data<- merge(FR_cluster_data, city_ids, by = "ctid")
FR_cluster_data<- mutate(FR_cluster_data,Destination=paste(city,",",country))
commaspaces <- function (x) gsub("\\s,", ",", x)
FR_cluster_data$Destination<- commaspaces(FR_cluster_data$Destination)

FR_cluster_data_sessions<- select(FR_cluster_data, -1,-6,-7)
FR_cluster_data_sessions<- FR_cluster_data_sessions[c(5,1:4)]
FR_cluster_data_1<- select(FR_cluster_data, -1,-5,-6,-7)
FR_cluster_data_1<- FR_cluster_data_1[c(4,1:3)]
FR_cluster_data_1$Destination<- as.factor(FR_cluster_data_1$Destination)

#check for na
which(is.na(FR_cluster_data_1$Destination))

#next check if all the destinations are in both data sets
Theme.Cities<- unique(Themes2$ctid)
Analytics.Cities<- unique(FR_cluster_data$ctid)

ctids_use<- intersect(Theme.Cities, Analytics.Cities)

#PROBLEM - NOT ALL CITIES IN ANALYTICS ARE IN THEMES
#next steps: filter the aggregate data and the thematic data to only contain cities in both
Theme_Dest<- subset(Themes2, ctid %in% ctids_use)
Theme_Dest<- subset(Theme_Dest, nchar(twid)<4)
Theme_Dest<- select(Theme_Dest, -1,-2)
Theme_Dest<- Theme_Dest[c(2,1)]

Theme_Dest<- mutate(Theme_Dest,binary=1)
Themes_Dest.Binary<- spread(Theme_Dest, twid, binary, fill=0)
Themes_Dest.Variables<- select(Themes_Dest.Binary, -1)

#now try filtering the thematic data to just 3 levels and no group 6
Theme_Dest_1<- subset(Theme_Dest, substring(twid,1,1) != 6)
Theme_Dest_1<- subset(Theme_Dest_1, nchar(twid) == 3)
Themes_Dest.Binary_1<- spread(Theme_Dest_1, twid, binary, fill=0)
Themes_Dest.Variables_1<- select(Themes_Dest.Binary_1, -1)

#now try with age and gender data - add this data also into the clustering
str(unique(Theme_Dest_1$ctid))
Demo_Data<- subset(FR_cluster_data, ctid %in% ctids_use)
str(unique(Demo_Data$ctid))
Demo_Data<- select(Demo_Data, 1:3)
Demo_Data<- unique(Demo_Data)
Demo_Data$Age<- as.character(Demo_Data$Age)
Demo_Data$Gender<- as.character(Demo_Data$Gender)
Demo_Data<- mutate(Demo_Data, binary=1)
Demo_Age<- 
  Demo_Data %>%
  select(1:2,4)
Demo_Gender<- 
  Demo_Data %>%
  select(1,3:4)
colnames(Demo_Age)<- c('ctid','twid','binary')
colnames(Demo_Gender)<- c('ctid','twid','binary')

#combine demographic data with the thematic data
Demo_Data<- rbind(Demo_Age,Demo_Gender)

Cluster_Data<- rbind(Demo_Data, Theme_Dest_1)
Cluster_Data<- unique(Cluster_Data)

#this data is ready for clustering
Cluster_Data.Binary<- spread(Cluster_Data, twid, binary, fill=0)

#remove the dependent variable for clustering
Cluster.Variables<- select(Cluster_Data.Binary, -1)

#now perform clustering
#write function to ascertan how many clusters each clustering should have

nclusters<- function(x) {
  var=NULL
  for(i in 2:30) { 
    set.seed(1)
    kmeans <- kmeans(x, centers=i, iter.max=1000)
    var[i]<- kmeans$tot.withinss}
  
  length(var)
  var<- var[2:30]
  
  x<- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
  y<- var
  df<- data.frame(x,y)
  colnames(df)<- c("Clusters","SSE")
  
  ggplot(df, aes(x=Clusters, y=SSE, col=Clusters)) + geom_point() +
    ggtitle("Clusters by Total SSE") + 
    theme(plot.title = element_text(color="#666666", face="bold", hjust=0),
          axis.title = element_text(color="#666666", face="bold"),
          axis.text.x = element_text(color="#666666", face="bold"),
          axis.text.y = element_text(color="#666666", face="bold", angle=45),
          panel.grid.major = element_line(colour = "grey"),
          panel.grid.minor = element_line(colour = "grey"),
          panel.background = element_blank(),
          legend.text = element_text(colour="#666666", face = "bold"),
          legend.title = element_text(colour="#666666", face = "bold"))
}


#now try with a combination of themes and demographic data
nclusters(Cluster.Variables)

#now with the right number of clusters
k=30
set.seed(1)
Cluster_1<- kmeans(Cluster.Variables, centers=k, iter.max=1000)

#look into initial results of k-means
str(Cluster_1$cluster)
mean(Cluster_1$size)
median(Cluster_1$size)
str(Cluster_Data.Binary)

#draw graphs from k-means

#make data frame from results
cluster_results<- as.data.frame(Cluster_1$cluster)
cluster_results$ctid<- Cluster_Data.Binary$ctid

cluster_results_named<- merge(cluster_results,  city_ids, by='ctid')
which(duplicated(cluster_results_named$ctid))

#duplicated values due to messy cityids file
cluster_results_named<- cluster_results_named[-c(which(duplicated(cluster_results_named$ctid))),]
centers.df<- as.data.frame(matrix(unlist(Cluster_1$centers), nrow=30))

centers.df$cluster<- c(1:30)
centers.df<- centers.df[c(41,1:40)]
variables<- as.matrix(dimnames(Cluster_1$centers))
variables<- unlist(variables[2])
variables<- append(variables, 'Cluster', after=0)
colnames(centers.df)<- variables

#how to visualize clusters
#prepare the data for use with ggplot
#centers.df.all<- as.data.frame(t(centers.df))
#colnames(centers.df.all)<- centers.df.all[1,]
#centers.df.all<-centers.df.all[-1,]
#centers.df.all<- rownames_to_column(centers.df.all, var="Theme")
#centers.df.all<- mutate(centers.df.all, Group = Theme)
#centers.df.all$Group[1:6]<- 1
#centers.df.all$Group[8:11]<- 2
#centers.df.all$Group[13:17]<- 3
#centers.df.all$Group[19:23]<- 4
#centers.df.all$Group[25:36]<- 5
#centers.df.all$Group[regexpr('-',centers.df.all$Group)>0]<- 'Age'
#centers.df.all$Group[38]<- 'Age'
#centers.df.all$Group[39:40]<- 'Gender'
#centers.df.theme<- subset(centers.df.all, (Group != 'Gender' & Group != 'Age'))

#centers.theme_agg<- select(centers.df.theme, -1)
#centers.theme_agg<- aggregate(. ~ Group, data=centers.theme_agg,FUN=mean)
#centers.theme_agg<- gather(centers.theme_agg, "cluster","center",2:31)

#this graph shows mean of each theme faced by cluster
#ggplot(centers.theme_agg, aes(x=Group,y=center, col=as.factor(Group))) + 
#  geom_point(size=3) + facet_wrap(facets='cluster') +  scale_color_manual(breaks = c(1,2,3,4,5),
#                                                                         values=c("green", "blue", "orange","red","purple"),
#                                                                          labels=c("Nature","Wellness","Leisure","Sport","Culture")) +
#  theme(legend.title=element_blank(), 
#        legend.text = element_text(color="#666666",face="bold", size = 11), 
#        plot.title = element_text(color="#666666", face="bold", size=14),
#        axis.title = element_text(color="#666666", face="bold"),
#        axis.text.x = element_text(color="#666666", size=10),
#        axis.text.y = element_text(color="#666666", size=10)) +
#  scale_x_discrete(name ="Theme") +
#  scale_y_discrete(name ="Mean of Centers",limits=c(0,0.25,0.5,0.75,1)) +
#  guides(colour = guide_legend(override.aes = list(size=5))) +
#  ggtitle("Mean of Centers per Thematic Group, shown separately per Destination Cluster")


#this graph shows each mean of each center, faceted by themegroup
#centers.theme_agg2<- centers.theme_agg
#centers.theme_agg2$cluster<- as.integer(centers.theme_agg2$cluster)
#centers.theme_agg2<- arrange(centers.theme_agg2, cluster)

#group_names <- c(
#  `1` = "Nature",
#  `2` = "Wellness",
#  `3` = "Leisure",
#  `4` = "Sport",
#  `5` = "Culture"
#)

#ggplot(centers.theme_agg2, aes(x=cluster,y=center, col=as.factor(cluster))) +
#  geom_point(size=5) +
#  facet_wrap(facets='Group', labeller = as_labeller(group_names)) +
#  labs(colour = "Clusters") +
#  theme(legend.title = element_text(color="#666666",face="bold", size = 11),
#        legend.text = element_text(color="#666666",face="bold", size = 11), 
#        plot.title = element_text(color="#666666", face="bold", size=16),
#        axis.title = element_text(color="#666666", face="bold"),
#        axis.text.x = element_text(color="#666666", size=10),
#        axis.text.y = element_text(color="#666666", size=10),
#        strip.text = element_text(color="#666666",size=12, face="bold"),
#        legend.position = c(0.84, 0.24)) +
#  scale_x_discrete(name ="Clusters", limits=c(1,5,10,15,20,25,30)) +
#  scale_y_discrete(name ="Mean of Centers",limits=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
#  guides(colour = guide_legend(override.aes = list(size=2))) +
#  ggtitle("Mean of Centers per Cluster, shown separately per Thematic Group")

#now visualise age, gender
#centers.df.age<- subset(centers.df.all, (Group == 'Age'))
#centers.df.gender<- subset(centers.df.all, (Group == 'Gender'))
#centers.df.age<- select(centers.df.age, -32)
#centers.age_agg<- gather(centers.df.age, "cluster","center",2:31)
#centers.df.gender<- select(centers.df.gender, -32)
#centers.gender_agg<- gather(centers.df.gender, "cluster","center",2:31)

#age graph
#centers.age_agg$cluster<- as.integer(centers.age_agg$cluster)
#centers.age_agg<- arrange(centers.age_agg, cluster)

#ggplot(centers.age_agg, aes(x=Theme,y=center, col=as.factor(Theme))) +
#  geom_point(size=3) +
#  facet_wrap(facets='cluster') +
#  labs(colour = "Age Group") +
#  theme(legend.title = element_text(color="#666666",face="bold", size = 11),
#        legend.text = element_text(color="#666666",face="bold", size = 10), 
#        plot.title = element_text(color="#666666", face="bold", size=16),
#        axis.title = element_text(color="#666666", face="bold"),
#        axis.text.x = element_text(color="#666666", angle=45),
#        axis.text.y = element_text(color="#666666", size=10),
#        strip.text = element_text(color="#666666",size=10, face="bold")) +
#  scale_x_discrete(name ="Age Group") +
#  scale_y_discrete(name ="Mean of Centers",limits=c(0,0.2,0.4,0.6,0.8,1)) +
#  ggtitle("Mean of Centers for each Age Group, shown separately per Cluster")

#gender graph
#centers.gender_agg$cluster<- as.integer(centers.gender_agg$cluster)
#centers.gender_agg<- arrange(centers.gender_agg, cluster)

#ggplot(centers.gender_agg, aes(x=Theme,y=center, col=as.factor(Theme))) +
#  geom_point(size=4) +
#  facet_wrap(facets='cluster') +
#  labs(colour = "Gender") +
#  theme(legend.title = element_text(color="#666666",face="bold", size = 11),
#        legend.text = element_text(color="#666666",face="bold", size = 11), 
#        plot.title = element_text(color="#666666", face="bold", size=16),
#        axis.title = element_text(color="#666666", face="bold"),
#        axis.text.x = element_text(color="#666666", size=10),
#        axis.text.y = element_text(color="#666666", size=10),
#        strip.text = element_text(color="#666666",size=12, face="bold")) +
#  scale_color_manual(breaks = c('male','female'), values=c('pink', 'blue')) +
#  scale_x_discrete(name ="Gender") +
#  scale_y_discrete(name ="Mean of Centers",limits=c(0,0.2,0.4,0.6,0.8,1)) +
#  guides(colour = guide_legend(override.aes = list(size=3))) +
#  ggtitle("Mean of Centers for each Gender, shown separately per Cluster")

#get all variables for the model building
Filtered_Ctids<- subset(FR_cluster_data, ctid %in% ctids_use)
FR_Model.Data_4<- merge(Filtered_Ctids, cluster_results, by='ctid')
FR_Model.Data_4<- select(FR_Model.Data_4, 9,2:5)
colnames(FR_Model.Data_4)<- c('Group','Age','Gender','Interest','Sessions')
FR_Model.Data_4$Group<- as.factor(FR_Model.Data_4$Group)

#split the interests variable to simply take the root interest
Interests4<- str_split_fixed(FR_Model.Data_4$Interest, "/", 2)
Interests4<- as.data.frame(Interests4)
colnames(Interests4)<- c('Interest','Extra')

#now change the data to just include the first
FR_Model.Data_4$SimpleInterest<- Interests4$Interest
FR_Model.Data_4<- select(FR_Model.Data_4,  -4)

#now aggregate the data
colnames(FR_Model.Data_4)<- c('Group','Age','Gender','Sessions','Interest')
Agg<- mutate(FR_Model.Data_4, All=paste(Group,'|',Age,'|',Gender,'|',Interest))
Agg<- Agg %>% 
  select(4,6)
sum(Agg$Sessions)
Agg<- aggregate(. ~ All,data=Agg, FUN=sum)
Agg<- Agg[c(2,1)]

x<- str_split_fixed(Agg$All, '[|]', 4)
y<- Agg$Sessions

Cluster_Agg_All<- as.data.frame(x)
colnames(Cluster_Agg_All)<- c('Group','Age','Gender','Interest')
Cluster_Agg_All$Sessions<- y

#function to calculate the accuracy of the random forest model, 
#taking the model and test data set name as parameters
accuracy<- function(x,y) {
  a<- predict(x, newdata = y)
  b<- table(y$Group, a)
  b<- as.data.frame(b)
  colnames(b)<-  c("Group","Pred","Freq")
  b<- mutate(b,b$Group == b$Pred)
  colnames(b)<- c("Group","Pred","Freq","Correct")
  True<- NULL
  for (i in 1:length(b$Correct)) {
    if(b$Correct[i] == TRUE) {
      True[i]<- b$Freq[i]
    } else {
      True[i]<- 0
    }
  }
  False<- NULL
  for (i in 1:length(b$Correct)) {
    if(b$Correct[i] == FALSE) {
      False[i]<- b$Freq[i]
    } else {
      False[i]<- 0
    }
  }
  b$True<- True
  b$False<- False
  TrueSum<- sum(b$True)
  FalseSum<- sum(b$False)
  print(paste("Accuracy:",round((TrueSum/(FalseSum+TrueSum))*100,digits=2),'%'))
}

#use all variables data from clustering with thematic data and with demographic age and gender data
set.seed(1234)
split<- sample.split(Cluster_Agg_All, SplitRatio = 0.5)
Train8<- subset(Cluster_Agg_All, split == TRUE)
Test8<- subset(Cluster_Agg_All, split == FALSE)

#random forest with updated data
FR.Forest7<- randomForest(Group ~ ., data = Train8, nodesize =25, ntree = 200, importance=TRUE)

#calculate the accuracy
accuracy(FR.Forest7,Test8) #11.88%
plot<- varImpPlot(FR.Forest7, main="Importance of Variables in Random Forest")

#updated function for accuracy  for testing
acc_updated<- function(x,y) {
  a<- predict(x, newdata = y)
  b<- table(y$Group, a)
  b<- as.data.frame(b)
  colnames(b)<-  c("Group","Pred","Freq")
  b<- mutate(b,b$Group == b$Pred)
  colnames(b)<- c("Group","Pred","Freq","Correct")
  True<- NULL
  for (i in 1:length(b$Correct)) {
    if(b$Correct[i] == TRUE) {
      True[i]<- b$Freq[i]
    } else {
      True[i]<- 0
    }
  }
  False<- NULL
  for (i in 1:length(b$Correct)) {
    if(b$Correct[i] == FALSE) {
      False[i]<- b$Freq[i]
    } else {
      False[i]<- 0
    }
  }
  b$True<- True
  b$False<- False
  TrueSum<- sum(b$True)
  FalseSum<- sum(b$False)
  print((TrueSum/(FalseSum+TrueSum))*100)
}

#for loop to test node size
#acc<- NULL
#for (i in 1:50) {
#  Forest<- randomForest(Group ~ ., data = Train8, nodesize =i, ntree = 200)
#  acc[i]<-acc_updated(Forest,Test8)
#}

#nodesize.x<- c(1:50)
#nodesize.y<- acc
#nodesize<- as.data.frame(nodesize.x)
#nodesize$nodesize.y<- nodesize.y
#ggplot(nodesize, aes(nodesize.x,nodesize.y)) + geom_point() +
#  geom_smooth() + 
#  ggtitle("Curve to Show Most Effective Nodesize") +
#  labs(x="Nodesize", y="Accuracy (%)") + 
#  theme(plot.title = element_text(color="#666666", face="bold", size=16),
#        axis.title = element_text(color="#666666", face="bold"))

#for loop to test ntree
#acc_ntree<- NULL
#for (i in 100:300) {
#  Forest<- randomForest(Group ~ ., data = Train8, nodesize=40, ntree = i)
#  acc_ntree[i]<-acc_updated(Forest,Test8)
#}

#ntree.x<- c(100:300)
#ntree.y<- acc_ntree
#ntree<- as.data.frame(ntree.x)
#ntree$ntree.y<- ntree.y

#ggplot(ntree, aes(ntree.x,ntree.y)) + geom_point() +
#  geom_smooth() + 
#  ggtitle("Curve to Show Most Effective Number of Trees") +
#  labs(x="ntree", y="Accuracy (%)") + 
#  theme(plot.title = element_text(color="#666666", face="bold", size=16),
#        axis.title = element_text(color="#666666", face="bold"))

#run again with final data and correct ntree, nodesize
set.seed(1234)
split<- sample.split(Cluster_Agg_All, SplitRatio = 0.5)
Train9<- subset(Cluster_Agg_All, split == TRUE)
Test9<- subset(Cluster_Agg_All, split == FALSE)

#random forest with updated data
FR.Forest8<- randomForest(Group ~ ., data = Train9, nodesize =40, ntree = 105)

#calculate the accuracy
accuracy(FR.Forest8,Test9) #13.14%

#create data frame to make usable predictions
Predictions<- as.data.frame(Test9)
Predictions$Predict<- predict(FR.Forest8, newdata = Test9)
Predictions<- arrange(Predictions, desc(Sessions))
View(Predictions)

write.csv(Cluster_Agg_All, 'inputpreddata.csv')

#analysis of centers for write up (using data from building graphs; to run this code please also uncomment code written to build graphs)
#nature<- subset(centers.theme_agg, Group ==1)
#View(subset(analysis, center < 0.75))
#leisure<- subset(centers.theme_agg, Group ==3)
#View(subset(analysis, center < 0.3))
#View(subset(analysis, center > 0.8))
#culture<- subset(centers.theme_agg, Group ==5)
#View(subset(culture, center > 0.61))
#View(subset(culture, center < 0.25784))
#summary(culture$center)
#quantile(culture$center)
#var(culture$center)
#var(wellness$center)
#var(sport$center)
#var(nature$center)
#var(leisure$center)
#wellness<- subset(centers.theme_agg, Group ==2)
#sport<- subset(centers.theme_agg, Group ==4)
#quantile(leisure$center)
#summary(sport$center)
#sd(nature$center)
#sd(culture$center)

#what accuracy would we have the way that we are estimating top destination now?
#SQL query below shows the top three destinations that we would have shown in advertising without this. It returns: New York/Barcelona/Paris
#SELECT c.city, c.country, SUM(search_count) AS Searches
#FROM bi_dwview.f_searches s
#JOIN bi_dwview.d_datestamp d ON (d.id = s.datestamp_ref AND d.datestamp BETWEEN '20160301' AND '20160531')
#JOIN bi_dwview.d_city c ON (s.dest_city_ref = c.id)
#JOIN bi_dwview.d_locale l ON (s.locale_ref = l.id AND l.locale = 'fr')
#GROUP BY c.city, c.country
#ORDER BY Searches DESC
#LIMIT 3

#which clusters are these?
select(subset(cluster_results_named, city == 'Paris' | city == 'New York' | city == 'Barcelona'),2:3)
#Cluster_1$cluster      city
#242                24  New York
#329                24 Barcelona
#499                24     Paris

#therefore, this is like predicting cluster 24 for all
subset<- Test9
subset$Group<- as.integer(subset$Group)
subset<- subset(subset, Group == 24)
true_pred<- sum(subset$Sessions)
all<- sum(Test9$Sessions)

(true_pred/all)*100 #result 2.57%
