#Dataset Analysis Report
#Aleksandra Viktorova
#March 1, 2020

#----LIBRARY----
library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)
library(cluster)
library(factoextra)
#----UPLOAD DATASET----

#Load the data set into R from the file.
DataSet <- read.csv(file=file.choose(), header=TRUE, sep=";")

#------Questions 1 ----
# Select and Group the data by Sate and Year,summarize the number of victims among women and
# men and then group a data by state. Find the average number of victims among women and men.
St_F <-as_tibble(select(DataSet[DataSet$Victim.Sex == "Female",],State,Year,Victim.Sex)) %>% 
          group_by(State,Year) %>% 
               summarise(mCo_F = length(Victim.Sex)) %>% 
                  group_by(State) %>%
                    summarise(PFemale = mean(mCo_F))

St_M <- as_tibble(select(DataSet[DataSet$Victim.Sex == "Male",],State,Year,Victim.Sex)) %>% 
           group_by(State,Year) %>%
                  summarise(mCo_M = length(Victim.Sex)) %>% 
                       group_by(State) %>%
                        summarise(PMale = mean(mCo_M))

# Create a shared data table
crimes <- data.frame(state=tolower(St_F$State),PFemale=St_F$PFemale,PMale = St_M$PMale)   

# Turn data from the maps package in to a data frame
states_map <- map_data("state")

# Merge the data sets together and sort by group, then order
crime_map <- merge(states_map,crimes,by.x="region", by.y="state") %>% 
               arrange(group, order)

# Build two geo-maps for data for men and women
ggplot(data = crime_map, aes(x =long, y =lat, group = group, fill = PMale)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic")+
  labs(x = "Longitude", y = "Latitude", fill="Average number", 
       title="The average number of victims for states US", subtitle = "among men") 

ggplot(data = crime_map, aes(x =long, y =lat, group = group, fill = PFemale)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic")+
  labs(x = "Longitude", y = "Latitude", fill="Average number", 
       title="The average number of victims for states US", subtitle = "among women")

#------Questions 2 ----
# Select and Group the data by Agency.Type and Crime.Solved,summarize the number of victims
TypeAgency <-as_tibble(select(DataSet,Agency.Type,Crime.Solved,Victim.Sex)) %>% 
             group_by(Agency.Type,Crime.Solved) %>% 
                 summarise(mVi = length(Victim.Sex))

#Create a data table
AgencyCrime <- cbind(TypeAgency[TypeAgency$Crime.Solved == "No",3],
                    TypeAgency[TypeAgency$Crime.Solved == "Yes",3])  
colnames(AgencyCrime) <- unique(TypeAgency$Crime.Solved)
rownames(AgencyCrime) <- unique(TypeAgency$Agency.Type)
AgencyCrime

#Chi-Square Test
chisq.test(AgencyCrime)

#------Questions 3 ----
# Select and Group the data by Perpetrator.Race and Weapon,summarize the number of perpetrators
WePerpetrator <- as_tibble(select(DataSet[DataSet$Perpetrator.Race != "Unknown" & DataSet$Weapon != "Unknown",],
                                       Perpetrator.Race,Perpetrator.Sex,Weapon)) %>% 
                           group_by(Perpetrator.Race,Weapon) %>% 
                               summarise(mPer = length(Perpetrator.Sex)) 
# Look the result
summary(WePerpetrator)

# Create a data table for the three races of perpetrators
PerWeapon <- cbind(WePerpetrator[WePerpetrator$Perpetrator.Race == "Asian/Pacific Islander",3],
                        WePerpetrator[WePerpetrator$Perpetrator.Race == "Black",3],
                        WePerpetrator[WePerpetrator$Perpetrator.Race== "White",3]) 
colnames(PerWeapon) <- c("Asian","Black","White")
rownames(PerWeapon) <- unique(WePerpetrator$Weapon)
PerWeapon

# Chi-Square Test for three races of perpetrators
chisq.test(PerWeapon)

# Chi-Square Test for three races of perpetrators with simulate p-value
chisq.test(PerWeapon,simulate.p.value = TRUE)

# Chi-Square Test for two races of perpetrators without Asians
chisq.test(select(PerWeapon,-Asian))     

#------Questions 4 ----
# Select of the necessary data
AgeTest <- as_tibble(select(DataSet[DataSet$Perpetrator.Age != 0 & DataSet$Victim.Age != 998,],
                            Perpetrator.Age,Victim.Age))

# Consider distribution of variables in each cluster 
boxplot(AgeTest,main="Distribution of two variables")

# T-test for two variables
t.test(AgeTest$Perpetrator.Age,AgeTest$Victim.Age)

#-----Question 5------
# Select of the necessary data and their group by parameters
Age_Victim <- as_tibble(select(DataSet[DataSet$Victim.Age != 998,],State,Victim.Age)) %>% 
                  group_by(State) %>% 
                    summarise(vAv = round(mean(Victim.Age),2))

Age_Perpet <- as_tibble(select(DataSet[DataSet$Perpetrator.Age != 0,],State,Perpetrator.Age)) %>% 
                 group_by(State) %>% 
                    summarise(pAv = round(mean(Perpetrator.Age),2))

Crime_State <-as_tibble(select(DataSet,State,Year,Crime.Solved)) %>% 
                 group_by(State,Year) %>% 
                    summarise(Crn = length(Crime.Solved)) %>% 
                       group_by(State) %>% 
                         summarise(cAv = round(mean(Crn),2))

# Check the correlation between the variables
cor(data.frame(Age_Victim$vAv,Age_Perpet$pAv,Crime_State$cAv))

# Create a data set
Clust_State <- data.frame(Age_Victim$vAv,Crime_State$cAv)
row.names(Clust_State) <- unique(Crime_State$State)

# Compute all the pairwise distances between observations in the data set
Clust_State.dist<-daisy(Clust_State,metric="euclidean")

# Hierarchical cluster analysis on a set of dissimilarities
Clust_State.h<-hclust(Clust_State.dist,method="ward.D")

# Find the optimal number of clusters
fviz_nbclust(Clust_State, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") 

# The dividing a dataset into two clusters
groups_cl <- cutree(Clust_State.h,k=2)
Clust_State$groups_cl <-factor(groups_cl)

#Show the result on a graph
ggplot(data = Clust_State, aes(x =Crime_State.cAv , y = Age_Victim.vAv)) + 
  geom_point(aes(color = groups_cl)) +
  labs(x = "Average number of crimes in state, Numb", y = "Average age of victim in state, Year",
       color="Cluster", title = "The result of dividing a dataset into two clusters")

# The dividing a dataset into four clusters
groups_cl <- cutree(Clust_State.h,k=4)
Clust_State$groups_cl <-factor(groups_cl)

#Show the result on a graph
ggplot(data = Clust_State, aes(x =Crime_State.cAv , y = Age_Victim.vAv)) + 
  geom_point(aes(color = groups_cl))  +
  labs(x = "Average number of crimes in state, Numb", y = "Average age of victim in state, Year",
       color="Cluster", title = "The result of dividing a dataset into four clusters")

#Kruskal test for two variables
kruskal.test(Clust_State$Age_Victim.vAv~Clust_State$groups_cl)
kruskal.test(Clust_State$Crime_State.cAv~Clust_State$groups_cl)

#Cluster dendrogram
plot(Clust_State.h,
     labels = abbreviate(rownames(Clust_State),1,method="left.kept"),
     hang=0.3, 
     xlab="States of USA",
     main="Cluster Dendrogram")

#Select 4 clusters
rect.hclust(Clust_State.h,k=4)




     
