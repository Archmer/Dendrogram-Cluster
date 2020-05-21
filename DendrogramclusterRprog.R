# Hierarchical Clustering

#Q1 : A US oil organisation needs to know its sales in various states in US and cluster
#the sates based on sales?

dataset<-read.csv("C:/Users/Clemr/OneDrive/Documents/ML assignment/Hierarchical clusterSAMPLEDATA.xlsx")
dataset

str(dataset)
pairs(dataset) # it helps us to get a quick preview of the variables that can be
                #clustered .for eg: the data points sales vs Fuel_cost seems to have 
                #datapoints grouped closer and similar observatins can be made from our
                #target variable in our sales vs anyother.

#scatter plot
plot(dataset$Fuel_cost~dataset$Sales)
with(text(dataset$Fuel_cost~dataset$Sales, labels = dataset$City, pos=4, cex = .3))

plot(dataset$Fixed_charge~dataset$Sales)
with(text(dataset$Fixed_charge~dataset$Sales, labels = dataset$City, pos = 4, cex = .3))

#normalization
z<- dataset[, -c(1,1)]
m<- apply(z,2,mean)
s<- apply(z,2,sd)
z<- scale(z,m,s)
z

#calculate the Euclidian distance

distance<- dist(z)
distance
print(distance, digits = 3)

#Clustering dendrogram(average)

hc1<- hclust(distance)
hc1
plot(hc1)
plot(hc1, labels = dataset$City, hang = -1)
# the resultant is clubbed using the euclidian distance between means of variables and 
#clubbed dependng on the euclidian distance

# Clutering Membership
member1<- cutree(hc1, 3)
member1
aggregate(z, list(member1), mean)

#actual values
aggregate(dataset[,-c(1,1)],list(member1),mean)

#Ans: The Cluster Dendrogram plot  shows the states with similar sales are clubbed 
i#nto one cluster

  