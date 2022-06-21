## tsfeatures

#install.packages("tsfeatures", dependencies = TRUE)
library(tsfeatures)

ts_list_ptf <- list()
for(i in 1:30){
  ts_list_ptf[[i]] <-  data[day(data$instrument)==i, "ptf"]
  
}
ptf_features <- tsfeatures(ts_list_ptf)
colnames(ptf_features) <- paste("ptf", colnames(ptf_features), sep = "_")
ptf_features <- ptf_features[,c(4,5,6,7,8,10,11,13,15)]
summary(ptf_features)

ts_list_smf <- list()
for(i in 1:30){
  ts_list_smf[[i]] <-  data[day(data$instrument)==i,"smf" ]
  
}
smf_features <- tsfeatures(ts_list_smf)
colnames(smf_features) <- paste("smf", colnames(smf_features), sep = "_")
smf_features <- smf_features[,c(4,5,6,7,8,10,11,13,15)]
summary(smf_features)

ts_list_netYon <- list()
for(i in 1:30){
  ts_list_netYon[[i]] <-  data[day(data$instrument)==i,"netYon" ]
  
}
netYon_features <- tsfeatures(ts_list_netYon)
colnames(netYon_features) <- paste("netYon", colnames(netYon_features), sep = "_")
netYon_features <- netYon_features[,c(4,5,6,7,8,10,11,13,15)]
summary(netYon_features)

features <- cbind( ptf_features,
                   smf_features,
                   netYon_features)


boxplot(features, main = "Boxplots before scaling")
summary(features)
features <- scale(features)
boxplot(features, main = "Boxplots after scaling")
features <- as.data.frame(features)

library(corrplot)
corrplot(cor(features), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

library(car)

Boxplot(features , id.method="y")
library(psych)
multi.hist(features[,1:4],nrow = 2, ncol=2,density=TRUE,freq=FALSE,bcol="lightblue",
           dcol= c("red","blue"),dlty=c("solid", "dotted"),
           main=colnames(features[,1:4])) 

#Data is clusterable or not
library(hopkins)
hopkins( features )#Value 0.7-1 indicates clustered data
get_clust_tendency(features, n=3) #default of n is 1/10th of number of rows

for(i in 1:30){
  print(hopkins(features, m=3))
}

########### kmeans  ############
#install.packages("factoextra",dependencies = TRUE)
library(factoextra)

k2 <- kmeans(features, centers = 2, nstart = 25)

fviz_cluster(k2, data =features)
#fviz_nbclust(pc_as_features, kmeans, method = "wss")
fviz_nbclust(features, kmeans, method = "silhouette")

library(cluster)
gap_stat <- clusGap(features, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)   

k3 <- kmeans(features, centers = 3, nstart = 25)
k4 <- kmeans(features, centers = 4, nstart = 25)
k5 <- kmeans(features, centers = 5, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "text", data = features) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "text",  data = features) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "text",  data = features) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "text",  data = features) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p2, p3, p4, p5, nrow = 2)

#library(useful)

#plot(k5,  data=features)

#fit <- FitKMeans(features, max.clusters=20, nstart=25)  #Hartigan’s rule
#PlotHartigan(fit)

library(tidyr)
library(dplyr)
km <- kmeans(features, centers = 3, nstart = 25)

features %>%
  as_tibble() %>%
  mutate(cluster = km$cluster,
         state = row.names(features  )) %>%
  ggplot(aes(ptf_trend, smf_spike, color = factor(cluster), label =1:30)) +
  geom_text()

#hierarchical
#hier1 <- hclust(d=dist(features), method = "single")
#plot(hier1,  main="Single")

# hier2 <- hclust(d=dist(features), method = "average")
# plot(hier2,  main="Average")
# 
# hier3 <- hclust(d=dist(features), method = "complete")
# plot(hier3, main="complete")
# 
# hier4 <- hclust(d=dist(features), method = "centroid")
# plot(hier4,  main="centroid")

#cutting for clusters
# plot(hier2)
# 
# rect.hclust(hier2, h=6.3, border="red") #according to height
# 
# rect.hclust(hier2, k=6, border="blue")  #according to number of clusters


# PAM

fviz_nbclust(features,pam, method = "silhouette")
#gap_stat <- clusGap(features, FUN = pam, nstart = 25,
                    K.max = 10, B = 50)
#fviz_gap_stat(gap_stat) 


pam2 <- pam(features, k=2, metric = "manhattan")
pam3 <- pam(features, k=3, metric = "manhattan")
pam4 <- pam(features, k=4, metric = "manhattan")
pam5 <- pam(features, k=5, metric = "manhattan")


# plots to compare
p2 <- fviz_cluster(pam2, geom = "text", data = features) + ggtitle("k = 2")
p3 <- fviz_cluster(pam3, geom = "text",  data = features) + ggtitle("k = 3")
p4 <- fviz_cluster(pam4, geom = "text",  data = features) + ggtitle("k = 4")
p5 <- fviz_cluster(pam5, geom = "text",  data = features) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p2, p3, p4, p5, nrow = 2)

fviz_silhouette(pam2, label=TRUE)
fviz_silhouette(pam3, label=TRUE)
fviz_silhouette(pam4, label=TRUE)
fviz_silhouette(pam5, label=TRUE)

pam5
plot(pam5)

# 
# my_cluster <- pam5$clustering
# df <- data.frame(date = index(ptf_ts ),value = coredata(ptf_ts ),row.names = NULL)
# df$hour <- factor(format(df$date, "%H"))
# df$day <- format(df$date, "%d")
# df$cluster <- as.character(rep(my_cluster, each=24) )
# 
# ggplot(df[df$cluster==3,] ) +
#   aes(hour, ptf, group = day, color = cluster ) + 
#   geom_line()+
#   geom_text(aes( label = day))
# 
# df <- data.frame(date = index(smf_ts ),value = coredata(smf_ts ),row.names = NULL)
# df$hour <- factor(format(df$date, "%H"))
# df$day <- format(df$date, "%d")
# df$cluster <- as.character(rep(my_cluster, each=24) )
# 
# ggplot(df[df$cluster==3,] ) +
#   aes(hour, smf, group = day, color = cluster ) + 
#   geom_line()+
#   geom_text(aes( label = day))
# 
# 
# df <- data.frame(date = index(netYon_ts ),value = coredata(netYon_ts ),row.names = NULL)
# df$hour <- factor(format(df$date, "%H"))
# df$day <- format(df$date, "%d")
# df$cluster <- as.character(rep(my_cluster, each=24) )
# 
# ggplot(df[df$cluster==3,] ) +
#   aes(hour, netYon, group = day, color = cluster ) + 
#   geom_line()+
#   geom_text(aes( label = day))
# 


#xx <- cbind(features[,5], pam5$clustering)
#Boxplot(xx[,1] ~ xx[,2] )

library(reshape2)
features.c <- cbind(as.data.frame( features[,1:4]), pam5$cluster)
colnames(features.c )[5]<-c("Group")

df.m <- melt(features.c, id.var = "Group")
df.m$Group <- as.character(df.m$Group)

p <- ggplot(data = df.m, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill = Group),outlier.size = 1) +
  facet_wrap( ~ variable, scales="free", ncol = 2) +
  xlab(label = NULL) + ylab(label = NULL) + ggtitle("Boxplots for the Clusters") +
  guides(fill=guide_legend(title="Groups"))

p 

library(kableExtra)
GroupsSummary <- describeBy(features.c, features.c[,'Group'])




#dbscan


#install.packages("fpc")
#install.packages("dbscan")

library("fpc")
# Compute DBSCAN using fpc package
# 
# #PCA
# pc <- prcomp(features)
# summary(pc)
# pc_as_features <- pc$x[,1:5]
# pc_as_features <- as.data.frame(pc_as_features)

db <- fpc::dbscan(pc_as_features , eps = 5, MinPts = 5)
# Plot DBSCAN results
plot(db, pc_as_features, main = "DBSCAN")
fviz_cluster(db, pc_as_features, stand = FALSE, geom = "point")
print(db)

library(dbscan)
dbscan::kNNdistplot(pc_as_features, k =  5)
abline(h = 3, lty = 2)


# GMM
ggplot(pc_as_features, aes(x = PC1)) + geom_density()
ggplot(pc_as_features, aes(x = PC2)) + geom_density()
ggplot(pc_as_features, aes(x = PC3)) + geom_density()
ggplot(pc_as_features, aes(x = PC4)) + geom_density()

features <- as.data.frame(features)
ggplot(features, aes(x = ptf_trend)) + geom_density()
ggplot(features, aes(x = ptf_spike)) + geom_density()
ggplot(features, aes(x = ptf_linearity)) + geom_density()
ggplot(features, aes(x = ptf_curvature)) + geom_density()
ggplot(features, aes(x = ptf_e_acf1 )) + geom_density()


