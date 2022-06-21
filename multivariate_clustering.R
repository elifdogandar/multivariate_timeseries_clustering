
library(xts)
library(ggplot2)
library(forecast) # for ggseasonplot()
library(lubridate)

#install.packages("readxl")
library(readxl)
data <- read_excel( "nisan_ptf_smf_netYon.xlsx")
summary(data)
str(data)
data$ptf <-scale(as.numeric(sub(",", ".", data$ptf, fixed = TRUE)))
data$smf <- scale(as.numeric(sub(",", ".", data$smf, fixed = TRUE)))
data$netYon <- scale(data$netYon)
data$instrument <- as.POSIXlt(data$instrument)
summary(data)
str(data)

#Converting to time series xts objects
ptf_ts <- as.xts(data$ptf, order.by = data$instrument)
names(ptf_ts) <- "ptf"

smf_ts <- as.xts(data$smf, order.by = data$instrument)
names(smf_ts) <- "smf"

netYon_ts <- as.xts(data$netYon , order.by = data$instrument)
names(netYon_ts) <- "netYon"

#merging time series
ts_all <- merge(ptf_ts,smf_ts,netYon_ts)
ts_all$hour <- hour(index(ts_all))
ts_all$day <- day(index(ts_all))

#is there any na values?
sum(is.na(ts_all))
plot(ptf_ts,  main="Piyasa Takas Fiyatı")
plot(smf_ts, main = "Sistem Marjinal Fiyatı")
plot(netYon_ts, main = "Sistem Yönü")

#Seasonplot ptf

library(zoo)
df <- data.frame(date = index(ptf_ts),value = coredata(ptf_ts),row.names = NULL)
df$hour <- factor(format(df$date, "%H"))
df$day <- format(df$date, "%d")

library(ggplot2)

ggplot(df) +
  aes(hour, ptf, group = day, color = day ) + 
  geom_line()+
  geom_text(aes( label = day))+
  theme(legend.position = c(0.85, 0.22), text = element_text(size=15))+
  ylab(" ")+
  ggtitle("Piyasa Takas Fiyatı") 

ggplot(df[df$day=="07" ,]) +
  aes(hour, ptf, group = day, color = day ) + 
  geom_line()+
  geom_text(aes( label = day))+
  theme(legend.position = c(0.85, 0.22), text = element_text(size=15))+
  ylab(" ")+
  ggtitle("Piyasa Takas Fiyatı") 

#moving averages

library(zoo)
#Make zoo object of data
temp.zoo<-zoo(df$ptf,df$date)

#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
m.av<-rollmean(temp.zoo, 24,fill = list(NA, NULL, NA))

#Add calculated moving averages to existing data frame
df$ptf.av=coredata(m.av)

#Add additional line for moving average in red
ggplot(df, aes(date, ptf)) + geom_line() + 
  geom_line(aes(date,ptf.av),color="red") +
  xlab(" ")+
  ggtitle("Moving Averages")+
  theme( text = element_text(size=15))



#Seasonplot smf

library(zoo)
df <- data.frame(date = index(smf_ts),value = coredata(smf_ts),row.names = NULL)
df$hour <- factor(format(df$date, "%H"))
df$day <- format(df$date, "%d")

library(ggplot2)

ggplot(df) +
  aes(hour, smf, group = day, color = day) + 
  geom_line()+
  geom_text(aes( label = day))+
  theme( text = element_text(size=15))+
  ylab(" ")+
  ggtitle("Sistem Marjinal Fiyatı") 

ggplot(df[df$day=="30" ,]) +
  aes(hour, smf, group = day, color = day) + 
  geom_line()+
  geom_text(aes( label = day))+
  theme( text = element_text(size=15))+
  ylab(" ")+
  ggtitle("Sistem Marjinal Fiyatı") 



#Seasonplot netYon

library(zoo)
df <- data.frame(date = index(netYon_ts),value = coredata(netYon_ts),row.names = NULL)
df$hour <- factor(format(df$date, "%H"))
df$day <- format(df$date, "%d")

library(ggplot2)

ggplot(df) +
  aes(hour, netYon_ts, group = day, color = day) + 
  geom_line()+
  geom_text(aes( label = day))+
  theme( text = element_text(size=15))+
  ylab(" ")+
  ggtitle("Sistem Yönü") 

#moving averages

library(zoo)
#Make zoo object of data
temp.zoo<-zoo(df$netYon,df$date)

#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
m.av<-rollmean(temp.zoo, 24,fill = list(NA, NULL, NA))

#Add calculated moving averages to existing data frame
df$yon.av=coredata(m.av)

#Add additional line for moving average in red
ggplot(df, aes(date, netYon)) + geom_line() + 
  geom_line(aes(date,yon.av),color="red") +
  xlab(" ")+
  ggtitle("Moving Averages")+
theme( text = element_text(size=15))
  


#data preperation for stwclust

ts_list <- list()
for(i in 1:30){
  ts_list[[i]] <- coredata(ts_all[ts_all$day==i, c(1,2,3)  ])
  
}
names(ts_list) <- 1:30



###################  DTWCLUST   ##################

#install.packages("dtwclust", dependencies = TRUE)
library(dtwclust)

# Cluster Validity Indices, Sil, D, CH, SF to be maximized 
# and COP, DB, DBstar to be minimized

#######  PARTITIONAL   #####################
# Multivariate series provided as a list of matrices

# Distance gak, centroid pam
mvc_gak_pam <- tsclust(ts_list, k = 2:6, distance = "gak", centroid = "pam", seed = 200 )
names(mvc_gak_pam) <- paste0("k_", 2:6)

mvc_gak_pam_cvis <- sapply(mvc_gak_pam, cvi, type = "internal")

mvc_gak_pam_k3 <- tsclust(ts_list, k = 3, distance = "gak", centroid = "pam", seed = 200 )
plot(mvc_gak_pam_k3 ,labels = list(nudge_x = -10, nudge_y = 1))
plot(mvc_gak_pam_k3 , type = "sc", clus = 1L) # Only first cluster
plot(mvc_gak_pam_k3 , type = "sc", clus = 2L) # Only first cluster
plot(mvc_gak_pam_k3 , type = "sc", clus = 3L) # Only first cluster


# Distance dtw, centroid pam
mvc_dtw_pam <- tsclust(ts_list, k = 2:6, distance = "dtw", centroid = "pam", seed = 200 )
names(mvc_dtw_pam) <- paste0("k_", 2:6)
mvc_dtw_pam_cvis <- sapply(mvc_dtw_pam, cvi, type = "internal")

mvc_dtw_pam_k2 <- tsclust(ts_list, k = 2, distance = "dtw", centroid = "pam", seed = 200 )
plot(mvc_dtw_pam_k2 ,labels = list(nudge_x = -10, nudge_y = 1))

mvc_dtw_pam_k3 <- tsclust(ts_list, k = 3, distance = "dtw", centroid = "pam", seed = 200 )
plot(mvc_dtw_pam_k3 ,labels = list(nudge_x = -10, nudge_y = 1))

# Distance dtw, centroid dba

mvc_dtw_dba <- tsclust(ts_list, k = 2:6, distance = "dtw", centroid = "dba", seed = 200 )
names(mvc_dtw_dba) <- paste0("k_", 2:6)
mvc_dtw_dba_cvis <- sapply(mvc_dtw_dba, cvi, type = "internal")

mvc_dtw_dba_k2 <- tsclust(ts_list, k = 2, distance = "dtw", centroid = "dba", seed = 200 )
plot(mvc_dtw_dba_k2 ,labels = list(nudge_x = -10, nudge_y = 1))

mvc_dtw_dba_k3 <- tsclust(ts_list, k = 3, distance = "dtw", centroid = "dba", seed = 200 )
plot(mvc_dtw_dba_k3 ,labels = list(nudge_x = -10, nudge_y = 1))
#plot(mvc_dtw_dba_k3 , type = "sc", clus = 2L,labels = list(nudge_x = -10, nudge_y = 1))


#########   HIERARCHICAL   ###################

##### Distance dtw, centroid pam   ###########
mvc_dtw_pam_hier <- tsclust(ts_list, type = "h" ,k = 2:6, distance = "dtw", seed = 200 )
names(mvc_dtw_pam_hier) <- paste0("k_", 2:6)
mvc_dtw_pam_hier_cvis <- sapply(mvc_dtw_pam_hier, cvi, type = "internal")

mvc_dtw_pam_hier_k2 <- tsclust(ts_list, type="h", k = 2, distance = "dtw", seed = 200 )
plot(mvc_dtw_pam_hier_k2, type = "sc", labels = list(nudge_x = -10, nudge_y = 1))
plot(mvc_dtw_pam_hier_k2, frame = TRUE)
rect.hclust(mvc_dtw_pam_hier_k2, k=2, border="blue")  
rect.hclust(mvc_dtw_pam_hier_k2, k=3, border="red")  

mvc_dtw_pam_hier_k3 <- tsclust(ts_list, type="h", k = 3, distance = "dtw", seed = 200 )
plot(mvc_dtw_pam_hier_k3, type = "sc", labels = list(nudge_x = -10, nudge_y = 1))
plot(mvc_dtw_pam_hier_k3, frame=TRUE)

###### Distance gak, centroid pam    ####

mvc_gak_pam_hier <- tsclust(ts_list, type = "h" ,k = 2:6, distance = "gak", seed = 200 )
names(mvc_gak_pam_hier) <- paste0("k_", 2:6)
mvc_gak_pam_hier_cvis <- sapply(mvc_gak_pam_hier, cvi, type = "internal")

mvc_gak_pam_hier_k2 <- tsclust(ts_list, type="h", k = 2, distance = "gak", seed = 200 )
plot(mvc_gak_pam_hier_k2, type = "sc", labels = list(nudge_x = -10, nudge_y = 1))
plot(mvc_gak_pam_hier_k2, frame = TRUE)
rect.hclust(mvc_gak_pam_hier_k2, k=2, border="blue")  
rect.hclust(mvc_gak_pam_hier_k2, k=6, border="red")  

plot(mvc_gak_pam_hier_k2, type = "sc", labels = list(nudge_x = -10, nudge_y = 1))


my_cluster <- mvc_dtw_pam_hier_k3@cluster
consensus_cluster <- c(5,4,4,5,5,
                       3,3,3,4,1,
                       1,1,4,5,3,
                       4,3,5,3,5,
                       4,4,4,4,3,
                       4,4,5,2,2
)

###########  ptf ###############
df <- data.frame(date = index(ptf_ts),value = coredata(ptf_ts),row.names = NULL)
df$hour <- factor(format(df$date, "%H"))
df$day <- format(df$date, "%d")
df$cluster <- as.character(rep(consensus_cluster, each=24) )

ggplot(df[df$cluster==4,] ) +
  aes(hour, ptf, group = day, color = cluster ) + 
  geom_line()+
  geom_text(aes( label = day))


##############  smf ###########
df <- data.frame(date = index(smf_ts),value = coredata(smf_ts),row.names = NULL)
df$hour <- factor(format(df$date, "%H"))
df$day <- format(df$date, "%d")
df$cluster <- as.character(rep(consensus_cluster , each=24) )

ggplot(df ) +
  aes(hour, smf, group = day, color = cluster ) + 
  geom_line()+
  geom_text(aes( label = day))

######## netYon ############

df <- data.frame(date = index(netYon_ts ),value = coredata(netYon_ts ),row.names = NULL)
df$hour <- factor(format(df$date, "%H"))
df$day <- format(df$date, "%d")
df$cluster <- as.character(rep(consensus_cluster, each=24) )

ggplot(df ) +
  aes(hour, netYon, group = day, color = cluster ) + 
  geom_line()+
  geom_text(aes( label = day))







#### pdc
#install.packages("pdc", dependencies = TRUE)
library(pdc)
ts_array <-  array(unlist(ts_list ) , dim = c(24,3,30))
ts_array <- aperm(ts_array, c(1,3,2))
clustering <- pdclust(ts_array)
plot(clustering)
cutree(clustering, k = 3)
plot(clustering, timeseries.as.labels = FALSE, labels = names(colnames(ts_array)))
mdsPlot(clustering )



