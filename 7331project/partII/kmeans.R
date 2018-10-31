usperm <- read.csv(unz("us_perm_visas.csv.zip", filename="us_perm_visas.csv"))
usperm1 <-subset(usperm,select = c(case_status,country_of_citizenship,country_of_citzenship,pw_amount_9089))
library(tidyr)
usperm1<-unite(usperm1, "country_of_citizenship", country_of_citizenship, country_of_citzenship, sep = "", remove = FALSE)
usperm1$country_of_citizenship<-as.factor(usperm1$country_of_citizenship)
usperm1<-subset(usperm1,select=-3)
usperm1$pw_amount_9089[which(usperm1$pw_amount_9089=="")]<-NA
usperm1$pw_amount_9089<-gsub(",","",usperm1$pw_amount_9089)
usperm1$pw_amount_9089<-as.numeric(usperm1$pw_amount_9089)
usperm1<-na.omit(usperm1)
usperm1$country_of_citizenship[which(usperm1$country_of_citizenship=="")]<-NA
usperm1<-na.omit(usperm1)
agg_wage <-aggregate(usperm1$pw_amount_9089, by = list(usperm1$country_of_citizenship), FUN = function(x) median(x, na.rm = TRUE))
head(agg_wage)
agg_wage <- data.frame(wage = agg_wage$x, row.names = agg_wage$Group.1)
head(agg_wage)
tbl_status <- table(usperm1$country_of_citizenship, usperm1$case_status)
head(tbl_status)
str(tbl_status)
head(as.data.frame(tbl_status))
agg_status <- as.data.frame.matrix(tbl_status)
head(agg_status)
str(agg_status)
agg_status <- agg_status / rowSums(agg_status) * 100
head(agg_status)
agg_status$Certified[agg_status$Certified==0&agg_status$`Certified-Expired`==0&agg_status$Denied==0&agg_status$Withdrawn==0]
agg_status<-na.omit(agg_status)
agg <- cbind(agg_status, agg_wage)
head(agg)
agg_scaled <- scale(agg)
head(agg_scaled)
km <- kmeans(agg_scaled, centers = 6)
pairs(agg_scaled, col = km$cluster)
km$centers
sort(km$cluster)
ks<-2:10
WSS <- sapply(ks, FUN=function(k) {
     kmeans(agg_scaled, centers=k, nstart=5)$tot.withinss
     })
 plot(ks, WSS, type="l")
 abline(v=6, col="red", lty=2)
 library(cluster)
 k <- clusGap(agg_scaled, FUN = kmeans,  nstart = 10, K.max = 10)
 k
 plot(k)
 d1<-dist(agg_scale)
 hc <- hclust(d1, method="complete")
 cluster_complete <- cutree(hc, k=4)
 sapply(list(
        km=km$cluster,
        hc_compl=cluster_complete
        ),
        FUN=function(x)
              fpc::cluster.stats(d1, x))[c("within.cluster.ss","avg.silwidth"),]
 plot(hc)
 rect.hclust(hc,k=6)
 d1 <- dist(agg_scaled)
 plot(silhouette(km$cluster, d1))
 library(seriation)
 pimage(d1)
 pimage(d1, order=order(km$cluster))
 dissplot(d1, labels=km$cluster, options=list(main="k-means with k=6"))
 