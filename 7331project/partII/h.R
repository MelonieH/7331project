usperm <- read.csv(unz("us_perm_visas.csv.zip", filename="us_perm_visas.csv"))
usperm3<-subset(usperm, select = c(case_status,class_of_admission, job_info_work_state))
summary(usperm3)
usperm3$class_of_admission[usperm3$class_of_admission=="H1B"]<-"H-1B"
summary(usperm3$class_of_admission)
usperm6<-data.frame(case_status=NA,class_of_admission="Parole",job_info_work_state =NA)
usperm3<-rbind(usperm3,usperm6)
summary(usperm3$class_of_admission)
usperm3$class_of_admission[usperm3$class_of_admission=="Parol"]<-"Parole"
usperm3$class_of_admission[usperm3$class_of_admission=="Parolee"]<-"Parole"
summary(usperm3$class_of_admission)
summary(usperm3)
usperm3$job_info_work_state[usperm3$job_info_work_state=="CA"]<-"CALIFORNIA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="TX"]<-"TEXAS"
usperm3$job_info_work_state[usperm3$job_info_work_state=="NJ"]<-"NEW JERSEY"
usperm3$job_info_work_state[usperm3$job_info_work_state=="NY"]<-"NEW YORK"
usperm3$job_info_work_state[usperm3$job_info_work_state=="WA"]<-"WASHINGTON"
usperm3$job_info_work_state[usperm3$job_info_work_state=="IL"]<-"ILLINOIS"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MA"]<-"MASSACHUSETTS"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MI"]<-"MICHIGAN"
usperm3$job_info_work_state[usperm3$job_info_work_state=="FL"]<-"FLORIDA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="VA"]<-"VIRGINIA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="GA"]<-"GEORGIA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="NC"]<-"NORTH CAROLINA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="PA"]<-"PENNSYLVANIA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="OH"]<-"OHIO"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MD"]<-"MARYLAND"
usperm3$job_info_work_state[usperm3$job_info_work_state=="AZ"]<-"ARIZONA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="CT"]<-"CONNECTICUT"
usperm3$job_info_work_state[usperm3$job_info_work_state=="OR"]<-"OREGON"
usperm3$job_info_work_state[usperm3$job_info_work_state=="IN"]<-"INDIANA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MO"]<-"MISSOURI"
usperm3$job_info_work_state[usperm3$job_info_work_state=="CO"]<-"COLORADO"
usperm3$job_info_work_state[usperm3$job_info_work_state=="WI"]<-"WISCONSIN"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MN"]<-"MINNESOTA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="SC"]<-"SOUTH CAROLINA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="TN"]<-"TENNESSEE"
usperm3$job_info_work_state[usperm3$job_info_work_state=="DE"]<-"DELAWARE"
usperm3$job_info_work_state[usperm3$job_info_work_state=="AL"]<-"ALABAMA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="KY"]<-"KENTUCKY"
usperm3$job_info_work_state[usperm3$job_info_work_state=="DC"]<-"DISTRICT OF COLUMBIA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="IA"]<-"IOWA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="NE"]<-"NEBRASKA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="UT"]<-"UTAH"
usperm3$job_info_work_state[usperm3$job_info_work_state=="KS"]<-"KANSAS"
usperm3$job_info_work_state[usperm3$job_info_work_state=="LA"]<-"LOUISIANA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="OK"]<-"OKLAHOMA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="AR"]<-"ARKANSAS"
usperm3$job_info_work_state[usperm3$job_info_work_state=="NH"]<-"NEW HAMPSHIRE"
usperm3$job_info_work_state[usperm3$job_info_work_state=="NM"]<-"NEW MEXICO"
usperm3$job_info_work_state[usperm3$job_info_work_state=="NV"]<-"NEVADA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="RI"]<-"RHODE ISLAND"
usperm3$job_info_work_state[usperm3$job_info_work_state=="ID"]<-"IDAHO"
usperm3$job_info_work_state[usperm3$job_info_work_state=="GU"]<-"GUAM"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MS"]<-"MISSISSIPPI"
usperm3$job_info_work_state[usperm3$job_info_work_state=="ND"]<-"NORTH DAKOTA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="VT"]<-"VERMONT"
usperm3$job_info_work_state[usperm3$job_info_work_state=="WV"]<-"WEST VIRGINIA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="HI"]<-"HAWAII"
usperm3$job_info_work_state[usperm3$job_info_work_state=="ME"]<-"MAINE"
usperm3$job_info_work_state[usperm3$job_info_work_state=="SD"]<-"SOUTH DAKOTA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="AK"]<-"ALASKA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MT"]<-"MONTANA"
usperm3$job_info_work_state[usperm3$job_info_work_state=="WY"]<-"WYOMING"
usperm3$job_info_work_state[usperm3$job_info_work_state=="PR"]<-"PUERTO RICO"
usperm3$job_info_work_state[usperm3$job_info_work_state=="VI"]<-"VIRGIN ISLANDS"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MP"]<-"NORTHERN MARIANA ISLANDS"
usperm3$job_info_work_state[usperm3$job_info_work_state=="MH"]<-"MARSHALL ISLANDS"
usperm3$job_info_work_state[usperm3$job_info_work_state==""]<-NA
usperm3<-na.omit(usperm3)
summary(usperm3$job_info_work_state)
usperm3$job_info_work_state<-droplevels(usperm3$job_info_work_state)
usperm3$class_of_admission<-droplevels(usperm3$class_of_admission)
tbl_case<-table(usperm3$class_of_admission,usperm3$case_status)
agg_case <- as.data.frame.matrix(tbl_case)
agg_case <- agg_case / rowSums(agg_case) * 100
head(tbl_case)
head(agg_case)
tbl_state<-table(usperm3$class_of_admission,usperm3$job_info_work_state)
agg_state <- as.data.frame.matrix(tbl_state)
agg_state <- agg_state / rowSums(agg_state) * 100
str(tbl_state)
aggh<-cbind(agg_case,agg_state)
head(aggh)
plot(hclust(d))
hclustd<-hclust(d)
rect.hclust(hclustd,k=2)                  
out.id=cutree(hclustd,k=2) 
out.id
ASW <- sapply(ks, FUN=function(k) {
       fpc::cluster.stats(d, cutree(hclustd, k))$avg.silwidth
   })
plot(ks, ASW, type="l")
ASW <- sapply(k1, FUN=function(k) {
       fpc::cluster.stats(d, cutree(hclustd, k))$avg.silwidth
   })
WSS <- sapply(ks, FUN=function(k) {
  kmeans(aggh, centers=k, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l")
abline(v=3, col="red", lty=2)
kmh <- kmeans(aggh, centers = 3)
cluster_h<-cutree(hclustd,k=2)
sapply(list(
  kmh=kmh$cluster,
  h=cluster_h
),
FUN=function(x)
  fpc::cluster.stats(d, x))[c("within.cluster.ss","avg.silwidth"),]
plot(silhouette(cutree(hclustd, 2), d))
pimage(d)
pimage(d, order=order(hclustd$order))
dissplot(d)