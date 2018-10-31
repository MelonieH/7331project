library("arules")
library(arulesViz)
uspermf<-subset(usperma,select = c(case_status,country_of_citizenship,pw_amount_9089,class_of_admission, job_info_work_state,SOC_NAME_short))
summary(uspermf)

uspermf1<-uspermf
uspermf1$country_of_citizenship[uspermf$country_of_citizenship!="CHINA"]<-NA
uspermf1<-na.omit(uspermf1)
uspermf1$country_of_citizenship<-droplevels(uspermf1$country_of_citizenship)
uspermf1<-subset(uspermf1,select=-2)
summary(uspermf1)

uspermf2<-uspermf
uspermf2$country_of_citizenship[uspermf$country_of_citizenship!="SOUTH KOREA"]<-NA
uspermf2<-na.omit(uspermf2)
uspermf2$country_of_citizenship<-droplevels(uspermf2$country_of_citizenship)
uspermf2<-subset(uspermf2,select=-2)
summary(uspermf2)

uspermf3<-uspermf
uspermf3$country_of_citizenship[uspermf$country_of_citizenship!="JAPAN"]<-NA
uspermf3<-na.omit(uspermf3)
uspermf3$country_of_citizenship<-droplevels(uspermf3$country_of_citizenship)
uspermf3<-subset(uspermf3,select=-2)
summary(uspermf3)

uspermf4<-uspermf
uspermf4$country_of_citizenship[uspermf$country_of_citizenship!="PHILIPPINES"]<-NA
uspermf4<-na.omit(uspermf4)
uspermf4$country_of_citizenship<-droplevels(uspermf4$country_of_citizenship)
uspermf4<-subset(uspermf4,select=-2)
summary(uspermf4)

uspermf1_disc<-discretizeDF(uspermf1)
summary(uspermf1_disc)
trans1 <- as(uspermf1_disc, "transactions")
dim(trans1)
itemFrequencyPlot(trans1, topN=10)
plot(sort(itemFrequency(trans1, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")

5/nrow(trans1)
fs1 <- apriori(trans1, parameter=list(target="frequent", support=0.0001891432))
fs1
barplot(table(size(fs1)), xlab="itemset size", ylab="count")
inspect(fs1[size(fs1)<2])
inspect(fs1[size(fs1)>5])

rules1 <- apriori(trans1, parameter = list(support = 0.02, confidence = 0.9))
rules1 <- sort(rules, by="lift")
inspect(head(rules1, by = "lift"))
plot(rules1, method = "graph", engine = "html")
rules1 <- apriori(trans1, parameter = list(support = 0.0001891432, confidence = 0.3),
                                    appearance = list(rhs = "case_status=Denied"))
inspect(head(rules1, by = "lift"))
inspectDT(rules1)
plot(rules1, engine = "html")
plot(rules1, method = "graph", engine = "html")

rules1 <- apriori(trans1, parameter = list(support = 0.05, confidence = 0.3),
                  appearance = list(rhs = "case_status=Certified"))
rules1 <- sort(rules1, by="lift")
inspect(head(rules1, by = "lift"))
plot(rules1, engine = "html")
plot(rules1, method = "graph", engine = "html")
inspectDT(rules1)


uspermf2_disc<-discretizeDF(uspermf2)
summary(uspermf2_disc)
trans2 <- as(uspermf2_disc, "transactions")
dim(trans2)
itemFrequencyPlot(trans2, topN=10)
plot(sort(itemFrequency(trans2, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")

5/nrow(trans2)
fs2 <- apriori(trans2, parameter=list(target="frequent", support=0.0002716505))
fs2
barplot(table(size(fs2)), xlab="itemset size", ylab="count")
inspect(fs2[size(fs2)<2])
inspect(fs2[size(fs2)>4])

rules2 <- apriori(trans2, parameter = list(support = 0.02, confidence = 0.9))
inspect(head(rules2, by = "lift"))
plot(rules2, method = "graph", engine = "html")
rules2 <- apriori(trans2, parameter = list(support = 0.00035, confidence = 0.3),
                  appearance = list(rhs = "case_status=Denied"))
inspect(head(rules2, by = "lift"))
inspectDT(rules2)
plot(rules2, engine = "html")
plot(rules2, method = "graph", engine = "html")

rules2 <- apriori(trans2, parameter = list(support = 0.04, confidence = 0.3),
                  appearance = list(rhs = "case_status=Certified"))
inspect(head(rules2, by = "lift"))
inspectDT(rules2)
plot(rules2, engine = "html")
plot(rules2, method = "graph", engine = "html")



uspermf3_disc<-discretizeDF(uspermf3)
summary(uspermf3_disc)
trans3 <- as(uspermf3_disc, "transactions")
dim(trans3)
itemFrequencyPlot(trans3, topN=10)
plot(sort(itemFrequency(trans3, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")

5/nrow(trans3)
fs3 <- apriori(trans3, parameter=list(target="frequent", support=0.001828822))
fs3
barplot(table(size(fs3)), xlab="itemset size", ylab="count")
inspect(fs3[size(fs3)<2])
inspect(fs3[size(fs3)>4])
rules3 <- apriori(trans3, parameter = list(support = 0.008, confidence = 0.9))
inspect(head(rules3, by = "lift"))
plot(rules3, method = "graph", engine = "html")
rules3 <- apriori(trans3, parameter = list(support = 0.0011, confidence = 0.3),
                  appearance = list(rhs = "case_status=Denied"))
inspect(head(rules3, by = "lift"))
inspectDT(rules3)
plot(rules3, engine = "html")
plot(rules3, method = "graph", engine = "html")

rules3 <- apriori(trans3, parameter = list(support = 0.03, confidence = 0.3),
                  appearance = list(rhs = "case_status=Certified"))
inspect(head(rules3, by = "lift"))
plot(rules3, engine = "html")
plot(rules3, method = "graph", engine = "html")
inspectDT(rules3)


uspermf4_disc<-discretizeDF(uspermf4)
summary(uspermf4_disc)
trans4 <- as(uspermf4_disc, "transactions")
dim(trans4)
itemFrequencyPlot(trans4, topN=10)
plot(sort(itemFrequency(trans4, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")

5/nrow(trans4)
fs4 <- apriori(trans4, parameter=list(target="frequent", support=0.0006457445))
fs4
barplot(table(size(fs4)), xlab="itemset size", ylab="count")
inspect(fs4[size(fs4)<2])
inspect(fs4[size(fs4)>4])
rules4 <- apriori(trans4, parameter = list(support = 0.03, confidence = 0.9))
inspect(head(rules4, by = "lift"))
plot(rules4, method = "graph", engine = "html")
rules4 <- apriori(trans4, parameter = list(support = 0.01, confidence = 0.3),
                  appearance = list(rhs = "case_status=Denied"))
inspect(head(rules4, by = "lift"))
inspectDT(rules4)
plot(rules4, engine = "html")
plot(rules4, method = "graph", engine = "html")

rules4 <- apriori(trans4, parameter = list(support = 0.03, confidence = 0.3),
                  appearance = list(rhs = "case_status=Certified"))
inspect(head(rules4, by = "lift"))
plot(rules4, engine = "html")
plot(rules4, method = "graph", engine = "html")
inspectDT(rules4)


