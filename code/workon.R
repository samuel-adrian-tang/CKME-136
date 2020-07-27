#import data
getwd() 
setwd("C:\\Users\\Samuel Tang\\Desktop\\CKME 136\\Dataset")
cleandata <- read.csv("lap_times (edited).csv")
#time(convert into seconds)
library(lubridate)
cleandata$Mins2Secs <- period_to_seconds(ms(cleandata$Time))
#Converting categorical variables
cleandata$AWD = 0
cleandata$AWD[which(grepl("AWD", cleandata$Drivetrain))] = 1
cleandata$RWD = 0
cleandata$RWD[which(grepl("RWD", cleandata$Drivetrain))] = 1
cleandata$FWD = 0
cleandata$FWD[which(grepl("FWD", cleandata$Drivetrain))] = 1
cleandata$MidEngine = 0
cleandata$MidEngine[which(grepl("Mid-Engine", cleandata$Engine_Layout))] = 1
cleandata$RearEngine = 0
cleandata$RearEngine[which(grepl("Rear-Engine", cleandata$Engine_Layout))] = 1
cleandata$FrontEngine = 0
cleandata$FrontEngine[which(grepl("Front-Engine", cleandata$Engine_Layout))] = 1
#subset numeric attributes to allow for correlation analysis
allnumeric <- cleandata[,c(1,2,6:9,12,13,16:25)]
#Subset Final Variables
finalvars <- allnumeric[,c(2,3,7,8,11:13,16)]
#SCALING
finalvars[c(1:5)] <- lapply(finalvars[c(1:5)], function(x) c(scale(x)))
#Creating the clusters
library(NbClust)
res.nbclust <- NbClust(finalvars, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
km4f <- kmeans(finalvars, centers = 4, nstart = 100)
km4f
finalvarscluster <- finalvars
finalvarscluster$cluster1 <- km4f$cluster
finalvarscluster$cluster2 <- 0
finalvarscluster$cluster3 <- 0
finalvarscluster$cluster3[which(finalvarscluster$cluster1 == 3)] <- 1
finalvarscluster$cluster2[which(finalvarscluster$cluster1 == 2)] <- 1
finalvarscluster$cluster1[which(finalvarscluster$cluster1 != 1)] <- 0
#Basic Model, MLR with final vars
finalvarsmodel <- lm(Mins2Secs~Year+Weight+Horsepower+Torque+Cylinders+AWD+MidEngine, finalvars)
summary(finalvarsmodel)
#Modified Model, MLR with final vars+clusters
finalvarsclustermodel <- lm(Mins2Secs~Year+Weight+Horsepower+Torque+Cylinders+AWD+MidEngine+cluster1+cluster2+cluster3, finalvarscluster)
summary(finalvarsclustermodel)


#Model Eval - 1A)
rn_train <- sample(nrow(finalvars), floor(nrow(finalvars)*0.7))
train <- finalvars[rn_train,]
test <- finalvars[-rn_train,]
model_mlr <- lm(Mins2Secs~Year+Weight+Horsepower+Torque+Cylinders+AWD+MidEngine, data=train) 
prediction <- predict(model_mlr, interval="prediction", newdata =test)
errors <- prediction[,"fit"] - test$Mins2Secs
hist(errors)
rmse <- sqrt(sum((errors)^2)/nrow(test))
rel_change <- abs(errors) / test$Mins2Secs
pred5 <- table(rel_change<0.05)["TRUE"] / nrow(test)
table(rel_change)
paste("RMSE:", rmse) #11.56290 (for this instance)
paste("PRED(5):", pred5) #92% of cases with less than 5% error (for this instance)


