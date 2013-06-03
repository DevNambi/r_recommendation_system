#Collaboration model by Mike Decuir and Dev Nambi
library(randomForest)
library(pROC)


training.data.old <- training.data
#training.data <- training.data.old

# Ensembling with Mike Decuir's model
training.data$MissingDependency <- 0
training.data$DependentInstalled <- 0
training.data$Suggested <- 0
training.data$Imported <- 0
training.data$Viewed <- 0

# add in installation dependency information
all.installed <- subset(installations,subset=Installed==1 | Package == "base",select=c(Package,User))
all.not.installed <- subset(installations,subset=Installed==0 & Package != "R",select=c(Package,User))
all.missing.dependencies <- merge(all.not.installed,depends,by.x="Package",by.y="LinkedPackage")[,2:3]
dependent.installed <- merge(all.installed,depends,by="Package")
dependent.installed <- subset(dependent.installed,subset=LinkedPackage != "R")
suggested <- merge(all.installed,suggests,by="Package")
imported <- merge(all.installed,imports,by="Package")

training.data[with(training.data,paste(Package, User)) %in% with(all.missing.dependencies, paste(Package, User)),"MissingDependency"] <- 1
training.data[with(training.data,paste(Package,User)) %in% with(dependent.installed,paste(LinkedPackage,User)),"DependentInstalled"] <- 1
training.data[with(training.data,paste(Package,User)) %in% with(suggested,paste(LinkedPackage,User)),"Suggested"] <- 1
training.data[with(training.data,paste(Package,User)) %in% with(imported,paste(LinkedPackage,User)),"Imported"] <- 1


# add in StackOverflow information
training.data <- merge(training.data, topics, by = 'Package', all.x = TRUE)
training.data$Topic[is.na(training.data$Topic) == TRUE] <- 0
training.data <- transform(training.data, Topic = factor(Topic))
training.data <- merge(training.data, stack.overflow, by = 'Package', all.x = TRUE)
training.data$Topic.y <- NULL
training.data$LogViewsIncluding <- training.data$LogViewsIncluding + training.data$LogViews

# algorithm 1: logistic regression

logit.fit5 <- glm(Installed ~ LogDependencyCount +
                    LogSuggestionCount +
                    LogImportCount +
                    LogViewsIncluding +
                    LogPackagesMaintaining +
                    CorePackage +
                    RecommendedPackage +
                    LogMedianReputation +
                    LogMedianScore +
                    LogMedianViews +
                    factor(User) +
                    Topic +
                    factor(MissingDependency) +
                    factor(DependentInstalled) +
                    factor(Suggested) +
                    factor(Imported),
                  data = training.data,
                  family = binomial(link = 'logit'),
                  subset=Package != "R" & Package != "base")

summary(logit.fit5)
training.data$Logit5Probabilities[training.data$Package == "R"] <- 0
training.data$Logit5Probabilities[training.data$Package == "base"] <- 1
training.data$Logit5Probabilities[training.data$Package != "R" & training.data$Package != "base"] <- predict(logit.fit5,type="response")

logit.roc <- roc(training.data$Installed, training.data$Logit5Probabilities)
logit.roc$auc #0.9756
plot(logit.roc)


# algorithm 2: a random forest
training.data.2 <- training.data
training.data.2$Package <- NULL
training.data.2$Maintainer <- NULL
training.data.2$LogViews <- NULL
training.data.2$LogMedianScore <- NULL
training.data.2$LogMedianAnswers <- NULL
training.data.2$LogMedianViews <- NULL
training.data.2$LogMedianReputation <- NULL

ptm <- proc.time()
package.rf <- randomForest(Installed ~ ., ntree=400, importance=TRUE, data=training.data.2, na.action=na.omit)
proc.time() - ptm

training.data$RandomForestProbabilities <- NULL
training.data$RandomForestProbabilities <- predict(package.rf, newdata=training.data)
training.data$RandomForestProbabilities[training.data$Package == 'R'] <- 0
training.data$RandomForestProbabilities[training.data$Package == 'base'] <- 1

logit.roc <- roc(training.data$Installed, training.data$RandomForestProbabilities)
logit.roc$auc
plot(logit.roc)


#let's see what an ensemble of the two algorithms looks like
training.data$EnsembleProbabilities <- (training.data$RandomForestProbabilities + training.data$Logit5Probabilities) / 2

logit.roc <- roc(training.data$Installed, training.data$EnsembleProbabilities)
logit.roc$auc #returns .9979
plot(logit.roc)




# now get the results for the test data
# first, make the test data look like the training data
test.data3<-cbind(RowID=as.numeric(rownames(test.data)),test.data)
test.data3 <- merge(test.data3, topics, by = 'Package', all.x = TRUE,sort=FALSE)
test.data3$Topic[which(is.na(test.data3$Topic))] <- 0
test.data3 <- transform(test.data3, Topic = factor(Topic))
test.data3 <- merge(test.data3, stack.overflow, by = 'Package', all.x = TRUE)
test.data3$Topic.y <- NULL
test.data3$LogViewsIncluding <- test.data3$LogViewsIncluding + test.data3$LogViews

test.data3$MissingDependency <- 0
test.data3$DependentInstalled <- 0
test.data3$Suggested <- 0
test.data3$Imported <- 0
test.data3$Viewed <- 0
test.data3[with(test.data3, paste(Package, User)) %in% with(all.missing.dependencies, paste(Package, User)),"MissingDependency"] <- 1
test.data3[with(test.data3,paste(Package,User)) %in% with(dependent.installed,paste(LinkedPackage,User)),"DependentInstalled"] <- 1
test.data3[with(test.data3,paste(Package,User)) %in% with(suggested,paste(LinkedPackage,User)),"Suggested"] <- 1
test.data3[with(test.data3,paste(Package,User)) %in% with(imported,paste(LinkedPackage,User)),"Imported"] <- 1
test.data3<-test.data3[order(test.data3$RowID),]


#run logistic prediction on the test data
test.data3$Logit5Probabilities <- predict(logit.fit5,test.data3,type="response")
test.data3[which(test.data3$Package == "R"),]$Logit5Probabilities <- 0
test.data3[which(test.data3$Package == "base"),]$Logit5Probabilities <- 1

#run random forest prediction on the test data
test.data3$RandomForestProbabilities <- NULL
test.data3$RandomForestProbabilities <- predict(package.rf, newdata=test.data3)

test.data3$RandomForestProbabilities[test.data$Package == 'R'] <- 0
test.data3$RandomForestProbabilities[test.data$Package == 'base'] <- 1

test.data3$EnsembleProbabilities <- (test.data3$RandomForestProbabilities + test.data3$Logit5Probabilities) / 2



write.table(test.data3$RandomForestProbabilities, file="joint-submission-rf.csv", row.names=FALSE, col.names=c("x"))
write.table(test.data3$Logit5Probabilities, file="joint-submission-logistic.csv", row.names=FALSE, col.names=c("x"))
write.table(test.data3$EnsembleProbabilities, file="joint-submission-ensemble.csv", row.names=FALSE, col.names=c("x"))



#write.table(test.data$RandomForestProbabilities, file="test-submission-rf.csv", row.names=FALSE, col.names=c("x"))
#write.table(test.data$EnsembleProbabilities, file="test-submission-ensemble.csv", row.names=FALSE, col.names=c("x"))




