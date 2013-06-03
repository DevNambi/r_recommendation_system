#use a random forest
library(randomForest)
library(pROC)

training.data.old <- training.data
#training.data <- training.data.old
training.data <- merge(training.data, topics, by = 'Package', all.x = TRUE)
training.data$Topic[is.na(training.data$Topic) == TRUE] <- 0
training.data <- transform(training.data, Topic = factor(Topic))
training.data <- merge(training.data, stack.overflow, by = 'Package', all.x = TRUE)
training.data$Topic.y <- NULL
training.data$LogViewsIncluding <- training.data$LogViewsIncluding + training.data$LogViews
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





logit.fit <- glm(Installed ~ LogDependencyCount +
                   LogSuggestionCount +
                   LogImportCount +
                   LogViewsIncluding +
                   LogPackagesMaintaining +
                   LogMedianReputation +
                   LogMedianScore +
                   LogMedianViews +
                   CorePackage +
                   RecommendedPackage +
                   factor(User) +
                   Topic, #adds topic as a factor
                 data = training.data,
                 family = binomial(link = 'logit'))

# summary(logit.fit)
# length(predict(logit.fit))
training.data$LogitProbabilities <- NULL
training.data$LogitProbabilities <- predict(logit.fit,type="response")
training.data$LogitProbabilities[training.data$Package == 'R'] <- 0
training.data$LogitProbabilities[training.data$Package == 'base'] <- 1

training.data$EnsembleProbabilities <- (training.data$RandomForestProbabilities + training.data$LogitProbabilities) / 2



logit.roc <- roc(training.data$Installed, training.data$EnsembleProbabilities)
logit.roc$auc
plot(logit.roc)




test.data.old <- test.data
#test.data <- test.data.old
test.data$rownum <- seq_len(nrow(test.data))
test.data <- merge(test.data, topics, by = 'Package', all.x = TRUE)
test.data$Topic[is.na(test.data$Topic) == TRUE] <- 0
test.data <- transform(test.data, Topic = factor(Topic))
test.data <- merge(test.data, stack.overflow, by = 'Package', all.x = TRUE)
test.data$Topic.y <- NULL
test.data$LogViewsIncluding <- test.data$LogViewsIncluding + test.data$LogViews
test.data <- test.data[with(test.data, order(rownum)), ]

test.data.2 <- test.data
test.data.2$Package <- NULL
test.data.2$Maintainer <- NULL
test.data.2$LogViews <- NULL
test.data.2$LogMedianScore <- NULL
test.data.2$LogMedianAnswers <- NULL
test.data.2$LogMedianViews <- NULL
test.data.2$LogMedianReputation <- NULL

test.data$RandomForestProbabilities <- NULL
test.data$RandomForestProbabilities <- predict(package.rf, newdata=test.data.2)

test.data$RandomForestProbabilities[test.data$Package == 'R'] <- 0
test.data$RandomForestProbabilities[test.data$Package == 'base'] <- 1


test.data$LogitProbabilities <- NULL
test.data$LogitProbabilities <- predict(logit.fit,type="response", newdata=test.data)
test.data$LogitProbabilities[test.data$Package == 'R'] <- 0
test.data$LogitProbabilities[test.data$Package == 'base'] <- 1

test.data$EnsembleProbabilities <- (test.data$RandomForestProbabilities + test.data$LogitProbabilities) / 2

#write.table(test.data$RandomForestProbabilities, file="test-submission-rf.csv", row.names=FALSE, col.names=c("x"))
#write.table(test.data$EnsembleProbabilities, file="test-submission-ensemble.csv", row.names=FALSE, col.names=c("x"))



# Ensembling with Mike Decuir's model
training.data$MissingDependency <- 0
training.data$DependentInstalled <- 0
training.data$Suggested <- 0
training.data$Imported <- 0
training.data$Viewed <- 0

all.installed <- subset(installations,subset=Installed==1 | Package == "base",select=c(Package,User))
all.not.installed <- subset(installations,subset=Installed==0 & Package != "R",select=c(Package,User))
all.missing.dependencies <- merge(all.not.installed,depends,by.x="Package",by.y="LinkedPackage")[,2:3]
dependent.installed <- merge(all.installed,depends,by="Package")
dependent.installed <- subset(dependent.installed,subset=LinkedPackage != "R")
suggested <- merge(all.installed,suggests,by="Package")
imported <- merge(all.installed,imports,by="Package")

training.data[with(training.data, paste(Package, User)) %in% with(all.missing.dependencies, paste(Package, User)),"MissingDependency"] <- 1
training.data[with(training.data,paste(Package,User)) %in% with(dependent.installed,paste(LinkedPackage,User)),"DependentInstalled"] <- 1
training.data[with(training.data,paste(Package,User)) %in% with(suggested,paste(LinkedPackage,User)),"Suggested"] <- 1
training.data[with(training.data,paste(Package,User)) %in% with(imported,paste(LinkedPackage,User)),"Imported"] <- 1

logit.fit5 <- glm(Installed ~ LogDependencyCount +
                    LogSuggestionCount +
                    LogImportCount +
                    LogViewsIncluding +
                    LogPackagesMaintaining +
                    CorePackage +
                    RecommendedPackage +
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
logit.fit5.roc <- roc(training.data$Installed,training.data$Logit5Probabilities)
logit.fit5.roc$auc #0.9756 on training data

test.data3<-cbind(RowID=as.numeric(rownames(test.data)),test.data)
test.data3 <- merge(test.data3, topics, by = 'Package', all.x = TRUE,sort=FALSE)
test.data3$Topic[which(is.na(test.data3$Topic))] <- 0
test.data3 <- transform(test.data3, Topic = factor(Topic))

test.data3$MissingDependency <- 0
test.data3$DependentInstalled <- 0
test.data3$Suggested <- 0
test.data3$Imported <- 0
test.data3[with(test.data3, paste(Package, User)) %in% with(all.missing.dependencies, paste(Package, User)),"MissingDependency"] <- 1
test.data3[with(test.data3,paste(Package,User)) %in% with(dependent.installed,paste(LinkedPackage,User)),"DependentInstalled"] <- 1
test.data3[with(test.data3,paste(Package,User)) %in% with(suggested,paste(LinkedPackage,User)),"Suggested"] <- 1
test.data3[with(test.data3,paste(Package,User)) %in% with(imported,paste(LinkedPackage,User)),"Imported"] <- 1
test.data3<-test.data3[order(test.data3$RowID),]

test.data3$Logit5Probabilities <- predict(logit.fit5,test.data3,type="response")
test.data3[which(test.data3$Package == "R"),]$Logit5Probabilities <- 0
test.data3[which(test.data3$Package == "base"),]$Logit5Probabilities <- 1

