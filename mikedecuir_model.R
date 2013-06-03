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
training.data[which(training.data$Package == "R"),]$Logit5Probabilities <- 0
training.data[which(training.data$Package == "base"),]$Logit5Probabilities <- 1
training.data[which(training.data$Package != "R" & training.data$Package != "base"),]$Logit5Probabilities <- predict(logit.fit5,type="response")
logit.fit5.roc <- roc(training.data$Installed,training.data$Logit5Probabilities)
logit.fit5.roc$auc #0.9735 on training data

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