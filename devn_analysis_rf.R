#use a random forest
library(randomForest)

training.data.old <- training.data
#training.data <- training.data.old
training.data <- merge(training.data, stack.overflow, by = 'Package', all.x = TRUE)
training.data$Topic <- NULL
training.data$LogViewsIncluding <- training.data$LogViewsIncluding + training.data$LogViews
training.data.2 <- training.data
training.data.2$Package <- NULL
training.data.2$Maintainer <- NULL

package.rf <- randomForest(Installed ~ ., ntree=1000, importance=TRUE, data=training.data.2, na.action=na.omit)

