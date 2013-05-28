#!/usr/bin/Rscript

logit.fit <- glm(Installed ~ LogDependencyCount +
                             LogSuggestionCount +
                             LogImportCount +
                             LogViewsIncluding +
                             LogPackagesMaintaining +
                             CorePackage +
                             RecommendedPackage +
                             factor(User), #adds user as a factor.
                 data = training.data,
                 family = binomial(link = 'logit'))

summary(logit.fit)
