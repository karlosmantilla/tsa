# MODELO LOGIT:

install.packages(c("aod","ggplot2"))

library(aod)
library(ggplot2)

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
summary(mydata)
sapply(mydata, sd)

xtabs(~admit + rank, data = mydata)
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

confint(mylogit)
confint.default(mylogit)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)
exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
    4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
    se = TRUE))
newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})

head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
    size = 1)

with(mylogit, null.deviance - deviance)

with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit)

# MODELO PROBIT

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

mydata$rank <- factor(mydata$rank)

head(mydata)

summary(mydata)

xtabs(~rank + admit, data = mydata)

myprobit <- glm(admit ~ gre + gpa + rank, family = binomial(link = "probit"), 
    data = mydata)

summary(myprobit)

confint(myprobit)

wald.test(b = coef(myprobit), Sigma = vcov(myprobit), Terms = 4:6)

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(myprobit), Sigma = vcov(myprobit), L = l)

newdata <- data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 
    4 * 4), gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4), rank = factor(rep(rep(1:4, 
    each = 100), 4)))

head(newdata)

newdata[, c("p", "se")] <- predict(myprobit, newdata, type = "response", se.fit = TRUE)[-3]

ggplot(newdata, aes(x = gre, y = p, colour = rank)) + geom_line() + facet_wrap(~gpa)

with(myprobit, null.deviance - deviance)

with(myprobit, df.null - df.residual)

with(myprobit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

logLik(myprobit)
