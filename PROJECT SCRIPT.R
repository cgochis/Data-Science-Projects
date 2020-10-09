data = read.csv("fifa19dataFINAL.csv")
str(data)
summary(data)
set.seed(123)
fifasample <- data[sample(1:nrow(data), 1000, replace=T),]

#T-Test: Preferred.Foot to Overall rank
var.test(Overall ~ Preferred.Foot, data=fifasample)
t.test(Overall ~ Preferred.Foot, data=fifasample, var.equal=T)

#T-Tets: Preferred.Foot to International Rep
var.test(International.Reputation ~ Preferred.Foot, data=fifasample)
t.test(International.Reputation ~ Preferred.Foot, data=fifasample, var.equal=F)

#T-Test: Preferred.Foot to Penalties
var.test(Penalties ~ Preferred.Foot, data=fifasample)
t.test(Penalties ~ Preferred.Foot, data=fifasample, var.equal=T)


#ANOVA format mpganova_age<-aov(mpg~modelage, autompg)
overall_work_anova <- aov(Overall ~ Work.Rate, fifasample)
international_work_anova <- aov(International.Reputation ~ Work.Rate, fifasample)
sprint_body_anova <- aov(SprintSpeed ~ Body.Type, fifasample)
agility_body_anova <- aov(Agility ~ Body.Type, fifasample)
stamina_body_anova <- aov(Stamina ~ Body.Type, fifasample)

anova(overall_work_anova)
anova(international_work_anova)
anova(sprint_body_anova)
anova(agility_body_anova)
anova(stamina_body_anova)

TukeyHSD(overall_work_anova)
TukeyHSD(international_work_anova)
TukeyHSD(sprint_body_anova)
TukeyHSD(agility_body_anova)
TukeyHSD(stamina_body_anova)

### It's regression time.......

valuemodel <- lm(Value ~ Age + FKAccuracy + HeadingAccuracy + BallControl + Aggression + Vision + Composure + SlidingTackle,data=fifasample)
summary(valuemodel)

library(olsrr)
ols_step_both_p(valuemodel)

valuemodel2 <- lm(Value ~ Age + BallControl + Vision + Composure,data=fifasample)
summary(valuemodel2)

valuemodel3 <- lm(Value ~ Age + I(Age^2) + BallControl + Vision + Composure,data=fifasample)
summary(valuemodel2)

library(car)
vif(valuemodel2)

par(mfrow=c(2,2))
plot(valuemodel3)
print(valuemodel3)

#Chi-Squared - Body Type & Work Rate

tabletest <- table(fifasample$Body.Type, fifasample$Work.Rate)
tabletest

tabletest2 <- tabletest[ , -5]
tabletest2

chisq <- chisq.test(tabletest2)

par(mfrow=c(1,1))
library(corrplot) #Install it if your R doesn’t have it
corrplot(chisq$residuals, is.cor = FALSE)





#Logistic Regression
fifasample$SkillRepHighLow<-cut(fifasample$Skill.Moves, c(0,2,6), c("LOW", "HIGH"))
summary(fifasample)

set.seed(111) #random number generator
sample1 <- sample.int(n = nrow(fifasample), size = floor(.75*nrow(fifasample)), replace = F)
train1 <- fifasample[sample1, ]
test1  <- fifasample[-sample1, ]
full.fit1 <- glm(SkillRepHighLow~Dribbling + FKAccuracy + BallControl + GKPositioning, family=binomial, data=train1)
summary(full.fit1)
confint(full.fit1)
exp(coef(full.fit1))
library(car)
vif(full.fit1)

train1$probs <- predict(full.fit1, type="response")
train1$probs[1:5] #inspect the first 5 predicted probabilities
contrasts(train1$SkillRepHighLow)
train1$predict <- rep("LOW", nrow(train1))
train1$predict[train1$probs>0.5]<-"HIGH"
table(train1$predict, train1$SkillRepHighLow)
mean(train1$predict==train1$SkillRepHighLow)

test1$prob = predict(full.fit1, newdata=test1, type="response")
test1$predict <- rep("LOW", nrow(test1))
test1$predict[test1$prob>0.5]<-"HIGH"
table(test1$predict, test1$SkillRepHighLow)

mean(test1$predict == test1$SkillRepHighLow)

##TREES
install.packages("randomForest")
library(randomForest)

set.seed(222)
sample2 <- sample.int(n = nrow(fifasample), size = floor(.75*nrow(fifasample)), replace = F)
train2 <- fifasample[sample1, ]
test2  <- fifasample[-sample1, ]

library(rpart)
skillhighlow.rp = rpart(SkillRepHighLow~Crossing +	Finishing +	HeadingAccuracy +	ShortPassing +Volleys	+ Dribbling	+ Curve	+ FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed	+ Agility	+ Reactions	+ Balance	+ ShotPower	+ Jumping	+ Stamina	+ Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes, data = train2, cp=0.0001)
skillhighlow.rp
printcp(skillhighlow.rp)

par(mfrow=c(1,1))
plotcp(skillhighlow.rp)

summary(skillhighlow.rp)
plot(skillhighlow.rp)
text(skillhighlow.rp, all=TRUE, use.n = TRUE)

test2$predictions <- predict(skillhighlow.rp, test2, type="class")
table(test2$SkillRepHighLow, test2$predictions)
mean(test2$SkillRepHighLow == test2$predictions)


skillhighlow.rf = randomForest(SkillRepHighLow~Crossing +	Finishing +	HeadingAccuracy +	ShortPassing +Volleys	+ Dribbling	+ Curve	+ FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed	+ Agility	+ Reactions	+ Balance	+ ShotPower	+ Jumping	+ Stamina	+ Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes, data = train2, importance = T)
skillhighlow.rf

skill.prediction = predict(skillhighlow.rf, test2)
table(skill.prediction, test2$SkillRepHighLow)
mean(test2$SkillRepHighLow == skill.prediction)

plot(skillhighlow.rf)
importance(skillhighlow.rf)
varImpPlot(skillhighlow.rf)







