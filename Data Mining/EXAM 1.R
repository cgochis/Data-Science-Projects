######### QUESTION 1 ##########
college_gpa_data <- read.csv('college_gpa_data.csv')
options(scipen = 999)
attach(college_gpa_data)
library(psych)
describe(college_gpa_data)

#a
GPA_MODEL_1 <- lm(GPA ~ SAT + total_hours + athlete + verb_math + hs_size + hs_rank + hs_percentile + female + white + black, data = college_gpa_data)
summary(GPA_MODEL_1)

#d & #e
library(leaps)
GPA_MODEL_SUBSETS <- regsubsets(GPA ~ SAT + total_hours + athlete + verb_math + hs_size + hs_rank + hs_percentile + female + white + black, data = college_gpa_data, nvmax = 10)
GPA_REG_SUBSETS <- summary(GPA_MODEL_SUBSETS)
GPA_REG_SUBSETS
GPA_REG_SUBSETS$adjr2

choose(10,1)
choose(10,2)
choose(10,3)
choose(10,4)
choose(10,5)
choose(10,6)
choose(10,7)
choose(10,8)
choose(10,9)
choose(10,10)

10+45+120+210+252+210+120+45+10+1

#e & f
GPA_BEST_5_MODEL <- lm(GPA ~ SAT + total_hours + hs_percentile + female + black, data = college_gpa_data)
summary(GPA_BEST_5_MODEL)

Z_GPA_DATA <- data.frame(scale(college_gpa_data))
describe(Z_GPA_DATA)

SCALED_BEST_5_MODEL <- lm(GPA ~ SAT + total_hours + hs_percentile + female + black, data = Z_GPA_DATA)
summary(SCALED_BEST_5_MODEL)

FITTED_BEST_5 <- fitted(GPA_BEST_5_MODEL)
library(Metrics)
rmse(GPA, FITTED_BEST_5)


######### QUESTION 2 ##########
las_vegas_mortgage_data <- read.csv('las_vegas_mortgage_data.csv')
attach(las_vegas_mortgage_data)
options(scipen = 999)

#a
addmargins(table(delinquent))
(199/1000) * 100

addmargins(table(mortgage_insurance))
(720/1000) * 100

addmargins(table(ARM))
(716/1000) * 100

addmargins(table(delinquent, mortgage_insurance))
(47/720) * 100

addmargins(table(delinquent, ARM))
(159/716) * 100

#b
VEGAS_MORTGAGE_LPM <- lm(delinquent ~ loan_to_value + refinance + mortgage_insurance + mortgage_rate + mortgage_amount + credit_score + loan_term + ARM, data = las_vegas_mortgage_data)
summary(VEGAS_MORTGAGE_LPM)

FITTED_VEGAS_LPM <- data.frame(fitted(VEGAS_MORTGAGE_LPM))
describe(FITTED_VEGAS_LPM)
FITTED_VEGAS_LPM

#c
VEGAS_LOGIT_MODEL <- glm(delinquent ~ loan_to_value + refinance + mortgage_insurance + mortgage_rate + mortgage_amount + credit_score + loan_term + ARM, family = binomial(), data = las_vegas_mortgage_data)
summary(VEGAS_LOGIT_MODEL)

exp(1.405649)
(4.078173 / (1+4.078173))*100

#d
VEGAS_PROBABILITY <- data.frame(fitted(VEGAS_LOGIT_MODEL))

PREDICTED_DELINQUENT <- ifelse(VEGAS_PROBABILITY > .20, 1, 0)
PREDICTED_DELINQUENT

VEGAS_CONFUSION_MATRIX <- table(PREDICTED_DELINQUENT, delinquent)
VEGAS_CONFUSION_MATRIX

((675 + 154) / 1000) * 100

((126 + 45) / 1000) * 100

#e
library(DiscriMiner)

DELINQUENT <- ifelse(delinquent==1, 'yes', 'no')

VEGAS_LDA_MODEL <- linDA((las_vegas_mortgage_data) [,2:9], DELINQUENT)
summary(VEGAS_LDA_MODEL)
VEGAS_LDA_MODEL$confusion

((711 + 148) / 1000) * 100
((51 + 90) / 1000) * 100

