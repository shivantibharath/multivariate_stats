#q1a
q1 <- read.csv("C:/Users/shiva/Downloads/couples.csv")

mod0 <- glm(therapy ~ 1, data = q1, family = "binomial")
summary(mod0)

q1$cohab<- as.factor(q1$cohab)
q1$therapy 

mod1 <- glm(therapy ~ factor(cohab), data = q1, family = "binomial")
summary(mod1)

anova(mod0, mod1, test = "LRT")

#Get odds ratios
exp(coef(mod1))

#Get CI for odds ratios
exp(confint(mod1))

## Use emmeans to get marginal probabilities
library(emmeans)
em <- emmeans(mod1, ~cohab, at = list(cohab = c(0,1)))
summary(em)[,2] #logit scale
exp(summary(em)[,2]) #odds
summary(em, type = "response") #Probability scale

library(performance)
r2_nagelkerke(mod1)

#q1b
q1$cohab<- as.factor(q1$cohab)
q1$therapy<- as.factor(q1$therapy)
chisq.test(table(q1$therapy, q1$cohab), correct = FALSE)

#q2
q2 <- read.csv("C:/Users/shiva/Downloads/turnout.csv")

q2$race<- as.factor(q2$race)
q2$vote<- as.factor(q2$vote)

mod0 <- glm(vote ~ 1,  data = q2, family = "binomial")
summary(mod0)

mod2 <- glm(vote ~ C(race) + age + educate + income, data = q2, family = "binomial")
summary(mod2)

anova(mod0, mod2, test = "LRT")

#Get odds ratios
exp(coef(mod2))

#Get CI for odds ratios 
exp(confint(mod2))

emmeans(mod2, ~race, type = "response")
r2_nagelkerke(mod2)
