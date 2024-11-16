#q1
q1 <- read.csv("C:/Users/shiva/Downloads/Espelage.csv")

#c path (total effect)
modC <- lm(SU ~ FV, data = q1)
summary(modC)
apa.reg.table(modC, prop.var.conf.level=0.90)

#a path
modA <- lm(FP ~ FV, data = q1)
summary(modA)
apa.reg.table(modA, prop.var.conf.level=0.90)

#b and c' paths
modB <- lm(SU ~ FP + FV, data = q1)
summary(modB)
apa.reg.table(modB, prop.var.conf.level=0.90)

modMed <- '
FP ~ a*FV
SU ~ b*FP + cprime*FV

ab := a*b
totEff := a*b + cprime

'
library(lavaan)

#fit model with the sem function
#these results give a test of ab equal to the Sobel test
fit <- sem(modMed, data = q1)
summary(fit, standardized = TRUE)
#get r2
lavInspect(fit, what = "r2")

#Bootstrap CI
fitBoot <- sem(modMed, data = q1, 
               se = "boot", bootstrap = 8000)

summary(fitBoot)

#use the parameterEstimates function to get the CI
parameterEstimates(fitBoot)

## Or use the ci = TRUE option in summary
summary(fitBoot, ci = TRUE)

#Need to use the semTools package to get Monte Carlo CI

library(semTools)

#Use the monteCarloCI function 
monteCarloCI(object = fit, 
             nrep = 50000)

monteCarloCI(object = fit, 
             nrep = 50000, plot = TRUE)

#q2
q2 <- read.csv("C:/Users/shiva/Downloads/bfi.csv")

#main effect
mod1 <- lm(bdi ~ stateanx + bfneur, data = q2)
summary(mod1)
apa.reg.table(mod1, prop.var.conf.level=0.90)

#moderation model
mod2 <- lm(bdi~ stateanx*bfneur, data= q2)
summary(mod2)
apa.reg.table(mod2, prop.var.conf.level=0.90)

#apatables
library(apaTables)
apa.reg.table(mod1, mod2)

#Compare nested models (same as test for the interaction)
anova(mod1, mod2)
summary(anova)
# Plot and probe with interactions package
library(interactions)

# Simple slopes and j-n intervals/plot
sim_slopes(mod2, pred = stateanx, 
           modx = bfneur, jnplot = TRUE)
