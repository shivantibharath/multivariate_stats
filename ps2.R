#q1
q1 <- read.csv("C:/Users/shiva/Downloads/hate_crimes.csv")
View(q1)
hc <- q1[complete.cases(q1),] 

mod1 <- lm(hate_crimes_per_100k_splc ~ median_household_income +share_population_in_metro_areas +share_non_white + gini_index+ share_non_citizen,
           data = hc)
summary(mod1) 
apa.reg.table(mod1, prop.var.conf.level = 0.90)

library(rwa)
# rwa
rwa(hc, "hate_crimes_per_100k_splc" , c("median_household_income", "share_population_in_metro_areas", "share_non_white", "gini_index", 
                         "share_non_citizen"))

#q4
regret <- read.csv("C:/Users/shiva/Downloads/regret_Miss.csv")
View (regret)

is.na(regret)
regret[regret == "NA"] <- NA

regret$Gender<- as.factor(regret$Gender)
regret$Pos<- as.numeric(regret$Pos)

mod2<- lm(tot_RS_app ~ Age + Gender +  Pos + Neg + Extra + Agree 
           + Consc + Neuro + Open + Anxiety + Avoidance,data = regret)
summary(mod2)
apa.reg.table(mod2, prop.var.conf.level = 0.90)

sum(is.na(regret$Pos))

##Use the mice package for MI 
library(mice)
library(Amelia) 

dat <- regret[,c("Age", "Gender","tot_RS_app", "Pos", "Neg", "Extra", "Agree", "Consc", "Neuro", "Open", "Anxiety", "Avoidance")] 
## Plot missing data pattern 
md.pattern(dat) 
imps <- mice(dat, m = 20 ) 

#Plot of MCMC chains 
plot(imps) 

#Plot observed and imputed data with a strip plot 
#and box plots 
stripplot(imps) 
dat 
imps <- mice(dat, m = 20, maxit =  20)
complete(imps, 1)

#pull 1st imputed data set 

#Fit model using the with command 
impM <- with(imps, lm(tot_RS_app ~ Age + Gender +  Pos + Neg + Extra + Agree 
                  + Consc + Neuro + Open + Anxiety + Avoidance)) 

#Pool results with pool, use summary to see more results 
results <- pool(impM) 
results 
summary(results)
pool.r.squared(impM)

impM1 <- with (imps, lm(tot_RS_app~1))
D1(impM, impM1)

citation("mice")