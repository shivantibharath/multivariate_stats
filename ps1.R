#q1
q1 <- read.csv("C:/Users/shiva/Downloads/regret.csv")
View(q1)
q1 <- q1[complete.cases(q1),] 

#outliers
hist(q1$Age) 
hist(q1$tot_Rgen) 
hist(q1$tot_RS_app) 
hist(q1$Avoidance) 
hist(q1$Anxiety) 
hist(q1$Extra) 
hist(q1$Agree)

#regression model (mod1a)
mod1a <- lm(tot_Rgen ~ Age + Gender, 
            
            data = q1) 
summary(mod1a) 
apa.reg.table(mod1a, prop.var.conf.level = 0.90) 

#checks for outliers on mod1a
#residuals 
resid(mod1a) 

#Standardized Residuals 
rstandard(mod1a) 

#Studentized Residuals 
rstudent(mod1a) 
rconcern <- rstudent(mod1a) > 3 
q1[rconcern,] 

rconcern <- rstudent(mod1a) < -3 
q1[rconcern,] 
rstudent(mod1a)[rconcern] 

#leverage 
hatvalues(mod1a) 
3*(14+1)/80  

#i consider 80 a small sample size hence i used 3 
concern <- hatvalues(mod1a) > .56  

#Find cases to investigate 
q1[concern,] 
hatvalues(mod1a)[concern] 

# Cook's d
cooks.distance(mod1a) 
Cconcern <- cooks.distance(mod1a) > 1 
q1[Cconcern,] 


#regression model (mod1b)
mod1b <- lm(tot_Rgen ~ Age + Gender +  Pos + Neg, 
            
            data = q1) 
summary(mod1b) 
apa.reg.table(mod1b, prop.var.conf.level = 0.90) 
anova(mod1a, mod1b) 

## CI for R2 difference 
ci.R2(5.5978, 2, 75, conf.level = .90) 

#checks for outliers on mod1b
#residuals 
resid(mod1b) 

#Standardized Residuals 
rstandard(mod1b) 

#Studentized Residuals 
rstudent(mod1b) 
rconcern <- rstudent(mod1b) > 3 
q1[rconcern,] 
rconcern <- rstudent(mod1b) < -3 
q1[rconcern,] 
rstudent(mod1b)[rconcern] 

#leverage 
hatvalues(mod1b) 
3*(14+1)/80  

#i consider 80 a small sample size hence i used 3 
concern <- hatvalues(mod1b) > .56  

#Find cases to investigate 
q1[concern,] 
hatvalues(mod1b)[concern] 

# Cook's d 
cooks.distance(mod1b) 
Cconcern <- cooks.distance(mod1b) > 1 
q1[Cconcern,] 

#regression model for mod1c
mod1c <- lm(tot_Rgen ~ Age + Gender +  Pos + Neg + Extra + Agree 
            
            + Consc + Neuro + Open, 
            
            data = q1)  
summary(mod1c) 
apa.reg.table(mod1c, prop.var.conf.level = 0.90) 
anova(mod1b, mod1c)
ci.R2(0.8056, 5, 70, conf.level = .90) 

#checks for outliers on mod1c
#residuals
resid(mod1c) 

#Standardized Residuals 
rstandard(mod1c) 

#Studentized Residuals 
rstudent(mod1c) 
rconcern <- rstudent(mod1c) > 3 
q1[rconcern,] 
rconcern <- rstudent(mod1c) < -3 
q1[rconcern,] 
rstudent(mod1c)[rconcern] 

#leverage 
hatvalues(mod1c) 
3*(14+1)/80  

#i consider 80 a small sample size hence i used 3 
concern <- hatvalues(mod1c) > .56  

#Find cases to investigate
q1[concern,] 
hatvalues(mod1c)[concern] 

# Cook's d 
cooks.distance(mod1c) 
Cconcern <- cooks.distance(mod1c) > 1 
q1[Cconcern,] 


#regression model for mod1d
mod1d <- lm(tot_Rgen ~ Age + Gender +  Pos + Neg + Extra + Agree 
            
            + Consc + Neuro + Open + Anxiety + Avoidance, 
            
            data = q1)  
summary(mod1d)
apa.reg.table(mod1d, prop.var.conf.level = 0.90) 
anova(mod1c, mod1d) 
ci.R2(0.2326, 2, 68, conf.level = .90) 
apa.reg.table(mod1d, prop.var.conf.level = 0.90, filename = "test.doc") 

#checks for outliers on mod1d
#residuals
resid(mod1d) 

#Standardized Residuals 
rstandard(mod1d) 


#Studentized Residuals 

rstudent(mod1d)
rconcern <- rstudent(mod1d) > 3 
q1[rconcern,] 
rconcern <- rstudent(mod1d) < -3 
q1[rconcern,]
rstudent(mod1d)[rconcern] 

#leverage 
hatvalues(mod1d) 
3*(14+1)/80  

#i consider 80 a small sample size hence i used 3 
concern <- hatvalues(mod1d) > .56  


#Find cases to investigate 
q1[concern,] 
hatvalues(mod1d)[concern] 


# Cook's d 
cooks.distance(mod1d) 
Cconcern <- cooks.distance(mod1d) > 1 
q1[Cconcern,] 


#second model 

#regression model for mod2a
mod2a <- lm(tot_RS_app ~ Age + Gender, 
            
            data = q1) 
summary(mod2a) 
apa.reg.table(mod2a, prop.var.conf.level = 0.90) 


#checks for outliers on mod2a

#residuals 
resid(mod2a) 

#Standardized Residuals 
rstandard(mod2a) 

#Studentized Residuals 
rstudent(mod2a) 
rconcern <- rstudent(mod2a) > 3 
q1[rconcern,] 
rconcern <- rstudent(mod2a) < -3 
q1[rconcern,] 
rstudent(mod2a)[rconcern] 

#leverage 
hatvalues(mod2a) 
3*(14+1)/80  

#i consider 80 a small sample size hence i used 3 
concern <- hatvalues(mod2a) > .56  

#Find cases to investigate 
q1[concern,] 
hatvalues(mod2a)[concern] 

# Cook's d 
cooks.distance(mod2a) 
Cconcern <- cooks.distance(mod2a) > 1 
q1[Cconcern,] 

#regression model for mod2b
mod2b <- lm(tot_RS_app ~ Age + Gender +  Pos + Neg, 
            
            data = q1) 
summary(mod2b) 
apa.reg.table(mod2b, prop.var.conf.level = 0.90) 
anova(mod2a, mod2b) 
ci.R2(0.0264, 2, 75, conf.level = .90) 

#checks for outliers on mod2b

#residuals 
resid(mod2b) 

#Standardized Residuals 
rstandard(mod2b) 

#Studentized Residuals 
rstudent(mod2b) 
rconcern <- rstudent(mod2b) > 3 
q1[rconcern,] 
rconcern <- rstudent(mod2b) < -3 
q1[rconcern,] 
rstudent(mod2b)[rconcern] 

#leverage 
hatvalues(mod2b) 
3*(14+1)/80  

#i consider 80 a small sample size hence i used 3 
concern <- hatvalues(mod2b) > .56  

#Find cases to investigate 
q1[concern,] 
hatvalues(mod2b)[concern] 

# Cook's d 
cooks.distance(mod2b) 
Cconcern <- cooks.distance(mod2b) > 1 
q1[Cconcern,] 

#regression model for mod2c
mod2c <- lm(tot_RS_app ~ Age + Gender +  Pos + Neg + Extra + Agree 
            
            + Consc + Neuro + Open, 
            
            data = q1)  
summary(mod2c) 
apa.reg.table(mod2c, prop.var.conf.level = 0.90) 
anova(mod2b, mod2c) 
ci.R2(0.4036, 5, 70, conf.level = .90) 

#checks for outliers on mod2c

#residuals 
resid(mod2c) 

#Standardized Residuals 
rstandard(mod2c) 

#Studentized Residuals 
rstudent(mod2c) 
rconcern <- rstudent(mod2c) > 3 
q1[rconcern,] 
rconcern <- rstudent(mod2c) < -3 
q1[rconcern,] 
rstudent(mod2c)[rconcern] 

#leverage 
hatvalues(mod2c) 
3*(14+1)/80  

#i consider 80 a small sample size hence i used 3 
concern <- hatvalues(mod2c) > .56  

#Find cases to investigate 
q1[concern,] 
hatvalues(mod2c)[concern] 

# Cook's d 
cooks.distance(mod2c) 
Cconcern <- cooks.distance(mod2c) > 1 
q1[Cconcern,] 

#regression model for mod2d
mod2d <- lm(tot_RS_app ~ Age + Gender +  Pos + Neg + Extra + Agree 
            
            + Consc + Neuro + Open + Anxiety + Avoidance, 
            
            data = q1)  
summary(mod2d) 
apa.reg.table(mod2d, prop.var.conf.level = 0.90) 
anova(mod2c, mod2d) 
ci.R2(6.7746, 2, 68, conf.level = .90)
apa.reg.table(mod2d, prop.var.conf.level = 0.90, filename = "test.docx") 

#outlier diagnosis for mod2d

#residuals 
resid(mod2d) 

#Standardized Residuals 
rstandard(mod2d) 

#Studentized Residuals 
rstudent(mod2d) 
rconcern <- rstudent(mod2d) > 3 
q1[rconcern,] 
rstudent(mod2d)[rconcern] 
rconcern <- rstudent(mod2d) < -3 
q1[rconcern,] 
rstudent(mod2d)[rconcern] 

#leverage 
hatvalues(mod2d) 
3*(14+1)/80
concern <- hatvalues(mod1) > .56 

#Find cases to investigate 
q1[concern,] 
hatvalues(mod2d)[concern] 

# Cook's d
cooks.distance(mod2d)
Cconcern <- cooks.distance(mod2d) > 1 
q1[Cconcern,] 


#apa tables for all models
apa.reg.table(mod1a,filename = "mod1a.doc") 
apa.reg.table(mod1b,filename = "mod1b.doc") 
apa.reg.table(mod1c,filename = "mod1c.doc") 
apa.reg.table(mod1d,filename = "mod1d.doc") 

apa.reg.table(mod2a,filename = "mod2a.doc") 
apa.reg.table(mod2b,filename = "mod2b.doc") 
apa.reg.table(mod2c,filename = "mod2c.doc") 
apa.reg.table(mod2d,filename = "mod2d.doc") 


#extra credit

#matrix algebra 

#Add column of 1s for intercept 
q1$intercept <- 1 

#regression model
mod1c <- lm(tot_Rgen ~ Age + Gender +  Pos + Neg + Extra + Agree 
            
            + Consc + Neuro + Open, 
            
            data = q1)  
summary(mod1c) 

# Create a vector for y 
y <- q1$tot_RS_app 

# Create a matrix of X 
x <- as.matrix(q1[, c("intercept", "Age", "Gender", "Pos", "Neg", "Extra", "Agree", 
                          
                          "Consc", "Neuro", "Open")]) 
x 

# Compute regression coefficients 
solve(t(x)%*%x)%*%t(x)%*%y 

#Coefficients from the model 
coef(mod1c) 

## Standardized Slopes 
R <- cor(x[,-1]) 
R 

r <- cor(x[,-1],y) 
r

solve(R)%*%r 

#From model
effectsize(mod1c, method = "basic") 

## Variance of slopes and standard errors 

#I'm going to cheat and pull sigma squared from the fitted model 
sigma2 <- summary(mod1c)$sigma^2 
sigma2 

sigma2 * solve(t(x)%*%x) 

#From model 
vcov(mod1c) 

## Standard errors 
sqrt(diag(sigma2 * solve(t(x)%*%x))) 

summary(mod1c)$coefficients[,2] 