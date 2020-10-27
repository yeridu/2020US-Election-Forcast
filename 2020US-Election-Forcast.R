#### FinalPOL697B
# Mario Morales
# Oct 6, 2020
# School of Government and Public Policy
# University of Arizona
# mariomorales@email.arizona.edu

####LIBRARIES
library(foreign) #to open data
library(gmodels) #to crosstables
library(tidyverse) #join files
library(lme4) #regressions
library(lmerTest) #regression
library(dplyr)
library(survey)  
library(mice) 
library(reshape2)
library(zoo)
library(pscl) #classify predictions as 0 and 1
library(lmtest)
library(ggplot2)
library(car)
library(ggridges)
library(MASS)
library(rstanarm)
library(bayesplot)
library(aod)
library(expss)

####DATASETS (https://projects.economist.com/us-2020-forecast/president/how-this-works)
f1 <- read.csv("C:/Users/Mario/Documents/Documents/DOCTORADO2/3sem/POL683/Forecasting/hei3.csv")

####DATA CLEANING
#Explore variables
class(f1$state) #factor
summary(f1$state)
class(f1$biden) #integer
summary(f1$biden) #mean=49
class(f1$trump) #integer
summary(f1$trump) #mean=44

#Recode
f1$state2<-car::recode(f1$state, "c('AK', 'AL', 'AZ', 'AR', 'FL', 'GA', 'IA', 'ID', 'IN',
                                    'MA', 'MD', 'MO', 'MS', 'ND', 'NE', 'NH', 'OH', 'OK',
                                    'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'WV', 'WY')='Republican'; 
                                  c('CA', 'CO', 'CT', 'DE', 'HI', 'IL', 'KS', 'KY', 'LA',
                                    'ME', 'MI', 'MN', 'MT', 'NV', 'NJ', 'NM', 'NY', 'NC',
                                    'OK', 'OR', 'PA', 'RI', 'VA', 'WA', 'WI')='Democrat'; else=NA")  #0=Rep & 1=Dem
class(f1$state2)
summary(f1$state2)
f1$state3<-as.numeric(f1$state2)
class(f1$state3)
summary(f1$state3)

f1$ratio1<-f1$biden/f1$trump
class(f1$ratio1) #numeric
summary(f1$ratio1) #mean=1.14
table(f1$ratio1)

f1$ratio2<-as.factor(ifelse(f1$ratio1 > 1, "Biden","Trump")) #1=Biden support
class(f1$ratio2) #factor
summary(f1$ratio2)

####VARIABLES SELECTION (NAs values out)
labels(f1)
f1 <- f1 %>%
  dplyr::select(state, state2, state3, ratio1, ratio2)
f1<-na.omit(f1) #Ignore NAs values
#Label final variables
var_lab(f1$state)="State original"
var_lab(f1$state2)="State categorical"
var_lab(f1$state3)="State continuous"
var_lab(f1$ratio1)="Ratio continuous"
var_lab(f1$ratio2)="Ratio categorical"

####PLOTS BASED ON DATACAMP (HIERARCHICAL & MIXED EFFECTS MODELS IN R)
# Plot the data prior manipulation
ggplot(data = f1, aes(y = ratio1, x = state, color=state2)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_manual(name="Party", values=c("blue","red"))+
  labs(y="Biden/Trump Ratio", x="States", title="Figure 1. Biden/Trump Ratio in Republican and Democrat States", caption="The Economist 10/25/20") +
  geom_hline(yintercept=1)

#PROFESSIONALIZATION GGPLOT EXAMPLE (https://github.com/crweber9874/jupyter/blob/master/Professionalization%20R%20Tutorial.ipynb)
####NULL MODEL
m1<-glm(relevel(ratio2, ref="Trump")~1, f1, family=binomial("logit"))
summary(m1)
print(m1$coef) #0.9621907 
confint(m1) #CIs using profiled log-likelihood (0.8218182 1.1055667)
#confint.default(m1) #CIs using SE
#For a change from Republican- to Democrat-State, the logit is expected to change by 0.9621907  
exp(m1$coef) #2.617424
#For a change from Republican- to Democrat-States, the odds are expected to change by a factor of 2.617424
#For a change from Republican- to Democrat-States, the odds are increased by a factor of 2.617424
#For a change from Republican- to Democrat-States, the odds are expected to be 2.617424 times greater/larger
100*(exp(m1$coef)-1)
#For a change from Republican states to Democrat ones, the percentage change in the odds is 161.7424
exp(cbind(OR = coef(m1), confint(m1))) # (2.274632, 3.020936)

#### MARGINAL PROBABILITIES
print("Probability of voting for Trump")
print((1)/(1+sum(exp(coef(m1)[1])))) #1-coef(m1) %*% c(1) %>% plogis() 
#There is a 27.64398% probability of voting for Trump
print("Probability of voting for Biden")
print(exp(coef(m1)[1])/(1+sum(exp(coef(m1)[1])))) #coef(m1) %*% c(1) %>% plogis() 
#There is a 72.35602% probability of voting for Biden
table(f1$ratio2)/sum(table(f1$ratio2))

#PROFESSIONALIZATION R TUTORIAL_FINAL (https://github.com/crweber9874/jupyter/blob/master/Professionalization%20R%20Tutorial_Final.ipynb)
#https://stats.idre.ucla.edu/r/dae/logit-regression/

####FINAL MODEL
m2<-glm(relevel(ratio2, ref="Trump")~relevel(state2, ref="Republican"), f1, family=binomial("logit"))
summary(m2) # 1.5585
exp(cbind(OR = coef(m2), confint(m2)))
#For a change from Republican states to a Democrat ones, the logit of Biden support is expected to change by 4.751774
#test for an overall effect of state2 (b supplies coeff, sigma supplies variance covariance matrix of error terms, and terms refers to terms in the model being tested)
100*(exp(m2$coef)-1)
#For a change from Republican states to Democrat ones, the percentage change in the odds of voting for Biden is 375.17738 

####CONDITIONAL PROBABILITIES (PROFESSIONALIZATION GGPLOT EXAMPLE)
print("Probability of voting for Biden if someone lives in a Republican or Democrat state")
coef(m2) %*% c(1, 0) %>% plogis() ### Pr(Biden|Rep) 0.5472637
#There is a 54.72637% probability of voting for Biden given someone lives in a state governed by Republicans
coef(m2) %*% c(1, 1) %>% plogis() ### Pr(Biden|Dem) 0.8517179
#There is a 85.17179% probability of voting for Biden given someone lives in a state governed by Democrats
print("Probability of voting for Trump if someone lives in a Republican or Democrat state")
1-(coef(m2) %*% c(1, 0) %>% plogis()) ### Pr(Biden|Rep) 0.4527363
#There is a 45.27363% probability of voting for Trump given someone lives in a state governed by Republicans
1-(coef(m2) %*% c(0, 1) %>% plogis()) ### Pr(Trump|Dem) 0.1738594
#There is a 17.38594% probability of voting for Trump given someone lives in a state governed by Democrats

#PREDICTED PROBABILITIES (I recode the variables to 0-1 values because I got errors runing the code using Trump-Biden and Republican-Democrat)
f1$ratio2<-as.factor(ifelse(f1$ratio1 > 1, 1,0)) #1=Biden support
class(f1$ratio2) #factor
summary(f1$ratio2)
f1$state2<-car::recode(f1$state, "c('AK', 'AL', 'AZ', 'AR', 'FL', 'GA', 'IA', 'ID', 'IN',
                                    'MA', 'MD', 'MO', 'MS', 'ND', 'NE', 'NH', 'OH', 'OK',
                                    'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'WV', 'WY')='0'; 
                                  c('CA', 'CO', 'CT', 'DE', 'HI', 'IL', 'KS', 'KY', 'LA',
                                    'ME', 'MI', 'MN', 'MT', 'NV', 'NJ', 'NM', 'NY', 'NC',
                                    'OK', 'OR', 'PA', 'RI', 'VA', 'WA', 'WI')='1'; else=NA")
class(f1$state2) #factor
summary(f1$state2)

m2<-glm(ratio2~state2, f1, family=binomial("logit"))
summary(m2) #  1.5585
exp(cbind(OR = coef(m2), confint(m2))) #4.751774 3.5110437 6.476807

nd1<-with(f1, data.frame(state2=factor(0:1))) #create new dataframe
nd1
nd1$state2p<-predict(m2, newdata = nd1, type="response")
nd1
#The predicted probability of Biden support is 0.5472637 for states governed by Republicans
#The predicted probability of Biden support is 0.8517179 for states governed by Democrats
nd2<-cbind(nd1, predict(m2, newdata = nd1, type="link", se=TRUE))
nd2<-within(nd2,{PredictedProb<-plogis(fit)
UL<-plogis(fit + (1.96*se.fit))
LL<-plogis(fit - (1.96*se.fit))})
nd2

#SIGNIFICANCE OF OVERAL MODEL
wald.test(b=coef(m2), Sigma = vcov(m2), Terms=2)
#The X2 test statistic of 99.7, with 1 df is associated with a p-value <0.001 indicating that the overall effect of the variable state is statistically significant. 
#Find whether the model with predictors fits significantly better than a model with just an intercept (i.e., a null model)
with(m2, null.deviance - deviance) #Diff in deviance for the two models is 75.46868
with(m2, df.null-df.residual)#df for the difference between the models is 1
with(m2, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail=FALSE))#p-value is 3.712489e-18
#X2 of 108.1577 with 1 df and associated p-value <0.001 tells that the model as a whole fits significantly better than the empty model
logLik(m2)

####INTRODUCE UNCERTAINTY
cov_b_matrix<-vcov(m2)
## Covariance matrix of parameters
cov_b_matrix # Intercept (-0.02797068) and coefficient (0.06702944)
sim_b<-MASS::mvrnorm(1000, coef(m2), cov_b_matrix)
dim(sim_b) # 1000 2
x_sim=rbind(c(1,0),
            c(1,1))
print("The design matrix")
x_sim #2x2 matrix
post_dat<-x_sim %*% t(sim_b) %>% plogis() 
dim(post_dat) # 2 1000
post_dat<-data.frame(t(post_dat))
head(post_dat)
## Each column represents 1000 simulations of a pr(y=1) for each party
names(post_dat)<-c("Republican", "Democrat")
post_dat$id<-seq(1:nrow(post_dat))
head(post_dat)
mean(post_dat[,1])
mean(post_dat[,2])
1-mean(post_dat[,1])
1-mean(post_dat[,2])
plot_dat<-post_dat %>% melt(id="id")
head(plot_dat)
# Joy Plot:
ggplot(data=plot_dat, aes(y=as.factor(variable),
                          x=value, fill=as.factor(variable))) +
  geom_density_ridges(alpha=0.8) +
  scale_fill_manual(name="", values=c("red", "blue"))+
  labs(title="Figure 2. Biden Support in Republican and Democrat States",
       caption="The Economist 10/25/20")+
  xlab("Probability")+
  ylab("Party")