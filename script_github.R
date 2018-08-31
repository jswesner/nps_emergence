###Install and open the packages below####
library(ggplot2)
library(brms)
library(tidyverse)
library(ggridges)
library(scales)
library(stringr)

#####Generalized linear model for individual dry mass#####
mdm<-brm(ind_mg_dry~1,data=dm,family=Gamma(link="log"),
         prior(normal(0,2),class="Intercept"))

pp_check(mdm,type="boxplot") #posterior predictive check
mdm #model outcome
postmdm<-posterior_samples(mdm) #extract posterior samples from each parameter
quantile(dm_p$postdm,probs=c(0.025,0.5,0.975))#summarize posterior samples (median +/- 95% credible int)


###### GAMM for emergence#######
m44<-brm(mgm2dayDM01~s(day_n)+(1|loc/year),data=emerge_data,family=Gamma(link="log"),
         prior=c(prior(normal(2.3,3.9),class="Intercept"),
                 prior(cauchy(0,10),class="sd")),
         chains=4,iter=2000)

pp_check(m44,type="boxplot")#posterior predictive check

m44#model outcome

###Fitted estimates m44###
marg44<-marginal_effects(m44,method="fitted",robust=TRUE)
marg44<-data.frame(marg44$day_n)


###Predictions for new sites m44######
testdata2<-data.frame(day_n=seq(17679,17791,length=112),
                      loc="new")
m44pr2<-data.frame(predict(m44,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                           allow_new_levels = TRUE,robust=TRUE))
m44pr2<-rownames_to_column(m44pr2)
m44pr2$rowname<-as.numeric(m44pr2$rowname)

###### total emergence mgdmm2yr from model m44 #####
testdata2<-data.frame(day_n=seq(17679,17791,length=112),
                      loc="new")
m44pr2s<-data.frame(predict(m44,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                            allow_new_levels = TRUE,summary=FALSE))
m44pr2s<-rownames_to_column(m44pr2s)
m44pr2s$rowname<-as.numeric(m44pr2s$rowname)

m44totp<-apply(m44pr2s,1,sum)
m44totp<-data.frame(m44totp)
mean(m44totp$m44totp)
quantile(m44totp$m44totp,probs=c(0.025,0.5,0.975))