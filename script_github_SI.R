library(brms)
library(tidyverse)
library(ggridges)
library(scales)
library(stringr)
#########  FIG S1 - test for site influence ######
#ALL sites
testdata2<-data.frame(day_n=seq(17679,17791,length=112),
                      loc="new")
m44pr2s<-data.frame(predict(m44,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                            allow_new_levels = TRUE,summary=FALSE))
m44pr2s<-rownames_to_column(m44pr2s)
m44pr2s$rowname<-as.numeric(m44pr2s$rowname)

m44totp<-apply(m44pr2s,1,sum)
m44totp<-data.frame(m44totp)

predict_gram<-m44totp%>%
  summarise('low95'=quantile(m44totp/1000,probs=0.025),
            `median`=quantile(m44totp/1000, probs=0.5),
            'mean'=mean(m44totp/1000),
            `upper95`=quantile(m44totp/1000, probs=0.975),
            'sd'=sd(m44totp/1000))

predict_gram


#without 'above' sites
m44_noabovem<-brm(mgm2dayDM01~s(day_n,k=-1)+(1|loc/year),data=subset(all_data,trt=="ambient"&loc!="above"),family=Gamma(link="log"),
                  prior=c(prior(normal(2.3,3.9),class="Intercept"),
                          prior(cauchy(0,2),class="sd")),
                  chains=1,iter=2000)
m44_noabovem
pp_check(m44_noabovem,type="boxplot")

testdata2<-data.frame(day_n=seq(17679,17791,length=112),
                      loc="new")
m44_noabpr<-data.frame(predict(m44_noabovem,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                               allow_new_levels = TRUE,summary=FALSE))
m44_noabpr<-rownames_to_column(m44_noabpr)
m44_noabpr$rowname<-as.numeric(m44_noabpr$rowname)

m44totp_noab<-apply(m44_noabpr,1,sum)
m44totp_noab<-data.frame(m44totp_noab)

predict_gram_noab<-m44totp_noab%>%
  summarise('low95'=quantile(m44totp_noab/1000,probs=0.025),
            `median`=quantile(m44totp_noab/1000, probs=0.5),
            'mean'=mean(m44totp_noab/1000),
            `upper95`=quantile(m44totp_noab/1000, probs=0.975),
            'sd'=sd(m44totp_noab/1000))

predict_gram_noab


#without 'below' sites
m44_nobelowm<-brm(mgm2dayDM01~s(day_n,k=-1)+(1|loc/year),data=subset(all_data,trt=="ambient"&loc!="below"),family=Gamma(link="log"),
                  prior=c(prior(normal(2.3,3.9),class="Intercept"),
                          prior(cauchy(0,2),class="sd")),
                  chains=1,iter=2000)
m44_nobelowm
pp_check(m44_nobelowm,type="boxplot")

m44_nobelpr<-data.frame(predict(m44_nobelowm,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                                allow_new_levels = TRUE,summary=FALSE))
m44_nobelpr<-rownames_to_column(m44_nobelpr)
m44_nobelpr$rowname<-as.numeric(m44_nobelpr$rowname)

m44totp_nobel<-apply(m44_nobelpr,1,sum)
m44totp_nobel<-data.frame(m44totp_nobel)

predict_gram_nobel<-m44totp_nobel%>%
  summarise('low95'=quantile(m44totp_nobel/1000,probs=0.025),
            `median`=quantile(m44totp_nobel/1000, probs=0.5),
            'mean'=mean(m44totp_nobel/1000),
            `upper95`=quantile(m44totp_nobel/1000, probs=0.975),
            'sd'=sd(m44totp_nobel/1000))

predict_gram_nobel


#without 'large' sites
m44_nolargem<-brm(mgm2dayDM01~s(day_n,k=-1)+(1|loc/year),data=subset(all_data,trt=="ambient"&loc!="largepool"),family=Gamma(link="log"),
                  prior=c(prior(normal(2.3,3.9),class="Intercept"),
                          prior(cauchy(0,2),class="sd")),
                  chains=1,iter=2000)
m44_nolargem
pp_check(m44_nolargem,type="boxplot")

m44_nlargpr<-data.frame(predict(m44_nolargem,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                                allow_new_levels = TRUE,summary=FALSE))
m44_nlargpr<-rownames_to_column(m44_nlargpr)
m44_nlargpr$rowname<-as.numeric(m44_nlargpr$rowname)

m44totp_nlarg<-apply(m44_nlargpr,1,sum)
m44totp_nlarg<-data.frame(m44totp_nlarg)

predict_gram_nlarg<-m44totp_nlarg%>%
  summarise('low95'=quantile(m44totp_nlarg/1000,probs=0.025),
            `median`=quantile(m44totp_nlarg/1000, probs=0.5),
            'mean'=mean(m44totp_nlarg/1000),
            `upper95`=quantile(m44totp_nlarg/1000, probs=0.975),
            'sd'=sd(m44totp_nlarg/1000))

predict_gram_nlarg


#without 'small' sites
m44_nosmallm<-brm(mgm2dayDM01~s(day_n,k=-1)+(1|loc/year),data=subset(all_data,trt=="ambient"&loc!="smallpool"),family=Gamma(link="log"),
                  prior=c(prior(normal(2.3,3.9),class="Intercept"),
                          prior(cauchy(0,2),class="sd")),
                  chains=1,iter=2000)
m44_nosmallm
pp_check(m44_nolargem,type="boxplot")


m44_nsmallpr<-data.frame(predict(m44_nosmallm,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                                 allow_new_levels = TRUE,summary=FALSE))
m44_nsmallpr<-rownames_to_column(m44_nsmallpr)
m44_nsmallpr$rowname<-as.numeric(m44_nsmallpr$rowname)

m44totp_nsmall<-apply(m44_nsmallpr,1,sum)
m44totp_nsmall<-data.frame(m44totp_nsmall)

predict_gram_nsmall<-m44totp_nsmall%>%
  summarise('low95'=quantile(m44totp_nsmall/1000,probs=0.025),
            `median`=quantile(m44totp_nsmall/1000, probs=0.5),
            'mean'=mean(m44totp_nsmall/1000),
            `upper95`=quantile(m44totp_nsmall/1000, probs=0.975),
            'sd'=sd(m44totp_nsmall/1000))

predict_gram_nsmall



####Table S2######
library(tidyverse)
cross_data%>%
  group_by(model)%>%
  summarize('median'=median((gm2d-0.053*gm2d)/2/1000),
            'low95'=quantile((gm2d-0.053*gm2d)/2/1000,prob=0.025),
            'high95'=quantile((gm2d-0.053*gm2d)/2/1000,prob=0.975))





####Figure S1 - Priors vs post ####
#m44####s
summary(m44,priors=TRUE)
##Priors##
int<-rnorm(1000,log(10),log(50))
#sd_loc<-rcauchy(1000,0,10)
#sd__locyr<-rcauchy(1000,0,10)
b_s_day_n<-runif(1000,-50,50)
#shape<-rgamma(10000,0.01,scale=0.01)

priorsm44<-data.frame(int,b_s_day_n)


##posteriors###
int<-rnorm(1000,3.2,.48)
#sd_loc<-rcauchy(1000,.59,.54)
#sd__locyr<-rcauchy(1000,.76,.35)
b_s_day_n<-rnorm(1000,1.96,1.33)
#shape<-rgamma(1000,1.18,scale=0.14)

postsm44<-data.frame(int,b_s_day_n)

priorsm44<-gather(priorsm44,parameter,value)
postsm44<-gather(postsm44,parameter,value)
priorsm44$prior_post<-"prior"
postsm44$prior_post<-"post"


#rbind priors and posts##
library(cowplot)
pr_post<-rbind(priorsm44,postsm44)
pr_post$parameter<-as.factor(pr_post$parameter)
figure_S2<-ggplot(pr_post,aes(value,fill=prior_post),alpha=0.4)+
  geom_density(alpha=0.2)+
  scale_fill_manual(values=c('blue','grey'))+
  coord_cartesian(xlim=c(-20,20))+
  facet_wrap(~parameter,scales="free")+
  theme_classic()+
  theme(axis.line.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
figure_S2
ggsave(figure_S2,file="figure_S2.jpg",width=6,height=3,dpi=500,units="in")
####Figure S2####

#m44totp, m44totp_noab, m44totp_nobel, m44totp_nlarg,m44totp_nsmall

all_sites<-gather(m44totp)
no_above<-gather(m44totp_noab)
no_below<-gather(m44totp_nobel)
no_large<-gather(m44totp_nlarg)
no_small<-gather(m44totp_nsmall)

colnames(all_sites)<-c("model","gm2d")
colnames(no_above)<-c("model","gm2d")
colnames(no_below)<-c("model","gm2d")
colnames(no_large)<-c("model","gm2d")
colnames(no_small)<-c("model","gm2d")

all_sites$model<-"all_sites"
no_above$model<-"no_above"
no_below$model<-"no_below"
no_large$model<-"no_large"
no_small$model<-"no_small"

cross_data<-rbind(all_sites,no_above,no_below,no_large,no_small)
cross_data$color<-ifelse(cross_data$model=="all_sites","black","gray")

library(ggridges)

figS2<-ggplot(cross_data,aes(x=(gm2d-0.053*gm2d)/2/1000,y=model,fill=color))+
  geom_density_ridges2()+
  scale_fill_grey(guide=FALSE)+
  scale_x_log10(breaks=c(1,10,100),labels=comma)+
  theme_classic()+
  theme(text=element_text(size=13))+
  xlab(expression(paste("Annual emergence production"," (gC/m"^2,"/y)")))


figS2

ggsave(figS2,file="figure_S2.jpg",dpi=500,width=6,height=3,units="in")



####Figure S3 #####
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
m44totp$study<-"Posterior predictive distribution (present study)"
lit_est$size<-factor(ifelse(grepl("Gratton",lit_est$study),"global lakes (Gratton and Vander Zanden 2009)",
                            "62 estimates from lentic habitats"))

figure_S3<-ggplot()+
  geom_vline(data=lit_est,aes(xintercept=gCm2yr,size=size,color=size),alpha=.5)+
  geom_density(data=m44totp,aes(x=(m44totp-0.053*m44totp)/2/1000,fill=study))+
  theme_classic()+
  scale_color_manual(values=c("#696969","#2e75b6"))+
  scale_size_manual(values=c(0.2,3))+
  theme(text=element_text(size=12),
        panel.grid=element_blank(),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=11),
        legend.title=element_blank())+
  scale_fill_manual(values='grey50')+
  scale_y_continuous(labels=comma)+
  xlab(expression(paste("Annual emergence production"," (gC/m"^2,"/y)")))+
  ylab("posterior density")+
  scale_x_log10(labels=comma)+
  annotation_logticks(sides="b",mid=unit(0.1,"cm"))
#  coord_cartesian(xlim=c(10,100000))
figure_S3
ggsave(figure_S3,file="figure_S3.jpg",dpi=500,width=8,height=3.5,units="in")  

####Figure S4####
library(ggplot2)
library(brms)
library(tidyverse)
library(ggridges)
library(scales)
library(stringr)

#Model centered on zero
get_prior(mgm2dayDM01~s(day_n)+(1|loc/year),data=subset(all_data,trt=="ambient"),family=Gamma(link="log"))

m44alt_unif<-brm(mgm2dayDM01~s(day_n)+(1|loc/year),data=subset(all_data,trt=="ambient"),family=Gamma(link="log"),
                 prior=c(prior(normal(0,1000),class="Intercept"), #sd is wider than original model (log(100) vs log(50))
                         prior(cauchy(0,2),class="sd")),
                 chains=4,iter=2000)

pp_check(m44alt_unif,type="boxplot")


###Fitted estimates m44alt_unif###
marg44alt_unif<-marginal_effects(m44alt_unif,method="fitted",robust=TRUE)
marg44alt_unif<-data.frame(marg44alt_unif$day_n)


###Predictions for new sites m44alt_unif###
testdata2<-data.frame(day_n=seq(17679,17791,length=112),
                      loc="new")
m44alt_unifpr2<-data.frame(predict(m44alt_unif,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                                   allow_new_levels = TRUE,robust=TRUE))
m44alt_unifpr2<-rownames_to_column(m44alt_unifpr2)
m44alt_unifpr2$rowname<-as.numeric(m44alt_unifpr2$rowname)

##predictions
testdata2alt_unif<-data.frame(day_n=seq(17679,17791,length=112),
                              loc="new")
m44alt_unifpr2s<-data.frame(predict(m44alt_unif,type="response",newdata=testdata2alt_unif,re_formula=~(1|loc/year),
                                    allow_new_levels = TRUE,summary=FALSE))
m44alt_unifpr2s<-rownames_to_column(m44alt_unifpr2s)
m44alt_unifpr2s$rowname<-as.numeric(m44alt_unifpr2s$rowname)

m44alt_uniftotp<-apply(m44alt_unifpr2s,1,sum)
m44alt_uniftotp<-data.frame(m44alt_uniftotp)


##### Model with wider sd priors

m44alt_sd<-brm(mgm2dayDM01~s(day_n)+(1|loc/year),data=subset(all_data,trt=="ambient"),family=Gamma(link="log"),
               prior=c(prior(normal(2.3,4.6),class="Intercept"), #sd is wider than original model (log(100) vs log(50))
                       prior(cauchy(0,2),class="sd")),
               chains=4,iter=2000)

pp_check(m44alt_sd,type="boxplot")

###Fitted estimates m44alt_sd###
marg44alt_sd<-marginal_effects(m44alt_sd,method="fitted",robust=TRUE)
marg44alt_sd<-data.frame(marg44alt_sd$day_n)


###Predictions for new sites m44alt_sd###
testdata2<-data.frame(day_n=seq(17679,17791,length=112),
                      loc="new")
m44alt_sdpr2<-data.frame(predict(m44alt_sd,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                                 allow_new_levels = TRUE,robust=TRUE))
m44alt_sdpr2<-rownames_to_column(m44alt_sdpr2)
m44alt_sdpr2$rowname<-as.numeric(m44alt_sdpr2$rowname)

##predict new sites with wider sd priors
testdata2alt_sd<-data.frame(day_n=seq(17679,17791,length=112),
                            loc="new")
m44alt_sdpr2s<-data.frame(predict(m44alt_sd,type="response",newdata=testdata2alt_sd,re_formula=~(1|loc/year),
                                  allow_new_levels = TRUE,summary=FALSE))
m44alt_sdpr2s<-rownames_to_column(m44alt_sdpr2s)
m44alt_sdpr2s$rowname<-as.numeric(m44alt_sdpr2s$rowname)

m44alt_sdtotp<-apply(m44alt_sdpr2s,1,sum)
m44alt_sdtotp<-data.frame(m44alt_sdtotp)
#####plot comparisons

##Combine data
m44totp$prior<-"original model"
m44alt_sdtotp$prior<-"wider sd"
m44alt_uniftotp$prior<-"centered on zero"

m44totp_pr_post<-m44totp[,c(1,3)]

colnames(m44totp_pr_post)<-c("mgDMm2y","prior")
colnames(m44alt_sdtotp)<-c("mgDMm2y","prior")
colnames(m44alt_uniftotp)<-c("mgDMm2y","prior")
compare_data<-rbind(m44totp_pr_post,m44alt_sdtotp,m44alt_uniftotp)
compare_data$prior<-factor(compare_data$prior)


#Plot Figure S4
figure_s4<-ggplot()+
  geom_density(data=compare_data,aes(x=(mgDMm2y-0.053*mgDMm2y)/2/1000,color=prior))+
  theme_classic()+
  scale_size_manual(values=c(0.2,3))+
  theme(text=element_text(size=12),
        panel.grid=element_blank(),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=11))+
  scale_color_grey()+
  scale_y_continuous(labels=comma)+
  xlab(expression(paste("Annual emergence production"," (gC/m"^2,"/y)")))+
  ylab("posterior density")+
  scale_x_log10(labels=comma)+
  annotation_logticks(sides="b",mid=unit(0.1,"cm"))
#  coord_cartesian(xlim=c(10,100000))
figure_s4
ggsave(figure_s4,file="figure_s4.jpg",dpi=500,width=8,height=3.5,units="in") 
