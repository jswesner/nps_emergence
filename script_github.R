###Install and open the packages below
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
dm_p<-data.frame(exp(postmdm$b_Intercept))
mean(dm_p$postdm) #use this as mean for emergence dataset (i.e. ind mass = rnorm(mean(dm_p$postdm),sd(dm_p$post_dm)))
sd(dm_p$postdm) #use this as sd for emergence dataset (i.e. ind mass = rnorm(mean(dm_p$postdm),sd(dm_p$post_dm)))


#####Estimate dry mass of samples using model outcome from dry mass regression (model mdm)####
emerge_data$est_indmg<-rnorm(nrow(emerge_data),mean(dm_p$postdm),sd(dm_p$postdm))
emerge_data$mgm2dayDM<-emerge_data$indm2day*emerge_data$est_indmg

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




###Fitted estimates m44######
marg44<-marginal_effects(m44,method="fitted",robust=TRUE)
marg44<-data.frame(marg44$day_n)


###Predictions for new sites m44######
testdata2<-data.frame(day_n=seq(17679,17791,length=112),
                      loc="new")
m44pr2<-data.frame(predict(m44,type="response",newdata=testdata2,re_formula=~(1|loc/year),
                           allow_new_levels = TRUE,robust=TRUE))
m44pr2<-rownames_to_column(m44pr2)
m44pr2$rowname<-as.numeric(m44pr2$rowname)





#######Figure 2 - emergence at collection sites#####
fig2<-ggplot()+
  geom_ribbon(data=m44pr2,aes(x=rowname,y=(Estimate-0.053*Estimate)/2,
                              ymin=(X2.5.ile-0.053*X2.5.ile)/2,
                              ymax=(X97.5.ile-0.053*X97.5.ile)/2,fill="predicted"))+
  #geom_ribbon(data=m44ft2,aes(x=rowname,y=Estimate,ymin=X2.5.ile,ymax=X97.5.ile,fill="fitted"))+
  geom_ribbon(data=marg44,aes(x=day_n-17678,y=(estimate__-0.053*estimate__)/2,
                              ymin=(lower__-0.053*lower__)/2,ymax=(upper__-0.053*upper__)/2,fill="fitted"))+
  
  geom_point(data=subset(all_data,trt=="ambient"),aes(x=day_n-17678,y=(mgm2dayDM01-0.053*mgm2dayDM01)/2,shape=year),
             size=1.8,alpha=.5)+
  scale_y_log10(breaks=c(1,10,100,1000),labels=comma)+
  coord_cartesian(ylim=c(0.042,1000))+
  scale_fill_manual(values=c("fitted"=alpha('grey12',.35),
                             "predicted"=alpha('grey12',.2),
                             "ft2"=alpha('green',.28)))+
  scale_shape_manual(values=c(15,16,17,18))+
  theme_bw()+
  geom_line(data=marg44,aes(x=day_n-17678,y=(estimate__-0.053*estimate__)/2),size=1,color="black")+
  scale_x_continuous(breaks=c(5,19,35,49,66,81,98,111),labels=c("Jun 1",
                                                                "Jun 15",
                                                                "Jul 1",
                                                                "Jul 15",
                                                                "Aug 1",
                                                                "Aug 15",
                                                                "Sep 1",
                                                                "Sep 15"))+
  xlab("")+
  ylab(expression(atop("Insect emergence", paste("(mgC/m"^2,"/day)"))))+
  theme(text=element_text(size=12),
        panel.grid=element_blank(),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=9),
        legend.title=element_blank())

fig2

#geom_point(data=subset(all_data,trt=="fishless"|trt=="Exclusion"),aes(x=day_n-17678,y=indm2day,color=year),
#size=3,alpha=.5,shape=16)

ggsave(fig2,file="figure_2.jpg",dpi=500,width=7,height=3.5,units="in")
#######Figure 3 - emergence by segment ##########
fig3<-ggplot(data=m44totp3g2,aes(x=reorder(reach,order),y=gCyrkm/1000,fill=year,label=label))+
  geom_boxplot(outlier.shape=NA,width=.6)+
  #geom_violin(width=0.2,alpha=0.2)+
  scale_fill_manual(values=c("grey30","grey50","grey70","grey90"))+
  coord_cartesian(ylim=c(1,1000))+
  #scale_y_continuous(breaks=c(1,100000))+
  #geom_text()+
  xlab("Segment")+
  ylab("Predicted annual emergence from backwaters (kgC/km/yr)")+
  annotate("text",x=c(1.26,2.26,5.92,6.92),y=10,label="NA",size=2)+
  #scale_y_log10(breaks=c(1,10,100,1000,10000,100000),labels=comma)+
  theme_classic()+
  theme(text=element_text(size=13))
fig3
ggsave(file="figure_3.jpg",fig3,dpi=500,width=7,height=5.5,units="in")


