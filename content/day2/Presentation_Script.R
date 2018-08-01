if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse,data.table,survey,srvyr)


#String Variables are brought in as factors
options(stringsAsFactors = T)
#Variances are centered around the estimate rather than the mean of the replicate weights
options(survey.replicates.mse = T)
#Remove Scientific Notation
options(scipen = 999)
#fread creates data.frames instead of data.tables
options(datatable.fread.datatable=F)
#Clear environment
rm(list=ls())


#Set Working Directory
setwd("~/Workshop_PUMS")

#Read Oregon 2016 1 yr housing unit records
or_h <- fread("ss16hor.csv",colClasses = c("SERIALNO"="character","PUMA"="character","ST"="character"))

#Conver column names to lower case
colnames(or_h)<-tolower(colnames(or_h))

#Read person level records and convert column names to lower case
or_p<-fread("ss16por.csv",colClasses = c("SERIALNO"="character","PUMA"="character", "MIGSP"="character", "ST"="character"))
colnames(or_p)<-tolower(colnames(or_p))


#Combine person and housing unit tables
or_pums<-or_p %>% left_join(or_h, by="serialno", suffix=c("_p","_h"))


#Add Race & Ethnicity and Cost Burdened Fields
or_pums<-or_pums%>%mutate(hisp_boolean= if_else(hisp==1,0,1),
                          race_hisp=factor(case_when(rac1p==1& hisp_boolean==0~"wa",
                                                     rac1p==2 & hisp_boolean==0~"black",
                                                     c(rac1p==3|rac1p==4|rac1p==5)& hisp_boolean==0~"ai_an",
                                                     rac1p==6& hisp_boolean==0~"asian",
                                                     rac1p==7& hisp_boolean==0~"nh_pi",
                                                     rac1p==8& hisp_boolean==0~"other",
                                                     rac1p==9& hisp_boolean==0~"multi",
                                                     hisp_boolean==1~"hisp"),
                                           levels=c("wa","black","ai_an","asian","nh_pi","other","multi","hisp")), burden=factor(case_when(ocpip>=30|grpip>=30~ "burdened", TRUE ~ "not burdened")))

p_design<-svrepdesign(weights= ~pwgtp,
                      repweights = 'pwgtp[0-9]+',
                      scale=4/80,
                      rscales = ncol('pwgtp[0-9]+'),
                      mse=T,
                      combined.weights = T,
                      type='JK1',
                      data= or_pums)


#join race & ethnicity and cost burdened fields to housing unit table
h_join<-left_join(or_h, or_pums %>% filter(sporder==1) %>% select(serialno,race_hisp,burden),by="serialno")

#housing unit-level survey design
h_design<-svrepdesign(weights= ~wgtp,
                      repweights = 'wgtp[0-9]+',
                      scale=4/80,
                      rscales = ncol('wgtp[0-9]+'),
                      mse=T,
                      combined.weights = T,
                      type='JK1',
                      data= h_join)


#Cost Burdened Households by Race & Ethnicity
kable(svyby(~burden,~race_hisp,h_design,svytotal,na.rm=T) %>% as.data.frame(.) %>% 
        rename(burdened=burdenburdened,notburdened=`burdennot burdened`,burdened_se=se1,notburdened_se=se2) %>%
        select(race_hisp,burdened,burdened_se,notburdened, notburdened_se))


# Household Income regressed on Race & Ethnicity of Householder
model<-svyglm(hincp ~ race_hisp ,h_design)
stargazer(model, type="text")
1-model$deviance/model$null.deviance

kable(svytotal(~as.factor(sex),p_design,na.rm=T))

# design
h_design2<- h_join %>% as_survey_rep(weights=wgtp,
                                     repweights=starts_with("wgtp"),
                                     combined_weights=T,
                                     type="JK1",
                                     scale=4/80,
                                     rscales=ncol('wgtp[0-9]+'))

# Calculate Burdened and Not Burdened Households by Race & Ethnicity(srvyr Package)
kable(h_design2 %>% group_by(burden,race_hisp) %>% summarise(hh=survey_total(,na.rm=T)) %>% arrange(race_hisp,burden))


kable(h_design2 %>% group_by(burden,race_hisp) %>% summarise(hh=survey_total(,na.rm=T),MHI=survey_median(hincp,na.rm=T,vartype="se")) %>% 
        #Create Total Households by Race & Ethnicity
        group_by(race_hisp)%>% mutate(total=sum(hh),total_se=moe_sum(hh_se,hh)) %>%
        #Ungroup and calculate shares and sampling error
        ungroup() %>% mutate(MHI_cv= 100*MHI_q50_se/MHI_q50,
                             hh_share= 100*hh/total,
                             hh_share_se= 100*moe_prop(hh,total,hh_se,total_se)) %>%
        #remove hh fields and arrange by Race & Ethnicity and Burden
        select(-total,-total_se,-hh,-hh_se) %>% arrange(race_hisp,burden)
)

