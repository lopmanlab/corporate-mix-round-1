## Original code by Carol Liu
## Edited by MCK on 05/05/2021

## Generating publicly facing dataset

rm(list=ls())

#install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(readxl)
library(mgsub)
library(reshape)
library(reshape2)
library(table1)
library(ggplot2)
library(kableExtra)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(cowplot)

## Save legend of ggplot2
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## Read in RDS files
enroll_cont<-read_rds("enroll_cont.rds")
contact_w<-read_rds("contact_wide.rds")
contact_long<-read_rds("contact_long.rds")

## Remove non-social, direct-proximity contacts
contact_long <- contact_long %>% filter(contact_noconvnophys != "1")


## One person doesnt have enrollment data... temporarily remove from contact
contact_w<-contact_w %>% filter(email!="ashley.floyd@emory.edu")
contact_long <- contact_long %>% filter(email!="ashley.floyd@emory.edu")


## Recoding
## Other recategoritzations


enroll_cont$num_hh2 <- enroll_cont$num_hh
enroll_cont$state_res2 <- enroll_cont$state_res
enroll_cont$hh_str <- 0



enroll_cont <- enroll_cont %>% mutate(
  num_hh2 = replace(num_hh2, is.na(num_hh2),"Live alone"),
  num_hh2 = replace(num_hh2, num_hh2=="1"|num_hh2=="2"|num_hh=="3","1-4 person"),
  num_hh2 = replace(num_hh2, (num_hh2=="4"|num_hh2=="5"|num_hh2=="6"|num_hh2=="More than 6"),"More than 4"),
  state_res2 = replace(state_res2, state_res2!="Georgia"& state_res2!="Illinois"&state_res2!="Virginia", "Other"),
  hh_str = ifelse(hh_parent=="1","Live with parent",
                  ifelse(hh_alone=="1","Live alone",
                         ifelse(hh_spouse=="1"& hh_roommate !="1" & hh_children !="1" & hh_sib !="1"&hh_other!="1","Spouse only",
                                ifelse(hh_spouse=="1"& hh_children=="1"&hh_roommate!="1"&hh_sib!="1"&hh_other!="1","Spouse and children only",
                                       ifelse(hh_roommate=="1" | hh_sib =="1"& hh_other!="1" & hh_spouse!="1"& hh_children!="1","Roommate or sibling","Other"))))),
  age_cat = replace(age_cat, age_cat=="70+"|age_cat=="60-69","60+"),
  race = factor(race, levels=c("Black","White","Asian","Mixed","Other")))
#                          gender = replace(gender,gender=="Prefer not to answer",NA))  # Replace prefer not to answer with NA

enroll_cont$edu1 <- enroll_cont$edu
enroll_cont <-enroll_cont %>% mutate(
  edu = na_if(edu,"8"),
  edu1=replace(edu1, (edu=="Bachelor's degree in college (4-year)" | 
                        edu=="Doctoral degree or Professional degree (PhD, JD, MD)" | 
                        edu=="Master's degree"), "Bachelors or higher"),
  edu1=replace(edu1, (edu1=="Associate degree in college (2-year)" | 
                        edu1=="High school graduate (high school diploma or equivalent including GED)" | 
                        edu1=="Some college but no degree"),"Less than Bachelors"),
  edu1= na_if(edu1,"8"))
  

contact_w$num_hh2<-contact_w$num_hh
contact_w$state_res2 <-contact_w$state_res
contact_w$hh_str<-0
contact_w <- contact_w %>% mutate(
  num_hh2 = replace(num_hh2, is.na(num_hh),"Live alone"),
  num_hh2 = replace(num_hh2, num_hh=="1"|num_hh=="2"|num_hh=="3","2-4 person"),
  num_hh2 = replace(num_hh2, (num_hh=="4"|num_hh=="5"|num_hh=="6"|num_hh=="More than 6"),"More than 4"),
  state_res2 = replace(state_res2, state_res2!="Georgia"& state_res2!="Illinois"&state_res2!="Virginia", "Other"),
  hh_str = ifelse(hh_parent=="1","Live with parent",
                  ifelse(hh_alone=="1","Live alone",
                         ifelse(hh_spouse=="1"& hh_roommate !="1" & hh_children !="1" & hh_sib !="1"&hh_other!="1","Spouse only",
                                ifelse(hh_spouse=="1"& hh_children=="1"&hh_roommate!="1"&hh_sib!="1"&hh_other!="1","Spouse and children only",
                                       ifelse(hh_roommate=="1" | hh_sib=="1"& hh_other!="1" & hh_spouse!="1"& hh_children!="1","Roommate or sibling","Other"))))),
  age_cat = replace(age_cat, age_cat=="70+"|age_cat=="60-69","60+"),
  race = factor(race, levels=c("Black","White","Asian","Mixed","Other")))
#                          gender = replace(gender,gender=="Prefer not to answer",NA))  # Replace prefer not to answer with NA

contact_long$cont_streetstore <-0
contact_long$num_hh2 <- contact_long$num_hh 
contact_long$hh_str <- NA
contact_long$cont_attr <- NA
contact_long$state_res2 <- contact_long$state_res

contact_long <- contact_long %>% mutate(
  contact_age = replace(contact_age, contact_age =="Less than 1 year old","0-9"),
  # make <1, then add (age in years) ylab
  contact_age = replace(contact_age,contact_age =="1 to 9 years old", "0-9"),
  contact_age = replace(contact_age, contact_age =="10 to 19 years old","10-19"),
  contact_age = replace(contact_age, contact_age =="20 to 29 years old","20-29"),
  contact_age = replace(contact_age, contact_age == "30 to 39 years old", "30-39"),
  contact_age = replace(contact_age, contact_age == "40 to 59 years old", "40-59"),
  contact_age = replace(contact_age, contact_age == "60 to 69 years old", "60+"),
  contact_age = replace(contact_age, contact_age == "70 to 79 years old", "60+"),
  contact_age = replace(contact_age,contact_age =="80 years old and older"|contact_age =="80 years and older","60+" ),
  contact_age = factor(contact_age,
                       levels=c("0-9","10-19","20-29","30-39","40-59","60+")),
  age_cat = replace(age_cat, age_cat=="70+"|age_cat=="60-69","60+"),
  
  cont_streetstore = replace(cont_streetstore, store=="1"|street =="1","1"),
  
  num_hh2 = replace(num_hh2, is.na(num_hh),"Live alone"),
  num_hh2 = replace(num_hh2, num_hh=="1"|num_hh=="2"|num_hh=="3","2-4 person"),
  num_hh2 = replace(num_hh2, (num_hh=="4"|num_hh=="5"|num_hh=="6"|num_hh=="More than 6"),"More than 4"),
  
  state_res2 = replace(state_res2, state_res2!="Georgia"& state_res2!="Illinois"&state_res2!="Virginia", "Other"),
  
  hh_str = ifelse(hh_parent=="1","Live with parent",
                  ifelse(hh_alone=="1","Live alone",
                         ifelse(hh_spouse=="1"& hh_roommate !="1" & hh_children !="1" & hh_sib !="1"&hh_other!="1","Spouse only",
                                ifelse(hh_spouse=="1"& hh_children=="1"&hh_roommate!="1"&hh_sib!="1"&hh_other!="1","Spouse and children only",
                                       ifelse(hh_roommate=="1" | hh_sib=="1"& hh_other!="1" & hh_spouse!="1"& hh_children!="1","Roommate or sibling","Other"))))),
  
  race = factor(race, levels=c("Black","White","Asian","Mixed","Other")),
  
  cont_attr = ifelse(contact_conv == "1" & contact_phys == "1", "conv_phys",
                     ifelse(contact_conv == "1", "conv_only",
                            ifelse(contact_phys == "1", "phys_only", NA))))


## Subset to those who filled in both days
## For main figures subset to the 304 who responded to both diary days
df_repeat<- contact_w %>% 
  select(email, diaryday, StartDate.y) %>% 
  arrange( email, StartDate.y) %>% 
  mutate(StartDate.y=as.Date(StartDate.y))

df_repeat <- reshape(df_repeat,dir="wide",idvar= "email", timevar = "diaryday")
colnames(df_repeat)[2:3] <- c("Day1Date","Day2Date")
df_repeat <- df_repeat %>% 
  left_join(enroll_cont %>% 
              select(email, StartDate),by="email") %>%
  dplyr::rename(EnrollDate = StartDate) %>%
  mutate(EnrollDate = as.Date(EnrollDate),
         diff_enroll_d1 = Day1Date-EnrollDate,
         diff_d1_d2 = Day2Date-Day1Date)

bothdays <- df_repeat %>% filter(!is.na(Day2Date))

both_contact_long <- contact_long[contact_long$email %in% bothdays$email,]
both_contact_w <- contact_w[contact_w$email %in% bothdays$email,]
both_enroll_cont <- enroll_cont[enroll_cont$email %in% bothdays$email, ]

### Generate unique identifiers based on the email
set.seed(87334)
identifier <- data.frame(email = unique(both_enroll_cont$email),
                         part_id = sample(1:304,304))
saveRDS(identifier,"identifier.RDS")

### join with RDS file and subset to columns that were used
both_enroll_cont_subset <-
  both_enroll_cont %>% 
  left_join(identifier, by="email") %>% 
  select(part_id,age,age_cat,gender,race,hispanic,edu,hh_str,state_res2, comp) %>%
  arrange(part_id)

saveRDS(both_enroll_cont_subset,"./data/clean/participant.RDS")


both_contact_long_subset <-both_contact_long %>%
  left_join(identifier, by ="email") %>%
  arrange(part_id) %>%
  mutate(cont_id= seq(1:1548)) %>%
  select(part_id,cont_id,diaryday,contact_age,contact_gender,cont_attr, totaltime,
         cont_home,street,store,cont_otherhome,work,school,worship, leisure,transport,healthcare,gym,playground,loc_other,contact_fromdayone)
saveRDS(both_contact_long_subset,"3.Clean/publish/contact.RDS")

## Number of contacts per day
tele_recode <- data.frame(office_tele = unique(both_contact_w$office_tele),
                          work_loc = c("telecommute","telecommute","office","both",NA,"office"))

both_contact_w_subset<- both_contact_w %>% 
                    left_join(
                            both_contact_long %>% group_by(email, diaryday) %>%
                            summarize(social_cont = n()),
                            by = c("email"="email","diaryday"="diaryday")) %>%
                            mutate(social_cont = replace(social_cont,is.na(social_cont),0)) %>%
                    left_join(identifier, by="email") %>%
                    left_join(tele_recode, by="office_tele")%>%
                    arrange(part_id) %>%
                    select(part_id,diaryday,work_loc,largegroup,social_cont)

saveRDS(both_contact_w_subset,"3.Clean/publish/day.RDS")


