## Datasets ---------------------------------
contact<-readRDS('./Phase1/Datasets/contact.rds')
part<-readRDS('./Phase1/Datasets/participant.rds')

# Packages ---------------------------------
package_list <- c(
  "reshape2",
  "ggplot2",
  "ggpubr",
  "dplyr",
  "tidyverse",
  "kableExtra",
  "gridExtra",
  "RColorBrewer",
  "cowplot",
  "cellranger"
)

if(F){
  install.packages(package_list)
}

invisible(lapply(package_list, function(x) library(x, character.only = T)))

rm(package_list)

## Functions ------------------------------------------
# Function used to save legend of ggplot2 (allows manipulating legend)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Function used to visualize age-specific contact mixing matrix with controls over title, text size, mid and max points for legend and legend position

contactmatrix_viz<-function(matrix1,title,txt_size, mid, max, legendpos){
  ggplot(data = matrix1, aes(x=factor(age_cat), y=factor(contact_age), fill=avg_cont)) +    ##var1 is age of person, var2 is age of contact
    geom_raster(hjust = 0.5, vjust = 0.5,show.legend=T)+
    scale_fill_gradient2(low = "white", high = "#273871", mid = "#7FABD3", midpoint = mid, limit = c(0,max))+
    xlab("Age of participant")+ylab("Age of contact")+labs(fill = "Average \ncontact")+
    theme_classic()+
    theme(plot.title = element_text(size = 12), legend.title=element_text(size = 10),
          axis.text.x = element_text(size = 10), # changed from txt_size (MK)
          axis.text.y = element_text(size= 10),
          legend.justification = "right",
          legend.position = legendpos) +
    ggtitle(title)
}


## Variable Creation -----------------------
contact$loc <- NA
contact <- contact %>% left_join(part, by="part_id") %>%     #Join contact with participant info                                                                     #Make contact and part age symmetric
  mutate(age_cat = as.character(age_cat),
         age_cat = replace(age_cat, age_cat == "40-49", "40-59"),
         age_cat = replace(age_cat, age_cat == "50-59", "40-59")) %>%
  mutate(loc = ifelse(cont_home == "1", "home",
               ifelse(cont_otherhome == "1", "other_home",
                      ifelse(street == "1"|store == "1", "street_store",
                             ifelse(work=="1", "work", "other")))))%>% 
  mutate(totaltime = as.character(totaltime),
  totaltime = replace(totaltime, totaltime=="Between 1 hour to 4 hours", "1-4 hrs"),
  totaltime = replace(totaltime, totaltime == "Between 15 minutes to 1 hour", "15mins-1 hr"),
  totaltime = replace(totaltime, totaltime == "Between 5 to 15 minutes", "5-15 mins"),
  totaltime = replace(totaltime, totaltime == "Less than 5 minutes", "<5 mins"),
  totaltime = replace(totaltime, totaltime == "More than 4 hours", "4+ hrs"),
  totaltime = as.factor(totaltime),
  totaltime = ordered(totaltime, levels= c("<5 mins", "5-15 mins", "15mins-1 hr","1-4 hrs","4+ hrs")),
  loc = as.factor(loc),
  loc = ordered(loc, levels = c("other_home","work","other","street_store","home"))
)

## Plot 1 y-axis = location x-axis= age ----
df3 <- contact %>% group_by(age_cat, loc) %>% summarize(n=n()) %>% mutate(prop=round((n/sum(n))*100,digits=2))

p3<-ggplot(df3, aes(x=age_cat,y=prop, fill=loc)) +
  geom_col(aes(fill = loc)) +
  ylab("% of contacts") + xlab("Participant age group") +
  ggtitle("A. Distribution of contacts by location")+
  theme_classic() +
  theme(axis.text.x = element_text(angle=0),
        legend.title= element_blank()) +
  scale_fill_brewer("Blues", 
                    labels = c("Other home","Work","Other", "Street/Store", "Own home"))


## Plot2: y-axis = duration x-axis = age ----
df4 <- contact %>% group_by(age_cat, totaltime) %>% summarize(n=n()) %>% mutate(prop=round((n/sum(n))*100,digits=2))

p4<-ggplot(df4, aes(x=age_cat,y=prop, fill=totaltime)) +
  geom_col(aes(fill = totaltime)) +
  ylab("% of contacts") + xlab("Participant age group") +
  ggtitle("B. Distribution of contacts by duration")+
  scale_fill_brewer("Blues") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=0),
        legend.title= element_blank())


## Plot 3: y-axis = physical vs conversational,  x-axis = age ----
df5 <- contact %>% group_by(age_cat, cont_attr) %>% summarize(n=n()) %>% mutate(prop=round((n/sum(n))*100,digits=2))

p5<-ggplot(df5, aes(x=age_cat,y=prop, fill=cont_attr)) +
  geom_col(aes(fill = cont_attr)) +
  ylab("% of contacts") + xlab("Participant age group") +
  ggtitle("C. Distribution of contacts by attribute")+
  scale_fill_brewer("Blues",
                    labels = c("Conv only","Conv & Phys","Phys only")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=0),
        legend.title= element_blank())


## Plots 4 y-axis = repeated, x-axis = age ----

df6 <- contact%>% filter(diaryday == "Second day" & !is.na(contact_fromdayone)) %>% group_by(age_cat,contact_fromdayone) %>% 
  summarize(n=n()) %>% mutate(prop=round((n/sum(n))*100,digits=2)) 
p6 <-ggplot(df6, aes(x=age_cat,y=prop, fill=contact_fromdayone)) +
  geom_col(aes(fill = contact_fromdayone)) +
  ylab("% of contacts")+ xlab("Participant age group") +
  ggtitle("D. Repeated vs. unique contacts")+
  scale_fill_brewer("Blues")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=0),
        legend.title= element_blank()) 


## 2x2 grid layout -----
lay<-rbind(c(1,2),
           c(3,4))
grid.arrange(p3,p4,p5,p6, 
             layout_matrix=lay)
