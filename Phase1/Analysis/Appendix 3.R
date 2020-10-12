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
make_matrix <- function(df1, title, txt_size=10, mid =1.25, max = 2.5, legendpos="top") {
  df1 %>%group_by(age_cat,contact_age) %>% 
    summarize(tot_contacts=n()) %>% 
    full_join(standard_str1, by= c("age_cat","contact_age"),keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(part_age1,by="age_cat") %>%
    mutate(avg_cont = (tot_contacts/n))%>%
    contactmatrix_viz(title=title, txt_size= txt_size, mid=mid, max=max, legendpos=legendpos)
}
contact <- contact %>% left_join(part, by="part_id") %>%     #Join contact with participant info                                                                     #Make contact and part age symmetric
  mutate(age_cat = as.character(age_cat),
         age_cat = replace(age_cat, age_cat == "40-49", "40-59"),
         age_cat = replace(age_cat, age_cat == "50-59", "40-59"))
standard_str1<-data.frame(age_cat = rep(c("0-9","10-19","20-29","30-39","40-59","60+"),6),
                          contact_age = rep(unique(contact$contact_age),each = 6))
part_age1 <- part %>% 
  mutate(age_cat = as.character(age_cat),
         age_cat = replace(age_cat, age_cat == "40-49", "40-59"),
         age_cat = replace(age_cat, age_cat == "50-59", "40-59")) %>%     
  group_by(age_cat)%>% dplyr::summarize(n=n())   
make_matrix(contact %>% filter(cont_attr == "conv_only"|cont_attr == "conv_phys"), "Conversational", txt_size =8)
make_matrix(contact %>% filter(cont_attr == "phys_only"|cont_attr == "conv_phys"), "Physical", txt_size =8)
make_matrix(contact %>% filter(cont_home =="1"), "At home", txt_size =8)
make_matrix(contact %>% filter(street =="1" | store=="1"), "Street or store", txt_size =8)
make_matrix(contact %>% filter(work =="1"), "At work", txt_size =8)

two_day_dup <-contact %>% filter(diaryday == "Second day" & contact_fromdayone == "Yes") %>%
  group_by(age_cat,contact_age) %>% 
  summarize(tot_contacts=n()) %>% 
  full_join(standard_str1, by= c("age_cat","contact_age"),keep=F) %>%
  replace(is.na(.), 0) %>%
  left_join(part_age1,by="age_cat") %>%
  mutate(avg_cont = (tot_contacts/n),
         avg_cont = avg_cont*2) 


two_day_unique <- contact %>%
  group_by(age_cat,contact_age) %>% 
  summarize(tot_contacts=n()) %>% 
  full_join(standard_str1, by= c("age_cat","contact_age"),keep=F) %>%
  replace(is.na(.), 0) %>%
  left_join(part_age1,by="age_cat") %>%
  mutate(avg_cont = (tot_contacts/n)) %>% 
  left_join (two_day_dup, by = c("age_cat","contact_age")) %>%
  mutate(avg_cont = avg_cont.x-avg_cont.y,
         avg_cont= replace(avg_cont, avg_cont<0,0)) %>%
  select(age_cat, contact_age, avg_cont)

contactmatrix_viz(two_day_dup, title = "Repeated over two days",txt_size = 8, max = 2.5, mid=1.25, legendpos = "top")
contactmatrix_viz(two_day_unique, title = "Unique over two days",txt_size = 8, max = 2.5, mid=1.25, legendpos = "top")

P <- contact %>%
  filter(contact_age != "0-9" & contact_age != "10-19") %>%
  group_by(age_cat,contact_age) %>%
  summarize(num_cell_contact=n()) %>% left_join(
    contact %>% 
      filter(contact_age != "0-9" & contact_age != "10-19") %>%
      group_by(age_cat) %>%
      summarize(num_age_contact = n()),
    by="age_cat"
  ) %>% mutate(
    frac = num_cell_contact/num_age_contact
  ) %>%
  select(age_cat, contact_age, frac) %>% 
  spread(contact_age, frac) 

P<- as.matrix(as.data.frame(P)[,2:5])
rownames(P)<-colnames(P)


# Calculate Q index for assortativeness
Q <- (sum(diag(P)) - 1) / (dim(P)[1] - 1)
