list.of.packages <- c(
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
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages,library,character.only=T))
rm(list.of.packages,new.packages) #Removes lists for cleanliness

## Function used to save legend of ggplot2 (allows manipulating legend)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## Function used to visualize age-specific contact mixing matrix with controls over title, text size, mid and max points for legend and legend position

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


## Function used to wrangle long form contact data into age-specific mixing matrix grid
## Last pipe does contact matrix visualization
make_matrix <- function(df1, title, txt_size=10, mid =1.25, max = 2.5, legendpos="top") {
  df1 %>%group_by(age_cat,contact_age) %>% 
    summarize(tot_contacts=n()) %>% 
    full_join(standard_str1, by= c("age_cat","contact_age"),keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(part_age1,by="age_cat") %>%
    mutate(avg_cont = (tot_contacts/n))%>%
    contactmatrix_viz(title=title, txt_size= txt_size, mid=mid, max=max, legendpos=legendpos)
}

## Create dummy data frame of standard structure for contact matrix
standard_str1<-data.frame(age_cat = rep(c("0-9","10-19","20-29","30-39","40-59","60+"),6),
                          contact_age = rep(unique(contact$contact_age),each = 6))

## Number participants by age group
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

lay<-rbind(c(1,2),
           c(3,4,5),
           c(6,7))
grid.arrange(p3,p4,p5,p6, 
             layout_matrix=lay)

