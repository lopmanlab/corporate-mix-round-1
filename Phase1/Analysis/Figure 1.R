#Figure 1
contact$loc <- NA

contact <- contact %>% mutate(
  loc = ifelse(cont_home == "1", "home",
               ifelse(cont_otherhome == "1", "other_home",
                      ifelse(street == "1"|store == "1", "street_store",
                             ifelse(work=="1", "work", "other")))))


unique<- nrow(contact) - (nrow(subset(contact, diaryday=="Second day" & contact_fromdayone == "Yes")))*2  # calculate unique by subtracting double of duplicates from day two from total number of contacts
repeated <- nrow(subset(contact, diaryday=="Second day" & contact_fromdayone == "Yes"))*2   #self reported repeats from day 2 multipled by 2


df <- rbind(data.frame(value = c("unique","repeated"), n = c(unique,repeated)) %>% mutate(prop=round((n/sum(n))*100,digits=2), var=rep("Repeated")),
            contact %>% group_by(loc) %>% summarize(n=n()) %>% mutate(prop=round((n/sum(n))*100,digits=2), var=rep("Location")) %>% dplyr::rename(value = loc),
            contact %>% group_by(cont_attr) %>% summarize(n=n()) %>% mutate(prop = round((n/sum(n))*100,digits=2),var=rep("Proximity"))  %>% dplyr::rename(value = cont_attr),
            contact %>% group_by(totaltime) %>% summarize(n=n()) %>% mutate(prop = round((n/sum(n))*100, digits =2 ), var=rep("Duration")) %>% dplyr::rename(value = totaltime))%>% 
  mutate(value1 = c("Unique","Repeated","Home","Other","Other's home","Street/Store","Work","Conv.Only","Conv & Phys","Phys.Only", "1-4 hrs","15mins -1hr","5-15 mins","< 5 mins",">4 hrs",NA),
         value1=as.factor(value1),
         value1= ordered(value1, levels= c("< 5 mins","5-15 mins","15mins -1hr","1-4 hrs",">4 hrs",
                                           "Other's home","Work","Other","Street/Store","Home",
                                           "Unique","Repeated",
                                           "Conv.Only","Phys.Only","Conv & Phys")),
         col = c("1","5","1","2","3","4","5","1","2","3","1","2","3","4","5","5"))


mypal <- c(brewer.pal (n = 5, name = "Purples"),
           brewer.pal(n = 5, name = "Blues"),
           brewer.pal(n=9, name="Greys")[c(2,4)],
           brewer.pal (n=9, name = "BuGn")[c(1,3,8)])


ggplot(df, aes(x=var,y=prop, fill=value1)) +
  geom_col(aes(fill=value1)) +
  geom_text(aes(label = value1),
            size = 3.5,
            position = position_stack(vjust = .5))+
  ylab("% of all contacts") + xlab("Contact attribute") +
  ggtitle("A. Distribution of contacts by contact attribute")+
  scale_fill_manual(values = mypal) + 
  theme_classic()+
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position= "none" )
