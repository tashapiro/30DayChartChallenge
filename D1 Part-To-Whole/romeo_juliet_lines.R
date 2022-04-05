library(gutenbergr)
library(tidyverse)
library(ggimage)
library(geomtextpath)

#get list of all shakespeare works, search by author
shakespeare_works<-gutenberg_works(author == "Shakespeare, William")

#find Romeo and Juliet
search<-shakespeare_works%>%filter(title=="Romeo and Juliet")%>%select(gutenberg_id)

#get Romeo and Juliet ID
rj_id<-search$gutenberg_id

#download Romeo and Juliet
rj_raw<-gutenberg_download(rj_id)

grepl("^[[:upper:]]+$",str_replace_all("CHORUS.","\\.",""))

#subset raw download
rj<-rj_raw%>%
  select(text)%>%
  slice(92:nrow(rj_raw))%>%
  filter(text!="")%>%
  mutate(
    text_id = row_number(),
    text_type = case_when(
      grepl("PROLOGUE|ACT",text)~"part",
      grepl("\\[_",text)~"action",
      grepl("^[[:upper:]]+$", str_replace(str_replace_all(text,"\\.","")," ",""))~"speaker",TRUE~"line")
  )

#get list of speakers and all the line numbers
speaker_lines<-rj%>%
  filter(text_type=="speaker")%>%
  select(-text_type)%>%
  rename(start_text=text_id, speaker=text)%>%
  mutate(end_text=lead(start_text)-1,
         speaker = str_replace(speaker, "\\.",""))%>%
  mutate(end_text = replace_na(end_text,nrow(rj)),)%>%
  rowwise()%>%
  do(data.frame(speaker = .$speaker, text_id = seq(.$start_text, .$end_text, by = 1)))


#characters
montague <- c("MONTAGUE","LADY MONTAGUE","ROMEO","BENVOLIO","ABRAM","BALTHASAR")
capulet <- c("CAPULET", "LADY CAPULET","JULIET","TYBALT","CAPULET'S COUSIN","NURSE","PETER",
             "SAMPSON","GREGORY")
neutral<- c("PRINCE","MERCUTIO","PARIS","FRIAR LAWRENCE","FRIAR JOHN","CHORUS","APOTHECARY")

#join speaker with rj text
rj<-rj%>%
  left_join(speaker_lines,by="text_id")%>%
  mutate(speaker = case_when(text_type %in% c('action','part')~ "",TRUE ~ speaker),
         speaker_type = case_when(speaker %in% montague ~ 'Montague',
                                  speaker %in% capulet ~ 'Capulet',
                                  speaker %in% neutral ~ 'Neutral')
         )

#summarise parts
parts<-rj%>%
  filter(text_type=="line" & speaker_type %in% c("Montague","Capulet","Neutral"))%>%
  group_by(speaker, speaker_type)%>%
  summarise(line_count=n())%>%
  arrange(-line_count)
parts$perc <- parts$line_count/sum(parts$line_count)

#houses
houses<-rj%>%
  filter(speaker_type %in% c("Montague","Capulet","Neutral"))%>%
  group_by(speaker_type)%>%
  summarise(line_count=n())%>%
  arrange(-line_count)
houses$perc <- houses$line_count/sum(houses$line_count)

#create factors for speaker_type
houses$speaker_type<-factor(houses$speaker_type, levels=c("Capulet","Montague","Neutral"))
parts$speaker_type<-factor(parts$speaker_type, levels=c("Capulet","Montague","Neutral"))

parts<-parts%>%
  arrange(desc(speaker_type), -line_count)%>%
  ungroup()%>%
  mutate(end_lim = cumsum(perc))%>%
  mutate(start_lim = replace_na(lag(end_lim),0),
         mid_pt = (end_lim+start_lim)/2,
         text_col = case_when(speaker_type=="Neutral"~"black",TRUE~"white"),
         fill_col = case_when(speaker_type=="Capulet"~"#1E91D6",
                              speaker_type=="Montague"~"#E84855",
                              speaker_type=="Neutral"~"#FFDA22"))

houses<-houses%>%
  arrange(desc(speaker_type))%>%
  ungroup()%>%
  mutate(end_lim = cumsum(perc))%>%
  mutate(start_lim = replace_na(lag(end_lim),0),
         mid_pt = (end_lim+start_lim)/2,
         text_col = case_when(speaker_type=="Neutral"~"black",TRUE~"white"),
         fill_col = case_when(speaker_type=="Capulet"~"#1E91D6",
                              speaker_type=="Montague"~"#E84855",
                              speaker_type=="Neutral"~"#FFDA22"))


total <- data.frame(x=1, y=1, label='BY HOUSE \n AND ROLE', pos = 0.5, image="rj.png")

#plot
ggplot()+
  geom_rect(data=data.frame(xmin=0, xmax=1, ymin=0, ymax=.75), 
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="white")+
  geom_image(data=total, mapping=aes(y=0,x=1,image=image), size=0.25)+
  geom_rect(data=houses, 
            mapping=aes(xmin=start_lim, xmax=end_lim, ymin=1, ymax=1.75, 
                        fill=fill_col), color="white")+
  geom_rect(data=parts, 
            mapping=aes(xmin=start_lim, xmax=end_lim, ymin=1.90, ymax=2.75, 
                        fill=fill_col), color="white")+
  geom_textpath(data=houses, mapping=aes(x=mid_pt, y=1.35, label=toupper(speaker_type)), color="black", size = 3, text_only = TRUE, family="Gill Sans") +
  geom_textpath(data=houses%>%filter(speaker_type!="Neutral"), mapping=aes(x=mid_pt, y=1.35, label=toupper(speaker_type)), upright=FALSE, color="white", size = 3, text_only = TRUE, family="Gill Sans") +
  geom_textpath(data=parts%>%filter(speaker_type!="Neutral" & perc>=0.02), mapping=aes(x=mid_pt, y=2.35, label=paste0(str_replace(speaker," ","\n"),"\n",round(perc*100,1),"%")), color="white", size = 2.2, text_only = TRUE, family="Gill Sans") +
  geom_textpath(data=parts%>%filter(speaker_type=="Neutral" & perc>=0.025), mapping=aes(x=mid_pt, y=2.35, label=paste0(str_replace(speaker," ","\n"),"\n",round(perc*100,1),"%")), color="black", size = 2.2, text_only = TRUE, family="Gill Sans") +
   #geom_text(data=parts%>%filter(perc>=0.02), mapping=aes(x=mid_pt, y=2.35, label=str_replace(speaker," ","\n"), color=text_col),  size=2.2, family="Gill Sans")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_polar()+
  labs(title="Don't Sign Me Up For Romeo", 
       fill= "",
       subtitle="Division of Lines by Speaking Parts in Shakespeare's Romeo & Juliet", 
       caption="Data from Project Gutenberg | Chart @tanya_shapiro")+
  theme(legend.position = "none",
        text = element_text(family="Gill Sans"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill="white"),
        plot.title=element_text(hjust=0.5, face="bold", vjust=-5.5),
        plot.subtitle=element_text(hjust=0.5, size=10, vjust=-7.5),
        plot.caption = element_text(vjust=5)
  )
  
 