library(rvest)
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggimage)

#import fonts for later
font_add_google("Chivo", "chivo")
showtext_auto()

#link to wiki page about supreme court justices
url<- 'https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States'

#scrape data from wikipedia url
data<-url%>%
  read_html()%>%
  html_elements(".wikitable")%>%
  html_table(trim=TRUE, fill=TRUE)%>%
  .[[1]]

#rename columns
names(data)<-c("index","blank","justice","state","position","succeeded","confirmed","tenured","length","nominated_by")

data<-data%>%mutate(id = row_number())

#clean up data
justices<-data%>%
  #some justices promoted to chief justice later, remove records to get distinct justices
  filter(!id %in% c(76, 64, 84, 9, 107))%>%
  select(-index,-blank)%>%
  separate(justice, into=c("name","born_died"), sep="\\(")%>%
  mutate(index = row_number(),
         born_died = str_replace(born_died, "\\)",""),
         position = str_replace(position, "\\s*\\[[^\\)]+\\]",""),
         position = case_when(position=="ChiefJustice"~"Chief Justice",position=="AssociateJustice"~"Associate Justice"),
         confirmed = str_replace(confirmed, "\\([^()]*\\)",""),
         confirmed = str_replace(confirmed, "\\s*\\[[^\\)]+\\]",""),
         confirmed = as.Date(confirmed, '%B %d, %Y'),
         demo = case_when(
           name %in% c('Thurgood Marshall','Clarence Thomas')~'black_male',
           name %in% c("Sandra Day O'Connor", "Ruth Bader Ginsburg","Elena Kagan","Amy Coney Barrett")~'white_female',
           name == 'Sonia Sotomayor' ~ 'hispanic_female',
           TRUE ~ 'white_male'
         )
  )%>%
  slice(1:115)


#get list of presidents with party 
pres_url<-'https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States'

#scrape president data
pres_data<-pres_url%>%
  read_html()%>%
  html_elements(".wikitable")%>%
  html_table(trim=TRUE, fill=TRUE)%>%
  .[[1]]

#rename columns
names(pres_data)<-c("index","portrait","name_birth_death","term","party_misc","party","election","vp")

#clean up data
presidents<-pres_data%>%
  select(name_birth_death,party)%>%
  separate(name_birth_death, into=c("name","birth_death"),sep="\\(")%>%
  mutate(party=str_replace(party, "\\s*\\[[^\\)]+\\]",""),
         name=str_replace(name, "\\s*\\[[^\\)]+\\]","")
         )%>%
  distinct(name,party)%>%
  slice(1:49)%>%
  mutate(index=row_number())

#some presidents have two party affiliation listed (different terms)
presidents<-presidents%>%filter(!index %in% c(7,19,20,12))%>%select(-index)
  
#join presidents to justices
justices<-justices%>%left_join(presidents, by=c("nominated_by"="name"))

#add Ketanji Brown Jackson (not yet on Wikipedia)
kbj<-data.frame(
  name="Ketanji Borwn Jackson",
  born_died = NA,
  state = NA,
  position = "Associate Justice",
  succeeded = "Breyer",
  confirmed = as.Date('2022-04-07'),
  tenured = NA,
  length = NA,
  nominated_by="Joe Biden",
  id=NA,
  index = 116,
  demo="black_female",
  party = "Democratic"
)

#combine kbj data with justices
justices<-rbind(justices,kbj)

#create positional variables
justices<-justices%>%
  mutate(y = (index-1) %/% 10,
         x = (index-1) %% 10)

#trick to get pngs for emojis - https://www.emilhvitfeldt.com/post/2020-01-02-real-emojis-in-ggplot2/
white_male_icon<-'https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/325/man-judge-light-skin-tone_1f468-1f3fb-200d-2696-fe0f.png'
black_male_icon<-'https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/325/man-judge-medium-dark-skin-tone_1f468-1f3fe-200d-2696-fe0f.png'
white_female_icon_2<-'https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/325/woman-judge-medium-light-skin-tone_1f469-1f3fc-200d-2696-fe0f.png'
white_female_icon<-'https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/325/woman-judge-light-skin-tone_1f469-1f3fb-200d-2696-fe0f.png'
hispanic_female_icon<-'https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/325/woman-judge-medium-skin-tone_1f469-1f3fd-200d-2696-fe0f.png'
black_female_icon<-'https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/325/woman-judge-medium-dark-skin-tone_1f469-1f3fe-200d-2696-fe0f.png'

#create new variable in justices to link icons
justices<-justices%>%
  mutate(icon = case_when(name=="Sandra Day O'Connor"~white_female_icon_2,
                          demo=="white_male"~white_male_icon,
                          demo=="white_female"~white_female_icon,
                          demo=="black_male"~black_male_icon,
                          demo=="black_female"~black_female_icon,
                          demo=="hispanic_female"~hispanic_female_icon),
         party_group = case_when(party %in% c("Federalist","Whig","Unaffiliated")~"Other",
                                 TRUE ~party))

#create factor
justices$party_group<-factor(justices$party_group, levels=c("Democratic-Republican","Democratic","Republican","Other"))

#plot
ggplot(data=justices, mapping=aes(x=x+1,y=y+1))+
  geom_point(size=12, aes(color=party_group), alpha=0.9)+
  scale_color_manual(values=c("#EBB027","#266DD3","#CC3333","grey"),
                     guide = guide_legend(title.position="top",
                                          title.hjust=0.5,override.aes = list(size=3)))+
  geom_image(aes(image=icon))+
  geom_text(aes(label=index), vjust=4.8, size=2)+
  scale_y_reverse(limits=c(12.5,1))+
  labs(title="United States Supreme Court Justices",
       subtitle="Emoji Portraits of all 116 Justices",
       color="NOMINATING PRESIDENT PARTY AFFILIATION",
       caption = "Data from Wikipedia | Chart @tanya_shapiro")+
  theme(legend.position = "top",
        legend.title=element_text(size=8),
        legend.key = element_rect(fill = NA),
        text = element_text(family="chivo"),
        plot.margin = margin(r=20,l=20, t=10, b=5),
        plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle=element_text(hjust=0.5),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())






ggsave("supreme_court_emojis.jpeg", height=8,width=6.5)
