library(tidyverse)
library(ggimage)
library(geomtextpath)

#personal peloton data wrangled with pelotonR https://www.littlemissdata.com/blog/pelotonr
data<-read.csv("peloton_data.csv")

#format data
data$timeframe<-factor(data$timeframe, levels=c("Previous YTD","Current YTD"))
data_wide<-data%>%spread(key=timeframe, value=workouts)
data$timeframe<-factor(data$timeframe, levels=c("Previous YTD","Current YTD"))
data$image<-paste0('image_icons/',data$fitness_discipline,'-remove.png')

#palette
pal<-c("#d63535","#FFC413","#60d394","#009fb7","#4439a5")

#data frame to produce curved arrows
curve<-data.frame(y1=c(14,10),y2 = c(16,6),x1=c(0.7,0.7),x2=c(0.89,0.89), curve=c(0.3,0.3), type=c("top","bottom"))

ggplot(data, aes(x=timeframe, y=workouts))+
  geom_point(size=10, aes(color=fitness_discipline))+
  #provide annotations for # of workouts besides points
  geom_text(data=data%>%filter(timeframe=="Previous YTD"), aes(x=0.92,y=workouts,label=workouts), size=3.5, family="Gill Sans")+
  geom_text(data=data%>%filter(timeframe=="Current YTD"), aes(x=2.08,y=workouts,label=workouts), size=3.5, family="Gill Sans")+
  #add geom segment lines with text from geomtextpath packages
  geom_textsegment(data=data_wide, 
               mapping=aes(x="Previous YTD", xend="Current YTD", y=`Previous YTD`, yend=`Current YTD`, 
                           color=fitness_discipline, label=fitness_discipline),
               family="Gill Sans")+
  #overlay png image on dots
  geom_image(aes(image=image), color="white",size=0.035, asp=1)+
  geom_point(data=data%>%filter(timeframe=="Current YTD" & fitness_discipline=="Running"),
             mapping=aes(x=timeframe, y=workouts, color=fitness_discipline), size=10)+
  geom_image(data=data%>%filter(timeframe=="Current YTD" & fitness_discipline=="Running"),
             mapping=aes(x=timeframe, y=workouts, image=image), size=0.035,asp=1, color="white")+
  #more annotations
  annotate("text",x=2.3, y=48.5, label="Big increase in Cycling. \n Purchased Bike June 2021.", family="Gill Sans", size=2.8)+
  annotate("text",x=0.7, y=12, label="Number of \n Workouts", size=2.8, family="Gill Sans")+
  annotate("text",x=2.29, y=2.7, label="Running & Circuit \n classes decline. Limited \n access to Treadmill in 2022.",
           size=2.8, family="Gill Sans")+
  #curved arrows
  geom_curve(data=curve%>%filter(type=='top'),mapping=aes(x=x1,xend=x2,y=y1,yend=y2),arrow=arrow(length = unit(0.07, "inch")), size=0.2, curvature=-0.3)+
  geom_curve(data=curve%>%filter(type=='bottom'),mapping=aes(x=x1,xend=x2,y=y1,yend=y2),arrow=arrow(length = unit(0.07, "inch")), size=0.2, curvature=0.3)+
  #adjust colors using manually created palette
  scale_color_manual(values=pal)+
  labs(title="PELOTON CLASS SWITCH UP",
       subtitle="Current (2022) & Previous (2021) YTD Comparison of personal Peloton workouts by Class Type \n Data for user @redsourpatchkid as of April 16th, 2022",
       x="", y="Number of Workouts", 
       color="Class Type",
       caption = "Data from Peloton API | Chart @tanya_shapiro")+
  theme_minimal()+
  theme(text=element_text(family="Gill Sans"),
        plot.title=element_text(face="bold",hjust=0.5),
        plot.subtitle=element_text(hjust=0.5, margin=margin(t=5, b=5)),
        plot.margin=margin(t=15,b=5),
        plot.caption=element_text(hjust=0.95),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.position="none")


ggsave("peloton_switch.jpeg", height=9, width=8)
