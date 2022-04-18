library(rvest)
library(tidyverse)
library(network)
library(ggnet)
library(showtext)
library(extrafont)


#Create Function to Scrape Partners ----
get_partners<- function(character) {
  
  base_url<- 'https://the-l-word.fandom.com/wiki/'
  
  url <- paste0(base_url,str_replace_all(character," ","_"))
  
  html<-read_html(url)
  
  partner_names<-html%>%
    html_nodes(".pi-item[data-source=partners]")%>%
    html_elements("a")%>%
    html_text()
  
  partner_links<-html%>%
    html_nodes(".pi-item[data-source=partners]")%>%
    html_elements("a")%>%
    html_attr("href")
  
  data <- data.frame(character = rep(character,length(partner_names)), partner_name =partner_names, partner_link = partner_links)
  
  data <- data%>%filter(partner_names != "See full list")
  
  data
  
}


#test function
get_partners("Bette Porter")



#Scrape Data ----
#list of original L Word characters 
og_characters <- c("Alice Pieszecki", "Bette Porter", "Carmen de la Pica Morales", "Dana Fairbanks", "Helena Peabody",
                "Jenny Schecter","Jodi Lerner","Kit Porter","Marina Ferrer","Max Sweeney","Papi",
                "Shane McCutcheon",'Tasha Williams', "Tim Haspel","Tina Kennard")


gq_characters <- c("Gigi Ghorbani","Sarah Finley", "Sophie Suarez", "Micah Lee")

minor_characters<-c('Cherie Jaffe', 'Dawn Denbo', 'Gabby Deveaux', 'Lara Perkins', 'Niki Stevens',
                    'Phyllis Kroll', 'Paige Sobel', 'Molly Kroll', 'Joyce Wischnia', 'Nadia Karella')

all_characters<-c(og_characters,gq_characters,minor_characters)

partners <- data.frame(character=NA, partner_name = NA, partner_link = NA)

#loop and scrape all partners for each character, append to partners dataframe
for (character in all_characters) {
  print(character)
  data <- get_partners(character)
  partners <- rbind(partners, data)
}

#Clean Data ----
partners<-partners%>%filter(!grepl("cite",partner_link) & !is.na(character))

#Create New Column with Concatenated Couple Alphabetical Order h/t https://stackoverflow.com/questions/25588426/pasting-elements-of-two-vectors-alphabetically
partners$couple<-paste0(pmin(partners$character, partners$partner_name),"-", pmax(partners$character, partners$partner_name))

#De-Dupe
deduped_partners<-partners%>%distinct(couple)%>%separate(couple, into=c("from_id","to_id"), sep="-")

#Manipulate Names
deduped_partners<-deduped_partners%>%
  mutate(from_id = case_when(from_id %in% og_characters ~toupper(word(from_id, 1)),
                             from_id == "Cindi Annabelle Tucker" ~ "Lover Cindi",
                             from_id == "Dani Nùñez" ~ "DANI",
                             from_id == "Sarah Finley" ~ "FINLEY",
                             from_id %in% gq_characters ~toupper(word(from_id, 1)),
                             TRUE ~ from_id),
         to_id = case_when(to_id %in% og_characters ~toupper(word(to_id, 1)),
                           to_id == "Cindi Annabelle Tucker" ~ "Lover Cindi",
                           to_id == "Sarah Finley" ~ "FINLEY",
                           to_id == "Dani Nùñez" ~ "DANI",
                           to_id %in% gq_characters ~toupper(word(to_id, 1)),
                             TRUE ~ to_id))
         

#Create Network Object ----
net_data <- as.network(x = deduped_partners, # the network object
                          directed = TRUE, # specify whether the network is directed
                          loops = FALSE, # do we allow self ties (should not allow them)
                          matrix.type = "edgelist" # the type of input
)

#first name lists for main characters
og_first<- c("Alice","Bette","Tina","Jenny","Shane","Kit","Papi","Max",
             "Jodi","Helena","Dana","Tim","Carmen","Marina","Tasha")
gq_first <-c("Gigi","Finley","Sophie","Dani")

#Add Attributes to Vertex to change color, size, and font type
net_data %v% "node_color" = ifelse(network.vertex.names(net_data) %in% toupper(og_first),"pink",
                                  ifelse(network.vertex.names(net_data) %in% toupper(gq_first),"#9D8294",
                                  "grey"))

net_data %v% "size" = ifelse(network.vertex.names(net_data) %in% c("SHANE","BETTE","ALICE"), 7,
                      ifelse(network.vertex.names(net_data) %in% toupper(og_first), 5,
                                  ifelse(network.vertex.names(net_data) %in% toupper(gq_first),5,
                                         3)))

net_data %v% "family" = ifelse(network.vertex.names(net_data) %in% toupper(og_first), "Roof runners Bold",
                             ifelse(network.vertex.names(net_data) %in% toupper(gq_first),"Roof runners Bold",
                                    "Roof Runners"))

#Plot ----
ggnet2(net_data,  color="white", mode='kamadakawai', edge.color="grey65")+
  geom_text(aes(label = str_replace(label," ","\n")), 
            size=net_data %v% "size",
            family = net_data %v% "family")+
  labs(title="The Chart", 
       subtitle="Relationships on The L Word", 
       caption="Data from The L Word Fandom Wiki | Chart @tanya_shapiro")+
  theme(text=element_text(family="Roof Runners"),
        plot.title=element_text(size=25, family="Roof runners Bold"),
        plot.caption = element_text(margin=margin(t=5)),
        plot.margin=margin(t=10,b=10,r=10,l=10))

#export
ggsave("the_chart.jpeg", height=7, width=9)
