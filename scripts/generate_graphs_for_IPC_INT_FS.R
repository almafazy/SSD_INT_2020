#IPC Analysis - INT/FSL FS Graphs
#ZACK ARNO, JAN 2020

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(scales)
library(extrafont)
library(ggpubr)
library(ggplot2)


int<-read.csv("inputs/ipc_wkshop/INT_DB.csv", stringsAsFactors=FALSE)

#INT/FSL GRAPHS
for(i in 1:length(int_county_names)){
  print(i)
  county_name_int<-int_county_names[i]

  int_plot<-int%>%
    tidyr::gather(key="Trigger_Type", value="Trigger_Val",c("Total.Triggers","FSL.Triggers")) %>%
    mutate(date=lubridate::ymd(paste0(year,"-",month,"-01"))) %>%
    filter(county==county_name_int) %>%
    ggplot(aes(x=date, y= Trigger_Val,group=factor(Trigger_Type), colour=factor(Trigger_Type)))+
    geom_line( stat = "identity",lwd=0.8)+
    geom_point(aes(x=date, y= Trigger_Val, group=factor(Trigger_Type)))+
    scale_colour_manual(values=c("#a7a9acff",'#EE5859'), labels=c("Total FSL", "Total INT"))+
    scale_y_continuous(limits = c(0,25))+
    scale_x_date(date_breaks = "1 month", date_labels = "%B")+
    labs(x= "Time Period", y= "Cumulative Score") +theme_clean()+
    theme(
      axis.text.x = element_text(angle=30, margin=margin(t=20,r=0, b=0, l=0), size=8),
      axis.text.y=element_text(angle=90, size=8),
      axis.title.y = element_text(size=8),
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = 'white',colour=NA),
      legend.position = c(.91, .83),
      legend.background = element_rect(fill=alpha('white', 0.7),colour="white", size = 0.5),
      legend.text = element_text(size=6),
      legend.key=element_blank(),
      legend.title = element_blank()

    )
  ggsave(paste0("outputs/ipc_wkshop/", county_name_int,".pdf"), width = 138.8, height = 60.8, units = "mm")
}


