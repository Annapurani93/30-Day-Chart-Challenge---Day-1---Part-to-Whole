library(readr)
library(readxl)
library(tidyverse)
library(ggtext)
read_excel("Share of women.xlsx")->women
hsize <- 4
data.frame(women)->women
colnames(women)<-c("Country","Women","Men","Tech")
women


women%>%
  rowwise()%>%
  mutate(Wshare=Women*100)%>%
  mutate(Mshare=Men*100)%>%
  select(-c(Women,Men,Tech,x))->women1

women1%>%
  gather("Wshare","Mshare",2:3)->women1

colnames(women1)<-c("Country","Gender","Share")

ggplot(women1, aes(x = hsize, y = Share, fill = Gender)) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(~Country)+
  geom_col(color = "white")+
  scale_fill_manual(values = c("#000000", "#70dbb7"))+
  geom_text(aes(label = paste0(Share,"%")),size=2.1,
             colour="black",
             position = position_stack(vjust = 0.5)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text= element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,0.5,1,0.5),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_markdown(size=18,face="bold",margin=margin(b=30)),
        plot.subtitle = element_markdown(size=16,margin=margin(b=30)),
        plot.caption = element_text(size=12, colour="white",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(2, "lines"))+
  labs(title="<span style='color:#70dbb7'>WOMEN <span style='color:#ffffff'>ACCOUNT FOR LESS THAN 50% OF<br> THE TOTAL WORKFORCE IN MOST COUNTRIES</span>",
        subtitle="<span style='color:#ffffff'>In most advanced countries - including the US and the UK, <span style= 'color:#70dbb7'> Women <span style='color:#ffffff'>account for <br>less than 50% of the total workforce. However, in India, the share is abysmal<br> at 20% </span>",
       caption="Data: Statista| Design and Analysis: @annapurani93")->plot

ggsave("womenshare.png",plot,width=9,height=12.7)
ggsave("womenshare.pdf",plot,width=9,height=12.7)


