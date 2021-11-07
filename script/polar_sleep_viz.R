# Visualising the sleep stage percentages for 130 subjects
# Author @Rahul Venugopal

# Loading libraries
library(dplyr)
library(ggplot2)
library(ggtext)
library(tidyr)
library(png)
library(cowplot)

# Content to be put on the center circle
df <- data.frame(
  label = c(
    "Sleep stage durations of<br> <span style = 'color:#e76a6a'>**N1 stage**</span>, <span style = 'color:#efefe8'>**N2 stage**,</span> & <span style = 'color:#6dac4f'>**N3 stage**</span><br>"),
  x = c(100),
  y = c(-100),
  hjust = c(0.5),
  vjust = c(1),
  angle = c(0),
  color = c("grey90")
)

# Load data
data <-read.csv("sleep_data.csv")

data_new <- data %>% gather(sleep_stage,percentage,N1Percent:N3Percent)

# creating labels first
label_data2 <- data_new %>% group_by(UUID) %>% 
  summarise(sum=sum(percentage))

label_data2$id <- c(1:nrow(label_data2))

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data2)
angle <-  90 - 360 * ( label_data2$id-.5 )  /number_of_bar    

label_data2$hjust<-ifelse( angle < -90, 1, 0)

# flip angle to make them readable
label_data2$angle<-ifelse(angle < -90, angle+180, angle)

# find order of countries to use for scale_x, turn into factor
order_df <- data %>%
  select(UUID,N3Percent)  %>% 
  arrange(desc(N3Percent)) %>% 
  select(UUID)
order <- order_df$UUID
order <- c(order)

pal <- c("#e76a6a","#efefe8","#6dac4f")

# Viz
sleep_plot <- data_new %>% 
ggplot( aes(fill=as.factor(sleep_stage), y=percentage, x=UUID)) + 
  
  #add bars, order, color
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(limits=order)+
  scale_fill_manual(values=pal,guide="none")+
  
  # add ylim to keep empty circle in middle
  ylim(-100,100) +
  
  # remove axes and turn polar
  theme_void() +
  coord_polar(direction = 1,
              clip = "off") +
  
  # add country labels
  geom_text(data=label_data2, aes(x=UUID, y=100, label=UUID, hjust=hjust), 
            color="#FFD700", fontface="bold",alpha=0.6, size=2.5, 
            angle= label_data2$angle, inherit.aes = FALSE )  +
  
  # add annotation in middle with colored words
  geom_richtext(inherit.aes = F, data=df,
                aes(x,y,label=label,hjust=hjust),
                fill=NA, label.color=NA,size=2.75,
                family="Helvetica Neue",color="grey90")+
  
  # annotate title in middle
  annotate(geom = "text",
           x=0,y=-55,
           hjust=.5, vjust=1,
           label="How good is your sleep?",
           size=3.5, lineheight=.8,
           family="Staatliches Regular",
           color="grey90")+
  
  #background fill
  theme(plot.background = element_rect(fill="#516869"),
        plot.caption = ggtext::element_markdown(size = 8,
                                                color = "#FFD700",
                                                alpha(0.1),
                                                hjust = 0.98,
                                                vjust = -0.2),
        panel.border = element_blank()) +
  
  labs(caption = '**Visualisation:** Rahul Venugopal <br>')
  
# load the logo
  image_loc = readPNG("sleeping.png")
  
  ggdraw(sleep_plot) + 
  
  draw_image(image_loc,
             x = 0.42, y = 0.345,
             width = 0.16, height = 0.16) + 
    
    annotate(geom = "text", x = 0.55, y = 0.95,
             label = "Subject ID of each volunteer",
             hjust = "left",
             size = 4,
             color = "#FFD700") + 
    
    annotate(geom = "curve",
             x = 0.55, y = 0.95,
             xend = 0.5, yend = 0.92,
             curvature = .3,
             color = "#FFD700",
             alpha = 0.3,
             arrow = arrow(length = unit(2, "mm")))
  
#save
ggsave("sleep_stages.pdf",
       width = 5,
       height = 5,
       units="in",
       device = cairo_pdf)