# Visualising the sleep stage percentages for 17 subjects from Nap study
# Author @Rahul Venugopal on 12th October 2023

# Loading libraries
library(dplyr)
library(ggplot2)
library(ggtext)
library(tidyr)
library(png)
library(cowplot)

# Load data
data <-read.csv("master_data.csv")

# Calculate sleep efficiency
data <- data %>%
  mutate(SE = (N1 + N2 + N3 + R))

# Sort the data based on sleep efficiency or whatever way
data <- data %>% 
  arrange(SE)

data$id <- 1:nrow(data)

# Change the format to to long (tidy) from wide format
data_new <- data %>% gather(sleep_stage,percentage,N1:W)

# Make sleep stages a factor
data_new$sleep_stage <- as.factor(data_new$sleep_stage)

# Changing the order of sleep stages for plot order
new_order <- c('R', 'W', 'N1', 'N2', 'N3')

data_new$sleep_stage <- factor(data_new$sleep_stage, levels = new_order)

# calculate the ANGLE of the labels
number_of_bar <- nrow(data)
angle <-  90 - 360 * (data$id-0.5)  /number_of_bar

data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle to make them readable
data$angle<-ifelse(angle < -90, angle+180, angle)

order <- data$id

pal <- c("#a6cee3","#e76a6a","#fdc086","#efefe8","#7fc97f")

# Viz
sleep_plot <- data_new %>% 
  ggplot( aes(fill=sleep_stage, y=percentage, x=id)) + 
  
  #add bars, order, color
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(limits=order)+
  scale_fill_manual(values=pal)+
  
  labs(title = '<br> Macro sleep architecture during a nap in <b style="color:#FFD700;">young</b> and <b style="color:#ffffff;">old</b> subjects') + 
  
  # add ylim to keep empty circle in middle
  ylim(-120,120) +
  
  coord_polar(direction = 1,
              clip = "off") +
  
  # remove axes and turn polar
  theme_void() +
  theme(legend.position = c(0.57,0.525),
        legend.key.size = unit(0.4, units = "cm"),
        legend.title = element_blank(),
        legend.text = element_text(colour="white", size=6, 
                                   face="bold"),
        plot.background = element_rect(fill="#516869"),
        plot.title = element_markdown(lineheight = 1.1,
                                      size = 10,
                                      hjust = 0.5,
                                      color = 'white'),
        plot.title.position = 'plot',
        panel.border = element_blank()) + 
  
  # add efficiency labels
  geom_text(data=data, aes(x=id, y=110, label=SE, hjust=hjust), 
            color = ifelse(data$SE>90, "#FFD700", "white"),
            fontface="bold",alpha=0.8, size=2.5, 
            angle= data$angle, inherit.aes = FALSE) +

  # annotate title in middle
  annotate(geom = "text",
           x=0,y=-55,
           hjust=.55, vjust=1,
           label=" ",
           size=3.5, lineheight=.8,
           color="grey90")

# load the logo
image_loc = readPNG("sleeping.png")

ggdraw(sleep_plot) + 
  
  draw_image(image_loc,
             x = 0.42, y = 0.32,
             width = 0.16, height = 0.16)

#save
ggsave("sleep_stages.pdf",
       width = 5,
       height = 5,
       units="in")