# Visualising the sleep stage percentages for 17 subjects from Nap study
# Author @Rahul Venugopal on 21st February 2024

# Loading libraries
library(dplyr)
library(ggplot2)
library(ggtext)
library(tidyr)
library(png)
library(cowplot)
library(magick)

# Load data
data <-read.csv("Spindle_Mastersheet_withSlowWaves_10012023_04072023.csv")

# Select relevant columns
data <- data %>% 
  filter(Channel == 'CZ') %>% 
  select(Group,Total.Sleep.Duration..min.., ActualSleep_duration,
         N1_duration, N2.Duration..min.., N3_duration, REM.Duration..min..)

# Computing percentage from durations
macro_percentages <- data %>% 
  mutate(N1 = (N1_duration/Total.Sleep.Duration..min..)*100) %>% 
  mutate(N2 = (N2.Duration..min../Total.Sleep.Duration..min..)*100) %>% 
  mutate(N3 = (N3_duration/Total.Sleep.Duration..min..)*100) %>% 
  mutate(R = (REM.Duration..min../Total.Sleep.Duration..min..)*100) %>% 
  mutate(W = 100 - (N1 + N2 + N3 + R)) %>% 
  mutate(SE = (ActualSleep_duration/Total.Sleep.Duration..min..)*100)

# Retain only group and percentages for viz | Sort the data based on group, then SE
data_viz <- macro_percentages %>% 
  select(Group, N1, N2, N3, R, W, SE)

# Sort the data based on sleep efficiency or whatever way
data_viz <- data_viz[
  with(data_viz, order(Group, SE)),]

data_viz$id <- 1:nrow(data_viz)

# Change the format to to long (tidy) from wide format
data_new <- data_viz %>% gather(sleep_stage,percentage,
                                N1:W)

# Make sleep stages a factor
data_new$sleep_stage <- as.factor(data_new$sleep_stage)

# Changing the order of sleep stages for plot order
new_order <- c('R', 'W', 'N1', 'N2', 'N3')

data_new$sleep_stage <- factor(data_new$sleep_stage, levels = new_order)


# Params for circular viz -------------------------------------------------

# calculate the ANGLE of the labels
number_of_bar <- nrow(data_viz)
angle <-  90 - 360 * (data_viz$id-0.5)  /number_of_bar

data_viz$hjust<-ifelse( angle < -90, 1, 0)

# flip angle to make them readable
data_viz$angle<-ifelse(angle < -90, angle+180, angle)

order <- data_viz$id

pal <- c("#a6cee3","#e76a6a","#fdc086","#efefe8","#7fc97f")

# Viz
sleep_plot <- data_new %>% 
  ggplot( aes(fill=sleep_stage, y=percentage, x=id)) + 
  
  #add bars, order, color
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(limits=order)+
  scale_fill_manual(values=pal)+
  
  labs(subtitle = "The sleep efficiency (in %) is added above each subject",
    title = 'Macro sleep architecture during a nap in <b style="color:#FFD700;">Meditator</b> and <b style="color:#ffffff;">Control</b> subjects') + 
  
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
                                      hjust = .1,
                                      vjust = 6,
                                      color = 'white'),
        plot.subtitle = element_markdown(lineheight = 0,
                                      size = 8,
                                      hjust = .1,
                                      color = 'white'),
        plot.title.position = 'plot',
        panel.border = element_blank()) + 
  
  # add efficiency labels
  geom_text(data=data_viz, aes(x=id, y=110, label=round(SE,2), hjust=hjust), 
            color = ifelse(data_viz$Group == 'MED', "#FFD700", "white"),
            fontface="bold",alpha=0.8, size=2.5, 
            angle= data_viz$angle, inherit.aes = FALSE) +
  
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
             x = 0.4, y = 0.33,
             width = 0.16, height = 0.16)

#save
ggsave("sleep_stages.pdf",
       width = 5,
       height = 5,
       units="in")

ggsave("sleep_stages.png",
       width = 6,
       height = 6,
       units="in")