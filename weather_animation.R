library(ggplot2)
library(gganimate)
library(dplyr)
library(stringr)

weather <- read.csv('weather.csv', stringsAsFactors = FALSE)


#filter data
weather <- weather %>% 
  filter(year >= 1900) %>% 
  mutate(month = factor(month, levels = unique(weather$month), ordered = TRUE),
         month_numeric = as.numeric(month))

#get min and max for every month
ranges <- weather %>% 
  group_by(month) %>% 
  filter(temperature == min(temperature, na.rm = TRUE) | 
         temperature == max(temperature, na.rm = TRUE))

#generate data to trick geom_step
fake_data <- weather %>% 
  filter(month_numeric ==  12) %>% 
  mutate(month = '', month_numeric = 13)

#combine
weather <- rbind.data.frame(weather, fake_data)

#generate data for fancy x axis month positioning
xaxis <- data.frame(x = seq(1.5, 12.5, 1),
                    y = 29,
                    label = levels(weather$month)[1:12])



#plot
g <- ggplot(weather, aes(frame = year, cumulative = TRUE))+
  geom_step(aes(x = month_numeric, y = temperature, group = year), color = '#3A3F4A', alpha = 0.5)+
  geom_text(aes(x = 7.5, y = 2.5, label = year, frame = year, family = 'Ubuntu Condensed'), 
            size = 52, color = 'gray', inherit.aes = F)+
  geom_text(data = xaxis, aes(x = x, y = y, label = label, family = 'Ubuntu Condensed'), 
            size = 6, color = '#3A3F4A', inherit.aes = F)+
  scale_x_continuous(breaks = 1:13, labels = NULL, expand = c(0.01, 0.01))+
  scale_y_continuous(breaks = seq(-15, 25, 5), limits = c(-17, 31),
                     expand = c(0, 0))+
  labs(title = "Температура в Києві, 1900-2014 рік",
       subtitle = str_wrap("Горизонтальні лінії на графіку позначають середню температуру для кожного місяця за період спостережень з 1900 до 2014 року", 160),
       caption = "\nДані: Центральна геофізична обсерваторія | Візуалізація: Textura.in.ua")+
  theme_minimal()+
    theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
          axis.title = element_blank(),
          axis.text = element_text(size = 16),
          axis.text.x = element_blank(),
          panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = 'bold', size = 40, margin = margin(b = 10, t = 20)),
          plot.subtitle = element_text(size = 20, face = 'plain', margin = margin(b = 30)),
          plot.caption = element_text(size = 16, margin = margin(b = 10), color = '#5D646F'),
          plot.background = element_rect(fill = '#EFF2F4'),
          plot.margin = unit(c(2, 2, 2, 2), 'cm'))

gganimate(g, title_frame = F, filename = 'weather.gif', ani.height = 1000, ani.width = 1200, 
          interval = 0.25, loop = 4)
