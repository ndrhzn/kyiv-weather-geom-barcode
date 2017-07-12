library(ggplot2)
library(dplyr)
library(stringr)

weather <- read.csv('weather.csv', stringsAsFactors = FALSE)


#filter data
weather <- weather %>% 
  #filter(year >= 1850) %>% 
  mutate(month = factor(month, levels = unique(weather$month), ordered = TRUE),
         month_numeric = as.numeric(month))

#generate data for fancy x axis month positioning
xaxis <- data.frame(x = seq(1.5, 12.5, 1),
                    y = 29,
                    label = levels(weather$month))

#get data for year labels
ranges <- weather %>% 
  group_by(month) %>% 
  filter(temperature == min(temperature, na.rm = TRUE) | 
           temperature == max(temperature, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(month, temperature) %>% 
  summarise(year = paste(year, collapse = ','), temp = unique(temperature)) %>% 
  select(month, temperature = temp, year = year ) %>% 
  inner_join(xaxis, by = c('month' = 'label')) %>% 
  select(-y) %>% 
  mutate(temperature = if_else(temperature == max(temperature), temperature+1, temperature-1))

#segment and annotation data

segment1 <- data.frame(
  
  x  = c(1.5),
    
  xend = c(2.25),
  
  y = c(4.75),
  
  yend = c(20),
  
  label.x = c(2.8),
  
  label.y  = c(19),
  
  label = c('рік, коли було зафіксовано максимальну середньомісячну температуру')
)


segment2 <- data.frame(
  
  x  = c(6.5),
  
  xend = c(7),
  
  y = c(11),
  
  yend = c(0),
  
  label.x = c(7.5),
  
  label.y  = c(-1),
  
  label = c('рік, коли було зафіксовано мінімальну середньомісячну температуру')
)

#generate data to trick geom_step
fake_december <- weather %>% 
  filter(month_numeric ==  12) %>% 
  mutate(month = '', month_numeric = 13)

#combine
weather <- rbind.data.frame(weather, fake_december)

png(filename = 'weather_static_step.png', width = 1200, height = 1000)

#plot
ggplot(weather)+
  geom_step(aes(x = month_numeric, y = temperature, group = year), color = '#3A3F4A', alpha = 0.3)+
  geom_text(data = xaxis, aes(x = x, y = y, label = label, family = 'Ubuntu Condensed'), 
            size = 6, color = '#3A3F4A', inherit.aes = F)+
  geom_text(data = ranges, aes(x = x, y = temperature, label = year, family = 'Ubuntu Condensed'), 
            size = 5, color = '#3A3F4A')+
  geom_curve(data = segment1, aes(x = x, y = y, xend = xend, yend = yend), curvature = -0.2)+
  geom_text(data = segment1, aes(x = label.x, y = label.y, label = str_wrap(label, 20), family = 'Ubuntu Condensed'), 
            size = 5, color = '#3A3F4A')+
  geom_curve(data = segment2, aes(x = x, y = y, xend = xend, yend = yend), curvature = 0.2)+
  geom_text(data = segment2, aes(x = label.x, y = label.y, label = str_wrap(label, 20), family = 'Ubuntu Condensed'), 
            size = 5, color = '#3A3F4A')+
  scale_x_continuous(breaks = 1:13, labels = NULL, expand = c(0.01, 0.01))+
  scale_y_continuous(breaks = seq(-15, 25, 5), limits = c(-19, 31),
                     expand = c(0, 0))+
  labs(title = "Температура в Києві, 1812-2014 рік",
       subtitle = str_wrap("Горизонтальні лінії на графіку позначають середню температуру для кожного місяця за період спостережень з 1812 до 2014 року", 160),
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

dev.off()
