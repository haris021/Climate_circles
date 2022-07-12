library(tidyverse)
library(lubridate)
library(fs)
library(janitor)
# import data
meteo <- dir_ls(regexp = ".csv$") %>% 
  map_df(read_csv)

stats_names <- unique(meteo$NAME)
stats_names
cities <- c("KOTLI","MUZAFFARABAD","JHELUM")

meteo <- select(meteo, NAME, DATE, TAVG:TMIN) %>%  
  filter(DATE >= "1991-01-01", DATE <= "2000-12-31") %>% 
    mutate(NAME = factor(NAME, stats_names, cities), 
           TAVG = ifelse(is.na(TAVG), (TMAX+TMIN)/2, TAVG),    
         yd = yday(DATE)) %>% 
  clean_names()

meteo
# estimate the daily averages
meteo_yday <- group_by(meteo, name, yd) %>% 
  summarise(ta = mean(tavg, na.rm = TRUE),
            tmx = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE))
meteo_yday
meteo_yday <- mutate(meteo_yday, yd = as_date(yd, origin = "1999-12-31"))
col_temp <- c("#cbebf6","#a7bfd9","#8c99bc","#974ea8","#830f74",
              "#0b144f","#0e2680","#223b97","#1c499a","#2859a5",
              "#1b6aa3","#1d9bc4","#1ca4bc","#64c6c7","#86cabb",
              "#91e0a7","#c7eebf","#ebf8da","#f6fdd1","#fdeca7",
              "#f8da77","#fcb34d","#fc8c44","#f85127","#f52f26",
              "#d10b26","#9c042a","#760324","#18000c")

grid_x <- tibble(x = seq(ymd("2000-01-01"), ymd("2000-12-31"), "month"), 
                 y = rep(-10, 12), 
                 xend = seq(ymd("2000-01-01"), ymd("2000-12-31"), "month"), 
                 yend = rep(41, 12))
theme_cc <- function(){ 
  
  theme_minimal(base_family = "Montserrat") %+replace%
    theme(plot.title = element_text(hjust = 0.5, colour = "white", size = 30, margin = margin(b = 20)),
          plot.caption = element_text(colour = "white", size = 9, hjust = .5, vjust = -30),
          plot.background = element_rect(fill = "black"),
          plot.margin = margin(1, 1, 2, 1, unit = "cm"),
          
          axis.text.x = element_text(face = "italic", colour = "white"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          
          legend.title = element_text(colour = "white"),
          legend.position = "bottom",
          legend.justification = 0.5,
          legend.text = element_text(colour = "white"),
          
          
          strip.text = element_text(colour = "white", face = "bold", size = 14),
          
          panel.spacing.y = unit(1, "lines"),
          panel.background = element_rect(fill = "black"),
          panel.grid = element_blank()
    ) 
}
# filter New York
ny_city <- filter(meteo_yday, name == "KOTLI") 

# graph
ggplot(ny_city) + 
  geom_linerange(aes(yd, 
                     ymax = tmx, 
                     ymin = tmin, 
                     colour = ta),
                 size=0.5, 
                 alpha = .7) + 
  scale_y_continuous(breaks = seq(-30, 50, 10), 
                     limits = c(-11, 42), 
                     expand = expansion()) +
  scale_colour_gradientn(colours = col_temp, 
                         limits = c(-12, 35), 
                         breaks = seq(-12, 34, 5)) + 
  scale_x_date(date_breaks = "month",
               date_labels = "%b") +
  labs(title = "CLIMATE CIRCLES", 
       colour = "Daily average temperature") 
# polar chart
ggplot(ny_city) + 
  geom_linerange(aes(yd, 
                     ymax = tmx, 
                     ymin = tmin, 
                     colour = ta),
                 size=0.5, 
                 alpha = .7) + 
  scale_y_continuous(breaks = seq(-30, 50, 10), 
                     limits = c(-11, 42), 
                     expand = expansion()) +
  scale_colour_gradientn(colours = col_temp, 
                         limits = c(-12, 35), 
                         breaks = seq(-12, 34, 5)) + 
  scale_x_date(date_breaks = "month",
               date_labels = "%b") +
  coord_polar() +
  labs(title = "CLIMATE CIRCLES", 
       colour = "Daily average temperature") 

ggplot(meteo_yday) + 
  geom_hline(yintercept = c(-10, 0, 10, 20, 30, 40), 
             colour = "white", 
             size = .4) +
  geom_segment(data = grid_x , 
               aes(x = x, 
                   y = y, 
                   xend = xend, 
                   yend = yend), 
               linetype = "dashed", 
               colour = "white", 
               size = .2) +
  geom_linerange(aes(yd, 
                     ymax = tmx, 
                     ymin = tmin, 
                     colour = ta),
                 size=0.5, 
                 alpha = .7) + 
  scale_y_continuous(breaks = seq(-30, 50, 10), 
                     limits = c(-11, 42), 
                     expand = expansion())+
  scale_colour_gradientn(colours = col_temp, 
                         limits = c(-12, 35), 
                         breaks = seq(-12, 34, 5)) + 
  scale_x_date(date_breaks = "month", 
               date_labels = "%b") +
  guides(colour = guide_colourbar(barwidth = 15,
                                  barheight = 0.5, 
                                  title.position = "top")
  ) +
  facet_wrap(~name, nrow = 1) +
  coord_polar() + 
  labs(title = "CLIMATE CIRCLES", 
       caption = "Developed by Haris Mushtaq",
       colour = "Daily average temperature") +
  theme_cc()
ggsave("test3.jpg", units="in", width=12, height=8, dpi=300)
