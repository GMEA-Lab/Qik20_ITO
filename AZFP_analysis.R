# starting template for acoustic data analysis. 
# please see Qik20_ITO.EV for most recent .EV file
library(tidyverse)
sl.depth <- read.csv("azfp_export/SLdepth.depth.csv")
colnames(sl.depth)[1] <- "Index"

#
sl.depth$datetime <- as.POSIXct(paste(sl.depth$Depth_date, sl.depth$Depth_time), format="%Y-%m-%d %H:%M:%S", tz="UTC")

#position of the mooring
latitude <-  67.48051
longitude <- -63.80826
#calculate sun angle
library(oce)
sun_angle <- sunAngle(t=sl.depth$datetime, lat= latitude ,lon= longitude)
sl.depth$sun_altitude <- sun_angle$altitude
#night and day
sl.depth <- sl.depth %>% #mutate(daynight = if_else(sun_altitude < 6, "Night", "Day"))
                          mutate(ToD = case_when(sun_altitude > 6 ~ "Day",
                               sun_altitude <= 6 & sun_altitude > -6 ~ "Twilight",
                               sun_altitude <= -6 ~ "Night"))

sl.depth %>% mutate(daynight = if_else(sun_altitude < 6, "Night", "Day")) # used for rle()

#calculate moving average
ma <- function(x, n =150){stats::filter(x, rep(1 / n, n), sides = 2)}
sl.depth$depth_ma <- ma(sl.depth$Depth_metres)

sl.depth %>% filter(ToD %in% "Day") %>% 
  ggplot(., aes(x=datetime, y=Depth_metres)) + 
  geom_point(alpha=0.2, color="grey42") + 
  geom_line(aes(x=datetime, y=depth_ma), inherit.aes = F, linetype="solid", color = "red", size=1) +
  xlab("Date") + ylab("Max SL depth [m]")  +
  theme_minimal()

#difference between day and night signal

#try to average by migration event. 

#run length encode based on day/night cycle
sl.depth <- sl.depth %>%
  mutate(newday = rle(sl.depth$daynight)$lengths %>% {rep(seq(length(.)), .)})


#snapshots
sl.depth  %>% filter(Index %in% c(30000:35000)) %>% 
  ggplot(., aes(x=datetime, y=Depth_metres, color=ToD)) + geom_point() + scale_y_reverse() +
  theme_minimal()

sl.depth  %>% filter(Index %in% c(30000:35000)) %>% 
  ggplot(., aes(x=datetime, y=Depth_metres, color=factor(newday))) + geom_point() + scale_y_reverse() +
  theme_minimal()


sl.depth %>% filter(Index %in% c(20000:40000)) %>% 
  ggplot(., aes(x=datetime, y=Depth_metres, color=ToD)) + geom_point() + scale_y_reverse() +
  theme_minimal()


sl.depth %>% group_by(newday, ToD) %>% 
  summarise(Depth_mean = mean(Depth_metres)) %>% 
  filter(ToD %in% c("Day", "Night")) %>% 
  ggplot(., aes(x=newday, y=Depth_mean, color=ToD)) + geom_point()









