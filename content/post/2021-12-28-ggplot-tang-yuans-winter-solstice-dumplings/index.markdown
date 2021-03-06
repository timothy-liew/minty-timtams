---
title: ggplot 'Tang Yuans'/Winter Solstice Dumplings
author: Timothy Liew
date: '2021-12-28'
slug: ggplot-tang-yuans-winter-solstice-dumplings
categories: []
tags: [ggplot, visualisation]
---

The winter solstice is a time where Chinese folk celebrate the Dongzhi Festival, which essentially is a great excuse to get together with the family. One crucial tradition is the consumption of "Tang Yuan", which are balls of glutinous rice flour with sweet filling. 

![Tang Yuan](https://media.istockphoto.com/photos/glue-pudding-or-tangyuan-in-bowlchinese-lantern-festival-food-picture-id1299770844?b=1&k=20&m=1299770844&s=170667a&w=0&h=OiFyd-yUT6Cfyr95tR61yxRnpaZEGm0HHiRJEZFl4SM=)

Unfortunately, I didn't get the chance to have any Tang Yuan this winter solstice, so I decided to make my own using ggplot.

# Tang Yuan Galore

```r
library(tidyverse)
library(gganimate)
library(RColorBrewer)

# We use a function to create random data for the plot
tang_yuan <- function(n){
    x <- runif(n, min = 10, max = 100)
    y <- runif(n, min = 10, max = 100)
    z <- runif(n, min = 5, max = 250)
    df <- data.frame(x,y,z)

  return(df) 
}

# Let's use rdply to rerun the function. We also divide the dataframe based on how many separate colours we want
grp <- 12
dat <- plyr::rdply(.n = 30, .expr = tang_yuan(120)) %>% 
  mutate(color = rep(1:grp, length.out = nrow(.)))

# And now for the actual plotting
a <- ggplot(dat, aes(x = x, y = y, size = z, color = as.factor(color))) + 
  geom_point(show.legend = FALSE) + 
  # This gives us a "round" plot simulating a bowl
  coord_polar() + 
  labs(title = "Happy Winter Solstice!", subtitle = "Here are some tang yuans") + 
  theme_minimal() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  size  = 20, 
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 15,
                                     face = "italic")) +
  # Change the color palette
  scale_color_brewer(palette = "Pastel2") +
  # This adds some animation from gganimate
  transition_states(states = .n,
                    transition_length = 1,
                    state_length = 0) +
  enter_grow() + 
  exit_fade() +
  ease_aes('linear', interval = 0.001)

# More adjustments in the form of number of frames and frame rate
a <- animate(a,
             fps = 10,
             nframes = 200)

# And one final step
anim_save("tangyuan.gif", a)
```

![](tangyuan.gif)
