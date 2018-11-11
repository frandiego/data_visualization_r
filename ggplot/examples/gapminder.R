packages <- c('gapminder','gganimate','scales','ggplot2','dplyr','scales')
invisible(sapply(packages, function(x) ifelse(x%in%rownames(installed.packages()),
                            library(x,character.only = T),{install.packages(x);
                              library(x,character.only = T)})))
ggdata <- filter(gapminder,
                 !country %in% c('Kuwait','Saudi Arabia'))
p <- ggplot(ggdata, aes(x=gdpPercap,
                        y=lifeExp,
                        size = pop,
                        frame = year,
                        color=factor(continent))) +
  geom_point() +
  geom_smooth(method = "loess", 
              show.legend = FALSE,
              se=F, 
              color='black',
              size=2,
              span=1) +
  scale_size(range = c(1,15),
             name = 'Population',labels = comma)+
  scale_color_discrete(name = 'Continent') +
  labs(y='Life Expectancy', 
       x = 'GDP Per Capita   Logarithmic scale 
       \nUS$ (inflation-adjusted)') +
  scale_x_log10()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
gganimate(p,interval = .5)