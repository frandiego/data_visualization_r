library(data.table)
library(ggplot2)
library(magrittr)
diamonds <- as.data.table(diamonds)
diamonds_filtered <- diamonds[carat<=3]

ggplot(diamonds_filtered,aes(carat,price)) + 
  geom_point(alpha=0.3,aes(color=factor(color)))+
  geom_smooth(method = 'lm')+
  scale_x_log10() + scale_y_log10()

ggplot(diamonds_filtered,aes(carat,price)) + 
  geom_point(alpha=0.3,aes(color=factor(color)))+
  geom_smooth(method = 'lm')+
  scale_x_log10() + scale_y_log10()+
  facet_wrap(~color)

# LET’S MAKE A LINEAR MODEL AND EVALUATE HOW IT WORKS PER COLOR
diamonds_filtered %>% lm(formula = log(price)~log(carat))->linear_model
diamonds_filtered[,c('fit','resid'):=list(linear_model$fitted.values,
                                          linear_model$residuals)]
summary_model_color <- diamonds_filtered[,.(fit=mean(fit),sd=mean(resid)),by=color]
summary_model_color

# CROSSBAR
ggplot(summary_model_color,aes(color,fit,ymin=fit-sd,ymax=fit+sd))+
  geom_crossbar(aes(fill=color))

ggplot(summary_model_color,aes(color,fit,ymin=fit-sd,ymax=fit+sd))+
  geom_errorbar(aes(color=color),size = 2)

ggplot(summary_model_color,aes(color,fit,ymin=fit-sd,ymax=fit+sd))+
  geom_linerange(aes(color=color),size = 2)

ggplot(summary_model_color,aes(color,fit,ymin=fit-sd,ymax=fit+sd))+
  geom_pointrange(aes(color=color),size = 2)
