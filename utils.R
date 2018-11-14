# package management
is_installed <- function(pck){
  pck %in% rownames(installed.packages())
}

require_install <- function(pck){
  if(!is_installed(pck)){install.packages(pck)}
  require(package = pck, character.only = T, quietly = T)
}

require_install <- Vectorize(FUN = require_install,
                             vectorize.args = 'pck',
                             SIMPLIFY = T,
                             USE.NAMES = F)

violin_range <- function(dt,y,x){
  
  factor_ = paste0('factor(',x,')')
  
  ggplot(dt,aes_string(y = y, 
                      x = factor_)) + 
    geom_violin() + labs(title = 'apparent temperature \nMadrid 2017')+
    stat_summary(fun.data="mean_sdl",  
                 fun.args = list(mult=1), 
                 geom="pointrange", color = "black") + 
    theme_classic()+
    labs(x = x)
}
