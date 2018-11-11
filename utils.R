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