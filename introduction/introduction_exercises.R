
# INTRO -------------------------------------------------------------------


#####  clean up the environment & garbage collecor
rm(list = ls())
gc()
source('utils.R')
#####  load packages if installed, if not, install and load it
packages <- c('data.table','ggplot2','purrr','scales','dplyr',
              'forcats','ggridges','lubridate','Hmisc','tidyr')
require_install(pck = packages)



# The data is a subset of the data contained in the World Health 
# Organization Global Tuberculosis Report

# creaate a list with all datasets which csv name start with tuberculosis...
ls_data <- list.files('data')
tb_data <- ls_data[grepl('tuberculosis',ls_data)]
tub <- sapply(file.path('data',tb_data),function(x) fread(x))
names(tub) <- gsub('tuberculosis_|.csv$','',tb_data)


tidy_data <- tub$tidy


# COLUMNS INSTEAD OF ROWS -----------------------------------------------
dt_pop <- tub$col_values_population
dt_cas <- tub$col_values_cases

# change the column names. The column names are stored in the firs row.
colnames(dt_pop) <- as.character(dt_pop[1,])
dt_pop <- dt_pop[-1]
colnames(dt_cas) <- as.character(dt_cas[1,])
dt_cas <- dt_cas[-1]

##### tidyverse way
dt_pop %>% 
  gather(year,value,'1999':'2000') %>% 
    rename(population = value) -> dt_pop_melted

dt_cas %>% 
  gather(year,value,'1999':'2000') %>% 
  rename(cases = value)  -> dt_cas_melted

dt_tidy <- dt_cas_melted %>% inner_join(dt_pop_melted,by=c('country','year'))
dt_tidy <- dt_tidy %>% mutate(year = as.integer(year)) %>% arrange(country,year)

## is equal to tidy_data
all(as.matrix(dt_tidy) == as.matrix(tidy_data))

##### data.table way
dt_pop_melted <- melt(dt_pop,id.vars = 'country',
                      variable.name = 'year',
                      value.name = 'population')

dt_cas_melted <- melt(dt_cas,id.vars = 'country',
                      variable.name = 'year',
                      value.name = 'cases')
dt_tidy <- merge(dt_pop_melted,dt_cas_melted)
setcolorder(dt_tidy,colnames(tidy_data))
## is equal to tidy_data?
all(as.matrix(dt_tidy) == as.matrix(tidy_data))


# ROWS INSTEAD OF COLUMNS -----------------------------------------------
dt <- tub$mult_rows
##### tidyverse way
dt_tidy <- spread(dt,key,value)
## is equal to tidy_data?
all(as.matrix(dt_tidy) == as.matrix(tidy_data))

##### data.table way
dt_tidy <- dcast(dt,country+year~key)
## is equal to tidy_data?
all(as.matrix(dt_tidy) == as.matrix(tidy_data))

# MULTIPLES VALUES -----------------------------------------------
dt  <- copy(tub$mult_values)
##### tidyverse way
dt_tidy <- separate(dt,col='rate',
                    into = c('cases','population'),
                    sep = '/',
                    convert = T) 
## is equal to tidy_data?
all(as.matrix(dt_tidy) == as.matrix(tidy_data))

##### data.table way
dt_tidy <- copy(dt)
dt_tidy[,c('cases','population') := tstrsplit(rate,'/',type.convert = T,fixed=T)]
dt_tidy[,rate:=NULL]
## is equal to tidy_data?
all(as.matrix(dt_tidy) == as.matrix(tidy_data))

# MULTIPLES COLUMNS -----------------------------------------------
dt <- copy(tub$mult_columns)


##### tidyverse way
dt %>% mutate(year = str_pad(year,width = 2,side='left',pad='0')) %>% 
       unite('year',c('century','year'),sep='') %>% 
       mutate(year=as.integer(year)) %>% 
       separate('rate',c('cases','population'),convert = T) -> dt_tidy

## is equal to tidy_data?
all(as.matrix(dt_tidy) == as.matrix(tidy_data))

##### data.table way
dt <- copy(tub$mult_columns)
dt[,year:= str_pad(string = year,width = 2, side = 'left',pad = '0')]
dt[,year := paste0(century,year)]
dt[,year:=as.integer(year)]
dt[,century := NULL]
dt <- separate(dt,'rate',c('cases','population'),convert = T)
dt[,country:=as.factor(country)]
## is equal to tidy_data?
all(as.matrix(dt) == as.matrix(tidy_data))






