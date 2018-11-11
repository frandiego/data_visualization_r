#####  clean up the environment & garbage collecor
rm(list = ls())
gc()

#####  load packages if installed, if not, install and load it
packages <- c('data.table','ggplot2','purrr','scales','dplyr',
              'forcats','ggridges')
invisible(sapply(packages, function(x) { 
  if (x %in% rownames(installed.packages())){
    library(x,character.only = T)
  }else{
    install.packages(x)
    library(x)
  }
}))
  

# EXPLORATORY VS EXPLANATORY PLOTS ---------------------------------------------

#####  read dataset
dt <- fread('data/weather_madrid_2017.csv',stringsAsFactors = F)

# clean data
dt[,date:=as.Date(date)]
dt[,c('icon','summary') := map(.SD,as.factor),.SDcols = c('icon','summary')]

# filter out night hours
dt <- dt[as.integer(hour)>=7& as.integer(hour)<=23] 

# calculate month and coerce it into a factor
dt[,month := lubridate::month(date)]
dt[,month := factor(month,levels = 1:12,labels = month.name)]

# looking at the data
dplyr::as_tibble(dt)
str(dt)
# quick descriptive analysis
dt[,as.list(summary(apparentTemperature)), by=month][order(-month)]

# exploratory plot
ggplot(dt,aes(y = apparentTemperature, 
              x = factor(month))) + 
  geom_boxplot() + labs(title = 'apparent temperature \nMadrid 2017')


# violin plot
ggplot(dt,aes(y = apparentTemperature, 
              x = factor(month))) + 
  geom_violin() + labs(title = 'apparent temperature \nMadrid 2017')

# violin plot with median
ggplot(dt,aes(y = apparentTemperature, 
              x = factor(month))) + 
  geom_violin() + labs(title = 'apparent temperature \nMadrid 2017')+
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "black")


# exploratory plot
# fct_rev function is used the flip the order of the levels in a factor 
ggplot(dt,aes(y = apparentTemperature, 
              x = fct_rev(month))) + 
  geom_boxplot() + labs(title = 'apparent temperature \nMadrid 2017') +
  coord_flip()


# explanatory plot
ggplot(dt,aes(x = apparentTemperature, 
              y = fct_rev(month))) + 
  geom_density_ridges2(fill='black',color = 'white',size = 1,scale=2) + 
  theme_minimal(base_size = 15) + 
  theme(axis.title = element_blank(),
        plot.title = element_text(family='Verdana',hjust = 0.5)) + 
  labs(title = 'Apparent Temperature (Celsius) \nMadrid 2017')

# joy division plot
ggplot(dt,aes(x = apparentTemperature, 
              y = fct_rev(month))) + 
  geom_density_ridges2(fill='black',color = 'white',size = 1,scale=2) + 
  theme_void(base_size = 15) + 
  theme(axis.title = element_blank(), 
        plot.background = element_rect(fill='black'))



# TIDY_DATA ---------------------------------------------------------------

##### READ DATA
# read the billboard dataset and take a look at it
dt <- fread('data/billboard.csv')
dt

##### MELT THE DATASET
# take the names of the columns we want to melt
week_column_names <- names(dt)[grepl('.week$',names(dt))]
# store the rest of the column names because they are going to be id.vars
id_column_names <- setdiff(colnames(dt),week_column_names)
# melt the dataset using melt() function from data.table package
dt_melted <- melt(dt,id.vars = id_column_names,measure.vars = week_column_names)
dt_melted

##### FILTER NAs
# remove those rows with no value (because the song left the list before the week 76)
dt_melted <- dt_melted[!is.na(value)]
dt_melted

##### QUICK EXPLORATION
# to know what we have to do next
str(dt_melted)

##### TWO VARIABLE IN THE SAME COLUMN (column 'track')
# separate the track name from the year (using tidyr package)
dt_melted <- tidyr::separate(dt_melted,track,sep=' - ', into = c('track','year'))
# remove year variable (it is redundant)
dt_melted[,year := NULL]
dt_melted

##### TWO VARIABLE IN THE SAME COLUMN (column 'time')
# separate the track name from the year (using tidyr package)
dt_melted <- tidyr::separate(dt_melted,time,sep=':', into = c('minutes','seconds'))
# create variable duration (we need to know that a minute has 60 seconds)
dt_melted[,duration_min := as.numeric(minutes) + as.numeric(seconds)/60]
# remove redundants variable
dt_melted[,c('time','minutes','seconds') := NULL]
dt_melted

##### CLEAN VARIABLE WITH THE INFORMATION OF THE NUMBER OF THE WEEK
# using gsub function and regular expressions
dt_melted[,week := gsub('[^0-9]','',variable)]
dt_melted[,week := as.integer(week)]
# remove variable colum
dt_melted[,variable := NULL]
dt_melted

##### CHANGE THE NAMES OF SOME COLUMNS
setnames(x = dt_melted,
         old = c('artist.inverted','date.entered','date.peaked'),
         new = c('artist','date','date_peaked'))
dt_melted

##### CHANGE THE DATA TYPES
factor_variables <- c('genre')
character_variables <- c('artist','track')
date_variables <- c('date','date_peaked')
integer_variable <- c('value','week')
numeric_variables <- c('duration_min')

# using a tric of data.table
dt_melted[,c(factor_variables) := map(.SD,as.factor),.SDcols = c(factor_variables)]
dt_melted[,c(character_variables) := map(.SD,as.character),.SDcols = c(character_variables)]
dt_melted[,c(date_variables) := map(.SD,as.Date),.SDcols = c(date_variables)]
dt_melted[,c(integer_variable) := map(.SD,as.integer),.SDcols = c(integer_variable)]
dt_melted[,c(numeric_variables) := map(.SD,as.numeric),.SDcols = c(numeric_variables)]
str(dt_melted)

##### DATE VARIABLE IS A STATIC VARIABLE
head(dt_melted[artist == 'Santana' & track == 'Maria, Maria'],10)

##### MAKE DATE A DYNAMIC VARIABLE
# number of days to add
dt_melted[,n_days_to_add := (week -1)*7]
# coerce into date
dt_melted[,date:=date + lubridate::days(n_days_to_add)]
# remove auxiliary variable
dt_melted[,n_days_to_add := NULL]
head(dt_melted[artist == 'Santana' & track == 'Maria, Maria'],10)


##### STATIC AND DYNAMIC VARIABLE
static_variables <- c('artist','track','genre','duration_min','date_peaked')
dynamic_variable <- c('date','value','week')
setcolorder(dt_melted,c(static_variables,dynamic_variable))
head(dt_melted[artist == 'Santana' & track == 'Maria, Maria'],10)

##### TWO OBSERVATIONAL UNITS IN THE SAME TABLE
head(dt_melted[artist == 'Santana' & track == 'Maria, Maria'],10)

##### CREATE A DATASET
# first we need an id per track (data.table GRP function)
dt_melted[,track_id := .GRP,by =c('track','artist')]
# create two separate tables
track <- dt_melted[,c('track_id',static_variables),with=F]
rank <- dt_melted[,c('track_id',dynamic_variable),with=F]
# remove duplicates
track <- unique(track)
rank <- unique(rank)
# look at inside
head(track)
head(rank)

##### STORE_TABLE IN LIST
billboard <- list()
billboard[['track']] <- track
billboard[['rank']] <- rank

##### WHAT IS INSIDE THE LIST
billboard$track
billboard$rank

