################################################################################   DEPENDENCIES ------------
############################################################  PACKAGES ----------------------------------------------------------------
packages <- c('shinydashboard','shiny','data.table','tidyquant','dplyr',
              'tbl2xts','dygraphs','TTR','magrittr','maps',
              'ggmap','plotly', 'leaflet')
invisible(sapply(packages, function(x) 
  library(x,character.only = T,verbose = F,warn.conflicts = T)))


############################################################  DATA -------------------------------------------------------------

path_symbols <- file.path('../data/ttr_stock_symbols.csv')
symbols <- fread(path_symbols)
set_tickers <- unique(symbols$Symbol)
set_names <- unique(symbols$Name)

################################################################################   USER INTERFACE ------------
############################################################ STOCKS ------------

############################## SELECT INPUT TICKERS
select_input_ticker <-  selectInput(inputId = 'id_ticker',
                                    choices = set_tickers,
                                    selected = 'AAPL',
                                    label = 'Ticker',
                                    multiple = F,
                                    selectize = T)

############################## SELECT INPUT NAMES
select_input_name <-    selectInput(inputId = 'id_name',
                                    choices = set_names,
                                    selected = 'Apple Inc.',
                                    label = 'Name',
                                    multiple = F,
                                    selectize = T)


############################## DATE RANGE
date_range <-          dateRangeInput(inputId = 'id_daterange', 
                                      label = 'Date Range', 
                                      start = as.Date('2010-01-01'), 
                                      end = as.Date(today()),
                                      format = "yyyy-mm-dd",
                                      startview = 'year',
                                      language = 'en',
                                      weekstart = 1,
                                      separator = ' - ')

############################## DATE CHECKBOX
checkbox_date <-       radioButtons(inputId = 'id_checkbox_date',
                                    label = 'Lag',
                                    choices  = list('1 month','3 months', '1 year', 'max'),
                                    selected = '1 year')


######################################## STOCKS SIDEBAR
sidebar_stock_menu <-   sidebarMenu(id = "sidebar_stock_menu",
                                     menuItem(text = "Stocks", icon = icon("bar-chart-o"),
                                              select_input_ticker,
                                              select_input_name,
                                              date_range,
                                              checkbox_date))


######################################## STOCKS BODY
body_stock_tab <- tabPanel('Stocks',           
                            dygraphOutput('dygraph_candlestick'),
                            dygraphOutput('dygraph_volume'))
                          

############################################################ GGPLOT ------------


############################## SELECT INPUT X

select_input_x <-       selectInput(inputId = 'id_ggplot_x',
                                   choices = names(mpg), 
                                   selected = 'displ',
                                   label = 'X Aesthetic',
                                   multiple = F,
                                   selectize = T)

############################## SELECT INPUT Y
select_input_y <-       selectInput(inputId = 'id_ggplot_y',
                                    choices = names(mpg), 
                                    selected = 'hwy',
                                    label = 'Y Aesthetic',
                                    multiple = F,
                                    selectize = T)

############################## SELECT INPUT COLOR

select_input_color <-   selectInput(inputId = 'id_ggplot_color',
                                    choices = names(mpg), 
                                    selected = 'year',
                                    label = 'Color Aesthetic',
                                    multiple = F,
                                    selectize = T)

############################## SELECT INPUT SIZE

select_input_size <-    selectInput(inputId = 'id_ggplot_size',
                                    choices = names(mpg)  , 
                                    selected = 'class',
                                    label = 'Size Aesthetic',
                                    multiple = F,
                                    selectize = T)


############################## MENU
sidebar_ggplot_menu <-   sidebarMenu(id = "sidebar_ggplot_menu",
                                    menuItem(text = "Ggplot", icon = icon("dashboard"),
                                             select_input_x,
                                             select_input_y,
                                             select_input_color,
                                             select_input_size))

######################################## BODY
body_ggplot_tab <-      tabPanel('Ggplot',
                              plotlyOutput('plotly_scatter',width = '100%',height = 700)
                              )


############################################################ HEADER  -----------
header <- dashboardHeader(title = 'ShinyDashboard Example')
############################################################ SIDEBAR  ----------
sidebar <- dashboardSidebar(
  sidebar_stock_menu,
  sidebar_ggplot_menu
)
############################################################ BODY --------------
body <- dashboardBody(
  tabBox(width = '100%',
         body_stock_tab,
         body_ggplot_tab
  )
 
)


############################################################ UI ---------------- 
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body, 
                    skin = 'black')

################################################################################   SERVER ------------

server <- function(input,output,session){
############################################################ STOCKS_TS ---------
  # REACTIVE FUNCTION TO GET STOCS DATA
  stock_ts <- reactive({
    ticker      <- tolower(input$id_ticker)
    date_min    <- as.Date(min(input$id_daterange))
    date_max    <- as.Date(max(input$id_daterange))
    
    tidyquant::tq_get(x = ticker) %>% 
      filter(between(date, date_min, date_max))
  })
############################################################ DATE WINDOW -------
  # FUNCTION TO GET THE DATE WINDOW (FOR THE RANGE SELECTOR)
  date_window <- reactive({
    lag <-     switch(input$id_checkbox_date,
                      '1 month' = months(1),
                      '3 months' = months(3),
                      '1 year' = years(1))
    date_window <-c(today() - lag  ,today()) 
    if(input$id_checkbox_date == 'max'){date_window = NULL}
    return(date_window)
  })
  
############################################################ DYGRAPH VOLUME ----
  output$dygraph_volume <- renderDygraph({
    stock_ts() %>% 
      select(date,volume) %>% 
        tbl_xts() %>% 
          dygraph(group = 'stock',main = 'Volume') %>% 
            dyRangeSelector(dateWindow = date_window()) %>% 
              dyLegend(show = "always", hideOnMouseOut = FALSE) %>% 
                dyOptions(fillGraph = TRUE, fillAlpha = 0.5) 
  })
############################################################ DYGRAPH CANDEL ----
  output$dygraph_candlestick <- renderDygraph({
    name = paste0(input$id_name, '\n',input$id_ticker)
    stock_ts() %>% 
      select(-volume,-adjusted) %>% 
        group_by(date) %>%
          mutate(mean = mean(open,high,low)) %>% 
            tbl_xts() %>% 
              dygraph(main = name) %>% 
                   dyLegend(show = "always", hideOnMouseOut = FALSE) 
        
  })
  
  
############################################################ GGPLOT ------------
  output$plotly_scatter <- renderPlotly({
    aes_color <- paste0('factor(',input$id_ggplot_color,')')
    aes_size   <- paste0('factor(',input$id_ggplot_size,')')
    geom <- ifelse(is.null(input$id_jitter),'geom_point()',
                   ifelse(input$id_jitter,'geom_jitter()','geom_point()'))
    size_factor <- ifelse(is.null(input$id_size_factor),'','scale_size_discrete(range = c(1,input$id_size_factor))')
    ggplot(data = mpg,
           aes_string(x = input$id_ggplot_x, 
                      y = input$id_ggplot_y, 
                      color=aes_color,
                      size=aes_size)) +
      theme_minimal()+
      eval(parse(text=size_factor))+
      eval(parse(text=geom))
      
  })
}
  

################################################################################ APP -----------
shinyApp(ui = ui,server = server)




