
# EMPTY SHINYDASHBOARD ----------------------------------------------------

# Set the header, sidebar and body
header <- dashboardHeader()
sidebar <- dashboardSidebar()
body <- dashboardBody()
# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header,sidebar,body)
# Create a server function that do nothing
server <- function(input, output) {}
# Run the APP
shinyApp(ui, server)

# EMPTY MESSAGES MENU----------------------------------------------------
header <- dashboardHeader(dropdownMenu(type = 'messages'))
ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# ADD A MESSAGE ----------------------------------------------------
header <- dashboardHeader(
  dropdownMenu(type = 'messages', 
               messageItem(
                 from = 'ie', icon = shiny::icon('university'),message = 'keep studying')
  ))

ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# EMPTY NOTIFICAITONS MENU----------------------------------------------------
header <- dashboardHeader(dropdownMenu(type = 'messages'),
                          dropdownMenu(type='notifications'))
ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# ADD A NOTIFICAITON ----------------------------------------------------
header <- dashboardHeader(
  dropdownMenu(type = 'messages') ,
  dropdownMenu(type = 'notifications',
               notificationItem(
                 text = '100 new users today', icon = shiny::icon('thumbs-up'),status = "success" )))

ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# EMPTY TASKS MENU----------------------------------------------------
header <- dashboardHeader(dropdownMenu(type = 'messages'),
                          dropdownMenu(type = 'notifications'),
                          dropdownMenu(type = 'task'))
ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# ADD A TASK ----------------------------------------------------
header <- dashboardHeader(
  dropdownMenu(type = 'messages') ,
  dropdownMenu(type = 'notifications'),
  dropdownMenu(type = 'tasks',
               taskItem(text = 'check emails',value = 25, color = 'orange')))

ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)


# ADD A TITLE ----------------------------------------------------
header <- dashboardHeader(title = 'ShinyDashboard',
  dropdownMenu(type = 'messages') ,
  dropdownMenu(type = 'notifications'),
  dropdownMenu(type = 'tasks',
               taskItem(text = 'check emails',value = 25, color = 'orange')))

ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# ADD A MENUR IN THE SIDEBAR ----------------------------------------------------

sidebar <- shinydashboard::dashboardSidebar(
  sidebarMenu(
    menuItem(text = 'CEO Dashboard', tabName = 'ceo')
  )
)

ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# ADD NEW MENU IN THE SIDEBAR ----------------------------------------------------

sidebar <- shinydashboard::dashboardSidebar(
  sidebarMenu(
    menuItem(text = 'CEO Dashboard ', tabName = 'ceo'),
    menuItem(text = 'CFO Dashboard', tabName = 'cfo')
  )
)
ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)

# ADD SOMETHING IN THE BODY ----------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'ceo',h1('CEO Dashboard Tab')),
    tabItem(tabName = 'cfo',h1('CFO Dashboard Tab'))
  ) 
)  

ui <- dashboardPage(header,sidebar,body)
shinyApp(ui, server)
