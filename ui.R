library(shiny)
library(shinydashboard)

#------------------------------------------------------------------------------
header <- dashboardHeader(
  title = tags$span('Shiny Chat', tags$sup('alpha'))
)

#------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  disable = TRUE,
  sidebarMenu(id = "tabs", menuItem("Chat", tabName = "chatroom"))
)

#------------------------------------------------------------------------------
dash <- tabItem(
  tabName = 'chatroom',
  fluidRow(
    box(title = 'Chat room', width = 8, height = 620,
        uiOutput("chat"), tags$hr(),
        column(11, textInput("entry", "")),
        column(1, tags$br(), actionButton("send", '', icon = icon("send"))),
        selectInput('opcoes', '', '', multiple = TRUE)
    ),
    
    box(width = 4, background = 'olive',
        textInput("user", "Your User ID:", value=""),
        tags$hr(),
        h5("Connected Users"),
        uiOutput("userList")
    )
  )
)

bod <- dashboardBody(tabItems(dash))

#------------------------------------------------------------------------------
dashboardPage(header, sidebar, bod, skin = 'green', title = 'shinyChat')
