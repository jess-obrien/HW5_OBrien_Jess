#Call libraries in
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(DT)

#Read in data and clean
oz<-read.csv('Michigan Ozone Levels 2020.csv')
ast<-read.csv('Childhood Asthma.csv')

#Ozone by year
mi_oz<-oz %>% filter (COUNTY %in% c("Wayne", "Ingham", "Benzie", "Schoolcraft")) #%>% arrange(dates) #Filter data to only the four desired counties and sort by the date the measurement was taken
dates<-as.Date(mi_oz$Date, "%m/%d/%Y")

#Table of maximum ozone levels in 2020, with county
oz_tab<-data.frame(oz %>%
                     arrange(desc(Daily.Max.8.hour.Ozone.Concentration)) %>%
                     slice(1:25)) 
colnames(oz_tab)[5]<-'Maximum Ozone Concentration'

#Childhood Asthma by year
ast$Value<-as.numeric(sub("%", "", ast$Value))

ui <- dashboardPage(
  
  # format
  skin="purple",
  
  # define the title
  dashboardHeader(
    title="Michigan Air Quality Indicators",
    titleWidth = 500
  ),
  
  # define the sidebar
  dashboardSidebar(
    # set sidebar menu  
    sidebarMenu(
      menuItem("Ozone Levels", tabName = "Ozone"),
      menuItem("Yearly Asthma Rates", tabName = "ast_yr"),
      menuItem("Top 25 Ozone Levels, 2020", tabName = "tab")
    )
  ),
  
  # define the body
  dashboardBody(
    tabItems(
      # first page
      tabItem("Ozone",
              h2("Ozone Levels in Selected Michigan Counties, 2020"),
              box(plotlyOutput("oz"), width= 500)),
      # second page
      tabItem("ast_yr",
              h2("Childhood Asthma by Year"),
              box(plotlyOutput("ast_yr"), width=500)),
      #third page
      tabItem("tab",
              h2("Top 25 Ozone Levels, 2020"),
              tableOutput("tab"),
      )
    )
  )
)

server <- function(input, output) {
  
  # --------------------------------------------------
  # Ozone Levels in 2020
  # --------------------------------------------------
  output$oz <- renderPlotly({
    plty<- plot_ly(mi_oz,
                   x=~dates,
                   y=~Daily.Max.8.hour.Ozone.Concentration,
                   color=~COUNTY,
                   type='scatter',
                   mode='lines'
    )
    plty<-plty%>%rangeslider()%>%layout(title="Maximum Ozone Levels in Michigan Counties, 2020",legend=list(title=list(text='County')),
                                        xaxis=list(title="Date"),
                                        yaxis=list(title="Maximum Ozone Reading \n in parts per million"))
    plty<-plty%>%layout(hovermode="x")
    plty})
  
  # --------------------------------------------------
  # table of top 25 Ozone levels and county in 2020
  # --------------------------------------------------
  output$tab <- renderTable(select(oz_tab, Date, COUNTY, 'Maximum Ozone Concentration'))
  
  
  
  # --------------------------------------------------
  # Plot of yearly asthma rates by age group
  # --------------------------------------------------
  output$ast_yr <- renderPlotly({
    ast_yr<- plot_ly(ast%>%filter(State=="Michigan"),
                     x=~Year,
                     y=~Value,
                     color=~Age.Group,
                     type='scatter',
                     mode='lines'
    )
    ast_yr<-ast_yr%>%rangeslider()%>%layout(title="Rate of Childhood Asthma in Michigan",legend=list(title=list(text='Age Group')),
                                            xaxis=list(title="Year"),
                                            yaxis=list(title="% Population with Asthma"))
    ast_yr<-ast_yr%>%layout(hovermode="x")
    ast_yr})
  
  
}

shinyApp(ui, server)