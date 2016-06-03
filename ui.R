library(shiny)

source("helper.R")
Levels <- factor(c(1,2,3,4,5,6))


shinyUI(fluidPage(
  title = "Assessment: Data Management Application",
  shinyjs::useShinyjs(),
  tags$head(includeCSS(file.path("www", "app.css"))),
  
  div(
    id = "titlePanel",
    "Assessment: Data Management Application"
  ),
  
  column(12, 
         wellPanel(
           
           tabsetPanel(
             id = "mainTabs", type = "tabs",
             
             tabPanel(
               title = "Data Input", id = 'datainput', value = 'datainput',
               br(),
               fluidRow(
                 column(3,
                        textInput("text", label = 'Table name', value = ""),
                        fileInput('file1', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        tags$hr(),
                        checkboxInput('header', 'Header', TRUE),
                        radioButtons('sep', 'Separator',
                                     c(Comma=',',
                                       Semicolon=';',
                                       Tab='\t'),
                                     ','),
                        radioButtons('quote', 'Quote',
                                     c(None='',
                                       'Double Quote'='"',
                                       'Single Quote'="'"),
                                     '"'),
                        actionButton("submit", "Submit", class = "btn-primary")
                 ),
                 column(
                   br(),
                   DT::dataTableOutput('contents'), width = 6)
               )),
           
           tabPanel(
             title = "Progression Summary", id = "progsum", value = "progsum",
             br(),
             fluidRow(
               column(4,
                      id = "leftPanel1",
                      div(
                        id = "assessTypePanel",
                        selectInput("sum", "Select Summary Table", sum_types)
                      ),
                      br(),
                      DT::dataTableOutput("progressionTable"),
                      br(),
                      DT::dataTableOutput('passfailTable')),
               column(
                 div(
                   id = "levelTypePanel",
                   selectInput("level", "Select Level", c("All",Levels))
                 ),
                 br(),
                 plotOutput("dist"),
                 br(),
                 plotOutput('desc'), width = 6))

           ),
           

           tabPanel(
             title = "Test Summaries", id = "testsums", value = "testsums",
             br(),
             fluidRow(
               column(6,
                      id = "leftPanel2",
                      div(
                        id = "skillTypePanel",
                        selectInput("courseLevel", "Select Course", choices = c('Make a choice', 'Grammar', 'Reading', 'Writing Adv', 'Writing Int', 'Writing Basic'), selected = 'Make a choice'))
                      ),
               column(6,
                      id = 'panel3',
                      div(
                        id = 'testSelect',
                        uiOutput('course')
                      )
               ),
              br(),
              column(6,
              DT::dataTableOutput('testsumTable'),
              br(),
              downloadButton('downloadSum', 'Download Summary'),
              br(),
              br(),
              downloadButton('downloadAdvWrite', 'Download Adv. Writing Summary')),
              column(6,
              plotOutput('stem'),
              br(),
              downloadButton('downloadPlot', 'Download Plot'))
              )
             ),
           
           tabPanel(
             title = "Test Analysis", id = "testana", value = "testana",
             br(),
             fluidRow(column(5,
                             id = "leftPanel8",
                             div(
                               id = "skillTypePanel",
                               selectInput("courseLevels", "Select Course", choices = c('', 'Grammar', 'Reading', 'Writing Adv', 'Writing Int', 'Writing Basic', 'Listening/Speaking'), selected = ''))
                      ),
                      column(5,
                             id = 'panel9',
                             div(
                               id = 'testSelect',
                               uiOutput('courseS')
                             )
                      ),
                      br(),
                      column(5,
                      DT::dataTableOutput("item_analysisTable"),
                      br(),
                      downloadButton("downloadBtnAna", "Download item analysis"),
                      br(),
                      br(),
                      downloadButton("downloadRaterAna", "Download rater analysis")),
                      column(5,
                      DT::dataTableOutput("OBJ_analysisTable"),
                      br(),
                      downloadButton("downloadrubricOBJ", "Download rubric analysis"),
                      br(),
                      br(),
                      downloadButton('downloadOBJ', 'Download obj summary'))
             ))
           
           )
           
      ))
))


