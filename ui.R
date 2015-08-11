

shinyUI(navbarPage(
  title='Twitter Topic Extractor'
,inverse =T,collapsable =T,
  
  tabPanel("",
    fluidRow(
      column(3,numericInput("K",'Number of topics',value=10,min=2,max=NA))
                 ),
    fluidRow(
      column(12, plotOutput("g")),
      column(12, dataTableOutput('tabl'))
             
                      ))))
        