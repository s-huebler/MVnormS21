#App

#Loading the dependencies
library(shiny)
library(shinyjs)

#User interface ----

ui<-fluidPage(

  #Aesthetics ----

  inlineCSS(list(
    ".round"="border: solid rgba(255, 99, 71, 0.3);
           border-radius: 50px;
           padding: 15px;",
    ".red"="text-color: red;"
    )),

  style = "
  font-size:20px;
  font-family:times;
  ",



  #Title ----
  titlePanel("Interactive Multivariate Assumptions Checks"
            ),

  #Choose Data ----
  fluidRow(
    column(3, class="round",
      radioButtons("inRow1",
                   "Data Upload/Download",
                   choices=c("a", "b", "c"))
    ),
    column(9,
      "Data Head",
      uiOutput("row1", class="pastel")
    )
  ), #End first row

  #Tabs for different tasks

  tabsetPanel(

    #Task 1 tab ----
    tabPanel( "Task 1",
    fluidRow(
    column(3,class="round",
      radioButtons("inRow1",
                   "Select Task 1 Input",
                   choices=c("d", "e", "f"))
    ),
    column(9,
      " Task 1 Output",
      uiOutput("task1")
    )
    ) #End fluid row
    ), #End tab 1


    #Task 2 tab ----
    tabPanel( "Task 2",
      fluidRow(
      column(3,class="round",
             radioButtons("inRow2",
                          "Select Task 2 Input",
                          choices=c("g", "h", "i"))
      ),
      column(9,
             " Task 2 Output",
             uiOutput("task2")
      )
    )#End fluid row
  ) #End tab 2

  ) #End tabset panels
) #End fluid page

#Server ----

server<-function(input, output, session){

#Data Table ----
output$row1<-renderUI({
  HTML("dataTable")
})

#Task 1 Output----
output$task1<-renderUI({
  HTML("test1")
})

#Task 2 Output ----
output$task2<-renderUI({
  HTML("test2")
})

}

#Run the app ----

shinyApp(ui=ui, server=server)
