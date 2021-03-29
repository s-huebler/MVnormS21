#App

#Loading the dependencies
library(shiny)
library(shinyjs)
library(dplyr)
library(stats)
library(ggplot2)
source("functions.R")

# Built in example dataset ----

four<-read.csv("fourmeasure.csv")

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
           #Selecting the type of data (example or uploaded)
           selectInput("data",
                       "Select which dataset to use",
                       choices = c("Example_Four_Measure", "Upload_Your_Own"),
                       selected = '"'),


           #Reading in the file
           fileInput("file", NULL, accept = c(".csv", ".XLS", ".xls"))
           ),

    column(9,
      "Data",
      uiOutput("row1", class="pastel"),
      tableOutput("dataHead")
    )
  ), #End first row

  #Tabs for different tasks

  tabsetPanel(

    #Task 1 tab ----
    tabPanel( "Task 1",
    fluidRow(
    column(3,class="round",
           radioButtons("vars1",
             "Select variable ",
              choices = c("x1", "x2", "x3", "x4", "dsq"))
    ),
    column(9,
      uiOutput("propExp"),
      plotOutput("propPlot"),
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

  # Read in data ----
  up<-reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = read.csv(input$file$datapath),
           XLS = read_excel(input$file$datapath),
           xls = read_excel(input$file$datapath),
           validate("Invalid file; Please upload a .csv or .xls file")
    )
  })

  dat<-reactive({
    if(input$data=="Example_Four_Measure"){
      four
    }else{
      as.data.frame(up())
    }
  })

  datNum <- reactive({
    dat() %>%
    select(where(is.numeric))
  })




#Data Table ----
output$dataHead<-renderTable({
  head(dat(),5)
})

#Task 1 Buttons Update ----
  newNames<-reactive({
    names(datNum())
  })

  #Updating x variable
  observe({

    temp<-as.vector(newNames())
    updateRadioButtons(session, "vars1", choices=temp)

  })

#Task 1 Output----

output$propExp <- renderUI({

  title <- "Proportion Test"

  exp <- paste(
    "The proportion test will give information about the tails of the distribution.
    If significantly more data points than expected under the normal distribution are
    outside the range of 1 or 2 standard deviations from the mean, the tails will be
    too thick and the test will
    fail. Similarly, if significantly fewer data points than expected under the
    normal distribution are outside the range of 1 or 2 standard deviations from the
    mean, the tails will be too skinny and the test will fail.", sep= " "
  )

  HTML( "<span style='font-size:150%'>", title, "</span>","<br>" ,
        "<span style='font-size:80%'>", exp)

})

v <- reactive({
  input$vars1
})

output$propPlot <- renderPlot({

  df <- datNum()
  v<-v()

  if(input$data=="Upload_Your_Own"){
    req(input$file)
  }

  if(is.element(v, newNames())){


  var <- df[,v]
  propPlot(var)
  }
else{

  blank<-data.frame("1"=c(1:10), "2"=c(1:10))
  ggplot(data=blank, aes(x=1, y=2))+
    theme_void()
}

})

output$task1 <- renderUI({

  v<-v()

  if(input$data=="Upload_Your_Own"){
    req(input$file)
  }

  if(is.element(v, newNames())){


  df <- datNum()
  var <- df[,input$vars1]

  test <- propTest(var)

  a <- paste(
    paste("The proportion test result for",
          input$vars1, "is",
          test$Result),
    paste("The proportion of values within 1 standard deviation for",
          input$vars1, "is",
          test$PropOne),
    paste("The proportion of values within 2 standard deviations for",
          input$vars1, "is",
          test$PropTwo),
    sep=" <br> "
  )


  HTML("<span style='font-size:80%'>",
    a)

}

})



#Task 2 Output ----
output$task2<-renderUI({
  HTML("test2")
  HTML("test2.2")
})

}

#Run the app ----

shinyApp(ui=ui, server=server)
