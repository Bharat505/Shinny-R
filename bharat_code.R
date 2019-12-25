#loading libraries

library(shiny)
library(shinythemes)

#defining the primary UI using navbarpage
ui<-navbarPage("Statistical Analysis Assignment 1", theme = shinytheme("cerulean"),
               tabPanel("Import Data",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Select the read.table parameters below")),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(verbatimTextOutput("summ")
                                  ,verbatimTextOutput('summary'),
                                  verbatimTextOutput("dl"),
                                  uiOutput("table1")
                                  )
                        ) ),
               #UI for Binomial distribution
               tabPanel("Binomial",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show"),
                                     plotOutput("binom")))),
               #UI for normal distribution
               tabPanel("Normal",
                        sidebarLayout(sidebarPanel(
                          uiOutput("selectmod"),
                          uiOutput("val_n"),
                          uiOutput("norm_colm"),
                          uiOutput("var_n")),
                          
                          mainPanel( 
                                     verbatimTextOutput("norm_val"),
                                     plotOutput("norm"),
                                     plotOutput("unif"))
                                
                          
                          )
                        
                        )
)
#Defining Server
server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
})  
output$summary=renderText({
  summary(data())
})
output$summ=renderPrint(
  print("Summary")
)
output$dl=renderPrint(
  print("Data Loaded")
)
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
output$table1 <- renderUI({
  tableOutput("table")
})
output$model_select<-renderUI({
  sliderInput("n_value","Select value of n ",min = 1,max = 100,value = 50)
})

output$var1_select<-renderUI({
  selectInput("out_var_select","Select Probabilty  ", choices =as.list(names(data())))
})


output$rest_var_select<-renderUI({
  
  textInput("ind_var_select","Please Select the Value")
})
  dat <- reactive({
  variable <- data()[,input$out_var_select] 
  
  
  if(is.null(variable)){return()}
  })
output$other_val_show<-renderPrint({
  
  var1 <- data()[,input$out_var_select]
  if (is.null(var)){return(NULL)}
  
  mean1 <- unique(var1)
  abc=length(var1[var1==as.numeric(input$ind_var_select)])
  len=length(var1)
  print("Select the Value you wish to use for Binomial Distribution for")
  print(unique(var1))
  
  
  #print(mean1)
  variable <- data()[,input$out_var_select]
  #print(table(variable))
  library(caret)
  var <- reactive((data()[,input$out_var_select]))
  prob=abc/len
  print("Probability for this scenario")
  print(prob)
  
  
})
output$binom<-renderPlot({
  var1 <- data()[,input$out_var_select]
  if (is.null(var)){return(NULL)}
  var2 <- data()[,input$out_var_select]
  if (is.null(var)){return(NULL)}
  abc=length(var1[var1==as.numeric(input$ind_var_select)])
  len=length(var1)
  prob=abc/len
  hist(rbinom(len,input$n_value,prob), xlab = input$out_var_select, col = "yellow", border = "black")
})
###################################
output$val_n<-renderUI({
  sliderInput("n_value_norm","Select Value of n ",min = 1,max = 100,value = 50)
})
output$selectmod=renderUI({
  radioButtons("dist", "Distribution type:",
               c("Normal" = "norm",
                 "Uniform" = "unif",
                 "Log-normal" = "lnorm"
                 ))
})
output$norm_colm<-renderUI({
  selectInput("norm_col1","ColumnName  ", choices =as.list(names(data())))
})




output$norm_val=renderPrint({
  var1 <- data()[,input$norm_col1]
  if (is.null(var)){return(NULL)}
  
  mea=mean(var1)
  s_d=sd(var1)
  print("Mean for the selected row is")
  print(mea)
  print("Standard deviation for the selected row is")
  print(s_d)
})
output$norm <- renderPlot({
  d <- reactive({
    var1 <- data()[,input$norm_col1]
    if (is.null(var)){return(NULL)}
    
    mea=mean(var1)
    s_d=sd(var1)
    dist <- switch(input$dist,
                   norm = rnorm(input$n_value_norm,mean=mea,sd=s_d),
                   lnorm = rlnorm(input$n_value_norm,mean=mea,sd=s_d),
                   unif=runif(runif(input$n_value_norm,min=min(var1),max = max(var1)))
                   )
   
    
  })
  #ab=d()
  #plot(ab(input$n_value_norm,mean=mea,sd=s_d))
  hist(d(), xlab = input$norm_col1, col = "green", border = "blue")
  
})

}
shinyApp(ui=ui,server=server)