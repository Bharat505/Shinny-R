#CA1-Statistical Analysis using Shiny
#Group Members:
#Bharat Jethwani - 10519364 - Probability Distribution and Data Import
#Neeraj Chavan - 10521393 - GLM and App Design
#Aayush Jain - 1051891 - Hypothesis Testing & Descriptive Analytics
#Prasad Tambe - 10515513 - Overall Layout and Dashboard
#Githublink - https://github.com/Bharat505/Shinny-R

#loading libraries
library(shiny)
library(shinythemes)

#defining the primary UI using navbarpage
ui<-navbarPage("Statistical Analysis Assignment 1",  theme = shinytheme("cerulean"),
               tabPanel("Import Data",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Select the read.table parameters below")),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(verbatimTextOutput("summ"),
                                  verbatimTextOutput('summary'),
                                  verbatimTextOutput("dl"),
                                  uiOutput("table1")
                        )
                        ) ),
               #UI for Binomial distribution
               tabPanel("Binomial",
                        sidebarLayout(sidebarPanel(
                          uiOutput("nval"),
                          uiOutput("sel_col"),
                          uiOutput("value_col")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("displ"),
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
                        
                        
               ),
               tabPanel("FitModel",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show")))),
               tabPanel("plot",sidebarLayout(sidebarPanel(
                 uiOutput("plot_col1"),
                 uiOutput("plot_col2")
               ),
               mainPanel( 
                 plotOutput("ploo")))),
               
               tabPanel("Hypothesis",
                        sidebarLayout(sidebarPanel(
                          uiOutput("hypo_select"),
                          textInput("mu", "Enter the value of mu")),
                          mainPanel( h2("The decision is:"),
                                     verbatimTextOutput("decision"))))
               
               
)
#Defining Server
server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
})  
#Defining Outputs
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
output$nval<-renderUI({
  sliderInput("n_value","Select value of n ",min = 1,max = 100,value = 50)
})

output$sel_col<-renderUI({
  selectInput("sel_column","Select Probabilty  ", choices =as.list(names(data())))
})


output$value_col<-renderUI({
  
  textInput("inp_val_col","Please Select the value")
})
dat <- reactive({
  variable <- data()[,input$sel_column] 
  
  
  if(is.null(variable)){return()}
})
output$displ<-renderPrint({
  
  var1 <- data()[,input$sel_column]
  if (is.null(var)){return(NULL)}
  
  mean1 <- unique(var1)
  abc=length(var1[var1==as.numeric(input$inp_val_col)])
  len=length(var1)
  print("Select the value you wish to use for Binomial Distribution:")
  print(unique(var1))
  
  
  #print(mean1)
  variable <- data()[,input$sel_column]
  #print(table(variable))
  library(caret)
  var <- reactive((data()[,input$sel_column]))
  prob=abc/len
  print("Probability for this scenario")
  print(prob)
  
  
})
output$binom<-renderPlot({
  var1 <- data()[,input$sel_column]
  if (is.null(var)){return(NULL)}
  var2 <- data()[,input$sel_column]
  if (is.null(var)){return(NULL)}
  abc=length(var1[var1==as.numeric(input$inp_val_col)])
  len=length(var1)
  prob=abc/len
  hist(rbinom(len,input$n_value,prob), xlab = input$sel_column, col = "yellow", border = "black")
})
################################### Normal Distribution
output$val_n<-renderUI({
  sliderInput("n_value_norm","Select value of n ",min = 1,max = 100,value = 50)
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



#####################################  GLM


output$model_select<-renderUI({
  selectInput("modelselect","Select Regression Type",choices = c("Logistic_reg"="logreg","Linear Regression"="linreg"))
})
output$var1_select<-renderUI({
  selectInput("out_var_select","Select Dependent/Output Var", choices =as.list(names(data())),multiple = FALSE)
})
output$rest_var_select<-renderUI({
  checkboxGroupInput("ind_var_select","Select input/independent var",choices =as.list(names(data())))
})
output$other_val_show<-renderPrint({
  input$ind_var_select
  input$out_var_select
  f<-data()
  library(caret)
  form <- sprintf("%s~%s",input$out_var_select,paste0(input$ind_var_select,collapse="+"))
  print(form)
  
  if(input$modelselect=='logreg'){
    logreg <-glm(as.formula(form),family=binomial(),data=f)
    
  }
  else if(input$modelselect=='linreg'){
    logreg <-glm(as.formula(form),family=gaussian(),data=f)
    
  }
  
  
  
  #logreg <-glm(as.formula(form),family=fam(),data=f)
  print(summary(logreg))
  
})


output$plot_col1=renderUI(
  selectInput("column1","Select column on x-axis",choices =as.list(names(data())))
)
output$plot_col2=renderUI(
  selectInput("column2","Select column on y-axis",choices =as.list(names(data())))
)
output$ploo=renderPlot({
  var1 <- data()[,input$column1]
  if (is.null(var)){return(NULL)}
  var2 <- data()[,input$column2]
  if (is.null(var)){return(NULL)}
  plot(var1,var2,pch = 16,xlab =input$column1,ylab= input$column1 )
})




output$hypo_select<-renderUI({
  selectInput("hypo","Select Column", choices =as.list(names(data())),multiple = FALSE)
})
output$decision<-renderPrint({
  
  var1 <- data()[,input$hypo]
  if (is.null(var)){return(NULL)}
  
  h<-data()
  var1=na.omit(var1)
  t1 <- t.test(var1, alternative = 'two.sided', mu = as.numeric(input$mu), conf.level = 0.05)
  alpha=0.05
  print(t1)
  ab=t1$statistic
  print(ab)
  print("check if p-values is less than 0.05 reject otherwise accept")
  
})

}
shinyApp(ui=ui,server=server)