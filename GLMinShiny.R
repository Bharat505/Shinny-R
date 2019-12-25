#install.packages("shinythemes")
library(shinythemes)

#loading the shiny library
library(shiny) 

#setting up the UI using NavbarPage and TabPanel
ui<-navbarPage("Shiny Assignment", theme = shinytheme("cyborg"),
              tabPanel("Import Data",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h3("Select the table parameters below: "),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    h3("Choose the separator below: "),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(uiOutput("table1"))
                        ) ),
               tabPanel("FitModel",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show")))),
              tabPanel("plot",sidebarLayout(sidebarPanel(
                uiOutput("plot_col1"),
                uiOutput("plot_col2"),
                ),
                mainPanel( 
                           plotOutput("ploo"))))
              
)

              

server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
})  
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
output$table1 <- renderUI({
  tableOutput("table")
})
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

}
shinyApp(ui=ui,server=server)