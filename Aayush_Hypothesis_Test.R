library(shinythemes)

#loading the shiny library
library(shiny) 
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
               
               
               
               
               
               tabPanel("Hypothesis",
                        sidebarLayout(sidebarPanel(
                          uiOutput("hypo_select"),
                          textInput("mu", "Enter the value of mu")),
                          mainPanel( h2("The decision is:"),
                                     verbatimTextOutput("decision"))))
               
)











server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
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