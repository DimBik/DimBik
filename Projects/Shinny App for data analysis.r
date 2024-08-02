# Clean environment
rm(list = ls())

# Libraries
library(xlsx)
library(shiny)
library(tidyverse)

# Load data
data <- read.xlsx('C:/Users/jimbi/OneDrive/Desktop/Notes/Intro to R for Data Science/Assessment 2/STAT8010_2021_assignment2.xlsx', sheetIndex = 1, stringsAsFactors=T )
baseline <- read.xlsx('C:/Users/jimbi/OneDrive/Desktop/Notes/Intro to R for Data Science/Assessment 2/STAT8010_2021_assignment2.xlsx', sheetIndex = 2)

names(data)[4] <- "dailyO2"
#View(data)
#summary(data)



# Front-end
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = 'var',
                'Select a variable',
                choices = names(data)
            ),
            selectInput(
                inputId = 'var2',
                'Select a variable',
                choices = names(data)
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Plot",
                    splitLayout(
                        verticalLayout(
                            plotOutput(outputId = 'plot'),
                            textOutput(outputId = 'appl'),
                            verbatimTextOutput('R2')
                        ),
                        verticalLayout(
                            hr(),
                            # Creates a checkbox in the well panel in tab 1
                            wellPanel(
                                checkboxInput(
                                    inputId = 'bool',
                                    'Linear Regrassion Model'
                                )
                            ),
                            hr(),
                            wellPanel(
                                selectInput(
                                    inputId = 'var3',
                                    'Choose Categorical Variable',
                                    choices = names(data)[7:10]
                                )
                            )
                        )
                    )
                ),
                tabPanel(
                    "Summary",
                    verbatimTextOutput(outputId = 'summ')
                ),           
                tabPanel(
                    "Histogram A",
                    plotOutput(outputId = 'hist1'),
                    textOutput(outputId = 'text1')
                ),           
                tabPanel(
                    "Histogram B",
                    plotOutput(outputId = 'hist2'),
                    textOutput(outputId = 'text2')
                )
            )
        )
    )
)

# Back-end
server <- function(input, output){
    fctr <- reactive(paste('as.factor(', input$var3, ')', sep = ''))

    o <- reactive({lm(as.formula(paste(input$var2, '~', input$var)), data = data)})
    x <- reactive({ifelse(input$var=='vessel' | input$var=='feed' | input$var=='base' | input$var=='hoses', yes = 1, no = 0)})
    y <- reactive({ifelse(input$var2=='vessel' | input$var2=='feed' | input$var2=='base' | input$var2=='hoses', yes = 1, no = 0)})
    k <- reactive(paste('as.factor(',input$var2,')',sep = ''))

    output$plot=renderPlot({
        if(x()==0 & y()==0){
            ggplot(data) + 
            geom_point(
                mapping = aes_string(
                    x = input$var,
                    input$var2,
                    colour = fctr()
                            )
                        ) + 
            if(input$bool){
                if(is.na(o()$coefficients[2])==T){
                    geom_abline(
                        slope = 1,
                        intercept = 0)
                }else{
                    geom_abline(
                        slope = o()$coefficients[2],
                        intercept = o()$coefficients[1])
                }
            }
        }else{
            if(x()==1 & y()==0){
                boxplot(as.formula(paste(input$var2, "~", input$var)),
                        data = data)
            }else{
                if(x()==0 & y()==1){
                    boxplot(as.formula(paste(input$var, "~", input$var2)),
                            data = data)
                }else{
                    ggplot(data = data) + 
                    geom_bar(
                        mapping = aes_string(
                            x = input$var,
                            fill = k()),
                        position = 'dodge')
                }
            }
        }
    })
    output$appl <- renderText({if(input$bool & (x()!=0 | y()!=0)){
    print("The regression line is not applicable")
    }})

    output$R2 <- renderPrint({if(input$bool & x()==0 & y()==0 & input$var!=input$var2){
        cat(
            c(
                "R^2=",
                round(cor(data[,input$var],data[,input$var2])^2,2),
                '\n',
                paste(
                    'y=',
                    round(lm(as.formula(paste(input$var2,'~',input$var)),data = data)$coefficients[1],2),
                    '+(',
                    round(lm(as.formula(paste(input$var2,'~',input$var)),data = data)$coefficients[2],2)
                    ,")x",
                    sep = ''
                )
            ),
            sep = ''
        )
    }else{
        if(input$bool & x()==0 & y()==0 & input$var==input$var2){
            cat(
                c(
                    "R^2=",
                    1,
                    '\n',
                    'y=x'
                ),
                sep=''
            )
        }
    }})

    output$summ <- renderPrint(summary(data))
    output$hist1 <- renderPlot({if(x()==0){
            hist(data[, input$var], xlab = input$var, main = paste('Histogram of', input$var))
    }})
    output$text1 <- renderText({if(x()==1){
        print("The histogram is not available as the variable is not continuous ")
    }})
    output$hist2 <- renderPlot({if(y()==0){
        hist(data[,input$var2], xlab=input$var2, main=paste('Histogram of', input$var2))
    }})
    output$text2 <- renderText(if(y()==1){
        print("The histogram is not available as the variable is not continuous ")
    })
}

# Run the app
shinyApp(ui = ui, server = server)