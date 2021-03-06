######################################
### AUTHOR: Miguel Lopez Garralon  ###
### DATE: 29/10/2019               ###
### TITLE: Tarea Reactividad 1     ###
######################################

# librerias y bases de datos

library(shiny)
library(datasets)
library(ggplot2)
data('mpg')
data('mtcars')
data('cars')
data('anscombe')
data('trees')
data('Titanic')

Titanic <- data.frame(Titanic)


ui <- fluidPage(

    titlePanel(
        'Tarea Reactividad 1'
    ),
    sidebarLayout(
        sidebarPanel(
            selectInput('dataSet', label = h3('Elige el DataSet'),
                        choices = list('MPG' = 'mpg',
                                       'MTCARS' = 'mtcars',
                                       'CARS' = 'cars',
                                       'ANSCOMBE' = 'anscombe',
                                       'TREES' = 'trees',
                                       'TITANIC' = 'Titanic'),
                        selected = 'mpg'),
            selectInput('tipoResultado', label = h3('Elige tipo de resumen'),
                        choices = list('Mostrar summary' = 'summary',
                                       'Mostrar primeras filas' = 'head',
                                       'Mostrar ultimas filas' = 'tail'),
                        selected = 'summary'),
            actionButton('mostrarResultado',
                         label = h3('Mostrar Resultado'))
        ),
        mainPanel(
            verbatimTextOutput('resultadoElegido')
            
        )
    )
)


server <- function(input, output) {

    output$resultadoElegido <- renderPrint({
        input$mostrarResultado
        
        # rompo las relaciones para que solo se active con el button
        baseDatos <- isolate(input$dataSet)
        seleccionResumen <- isolate(input$tipoResultado)
        
        # asigno la seleccion de la base de datos
        seleccionBaseDatos <- switch(baseDatos,
                                     'mpg' = mpg,
                                     'mtcars' = mtcars,
                                     'cars' = cars,
                                     'anscombe' = anscombe,
                                     'trees' = trees,
                                     'Titanic' = Titanic)
        # creo la funcion que muestra el tipo de resumen seleccionado
        elegirResumen <- ifelse(seleccionResumen == 'summary',
                                summary,
                                ifelse(seleccionResumen == 'head',
                                       head, tail))
        
        
        elegirResumen(seleccionBaseDatos)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
