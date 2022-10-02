#Existen 3 elementos básicos para crear la aplicación

install.packages("shiny")
library(shiny)  #Siempre se debe correr el paquete primero

ui <- fluidPage() #Interfaz

server <- function(input, output) {} #servidor

shinyApp(ui = ui, server = server) #Correr la aplicación

###Ejercicios

#1. Agreguen "texto" en fluidPage y corran todo de nuevo

ui <- fluidPage("Hola mundo") 

server <- function(input, output) {} 

shinyApp(ui = ui, server = server) 

#2. Qué pasa si le dan stop?

##Inputs y outputs
#1. Agreguen lo siguiente al fluidPage y corran la app de nuevo:

#sliderInput(inputId = "num",label = "Choose a number"
#,min = 1,max = 100,value = 35),
#plotOutput(outputId = "histog")

#2. Probar lo mismo, pero en vez de sliderInput, utilizar numericInput

#inputId me permite ponerle un nombre al input. Se recomienda que sea un nombre unico y fácil de recordar

#En plotOutput se puede ver como al inicio me dice que tipo de Output voy a usar. En este caso seria un grafico (plot).

#Al igual que los inputs, los outputs deben tener nombres unicos.


### Servidor #####

#Agreguen lo siguiente al server:
#output$histog=renderPlot({
#title="100 random normal values"
#hist(rnorm(input$num),main = title)
#})

#Paso 1
#Se guardan los objetos a mostrar en output$
#Por que output$histog??

#Paso 2
#Crear objetos a mostrar con la funcion render
#Render me permite crear el tipo de output que uno desea.
#Ejemplos de render serian renderImage, renderPlot, renderTable, renderText, etc
#Lo que va dentro del render es programacion comun y corriente. En este caso un histograma con sus especificaciones

#Paso 3
#Usar valores de input con input$
#Por que input$num?
#Los valores de los input cambian siempre que el usuario cambie el input
#Si se quiere que el output dependa de esos valores, use los valores dentro de la funcion render



