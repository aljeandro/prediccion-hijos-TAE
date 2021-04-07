library(xgboost)
library(shiny)
library(shinyWidgets)
library(shinythemes)

datos <- read.csv('Resultado1.csv',sep = ';', encoding = 'UTF-8')


#for ( i in c(1:19))
#{
#    datos[,i]= as.numeric(datos[,i])
#}

modelopython<-xgb.load('modelo1.bin')


# Define UI for application that draws a histogram
ui <-( 
    navbarPage(
        title="TAE",
        theme=shinytheme("spacelab"),
        inverse=TRUE,
        position = "static-top",
        tabPanel("Clasificacion Hijos",
                 
                 fluidPage(
                     flowLayout(
                         numericInput("S1",
                                      label = h4("Actividad Economica"),
                                      value=1, 
                                      min = min(datos$ACTIVIDAD_ECONOMICA),
                                      max=max(datos$ACTIVIDAD_ECONOMICA)),
                         numericInput("S2",
                                      label = h4("Grado Aprobado"),
                                      value=1,
                                      min = min(datos$GRADO_APROBADO),
                                      max=max(datos$GRADO_APROBADO)),
                         numericInput("S3", 
                                      label = h4("Horas Trabaja Semana"), 
                                      value=1,
                                      min = min(datos$HORAS_SEM_TRABAJA),
                                      max=max(datos$HORAS_SEM_TRABAJA)),
                         numericInput("S4", 
                                      label = h4("Ganancia Neta"), 
                                      value=1,
                                      min = min(datos$GANANCIA_NETA),
                                      max=max(datos$GANANCIA_NETA)),
                         numericInput("S5",
                                      label=h4("Municipio Nacimiento"),
                                      value=1,
                                      min = min(datos$MUNICIPIO_NACIMIENTO),
                                      max=max(datos$MUNICIPIO_NACIMIENTO)),
                         numericInput("S6",
                                      label = h4("Nivel Educativo"),
                                      value=1,
                                      min = min(datos$NIVEL_EDUCATIVO),
                                      max=max(datos$NIVEL_EDUCATIVO)),
                         numericInput("S7",
                                      label = h4("Estrato Tarifa"),
                                      value=1,
                                      min = min(datos$ESTRATO_TARIFA),
                                      max=max(datos$ESTRATO_TARIFA)),
                         numericInput("S8",
                                      label = h4("Departamento"),
                                      value=1,
                                      min = min(datos$DEPARTAMENTO),
                                      max=max(datos$DEPARTAMENTO)),
                         numericInput("S9",
                                      label = h4("Satisfaccion Ingreso"),
                                      value = 1,
                                      min = min(datos$SACTISFACCION_INGRESO),
                                      max=max(datos$SACTISFACCION_INGRESO)),
                         numericInput("S10",
                                      label = h4("Satisfaccion Salud"),
                                      value = 1, 
                                      min = min(datos$SACTISFACCION_SALUD),
                                      max = max(datos$SACTISFACCION_SALUD)),
                         numericInput("S11",
                                      label = h4("Satisfaccion Trabajo"),
                                      value = 1,
                                      min = min(datos$SACTISFACCION_TRABAJO),
                                      max=max(datos$SACTISFACCION_TRABAJO)),
                         numericInput("S12",
                                      label = h4("Escalon Vida"),
                                      value = 1,
                                      min = min(datos$ESCALON_VIDA),
                                      max=max(datos$ESCALON_VIDA)),
                         numericInput("S13",
                                      label = h4("Material Pisos"),
                                      value = 1,
                                      min = min(datos$MATERIAL_PISOS),
                                      max = max(datos$MATERIAL_PISOS)),
                         numericInput("S14",
                                      label = h4("Material Techo"),
                                      value = 1,
                                      min = min(datos$MATERIAL_TECHO),
                                      max=max(datos$MATERIAL_TECHO)),
                         numericInput("S15",
                                      label = h4("Estado Civil"),
                                      value = 1,
                                      min = min(datos$ESTADO_CIVIL),
                                      max=max(datos$ESTADO_CIVIL)),
                         numericInput("S16",
                                      label = h4("Nivel Educativo Padre"),
                                      value = 1,
                                      min = min(datos$NIVEL_EDUCACION_PADRE),
                                      max=max(datos$NIVEL_EDUCACION_PADRE)),
                         numericInput("S17",
                                      label = h4("Nivel Educativo Madre"),
                                      value = 1,
                                      min = min(datos$NIVEL_EDUCACION_MADRE),
                                      max = max(datos$NIVEL_EDUCACION_MADRE)),
                         numericInput("S18",
                                      label = h4("Edad"),
                                      value = 1,
                                      min = min(datos$EDAD),
                                      max = max(datos$EDAD)),
                         textOutput("text"),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         img(src="1.png", height="150", width="250"),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         verbatimTextOutput(""),
                         wellPanel(
                             helpText(   a("Link Video Promocional ",
                                           target="_blank",
                                           href="https://drive.google.com/file/d/1_HMWuEdcJqzFAHetC4WHc6mIsybm-Omq/view?usp=sharing"))),
                        wellPanel(
                            helpText(   a("Link Reporte Técnico",
                                        target="_blank",
                                        href="https://drive.google.com/file/d/14cri3ADiid7oOldZsjodomW4Syg2_Yxo/view?usp=sharing")
                             )
                         )
                         
                     )
                 )
        )))

server <- function(input, output,session) {
    output$text <- renderText({
        {
            Data1 = reactive({
                df<-data.frame(
                    ACTIVIDAD_ECONOMICA = input$S1,
                    GRADO_APROBADO = input$S2,
                    HORAS_SEM_TRABAJA = input$S3,
                    GANANCIA_NETA = input$S4,
                    MUNICIPIO_NACIMIENTO = input$S5,
                    NIVEL_EDUCATIVO = input$S6,
                    ESTRATO_TARIFA = input$S7,
                    DEPARTAMENTO = input$S8,
                    SACTISFACCION_INGRESO = input$S9,
                    SACTISFACCION_SALUD = input$S10,
                    SACTISFACCION_TRABAJO = input$S11,
                    ESCALON_VIDA = input$S12,
                    MATERIAL_PISOS = input$S13,
                    MATERIAL_TECHO = input$S14,
                    ESTADO_CIVIL = input$S15,
                    NIVEL_EDUCACION_PADRE = input$S16,
                    NIVEL_EDUCACION_MADRE = input$S17,
                    EDAD = input$S18)
            })
            
            prediccion <-predict(modelopython, newdata = as.matrix(Data1()))
            #print(prediccion)
            paste(c("Hijos Estimados:" ,prediccion))
            
            
            
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

