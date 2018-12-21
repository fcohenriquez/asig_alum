#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                # Application title
                titlePanel("Asignacion de alumnos a nuevos cursos"),
                
                # Sidebar with a slider input para el numero total de alumnos
                sidebarLayout(
                  sidebarPanel(
                    tags$head(
                      tags$title('My first page')
                    ),
                    tags$body(
                      h4('Suba el archivo con los datos de los alumnos'),
                      p('El archivo debe ser ',
                        strong('CSV'),
                        ' separado por comas.'),
                      div(id='myDiv1', class='simpleDiv',
                          'Los campos deben ser: '),
                      div(id='myDiv2', class='simpleDiv',
                          'row, id, curso_orig, ninas, elegido_1, ..., elegido_n, incompatible.')
                    ),
                    
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Select a file ----
                    fileInput("file1", "Subir archivo de alumnos",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    
                    
                    
                    
                    sliderInput("n_curs_fin", "Numero de cursos finales:",
                                min = 1, max = 5, value = 3),
                    
                    # Button
                    downloadButton("downloadData", "Descargar Asignacion")
                    
                  ),
                  
                  # Muestra la tabla de asignacion final de los alumnos
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("datos de alumnos", 
                                         textOutput("dat_orig"),  dataTableOutput("contents")),
                                tabPanel("resultado asignacion", 
                                         textOutput("resu_asig"), dataTableOutput("asig_curso")),
                                tabPanel("Estadisticas Proceso",
                                         htmlOutput("t_sum_origen"),
                                         tableOutput("sum_origen"),
                                         htmlOutput("t_sum_res"),
                                         tableOutput("sum_res"))
                                
                    )
                    
                  )
                ),
                # Para colocar un link
                uiOutput("tab"),
                # WHERE YOUR FOOTER GOES
                hr(),
                print("Aplicacion desarrollada por Francisco Henriquez, 2018")
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Para colocar un link
  url <- a("Pagina de Consulta", href="https://github.com/fcohenriquez/asig_alum")
  output$tab <- renderUI({
    tagList("Vinculo para documentacion:", url)
  })
  
  output$dat_orig <- renderText({
    req(input$file1)
    print("Estos son los datos originales")
    
  })
  
  
  
  output$contents <- renderDataTable({
    
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath)
      },
      error = function(e) {
        # return a safe Error if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
    
  })
  
  output$resu_asig <- renderText({
    req(input$file1)
    print("Estos son los resultados de la asignacion")
  })
  
  
  f_asig <- function(f,y){
    
    
    
    ###################################################################################################################
    # Problema asignacion alumnos
    ##################################################################################################
    
    
    #title: "Problema asignacion alumnos"
    #author: "Francisco Henriquez"
    #date: "20/12/2018"
    #output: asignacion.csv
    
    library(lpSolve)
    
    
    # Parametros tomados de una planilla
    #####################################
    
    
    set.seed(123456)
    
    datos_entrada <-read.csv(f,row.names=1)
    
    tot_alum <- nrow(datos_entrada)
    num_cursos_orig <- length(levels(as.factor(datos_entrada$curso_orig)))
    num_cursos <- y
    num_escogidos <- ncol(datos_entrada)-4
    
    
    tam_cursos <- tot_alum%/%num_cursos
    
    #Se simula el curso original del que provenia el estudiante
    curso_orig <- datos_entrada$curso_orig
    
    max_mismo_curs_orig <- (tot_alum/num_cursos_orig)%/%num_cursos
    
    #Se genera un listado aleatorio de ninas para modelar 
    ninas <- datos_entrada$ninas
    
    ninas_por_curso <- sum(ninas)%/%num_cursos
    
    
    # list de incompatibles (no puede haber m?s incompatibles que cursos a asignar)
    
    
    incomp <- data.frame(id=datos_entrada$id,incompatible=datos_entrada$incompatible)
    
    incomp <- subset(incomp,!is.na(incompatible))
    
    num_par_incomp <- nrow(incomp)
    n_incompat <-num_par_incomp
    
    incompatibles <- list(c(incomp[1,1],incomp[1,2]))
    for (i in c(2:num_par_incomp)) {
      
      aux_inc <- list(c(incomp[i,1],incomp[i,2]))
      incompatibles <- c(incompatibles,aux_inc)
      
    }
    
    
    
    rm( incomp, aux_inc)
    
    
    
    
    # Companeros escogidos 
    
    escogidos <- datos_entrada[,4]
    
    if (num_escogidos>1) {
      n_e1 <- num_escogidos-1
      for (i in c(1:n_e1)) {
        escogidos <- cbind(escogidos,datos_entrada[,4+i])
        
      }
      rm(n_e1)
    }
    
    
    # Funcion objetivo
    ###############################
    
    
    
    #Se genera el vector funcion objetivo con el numero de elementos como la combinacion
    #de alumnos y cursos mas las variables auxiliares que se deben usar para modelar
    #los companeros escogidos
    
    cursos.obj <- rep (1, num_cursos*tot_alum+tot_alum*num_escogidos*num_cursos)
    
    
    
    # Restricciones
    #####################
    
    #tamano de curso (rest_tam_alum: cada alumno tiene que estar en un solo curso)
    
    rest_tam_alum <- matrix(rep(0,num_cursos*tot_alum^2),nrow=tot_alum)
    
    for (i in c(1:tot_alum)) {
      id_alum <- i
      for (j in c(1:num_cursos)) {
        rest_tam_alum[i,(id_alum)] <- 1
        id_alum <- id_alum+tot_alum
      }
      id_alum <- id_alum+1
    }
    
    #rest_tam_curso: cada curso tiene que tener el numero de alumnos determinado
    
    rest_tam_curso <- matrix(rep(0,num_cursos^2*tot_alum),nrow=num_cursos)
    
    for (i in c(1:num_cursos)) {
      if (i==1) {
        bloque_curso <- 1
      }
      for (j in c(1:tot_alum)) {
        rest_tam_curso[i,bloque_curso] <- 1    
        bloque_curso <- bloque_curso+1
      }
      
    }
    
    
    # Restricciones de ninas (este tipo de restricciones se puede utilizar para distribuir proporcionadamente otros grupos como los de mejor desempeno academico, etc.)
    
    rest_ninas <- rest_tam_curso 
    ini_tot_alum <- 1
    for (i in c(1:num_cursos)) {
      rest_ninas[i,ini_tot_alum:(tot_alum*i)] <- ninas
      ini_tot_alum <- tot_alum*i+1
    }
    
    
    # Restricciones de cantidad maxima por curso original (tiene que haber num_cursos_orig*num_cursos restricciones)
    
    for (j in c(1:num_cursos_orig)) {
      aux <- rep(curso_orig,num_cursos)
      aux1 <-replace(aux,aux!=j,0)
      aux1 <-replace(aux1,aux1==j,1)
      
      if (j==1) {
        aux2 <- aux1
      }
      else  {
        aux2 <- rbind(aux2, aux1)
      }
    }
    
    rest_curso_orig <- aux2
    
    for (i in c(1:(num_cursos-1))) {
      rest_curso_orig <- rbind(aux2, rest_curso_orig)
    }
    
    k <- 0
    for (i in c(1:num_cursos)) {
      for (j in c(1:num_cursos_orig)) {
        k <- k+1
        rest_curso_orig[k, ] <-  rest_curso_orig[k, ]*rest_tam_curso[i,]
      }
    }
    
    
    
    # Restricciones de alumnos incompatibles  (notese que el grupo de alumnos no puede ser mayor al numero de cursos). Tiene que haber 3 restricciones por cada grupo de incompatibles
    
    aux<- rep(0,tot_alum*num_cursos)
    aux0 <- aux
    rest_incompatibles <- matrix(rep(aux,(num_cursos*n_incompat)), nrow=(num_cursos*n_incompat))
    incompat <- 0
    pos_incomp <-0
    for (i in c(1:num_cursos)) {
      for (j in c(1:n_incompat)) {
        for (k in incompatibles[j]) {
          incompat <-incompat+1
          aux <- aux0
          
          aux[pos_incomp+k] <- 1
          rest_inc <-incompat 
          rest_incompatibles[rest_inc,] <- aux
          
        }
      }
      pos_incomp <- pos_incomp+(tam_cursos*num_cursos)
    }
    
    
    # Restricciones de estar por lo menos con un escogido 
    
    # para cada alumno, se debe generar una restriccion por curso y por preferencia 
    # donde se indique que quedar por lo menos con uno de los escogidos
    # Es una restriccion de holguras activas por alumno, 
    # compuesta de num_cursos*num_escogidos coeficientes
    
    aux <- c(rep(0,num_cursos*tot_alum+tot_alum*num_escogidos*num_cursos))
    contador <- 1
    i_pos2 <- 0
    for(i in c(1:tot_alum)) {
      i_pos <- 0
      j_pos <- 0
      aux2 <- aux
      for (j in c(1:num_cursos)) {
        k_pos <- 1 
        for (k in escogidos[i,] ) {
          aux1 <- aux
          aux1[i+i_pos] <- 1 #Alumno que manifiesta preferencia
          aux1[k+i_pos] <- 1 #Alumno preferido
          aux1[num_cursos*tot_alum+k_pos+j_pos+i_pos2] <- 2 #Holgura
          aux2[num_cursos*tot_alum+k_pos+j_pos+i_pos2] <- 1 # Holgura
          k_pos <- k_pos+1
          if (contador==1) {
            rest_escogido <- aux1
          }
          else {
            rest_escogido <- rbind(rest_escogido,aux1)
          }
          contador <- contador+1
        }
        i_pos <- i_pos+tot_alum 
        j_pos <- j_pos+num_escogidos
      }
      i_pos2 <- i_pos2+(num_escogidos*num_cursos)
      if (i==1) {
        rest_holguras <- aux2
      }
      else {
        rest_holguras <- rbind(rest_holguras,aux2)
      }
    }
    
    
    # Aca se agregan las distintas restricciones (primero se incorporan las restricciones de preferencias)
    
    aux_pref <-matrix(rep(0,nrow(rest_tam_alum)*(tot_alum*num_cursos*num_escogidos)), nrow=nrow(rest_tam_alum))
    rest_tam_alum <- cbind(rest_tam_alum,aux_pref)
    
    aux_pref <-matrix(rep(0,nrow(rest_tam_curso)*(tot_alum*num_cursos*num_escogidos)), nrow=nrow(rest_tam_curso))
    rest_tam_curso <- cbind(rest_tam_curso,aux_pref)
    
    aux_pref <-matrix(rep(0,nrow(rest_ninas)*(tot_alum*num_cursos*num_escogidos)), nrow=nrow(rest_ninas))
    rest_ninas <- cbind(rest_ninas,aux_pref)  
    
    aux_pref <-matrix(rep(0,nrow(rest_curso_orig)*(tot_alum*num_cursos*num_escogidos)), nrow=nrow(rest_curso_orig))
    rest_curso_orig <- cbind(rest_curso_orig,aux_pref) 
    
    aux_pref <-matrix(rep(0,nrow(rest_incompatibles)*(tot_alum*num_cursos*num_escogidos)), nrow=nrow(rest_incompatibles))
    rest_incompatibles <- cbind(rest_incompatibles,aux_pref)  
    
    restricciones <- rbind(rest_tam_alum, rest_tam_curso, rest_ninas, rest_curso_orig, 
                           rest_incompatibles, rest_escogido, rest_holguras)
    
    rm(aux_pref)
    
    # Direccion
    #################
    
    # Direccion tamanos 
    dir_tam_alum <- rep("==",tot_alum) #Esta restriccion indica que hay y un solo alumno por curso y que todos los alumnos esten en un curso
    dir_tam_curso <- rep(">=",num_cursos) #Esta restriccion es para hacer que todos los cursos tengan cantidades similares de alumnos (no son iguales porque el total de alumnos puede que no sea divisible por el tamano de los cursos)
    dir_ninas <- rep(">=",num_cursos) #Esta restriccion es para hacer que todos los cursos tengan cantidades similares de alumnos y alumnas (no son iguales porque el total de alumnos puede que no sea divisible por el tamano de los cursos)
    dir_curso_orig <- rep(">=",(num_cursos*num_cursos_orig)) #Esta restriccion es para hacer que todos los cursos tengan cantidades similares de alumnos de distintos cursos originales (no son iguales porque el total de alumnos puede que no sea divisible por el tamano de los cursos)
    dir_incompatible <- rep("<=",(num_cursos*n_incompat))
    dir_escogidos <- rep(">=",(tot_alum*num_cursos*num_escogidos))
    dir_holguras <- rep("<=", tot_alum)
    
    #agregacion de direcciones
    direcciones <-c(dir_tam_alum,dir_tam_curso, dir_ninas, dir_curso_orig, dir_incompatible,
                    dir_escogidos, dir_holguras)
    
    
    # Coeficientes restricciones
    ###############################
    
    coef_tam_alum <- rep(1,tot_alum) #Esta restriccion indica que hay y un solo alumno por curso y que todos los alumnos esten en un curso
    coef_tam_curso <- rep(tam_cursos,num_cursos) #Esta restriccion es para hacer que todos los cursos tengan cantidades similares de alumnos (no son iguales porque el total de alumnos puede que no sea divisible por el tamano de los cursos)
    coef_ninas <- rep(ninas_por_curso,num_cursos) #Esta restriccion es para hacer que todos los cursos tengan cantidades similares de alumnos y alumnas (no son iguales porque el total de alumnos puede que no sea divisible por el tamano de los cursos)
    coef_curso_orig <- rep(max_mismo_curs_orig, (num_cursos*num_cursos_orig)) #Esta restriccion es para hacer que todos los cursos tengan cantidades similares de alumnos de distintos cursos originales (no son iguales porque el total de alumnos puede que no sea divisible por el tamano de los cursos)
    coef_incompatible <- rep(1,(num_cursos*n_incompat)) #Esta restriccion es para que no esten en el mismo cursos los alumnos incompatibles
    coef_escogidos <- rep(2,(tot_alum*num_cursos*num_escogidos))
    coef_holguras <- rep((num_cursos*num_escogidos-1), tot_alum)
    
    # Agregacion de coeficientes de restriccion
    coeficientes <- c(coef_tam_alum,coef_tam_curso,coef_ninas, coef_curso_orig, coef_incompatible,
                      coef_escogidos, coef_holguras)
    
    
    # Programacion lineal entera
    #####################################################################################
    
    
    lp_cursos <- lp ("max", cursos.obj, restricciones, direcciones, coeficientes)
    
    # Exportacion de resultados
    
    result <- matrix(lp_cursos$solution[1:(tot_alum*num_cursos)], ncol=num_cursos)
    
    result <- as.data.frame(result)
    result <- round(result,0)
    
    result$Curso_final<-0 
    for (i in c(1:num_cursos)) {
      
      result$Curso_final[result[,i]==1] <-i 
    }
    table(result$Curso_final)
    result <- data.frame(Curso_final=result$Curso_final)
    
    result <- as.data.frame(cbind(ninas,curso_orig,result,escogidos))
    
    
    for (i in c(1:num_escogidos)) {
      vnn<- paste("Escogido ",i, sep="")
      colnames(result)[colnames(result)==i] <- vnn
      
    }
    
    colnames(result)[colnames(result)=="ninas"] <- "genero"
    
    result$genero <- as.factor(result$genero)
    levels(result$genero) <- c("Masculino","Femenino")
    
    
    rownames(result) <- c(seq(from=1, to=tot_alum, by=1))
    
    result1 <- data.frame(id=rownames(result),curso_orig=result$curso_orig,curso_final=result$Curso_final )
    
    rm(bloque_curso, aux, aux1, aux2, i, j, k, vnn, id_alum)
    
    
    
    return(result1)
  }
  
  
  
  output$asig_curso <- renderDataTable({
    req(input$file1)
    f_asig(input$file1$datapath, input$n_curs_fin)
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("asignacion", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(f_asig(input$file1$datapath, input$n_curs_fin), file, row.names = TRUE)
    }
  )
  # Aca comienza la generacion de la reportabilidad
  
  
  output$t_sum_origen <- renderUI({
    req(input$file1)
    datos_entrada <- read.csv(input$file1$datapath)
    datos_entrada <- as.data.frame(datos_entrada)
    num_escogidos <- ncol(datos_entrada)-4
    incomp <- data.frame(id=datos_entrada$id,incompatible=datos_entrada$incompatible)
    incomp <- subset(incomp,!is.na(incompatible))
    n_incompat <- nrow(incomp)
    
    
    mylist <- c("Estadisticas de los datos originales",
                "",
                paste("la cantidad total de alumnos es", nrow(datos_entrada), sep=" "),
                paste("la cantidad de cursos originales es",  length(levels(as.factor(datos_entrada$curso_orig))), sep=" "),
                paste("la cantidad de companeros escogidos es", num_escogidos, "OJO!!!!!" , sep=" "),
                paste("el promedio de alumnos por curso es", nrow(datos_entrada)/length(levels(as.factor(datos_entrada$curso_orig))), sep=" "),
                paste("el numero de parejas incompatibles es", n_incompat, sep=" "),
                paste("el porcentaje de ninas es", round(100*mean(datos_entrada$ninas),2), sep=" "),
                "",
                "Distribucion de alumnos por curso"
                
    )
    
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  output$sum_origen <- renderTable({
    req(input$file1)
    datos_entrada <- read.csv(input$file1$datapath)
    a <- table(datos_entrada$curso_orig,datos_entrada$ninas)
    b <- table(datos_entrada$curso_orig)
    c <- cbind(rownames(table(datos_entrada$curso_orig)),a,b)
    d <- c("Total",sum(a[,1]),sum(a[,2]), sum(b))
    c <- rbind(c,d)
    c <- as.data.frame(c)
    colnames(c) <- c("Curso", "Ninos", "Ninas", "Total")
    return(c)
    
  })
  
  output$t_sum_res <- renderUI({
    req(input$file1)
    resultados <- f_asig(input$file1$datapath, input$n_curs_fin)
    mylist <- c("","Estadisticas de la asignacion",
                "",
                paste("Numero de alumnos asignados", nrow(resultados), sep=" "),
                "",
                "Tabla "
                
    )
    
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
    
  })
  
  output$sum_res <- renderTable({
    req(input$file1)
    resultados <- f_asig(input$file1$datapath, input$n_curs_fin)
    datos_entrada <- read.csv(input$file1$datapath)
    datos_entrada <- as.data.frame(datos_entrada)
    #resultados$curso_orig <- as.factor(resultados$curso_orig)
    #resultados$curso_final <- as.factor(resultados$curso_final)
    a <- table(resultados$curso_orig,resultados$curso_final)
    #a <- as.data.frame(a)
    union <- merge(datos_entrada,resultados, by=c("id", "id"))
    
    b <- table (union$curso_final,union$ninas)
    c <- table (union$curso_orig.x,union$ninas )
    return (c)
    
  })  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

