#setwd("~/R/asig_cursos")
setwd("~/Documentos/R/asignacion_cursos")

set.seed(1234)

tot_alum <- 108
num_cursos_orig <- 4
num_cursos <- 3
num_escogidos <- 3

tam_cursos <- tot_alum%/%num_cursos

id <- c(seq(from=1, to=tot_alum, by=1))

#Se simula el curso original del que provenia el estudiante
curso_orig <- c(rep(1,tot_alum/num_cursos_orig))
for (i in c(2:num_cursos_orig)) {
  curso_orig <- c(curso_orig,rep(i,tot_alum/num_cursos_orig))
}

max_mismo_curs_orig <- (tot_alum/num_cursos_orig)%/%num_cursos

#Se genera un listado aleatorio de ninas para modelar 
ninas <- runif(tot_alum) 
ninas[ninas>=.5] <- 1
ninas[ninas<1] <- 0

ninas_por_curso <- sum(ninas)%/%num_cursos


# list de incompatibles (no puede haber m?s incompatibles que cursos a asignar)

num_par_imcomp <- 5

incomp <- sample(1:tot_alum, num_par_imcomp*2, replace=FALSE)

incomp_1 <-cbind(c(rep(0,num_par_imcomp)),c(rep(0,num_par_imcomp)))

for (i in c(1:num_par_imcomp)) {
  j <- i*2
  incomp_1[i,1] <- incomp[i]
  incomp_1[i,2] <- incomp[j]
  
}

incompatibles <- list(c(1, 2),
                      c(7,8)
)

n_incompat <-num_par_imcomp

# Companeros escogidos 


    #primero se genera el numero de grupos de escogidos
    n_grup_esc <- tot_alum%/%(num_escogidos+1)
    
    # se generan los n grupos escogidos
    
    for (i in c(1:(num_escogidos+1))) {
      if (i==1) {
        g_escogido <-data.frame(g_escogido=sample(n_grup_esc,n_grup_esc))
      }
      else{
        aux_g_esc <- data.frame(g_escogido=sample(n_grup_esc,n_grup_esc))
        g_escogido <- rbind(g_escogido,aux_g_esc)
      }
    }

    if ((num_escogidos+1)*n_grup_esc<tot_alum) {
      aux_g_esc <- data.frame(g_escogido=sample(n_grup_esc,tot_alum-((num_escogidos+1)*n_grup_esc)))
      g_escogido <- rbind(g_escogido,aux_g_esc)
    }
    

      # Se genera un dataframe con los vectores de escogidos
      g_escogido <- cbind(id,g_escogido)
      
      g_escogido <- g_escogido[order(g_escogido$g_escogido, g_escogido$id),]
      
      g_escogido$escog<- ave(g_escogido$id, g_escogido$g_escogido , FUN=rank)
      
      g_escogido_rs <- reshape(g_escogido, idvar = "g_escogido", timevar = "escog", direction = "wide")
      
      g_escogido <- g_escogido[order(g_escogido$id),]
      
      for (i in c(1:tot_alum)) {
        g_i <- g_escogido$g_escogido[g_escogido$id==i]
        g_escogido_i <- subset(g_escogido_rs,g_escogido==g_i)
        g_escogido_i$g_escogido <- NULL
        print(g_escogido_i)
        n_vec_esc <- ncol(g_escogido_i)
        g_escogido_i_a <- rep(0,ncol(g_escogido_i))
        for (j in c(1:ncol(g_escogido_i))) {
          aux <- sum(g_escogido_i[1,j],1, na.rm =T)
          aux <- aux-1

          if (aux!=i  &  aux!=0) {
            g_escogido_i_a[j] <-aux 
          }
        }
        g_escogido_i_a <- sort(g_escogido_i_a, decreasing = T)
        g_escogido_i_a <-g_escogido_i_a[1:num_escogidos]
        print(g_escogido_i_a)
        
        if (i==1) {
          escogidos <- g_escogido_i_a
        }
        else  {
          escogidos <- rbind(escogidos,g_escogido_i_a)
        }

      }
    

      

            
# Reunion de los  datos simulados

incomp_1 <- as.data.frame(incomp_1)
colnames(incomp_1) <- c("id","incompatible")

datos_entrada <- cbind(id,curso_orig,ninas,escogidos)
datos_entrada <-as.data.frame(datos_entrada)

datos_entrada <- merge(datos_entrada,incomp_1,by=c("id","id"), all.x=TRUE)


write.csv(datos_entrada, file = "datos_entrada.csv")

summary(datos_entrada)
