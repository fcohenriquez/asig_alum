
setwd("~/R/asig_cursos")

set.seed(123456)

tot_alum <- 108
num_cursos_orig <- 4
num_cursos <- 3
num_escogidos <- 3

tam_cursos <- tot_alum%/%num_cursos

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

#incomp_1[1,]

incompatibles <- list(c(1, 2),
                      c(7,8)
)

#n_incompat <- length(incompatibles)
n_incompat <-num_par_imcomp

# Companeros escogidos 



for (i in c(1:tot_alum)) {
  i_1 <- i-1  
  i1 <- i+1
  if (i==1) {
    univ_samp <- c(2:tot_alum)
  }
  if (i==tot_alum) {
    univ_samp <- c(1:i_1)
  }
  if (i>1 & i<tot_alum) {
    univ_samp <-c(1:i_1,i1:tot_alum)
  }
  
  escogidos_aux<- sample(univ_samp,num_escogidos)
  
  if (i==1) {
    escogidos <- escogidos_aux
  }
  else {
    escogidos <- rbind(escogidos,escogidos_aux)
  }
  
}

#c(-i)

rm(univ_samp, escogidos_aux)

id <- c(seq(from=1, to=tot_alum, by=1))

incomp_1 <- as.data.frame(incomp_1)
colnames(incomp_1) <- c("id","incompatible")

datos_entrada <- cbind(id,curso_orig,ninas,escogidos)
datos_entrada <-as.data.frame(datos_entrada)

datos_entrada <- merge(datos_entrada,incomp_1,by=c("id","id"), all.x=TRUE)


write.csv(datos_entrada, file = "datos_entrada.csv")

summary(datos_entrada)



