#La idea del paquete es que permita simular si el promedio y la raz??n entre dos variables,
# var??a la confianza planteada dependiendo de la variable a utilizar. La salida de la funci??n
#es el gr??fico de densidad con la funci??n te??rica y la funci??n de los datos. Y muestra la media poblacional seleccioanda.


#' Funcion TLC para la media
#'
#' @param var1 Variable a utilizar
#' @param n Tama??o o tama??os de muestras
#' @param k n??mero de iteraciones
#' @param t tiempo de espera
#'
#' @return
#' @export
#' @examples variable1=rnorm(1000,50,10);tlc(variable1,c(32,200),20,2)
#'
tlc= function(var1,n,k,t) { # var1 (Variable a analizar), n (Tama??os de muestra), k numero de iteracciones, t de tiempo
  med2=mean(var1)
  var2=var(var1)
  sd2=sqrt(var1)

  for(j in 1:length(n)){
    med=c()

    for(i in 1:k){

      id=sample(1:length(var1),n[j])
      y=var1[id]
      med[i]=mean(y)
    }
    print(hist(med,freq=F, main = paste("Histograma (tama??o de muestra=" ,n[j],")"),yaxt="n"))
    curve(dnorm(x,mean(var1),sd(var1)/sqrt(n[j])),add=T,col=2)
    lines(density(var1),col=4)
    Sys.sleep(t)
  }
}




#' Funcion TLC para la razon de dos variables
#'
#' @param x1 Variable 1
#' @param y1 Variable 2
#' @param n Tama??o o tama??os de muestras
#' @param k n??mero de iteraciones
#' @param t tiempo de espera
#'
#' @return
#' @export
#'
#' @examples variable1=rnorm(1000,50,10);Variable2=rpois(1000,60);n1=c(5,70,100) ;tlc_razon(variable1,Variable2,n1,10,2)
#'
tlc_razon= function(x1,y1,n,k,t) {


  for(j in 1:length(n)){
    raz=c()

    for(i in 1:k){

      id=sample(1:length(x1),n[j])
      a=x1[id]
      b=y1[id]
      raz[i]=(sum(a)/sum(b))
    }

    print(plot(density(raz),las=1, main=paste("Gr??fico de densidad (tama??o de muestra=" ,n[j],")")))
    r= sum(x1)/sum(y1)
    curve(dnorm(x,mean(raz),sd(raz)),col=2, add=T) #De este tengo duda
    abline(v=r,col=2)
    Sys.sleep(t)
  }
}
