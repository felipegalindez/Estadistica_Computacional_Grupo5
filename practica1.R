#Ejercicio 1 Usando los métodos vistos en el curso convertir los números binarios a números decimales
#10101dos = 1*2^4 + 0*2^3 + 1*2^2 + 0*2^1 +1* 2^0
#11111110dos = 1*2^7 + 1*2^6 + 1*2^5 + 1*2^4 + 1*2^3 + 1*2^2 + 1*2^1 +0* 2^0
#111000dos = 1*2^5 + 1*2^4+1*2^3 + 0*2^2 + 0*2^1 +0* 2^0
#sec <- c(1,0,1,0,1)
#sec <- c(1,1,1,1,1,1,1,0)
sec <- c(1,1,1,0,0,0)
n <-length(sec)
resultado2 <- 0
for(i in 1:n){
  resultado2 <- resultado2 + sec[i]*2^(n-i)
}
print(resultado2)
#Convertir las fracciones binarias a decimales

#0,11011dos
#0,1010101dos
#1,0110101dos
#11,0010010001dos
#sec2_a<-c(0)
#sec2_b<-c(1,1,0,1,1)
#sec2_a<-c(0)
#sec2_b<-c(0,1,0,1,0,1)
#sec2_a<-c(1)
#sec2_b<-c(0,1,1,0,1,0,1)
sec2_a<-c(1,1)
sec2_b<-c(0,0,1,0,0,1,0,0,0,1)
n2_a<-length(sec2_a)
n2_b<-length(sec2_b)
resultado2_a<-0
resultado2_b<-0
resultado2<-0
for(i in 1:n2_a){
  resultado2_a <- resultado2_a + sec2_a[i]*2^(n2_a-i)
}
for(j in 1:n2_b){
  resultado2_b <- resultado2_b + sec2_b[j]*2^(-j)
}
resultado2 <- resultado2_a + resultado2_b
print(resultado2)

#Ejercicio 2 Suponiendo que 1,0110101dos y 11,0010010001dos son aproximadamente √2 y π, cual sería el error de:
#√2-1,0110101dos, usando como √2 = 1,41421356237309
sec2_a<-c(1)
sec2_b<-c(0,1,1,0,1,0,1)
n2_a<-length(sec2_a)
n2_b<-length(sec2_b)
resultado2_a<-0
resultado2_b<-0
resultado2<-0
for(i in 1:n2_a){
  resultado2_a <- resultado2_a + sec2_a[i]*2^(n2_a-i)
}
for(j in 1:n2_b){
  resultado2_b <- resultado2_b + sec2_b[j]*2^(-j)
}

valor_num_binario_a <- resultado2_a+resultado2_b
print(valor_num_binario_a)
cuenta_R_a <- 1.41421356237309-valor_num_binario_a
valor_teorico_a <- sqrt(2)-valor_num_binario_a
error_a <- valor_teorico_a-cuenta_R_a
print(error_a)
print(cuenta_R)
print(valor_teorico)
if(valor_teorico_a>cuenta_R_a){print("R redondea para abajo")} else {print("R redondea para arriba")}
#π-11,0010010001dos, donde π = 3,141592653,58979
sec2_a<-c(1,1)
sec2_b<-c(0,0,1,0,0,1,0,0,0,1)
n2_a<-length(sec2_a)
n2_b<-length(sec2_b)
resultado2_a<-0
resultado2_b<-0
resultado2<-0
for(i in 1:n2_a){
  resultado2_a <- resultado2_a + sec2_a[i]*2^(n2_a-i)
}
for(j in 1:n2_b){
  resultado2_b <- resultado2_b + sec2_b[j]*2^(-j)
}
valor_num_binario_b <- resultado2_a+resultado2_b
print(valor_num_binario_b)
cuenta_R_b <- 3.14159265358979-valor_num_binario_b
valor_teorico_b <- pi-valor_num_binario_b
error_b <- valor_teorico_b-cuenta_R_b
print(cuenta_R_b)
print(valor_teorico_b)
print(error_b)
if(valor_teorico_b>cuenta_R_b){print("R redondea para abajo")} else {print("R redondea para arriba")}

#Ejercicio 3 Probar que el desarrollo binario de 1/5 = 0.0011dos es equivalente a 1/5 = 3/16 + 3/256 +  3/4096+ .... usando la siguiente propiedad

sec2_a<-c(0)
sec2_b<-c(0,0,1,1)
n2_a<-length(sec2_a)
n2_b<-length(sec2_b)
resultado2_a<-0
resultado2_b<-0
resultado2<-0
for(i in 1:n2_a){
  resultado2_a <- resultado2_a + sec2_a[i]*2^(n2_a-i)
}
for(j in 1:n2_b){
  resultado2_b <- resultado2_b + sec2_b[j]*2^(-j)
}

valor_num_binario_a <- resultado2_a+resultado2_b
print(valor_num_binario_a)

valor_teorico <-1/5
print(valor_teorico)

valor_bin2dec <-valor_num_binario_a
error <- valor_teorico-valor_bin2dec
print(error)

#sumatoria
binomioxsumatoria <- 0
i<-0
c <-3/16
r<-1/16
n <-1000

while (i<=n) {
  binomioxsumatoria<-binomioxsumatoria + c*r^i
  i<-i+1
  
}
print(binomioxsumatoria)

#igualdad con abs(r)<1
binomioxcondicion <- numeric(1)
binomioxcondicion <-c/(1-r)
print(binomioxcondicion)

#binomioxsumatoria-binomioxcondicion
if(binomioxsumatoria==binomioxcondicion){
  cat("Demostrado")
} else print("No se cumple la igualdad con: c=")

#Ejercicio 4 Usando la tabla que pudo construir en el obligatorio 1 para generar los números dada una mantisa de 4 cifras y un exponente n = 8, efectuar los siguientes cálculos

#Ejercicio 5 Hallar el error absoluto Ex y relativo Rx y el número de cifras significativas de la aproximación
x <- 2.71828182
x_prima <- 2.7182

y=98350
y_prima = 98000

z=0.000068
z_prima = 0.00006

#funcion error absoluto
error_abs <- function(a,b){
  error<- abs(a-b)
  return(error)
}
#funcion error relativo
error_rel <- function(a,b){
  if(a!=0){
    error<- abs(a-b)/abs(a)
  } else error<- (-1)
  return(error)
}

#funcion num de cifras significativas de la aproximacion
#cifras en las que no existe error
#?

a<-2.71828182
b<-2.7182
error_abs(a,b)
error_rel(a,b)
abs(error_abs(a,b)-error_rel(a,b))

a<-98350
b<-98000
error_abs(a,b)
error_rel(a,b)
abs(error_abs(a,b)-error_rel(a,b))

a<-0.000068
b<-0.00006
error_abs(a,b)
error_rel(a,b)
abs(error_abs(a,b)-error_rel(a,b))


#Ejercicio 6 Completar el cálculo
p = 0.2553074606
x <- 1/4
res_integral <- x + (x^3)/3 + (1/(2*1*5))*x^5 + (1/(3*2*1*7))*x^7
error <- abs(p-res_integral)
print(error)
a<-p
b<-res_integral
error_abs(a,b)
error_rel(a,b)


#Ejercicio 7 Discutir la propagación de errores
p <-1.00001
p_prima <-1.00000
q <-2.11228
q_prima <-2.112
r <-3.11111
r_prima<-3.1

error_abs_p <-error_abs(p,p_prima)
error_rel_p <-error_rel(p,p_prima)
  
error_abs_q <-error_abs(q,q_prima)
error_rel_q <-error_rel(q,q_prima)

error_abs_r <-error_abs(r,r_prima)
error_rel_r <-error_rel(r,r_prima)

suma <- p+q+r
suma_prima <- p_prima + q_prima + r_prima

error_abs_suma <- error_abs(suma,suma_prima)
error_rel_suma <- error_rel(suma,suma_prima)

error_abs_p + error_abs_q + error_abs_r
error_rel_p + error_rel_q + error_rel_r

cociente <- p/q
cociente_prima_abs <- (p_prima + error_abs_p)/(q_prima + error_abs_q)
cociente_prima_rel <- (p_prima + error_rel_p)/(q_prima + error_rel_q)


error_abs_cociente <- error_abs(cociente,cociente_prima_abs)
error_rel_cociente <- error_rel(cociente,cociente_prima_rel)
error_abs_cociente
error_rel_cociente

producto <- p*q*r
producto_prima_abs <- (p_prima + error_abs_p)*(q_prima + error_abs_q)*(r_prima + error_abs_r)
producto_prima_rel <-(p_prima + error_rel_p)*(q_prima + error_rel_q)*(r_prima + error_rel_r)

error_abs_producto <- error_abs(producto,producto_prima_abs)
error_rel_producto <- error_rel(producto,producto_prima_rel)
print(error_abs_producto)
print(error_rel_producto)
error_rel_p*error_rel_q*error_rel_r
