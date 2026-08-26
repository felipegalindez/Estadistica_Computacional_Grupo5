#Ejercicio 1 Usando los métodos vistos en el curso convertir los números binarios a números decimales
#10101dos = 1*2^4 + 0*2^3 + 1*2^2 + 0*2^1 +1* 2^0
#11111110dos = 1*2^7 + 1*2^6 + 1*2^5 + 1*2^4 + 1*2^3 + 1*2^2 + 1*2^1 +0* 2^0
#111000dos = 1*2^5 + 1*2^4+1*2^3 + 0*2^2 + 0*2^1 +0* 2^0
sec <- c(1,1,1,1,1,1,1,0)
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
