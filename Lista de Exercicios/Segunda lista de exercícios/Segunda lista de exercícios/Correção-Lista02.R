steven<- c(0,1,0)
garnit<- c(0,0,1)
vitoria_steven<- 0
vitoria_garnit<- 0

for (k in 1:10000) {
  resultado <- sample(0:1,size = 3,replace = TRUE)

while(sum(resultado == steven)< 3 & sum(resultado == garnit)<3){
  resultado <- c(resultado[2:3],sample(0:1,size = 1))
}
if(sum(resultado == steven) == 3){
  vitoria_steven <- vitoria_steven +1
}else{
  vitoria_garnit <- vitoria_garnit +1
}}

vitoria_steven
vitoria_garnit
