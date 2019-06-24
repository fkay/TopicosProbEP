#source("C:/Users/Fabricio/OneDrive/IME-BMAC/7o Sem - 01_2019/MAE0699 - TOpicos de probabilidade/EP/EP_aux.R")
source("EP_aux.R", encoding = 'utf-8')


fw = fit_dist(10,1000,0.4,"pois")
fw = fit_dist(10,1000,0.4,"geom")
fw = fit_dist(10,1000,0.4,"exp")

N = 10
p = 0.7

A = generateMatrix(N, p)
print(A)
for(i in 1:N) {
  Ti = findPath(i,i,A,0,N+1)
  cat(sprintf("T(%d) = %d\n",i, Ti))
}


testeSamplesT(8, 1000, 0.4)

testeSamplesT(8:12, 100, 0.4, overlap = TRUE)

testeSamplesC(8:9, 100, 0.4)

m = testeAmostras(n = 8, c(25, 50, 75, 100, 150, 200, 250, 
                           300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800), p = 0.4)

library(igraph)
plot(graph_from_adjacency_matrix(A, mode = 'undirected', weighted = TRUE))

#p = 1.40
#z = c(1,2,3,4,5,6,7,8)
#d = testeSampleT(10, 1000, 0.4)
#plot(d$distT[3:10], col='blue', type = "b")
#origin = dexp(z, p)
#lines(origin, col = "red", type = "b")


#p = 0.834376863446631

n = 10
N = 1000
p = 0.4

dd = testeSampleT(n, N, p)
distT = dd$distTAcum
dT = distT[3:n]
dTp = dT / sum(dT)
dTP = dT / sum(distT)
c = rep(1,dT[1])
for(i in 2:length(dT)) 
  c = append(c, rep(i,dT[i]))
lambd = mean(c)
geo = 1/lambd
plot(c(NA,NA,dTp), x = 1:n, col="blue", type = "b", xlab="T", ylab="Probabilidade")
lines(c(NA,NA,dTP), col = "darkgray", type = "b")
lines(c(0.4,0,dTp*sum(dTP),1-(sum(dTp*sum(dTP)) -0.4)), col = "green", type = "b")
Exponencial = dexp(3:n, lambd)
curve(Exponencial, xlim=c(3,n), col = "green", type = "b", add = T)

curve(dexp(x-3, lambd), xlim=c(3,n), col = "purple", type = "b", add = T)
lines(dpois(c(NA,NA,3:(n-1)), lambd ), col = "black", type = "b", add = T)
lines(dgeom(0:(n-2),geo), x = 3:(n+1), col = "green", type = "l")
grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

#CALCULAR AS DISTRIBUICOES
geometrica = dgeom(0:(n-3),geo)
Exponencial = dexp(0:(n-2), lambd)
pois = dpois(0:(n-2),lambd)

#DISTRIBUICAO PARA C

n = 10
N = 100
p = 0.4

d = testeSampleC(n, N, p)
distC = d$distCAcum
dT = distC[2:n]
dTp = dT / sum(dT)
dTP = dT / sum(distC)
c = rep(1,dT[1])
for(i in 2:n) 
  c = append(c, rep(i,dT[i]))
lambd = mean(c)
geo = 1/lambd
plot(c(NA,dTp), x = 1:n, col="white", type = "b", xlab="N? de Vertices", ylab="Probabilidade C")
lines(dgeom(0:(n-2),geo), x = 2:(n), col = "green", type = "l")
curve(dexp(x-2, lambd), xlim=c(2,n), col = "purple", type = "b", add = T)
lines(dpois(c(NA,2:(n-2)), lambd ), col = "black", type = "b", add = T)


#DISTRIBUICOES C
geometrica = dgeom(0:(n-2),geo)
exponencial = dexp(0:(n-2), lambd)

diferenca1 = sum((geometrica-dTp)^2)
diferenca2 = sum((exponencial-dTp)^2)
chi2 = sum((geometrica-dTp)^2/geometrica) 

lines(c(0.4,geometrica*sum(dTP),1 - sum(geometrica*sum(dTP)) - 0.4), col = "green", type = "b")
  
lines(c(NA,dTP), col = "darkgray", type = "b")
lines(c(NA,dTp), col = "green", type = "b")
