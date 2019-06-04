#source("C:/Users/Fabricio/OneDrive/IME-BMAC/7o Sem - 01_2019/MAE0699 - TOpicos de probabilidade/EP/EP_aux.R")
source("EP_aux.R")


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

d = testeSampleT(n, N, p)
distT = d$distTAcum
dT = distT[3:n]
dTp = dT / sum(dT)
dTP = dT / sum(distT)
c = rep(1,dT[1])
for(i in 2:n) 
  c = append(c, rep(i,dT[i]))
lambd = mean(c)
geo = 1/lambd
plot(c(NA,NA,dTp), x = 1:n, col="blue", type = "b", xlab="Nº de Vertices", ylab="Probabilidade")
lines(c(NA,NA,dTP), col = "darkgray", type = "b")
curve(dexp(x-2, 0.834601), xlim=c(2,n), col = "orange", type = "b", add = T)
curve(dpois(x, 1.209475), xlim=c(0,n-2), col = "black", type = "s", add = T)
lines(dgeom(0:(n-2),geo), x = 1:(n-1), col = "green", type = "b")
grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)


