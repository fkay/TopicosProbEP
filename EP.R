#source("C:/Users/Fabricio/OneDrive/IME-BMAC/7o Sem - 01_2019/MAE0699 - TOpicos de probabilidade/EP/EP_aux.R")
source("EP_aux.R")

fw = fit_dist(8,1000,0.4,"pois")
fw = fit_dist(8,1000,0.4,"geom")
fw = fit_dist(8,1000,0.4,"exp")

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

p = 1.3
z = c(1,2,3,4,5,6)
plot(d$distT[3:8], col='blue', type = "b")
lines(dexp(z, p), col = "red", type = "b")
