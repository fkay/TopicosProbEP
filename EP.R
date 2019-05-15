#source("C:/Users/Fabricio/OneDrive/IME-BMAC/7o Sem - 01_2019/MAE0699 - Tópicos de probabilidade/EP/EP_aux.R")
source("EP_aux.R")

N = 10
p = 0.2

A = generateMatrix(N, p)
print(A)
for(i in 1:N) {
  Ti = findPath(i,i,A,0,N+1)
  cat(sprintf("T(%d) = %d\n",i, Ti))
}

# Testa a distribuição de T para vários N
testeSamples <- function(range_n, sampleSz, p) {
  for(n in range_n) {
    cat(sprintf("Checking for n = %d ", n))
    distT = testeSample(n, sampleSz, p)
    plot(distT, main = sprintf("n = %d", n), xlab = "T", ylab = "P(T)", type = "b",  col="red")
  }
}

testeSamples(8:9, 10, 0.3)