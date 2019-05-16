# funcoes de apoio
findPath <- function(vEnd, vStart, paths, T, Tmax) {
  # verifica se esse caminho ja esta maior ou igual ao melhor encontrado
  if(T >= Tmax) {
    return(Tmax)
  }
  
  # verifica loop somente no primeiro passo
  if (T == 0 && paths[vStart, vEnd] == TRUE) {
    Tmax = 1
  }
  else {
    # varre todos os caminhos possiveis partindo de vStart
    for(j in 1:length(paths[1,])) {
      # desconsidero loops
      if(j != vStart) {
        # se existe caminho de vStar -> j
        if (paths[vStart, j] == TRUE) {
          # se cheguei ao final, retorno o valor encontrado de passos
          if (j == vEnd) {
            return(T + 1)
          }
          else {
            # desabilito os caminhos de volta
            paths[vStart,j] = paths[j,vStart] = FALSE   
            
            # chamada recursiva para os caminhos no próximo passo
            temp = findPath(vEnd, j, paths, T + 1, Tmax)
            
            # verifica se encontrou um caminho menor que o guardado
            if (temp < Tmax) {
              Tmax = temp
            }
          }
        }
      }
    }
  }
  return(Tmax)
}

# gera matriz triangular, com tamanho NxN probabilidade de caminhos p
generateMatrix <- function(N, p) {
  A = matrix(runif(N*N) < p, N, N)
  return(as.matrix(Matrix::forceSymmetric(A, uplo = "U")))
}

testeSampleT <- function(n, sampleSz, p) {
  # array com a distribuicao
  distT = array(0,n+1)
  # para cada distribuicao faz sampleSz testes
  for(i in 1:sampleSz) {
    #cat(sprintf("."));
    A = generateMatrix(n, p)
    for(v in 1:n) {
      T = findPath(v,v,A,0,n+1)
      distT[T] = distT[T] + 1
    }
  }
  distTP = distT / (sampleSz * n)
  cat(sprintf("\n"))
  cat(sprintf("n = %d\n", n))
  # prepara tabela para impressao
  df <- data.frame(t(distTP), row.names = 'Distr. T')
  names <- sprintf("%d", 1:(n+1))
  names[n+1] = "inf"
  colnames(df) = names
  print(format(df, digits = 3, nsmall = 4, justify = 'centre'))
  return(list("distT" = distTP, "distTAcum" = distT))
}

# Testa a distribuição de T para vários tamanhos de grafo
testeSamplesT <- function(range_n, sampleSz, p) {
  for(n in range_n) {
    d = testeSampleT(n, sampleSz, p)
    plot(d$distT, main = sprintf("n = %d", n), xlab = "T", ylab = "P(T)", type = "b", col="red")
  }
}

testeSampleC <- function(n, sampleSz, p) {
  # array com a distribuicao
  distC = array(0,n+1)
  # para cada distribuicao faz sampleSz testes
  for(i in 1:sampleSz) {
    #cat(sprintf("."));
    A = generateMatrix(n, p)
    for(v in 1:n) {
      for(w in 1:n) {
        C = findPath(w,v,A,0,n+1)
        distC[C] = distC[C] + 1
      }
    }
  }
  distCP = distC / (sampleSz * n)
  cat(sprintf("\n"))
  cat(sprintf("n = %d\n", n))
  # prepara tabela para impressao
  df <- data.frame(t(distCP), row.names = 'Distr. C')
  names <- sprintf("%d", 1:(n+1))
  names[n+1] = "inf"
  colnames(df) = names
  print(format(df, digits = 3, nsmall = 4, justify = 'centre'))
  return(list("distC" = distCP, "distCAcum" = distC))
}

testeSamplesC <- function(range_n, sampleSz, p) {
  for(n in range_n) {
    cat(sprintf("Checking for n = %d ", n))
    d = testeSampleC(n, sampleSz, p)
    plot(d$distC, main = sprintf("n = %d", n), xlab = "C", ylab = "P(C)", type = "b",  col="blue")
  }
}

testeAmostras <- function(n, samples, p) {
  vals = matrix(0, ncol = 0, nrow = n+1)
  distAcum = array(0, n+1)
  for(i in 1:length(samples)) {
    if(i==1) {
      sample = samples[i]
    }
    else {
      sample = samples[i] - samples[i-1]
    }
    cat(sprintf("Checking for sample = %d", samples[i]))
    d = testeSampleT(n, sample, p)
    distAcum = distAcum + d$distTAcum
    distT = (distAcum) / (samples[i] * n)
    vals = cbind(vals, distT)
  }
  colnames(vals) = sprintf("%d", samples)
  sSz = length(samples)
  m = vals[,2:sSz] - vals[,1:(sSz-1)]
  m = m^2
  vals = colSums(m)
  plot(names(vals), vals, main = sprintf("Convergencia dos erros para n = %d", n) , xlab = "Tamanho da Amostra", ylab = "Erro", type = "b",  col="green")
  return(vals)
}
