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
  return(Matrix::forceSymmetric(A, uplo = "U"))
}

testeSample <- function(n, sampleSz, p) {
  # array com a distribuicao
  distT = array(0,n+1)
  # para cada distribuicao faz 1000 testes
  for(i in 1:sampleSz) {
    #cat(sprintf("."));
    A = generateMatrix(n, p)
    for(v in 1:n) {
      T = findPath(v,v,A,0,n+1)
      distT[T] = distT[T] + 1
    }
  }
  distT = distT / (sampleSz * n)
  cat(sprintf("\n"))
  cat(sprintf("n = %d\n", n))
  # prepara tabela para impressao
  df <- data.frame(t(distT), row.names = 'Distr. T')
  names <- sprintf("%d", 1:(n+1))
  names[n+1] = "inf"
  colnames(df) = names
  #print(df)
  print(format(df, digits = 3, nsmall = 4, justify = 'centre'))
  return(distT)
}