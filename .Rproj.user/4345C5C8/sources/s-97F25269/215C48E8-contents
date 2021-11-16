matrix_fact <-
  function(R,P,Q,K, steps=5000,alpha=0.002,beta=0.02){
    #R rating matrix
    #P user features matrix
    #Q item (movie) features matrix
    #K latent features
    #steps = iterations
    #alpha = learning rate
    #beta = regularization param
    Q = t(Q)
    for (step in 1:steps){
      for (i in 1:nrow(R)){
        for (j in 1:ncol(R)){
          if (R[i,j] >0) {

            eij <- R[i,j] - (P[i,]%*%Q[,j])

            for (k in 1:K){
              P[i,k] = P[i,k] + alpha * (2 * eij * Q[k,j] - beta * P[i,k])
              Q[k,j] = Q[k,j] + alpha * (2 * eij * P[i,k] - beta * Q[k,j])
            }
          }
        }
      }

    e = 0

    for (i in 1:nrow(R)){

      for (j in 1:ncol(R)){

        if (R[i,j] > 0) {

          e = e + (R[i,j]-(P[i,]%*%Q[,j]))^2

          for (k in 1:K){
            e = e + (beta/2) * ((P[i,k])^2+ (Q[k,j])^2)
          }

        }

      }

    }

    if (e < 0.001){
      break
    }

    }

    return(list(
          P=P,
          Q = t(Q))
    )

  }

R <-
  matrix(c(3,5,0,1,
         4,0,0,1,
         1,1,0,5,
         1,0,0,4,
         0,1,5,4,
         2,1,3,0),nrow=6,byrow = TRUE)
R
R[1,2]
length(R[1,])

N <- nrow(R)
M <- ncol(R)
K = 3

P <- matrix(rexp(N*K), N)
Q <- matrix(rexp(M*K), M)


output <- matrix_fact(R,P,Q,K)
output

mf_example <- output$P %*% t(output$Q)
mf_example
