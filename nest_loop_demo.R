A <- data.frame(gene_1 = c(1:6), gene_2 = seq(10,60, by = 10), gene_3 = seq(5,35, by = 6))

B <- data.frame(gene_1 = c(11:16), gene_2 = seq(100,600, by = 100), gene_3 = seq(50,80, by = 6))




#-------------------FOR_LOOP------------------------------------------------------------------------------
C <- list()
D <- list()
for(j in 1:3){
  for(i in 1:3){
    C[[i]] <- lm(A[,j]~B[,i])
  }
  D[[j]] <- C
}



#------------------lapply----------------------------------------------------
Z <- list()
W <- list()
FUN_2 <- function(y){
  x <- c(1:3)
  FUN_1 <- function(x){
    lm(A[,y]~(B[,x])
  }
  W <- lapply(x, FUN_1)
  Z[[y]] <- W
}

y <- c(1:3)
Z <- lapply(y, FUN_2)




#------------------lapply----------------------------------------------------
FUN_2 <- function(y){
  x <- list(B[,1], B[,2], B[,3])
  FUN_1 <- function(x){
    lm(y~x)
  }
  W <- lapply(x, FUN_1)
  Z <- list(W)
}

y <- list(A[,1], A[,2], A[,3])
Z <- lapply(y, FUN_2)









