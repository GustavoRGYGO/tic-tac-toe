tab <- matrix(0,nrow = 3,ncol = 3)
i <- 0
vez <- -1

while(i<9){
  vez <- vez*-1
  x <- as.numeric(readline("escolha a linha="))
  y <- as.numeric(readline("escolha a coluna="))
  if (tab[x,y] != 0){
    print("vc eh burro, jogue novamente")
    vez <- vez*-1
    i <- i-1
  }
  else{
    tab[x,y] <- vez
  }
  if(win(tab)){
    print(paste("ganhou!", vez))
    break
  }
  if(i==8){
    print("VELHA")
  }
  print(tab)
  i <- i+1
 
 
  }
  win <- function(tab){
  antidiag <- c(tab[1,3],tab[2,2],tab[3,1])
  casos <- c(colSums(tab), rowSums(tab), sum(diag(tab)), sum(antidiag))
  any(abs(casos)==3)
}
