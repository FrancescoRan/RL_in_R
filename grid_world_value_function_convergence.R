# value function in grid world

# creo la prima griglia

Move_Grid_Reflect <- function(m, pMove, reward) {
  
  # questa funzione mi permette di "muovermi", nella griglia.
  # infatti scompone la griglia in 4 direzioni e ad ogni direzione ricrea la griglia 
  # ma spostata verso quella direzione. es. verso nord ricopia la prima riga della matrice originale e la mette 
  # nella prima riga della nuova matrice_nord. per le altre fa lo stesso e poi le somma.
  # moltiplico per la prob di spostarsi e ci tolgo il reward (costante) ad ogni nuovo stato raggiunto
  
  # i muri riflettono: gli stati fuori dal grid hanno lo stesso valore di quelli immediatamente precedenti!!
  mW = cbind( m[,1], m[,-ncol(m)] )
  mN = rbind( m[1,], m[-nrow(m),] )
  mE = cbind( m[,-1], m[,ncol(m)] )
  mS = rbind( m[-1,], m[nrow(m),] ) 
  
  mNew <- pMove*(mN + mE + mS + mW) + reward
  mNew[1,1] <- mNew[I,J] <- mNew[1,J] <- mNew[I,1] <- 0 # sono le celle target, zero valore per loro

  mNew[round(I/2),round(J/2)] <- gw_array[round(I/2),round(J/2),1] # sono gli ostacoli
  mNew[round(I/4),round(J/3)] <- gw_array[round(I/4),round(J/3),1]
  mNew[round(I/2),round(J/6)] <- gw_array[round(I/2),round(J/6),1]
  
  return(mNew)
} 
I <- 60
J <- 60
K <- 10000

gw_array        <- array(data = NA, dim = c(I,J,K))
gw_array[,,1]   <- matrix(rep(-1,each=I), nrow = I, ncol = J, byrow = TRUE)
gw_array[1,1,1] <- gw_array[I,J,1] <- gw_array[1,J,1] <- gw_array[I,1,1] <- 0

gw_array[round(I/2),round(J/2),1] <- -500 # ostacoli che penalizzano
gw_array[round(I/4),round(J/3),1] <- -500
gw_array[round(I/2),round(J/6),1] <- -500

for(k in 2:K){
  gw_array[,,k] <- Move_Grid(gw_array[,,k-1], pMove = 0.25, reward = -2)
}

Move_Grid_noReflect <- function(m, pMove, reward) {
  
  # questa funzione mi permette di "muovermi", nella griglia.
  # infatti scompone la griglia in 4 direzioni e ad ogni direzione ricrea la griglia 
  # ma spostata verso quella direzione. es. verso nord ricopia la prima riga della matrice originale e la mette 
  # nella prima riga della nuova matrice_nord. per le altre fa lo stesso e poi le somma.
  # moltiplico per la prob di spostarsi e ci tolgo il reward (costante) ad ogni nuovo stato raggiunto
  
  # i muri non riflettono ma penalizzano di penality
  penality <- -100
  mW <- cbind(rep(penality,nrow(m)), m[,-ncol(m)]) 
  mN <- rbind(rep(penality,ncol(m)), m[-nrow(m),])
  mE <- cbind(m[,-1], rep(penality,nrow(m)))
  mS <- rbind(m[-1,], rep(penality,ncol(m)))
  
  mNew <- pMove*(mN + mE + mS + mW) + reward
  mNew[1,1] <- mNew[I,J] <- mNew[1,J] <- mNew[I,1] <- 0 # sono le celle target, zero valore per loro
  
  mNew[round(I/2),round(J/2)] <- gw_array[round(I/2),round(J/2),1] # sono gli ostacoli
  mNew[round(I/4),round(J/3)] <- gw_array[round(I/4),round(J/3),1]
  mNew[round(I/2),round(J/6)] <- gw_array[round(I/2),round(J/6),1]
  
  return(mNew)
} 

I <- 60
J <- 60
K <- 10000

gw_array        <- array(data = NA, dim = c(I,J,K))
gw_array[,,1]   <- matrix(rep(-1,each=I), nrow = I, ncol = J, byrow = TRUE)
gw_array[1,1,1] <- gw_array[I,J,1] <- gw_array[1,J,1] <- gw_array[I,1,1] <- 0

gw_array[round(I/2),round(J/2),1] <- -500 # ostacoli che penalizzano
gw_array[round(I/4),round(J/3),1] <- -500
gw_array[round(I/2),round(J/6),1] <- -500

for(k in 2:K){
  gw_array[,,k] <- Move_Grid(gw_array[,,k-1], pMove = 0.25, reward = -2)
}
require(raster)
plot(raster::brick(gw_array[,,c(1,20,50,80,130,170,seq(200,K,length.out = 10))]),
     axes=FALSE, main="Value function")

### COMMENTO

# siccome il reward è -1 , e la probabilità di spostarsi è 1/4 ovvero uniforme, il valore della 
# state value function (ogni celletta della griglia) si interpretaa come il numero di passi necessari
# a raggiungere 1 dei 4 target agli angoli
