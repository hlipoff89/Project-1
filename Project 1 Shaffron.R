#Hilary Lipoff

rm(list=ls())

#creating vector b
rm(list=ls())

load("ncaaf_2010.rdata")

bvector <- function(b){
  1 + (df$wins-df$losses)/2
}

b<- bvector(df$teams)

#creating matrix 
A <- matrix(rep(0,14400), nrow = 120)

for (i in 1:120) {
  A[i,i]  = 2 + df$wins[i] + df$losses[i]
}

for (j in 1:120){
  for (i in 1:(df$wins[j]+df$losses[j])) {
    A[j,df$opponents[[j]][i]]=A[j,df$opponents[[j]][i]]-1
  }
  
}

TeamRankings <- data.frame(df$teams, solve(A,b))
names(TeamRankings) <- c("Names","Strength")
TeamRankings <- TeamRankings[order(-TeamRankings$Strength),]

