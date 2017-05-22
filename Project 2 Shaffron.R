rm(list=ls())

load("ncaaf_2010.rdata")

#initial rating/score
s <- numeric(1)

for (i in 1:120) {
  s[i] = (1+df$wins[i])/(2+df$wins[i]+df$losses[i])
}


#effective wins
n <- numeric(1)
for (i in 1:120) {
    n[i] = (df$wins[i]-df$losses[i])/2 + sum(s[df$opponents[[i]]])
      }
    


#new scores
q <- integer(1)
for (i in 1:120) {
    q[i] = (1+(n[i]))/(2+df$wins[i]+df$losses[i])
}

  while(max(abs(s-q)) >= .00001){
    for (i in 1:120) {
      s <- q
      n[i] = (df$wins[i]-df$losses[i])/2 + sum(s[df$opponents[[i]]])
      q[i] = (1+(n[i]))/(2+df$wins[i]+df$losses[i])
    }
  }

Rankings <- data.frame(df$teams,q)
names(Rankings) <- c("Team Name","Score")
Solution <- Rankings[order(-Rankings$Score),]

