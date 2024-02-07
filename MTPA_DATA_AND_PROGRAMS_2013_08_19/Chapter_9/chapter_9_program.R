# Game-day Simulator for Baseball
library(lattice)  # graphics package used to create probability matrix visual
simulator <- function(away.mean,home.mean,niterations) { 
# input is runs scored means output is probability of winning for away team
away.game.score <- numeric(niterations)
home.game.score <- numeric(niterations)
away.win <- numeric(niterations)
i <- 1
while (i < niterations + 1) { 
  away.game.score[i] <- rnbinom(1,mu=away.mean, size = 4)
  home.game.score[i] <- rnbinom(1,mu=home.mean, size = 4)
  if(away.game.score[i]>home.game.score[i]) away.win[i] <- 1
  if(away.game.score[i]>home.game.score[i] || 
  away.game.score[i]<home.game.score[i]) i <- i + 1 
  }
n.away.win <- sum(away.win)
n.away.win/niterations  # return probability of away team winning 
} 
set.seed(1234)  # set to obtain reproducible results 
niterations <- 100000  # set to smaller number for testing
# probability matrix for results... home team is rows, away team is columns
probmat <- matrix(data = NA, nrow = 9, ncol = 9,  
  dimnames = list(c(as.character(1:9)), c(as.character(1:9)))) 
for (index.home in 1:9)
for (index.away in 1:9)
if (index.home > index.away) {
  probmat[index.home,index.away] <- 
    simulator(index.away, index.home, niterations) 
  probmat[index.away,index.home] <- 1 - probmat[index.home, index.away] 
  }
# create probability matrix visual
x <- rep(1:nrow(probmat),times=ncol(probmat))
y <- NULL
for (i in 1:ncol(probmat)) y <- c(y,rep(i,times=nrow(probmat)))
probtext <- sprintf("%0.3f", as.numeric(probmat))  # fixed format 0.XXX
text.data.frame <- data.frame(x, y, probtext)
text.data.frame$probtext <- as.character(text.data.frame$probtext)
text.data.frame$probtext <- ifelse((text.data.frame$probtext == "NA"),
    NA,text.data.frame$probtext)  # define diagonal cells as missing
text.data.frame <- na.omit(text.data.frame)  # diagonal cells
print(levelplot(probmat, cuts = 25, tick.number = 9,
  col.regions=colorRampPalette(c("violet", "white", "light blue")),
  xlab = "Visiting Team Runs Expected", 
  ylab = "Home Team Runs Expected",
  panel = function(...) {
    panel.levelplot(...)  
    panel.text(text.data.frame$x, text.data.frame$y, 
    labels = text.data.frame$probtext)
    }))
# suggestion for students: develop similar tables for football and basketball   


