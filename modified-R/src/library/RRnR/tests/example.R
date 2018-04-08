run <- function()
{
  tangent <- function(y, x) y/x
  adjacent <- function(a, c) cospi(a / 180.0)*c
  opposite <- function(a, c) sinpi(a / 180.0)*c
  
  scale <- 100
  angles <- floor(runif(100, min=0, max=180*scale))/scale
  
  tangents <- c()
  for(i in 1:length(angles))
  {
    adj <- adjacent(angles[i], 1)
    opp <- opposite(angles[i], 1)
    tan <- tangent(opp, adj)
    
    tangents[i] <- tan
  }
  
  mean(tangents)
}

detect <- function(res) is.infinite(res)

percentage <- function(sz)
{
  cnt <- 0
  for(i in 1:sz)
  {
    m <- run()
    if(detect(m)) cnt <- cnt + 1
  }
  
  100 * cnt / sz
}
