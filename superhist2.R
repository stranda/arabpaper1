##x has to be a list of different vectors to compare
## i.e. superhist2(x=list(a,b))
superhist2 <- function(x, freq=T,  nbreaks ="Sturges",xl=NULL,verb=F,...)
{
  dev <- .Device
  if (verb)  print(dev)
  junk = NULL
  grouping = NULL
  for(i in 1:length(x))
    {
      junk = c(junk,x[[i]])
      grouping <- c(grouping, rep(i,length(x[[i]])))
    }
  grouping <- factor(grouping)
  n.gr <- length(table(grouping))
  xr <- range(junk)*1.05
  
  xr[1] <- xr[1]*0.9
  if (!is.null(xl)) xr <- xl
  histL <- tapply(junk, grouping, hist, breaks=nbreaks, plot = FALSE)
  if (freq==T)  maxC <- max(sapply(lapply(histL, "[[", "counts"), max))
  else   maxC <- max(sapply(lapply(histL, "[[", "density"), max))
  if((TC <- transparent.cols <- .Device %in% c("pdf", "png", "CairoPNG")))
    {
      cols <- hcl(h = seq(30, by=360 / n.gr, length = n.gr),
                  l = 65, alpha = 0.5)
    }
  else
    {
      h.den <- c(10, 15, 20)
      h.ang <- c(45, 15, -30)
    }
  if(TC)
    {
      plot(histL[[1]], xlim = xr, ylim= c(0, maxC), freq=freq,col = cols[1],...)
    }
  else
    {
      plot(histL[[1]], xlim = xr, ylim= c(0, maxC), freq=freq, density = h.den[1], angle = h.ang[1], ...)
    }
  if(!transparent.cols)
    {
      for(j in 2:n.gr) plot(histL[[j]], add = TRUE, freq=freq, density = h.den[j], angle = h.ang[j],...)
    }
  else
    {
      for(j in 2:n.gr) plot(histL[[j]], add = TRUE, freq=freq, col = cols[j],...) }
  invisible()
}
