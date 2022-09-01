
# perform the exceedance projection analysis
analysis <- function(params){
  
  # unpack input parameters
  
  MRI.new <- params$MRI.new
  df <- params$df
  MRI.all <- params$MRI.all
  y.label <- params$y.label
  Title <- params$Title
  
  n.dat <- nrow(df)
  n.proj <- n.dat + length(MRI.new)
  
  # Get starting values for nonlinear least squares fit by linear Gumbel fit
  
  df$p <- 1/df$MRI
  df$Qgum <- -log(-log(1-df$p))
  parms <- as.numeric(coefficients(lm(y~Qgum,data=df)))
  
  mu0 <-    parms[1]
  sigma0 <- parms[2]
  k0 <-     0
  start=list(mu=mu0,sigma=sigma0,k=k0)
  
  #  Use linear fit if n = 2  
  # Compute nonlinear least squares fit
  
  if(n.dat >= 3 ) {
    fit <- nls(y ~ mu + sigma*qgev(1-p,k), start=start,data=df, control = list(maxiter = 50, tol = 1e-50, minFactor = 1/1024, printEval = FALSE, warnOnly = TRUE))
    coefficients(fit)
    parms.nl <- signif(as.numeric(coefficients(fit)),3)
    mu <- parms.nl[1]; sigma <- parms.nl[2]; k <- parms.nl[3]
  } else {
    parms.nl <- c(mu0,sigma0,k0)
    mu <- parms.nl[1]; sigma <- parms.nl[2]; k <- parms.nl[3]
  }
  # simulated error bars for exceedances 
  
  ret.levels <- as.data.frame(matrix(rep(NA,n.proj), nrow=1))
  colnames(ret.levels) <- paste0(MRI.all,"-yr")
  for(rep in 1:1000) {
    rgev <- rgev(100,xi=k,mu=mu,beta=sigma)
    GEV.fit <- fevd(rgev, type = c("GEV"), method = c("MLE"),
                    threshold = NULL,time.units="years")
    ret.levels[rep,]<- return.level(GEV.fit,return.period=MRI.all,do.ci=FALSE)
  } 
  
  quantiles<-as.data.frame(matrix(rep(NA,6),nrow=1))
  q <- c(.05,.25,.50,.75,.95)
  colnames(quantiles)<-c("MRI",paste0("P",100*q))
  for(m in 1:n.proj){
    quantiles[m,] <- c(MRI.all[m],quantile(ret.levels[,m],q))
  }
  
  quantiles$p <- 1/quantiles$MRI
  quantiles$Qgum <- -log(-log(1-quantiles$p))
  
  # report results
  
  df.proj <- as.data.frame(MRI<-c(df$MRI,MRI.new))
  colnames(df.proj) <- "MRI"
  df.proj$p <- 1/df.proj$MRI
  df.proj$Qgum <- -log(-log(1-df.proj$p))
  df.proj$y <- mu + sigma*qgev(1-df.proj$p,k) 
  
  ylim <- c(min(c(df$y,df.proj$y,quantiles[,2]),na.rm=TRUE), 
            max(c(df.proj$y,df$y,quantiles[,6]),na.rm=TRUE))
  plim <- c(min(c(df$p,df.proj$p),na.rm=TRUE), 
            max(c(df$p,df.proj$p),na.rm=TRUE))
  Qlim <- c(min(df.proj$Qgum),max(df.proj$Qgum))
  
  # prediction curve
  
  pgrid <- rev(plim[1] + c(0:100)*diff(plim)/100)
  Qgrid <- -log(-log(1-pgrid))
  ygrid <- mu+sigma*qgev(1-pgrid,k)
  
  # plot observed, projected, fit
  
  plim <- c(min(df.proj$p),max(df.proj$p))
  
  xticks <- qgev(1-df.proj$p,0)
  xtic.labs <- as.character(df.proj$MRI)
  
  
  # simulated error bars
  
  x.seb   <- c(quantiles$Qgum,rev(quantiles$Qgum))
  y90.seb <- c(quantiles$P95,rev(quantiles$P5))
  
  
  y.proj <- signif(ygrid[length(ygrid)],3)
  ci.proj <- signif(quantiles[n.proj,c("P5","P95")],3)
  
  # generate plot
  
  plot(NULL,ylim=ylim,xlim=Qlim,xaxt="n",
       ylab=y.label,
       xlab="MRI (yrs)",col="black",main=Title,pch=16)
  axis(side=1,at=xticks,labels=xtic.labs)
  polygon(y=y90.seb,x=x.seb,col="lightgray",border=NA)
  points(y ~ Qgum, data=df.proj[1:n.dat,],pch=16)
  points(y ~ Qgum,data=df.proj[(n.dat+1):n.proj,],pch=16,col="red")
  lines(ygrid ~ Qgrid,lty=2)
  legend("topleft",
         legend=c("Given",paste0("mu=",mu,", sigma=",sigma,", k=",k),
                  paste0("Projected"),
                  "Resampled 90% Confidence"),
         bty="n",lty=c(NA,2,NA,NA),pch=c(16,NA,16,15),col=c("black","black","red","lightgray"))
  
  # print quantiles
  
  quantiles[,c(2:6,8)] <- signif(quantiles[,c(2:6,8)],3)
  quantiles$mean <- signif(df.proj$y,3)
  print(quantiles[,c(1:6,9)])
  
}