###
### simulation-based power analysis for the project described in this folder
###

####function to simulate fitness reps from a single line
simreps <- function(n,mean,sd=0,propng=0.1,propdie=0.1)
    {
        if (sd==0){sd = exp(1.06+0.62*log(mean))} #this is the mean/sd relationship
        ret <- rnorm(n,mean,sd)
        ret[ret<0] <- 0
        ret[runif(n)<propng] <- 0 #add in non-germinants
        ret[runif(n)<propdie] <- 0 #add in deadplants
        df <- data.frame(pheno = ret)
        df$mean <- mean
        df$sd <- sd
        df
    }

####function to simulate means of lines
choosemeans <- function(n)
    {
        ##weibull with these params best fits the observed means of each line in the initial experiment
        ##
        rweibull(n,shape=1.2549,scale=34.84)
    }

simulate.dataset <- function(lines=115, control=37, reps=10, control.reps=120, sd=NULL, propng=0.1, propdie=0.1)
    {
        linemeans <- c(choosemeans(lines)) #make the line means to simulate from
        retdf <- do.call(rbind,lapply(linemeans,
                                      function(x){
                                          df=simreps(reps,x,sd,propng,propdie);df$line=paste0(x,runif(1));df})
                         )
        retdf$line <- as.numeric(unclass(as.factor(retdf$line)))
        ctrl <- simreps(control.reps,control,sd,propng,propdie)
        ctrl$line <- 0
        df <- rbind(retdf,ctrl)
        lines <- df%>%group_by(line)%>%summarise(mx=max(pheno))
        lines <- lines[lines$mx>0,]
        df <- df[as.character(df$line)%in%as.character(lines$line),]
    }

fix.lines <- function(df)
{
    df$line <- relevel(factor(df$line),"0")
    df
}
analyze.zeroinf.nb <- function(df)
    {
        df <- fix.lines(df)
        fit <- zeroinfl(round(pheno)~line,data=df,dist="negbin")
        cf <- data.frame(summary(fit)$coefficients[[1]])
        cf <- cf[-c(1,dim(cf)[1]),]
        names(cf) <- c("Estimate","StdErr","z","p.value")
        cf$sig <- cf$p.value<0.05
        cf$bonsig <- cf$p.value<0.05/dim(cf)[1]
        cf$sign <- ifelse(cf$Estimate<0,"below","above")
        list(sig=with(cf,table(sig,sign)[2,])/dim(cf)[1],
             bonsig=with(cf,table(bonsig,sign)[2,])/dim(cf)[1])
    }

analyze.shuffle <- function(df)
    {
        fitline <- with(df,aggregate(pheno,by=list(line=line),mean))
        avg.reps <- round(mean(table(df$line[df$line!="0"])))
        names(fitline)[2] <- "pheno"
        numreps <- 10*dim(fitline)[1]
        cntrlmeans <- rep(0,numreps)
        for (i in 1:numreps)
            {
                cntrlmeans[i] <- mean(
                    sample(df$pheno[as.character(df$line)=="0"],size=round(avg.reps))
                    )
            }
        rng <- range(cntrlmeans)
        qntile <- quantile(cntrlmeans,c(0.025,0.975))
        list(range=c("above"=sum(fitline$pheno>rng[2]),"below"=sum(fitline$pheno<rng[1]))/(dim(fitline)[1]-1),
             quant=c("above"=sum(fitline$pheno>qntile[2]),"below"=sum(fitline$pheno<qntile[1]))/(dim(fitline)[1]-1))
    }


##############################################
## simulate
##
require(dplyr)
require(pscl)
require(parallel)

treats <- expand.grid(n=c(2,6,10,15,20,30,40),
                      #n=20,
                      ctrl.reps=c(120,20,50),
                      sd=c(0,12,24,48),
                      replicate=1:100)
treats <- treats[treats$ctrl.reps>=treats$n,]

res <- do.call(rbind,mclapply(1:dim(treats)[1],
                mc.cores=4,
                function(i){
                    print(i)
                    df <- simulate.dataset(reps=treats$n[i],control.reps=treats$ctrl.reps[i],sd=treats$sd[i])
                    mns <- unique(df$mean)
                    ctrl.quant <- sum(mns<=df$mean[as.character(df$line)=="0"])/length(mns)
                    shuffle=tryCatch(analyze.shuffle(df),error=function(e){NULL})
                    if (!is.null(shuffle)){shufflevec <- c(shuffle$range[1],shuffle$range[2],
                                                           shuffle$quant[1],shuffle$quant[2])
                                       } else {shufflevec <- rep(NA,4)}
                    zero=tryCatch(analyze.zeroinf.nb(df),error=function(e){NULL})
                    if (!is.null(zero)){zerovec <- c(zero$sig[1],zero$sig[2],
                                                     zero$bonsig[1],zero$bonsig[2])
                                    } else {zerovec <- rep(NA,4)}
                    mat <- matrix(c(unlist(treats[i,]),shufflevec,zerovec,ctrl.quant),nrow=1)
                    colnames(mat) <- c("n","ctrl.reps","sd","replicate",
                                       "shuffle.range.above","shuffle.range.below",
                                       "shuffle.quant.above","shuffle.quant.below",
                                       "zero.sig.above","zero.sig.below",
                                       "zero.bonsig.above","zero.bonsig.below",
                                       "ctrl.quant"
                                       )
                    data.frame(mat)
                        
                })
)

save(file="truepower.rda",res)

