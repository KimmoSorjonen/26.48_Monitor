

############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

## Loading packages

library(readxl)
library(metafor)
library(qgraph)

## Download Excel file from OSF

download.file(
  "https://osf.io/download/hf6ku",
  destfile = "dataset.xlsx",
  mode = "wb"
)

## Load dataset into R

dforg <- read_excel("dataset.xlsx")

df1 <- data.frame(scale(dforg[,4:12])) ## time 1
df2 <- data.frame(scale(dforg[,13:21])) ## time 2

names(df1) <- names(df2) <-
  c("PMC1","PMC2","PMC3","PMC4","PMC5",
    "MHL1","MHL2","MHL3","MHL4")

#######################
## The MultiNet function
## taking two data sets,
## a vector of names (optional),
## a vector of colors,
## and a vector of letter locations (optional) as input

MultiNet <- function(data1,data2,namevec=names(data1),colvec,legvec=rep("topleft",4)){
  
  datadiff <- data2-data1 ## difference scores
  
  datamat1 <- datamat2 <- datamat3 <- datamat4 <- datamat5 <- vector() ## to be filled below
  
  for(x in 1:ncol(data1)){ ## looping through all variables as x
    
    datarow1 <- datarow2 <- datarow3 <- datarow4 <- datarow5 <- vector() ## to be filled below
    
    for(d in 1:ncol(datadiff)){ ## looping through difference scores
      
      if(x==d){ ## if outcome = x
        eff1 <- eff2 <- eff3 <- eff4 <- 0
        i2 <- NA}
      
      if(x!=d){ ## if outcome not equal to x
        
        fit1 <- lm(datadiff[,d] ~ data1[,x]+data1[,d]) ## model 1
        fit2 <- lm(datadiff[,d] ~ data1[,x]+data2[,d]) ## model 2
        fit3 <- lm(datadiff[,d] ~ data1[,x]) ## model 3
        
        pv1 <- summary(fit1)$coefficients[2,4] ## p-value 1
        pv2 <- summary(fit2)$coefficients[2,4] ## p-value 2
        pv3 <- summary(fit3)$coefficients[2,4] ## p-value 3
        
        eff1.temp <- fit1$coefficients[2] ## effect 1
        eff2.temp <- fit2$coefficients[2] ## effect 2
        eff3.temp <- fit3$coefficients[2] ## effect 3
        
        se1 <- summary(fit1)$coefficients[2,2] ## se 1
        se2 <- summary(fit2)$coefficients[2,2] ## se 2
        se3 <- summary(fit3)$coefficients[2,2] ## se 3
        
        ball <- c(eff1.temp,eff2.temp,eff3.temp) ## all effects
        seall <- c(se1,se2,se3) ## all standard errors
        
        b.fish <- 0.5*log((1+ball)/(1-ball)) ## Fisher's transformation of effects
        se.fish <- 0.5*log((1+seall)/(1-seall)) ## Fisher's transformation of SE
        
        ###### Following Bartos et al.
        
        w <- 1/3 ## equal weight to the three effects
        random1 <- rma(yi=b.fish, vi=se.fish^2)
        res <- rma(yi=b.fish, vi=(se.fish^2)/w, tau=random1$tau2)
        
        ######
        
        pv4 <- res$pval ## p-value for meta-effect
        pred <- predict(res, transf=transf.ztor) ## transforms back from Fisher's
        eff4.temp <- pred$pred ## meta-effect
        
        i2 <- round(res$I2,4) ### a measure of heterogeneity 
        
        ## printing the meta-effect if it is significant
        
        if(pv4<0.05) print(paste("x=",namevec[x],"; y=",namevec[d],
                                 "; b=",round(eff4.temp,3),"; low=",round(pred$ci.lb,3),
                                 "; upp=",round(pred$ci.ub,3),"; p=",round(pv4,4), sep=""))
        
        #############
        
        eff1 <- ifelse(pv1<0.05,eff1.temp,0) ## rounding, effect 1
        eff2 <- ifelse(pv2<0.05,eff2.temp,0) ## rounding, effect 2
        eff3 <- ifelse(pv3<0.05,eff3.temp,0) ## rounding, effect 3
        eff4 <- ifelse(pv4<0.05,eff4.temp,0) ## rounding, effect 4
      }
      
      datarow1 <- c(datarow1,eff1) ## adding effect 1 to a vector
      datarow2 <- c(datarow2,eff2) ## adding effect 2 to a vector
      datarow3 <- c(datarow3,eff3) ## adding effect 3 to a vector
      datarow4 <- c(datarow4,eff4) ## adding effect 4 to a vector
      
      datarow5 <- c(datarow5,i2) ## heterogeneity
    }
    
    datamat1 <- rbind(datamat1,datarow1) ## adding vector to a matrix
    datamat2 <- rbind(datamat2,datarow2) ## adding vector to a matrix
    datamat3 <- rbind(datamat3,datarow3) ## adding vector to a matrix
    datamat4 <- rbind(datamat4,datarow4) ## adding vector to a matrix
    
    datamat5 <- rbind(datamat5,datarow5) ## heterogeneity
  }
  
  ## naming rows and columns in the matrices
  
  row.names(datamat1) <- row.names(datamat2) <-
    row.names(datamat3) <- row.names(datamat4) <-
    row.names(datamat5) <-  
    colnames(datamat1) <- colnames(datamat2) <-
    colnames(datamat3) <- colnames(datamat4) <- 
    colnames(datamat5) <- namevec
  
  datalist.tot <- list(datamat1,datamat2,datamat3,datamat4,datamat5)
  datalist <- list(datamat1,datamat2,datamat3,datamat4) ## list with all matrices
  
  mx <- max(unlist(lapply(datalist,function(m) max(abs(m))))) ## the strongest effect
  
  par(mar=c(0,0,0,0), oma=c(1,1,1,1), mfrow=c(2,2)) ## setting margins and layout
  
  for(mod in 1:4){ ## the four panels in the figure
    
    ## printing the network graph
    
    qgraph(datalist[[mod]], labels=namevec, asize=6, theme="colorblind", maximum=mx,
           palette="colorblind", legend=F, color=colvec, 
           node.width=1.5, edge.width=2, edge.labels=F, diag=F)
    
    legend(legvec[mod], title=LETTERS[mod], legend="", bty="n", cex=1.5) ## panel letter
    
    box(,col="gray") ## box around plot
    
  }
  
  return(datalist.tot) ## the function returns the five matrices
}

#######################
## Applying MultiNet  on data
## used by Guo et al.

colvec <- c(rep("orange",5),rep("lightgreen",4))

legvec <- c("topleft","bottomleft","topleft","topleft")

res <- MultiNet(data1=df1,data2=df2,colvec=colvec, legvec=legvec) ## applying MultiNet

res ## a look at the matrices


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################


