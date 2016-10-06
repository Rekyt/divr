#' Violon plots for Groups
#'
#' Produce a violon plot acording to groups taking into acount weightings and compute comparison tests
#'
#' @usage vioplot(x, gp, weights, ...)
#'
#' @param x a numeric vector of data
#' @param gp factors with the same length as x decribing groups
#' @param weights a numeric vector with the same length as x
#' @param Test a character value for group comparison tests "tukey" or "wilcox"
#' @param wex the maximum value for the density curve
#' @param h the kernel density smoothing parameter
#' @param add if TRUE adds the plot to the current plot window
#' @param nblim minimum length of x for computing the kernel density
#' @param boxplot logical value for adding a boxplot or not
#' @param side a character value describind if the distribution should be drawn in the "above", "below", "right", "left" or "both" side(s)
#' @param fill the color of the distribution area
#' @param boxplot.fill the color of the boxplot
#' @param pts logical value for adding x data on the curve distrivution
#' @param horizontal logical value
#'
#' @examples
#'
#' data("ChickWeight")
#' vioplotGP(x = chickwts$weight, gp= chickwts$feed, las=2, horizontal = F, side="both",
#' wex=.4, fill="grey70", boxplot.fill="grey30", xlab="Chick weights (mg)", Test="tukey",
#' pos.letters = 2, col.letters = "white")
#'
#' @seealso
#'
#' For group comparison see vioplotGp
#'
#' @export

vioplotGP<-function(x,gp,weights=NULL,labels=NA,xlab=NA,xlim=NA,ylim=NA,at=NA,wex=.75,
                    h=NA,nblim=10,Test=NA,Testadj=0.01,pos.letters=1,col.letters=1, add=F,col=1,pch=19,lwd=1,
                    lty=1,cex=1,pcex=1, bg=1, las=0, boxplot=TRUE, side="above", fill=NA,
                    pts=TRUE, boxplot.fill="white", horizontal=TRUE){

  # check
  if (!is.na(Test)&!(Test%in%c("wilcox","tukey"))) stop("Test argument should be 'pairwise' or 'tukey'")

  #data descriptors
  ngp<-nlevels(gp)
  xgp<-mapply(function(X){x[gp==X]},levels(gp))
  if(class(xgp)!="list")xgp<-list(xgp)
  quart<-lapply(xgp,quantile)

  #weights
  w<-weights
  n<-length(x)
  if (is.null(w)) w<-rep(1/n,n) else if (length(w)!=n) stop("Weight length not correct")
  w<-w/sum(w)
  wgp<-mapply(function(X){w[gp==X]/sum(w[gp==X])},levels(gp))
  if(class(wgp)!="list") xgp<-list(wgp)

  # side
  if (!side%in%c("above","below","right","left","both")) stop('side argument should be "above","below", "right", "left" or "both"')

  #at
  if (all(is.na(at))) at<-1:ngp else if (length(at)!=ngp) stop("at length not correct")

  #labels
  if (all(is.na(labels))) labels<-levels(as.factor(gp))

  #test if test
  testdone=FALSE
  if (!is.na(Test)&Test=="wilcox"){
    test<-pairwise.wilcox.test(x,gp, p.adj="bon")$p.value
    p.values<-cbind(rbind(rep(NA,ngp-1),test),rep(NA,ngp));colnames(p.values)<-levels(gp);rownames(p.values)<-levels(gp)
    for (i in 1:(ngp-1)){
      p.values[i,(i+1):ngp]<-p.values[(i+1):ngp,i]
    }
    diag(p.values)<-1
    stat=""
    testdone<-TRUE
  } else if (!is.na(Test)&Test=="tukey"){
    library(multcomp)
    test<-summary(glht(lm(x~gp, weights = w),mcp(gp="Tukey")))$test
    comp<-as.numeric(test$pvalues)
    compnames<-do.call(cbind,strsplit(names(test$coefficients)," - "))
    p.values<-matrix(1,ngp,ngp)
    colnames(p.values)=rownames(p.values)=levels(gp)
    for (i in 1:length(comp)){
      p.values[compnames[1,i],compnames[2,i]]<-p.values[compnames[2,i],compnames[1,i]]<-comp[i]
    }
    Fval<-round(summary(aov(x~gp, weights=w))[[1]][1,"F value"],2)
    pval<-summary(aov(x~gp, weights=w))[[1]][1,"Pr(>F)"]
    pval<-ifelse(pval<0.001,"***",ifelse(pval<0.01,"**",ifelse(pval<0.05,"*",ifelse(pval<0.1,".","ns"))))
    Fname<-ifelse(add,"F '","F")
    stat<-paste0(Fname, " = ",Fval,pval)
    testdone=TRUE
  }
  if(testdone) {
    p.values[p.values>0.05]<-1
    p.values[p.values!=1]<-0
    ordre<-order(unlist(lapply(xgp,mean,na.rm=T)))
    p.values<-p.values[ordre,ordre]
    p.values<-unique(p.values)

    if(nrow(p.values)==0){
      lettres<-rep("a",ngp)
      names(lettres)<-levels(gp)
    }else{
      lettres<-rep("",ngp)
      k=1
      while(any(lettres=="")&k<ngp){
        lettres[which(p.values[k,k:ngp]==1)+k-1]<-paste0(lettres[which(p.values[k,k:ngp]==1)+k-1],letters[k])
        k=k+1
      }
      names(lettres)<-colnames(p.values)
    }
    if(add){
      greekletters<-c("alpha","beta","gamma","epsilon","zeta")
      lettres<-lapply(strsplit(lettres,""),function(X){mapply(function(x){which(letters==x)},X)})
      lettres<-lapply(lettres, function(X){greekletters[X]})
      lettres<-unlist(lapply(lettres, function(X){do.call(paste, c(as.list(X), sep="*"))}))
    }
  }

  #plot window and par
  par0<-par()["cex"]
  par(cex=cex)

  if(!add){
    if(horizontal){
      plot.new()
      if (any(is.na(xlim))) xlimi<-c(min(x),max(x)) else xlimi=xlim
      if (any(is.na(ylim))) ylimi<-c(min(at)-1,max(at)+1) else ylimi=ylim
      plot.window(xlim=xlimi, ylim=ylimi)
      box(which ="plot",col=1, lwd=1)
      axis(1, las=las)
      if (any(labels!="none")){
        axis(2, at=at,labels=labels, las=las)
      }
      if(all(is.na(xlab))){
        title(xlab=deparse(substitute(x)))
      }else if (any(xlab!="none")){
        title(xlab=xlab)
      }
    }else{
      plot.new()
      if (any(is.na(xlim))) ylimi<-c(min(x),max(x)) else ylimi=xlim
      if (any(is.na(ylim))) xlimi<-c(min(at)-1,max(at)+1) else xlimi=ylim
      plot.window(xlim=xlimi, ylim=ylimi)
      box(which ="plot",col=1, lwd=1)
      axis(2, las=las)
      if (any(labels!="none")){
        axis(1, at=at,labels=labels, las=las)
      }
      if(all(is.na(xlab))){
        title(ylab=deparse(substitute(x)))
      }else if (any(xlab!="none")){
        title(ylab=xlab)
      }
    }
  }

  #par for each vioplot
  wex<-rep(wex,length.out=ngp)
  h<-rep(h,length.out=ngp)
  col<-rep(col,length.out=ngp)
  pch<-rep(pch,length.out=ngp)
  lwd<-rep(lwd,length.out=ngp)
  lty<-rep(lty,length.out=ngp)
  pcex<-rep(pcex,length.out=ngp)
  bg<-rep(bg,length.out=ngp)
  side<-rep(side,length.out=ngp)
  fill<-rep(fill,length.out=ngp)
  pts<-rep(pts,length.out=ngp)
  boxplot.fill<-rep(boxplot.fill,length.out=ngp)

  # vioplots
  hi<-NULL
  for (i in 1:ngp){
    hi<-c(hi,vioplot(xgp[[i]],weights=wgp[[i]], wex=wex[i], h=h[i], add=T,
                     nblim=nblim, at=at[i], pch=pch[i], lwd=lwd[i],col=col[i],
                     lty=lty[i], cex=pcex[i], bg=bg[i], boxplot = boxplot,
                     side=side[i], fill=fill[i], pts=pts[i],boxplot.fill = boxplot.fill[i],
                     horizontal = horizontal))
    if (testdone){
      line<-ifelse(add,-2.2,-1.2)
      mtext(side=3,adj=Testadj, text=stat, line=line, cex=cex, las=1)
      lettre<-lettres[names(lettres)==levels(gp)[i]]
      if(add){
        if (horizontal) text(quart[[i]][3],at[i],parse(text=lettre), pos=pos.letters,cex=cex, col=col.letters) else text(at[i],quart[[i]][3],parse(text=lettre), pos=pos.letters,cex=cex, col=col.letters)
      }else{
        if (horizontal) text(quart[[i]][3],at[i],lettre, pos=pos.letters,cex=cex, col=col.letters) else text(at[i],quart[[i]][3],lettre, pos=pos.letters,cex=cex, col=col.letters)
      }
    }
  }
  names(hi)<-levels(gp)
  par(par0)
  return(hi)
}

