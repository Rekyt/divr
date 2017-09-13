#' climate plots
#'
#' Produce a climate plot from monthly mean temperature and total precipitations
#'
#' @param temp a numeric vector monthly mean temperatures (12 values)
#' @param prec a numeric vector monthly total precipitations (12 values)
#' @param min an numeric vector monthly minimal temperatures (12 values); optional, default value = NA
#' @param max a numeric vector monthly maximal temperatures (12 values); optional, default value = NA
#' @param site  the site name character
#' @param alt the site altitude; optional, default value = NA
#' @param Tmin minimal value for the temperature y-axis; default value = NA
#' @param H a two-column matrix describing the low water availability periods (start and end between 0 and 12); optional, default value = NA
#' @param cex text size; default value = 1
#'
#' @examples
#'
#' temp<-c(6.5,7.2,9.9,12.5,16.5,20.6,23.8,23.4,19.4,15.4,10.3,7.3)
#' min<-c(1.7,2,4.5,7.3,11.4,14.9,17.9,17.5,13.8,10.7,5.4,2.5)
#' max<-c(11.4,12.2,15.3,17.8,21.7,26.2,29.6,29.3,25.1,20.1,14.9,11.9)
#' prec<-c(67.9,64.1,40.6,66.3,54.1,35.4,20.4,40.3,105.1,113.9,85.7,87.7)
#' par(mar=c(3,3,1,3))
#' plotOmbro(temp,prec,min,max,site="Montpellier",alt=57)
#'
#'
#' @import graphics stats
#'
#' @export

plotOmbro<-function(temp,prec,min=NA,max=NA,site=NA,alt=NA, Tmin=NA, H=NA, cex=1){

  if (length(na.omit(temp))!=12|length(na.omit(prec))!=12){
    return("Temp or Prec are not a 12 values vector")
  }else{
    if (is.na(site)|!is.character(site)){
      return("The site name is not conform")
    }else{
      moy<-c(mean(temp), sum(prec))
      prec[prec>100]<-100+(prec[prec>100]-100)*.1
      mini<-ifelse(is.na(Tmin),ifelse(min(temp*2)<0,min(temp*2),0),Tmin*2)
      plot(temp*2, t="l",yaxt="n", xaxt="n",ylab="",xlab="", ylim=c(mini,110))

      if(!all(is.na(H))&ncol(as.matrix(H))==2){
        H<-as.matrix(H)
        for (i in 1:nrow(H)){
          rect(H[i,1],mini,H[i,2],par()$usr[4]-.1, col="grey80", border=NA)
        }
        lines(temp*2, t="l",yaxt="n", xaxt="n",ylab="",xlab="", ylim=c(mini,110))
        box()
      }

      # Axes
      axis(1, labels=F, at=-.5:12.5)
      deb<-ifelse(is.na(Tmin),ifelse(min(temp)<0,min(temp),0),Tmin)
      axis(2, labels=seq(floor(deb/5)*5,50,5), at=seq(floor(deb/5)*10,100,10), las=2)
      axis(4, labels=c(seq(0,100,10),200), at=seq(0,110,10), las=2)

      #titres
      title<-ifelse(is.na(alt), site, paste0(site," (",alt," m)"))
      mtext(side=3,title,adj=.05, line=-1, cex=cex)
      mtext(side=3,parse(text=paste0("'",round(moy[1],1),"'*degree*'C; ",round(moy[2],1)," mm'")),adj=.05, line=-2, cex=cex)

      mtext(side=2, at = 110, text = expression(degree*'C'), las=2, cex=cex, line=1)
      mtext(side=4, at = 118, text = "mm", las=2, cex=cex, line=1)
      mtext(side=1, c("J","F","M","A","M","J","J","A","S","O","N","D"), at=1:12, cex=cex)

      #Remplissage pluie
      temp2<-NULL
      prec2<-NULL
      for (i in 1:11){
        if(temp[i]==temp[i+1]){
          temp2<-c(temp2,rep(temp[i],5))
        }else{
          temp2<-c(temp2,seq(temp[i],temp[i+1]-(temp[i+1]-temp[i])/5,(temp[i+1]-temp[i])/5))
        }
        if(prec[i]==prec[i+1]){
          prec2<-c(prec2,rep(prec[i],5))
        }else{
          prec2<-c(prec2,seq(prec[i],prec[i+1]-(prec[i+1]-prec[i])/5,(prec[i+1]-prec[i])/5))
        }
      }
      temp2<-c(temp2,temp[12])
      prec2<-c(prec2,prec[12])
      lignes<-data.frame(pt=seq(1,12,.2),temp2,prec2)
      lignes<-lignes[(lignes$temp2*2)<lignes$prec2,]
      segments(lignes$pt,lignes$prec2,lignes$pt,lignes$temp2*2, col="grey")

      #Remplissage deficit
      point<-data.frame(pt=seq(1,12,.2),temp2,prec2)
      point<-point[point$prec2<(point$temp2*2),]
      ptsx<-rep(seq(0,12,.2),each=56)
      ptsy<-rep(seq(0,110,2),61)
      pts<-data.frame(x=ptsx,y=ptsy)

      draw<-NULL
      for (i in 1:nrow(point)){
        drawy<-pts$y[which(round(pts$x,2)==round(point$pt[i],2)&pts$y>(point$prec2[i]+1)&pts$y<((point$temp2[i]*2)-1))]
        drawx<-rep(point$pt[i],length(drawy))
        draw<-rbind(draw,cbind(drawx,drawy))
      }

      points(draw[,1],draw[,2],pch=".")

      #Remplissage >100
      sup100<-which(prec>100)
      inf<-(sup100-1)[!(sup100-1)%in%sup100&(sup100-1)>0]
      sup<-(sup100+1)[!(sup100+1)%in%sup100&(sup100+1)<13]
      b<-c(inf,sup-1)[order(c(inf,sup-1))]

      x100<-(100-prec[b])/(prec[b+1]-prec[b])+(b)

      xx<-c(1:12,x100)[order(c(1:12,x100))]
      xx<-c(xx,rev(xx))

      yy<-c(prec,rep(100,length(x100)))[order(c(1:12,x100))]
      yys<-rev(yy)
      yys[yys>100]<-100
      yy<-c(yy,yys)
      polygon(xx,yy,col=1)
      lines(prec)
      #barre temp extremes
      if (all(is.na(min))|all(is.na(max))){
      }else{
        if(any(min<0)){
          for (i in which(min<0)){
            rect(i-.5,par()$usr[3],i+.5,mini, col="grey",border="grey")
          }
        }
        if(any(max<0)){
          for (i in which(max<0)){
            rect(i-.5,par()$usr[3],i+.5,mini, col=1)
          }
        }
        rect(par()$usr[1],par()$usr[3],par()$usr[2],mini)
      }
    }
  }
}
