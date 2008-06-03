parallelCoordinates=function(x, bicResult, number, plotConditions=TRUE, absoluteLimits=TRUE,geneTitle=FALSE,...)
  {
  n=dim(x)[1]
  m=dim(x)[2]
    
  bicRows=row(matrix(bicResult@RowxNumber[,number]))[bicResult@RowxNumber[,number]==T]
  bicCols=row(matrix(bicResult@NumberxCol[number,]))[bicResult@NumberxCol[number,]==T]

  if(geneTitle)
  {
  if(absoluteLimits)
    {
    if(plotConditions)  matplot(x[bicRows,bicCols],type='l',lty=1, ylab="Expression level", xlab="Gene", ylim=c(min(x),max(x)))
    else                matplot(t(x[bicRows,bicCols]),type='l',lty=1, ylab="Expression level", xlab="Condition", ylim=c(min(x),max(x)))
    }
  else
    {
    if(plotConditions)  matplot(x[bicRows,bicCols],type='l',lty=1, ylab="Expression level", xlab="Gene")
    else                matplot(t(x[bicRows,bicCols]),type='l',lty=1, ylab="Expression level", xlab="Condition")
    }

  if(plotConditions)
      title(main=paste("Expression levels of conditions \nin Bicluster",number," across their genes\n",
        "(genes=", length(bicRows), "conditions=",length(bicCols),")",sep=" "))
  else
      title(main=paste("Expression levels of genes \nin Bicluster",number," across their conditions\n",
        "(genes=", length(bicRows), "conditions=",length(bicCols),")",sep=" "))
  
  }
 
else
{

if(absoluteLimits)
    {
    if(plotConditions)  matplot(x[bicRows,bicCols],type='l',lty=1, ylim=c(min(x),max(x)),...)
    else                matplot(t(x[bicRows,bicCols]),type='l',lty=1, ylim=c(min(x),max(x)),...)
    }
  else
    {
    if(plotConditions)  matplot(x[bicRows,bicCols],type='l',lty=1,...)
    else                matplot(t(x[bicRows,bicCols]),type='l',lty=1,...)
    }
}
}