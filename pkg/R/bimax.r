#
#  Copyright (C) 2007 Sebastian Kaiser
#
#  Prelic, A.; Bleuler, S.; Zimmermann, P.; Wille, A. & , P. A systematic comparison and evaluation of biclustering methods for gene expression data Bioinformatics, Oxford Univ Press, 2006
#
# Calling the C Code from the bimax.c file.

cbimax<- function(logicalmatrix,minr=2,minc=2,number=100,er=0)
   .C("bimax",
   as.integer(logicalmatrix),
   as.integer(nrow(logicalmatrix)),
   as.integer(ncol(logicalmatrix)),
   as.integer(minr),
   as.integer(minc),
   as.integer(matrix(0,nrow=nrow(logicalmatrix),ncol=number)),
   as.integer(matrix(0,nrow=number,ncol=ncol(logicalmatrix))),
   as.integer(vector(mode="integer",length=nrow(logicalmatrix)+ncol(logicalmatrix))),
   as.integer(number),
   as.integer(er))

  

bimax<- function(logicalmatrix,...){
ausgabe<-cbimax(logicalmatrix,...)
if (ausgabe[[10]]==1)
{warning("Too many biclusters found change number or minimal dimension of expected bicluster!")
ausgabe}
else
ausgabe
}