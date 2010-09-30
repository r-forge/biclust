### Function to extract bicluster

bicluster <- function(x, BicRes, number=1:BicRes@Number)
{
    res <- list()
    for(i in 1:length(number))
    {
        res[[i]] <- x[BicRes@RowxNumber[,number[i]],BicRes@NumberxCol[number[i],]]
    }
    names(res) <- paste("Bicluster",number,sep="")
    return(res)
}

biclusternumber <- function(BicRes, number=1:BicRes@Number)
{
    res <- list()
    for(i in 1:length(number))
    {
        res[[i]] <- list()
        res[[i]][[1]] <- which(BicRes@RowxNumber[,number[i]])
        res[[i]][[2]] <- which(BicRes@NumberxCol[number[i],])
        names(res[[i]])<-c("Rows","Cols")
    }
    names(res) <- paste("Bicluster",number,sep="")
    return(res)
}
