\name{biclustmember}
\alias{clustmember}
\alias{biclustmember}

\title{Draw Heatmap}
\description{ Draws a membership graph cluster x columns, to show which }
\usage{
biclustmember(bicResult, x, mid = T, cl_label = "CL.", Titel = "BiCluster Membership Graph",...)

clustmember(res, x, mid = T, cl_label = "CL.", Titel = "Cluster Membership Graph",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix}
  \item{bicResult}{BiclustResult object with a bicluster result set. If this value
    is set to NULL, the data matrix is drawn as a heatmap, without any reordering. Default NULL.}
  \item{res}{Cluster Result (is converted into a kcca object)}
  \item{mid}{If TRUE, shows the value of the remaining objects inside the cluster value, else shows both aside each other.}
  \item{cl_label}{Ticks of x-axis}
  \item{Titel}{Gives the title of the plot}
  \item{...}{Additional plot options or if neccessary option for as.kcca}
  }
%\details{}
%\value{}
%\references{}

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

\seealso{
\code{\link{bubbleplot}} for simultaneous representation of biclusters.
\code{\link{parallelCoordinates}}for single representation of biclusters as lines of gene or condition profiles.
\code{\link{drawHeatmap}}for Heatmap representation of biclusters.
  }
\examples{
  s2=matrix(rnorm(400),20,20)
  s2[12:16,12:16]=rnorm(25,3,0.3)
  set.seed(1)
  bics <- biclust(s2,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  iter.startup = 5, iter.layer = 30,  verbose = TRUE)
  biclustmember(bics,s2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}
