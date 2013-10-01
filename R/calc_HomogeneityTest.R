calc_HomogeneityTest<- structure(function( # Apply a simple homogeneity test after Galbraith (2003)
  ### A simple homogeneity test for De estimates
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  
  ##section<<
  ## version 0.1 [2013-09-04] 
  # ===========================================================================
  
  input.data,
  ### \code{\link{data.frame}} (\bold{required}): two column data frame with De
  ### values and corresponding De errors
  log = TRUE,
  ### \code{\link{logical}} (with default): peform the homogeniety test with
  ### (un-)logged data
  sample.id="unknown sample" 
  ### \code{\link{character}} (with default): sample id
  ){                     
  
  
##============================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##============================================================================##
  
  if(is.data.frame(input.data)==FALSE) { print("Input data needs to be of type 
                                               data.frame",quote=F) 
                                         stop(domain=NA) }
  
##============================================================================##
## CALCULATIONS
##============================================================================##

  if(log==TRUE){
    input.data<- log(input.data)
  }
  
wi<- 1/input.data[2]^2
wizi<- wi*input.data[1]
mu<- sum(wizi)/sum(wi)
gi<- wi*(input.data[1]-mu)^2

G<- sum(gi)
df<- length(wi)-1
n<- length(wi)
P<- pchisq(G, df, lower.tail = FALSE)

##============================================================================##
## OUTPUT
##============================================================================##
  
  
  cat("\n [calc_HomogeneityTest]")
  cat(paste("\n\n ---------------------------------"))
  cat(paste("\n sample ID:         ", sample.id))
  cat(paste("\n n:                 ", n))
  cat(paste("\n ---------------------------------"))
  cat(paste("\n mu:                ", round(mu,4)))
  cat(paste("\n G-value:           ", round(G,4)))
  cat(paste("\n Degrees of freedom:", df))
  cat(paste("\n P-value:           ", round(P,4)))
  cat(paste("\n ---------------------------------\n\n"))  

      #return value
      results<- data.frame(id=sample.id,n=n,g.value=G,df=df,P.value=P)
      
      invisible(list(results=results))
  ### Returns a terminal output. In addition a list is returned containing the
  ### following element: \cr\cr
  ### \code{results} data frame with statistical parameters.
  
  ##details<<
  ## For details see Galbraith (2003).
  
  ##references<<
  ## Galbraith, R.F., 2003. A simple homogeneity test for estimates of dose
  ## obtained using OSL. Ancient TL, 21, pp. 75-77.
  
  ##seealso<<
  ## \code{\link{pchisq}}
      
}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## apply the common dose model
  calc_HomogeneityTest(ExampleData.DeValues)
})