# Leonardo Espinosa 
# Assignment 3
# Excercise 2

# In this part the allsolutions.cvs list is sorted.
MySortSolList <- function(){
  tmp1 <- read.csv("allsolutions.csv",header = FALSE,sep = ',')
  file.remove("allsolutions.csv")
  tmp2 <- as.matrix.data.frame(tmp1)
  K <- colnames(tmp2)[ncol(tmp2)]
  tmp3 <- tmp2[order(tmp2[, K],decreasing=TRUE),]
  write.table(tmp3,file="allsolutions.csv",sep=",", col.names = F, row.names = F)
}

# The best solution is chosen from the tabuvectors.csv list, used to start the search again,
MyTabuSols <- function(MyVSolution){
  L=length(MyVSolution)
  tmp1 <- read.csv("tabuvectors.csv",header = FALSE,sep = ',')
  tmp2 <- as.matrix.data.frame(tmp1)
  K <- colnames(tmp2)[ncol(tmp2)]
  tmp3 <- tmp2[order(tmp2[, K]),]
  tmp4 <- tail(tmp3, n = 1)
  write.table(tmp4,file="best.csv",sep=",", col.names = F, row.names = F)
  tmp5 <- read.csv("best.csv",header = FALSE,sep=',')
  tmp6 <- as.numeric(tmp5)
  BestSol <- tmp6[1:L]
  return(BestSol)
}

# The tabu list of random numbers, the last m numbers.
InMyTabuList <- function(Rval,MTabuList){
  c <- read.csv("tabulist.csv",header = FALSE)
  d<- as.vector(c)
  f <- tail(d,MTabuList)
  any(f==Rval)
}


# Return the evaluation value, calculating the penaly value.
GetEvaluationValue <- function(Value,Weight,Capacity,Penalty,MyVSolution){
  v <- sum(MyVSolution * Value)
  w <- sum(MyVSolution * Weight)
  ev <- v - (Penalty * max(0,w-Capacity))
  return(ev)
}


# Here the tabu search algorithm is implemented
MyTabuSearch <- function(Value,Weight,Capacity,MTabuList,Penalty,MaxIte,MMAX=1){

for (j in 1:MMAX){  
  ftmp0 <- "best.csv"
  if (file.exists(ftmp0)) file.remove(ftmp0)
  
  ftmp1 <- "tabulist.csv"
  if (file.exists(ftmp1)) file.remove(ftmp1)
  
  ftmp2 <- "tabuvectors.csv"
  if (file.exists(ftmp2)) file.remove(ftmp2)
  
  L1 <- length(Value)
  TrivialS <- vector(mode = "numeric", length = L1)
  i = 1
  while(i < MaxIte){
        if (i == 1){ 
          i <- i +1
          Ran <- sample(1:L1,1)
          TrivialS[Ran]=1
          write(Ran,file="tabulist.csv",append=TRUE)
          MyVSolution <- TrivialS
          P <- GetEvaluationValue(Value,Weight,Capacity,Penalty,MyVSolution)
          MyVSolutionWP <- append(MyVSolution, P)
          write(MyVSolutionWP,file="tabuvectors.csv", ncolumns = L1+1, append=TRUE, sep = ',')
        }
       if (i > 1){
          Ran <- sample(1:L1,1)
          if (InMyTabuList(Ran,MTabuList)==FALSE){
            i <- i +1
            write(Ran,file="tabulist.csv",append=TRUE)
            if (MyVSolution[Ran]==1) {
               MyVSolution[Ran]=0
               P <- GetEvaluationValue(Value,Weight,Capacity,Penalty,MyVSolution)
               MyVSolutionWP <- append(MyVSolution, P)
               write(MyVSolutionWP,file="tabuvectors.csv", ncolumns = L1+1, append=TRUE, sep = ',')}
            else {
              MyVSolution[Ran]=1
              P <- GetEvaluationValue(Value,Weight,Capacity,Penalty,MyVSolution)
              MyVSolutionWP <- append(MyVSolution, P)
              write(MyVSolutionWP,file="tabuvectors.csv", ncolumns = L1+1, append=TRUE, sep = ',')}
            }
          else { i <- i-1}
        }
   MyVSolution <- MyTabuSols(MyVSolution)
  }
  MyVSolution2 <- MyTabuSols(MyVSolution)
  MyVSolution2data <- as.data.frame(MyVSolution2)
  MyBestCapacity <- sum(MyVSolution2 * Weight)
  MyBestValue <- sum(MyVSolution2 * Value)
  MyBestPair  <- c(MyBestCapacity,MyBestValue)    
  MyFullVSolution <- append(MyVSolution,MyBestPair)
  write(MyFullVSolution,file="allsolutions.csv", ncolumns = L1+2, append=TRUE, sep = ',')
  
  if(MMAX==1){
   print("The best solution for this run has:")
   print(paste("Capacity: ",MyBestCapacity))
   print(paste("Value: ",MyBestValue))
   print(paste("Vector: ",MyVSolution2data))}
  
  } 
  if(MMAX>1){
    MySortSolList()
    MyUniversalBest <- read.csv("allsolutions.csv",header = FALSE,sep = ',')
    MyUniversalBestV <- as.numeric(head(MyUniversalBest,n=1))
    
    MyVUSolution2 <- as.data.frame(MyUniversalBestV[1:L1])
    MyBestUCapacity <- MyUniversalBestV[L1+1]
    MyBestUValue <- MyUniversalBestV[L1+2]
    
    print(paste("The best solution after: ",MMAX," searchs"))
    print(paste("Capacity: ",MyBestUCapacity))
    print(paste("Value: ",MyBestUValue))
    print(paste("Vector: ",MyVUSolution2))}
}



# Test 
value <- c(135, 139, 149, 150, 156, 163, 173, 184, 192, 201, 210, 214, 221, 229, 240)
weight <- c(70, 73, 77, 80, 82, 87, 90, 94, 98, 106, 110, 113, 115, 118, 120)
capacity <- 750
mtabulist <- 5
maxit <- 200
penalty <- 100

MyTabuSearch(value,weight,capacity,mtabulist,penalty,maxit,5)
  




