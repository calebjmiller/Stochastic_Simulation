### Stochastic Simulation 
# Tests for Random Number Generators
# Runs Up/Down Test for Independence
# Runs Above/Below the Mean Test for Independence
# Length or Runs (Up/Down) Test for Independence
# Autocorrelation Test for Independence
# Chi-squared test for Uniformity
# Serial Test
# Kolmogorov-Smirnov Test
### Caleb Miller

###################
### Get Samples ###
###################

# Set seed for reproducibility of results
set.seed(1)

# Get a large sample of uniform random numbers
n <-100000
samples <-runif(n)

#########################
### Runs Up/Down Test ###
#########################

# Get R_n
Rn<-0
run<-matrix(0,(n-1),1)

for (i in 1:(n-1)){
  if(samples[i] < samples[i+1]){
    run[i]=1
  }
}
for (i in 1:(n-2)){
  if(run[i] != run[i+1]){
    Rn<-Rn+1
  }
}

# Get E[R_n]
ERn <- (2*n-1)/3

# Get Var[Rn]
VarRn <- (16*n-29)/90

# Get Z_n
Zn <- (Rn-ERn)/sqrt(VarRn)

# Result of Test
if(-1.96<Zn & Zn< 1.96){
  print("This sample has passed the runs up and down test for independence")
} else{
  print("This sample has not passed the runs up and down test for independence")
}

######################################
### Runs Above/Below the Mean Test ###
######################################

# Get R_n
Rn<-0
mrun<-matrix(0,n,1)

for(i in 1:(n)){
  if(samples[i]>.5)
  mrun[i]=1
}
for (i in 1:(n-1)){
  if(mrun[i] != mrun[i+1]){
    Rn<-Rn+1
  }
}

# Get E[R_n]
ERn<-(n+1)/2

# Get Var[R_n]
VarRn<-(n-1)/4

# Get Z_n
Zn<- (Rn-ERn)/sqrt(VarRn)

# Result of Test
if(-1.96<Zn & Zn< 1.96){
  print("This sample has passed the runs above and below mean test for independence")
} else{
  print("This sample has not passed the runs above and below mean test for independence")
}

###########################
### Length of Runs Test ###
###########################

# TEST
# n<-1000;
#samples <-runif(n)
#set.seed(1)
# TEST


lengthu <-matrix(0,n-1,1)
lengthd <-matrix(0,n-1,1)

j<-1

for (i in 1:(n-1)){
  if(samples[i] < samples[i+1]){
    lengthu[j]=lengthu[j]+1
  } else{
    j=j+1
  }
}

j<-1

for (i in 1:(n-1)){
  if(samples[i] > samples[i+1]){
    lengthd[j]=lengthd[j]+1
  } else{
    j=j+1
  }
}



# Expected and Observed
E <- c(41666.75, 18333.10, 5277.65,1150.75,203.36,34.72)

O = matrix(0,1,6)

for (i in 1:6){
  O[i]<- sum(lengthu ==i) + sum(lengthd ==i)
}

O[6] <- O[6] + sum(lengthu == 7) + sum(lengthu == 8) + sum(lengthu == 9)+sum(lengthd == 7) + sum(lengthd == 8) + sum(lengthd == 9)

W = sum(((O-E)^2)/E)

# Result of Test
if(W< 11.075){
  print("This sample has passed the length of runs test for independence")
} else{
  print("This sample has not passed the length of runs test for independence")
}
