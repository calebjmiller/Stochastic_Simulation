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
Zn = (Rn-ERn)/sqrt(VarRn)

# Result of Test
if(-1.96<Zn & Zn< 1.96){
  print("This sample has passed the runs up and down test for independence")
} else{
  print("This sample has passed the runs up and down test for independence")
}




