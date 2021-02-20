

library(rstan)

kag_data <- read.csv("C:\\Users\\ACS\\Downloads\\train.csv")
y <- kag_data$target
X <- as.matrix(kag_data[,3:302])

#X <- as.matrix(XX)
n=dim(X)[1]
p=dim(X)[2]

mymodel <- "
data{
int n;
int p;
int y[n];
matrix[n, p] X;
}

parameters{
real alpha;
vector<lower=0>[p] beta;
}

transformed parameters {
vector[n] mu;
mu = alpha + X * beta;
}

model{
alpha ~ normal(0,3);
beta ~ normal(0,3);
//y ~ bernoulli_logit(alpha + X*beta);
target+= bernoulli_logit_lpmf(y | mu);
}
"


results <- stan(model_code=mymodel, data=list(n, p, y, X), iter=1000, chains = 1, thin = 2)
#print(results, pars = c("alpha", "beta"))
plot(results,par=list("beta"))

mu_summary <- as.matrix(summary(results, pars = c("mu"), probs = c(0.5))$summary)
#print(mu_summary)

pr = 1/(1+exp(-mu_summary[,4]))
plot(pr,col=y+1,pch=1)


pred <- predict(rf, newdata = X_test)

results <- data.frame(id=250:19999, target=pred)
colnames(results)=c("id","target")
write.csv(results, file = "submission.csv", row.names = F)
#############################################################