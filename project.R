df <- Student_Satisfaction_E_learning_Responses_1_


#install.packages("gginference")
library(gginference)

#Manual Calculation
tps <- df$Total_Perceived_Stress


original_sample.n <- length(tps)
original_sample.mean <- mean(tps)
original_sample.sd <- sd(tps)
original_sample.se <- original_sample.sd/sqrt(original_sample.n)
original_sample.t_val <- (original_sample.mean - 3)/original_sample.se
hist(tps)

#For Original Sample
original_sample.t <- t.test(x=tps, alternative="greater", mu=3)
ggttest(original_sample.t)

print("Original Sample Stats")
original_sample.n
original_sample.mean
original_sample.sd
original_sample.se
original_sample.t_val
original_sample.t

#Bootstraping
B<-1000

Boots.BootstrapSamples <- matrix( sample(tps, size= original_sample.n*B, replace=TRUE), 
                            nrow=original_sample.n, ncol=B)
Boots.mean<-c()

for (i in 1: B){
  Boots.means[i] <- mean(Boots.BootstrapSamples[1:original_sample.n,i])
}

Boots.t <- t.test(x=Boots.means, alternative="greater", mu=3)
ggttest(Boots.t)

print("Bootstrap Samples Stats")
Boots.t
