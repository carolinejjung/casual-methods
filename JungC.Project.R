island <- read.csv("/Users/carolinejung/Library/Mobile Documents/com~apple~CloudDocs/Desktop/STAT 309/Project/Islander_data.csv", header=TRUE)

# DATA EXPLORATION ---------------
dim(island)

# justifying why we use Diff (gain scores)
plot(island$Mem_Score_After~island$Mem_Score_Before,
     main="Memory Scores Before and After Drug Intake") #correlation
cor(island$Mem_Score_After,island$Mem_Score_Before)

# covariate: dosage
boxplot(island$Diff~island$Dosage, main="Difference in Memory Score vs Dosage", ylab="Difference in Memory Score", xlab="Dosage", names=c("low", "medium", "over recommended")) #3 dosages is more diff from 1 or 2 dosages
par(mfrow=c(1,3))
hist(island$Diff[island$Dosage==1], xlim=c(-50, 50), ylim=c(0,25), breaks=5, main="1 Dosage")
hist(island$Diff[island$Dosage==2], xlim=c(-50, 50), ylim=c(0,25), breaks=10, main="2 Dosages")
hist(island$Diff[island$Dosage==3], xlim=c(-50, 50), ylim=c(0,25), breaks=15, main="3 Dosages")
par(mfrow=c(1,1))

# covariate: type of memories
boxplot(island$Diff~island$Happy_Sad_group, main="Diff in Memory Score vs 
        Type of Memory Primed on", ylab="Diff in Memory Score", xlab="Type of Memory Primed on", names=c("Happy", "Sad"))
par(mfrow=c(1,2))
hist(island$Diff[island$Happy_Sad_group=="H"], xlim=c(-50, 50), ylim=c(0,50), breaks=5, main="Diff in Memory Scores", xlab="Happy Memories")
hist(island$Diff[island$Happy_Sad_group=="S"], xlim=c(-50, 50), ylim=c(0,50), breaks=5, main="Diff in Memory Scores", xlab="Sad Memories")

# covariate: age
plot(island$Diff~island$age, ylab="Diff in Memory Scores", xlab="Age", main="Diff in Memory Scores vs Age") #not much of a correlation
cor(island$Diff,island$age)

# age groups - new column
hist(island$age, main="Age Among Islanders", xlab="Age")
island$agegroup[20<island$age & island$age <= 40] <- "20-40"
island$agegroup[40<island$age & island$age <= 60] <- "40-60"
island$agegroup[60<island$age & island$age <= 83] <- "60-83"

boxplot(island$Diff~island$agegroup)
par(mfrow=c(1,3))
hist(island$Diff[island$agegroup=="20-40"], xlim=c(-50, 50), ylim=c(0,50), breaks=5, main="20-40")
hist(island$Diff[island$agegroup=="40-60"], xlim=c(-50, 50), ylim=c(0,50), breaks=5, main="40-60")
hist(island$Diff[island$agegroup=="60-83"], xlim=c(-50, 50), ylim=c(0,50), breaks=5, main="60-83")

# TREATMENT GROUPS ------------------
# make treatment groups
island$treat[island$Drug=="S"] <- 0 #placebo
island$treat[island$Drug!="S"] <- 1 #received drug

control <- island[island$treat==0,]
active <- island[island$treat==1,]

# graphic of response variable between treatment groups
par(mfrow=c(1,2))
hist(control$Diff, xlim=c(-50,50), ylim=c(0,50), main="Control")
hist(active$Diff, xlim=c(-50,50), ylim=c(0,50), main="Active")

# calculate distribution of data
round(apply(control[,c(3,7,8,9)], 2, mean),2)
round(apply(active[,c(3,7,8,9)], 2, mean),2)

# INITIAL COVARIATE BALANCE -------
# calculate proportion of memory
sum(active$Happy_Sad_group=="H")/length(active$Happy_Sad_group=="H")
sum(control$Happy_Sad_group=="H")/length(control$Happy_Sad_group=="H")

sum(active$Happy_Sad_group=="S")/length(active$Happy_Sad_group=="S")
sum(control$Happy_Sad_group=="S")/length(control$Happy_Sad_group=="S")

# proportion of dosage
sum(active$Dosage==1)/length(active$Dosage==1)
sum(control$Dosage==1)/length(control$Dosage==1)

sum(active$Dosage==2)/length(active$Dosage==2)
sum(control$Dosage==2)/length(control$Dosage==2)

sum(active$Dosage==3)/length(active$Dosage==3)
sum(control$Dosage==3)/length(control$Dosage==3)

# proportion of age group
sum(active$agegroup=="20-40")/length(active$agegroup=="20-40")
sum(control$agegroup=="20-40")/length(control$agegroup=="20-40")

sum(active$agegroup=="40-60")/length(active$agegroup=="40-60")
sum(control$agegroup=="40-60")/length(control$agegroup=="40-60")

sum(active$agegroup=="60-83")/length(active$agegroup=="60-83")
sum(control$agegroup=="60-83")/length(control$agegroup=="60-83")

# mean age
mean(active$age)
mean(control$age)


# MULTIPLE IMPUTATION-------
# Empirical Distribution with Donor Pools (Covariates)
# Covariate 1: Dosages -----
dose1.y0 <- island$Diff[island$Dosage==1 & island$Drug=="S"]
dose1.y1 <- island$Diff[island$Dosage==1 & island$Drug!="S"]

dose2.y0 <- island$Diff[island$Dosage==2 & island$Drug=="S"]
dose2.y1 <- island$Diff[island$Dosage==2 & island$Drug!="S"]

dose3.y0 <- island$Diff[island$Dosage==3 & island$Drug=="S"]
dose3.y1 <- island$Diff[island$Dosage==3 & island$Drug!="S"]

meandiff.dose <- c()
mediandiff.dose <- c()
island$y1.dose <- island$Diff
island$y0.dose <- island$Diff

for(i in 1:10000){
  #estimate Y0 for data where dosage is 1
  n1 <- length(island$y0.dose[island$Dosage==1 & island$Drug!="S"])
  island$y0.dose[island$Dosage==1 & island$Drug!="S"] <- sample(dose1.y0, n1, replace=TRUE)
  
  #estimate Y0 for data where dosage is 2
  n2 <- length(island$y0.dose[island$Dosage==2 & island$Drug!="S"])
  island$y0.dose[island$Dosage==2 & island$Drug!="S"] <- sample(dose2.y0, n2, replace=TRUE)
  
  #estimate Y0 for data where dosage is 3
  n3 <- length(island$y0.dose[island$Dosage==3 & island$Drug!="S"])
  island$y0.dose[island$Dosage==3 & island$Drug!="S"] <- sample(dose3.y0, n3, replace=TRUE)
  
  #estimate Y1 for data where dosage is 1
  n4 <- length(island$y1.dose[island$Dosage==1 & island$Drug=="S"])
  island$y1.dose[island$Dosage==1 & island$Drug=="S"] <- sample(dose1.y1, n4, replace=TRUE)
  
  #estimate Y1 for data where dosage is 2
  n5 <- length(island$y1.dose[island$Dosage==2 & island$Drug=="S"])
  island$y1.dose[island$Dosage==2 & island$Drug=="S"] <- sample(dose2.y1, n5, replace=TRUE)
  
  #estimate Y1 for data where dosage is 3
  n6 <- length(island$y1.dose[island$Dosage==3 & island$Drug=="S"])
  island$y1.dose[island$Dosage==3 & island$Drug=="S"] <- sample(dose3.y1, n6, replace=TRUE)

  meandiff.dose[i] <- mean(island$y1.dose) - mean(island$y0.dose)
  mediandiff.dose[i] <- median(island$y1.dose) - median(island$y0.dose)
}
mean(meandiff.dose); quantile(meandiff.dose,c(0.025,0.975))
mean(mediandiff.dose); quantile(mediandiff.dose,c(0.025,0.975))

# Covariate 2: Age Groups ---------
age1.y0 <- island$Diff[island$agegroup=="20-40" & island$Drug=="S"]
age1.y1 <- island$Diff[island$agegroup=="20-40" & island$Drug!="S"]

age2.y0 <- island$Diff[island$agegroup=="40-60" & island$Drug=="S"]
age2.y1 <- island$Diff[island$agegroup=="40-60" & island$Drug!="S"]

age3.y0 <- island$Diff[island$agegroup=="60-83" & island$Drug=="S"]
age3.y1 <- island$Diff[island$agegroup=="60-83" & island$Drug!="S"]

meandiff.age <- c()
mediandiff.age <- c()
island$y1.age <- island$Diff
island$y0.age <- island$Diff

for(i in 1:10000){
  #estimate Y0 for data where agegroup is 20-40
  n1 <- length(island$y0.age[island$agegroup=="20-40" & island$Drug!="S"])
  island$y0.age[island$agegroup=="20-40" & island$Drug!="S"] <- sample(age1.y0, n1, replace=TRUE)
  
  #estimate Y0 for data where agegroup is 40-60
  n2 <- length(island$y0.age[island$agegroup=="40-60" & island$Drug!="S"])
  island$y0.age[island$agegroup=="40-60" & island$Drug!="S"] <- sample(age2.y0, n2, replace=TRUE)
  
  #estimate Y0 for data where agegroup is 60-83
  n3 <- length(island$y0.age[island$agegroup=="60-83" & island$Drug!="S"])
  island$y0.age[island$agegroup=="60-83" & island$Drug!="S"] <- sample(age3.y0, n3, replace=TRUE)
  
  #estimate Y1 for data where agegroup is 20-40
  n4 <- length(island$y1.age[island$agegroup=="20-40" & island$Drug=="S"])
  island$y1.age[island$agegroup=="20-40" & island$Drug=="S"] <- sample(age1.y1, n4, replace=TRUE)
  
  #estimate Y1 for data where agegroup is 40-60
  n5 <- length(island$y1.age[island$agegroup=="40-60" & island$Drug=="S"])
  island$y1.age[island$agegroup=="40-60" & island$Drug=="S"] <- sample(age2.y1, n5, replace=TRUE)
  
  #estimate Y1 for data where agegroup is 60-83
  n6 <- length(island$y1.age[island$agegroup=="60-83" & island$Drug=="S"])
  island$y1.age[island$agegroup=="60-83" & island$Drug=="S"] <- sample(age3.y1, n6, replace=TRUE)
  
  meandiff.age[i] <- mean(island$y1.age) - mean(island$y0.age)
  mediandiff.age[i] <- median(island$y1.age) - median(island$y0.age)
}
mean(meandiff.age); quantile(meandiff.age,c(0.025,0.975))
mean(mediandiff.age); quantile(mediandiff.age,c(0.025,0.975))

# Covariate 3: Happy vs Sad Memory Priming ---------
h.y0 <- island$Diff[island$Happy_Sad_group=="H" & island$Drug=="S"]
h.y1 <- island$Diff[island$Happy_Sad_group=="H" & island$Drug!="S"]

s.y0 <- island$Diff[island$Happy_Sad_group=="S" & island$Drug=="S"]
s.y1 <- island$Diff[island$Happy_Sad_group=="S" & island$Drug!="S"]

meandiff.mem <- c()
mediandiff.mem <- c()
island$y1.mem <- island$Diff
island$y0.mem <- island$Diff

for(i in 1:10000){
  #estimate Y0 for data where happy
  n1 <- length(island$y0.mem[island$Happy_Sad_group=="H" & island$Drug!="S"])
  island$y0.mem[island$Happy_Sad_group=="H" & island$Drug!="S"] <- sample(h.y0, n1, replace=TRUE)
  
  #estimate Y0 for data where sad
  n2 <- length(island$y0.mem[island$Happy_Sad_group=="S" & island$Drug!="S"])
  island$y0.mem[island$Happy_Sad_group=="S" & island$Drug!="S"] <- sample(s.y0, n2, replace=TRUE)
  
  #estimate Y1 for data where happy
  n4 <- length(island$y1.mem[island$Happy_Sad_group=="H" & island$Drug=="S"])
  island$y1.mem[island$Happy_Sad_group=="H" & island$Drug=="S"] <- sample(h.y1, n4, replace=TRUE)
  
  #estimate Y1 for data where sad
  n5 <- length(island$y1.mem[island$Happy_Sad_group=="S" & island$Drug=="S"])
  island$y1.mem[island$Happy_Sad_group=="S" & island$Drug=="S"] <- sample(s.y1, n5, replace=TRUE)
  
  meandiff.mem[i] <- mean(island$y1.mem) - mean(island$y0.mem)
  mediandiff.mem[i] <- median(island$y1.mem) - median(island$y0.mem)
}
mean(meandiff.mem); quantile(meandiff.mem,c(0.025,0.975))
mean(mediandiff.mem); quantile(mediandiff.mem,c(0.025,0.975))






