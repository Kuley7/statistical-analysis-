#statistical analysis using R 
lifedata<-read.table("D:/statisticalanalysis/TimeInLifeData.txt",header = TRUE)
TimeInLifeData
View(TimeInLifeData)

lifedata <- read.table(file.choose(), sep = "\t", header = FALSE)
lifedata
head(lifedata, 10)
tail
colnames(lifedata) <- c("part", "time", "sex", "age", 
                        "diff", "bodytemp", "heartrate", 
                        "marriage", "job", "family", "edu", 
                        "support", "health")
##checking data set for errors 
summary(lifedata)
boxplot(lifedata$age)
boxplot(lifedata$bodytemp)
##checking for a missing value 
is.na(lifedata)
lifedata[rowSums(is.na(lifedata))>0,]
##correcting the missing data 

na.omit(lifedata)
lifedata[rowSums(is.na(lifedata))<2,]

lifedata<-lifedata[rowSums(is.na(lifedata)) <2,]
lifedata
summary(lifedata)
lifedata$bodytemp[is.na(lifedata$bodytemp)]<-median(lifedata$bodytemp,na.rm =TRUE)
lifedata$bodytemp[is.na(lifedata$bodytemp)]
 #column calling
lifedata[24,]
#checking for outliers 
boxplot.stats(lifedata$bodytemp)$out
outs<-boxplot.stats(lifedata$bodytemp)$out
##  Fix the dataset errors
na.omit(lifedata)
lifedata <- lifedata[rowSums(is.na(lifedata)) < 2,]
summary(lifedata)
lifedata[rowSums(is.na(lifedata)) > 0,]
lifedata$bodytemp[is.na(lifedata$bodytemp)] <- median(lifedata$bodytemp, na.rm = TRUE)
outs <- boxplot.stats(lifedata$bodytemp)$out
lifedata$bodytemp[lifedata$bodytemp %in% outs] <- median(lifedata$bodytemp, na.rm = TRUE)
summary(lifedata)
lifedata$age[lifedata$age == 1430] <- median(lifedata$age, na.rm = TRUE)
summary(lifedata)
lifedata <- droplevels(lifedata, exclude = "")
summary(lifedata)

#exploring Continous variables
pdf(file ="D:\\statistical analysis\\Output\\out.pdf")
sink("D:\\statistical analysis\\Output\\out.txt")
par(mfrow = c(1,2))
hist(lifedata$diff, xlab = "Difference Measure", main = NA, col = "skyblue")
#adding the margin around the histogram 
box()
##  Create a box plot of the dependent variable
boxplot(lifedata$diff, ylab = "Difference Measure", main = NA, col = "skyblue")
##  Restore the graphics device to single plot configuration
par(mfrow = c(1,1))
##  Add a title to the plot
title(main = "Difference Measure")
##  Create a data frame with dependent variable summary statistics
diff <- data.frame(n =length(lifedata$diff), min = min(lifedata$diff), 
                   max=max(lifedata$diff), mean=mean(lifedata$diff), 
                   median=median(lifedata$diff), var=var(lifedata$diff), 
                   sd=sd(lifedata$diff))
#print in 3 decimal places
print("diff")
print(diff,digits = 3)
par(mfrow = c(1,2))
hist(lifedata$age, xlab = "Age  Measure", main = NA, col = "blue")
boxplot(lifedata$age, ylab = "Age Measure", main = NA, col = "blue")
box()
par(mfrow = c(1,1))
title(main = "Age Measure")
par(mfrow = c(1,2))
hist(lifedata$age, xlab = "Age  Measure", main = NA, col = "blue")
boxplot(lifedata$age, ylab = "Age Measure", main = NA, col = "blue")
box()
par(mfrow = c(1,1))
title(main = "Age Measure")
agestats <- data.frame(n =length(lifedata$age), min = min(lifedata$age), 
                   max=max(lifedata$age), mean=mean(lifedata$age), 
                   median=median(lifedata$age), var=var(lifedata$age), 
                   sd=sd(lifedata$age))
print("agestats")
print(agestats,digits = 3)
##  Stop the sink and close the graphics device

##  Remove age and diff objects from workspace
rm(agestats)
rm(diff)
##	Explore categorical variables and run sample statistics:
##  table performs counts at each factor level of the variable
table(lifedata$sex)
table(lifedata$marriage)
##  Set up graphics device to display a 2 row by 3 column configuration of plots
par(mfrow = c(2,3))
#Create bar plots of proportion of subjects in each factor of the categorical variable
barplot(table(lifedata$sex)/length(lifedata$sex), col = "red", ylim = c(0,1),
        xlab = "sex", ylab = "Proportion", main = "Sex of Participants",
        names = c("Female", "Male"))
box()
barplot(table(lifedata$marriage)/length(lifedata$marriage), col = "red", ylim = c(0,1),
        xlab = "Married", ylab = "Proportion", main = "Marital Status",
        names = c("No", "Yes"))
box()
##  Restore the graphics device to single plot configuration
par(mfrow=c(1,1))
###  Categorical Variables  ###

##  Attach the lifedata (so we can use variable names without having to reference the column)
attach(lifedata)
##  Apply Factors to Variables (so that we can use more detailed descriptors as labels)
sex <- factor(sex, labels = c("Female", "Male"))
marriage = factor(marriage, labels=c("Not Married", "Married"))
job = factor(job, labels=c("Unemployed", "Employed"))
family = factor(family, labels=c("No Family", "Have Family"))
edu = factor(edu, labels=c("HS", "UGrad", "Grad"))
summary(sex)
##  Create data frames for the dependent variable and categorical IVs
lifedata.dv <- diff
lifedata.cat <- data.frame(sex, marriage, job, family, edu)

##  Create vector of Variable Names
vars.cat <- colnames(lifedata.cat)

##  Function for analysis of categorical IVs against the continuous


concatfun <- function(eachvar, name, response){
  ## Print summary stats of the DV by each IV and factor 
  print(tapply(response, eachvar, summary))
  ## Create box plot of DV by each IV and factor
  boxplot(response ~ eachvar, main=name, ylab="Difference Measure")
  ## if statement adds a conditional branch to the program flow
  ## here if the variable has two levels (factors i.e. male/female)
  ## the first branch is executed, if more than two levels the second 
  ## branch (after the else if) is executed
  if(length(levels(eachvar)) == 2) {
    ## if 2 levels we can perform a two sample t test
    ttest <- t.test(response~eachvar)
    print(ttest)
    ## Here we have a nested if else to deal with the t test p value
    ## if the p value is <0.001 (very significant) we store that as a
    ## text string to variable pval, else we round the value and store to pval
    if(ttest$p.value < 0.001){
      pval <- "<0.001"
    } else{
      pval <- round(ttest$p.value, digits=4)
    }
    ## Here we are thinking about the output figure and display of the pval
    ## paste together "p-value" with the t test p value
    pvallab <- paste("p-value:", pval)
    ## Add that text to the box plot
    mtext(pvallab)
  } 
  else if(length(levels(eachvar)) > 2) {
    ## if more than two levels perform an ANOVA
    fit <- aov(response ~ eachvar, data=lifedata)
    fitsum<-summary(fit)
    print("ANOVA")
    print(fitsum)
    ## same p value treatment as above
    if(fitsum[[1]][, 5][1] < 0.001){
      pval <- "<0.001"
    } else{
      pval <- round(fitsum[[1]][, 5][1], digits=4)
    }
    pvallab <- paste("p-value:", pval)
    mtext(pvallab)
  }	
}
##  Loop for sending categorical variables to categorical analysis function defined above
##  loop iterates through the length of the cat variable data frame, pulling out one 
##  variable per iteration

for(i in  1:length(lifedata.cat)){
  eachvar <- lifedata.cat[,i]
  name <- vars.cat[i]
  concatfun(eachvar, name, lifedata.dv)
}
sink()
dev.off()
##  Create data frame for the continuous IVs
lifedata.con <- data.frame(age, bodytemp, heartrate, support, health)

##  Create vector of Variable Names
vars.con <- colnames(lifedata.con)

##  Loop through con variables, plotting each against DV
for(i in 1:length(lifedata.con)){
  eachvar <- lifedata.con[,i]
  name <- vars.con[i]
  plot(eachvar, lifedata.dv, xlab = name, ylab = "Difference Measure")
}
##  Function for analysis of continuous IVs against continuous DV
conconfun <- function(eachvar, name, response){
  ## Create nice text header for stats output
  header <- paste("##########", name, "##########")
  ## print the nice header
  print(header)
  ## print the results of a Pearson's Correlation
  print(cor.test(response, eachvar))
  ## Plot IV vs DV as scatter plot
  plot(eachvar, response, xlab=name, ylab="Difference Measure", type="p", pch=19, col="black")
  ## Fit a linear model
  fit <- lm(response ~ eachvar)
  ## Add red line of model fit to plot
  abline(fit, col="red", lwd=2)
  ## Create summary of model fit
  fitsum <- summary(fit)
  ## print model summary
  print(fitsum)
  ## if else similar to previous cat function to parse out p values
  if(fitsum$coef[2,4] < 0.001){
    pval <- "<0.001"
  } else{
    pval <- round(fitsum$coef[2,4], digits=4)
  }
  ## create text string for adjusted r squared and p value
  labtext <- paste("Adj. R^2:", round(fitsum$adj.r.squared, digits=4), "p-value:", pval)
  ## add text string to plot
  mtext(labtext, side=3) 
  ## Set graphics device using layout command to a 2x2 grid that will be populated by 4 figures
  layout(matrix(c(1,2,3,4),2,2))
  ## Plot the model diagnostics
  plot(fit)
  ## Reset graphics device
  par(mfrow=c(1,1))
}
for(i in 1:length(lifedata.con)){
  eachvar <- lifedata.con[,i]
  name <- vars.con[i]
  conconfun(eachvar, name, lifedata.dv)
}

