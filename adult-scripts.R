#setwd("C:/Users/yxlre/OneDrive/Harvard Extension/11 - 2016Sp - MATH E-156 Mathematical Foundations of Statistical Software/Midterm Project")
# adult <- read.csv("adult.csv")

adult <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                    sep = ",", header = F, col.names = c("age", "workclass", "fnlwgt", "education",
                    "education_num", "marital_status", "occupation", "relationship", "race",
                    "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country",
                    "income"))
str(adult)

# Boxplot to compare education_num ~ income
boxplot(adult$education_num ~ adult$income, xlab = "income", ylab = "education_num")

# Boxplot to compare education_num ~ sex
boxplot(adult$education_num ~ adult$sex, xlab = "sex", ylab = "education_num")

table(adult$sex, adult$income)
plot(adult$sex, adult$income, xlab = "sex", ylab = "income")

# Test of independence of sex and income
chisq <- function(Obs) {
  Expected <- outer(rowSums(Obs), colSums(Obs) / sum(Obs))
  sum((Obs - Expected) ^ 2 / Expected)
}
obs <- chisq(table(adult$sex, adult$income)); obs # chisq()
chisq.test(table(adult$sex, adult$income)) # chisq.test()
summary(table(adult$sex, adult$income)) # summary(), notice the small p-value
N = 10^4-1; result <- numeric(N)
for (i in 1:N) {
  result[i] <- chisq(table(sample(adult$sex), adult$income))
}
hist(result, probability = T)
curve(dchisq(x, 1), add = TRUE, col = "red")
# abline(v = chisq(table(adult$sex, adult$income)))
pval <- (sum(result > obs) + 1) / (N + 1); pval

# Test of independence of sex and income on each education level (education_num 1 ~ 16)
pval.edu <- numeric(16)
name.edu <- character(16)
for (i in 1:16) {
  adult.subset <- subset(adult, education_num == i)
  name.edu[i] <- as.character(adult.subset[1, "education"])
  pval.edu[i] <- summary(table(adult.subset$sex, adult.subset$income))$p.value
  # print(paste(i, adult[which(adult$education_num == i)[1], "education"]), max.levels = 0)
}
par(cex.axis = 0.75)
plot(pval.edu, xlab = "", ylab = "p-value", xaxt = "n")
axis(1, at = 1:16, labels = name.edu, las = 2)
abline(v = 1:16, h = seq(0, 0.25, 0.01), col = "grey", lty = 3)
par(cex.axis = 1)
#

# Test of Difference in Coefficient of Variation (CV) in Capital Income between income levels
capital_revenue <- adult$capital_gain - adult$capital_loss
adult2 <- cbind(adult, capital_revenue)
capital_revenue.high <- capital_revenue[which(adult$income == " >50K" & capital_revenue != 0)]
capital_revenue.low <- capital_revenue[which(adult$income == " <=50K" & capital_revenue != 0)]
CV.high <- sd(capital_revenue.high) / mean(capital_revenue.high); CV.high

# HoHOHO! NoNoNo
CV.low <- sd(capital_revenue.low) / mean(capital_revenue.low); CV.low

# ####<<<<<<< HEAD
#THIS IS JUST A TEST FROM MATT SZUCS



=======

#Jon markdown
#Matt ggplot
#all enjoy weekend, bootstrap
#need short script
#hours per week by age, gender
>>>>>>> refs/remotes/origin/master
