
wbcd <- read.csv("R/workspace/R-programming/ML/wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1]

table(wbcd$radius_se)
typeof(wbcd)
matrix_wbcd <- as.matrix(wbcd$radius_mean)
typeof(matrix_wbcd)
matrix_wbcd

wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
wbcdDiagnosisProbabilidade <- round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

var <- wbcd$concavity_mean

plot(var)#Scatterplots

barplot(var)#barplot
pie(var)#pieplot

stem(var)#stemplot
hist(var)#histogram

plot.ts(var)#timeplot

boxplot(var, var)#boxplot

cor(matrix_wbcd, use="complete.obs", method="pearson")


fit <- lm(radius_mean ~ smoothness_mean , data=wbcd)# Multiple Linear Regression  
summary(fit) # show results
plot(fit)



