wbcd <- read.csv("R/workspace/R-programming/ML/wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1]

var <- wbcd$concavity_mean

mean(var)
median(var)
quantile(var)
summary(var)

table(wbcd$diagnosis, wbcd$radius_mean)
table(wbcd$radius_mean, wbcd$diagnosis)

