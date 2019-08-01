#this function calculates t-statistic for two independent samples using mean, SD, sample size, and level of significance *******
#default value of significance is set at 0.05 ********
#m1, sd1, n1 are mean, std dev. and sample size of 1st treatment. m2, sd2, n2 are mean, std dev. and sample size of 1st treatment *****
#p = level of significance, which can be changed while running the function ******
#tl = two tailed or one tailed; default set to 2-tailed and can be changed to 1. ******
#This script was made by Ankur Jamwal on 31 July 2019***************
#Script update on 01 August 2019**************

t_test <- function(m1, m2, sd1, sd2, n1, n2, p = 0.05, tl = 2){
  t <- (m1-m2)/sqrt((sd1^2/n1)+(sd2^2/n2))
  df <- ((n1+n2-2))
  pt <- tl*pt(t,df)
  if(pt < p){
    paste0("there is significant difference between treatment at ", p, " level", " the t-statistic is ", t, " the p-value is ", pt, " and has ", df, " degrees of freedom")
  }
else {
  paste("treatments are not statistically significant at", p, "level.", "The t-statistics is ", t, " the p-value is ", pt)
}
}

t_test()


