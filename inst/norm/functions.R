propTest <- function(x){
  xbar <- mean(x)
#print(xbar)
  sD <- sd(x)

#print(sD)
  n <- length(x)

  count1 <- 0
  for (i in 1:n){
    if (x[i] < xbar - sD ) {count1 = count1 + 1}
    #print("a")}
    if (x[i] > xbar + sD) {count1 = count1 +1}
    #print("b")}
    count1 = count1
  }
  #print(count1)


  count2 <- 0
  for (i in 1:n){
    if (x[i] < xbar - 2*sD) {count2 = count2 + 1}
    if (x[i] > xbar + 2*sD) {count2 = count2 +1}
    count2 = count2
  }
  #print(count2)

  oneSD <- ifelse(abs((1-count1/n)-.683) > 1.396/sqrt(n), "Fail", "Pass")
  twoSD <- ifelse(abs((1-count2/n)-.954) > .628/sqrt(n), "Fail", "Pass")


  if(identical(oneSD, twoSD)){
    ret <- oneSD
  }else {
    ret <- "Fail"}

  final <- list("Result" = ret,
       "PropOne" = 1-count1/n,
       "PropTwo" = 1-count2/n)

}

propPlot <- function(vec){
  mu <- mean(vec)
  dev <- sd(vec)
  a <- mu + dev
  b <- mu - dev
  c <- mu + 2 * dev
  d <- mu - 2 * dev


  p<-ggplot(data=data.frame("data"=vec), aes(data))+
    geom_freqpoly(bins=length(vec)/3)+
    theme_classic()+
    geom_vline(xintercept = a,
               color="blue",
               lty=2)+
    geom_vline(xintercept = b,
               color="blue",
               lty=2)+
    geom_vline(xintercept = c,
               color="red",
               lty=2)+
    geom_vline(xintercept = d,
               color="red",
               lty=2)+
    geom_vline(xintercept = mu,
               color= "black",
               lty=2)

  suppressWarnings(p)
}
