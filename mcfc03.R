
# assume that Risk is a table and that  mc is an environment.  
# these assumptions will hold if you have already sourced mcfc01.R
# source("mcfc01.R")


# given prob, best, expected, worst
# create the discrete distribution:  spread them out into 100 buckets
# get mean & sd of that discrete distribution
# use formula for sum of mean and sum of sd
# create confidence intervals based on an assumption of normality


#  compare against the sampled monte carlo result
#  in the sequel, add correlation assumptions

mc  $ create_discrete_distribution <- function(prob, best, expected, worst) {
  
  
  if (is.na(prob) || 1 < prob || prob < 0) { warning("bad probability passed into create_discrete_distribution") ; return (rep(0,100)); }
  
  if ( is.na(best) || is.na(expected) || is.na(worst) || expected < best || worst < best || worst < expected) { warning("bad costs passed into create_discrete_distribution") ; return (rep(0,100)); }
  
  prob <- 100*prob;
  
  c ( rep(0, 100-prob),
      rep(best, round(prob/6) ),
      rep(expected, round(4*prob/6)),
      rep(worst, round(prob/6))
  )
  
}

mc $ discrete_mean <-   function(prob, best, expected, worst) { mc  $ create_discrete_distribution (prob, best, expected, worst) %>% mean() }
mc $ discrete_sd   <-   function(prob, best, expected, worst) { mc  $ create_discrete_distribution (prob, best, expected, worst) %>% sd() }


R2 <- Risk [Risk$Business_Unit__c=="A14 Section 2",]
R2 <- R2 [R2$Risk__Mitigation__c=="Target",]
R2 <- R2 [R2$Risk__Mitigation__c=="Target",]
R2 <- R2 [!is.na(R2$prob),]
R2 <- R2 [!is.na(R2$best),]
R2 <- R2 [!is.na(R2$expected),]
R2 <- R2 [!is.na(R2$worst),]
R2 <- R2 [(R2$best < R2$expected) & (R2$expected < R2$worst), ]

R2$prob <- R2$prob/100


# precondition:  it is a data frame containing the fields (prob, best, expected, worst)
mc $ calculate_discrete_statistics <- function (it)  {
  if (!is.data.frame(it)) { warning("bad data frame passed to calculate_discrete_statistics "); return (it); }
  
  mu    <- mapply(mc$discrete_mean, it$prob, it$best, it$expected, it$worst)
  sigma <- mapply(mc$discrete_sd, it$prob, it$best, it$expected, it$worst)
  
  it <- it %>% cbind(mu)
  it <- it %>% cbind(sigma)
  
  it
}

R2 <- R2 %>% mc $ calculate_discrete_statistics()


# precondition:  mu & sigma are 'columns'
mc $ discrete_aggregated <- function( mu, sigma ) {
  
  c( mu %>% mean(), sigma %*% sigma %>% sqrt())
  
}

mc $ discrete_confidence_interval <- function( mu, sigma ) qnorm( 1:9/10, mu, sigma)

mc $ discrete <- function (it) {
  
  it <- it %>% mc $ calculate_discrete_statistics()
  
  musigma <- mc$discrete_aggregated(it$mu, it$sigma)
  
  mc $ discrete_confidence_interval( musigma[1], musigma[2])
  
  
  
}

R2 %>% mc$ discrete() %>% plot()    



#####################################################################################
#####################################################################################
#####################################################################################



mc $ filter_bad_data <- function( Risk_ ) {
  
  
  
  
  R2 <- Risk_
  R2 <- R2 [!is.na(R2$prob),]
  R2 <- R2 [!is.na(R2$best),]
  R2 <- R2 [!is.na(R2$expected),]
  R2 <- R2 [!is.na(R2$worst),]
  R2 <- R2 [(R2$best < R2$expected) & (R2$expected < R2$worst), ]
  
  R2$prob <- R2$prob/100
  
  
  if (10 < R2 %>% nrow()) R2 <- R2[1:10, ]
  
}

# the_cost = the regular histogram
# Risk_ = the raw (filtered) data, from which we will generate the discrete approximation
mc $ make_histogram_discrete <- function(the_cost, Risk_) {
  
  
  
  some_discrete_stuff <- Risk_ %>% mc $ filter_bad_data() %>% mc$ discrete() 
  
  
  
  the_legend <- the_cost %>% mc$normalize() %>% sapply(mc$nn) %>% unlist()
  
  q0 <- data.frame(the_cost, the_legend)
  
  nbin <- (the_cost %>% length())/10 %>% round()
  nbin <- nbin %>% max(10)
  
  ggplot(q0, aes(x=the_cost, fill=the_legend))    +    
    geom_histogram(alpha=0.5, position="identity",bins=nbin )    + 
    theme(axis.line = element_line(size=rel(2)) ,text = element_text(size=24)) + 
    labs(x="$",y="#") +
    geom_vline(aes(xintercept = some_discrete_stuff[1])) +
  geom_vline(aes(xintercept = some_discrete_stuff[2])) +
  geom_vline(aes(xintercept = some_discrete_stuff[3])) +
  geom_vline(aes(xintercept = some_discrete_stuff[4])) +
  geom_vline(aes(xintercept = some_discrete_stuff[5])) +
  geom_vline(aes(xintercept = some_discrete_stuff[6])) +
  geom_vline(aes(xintercept = some_discrete_stuff[7])) + 
  geom_vline(aes(xintercept = some_discrete_stuff[8])) +
  geom_vline(aes(xintercept = some_discrete_stuff[9]))

  
  
  
  
}
  




















