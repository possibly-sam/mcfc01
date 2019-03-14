# asdf


# Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) 
# Ramin Takin
# Phillip Abbott
# Liza French
# Annette Z
# Anne-Marie
# Ken Allan

# setwd("/home/phillip/Documents/stt/project-hack-ii/mcfc01/mcfc01")
Risk <- read.csv("./risk_KA_11.csv", stringsAsFactors = FALSE)
# Risk <- read.csv("/home/phillip/Documents/stt/project-hack-ii/mcfc01/mcfc01/risk_KA_10.csv", stringsAsFactors = FALSE)

# ignore null values  ==> 129824 rows
Risk <- Risk[Risk$Risk__Value__c!="NULL",] 
Risk <- Risk[Risk$Business_Unit__c!="#N/A",] # filter out the summation row
Risk <- Risk[!is.na(Risk$INDEX),]

#Risk <- Risk[Risk$Risk__Category__c=="Cost", ]
# Risk <- Risk[Risk$Risk__Value__c!="0.00",] 

Risk <- Risk %>% mutate(worst=as.numeric(as.character(Risk__Worst_Case__c)))
Risk <- Risk %>% mutate(expected=as.numeric(as.character(Risk__Value__c)))
Risk <- Risk %>% mutate(best=as.numeric(as.character(Risk__Best_Case__c)))
Risk$prob <- abs(Risk$prob)




# Risk <- Risk %>% mutate( betweenesstest = (worst-expected)*(expected-best))

# those with no inclusion, suspect a typo; off by factor of 10 either way.  but only 137
#Risk <- Risk[0 <= Risk$betweenesstest,  ]

#Risk$worst[ Risk$Risk__Type__c=="Opportunity"] <- -Risk$worst[ Risk$Risk__Type__c=="Opportunity"]
#Risk$expected[ Risk$Risk__Type__c=="Opportunity"] <- -Risk$expected[ Risk$Risk__Type__c=="Opportunity"]
#Risk$best[ Risk$Risk__Type__c=="Opportunity"] <- -Risk$best[ Risk$Risk__Type__c=="Opportunity"]

# Risk <- Risk %>% cbind( prob=runif( nrow(Risk) ))

mc <- environment(emptyenv())


# collapse, like a quantum wave function
mc $ collapse <- function(best, typical, worst, p=1) {
  
  if (p < runif(1) ) return (0) # risk didn't trigger 
  
  if (typical < best || worst < typical) {
    warning("Bad range in collapse()")
    return(0)
    tempp_ <- best
    best <- worst
    worst <- tempp_
  }
  
  mu <- (best + 4*typical + worst)/6
  sd <- sqrt( abs((mu-best)*(worst-mu))/7 )
  #sd <- abs((worst-best)/2)
  
  # cat(paste(sd," ", mu))
  rnorm(1, mu, sd)
  
}

# precondition:  the_table has the expected columns:  best, expected, worst, prob
mc$collapse_table <- function(the_table) {
  mapply( mc$collapse, the_table$best, the_table$expected, the_table$worst, the_table$prob)
}

mc$ collapse_table_subset_and_accumulate <- function(the_table, n) {
  
  the_table %>% sample_n(n) %>% mc$collapse_table() %>% sum() 
  
  
}

mc $ collapse_risk <- function(it) mc$collapse( it$best, it$expected, it$worst)


the_table <- Risk
n <- 5

the_cov <- .1
mc $ multivariate <- function( the_table, n, the_cov) {
  
  q0 <- the_table %>% sample_n(n)
  q0 <- q0[, c("best", "expected", "worst", "prob")]
  q0 <- q0 %>% mutate( mu= (best + 4*expected + worst)/6, sd=abs((worst-best)/2)  )
  fixed_ones <- q0[q0$sd==0,"mu"]
  q0 <- q0[q0$sd!=0, ]
  
  n <- q0 %>% nrow()
  
  covariance_matrix <- rep(the_cov, n*n) %>% matrix(n,n)
  
  for (k in 1:n) covariance_matrix[k,k] <- q0$sd[k]
  
  mvrnorm(1, q0$mu, covariance_matrix) %>% sum() + fixed_ones %>% sum()
  
}

the_cov <- .01
mc $ mv_prob <- function( the_table, n, the_cov = .01) {
  
  q0 <- the_table %>% sample_n(n)
  q0 <- q0[, c("best", "expected", "worst", "prob")]
  
  # create the covariance matrix for the probabilities
  covariance_matrix <- rep(the_cov, n*n) %>% matrix(n,n)
  for (k in 1:n) covariance_matrix[k,k] = 1/12 # variance of the uniform distribution
  unadjusted_probabilities <- runif(n)
  adjusted_probabilties_ <- mvrnorm(1, unadjusted_probabilities, covariance_matrix) 
  adjusted_probabilties <- pnorm( adjusted_probabilties_, unadjusted_probabilities, 1/12)
  
  q0 <- q0[adjusted_probabilties<= q0$prob, ]
  
  n <- q0 %>% nrow()
  
  result <- 0
  for (k in 1:n) {
    b0 <- q0[k,1]
    e0 <- q0[k,2]
    w0 <- q0[k,3]
    result <- result + mc$collapse(b0, e0, w0)
  }
  result
  
}

mc $ mc <- function( the_table, n) {
  
  q0 <- the_table %>% sample_n(n)
  q0 <- q0[, c("best", "expected", "worst", "prob")]
  
  
  result <- 0
  for (k in 1:n) {
    b0 <- q0[k,1]
    e0 <- q0[k,2]
    w0 <- q0[k,3]
    result <- result + mc$collapse(b0, e0, w0)
  }
  result
  
}

the_table <- Risk
numberoftimes <- 99
samplesize=9
mc $ manytimes <- function(the_table, numberoftimes, samplesize, the_cov = 0.01) {
  
 # if (0 == samplesize) samplesize <- the_table %>% nrow()
  
#  result <- 1:numberoftimes
#  result <- 0
#  for (k in 1:numberoftimes) result[k] <- mc$mc (the_table, samplesize)
#  result
  1:numberoftimes %>% lapply( function(it) q0 %>% mc$collapse_table_subset_and_accumulate(samplesize)) %>% unlist()
  
}

# mc $ manytimes(Risk, 9, 9) %>% hist( breaks=29)


##########################################################################################################
##########################################################################################################
##########################################################################################################


mc $ get_ <- function( it, the_table = Risk) {
  the_table[[it]] %>% unique() %>% sort()
  
}


mc $ get_business_units <- function( the_table = Risk) { mc $ get_("Business_Unit__c", the_table) }
mc $ get_risk_name <- function( the_table = Risk) { mc $ get_("grc__Risk_Name__c", the_table) }
mc $ get_risk_type <- function( the_table = Risk) { mc $ get_("Risk__Type__c", the_table) }
mc $ get_extract_date <- function( the_table = Risk) { mc $ get_("ExtractDate", the_table) }
mc $ get_status <- function( the_table = Risk) { mc $ get_("grc__Status__c", the_table) }
mc $ get_design_element <- function( the_table = Risk) { mc $ get_("grc__Status__c", the_table) }
mc $ get_contractual <- function( the_table = Risk) { mc $ get_("XactHE__Contractual_Allocation__c", the_table) }
mc $ get_category <- function( the_table = Risk) { mc $ get_("Risk__Category__c", the_table) }
mc $ get_mitigation <- function( the_table = Risk) { mc $ get_("Risk__Mitigation__c", the_table) }



mc $ nn <- function( x ) {
  if (x < -2) return ('Very Best');
  if (x < -1) return ('Best');
  if (x <  1) return ('Average');
  if (x <  2) return ('Worst');
  return ('Very Worst');
}


mc $ normalize <- function(x) {
  m <- mean(x);
  s <- sd(x);
  x <- (x-m)/s;
  x
}



the_cost <- mc $ manytimes(Risk, 101, 9, .01) 

mc $ make_histogram <- function(the_cost) {

  the_legend <- the_cost %>% mc$normalize() %>% sapply(mc$nn) %>% unlist()

  q0 <- data.frame(the_cost, the_legend)
  
  nbin <- (the_cost %>% length())/10 %>% round()
  nbin <- nbin %>% max(10)

   ggplot(q0, aes(x=the_cost, fill=the_legend))    +    geom_histogram(alpha=0.5, position="identity", bins=nbin )    + theme(axis.line = element_line(size=rel(2)) ,text = element_text(size=24)) + labs(x="$",y="#")
   
   
  #ggplot(q0, aes(x=the_cost)) +    geom_histogram()
  
}


mc $ perc <- function(it, m, s) round(m+qnorm(it)*s)

mc $ some_nice_limits <- function(the_cost) {
  
  m <- mean(the_cost)
  s <- sd(the_cost)
  
  data.frame( `Very Best`= c("5%", mc$perc(.05,m,s)),
              `Best` = c("20%", mc$perc(.20,m,s)),
              `Average`=c("50%", mc$perc(.50,m,s)),
              `Worst` = c("80%", mc$perc(.80,m,s)),
              `Very Worst`=c("95%", mc$perc(.95,m,s))
  )
  
  
  
}






