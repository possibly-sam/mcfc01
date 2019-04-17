
mc <- new.env(emptyenv())

# the_file_name <- "./risk_KA_11.csv"

# read the table from the specified file name
mc $ get_table <- function (the_file_name) { read.csv(the_file_name, stringsAsFactors = FALSE) }





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


mc $ CRiskItem <- function ( p, b, e, w) {
  
  result <- new.env(emptyenv())
  
  result $ p = p
  result $ b = b
  result $ e = e
  result $ w = w
  
  result $ collapse <- function() mc$collapse( result$b, result$e, result$w, result$p)
  
  result $ mu <- function() result$p*( result$b + 4*result$e + result$w) / 6
  result $ sigma <- function() p* ((result$w-result$mu())* (result$mu() - result$b) / 7 ) %>% abs() %>% sqrt()
  
  #result $ collapsed <- result$ collapse()
  ##result $ mu_ <- result$mu()
  #result $ sigma <- result$sigma()
  
  
  result
  
  
  
  
}




# This is the good one.
mc $ CMCTable <- function( the_table ) {
  
  # the_table %>% nrow() %>% cat()
  
  result <- new.env(emptyenv())
  result$ my_table <- the_table
  
  result $ get_size <- function() result$ my_table  %>% nrow()
  
  result $ clean_table <- function() {
    
    Risk <- result$ my_table 
    
    # ignore null values  ==> 129824 rows
    Risk <- Risk[Risk$Risk__Value__c!="NULL",] 
    Risk <- Risk[Risk$Business_Unit__c!="#N/A",] # filter out the summation row
    Risk <- Risk[!is.na(Risk$INDEX),]
    
    # might as well just clean up the silly stuff at the beginning

    Risk <- Risk [!is.na(Risk$prob),]
    Risk <- Risk [0 <= Risk$prob,]
    Risk$prob <- Risk$prob/100
    
    Risk <- Risk %>% mutate(worst=as.numeric(as.character(Risk__Worst_Case__c)))
    Risk <- Risk %>% mutate(expected=as.numeric(as.character(Risk__Value__c)))
    Risk <- Risk %>% mutate(best=as.numeric(as.character(Risk__Best_Case__c)))
    Risk <- Risk %>% mutate(mu= mc $ CRiskItem(prob, best, expected, worst) $ mu() )
    Risk <- Risk %>% mutate(sigma= mc $ CRiskItem(prob, best, expected, worst) $ sigma() )
    

    Risk <- Risk [!is.na(Risk$best),]
    Risk <- Risk [!is.na(Risk$expected),]
    Risk <- Risk [!is.na(Risk$worst),]
    Risk <- Risk [(Risk$best < Risk$expected) & (Risk$expected < Risk$worst), ]
                            
    
    
    
    Risk
  } 
  
  result $ table_collapse <- function() mapply( mc$collapse, result$ my_table $best, result$ my_table $expected, result$ my_table $worst, result$ my_table $prob)
  
  
  # if we take a subset, we can assume that the collapse
  # of the whole would be proportional
  result $ collapse <- function(n=0)  {
    # take the whole table if n zero, not specified, or bigger than the whole table anyway.
    # the_table %>% nrow() %>% paste("") %>% cat()
    if (0 == n || result$get_size() < n) 
      return (result $ table_collapse() %>% sum())
    
    the_table_subset <- mc $ CMCTable( result$ my_table  %>% sample_n(n))
    result$get_size() *(the_table_subset$table_collapse() %>% sum() ) / n
  }
  
  # return a vector
  result $ many_collapses <- function(numberoftimes, sample_size=0) {
    1:numberoftimes %>% lapply( function(it)  result $ collapse(sample_size)) %>% unlist()
  }
  
  
  result $ get_filtered_data <- function(the_business_unit, the_extract_date, the_risk_mitigation) {  
    R2 <- result$ my_table 
    
    # R2 %>% nrow() %>% cat()
    
    if ("All" != the_business_unit)  R2 <- R2[ R2$Business_Unit__c==the_business_unit,]
    R2 <- R2[ R2$ExtractDate==the_extract_date,]
    R2 <- R2[ R2$Risk__Mitigation__c==the_risk_mitigation,]
    
    # R2 %>% nrow() %>% cat()
    
    R2[, c("prob",  "best",  "expected", "worst", "mu", "sigma")]
  }
  
  result $ get_collapsed_data <- function( ) {
    the_collapsed_table <- result$my_table
    # the_collapsed_table <- the_collapsed_table %>% mutate(instance=mc $ CRiskItem(prob, best, expected, worst) $ collapsed)
    the_collapsed_table <- the_collapsed_table %>% cbind( instance=mapply ( function(p, b, e, w) mc $ CRiskItem (p,b,e,w)$collapse() , 
                                                                   the_collapsed_table$prob, 
                                                                   the_collapsed_table$best, 
                                                                   the_collapsed_table$expected, 
                                                                   the_collapsed_table$worst)  )
    the_collapsed_table
  }
  
  result $ GetBusinessUnits  <- function() { result$my_table$Business_Unit__c    %>% unique() %>% sort() }
  result $ GetExtractDates   <- function() { result$my_table$ExtractDate         %>% unique() %>% sort() }
  result $ GetRiskMitigation <- function() { result$my_table$Risk__Mitigation__c %>% unique() %>% sort() }
  
  
  result
  
} # end class CMCTable

mc $ CMCCollapsedTable <- function(the_table) {
  
  result <- new.env(emptyenv())
  
  # the expected value of the sum is the sum of the expected values;
  result $ mu <- the_table$mu %>% sum()
  # TODO:  we can calculate the variance of the sum with the covariance matrix
  #        to account for correlation between the risk items;
  # the variance of the sum is the sum of the variances (independence assumption)
  result $ sigma <- the_table$sigma %*% the_table$sigma %>% sqrt()
  
  result $ instance <- the_table$instance %>% sum()
  
  result

  }




mc $ test_CMCTable <- function(the_file_name) {

  q0 <-   mc$get_table(the_file_name) %>% mc$CMCTable()

  q1 <- q0 $clean_table() %>% mc$CMCTable()

  q2 <- q1 $get_filtered_data("A14 Section 2", 43348, "Target" ) %>% mc$CMCTable()

  q0 %>% ls.str()

  q0$get_size()
  q1$get_size()
  q2$get_size()

  q2$many_collapses(99,99)

}

# "./risk_KA_11.csv" %>% mc $ test_CMCTable

# Two-Factor Authentication Backup Codes

slack <- read.csv(text='code
272467
579252
921833
986372
465389
215318
441593
308278
887649
905735')

