

cluster_size_prob = function(r0, kappa, seed, max_size){
  prob_list = list(NA)
  
  for (k in 1:length(r0)){
    prob = integer(max_size)
    for( i in 1:max_size){
      prob[i] = exp(lgamma(kappa[k] * i + i - 1) - lgamma(kappa[k]*i) - lgamma(i +1) + (i-1)*log(r0[k]/kappa[k]) -(kappa[k] * i + i - 1 ) * log(1 + r0[k]/kappa[k]))
    }
    prob_list[[k]] = data.frame(prob)
  }
  
  over_size_prob_list = list(NA)
  
  for (k in 1:length(r0)){
    over_size_prob = integer(max_size)
    for( j in 1:c(max_size)){
      if( j ==1 ){
        over_size_prob[c(j)] = 1
      } else {
        over_size_prob[c(j)] = 1 - sum(prob_list[[k]][1:c(j),])
      }
    }
    over_size_prob_list[[k]] = data.frame(over_size_prob)
  }
  
  over_size_prob_more_seed_list = list(NA)
  
  for (k in 1:length(r0)){
    over_size_prob_more_seed = integer(max_size)
    for (j in 1:max_size ){
      over_size_prob_more_seed[j]  = 1 - ( 1 - over_size_prob_list[[k]][j,])^seed
    }    over_size_prob_more_seed_list[[k]] = data.frame(over_size_prob_more_seed)
  }
  
  results1 = do.call(cbind.data.frame, prob_list)
  results2 = do.call(cbind.data.frame, over_size_prob_list)
  results3 = do.call(cbind.data.frame, over_size_prob_more_seed_list)
  
  return(list(results1,results2,results3))
  
}
