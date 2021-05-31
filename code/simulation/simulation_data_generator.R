
###########  step1: prepare the code which is used to creating simulation data, and wrap them as a function: simulation_data_generator

###########   parameter interpretation  #############
# @@ N_classes @@: sample size of all classes.  example: N_classes <- c(30, 40, 50)
# @@ N_var @@: number of total variables.  example: N_var <- 300
# @@ mean_classes @@: mean of signal variables in all classes.  example: mean_classes <- c(5, 4, 3)
# @@ N_clusters @@: size of each cluster.  example: N_clusters <- c(30, 30, 20, 15)
# @@ sigma_inner_clusters @@: correlation coefficient (sigma) in each cluster.  example: sigma_inner_clusters <- c(0.8, 0.5, 0.7, 0.8)
# @@ sigma_between_clusters @@: correlation coefficient (sigma) between two of all clusters. For each row, the first two elements are two cluster names, and the third element is the correlation coeffiecnt between these two clusters.
#    example??sigma_between_clusters <- rbind(c(1, 2, 0.3),
                                            # c(1, 3, 0.4),
                                            # c(1, 4, 0.2),
                                            # c(2, 3, 0.2))
# addNoise_correlation <- TRUE


### parameter example
# N_classes <- c(30, 40, 50)
# N_var <- 300
# mean_classes <- c(5, 4, 3)
# N_clusters <- c(30, 30, 20, 15)
# sigma_inner_clusters <- c(0.8, 0.5, 0.7, 0.8)
# sigma_between_clusters <- rbind(c(1, 2, 0.3),
#                                 c(1, 3, 0.4),
#                                 c(1, 4, 0.2),
#                                 c(2, 3, 0.2))
# addNoise_correlation <- TRUE


###########   main function   #############
# simulation_data_generator <- function(N_pos, N_neg, N_var, mean_neg, mean_pos, N_clusters, 
#                                       sigma_inner_clusters, sigma_between_clusters, addNoise_correlation=TRUE, Tol=1e-6){
  
simulation_data_generator <- function(N_classes, N_var, mean_classes, sd_mean_noise=0, N_clusters, 
                                      sigma_inner_clusters, sigma_between_clusters, sd_var, 
                                      addNoise_correlation=TRUE, sd_noise_correlation=0, Seed=123){
  ### creating intermediate variables
  N_cluster_total <- sum(N_clusters)    ## sum of the sizes of all clusters
  No_clusters <- length(N_clusters)     ## number of clusters
  
  ### change the type of sigma_between_clusters as dataframe
  sigma_between_clusters <- as.data.frame(sigma_between_clusters)
  colnames(sigma_between_clusters) <- c("cluster_name1", "cluster_name2", "sigma_between_two_clusters")
  
  ### create label
  Num_Class <- length(N_classes)
  Y <- rep(1:Num_Class, N_classes)
  
  ### empty correltion matrix
  correltion_matrix <- matrix(0, N_cluster_total, N_cluster_total)
  
  cluster_i <- 1
  for (cluster_i in 1:No_clusters) {
    # N_clusters_i <- N_clusters[cluster_i]
    position_begin <- ifelse(cluster_i==1, 1, sum(N_clusters[1:(cluster_i-1)])+1)
    position_end <- ifelse(cluster_i==1, N_clusters[1], sum(N_clusters[1:(cluster_i)]))
    
    correltion_matrix[position_begin:position_end, position_begin:position_end] <- sigma_inner_clusters[cluster_i]
    
  }
  
  # combination_i <- 1
  ##### assign the values of the elements in the correlation matrix according to ??sigma_between_clusters??
  for (combination_i in 1:nrow(sigma_between_clusters)) {
    cluster_name1_i <- sigma_between_clusters[combination_i, "cluster_name1"]
    cluster_name2_i <- sigma_between_clusters[combination_i, "cluster_name2"]
    
    # N_clusters_i_1 <- N_clusters[cluster_name1_i]
    # N_clusters_i_2 <- N_clusters[cluster_name2_i]
    
    position_begin_1 <- ifelse(cluster_name1_i==1, 1, sum(N_clusters[1:(cluster_name1_i-1)])+1)
    position_end_1 <- ifelse(cluster_name1_i==1, N_clusters[1], sum(N_clusters[1:(cluster_name1_i)]))
    
    position_begin_2 <- ifelse(cluster_name2_i==1, 1, sum(N_clusters[1:(cluster_name2_i-1)])+1)
    position_end_2 <- ifelse(cluster_name2_i==1, N_clusters[1], sum(N_clusters[1:(cluster_name2_i)]))
    
    correltion_matrix[position_begin_1:position_end_1, position_begin_2:position_end_2] <- sigma_between_clusters[combination_i, "sigma_between_two_clusters"]
    correltion_matrix[position_begin_2:position_end_2, position_begin_1:position_end_1] <- sigma_between_clusters[combination_i, "sigma_between_two_clusters"]
    
  }
  
  ##### assign the values of the elements in the diagonal to 1
  diag(correltion_matrix) <- 1
  
  ##### add noise on the correlation matrix
  if(addNoise_correlation==TRUE){
    set.seed(Seed)
    correltion_matrix_noise <- matrix(round(rnorm(N_cluster_total*N_cluster_total, 0, sd_noise_correlation),1), nrow=N_cluster_total, ncol=N_cluster_total)
    diag(correltion_matrix_noise) <- 0
    correltion_matrix <- correltion_matrix + correltion_matrix_noise
    correltion_matrix[correltion_matrix>1] <- 1
    
    ##### make the correlation matrix as symmetric matrix
    ut <- upper.tri(correltion_matrix)
    correltion_matrix[ut] <- t(correltion_matrix)[ut]
  }
  
  ##### convert correlation matrix to covariance matrix. They are same when the sd of each variable is equal to 1. 
  # library(MBESS)
  # covraiance_matrix <- MBESS::cor2cov(correltion_matrix, sd=rep(1, N_cluster_total))
  
  sd <- rep(sd_var, N_cluster_total)
  covraiance_matrix <- diag(sd) %*% correltion_matrix %*% diag(sd)
  
  ##### create signal data
  set.seed(Seed)
  data_final <- c()
  for (class_i in 1:Num_Class) {
    mean_class_i <- rep(mean_classes[class_i, ], N_clusters) + rnorm(N_cluster_total, 0, sd_mean_noise)
    # mean_class_i <- rep(mean_classes[class_i, ], N_clusters)
    N_class_i <- N_classes[class_i]
    
    set.seed(Seed)
    data_class_i <- mvtnorm::rmvnorm(n=N_class_i, mean=mean_class_i, sigma=covraiance_matrix, method = "svd")
    
    ###### create noise data
    set.seed(Seed)
    data_class_i_noise <- matrix(rnorm(N_class_i*(N_var-N_cluster_total), sd = sd_var), nrow = N_class_i, ncol = N_var-N_cluster_total)
    
    ##### bind the signal data and noise data
    data_class_i <- cbind(data_class_i, data_class_i_noise)
    
    ##### check the correlation matrix of simulation dataset
    correlation_caculated_class_i <- cor(data_class_i)
    
    data_final <- rbind(data_final, data_class_i)
    
  }
  
  ##### bind the positive sample and negative sample 
  data_final <- cbind(Y, data_final)
  names_X <- paste0("X", c(1:N_var))
  colnames(data_final) <- c("label", names_X)
  data_final <- as.data.frame(data_final)
  data_final$label <- factor(data_final$label)
  
  id <- rownames(data_final)
  data_final <- cbind(subID=id, data_final)
  
  data_final$subID<-1:length(data_final[,1])
  
  return(list(data_final=data_final))
  
}

