
##### parameters
# @K: 类别数，number of class
# @n_train: 训练集总样本量，overall sample size of training dataset
# @n_test: 测试集总样本量，overall sample size of testing dataset
# @p: 变量个数，number of features
# @name_y: 因变量名???
# @names_x: 
# @num_bootstrap: bootstrap次数，times of bootstrap

# X=data_train_x
# Y=data_train_y
# NEW_X=data_test_x
# num_bootstrap=10

# pathname <- "brain_tumour_radiomics"

Adaptive_threshold_moving <- function(X, Y, NEW_X, num_bootstrap=10, pathname = "directory_temp", fold_i=fold_i, rep_i=rep_i, plot_scatter=FALSE){


if (!requireNamespace("e1071", quietly = TRUE))
install.packages("e1071", repos='https://cloud.r-project.org')

if (!requireNamespace("adabag", quietly = TRUE))
install.packages("adabag", repos='https://cloud.r-project.org')

if (!requireNamespace("metaheuristicOpt", quietly = TRUE))
install.packages("metaheuristicOpt", repos='https://cloud.r-project.org')

if (!requireNamespace("foreach", quietly = TRUE))
install.packages("foreach", repos='https://cloud.r-project.org')

if (!requireNamespace("doParallel", quietly = TRUE))
install.packages("doParallel", repos='https://cloud.r-project.org')

if (!requireNamespace("caret", quietly = TRUE))
install.packages("caret", repos='https://cloud.r-project.org')

if (!requireNamespace("glmnet", quietly = TRUE))
install.packages("glmnet", repos='https://cloud.r-project.org')

if (!requireNamespace("C50", quietly = TRUE))
install.packages("C50", repos='https://cloud.r-project.org')

if (!requireNamespace("naivebayes", quietly = TRUE))
install.packages("naivebayes", repos='https://cloud.r-project.org')

if (!requireNamespace("party", quietly = TRUE))
install.packages("party", repos='https://cloud.r-project.org')

if (!requireNamespace("wsrf", quietly = TRUE))
install.packages("wsrf", repos='https://cloud.r-project.org')

  
  library(openxlsx)
  # library(dplyr)
  library(ggplot2)
  
  library(ranger)
  library(e1071)
  library(adabag)
  library(C50)
  library(naivebayes)
  library(party)
  library(wsrf)
  library(caret)
  
  library(metaheuristicOpt)
  
  # library(rpart)
  
  library(CVXR)
  
  ############### 2. train ###############
  #### 2.1 train binary classifiers
  if(class(Y)!="factor") {Y <- factor(Y)}
  K <- length(levels(Y))
  
  class_combinations <- combn(c(1:K), 2)   ### combinations of two of all classes
  
  # combination_i <- 1
  
  name_y <- "Y"
  name_y_numerical <- "Y_binary_numerical"
  name_y_factor <- "Y_binary_factor"
  names_x <- colnames(X)
  
  data_train_y_x <- cbind(Y, X)
  data_train_y_x <- as.data.frame(data_train_y_x)
  colnames(data_train_y_x) <- c("Y", names_x)
  
  # model_names <- c("RandomForest", "SVM", "naivebayes", "C50", "ctree", "wsrf", "KNNreg", "KNNclassification", "LASSO", "AdaBoost", "C4.5", "CART", "MultilayerPerception")
  
  # model_names <- c("RandomForest", "SVM", "naivebayes", "C50", "ctree", "wsrf", "KNNclassification", "CART", "AdaBoost")
  model_names <- c("RandomForest", "SVM", "wsrf", "CART", "AdaBoost")
  #model_names <- c("AdaBoost")

  
  # num_bootstrap <- 10
  options(warn = 0)
  tictoc::tic()
  print('fit binary classifiers Start \n')  
  library(foreach)
  library(doParallel)
  cores <- detectCores()
  num_cores = floor(0.7*cores)
  
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  # stopCluster(cl)

  auc_model_list <- list()     ## normal for loop

  # auc_model_list <- foreach (bootstrap_i = 1:num_bootstrap) %dopar% {       ## dopar loop

  idx_inbag <- list()
  idx_outbag <- list()
  for(bootstrap_i in 1:num_bootstrap){    ## normal for loop
    cat(bootstrap_i, "\n")

    idx_inbag_temp <- c()
    for (K_i in 1:K) {
      idx_K_i <- c(1:length(Y))[is.element(Y, K_i)]

      set.seed(bootstrap_i)
      idx_inbag_K_i <- sample(idx_K_i, floor(0.6*length(idx_K_i)),  replace = FALSE)

      idx_inbag_temp <- c(idx_inbag_temp, idx_inbag_K_i)
    }
    idx_outbag_temp <- c(1:length(Y))[!is.element(c(1:length(Y)), idx_inbag_temp)]

    data_train_y_x_inbag <- data_train_y_x[idx_inbag_temp, ]
    data_train_y_x_outbag <- data_train_y_x[idx_outbag_temp, ]
    
    idx_inbag[[bootstrap_i]] <- idx_inbag_temp
    idx_outbag[[bootstrap_i]] <- idx_outbag_temp

    save(data_train_y_x_inbag, file=paste0("code/", pathname, "/data_train_y_x_inbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
    save(data_train_y_x_outbag, file=paste0("code/", pathname, "/data_train_y_x_outbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))

    # auc_model_list_bootstrap <- list()     ### normal for loop

    auc_model_list_bootstrap <- foreach(combination_i = c(1:ncol(class_combinations))) %dopar% {       ### dopar loop

    # for(combination_i in c(1:ncol(class_combinations))){     ### normal for loop
      cat("================combination_i ", combination_i, "==============\n")

      ## extract binary dataset
      class_1 <- class_combinations[1, combination_i]
      class_2 <- class_combinations[2, combination_i]

      ## inbag
      data_train_y_x_inbag_class1 <- data_train_y_x_inbag[data_train_y_x_inbag$Y==class_1, ]
      data_train_y_x_inbag_class2 <- data_train_y_x_inbag[data_train_y_x_inbag$Y==class_2, ]

      data_train_y_x_inbag_binary <- rbind(data_train_y_x_inbag_class1, data_train_y_x_inbag_class2)
      data_train_y_x_inbag_binary$Y_binary_numerical <- ifelse(data_train_y_x_inbag_binary$Y==class_1, 1, -1)
      data_train_y_x_inbag_binary$Y_binary_factor <- as.factor(data_train_y_x_inbag_binary$Y_binary_numerical)

      ## outbag
      data_train_y_x_outbag_class1 <- data_train_y_x_outbag[data_train_y_x_outbag$Y==class_1, ]
      data_train_y_x_outbag_class2 <- data_train_y_x_outbag[data_train_y_x_outbag$Y==class_2, ]

      data_train_y_x_outbag_binary <- rbind(data_train_y_x_outbag_class1, data_train_y_x_outbag_class2)
      data_train_y_x_outbag_binary$Y_binary_numerical <- ifelse(data_train_y_x_outbag_binary$Y==class_1, 1, -1)
      data_train_y_x_outbag_binary$Y_binary_factor <- as.factor(data_train_y_x_outbag_binary$Y_binary_numerical)



      auc_binary_list_bootstrap_combination <- list()     ### auc_binary_list_combination_bootstrap
      model_list_bootstrap_combination <- list()           ### model_list_combination_boostrap
      prediction_AllClass_inbag_list_bootstrap_combination <- list()
      prediction_AllClass_outbag_list_bootstrap_combination <- list()
      for (model_i in model_names) {
        cat("model_i ", model_i, "\n")
        if(model_i=="RandomForest"){

          num.trees_c <- seq(100, 1000, 200)
          min.node.size_c <- c(5, 10, 15)
          max.depth_c <- c(5, 8, 10)
          
#          num.trees_c <- seq(100, 500)
#          min.node.size_c <- c(5, 15)
#          max.depth_c <- c(5, 10)

          parameters_grid <- expand.grid(num.trees_c=num.trees_c, min.node.size_c=min.node.size_c, max.depth_c=max.depth_c)

          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          auc_temp <- c()
          for (row_i in 1:nrow(parameters_grid)) {
            modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
            model_fit <- ranger::ranger(formula = modelformula, data = data_train_y_x_inbag_binary,
                                        num.trees =parameters_grid$num.trees_c[row_i],
                                        min.node.size = parameters_grid$min.node.size_c[row_i],
                                        max.depth = parameters_grid$max.depth_c[row_i],
                                        importance = "impurity", probability = TRUE)
            
            model_fit_list_temp[[row_i]] <- model_fit
            
            prediction_temp <- predict(model_fit, data = data_train_y_x_outbag_binary)$predictions
            prediction_temp <- -1 + prediction_temp[, 2]*2
            auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp)$auc
            
            prediction_AllClass_inbag_temp <- predict(model_fit, data = data_train_y_x_inbag)$predictions
            prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
            prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
            
            prediction_AllClass_outbag_temp <- predict(model_fit, data = data_train_y_x_outbag)$predictions
            prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
            prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
          }
          # auc_temp <- max(auc_temp)
          auc_parameters_grid <- cbind(auc_temp, parameters_grid)

        } else if(model_i=="SVM"){

          type_c <- c("C-classification")
          kenel_c <- c("linear", "radial", "polynomial", "sigmoid")
          degree_c <- c(2, 3)    ## 	parameter needed for kernel of type polynomial (default: 3)
          # gamma_c <- c(0.001, 0.01, 0.1, 1)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
          gamma_c <- c(0.001, 1)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
          coef0_c <- c(0)    ##  parameter needed for kernels of type polynomial and sigmoid (default: 0)
          cost_c <- c(0.001, 0.01, 0.05, 0.1, 1)  	##  cost of constraints violation (default: 1)—it is the ‘C???-constant of the regularization term in the Lagrange formulation.
          nu_c <- c(0.1, 0.3, 0.5, 0.7, 1)   ##  parameter needed for nu-classification, nu-regression, and one-classification
          tolerance_c <- c(0.001)   ##  tolerance of termination criterion (default: 0.001)
          epsilon_c <- c(0.1)   ##  epsilon in the insensitive-loss function (default: 0.1)
          shrinking_c <- c("TRUE")   ##  option whether to use the shrinking-heuristics (default: TRUE)
          
          
#          type_c <- c("C-classification")
#          kenel_c <- c("linear", "polynomial")
#          degree_c <- c(2)    ## 	parameter needed for kernel of type polynomial (default: 3)
#          # gamma_c <- c(0.001, 0.01, 0.1, 1)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
#          gamma_c <- c(0.001)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
#          coef0_c <- c(0)    ##  parameter needed for kernels of type polynomial and sigmoid (default: 0)
#          cost_c <- c(0.001, 1)  	##  cost of constraints violation (default: 1)—it is the ‘C???-constant of the regularization term in the Lagrange formulation.
#          nu_c <- c(0.1, 0.3, 0.5, 0.7, 1)   ##  parameter needed for nu-classification, nu-regression, and one-classification
#          tolerance_c <- c(0.001)   ##  tolerance of termination criterion (default: 0.001)
#          epsilon_c <- c(0.1)   ##  epsilon in the insensitive-loss function (default: 0.1)
#          shrinking_c <- c("TRUE")   ##  option whether to use the shrinking-heuristics (default: TRUE)

          parameters_grid <- expand.grid(type_c=type_c, kenel_c=kenel_c, degree_c=degree_c, gamma_c=gamma_c, coef0_c=coef0_c,
                                         cost_c=cost_c, nu_c=nu_c, tolerance_c=tolerance_c, epsilon_c=epsilon_c, shrinking_c=shrinking_c)

          parameters_grid$degree_c <- ifelse(is.element(parameters_grid$kenel_c, c("polynomial")), parameters_grid$degree_c, 3)
          parameters_grid$gamma_c <- ifelse(is.element(parameters_grid$kenel_c, c("radial", "polynomial", "sigmoid")), parameters_grid$gamma_c, 0.5)
          parameters_grid$coef0_c <- ifelse(is.element(parameters_grid$kenel_c, c("polynomial", "sigmoid")), parameters_grid$coef0_c, 0)
          parameters_grid$nu_c <- ifelse(is.element(parameters_grid$type_c, c("nu-classification", "nu-regression")), parameters_grid$nu_c, 0.5)
          parameters_grid <- dplyr::distinct(parameters_grid)

          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          auc_temp <- c()
          for (row_i in 1:nrow(parameters_grid)) {
            cat(row_i, "\n")
            modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
            model_fit <-  e1071::svm(modelformula, data = data_train_y_x_inbag_binary,
                                     type = 'C-classification',
                                     kernel = parameters_grid$kenel_c[row_i],
                                     degree = parameters_grid$degree_c[row_i],
                                     gamma = parameters_grid$gamma_c[row_i],
                                     coef0_c = parameters_grid$coef0_c[row_i],
                                     cost = parameters_grid$cost_c[row_i],
                                     nu = parameters_grid$nu_c[row_i],
                                     tolerance = parameters_grid$tolerance_c[row_i],
                                     epsilon = parameters_grid$epsilon_c[row_i],
                                     shrinking = parameters_grid$shrinking_c[row_i],
                                     probability=TRUE)
                                     
            attr(model_fit$terms, ".Environment") <- NULL
            
            model_fit_list_temp[[row_i]] <- model_fit

            prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, probability = TRUE)
            prediction_temp <- attr(prediction_temp, "probabilities")
            prediction_temp <- -1 + prediction_temp[, 1]*2
            auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
            
            prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, probability = TRUE)
            prediction_AllClass_inbag_temp <- attr(prediction_AllClass_inbag_temp, "probabilities")
            prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 1]*2
            prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
            
            prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, probability = TRUE)
            prediction_AllClass_outbag_temp <- attr(prediction_AllClass_outbag_temp, "probabilities")
            prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 1]*2
            prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
          }
          # auc_temp <- max(auc_temp)
          auc_parameters_grid <- cbind(auc_temp, parameters_grid)


        } else if(model_i=="naivebayes"){
          library(naivebayes)
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          
          modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
          model_fit <-  naivebayes::naive_bayes(modelformula, data = data_train_y_x_inbag_binary)
          model_fit_list_temp[[1]] <- model_fit
          
          prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
          prediction_temp <- -1 + prediction_temp[, 2]*2
          auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
          auc_parameters_grid <- as.data.frame(auc_temp)
          
          prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
          prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
          prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
          
          prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
          prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
          prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
          
        } else if(model_i=="C50"){
          library(C50)

          x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x])
          y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_factor]
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          
          set.seed(318)
          model_fit <- C50::C5.0(x_train_binary_temp_inbag, y_train_binary_temp_inbag)
          model_fit_list_temp[[1]] <- model_fit
          
          x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
          y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_factor]
          prediction_temp <- predict(model_fit, newdata = x_train_binary_temp_outbag, type = "prob")
          prediction_temp <- -1 + prediction_temp[, 2]*2
          auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
          auc_parameters_grid <- as.data.frame(auc_temp)
          
          x_train_temp_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
          prediction_AllClass_inbag_temp <- predict(model_fit, newdata = x_train_temp_inbag, type = "prob")
          prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
          prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
          
          x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
          prediction_AllClass_outbag_temp <- predict(model_fit, newdata = x_train_temp_outbag, type = "prob")
          prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
          prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
          
        } else if(model_i=="ctree"){
          library(party)
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          
          modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
          model_fit <-  party::ctree(modelformula, data = data_train_y_x_inbag_binary)
          model_fit_list_temp[[1]] <- model_fit
          
          prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
          prediction_temp <- do.call(rbind, prediction_temp)
          prediction_temp <- -1 + prediction_temp[, 2]*2
          auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
          auc_parameters_grid <- as.data.frame(auc_temp)
          
          prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
          prediction_AllClass_inbag_temp <- do.call(rbind, prediction_AllClass_inbag_temp)
          prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
          prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
          
          prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
          prediction_AllClass_outbag_temp <- do.call(rbind, prediction_AllClass_outbag_temp)
          prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
          prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
          
        } else if(model_i=="RLT"){
#          library(RLT)
#          
#          x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x])
#          y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_numerical]
#          
#          model_fit_list_temp <- list()
#          prediction_AllClass_inbag_list_temp <- list()
#          prediction_AllClass_outbag_list_temp <- list()
#          
#          set.seed(318)
#          model_fit <- RLT::RLT(x_train_binary_temp_inbag, y_train_binary_temp_inbag, model = "classification", use.cores = 1)
#          model_fit_list_temp[[1]] <- model_fit
#          
#          x_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, names_x]
#          y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_numerical]
#          prediction_temp <- predict(model_fit, newdata = x_train_binary_temp_outbag)
#          # prediction_temp <- prediction_temp[,2]
#          auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
#          auc_parameters_grid <- as.data.frame(auc_temp)
#          
#          x_train_temp_inbag <- data_train_y_x_inbag[, names_x]
#          prediction_AllClass_inbag_temp <- predict(model_fit, newdata = x_train_temp_inbag)
#          # prediction_AllClass_inbag_temp <- prediction_AllClass_inbag_temp[, 2]
#          prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
#          
#          x_train_temp_outbag <- data_train_y_x_outbag[, names_x]
#          prediction_AllClass_outbag_temp <- predict(model_fit, newdata = x_train_temp_outbag)
#          # prediction_AllClass_outbag_temp <- prediction_AllClass_outbag_temp[, 2]
#          prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
          
        } else if(model_i=="wsrf"){
          library(wsrf)
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          
          modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
          model_fit <-  wsrf::wsrf(modelformula, data = data_train_y_x_inbag_binary, parallel=FALSE)
          model_fit_list_temp[[1]] <- model_fit
          
          prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
          prediction_temp <- prediction_temp$prob
          prediction_temp <- -1 + prediction_temp[, 2]*2
          auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
          auc_parameters_grid <- as.data.frame(auc_temp)
          
          prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
          prediction_AllClass_inbag_temp <- prediction_AllClass_inbag_temp$prob
          prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
          prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
          
          prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
          prediction_AllClass_outbag_temp <- prediction_AllClass_outbag_temp$prob
          prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
          prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
          
        } else if(model_i=="KNNreg"){
          library(caret)

          x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x])
          y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_numerical]

          k_c <- seq(-10, 10, 2) + sqrt(nrow(x_train_binary_temp_inbag))
          k_c <- k_c[k_c >= 2]
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          parameters_grid <- as.data.frame(k_c)
          auc_temp <- c()
          for(row_i in 1:nrow(parameters_grid)){
            model_fit <- caret::knnreg(x_train_binary_temp_inbag, y_train_binary_temp_inbag, k=sqrt(nrow(x_train_binary_temp_inbag)))
            model_fit_list_temp[[row_i]] <- model_fit
            
            x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
            y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_numerical]
            prediction_temp <- predict(model_fit, newdata = x_train_binary_temp_outbag)
            auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
            
            x_train_temp_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
            prediction_AllClass_inbag_temp <- predict(model_fit, newdata = x_train_temp_inbag)
            prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
            
            x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
            prediction_AllClass_outbag_temp <- predict(model_fit, newdata = x_train_temp_outbag)
            prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
          }
          auc_parameters_grid <- cbind(auc_temp, parameters_grid)


        } else if(model_i=="KNNclassification"){
          library(caret)
          
          x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x])
          y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_factor]
          
          k_c <- seq(-10, 10, 2) + sqrt(nrow(x_train_binary_temp_inbag))
          k_c <- k_c[k_c >= 2]
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          parameters_grid <- as.data.frame(k_c)
          auc_temp <- c()
          for(row_i in 1:nrow(parameters_grid)){
            model_fit <- caret::knn3(x_train_binary_temp_inbag, y_train_binary_temp_inbag, k=sqrt(nrow(x_train_binary_temp_inbag)))
            model_fit_list_temp[[row_i]] <- model_fit
            
            x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
            y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_factor]
            prediction_temp <- predict(model_fit, newdata = x_train_binary_temp_outbag)
            prediction_temp <- -1 + prediction_temp[, 2]*2
            auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
            
            x_train_temp_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
            prediction_AllClass_inbag_temp <- predict(model_fit, newdata = x_train_temp_inbag)
            prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
            prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
            
            x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
            prediction_AllClass_outbag_temp <- predict(model_fit, newdata = x_train_temp_outbag)
            prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
            prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
          }
          auc_parameters_grid <- cbind(auc_temp, parameters_grid)
          
          
        } else if(model_i=="LASSO"){
          # library("glmnet")

          x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x])
          y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_numerical]
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()

          set.seed(318)
          model_fit <- glmnet::cv.glmnet(x_train_binary_temp_inbag, y_train_binary_temp_inbag, family="binomial", maxit = 10000)
          model_fit_list_temp[[1]] <- model_fit
          # plot(cv.fit)
          # fit = glmnet(x_radio_train, y_train, family = "binomial", lambda = cv.fit$lambda.min)

          x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
          y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_numerical]
          prediction_temp <- predict(model_fit, newx = x_train_binary_temp_outbag, s = "lambda.min")
          prediction_temp <- c(prediction_temp)
          auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
          auc_parameters_grid <- as.data.frame(auc_temp)
          
          x_train_temp_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
          prediction_AllClass_inbag_temp <- predict(model_fit, newx = x_train_temp_inbag, s = "lambda.min")
          prediction_AllClass_inbag_temp <- c(prediction_AllClass_inbag_temp)
          prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
          
          x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
          prediction_AllClass_outbag_temp <- predict(model_fit, newx = x_train_temp_outbag, s = "lambda.min")
          prediction_AllClass_outbag_temp <- c(prediction_AllClass_outbag_temp)
          prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
          

        } else if(model_i=="AdaBoost"){
          library(adabag)
          # tictoc::tic()
          # boos_c <- c(TRUE, FALSE)
          boos_c <- c(TRUE)
          # mfinal_c <- seq(50, 150, 30)
          mfinal_c <- c(50, 150)
          minsplit_c <- c(5, 10)
          maxdepth_c <- c(5, 30)
          
          
#          mfinal_c <- c(50)
#          minsplit_c <- c(5)
#          maxdepth_c <- c(5)

          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          parameters_grid <- expand.grid(boos_c=boos_c, mfinal_c=mfinal_c, minsplit_c=minsplit_c, maxdepth_c=maxdepth_c)
          # a <- Sys.time()
          auc_temp <- c()
          for (row_i in 1:nrow(parameters_grid)) {
            cat(row_i, "\n")
            modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
            model_fit <-  adabag::boosting(modelformula, data = data_train_y_x_inbag_binary,
                                           boos = parameters_grid$boos_c[row_i],
                                           mfinal = parameters_grid$mfinal_c[row_i],
                                           coeflearn = "Breiman",    ### different coeflean correspond to different kind of adaboost. see details in help document.
                                           control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
                                                                   maxdepth = parameters_grid$maxdepth_c[row_i]))
            
            for(tree_i in seq_len(length(model_fit$trees))){
               attr(model_fit$trees[[tree_i]]$terms, ".Environment") = NULL
            }
            attr(model_fit$formula, ".Environment") <- NULL
            attr(model_fit$terms, ".Environment") <- NULL
            
            model_fit_list_temp[[row_i]] <- model_fit

            prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary)$prob
            prediction_temp <- -1 + prediction_temp[, 2]*2
            auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
            
            
            prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag)$prob
            prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
            prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
            
            
            prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag)$prob
            prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
            prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
          }
          # auc_temp <- max(auc_temp)
          auc_parameters_grid <- cbind(auc_temp, parameters_grid)
          # tictoc::toc()

        } else if(model_i=="C4.5"){
          # load the package
          library(RWeka)
          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()

          modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
          model_fit <-  RWeka::J48(modelformula, data = data_train_y_x_inbag_binary)
          model_fit_list_temp[[1]] <- model_fit
          
          prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "probability")
          prediction_temp <- -1 + prediction_temp[, 2]*2
          auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
          auc_parameters_grid <- as.data.frame(auc_temp)

          prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "probability")
          prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
          prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
          
          prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "probability")
          prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
          prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp

        } else if(model_i=="CART"){
          

          # modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
          # model_fit <-  rpart::rpart(modelformula, data = data_train_binary_temp)
          # prediction_temp <- predict(model_fit, newdata=data_train_binary_temp, type = "prob")
          # prediction_temp <- log(prediction_temp[,2]/prediction_temp[,1])

          minsplit_c <- c(5, 10, 15)
          maxdepth_c <- c(5, 8, 10, 20, 30)
          cp_c <- seq(0.05, 0.1, 0.01)

          
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          parameters_grid <- expand.grid(minsplit_c=minsplit_c, maxdepth_c=maxdepth_c, cp_c=cp_c)
          auc_temp <- c()
          for (row_i in 1:nrow(parameters_grid)) {
            modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
            model_fit <-  rpart::rpart(modelformula, data = data_train_y_x_inbag_binary,
                                       parms = list(split="gini"),
                                       control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
                                                               maxdepth = parameters_grid$maxdepth_c[row_i],
                                                               cp = parameters_grid$cp_c[row_i]))
            
            attr(model_fit$terms, ".Environment") <- NULL
            
            model_fit_list_temp[[row_i]] <- model_fit
            
            prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
            prediction_temp <- -1 + prediction_temp[, 2]*2
            auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
            
            prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
            prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
            prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
            
            prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
            prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
            prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
            # par(xpd=NA)
            # plot(model_fit)
            # text(model_fit, digits = 3)
            
          }
          # auc_temp <- max(auc_temp)
          auc_parameters_grid <- cbind(auc_temp, parameters_grid)


        } else if(model_i=="MultilayerPerception"){

          library(neuralnet)

          # neuralnet package
          # size_c <- c("5, 10, 5", "5, 10, 5", "10, 20, 10")
          size_c <- c("5", "5, 10")

          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          parameters_grid <- as.data.frame(size_c)
          auc_temp <- c()
          for(row_i in 1:nrow(parameters_grid)){
            set.seed(318)
            cat(row_i, "\n")

            size_temp <- as.numeric(strsplit(as.character(parameters_grid$size_c[row_i]), ",")[[1]])

            model_fit <- neuralnet::neuralnet(modelformula, data = data_train_y_x_inbag_binary,
                                              hidden = size_temp, linear.output = FALSE)
            model_fit_list_temp[[row_i]] <- model_fit

            if(inherits(try(predict(model_fit, newdata=data_train_y_x_outbag_binary)), "try-error")){
              auc_temp[row_i] <- 0.5
              prediction_temp <- rep(0, nrow(data_train_y_x_outbag_binary))
            } else {
              prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary)
              auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, c(prediction_temp), quiet=TRUE)$auc
            }
            
            
            if(inherits(try(predict(model_fit, newdata=data_train_y_x_inbag)), "try-error")){
              prediction_AllClass_inbag_temp <- rep(0, nrow(data_train_y_x_inbag))
            } else {
              prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag)
            }
            prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
            
            
            if(inherits(try(predict(model_fit, newdata=data_train_y_x_outbag)), "try-error")){
              prediction_AllClass_outbag_temp <- rep(0, nrow(data_train_y_x_outbag))
            } else {
              prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag)
            }
            prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
          }
          # auc_temp <- max(auc_temp)
          auc_parameters_grid <- cbind(auc_temp, parameters_grid)

        }

        # auc_binary_temp <- c(auc_binary_temp, auc_temp)
        auc_binary_list_bootstrap_combination[[match(model_i, model_names)]] <- auc_parameters_grid
        model_list_bootstrap_combination[[match(model_i, model_names)]] <- model_fit_list_temp
        prediction_AllClass_inbag_list_bootstrap_combination[[match(model_i, model_names)]] <- prediction_AllClass_inbag_list_temp
        prediction_AllClass_outbag_list_bootstrap_combination[[match(model_i, model_names)]] <- prediction_AllClass_outbag_list_temp

      }

      # auc_model_list_bootstrap[[combination_i]] <- auc_binary_list_bootstrap_combination
      # auc_model_list_bootstrap[[combination_i+ncol(class_combinations)]] <- model_list_bootstrap_combination

      auc_model_list_bootstrap_temp <- list(auc = auc_binary_list_bootstrap_combination, model=model_list_bootstrap_combination, prediction_inbag=prediction_AllClass_inbag_list_bootstrap_combination, prediction_outbag=prediction_AllClass_outbag_list_bootstrap_combination)

      # auc_model_list_bootstrap[[combination_i]] <- auc_model_list_bootstrap_temp   ## normal for loop
      auc_model_list_bootstrap_temp ## dopar loop

    }

    auc_model_list[[bootstrap_i]] <- auc_model_list_bootstrap   ## normal for loop
    # auc_model_list_bootstrap    ## dopar loop

  }

  stopCluster(cl)

  print('fit binary classifiers Done \n')
  tictoc::toc()

  
  save(idx_inbag, file=paste0("code/", pathname, "/idx_inbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
  save(idx_outbag, file=paste0("code/", pathname, "/idx_outbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
  save(auc_model_list, file=paste0("code/", pathname, "/auc_model_list_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_", paste0(model_names, collapse="_"), "_.RData"))
  
  
  # load(file=paste0("code/", pathname, "/auc_model_list_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_", paste0(model_names, collapse="_"), "_.RData"))
  # load(file=paste0("code/", pathname, "/data_train_y_x_inbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
  # load(file=paste0("code/", pathname, "/data_train_y_x_outbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))



#### 2.2 obtain the best parameter of each binary classifier

# bootstrap_i <- 1
# combination_i <- 3

idx_best_parameter_model_combination_bootstrap <- list()
clusterCtriteria_best_parameter_model_combination_bootstrap <- list()
auc_best_parameter_model_combination_bootstrap <- list()
prediction_AllClass_inbag_best_parameter_model_combination_bootstrap <- list()
prediction_AllClass_outbag_best_parameter_model_combination_bootstrap <- list()
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  idx_inbag_temp <- idx_inbag[[bootstrap_i]]
  idx_outbag_temp <- idx_outbag[[bootstrap_i]]
  
  data_train_y_x_inbag <- data_train_y_x[idx_inbag_temp, ]
  data_train_y_x_outbag <- data_train_y_x[idx_outbag_temp, ]


  idx_best_parameter_model_combination <- c()
  clusterCtriteria_best_parameter_model_combination <- c()
  auc_best_parameter_model_combination <- c()
  prediction_AllClass_inbag_best_parameter_model_combination <- list()
  prediction_AllClass_outbag_best_parameter_model_combination <- list()
  for (combination_i in c(1:ncol(class_combinations))) {
    cat("combination_i ", combination_i, "\n")
    
    class_1 <- class_combinations[1, combination_i]
    class_2 <- class_combinations[2, combination_i]
    
    auc_model_list_bootstrap <- auc_model_list[[bootstrap_i]]
    auc_binary_list_bootstrap_combination <- auc_model_list_bootstrap[[combination_i]]$auc
    prediction_AllClass_inbag_list_bootstrap_combination <- auc_model_list_bootstrap[[combination_i]]$prediction_inbag
    prediction_AllClass_outbag_list_bootstrap_combination <- auc_model_list_bootstrap[[combination_i]]$prediction_outbag

    idx_best_parameter_model <- c()
    clusterCtriteria_best_parameter_model <- c()
    auc_best_parameter_model <- c()
    prediction_AllClass_inbag_best_parameter_model <- c()
    prediction_AllClass_outbag_best_parameter_model <- c()
    for (model_i in model_names) {
      auc_parameters_grid <- auc_binary_list_bootstrap_combination[[match(model_i, model_names)]]
      
      prediction_AllClass_inbag_list_temp <- prediction_AllClass_inbag_list_bootstrap_combination[[match(model_i, model_names)]]
      prediction_AllClass_outbag_list_temp <- prediction_AllClass_outbag_list_bootstrap_combination[[match(model_i, model_names)]]
      cluster_criteria_parameters_inbag <- c()
      cluster_criteria_parameters_outbag <- c()
      for(parameter_i in seq_len(length(prediction_AllClass_outbag_list_temp))){
        
        idx_current_classes_inbag <- is.element(data_train_y_x_inbag$Y, c(class_1, class_2))
        idx_current_classes_outbag <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        
        prediction_AllClass_inbag_temp <- prediction_AllClass_inbag_list_temp[[parameter_i]]
        cluster_criteria_temp <- clusterCrit::intCriteria(as.matrix(prediction_AllClass_inbag_temp[idx_current_classes_inbag]), as.integer(data_train_y_x_inbag$Y[idx_current_classes_inbag]), crit = "Davies_Bouldin")$davies_bouldin
        cluster_criteria_parameters_inbag <- c(cluster_criteria_parameters_inbag, cluster_criteria_temp)
        
        prediction_AllClass_outbag_temp <- prediction_AllClass_outbag_list_temp[[parameter_i]]
        cluster_criteria_temp <- clusterCrit::intCriteria(as.matrix(prediction_AllClass_outbag_temp[idx_current_classes_outbag]), as.integer(data_train_y_x_outbag$Y[idx_current_classes_outbag]), crit = "Davies_Bouldin")$davies_bouldin
        cluster_criteria_parameters_outbag <- c(cluster_criteria_parameters_outbag, cluster_criteria_temp)
        
      }
      cluster_criteria_parameters_inbag <- ifelse(cluster_criteria_parameters_inbag==0, 0.001, cluster_criteria_parameters_inbag)
      cluster_criteria_parameters_outbag <- ifelse(cluster_criteria_parameters_outbag==0, 0.001, cluster_criteria_parameters_outbag)
      cluster_criteria_parameters_temp <- cbind(cluster_criteria_parameters_inbag/cluster_criteria_parameters_outbag, cluster_criteria_parameters_outbag/cluster_criteria_parameters_inbag)
      cluster_criteria_parameters_temp <- apply(cluster_criteria_parameters_temp, 1, max)*(cluster_criteria_parameters_inbag+cluster_criteria_parameters_outbag)
      cluster_criteria_parameters <- cluster_criteria_parameters_temp
      auc_clusterCriteria_parameters_grid <- cbind(auc_parameters_grid, cluster_criteria_parameters)
      
      if(max(auc_clusterCriteria_parameters_grid[, 1]) > 0.9){
        # auc_clusterCriteria_parameters_grid <- auc_clusterCriteria_parameters_grid[auc_clusterCriteria_parameters_grid[, 1]>0.9, ]
        auc_include_boolean <- auc_clusterCriteria_parameters_grid[, 1]>0.9
      } else {
        # auc_clusterCriteria_parameters_grid <- auc_clusterCriteria_parameters_grid[which.max(auc_clusterCriteria_parameters_grid[, 1]), ]
        auc_include_boolean <- auc_clusterCriteria_parameters_grid[, 1]==max(auc_clusterCriteria_parameters_grid[, 1])
      }
      
      order_clusterCriteria <- order(auc_clusterCriteria_parameters_grid[, ncol(auc_clusterCriteria_parameters_grid)])
      auc_include_boolean <- auc_include_boolean[order_clusterCriteria]
      order_clusterCriteria <- order_clusterCriteria[auc_include_boolean]
      idx_best_parameter_temp <- order_clusterCriteria[1]
      clusterCtriteria_best_parameter_temp <- auc_clusterCriteria_parameters_grid[idx_best_parameter_temp, ncol(auc_clusterCriteria_parameters_grid)]
      auc_best_parameter_temp <- auc_clusterCriteria_parameters_grid[idx_best_parameter_temp, 1]
      prediction_AllClass_inbag_best_parameter_temp <- prediction_AllClass_inbag_list_temp[[idx_best_parameter_temp]]
      prediction_AllClass_outbag_best_parameter_temp <- prediction_AllClass_outbag_list_temp[[idx_best_parameter_temp]]

      idx_best_parameter_model <- c(idx_best_parameter_model, idx_best_parameter_temp)
      clusterCtriteria_best_parameter_model <- c(clusterCtriteria_best_parameter_model, clusterCtriteria_best_parameter_temp)
      auc_best_parameter_model <- c(auc_best_parameter_model, auc_best_parameter_temp)
      prediction_AllClass_inbag_best_parameter_model <- cbind(prediction_AllClass_inbag_best_parameter_model, prediction_AllClass_inbag_best_parameter_temp)
      prediction_AllClass_outbag_best_parameter_model <- cbind(prediction_AllClass_outbag_best_parameter_model, prediction_AllClass_outbag_best_parameter_temp)
      
    }
    colnames(prediction_AllClass_inbag_best_parameter_model) <- model_names
    colnames(prediction_AllClass_outbag_best_parameter_model) <- model_names
    idx_best_parameter_model_combination <- cbind(idx_best_parameter_model_combination, idx_best_parameter_model)
    clusterCtriteria_best_parameter_model_combination <- cbind(clusterCtriteria_best_parameter_model_combination, clusterCtriteria_best_parameter_model)
    auc_best_parameter_model_combination <- cbind(auc_best_parameter_model_combination, auc_best_parameter_model)
    prediction_AllClass_inbag_best_parameter_model_combination[[combination_i]] <- prediction_AllClass_inbag_best_parameter_model
    prediction_AllClass_outbag_best_parameter_model_combination[[combination_i]] <- prediction_AllClass_outbag_best_parameter_model
  
  }
  idx_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- idx_best_parameter_model_combination
  clusterCtriteria_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- clusterCtriteria_best_parameter_model_combination
  auc_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- auc_best_parameter_model_combination
  prediction_AllClass_inbag_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- prediction_AllClass_inbag_best_parameter_model_combination
  prediction_AllClass_outbag_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- prediction_AllClass_outbag_best_parameter_model_combination

}



#### 2.3 obtain the optimal combinations of binary classifiers according to clustering criteria
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  clusterCtriteria_best_parameter_model_combination <- clusterCtriteria_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  auc_best_parameter_model_combination <- auc_best_parameter_model_combination_bootstrap[[bootstrap_i]]

  clusterCtriteria_best_parameter_model_combination[auc_best_parameter_model_combination < 0.9] <- NA
  
  idx_models <- apply(clusterCtriteria_best_parameter_model_combination, 2, which.min)
  idx_combinations <- c(1:ncol(class_combinations))
  
  print("idx_combinations")
  print(idx_combinations)
  print("idx_models")
  print(idx_models)
  
}

write.table(idx_combinations, file=paste0("code/", pathname, "/idx_combinations_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.txt"))
write.table(idx_models, file=paste0("code/", pathname, "/idx_models_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.txt"))


#### 2.4 obtain the prediction of each best binary classifier 

model_storage_list <- vector(mode = "list", length = num_bootstrap)

for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  idx_inbag_temp <- idx_inbag[[bootstrap_i]]
  idx_outbag_temp <- idx_outbag[[bootstrap_i]]
  
  data_train_y_x_inbag <- data_train_y_x[idx_inbag_temp, ]
  data_train_y_x_outbag <- data_train_y_x[idx_outbag_temp, ]
  
  
  idx_best_parameter_model_combination <- idx_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  
  # prediction_AllClass_inbag_best_parameter_model_combination <- prediction_AllClass_inbag_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  # prediction_AllClass_outbag_best_parameter_model_combination <- prediction_AllClass_outbag_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  
  # cluster_criteria_bootstrap_combination <- c()
  prediction_train_inbag_AllBinaryClassifiers <- c()
  prediction_train_outbag_AllBinaryClassifiers <- c()
  best_model_names <- c()
  for (combination_i in seq_len(length(idx_combinations))) {
    cat("combination_i ", combination_i, "\n")
    
    idx_best_combination <- idx_combinations[combination_i]
    idx_best_model <- idx_models[combination_i]
    idx_best_parameter <- idx_best_parameter_model_combination[idx_best_model, idx_best_combination]
    
    
    # prediction_AllClass_inbag_best_parameter_model <- prediction_AllClass_inbag_best_parameter_model_combination[[idx_best_combination]]
    # prediction_AllClass_outbag_best_parameter_model <- prediction_AllClass_outbag_best_parameter_model_combination[[idx_best_combination]]
    
    # prediction_AllClass_inbag_best_parameter <- prediction_AllClass_inbag_best_parameter_model[, idx_best_model]
    # prediction_AllClass_outbag_best_parameter <- prediction_AllClass_outbag_best_parameter_model[, idx_best_model]
    
    # prediction_inbag <- prediction_AllClass_inbag_best_parameter
    # prediction_outbag <- prediction_AllClass_outbag_best_parameter
    
    
    best_model_names <- c(best_model_names, paste0("combination_", idx_best_combination, "_", model_names[idx_best_model]))

    model_fit <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$model[[idx_best_model]][[idx_best_parameter]]
    
    model_storage_list[[bootstrap_i]][[idx_best_combination]] <- model_fit

    if(is.element(model_names[idx_best_model], c("RandomForest"))){
      prediction_inbag <- predict(model_fit, data=data_train_y_x_inbag, probability = TRUE)$predictions
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      prediction_outbag <- predict(model_fit, data=data_train_y_x_outbag, probability = TRUE)$predictions
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("SVM"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, probability = TRUE)
      prediction_inbag <- attr(prediction_inbag, "probabilities")
      prediction_inbag <- -1 + prediction_inbag[, 1]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, probability = TRUE)
      prediction_outbag <- attr(prediction_outbag, "probabilities")
      prediction_outbag <- -1 + prediction_outbag[, 1]*2
    } else if(is.element(model_names[idx_best_model], c("naivebayes"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("C50"))) {
      x_train_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
      prediction_inbag <- predict(model_fit, newdata=x_train_inbag, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      x_train_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
      prediction_outbag <- predict(model_fit, newdata=x_train_outbag, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("ctree"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
      prediction_inbag <- do.call(rbind, prediction_inbag)
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
      prediction_outbag <- do.call(rbind, prediction_outbag)
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("wsrf"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
      prediction_inbag <- prediction_inbag$prob
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
      prediction_outbag <- prediction_outbag$prob
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("KNNreg"))) {
      x_train_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
      prediction_inbag <- predict(model_fit, newdata = x_train_inbag)

      x_train_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
      prediction_outbag <- predict(model_fit, newdata = x_train_outbag)
    } else if(is.element(model_names[idx_best_model], c("KNNclassification"))) {
      x_train_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
      prediction_inbag <- predict(model_fit, newdata = x_train_inbag)
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      x_train_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
      prediction_outbag <- predict(model_fit, newdata = x_train_outbag)
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("LASSO"))){
      x_train_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
      prediction_inbag <- c(predict(model_fit, newx=x_train_inbag, s = "lambda.min"))

      x_train_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
      prediction_outbag <- c(predict(model_fit, newx=x_train_outbag, s = "lambda.min"))
    } else if(is.element(model_names[idx_best_model], c("AdaBoost"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag)$prob
      prediction_inbag <- -1 + prediction_inbag[, 2]*2

      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag)$prob
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("C4.5"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, type = "probability")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, type = "probability")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("CART"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("MultilayerPerception"))){
      x_train_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
      prediction_inbag <- c(predict(model_fit, newdata = x_train_inbag))

      x_train_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
      prediction_outbag <- c(predict(model_fit, newdata = x_train_outbag))
    } else {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag)
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag)
    }
    
    prediction_train_inbag_AllBinaryClassifiers <- cbind(prediction_train_inbag_AllBinaryClassifiers, prediction_inbag)
    prediction_train_outbag_AllBinaryClassifiers <- cbind(prediction_train_outbag_AllBinaryClassifiers, prediction_outbag)
    
  }
  
  prediction_train_inbag_AllBinaryClassifiers <- as.data.frame(prediction_train_inbag_AllBinaryClassifiers)
  colnames(prediction_train_inbag_AllBinaryClassifiers) <- best_model_names
  
  prediction_train_outbag_AllBinaryClassifiers <- as.data.frame(prediction_train_outbag_AllBinaryClassifiers)
  colnames(prediction_train_outbag_AllBinaryClassifiers) <- best_model_names
}


prediction_train_inbag_AllBinaryClassifiers <- prediction_train_inbag_AllBinaryClassifiers[, sort.int(idx_combinations, index.return =T)$ix]
prediction_train_outbag_AllBinaryClassifiers <- prediction_train_outbag_AllBinaryClassifiers[, sort.int(idx_combinations, index.return =T)$ix]

save(model_storage_list, file=paste0("code/", pathname, "/model_storage_list_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
save(prediction_AllClass_inbag_list_bootstrap_combination, file=paste0("code/", pathname, "/prediction_AllClass_inbag_list_bootstrap_combination_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
save(prediction_AllClass_outbag_list_bootstrap_combination, file=paste0("code/", pathname, "/prediction_AllClass_outbag_list_bootstrap_combination_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
print(ls())
print(sort(sapply(ls(),function(x){object.size(get(x))})))
rm(list=c("auc_model_list", "auc_model_list_bootstrap", "prediction_AllClass_inbag_list_bootstrap_combination", "prediction_AllClass_outbag_list_bootstrap_combination"), envir = environment())
print(ls())
print(sort(sapply(ls(),function(x){object.size(get(x))})))

save(list=ls(), envir = environment(), file=paste0("code/", pathname, "/ATM_saved_test_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))


# save.image(file = "image_saved1.RData")

# load(file = "image_saved1.RData")


# write.xlsx(prediction_trainset_AllBinaryClassifiers, paste0("code/QuadraticProgramming/", pathname, "/prediction_trainset_AllBinaryClassifiers_rep_i_", rep_i, "_fold_i_", fold_i, "_.xlsx"), row.names=FALSE, col.names=TRUE)

# prediction_trainset_AllBinaryClassifiers <- scale(prediction_trainset_AllBinaryClassifiers, center = FALSE, scale=TRUE)

# write.xlsx(prediction_trainset_AllBinaryClassifiers, paste0("code/QuadraticProgramming/", pathname, "/prediction_trainset_AllBinaryClassifiers_scale_rep_i_", rep_i, "_fold_i_", fold_i, "_.xlsx"), row.names=FALSE, col.names=TRUE)


#### 遗传算法优化cost

source(paste0(path, '/code/1_function_OVO_code_matrix_generator.R'))
M <- OVO_matrix(K=K)

fitness_function <- function(x, prediction_matrix_inbag, N_row_inbag, label_real_inbag, num_combination, prediction_matrix_outbag, N_row_outbag, label_real_outbag){
  
  DELTA <- x[1]
  LAMBDA <- x[2]
  GAMMA <- x[-c(1,2)]
  
  cost_values <- LAMBDA*GAMMA[label_real_inbag]
  
  ## part 1: obtain the optimal threshold of each best performed binary classifier using Quadratic Programming
  
  # source(paste0(path, '/code/1_function_OVO_code_matrix_generator.R'))
  # M <- OVO_matrix(K=K)
  
  H <- diag(c(rep(1, ncol(M)), rep(0, N_row_inbag)))
  
  f <- c(rep(0, ncol(M)), cost_values)
  
  ## generate matrix A
  idx_box <- rep(1:K, N_row_inbag)
  idx_identity <- as.numeric(as.character(rep(label_real_inbag, rep(K, N_row_inbag))))
  
  idx_class_others <- idx_box[idx_box!=idx_identity]
  
  idx_class_identity <- as.numeric(as.character(rep(label_real_inbag, rep(K-1, N_row_inbag))))
  
  M_others <- M[idx_class_others, ]
  M_identity <- M[idx_class_identity, ]
  
  A_delta <- M_others - M_identity
  
  matrix_temp <- diag(x = -1, N_row_inbag)
  
  A_kexi <- matrix_temp[rep(1:N_row_inbag, rep(K-1, N_row_inbag)), ]
  
  A <- cbind(A_delta, A_kexi)
  
  ## generate vector B
  # delta_yi_r <- 5
  delta_yi_r <- DELTA
  
  prediction_matrix_rep <- prediction_matrix_inbag[rep(1:N_row_inbag, rep(K-1, N_row_inbag)), ]
  
  b <- apply(M_others * prediction_matrix_rep, 1, sum) -
    apply(M_identity * prediction_matrix_rep, 1, sum) -
    rep(delta_yi_r, nrow(A_delta))
  
  library(CVXR)
  
  w <- Variable(num_combination + N_row_inbag)
  
  objective <- Minimize(sum(quad_form(w, H) + t(f)%*%w))
  
  constraints_list <- list(A%*%w<=b, w>=c(rep(-Inf, num_combination), rep(0, N_row_inbag)))
  
  problem <- Problem(objective, constraints = constraints_list)
  
  result <- CVXR::psolve(problem)
  result$value
  adjust_kexi_values <- c(result$getValue(w))
  
  adjust_values <- adjust_kexi_values[1:num_combination]
  
  kexi_values <- adjust_kexi_values[num_combination+1:N_row_inbag]

  
  
  #########
  adjust_values_matrix_inbag <- t(replicate(nrow(prediction_matrix_inbag), adjust_values))
  
  prediction_matrix_inbag_final <- as.matrix(prediction_matrix_inbag) + adjust_values_matrix_inbag
  
  ## part 3: obtain the predicted class of each instance according to respect loss
  
  ### part 3.2: inbag
  class_prediction_inbag_temp <- c()
  Margin_inbag <- c()
  for (i in 1:N_row_inbag) {
    # cat(i, "  ", "\n")
    loss_i <- -M%*%as.numeric(prediction_matrix_inbag_final[i, ])

    label_real_i <- label_real_inbag[i]
    # cat(i, "\n")
    # cat(label_real_i, "\n")
    # cat(loss_i, "\n")
    # cat(loss_i[] - loss_i[label_real_i], "\n")
    Margin_inbag <- c(Margin_inbag, loss_i[] - loss_i[label_real_i])

    class_prediction_inbag_temp <- c(class_prediction_inbag_temp, c(1:K)[which.min(loss_i)])
  }

  Margin_inbag <- Margin_inbag[Margin_inbag!=0]
  summary(Margin_inbag)
  
  confusion_matrix <- table(factor(class_prediction_inbag_temp, levels = c(1:length(table(label_real_inbag)))), label_real_inbag)
  sum_row <- apply(confusion_matrix, 1, sum)
  sum_col <- apply(confusion_matrix, 2, sum)
  PRECISION <- diag(confusion_matrix)/sum_row
  PRECISION <- ifelse(is.na(PRECISION), 0, PRECISION)
  RECALL <- diag(confusion_matrix)/sum_col
  Fi <- 2*PRECISION*RECALL/(PRECISION+RECALL)
  Fi <- ifelse(is.na(Fi), 0, Fi)
  (F1_inbag <- mean(Fi))
  confusion_matrix_inbag <- confusion_matrix
  
  
  ## part 2: adjust the output of each binary classifier on outbag dataset using respective adjust_value 
  
  ### part 2.2: outbag
  adjust_values_matrix_outbag <- t(replicate(nrow(prediction_matrix_outbag), adjust_values))
  
  prediction_matrix_outbag_final <- as.matrix(prediction_matrix_outbag) + adjust_values_matrix_outbag
  
  ## part 3: obtain the predicted class of each instance according to respect loss
  
  ### part 3.2: outbag
  class_prediction_outbag_temp <- c()
  Margin_outbag <- c()
  for (i in 1:N_row_outbag) {
    # cat(i, "  ", "\n")
    loss_i <- -M%*%as.numeric(prediction_matrix_outbag_final[i, ])
    
    label_real_i <- label_real_outbag[i]
    # cat(i, "\n")
    # cat(label_real_i, "\n")
    # cat(loss_i, "\n")
    # cat(loss_i[] - loss_i[label_real_i], "\n")
    Margin_outbag <- c(Margin_outbag, loss_i[] - loss_i[label_real_i])
    
    class_prediction_outbag_temp <- c(class_prediction_outbag_temp, c(1:K)[which.min(loss_i)])
  }
  Margin_outbag <- Margin_outbag[Margin_outbag!=0]
  summary(Margin_outbag)
  
  ## part 4: calculate the prediction performance  (use the function defined myself)
  
  
  ### part 4.2: outbag
  # source(paste0(path, '/code/1_function_metrics_unbalance_classification.R'))
  # F1_outbag <- F1(label_real_outbag, class_prediction_outbag_temp)
  if(is.null(label_real_outbag)){
    F1_outbag <- NA
    confusion_matrix_outbag <- NA
  } else {
    confusion_matrix <- table(factor(class_prediction_outbag_temp, levels = c(1:length(table(label_real_outbag)))), label_real_outbag)
    sum_row <- apply(confusion_matrix, 1, sum)
    sum_col <- apply(confusion_matrix, 2, sum)
    PRECISION <- diag(confusion_matrix)/sum_row
    PRECISION <- ifelse(is.na(PRECISION), 0, PRECISION)
    RECALL <- diag(confusion_matrix)/sum_col
    Fi <- 2*PRECISION*RECALL/(PRECISION+RECALL)
    Fi <- ifelse(is.na(Fi), 0, Fi)
    (F1_outbag <- mean(Fi))
    confusion_matrix_outbag <- confusion_matrix
  }
  
  return(list(adjust_values=adjust_values, kexi_values=kexi_values,
              Margin_inbag=Margin_inbag, Margin_outbag=Margin_outbag,
              class_prediction_inbag=class_prediction_inbag_temp, class_prediction_outbag=class_prediction_outbag_temp,
              confusion_matrix_inbag=confusion_matrix_inbag, confusion_matrix_outbag=confusion_matrix_outbag,
              F1_inbag=F1_inbag, F1_outbag=F1_outbag))
  
}


# save(K, M, fitness_function, prediction_train_inbag_AllBinaryClassifiers, data_train_y_x_inbag, class_combinations, prediction_train_outbag_AllBinaryClassifiers,data_train_y_x_outbag,
#      file = "CVXR_test.RData")



print("test fitness_function start")

x = c(0, 1, max(table(data_train_y_x_inbag$Y))/table(data_train_y_x_inbag$Y))
# x = c(4, 1, 1.214286, 1.100000, 10)
# x = c(4, 1, 1.214286, 1.100000, 1.478261)
# x = c(4, 1, 1.184286, 1.100000, 1.478261)
#prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers
#N_row_inbag = nrow(data_train_y_x_inbag)
#label_real_inbag = data_train_y_x_inbag$Y
#num_combination = ncol(class_combinations)
#prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers
#N_row_outbag = nrow(data_train_y_x_outbag)
#label_real_outbag = data_train_y_x_outbag$Y

try(
aa <- fitness_function(x = x,
                       prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers,
                       N_row_inbag = nrow(data_train_y_x_inbag),
                       label_real_inbag = data_train_y_x_inbag$Y,
                       num_combination = ncol(class_combinations),
                       prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers,
                       N_row_outbag = nrow(data_train_y_x_outbag),
                       label_real_outbag = data_train_y_x_outbag$Y)

)

print("test fitness_function end")

if(plot_scatter){
  library(plotly)

  # prediction_plot_inbag <- prediction_matrix_inbag_final
  prediction_plot_inbag <- prediction_train_inbag_AllBinaryClassifiers
  prediction_plot_inbag <- as.data.frame(prediction_plot_inbag)
  colnames(prediction_plot_inbag) <- c("c1", "c2", "c3")
  prediction_plot_inbag$label <- data_train_y_x_inbag$Y
  prediction_plot_inbag$class <- 1
  aaa <- cbind(M, c(1, 2, 3), c(2, 2, 2))
  colnames(aaa) <- colnames(prediction_plot_inbag)
  prediction_plot_inbag <- rbind(aaa, prediction_plot_inbag)
  
  prediction_plot_inbag$label <- factor(prediction_plot_inbag$label)
  prediction_plot_inbag$class <- factor(prediction_plot_inbag$class)
  
  fig <- plot_ly(prediction_plot_inbag, x=~c1, y=~c2, z=~c3, color = ~label, symbol = ~class, symbols = c('circle', 'x'))
  fig <- fig %>% add_markers()
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'classifier_1'),
                                     yaxis = list(title = 'classifier_2'),
                                     zaxis = list(title = 'classifier_3')))
  # fig
  
  htmlwidgets::saveWidget(as_widget(fig), file=paste0(path, "/results/", pathname, "/3D_ScatterPlot_rep_i_", rep_i, "_fold_i_", fold_i, "_.html"))
}




source(paste0(path, "/code/GA_functions/GA_0_main.R"))
source(paste0(path, "/code/GA_functions/GA_1_generate_population_functions.R"))
source(paste0(path, "/code/GA_functions/GA_2_selection_functions.R"))
source(paste0(path, "/code/GA_functions/GA_3_crossover_functions.R"))
source(paste0(path, "/code/GA_functions/GA_4_mutation_functions.R"))
source(paste0(path, "/code/GA_functions/GA_5_parallel.R"))
source(paste0(path, "/code/GA_functions/GA_6_gaControl.R"))
source(paste0(path, "/code/GA_functions/GA_7_miscfun.R"))


print('GA search optimal cost Start \n')
tictoc::tic()

library(parallel)
library(doParallel)

cores <- detectCores()
num_cores = floor(0.4*cores)
num_cores = 10

DELTA_initial <- c(3)
DELTA_lower <- c(0)
DELTA_upper <- c(10)

GAMMA_initial <- c(max(table(data_train_y_x_inbag$Y))/table(data_train_y_x_inbag$Y))
GAMMA_lower <- 0.5*GAMMA_initial
GAMMA_upper <- 3*GAMMA_initial

LAMBDA_initial <- c(1)
LAMBDA_lower <- c(1)
LAMBDA_upper <- c(5)

x_initial <- c(DELTA_initial, LAMBDA_initial, GAMMA_initial)
x_lower <- c(DELTA_lower, LAMBDA_lower, GAMMA_lower)
x_upper <- c(DELTA_upper, LAMBDA_upper, GAMMA_upper)

num_combination <- ncol(class_combinations)

GA_results <- ga(type="real-valued", fitness=fitness_function,
                 prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers,
                 N_row_inbag = nrow(data_train_y_x_inbag),
                 label_real_inbag = data_train_y_x_inbag$Y,
                 num_combination = num_combination,
                 prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers,
                 N_row_outbag = nrow(data_train_y_x_outbag),
                 label_real_outbag = data_train_y_x_outbag$Y,
                 lower = x_lower, upper = x_upper,
                 population=gareal_Population_R, selection=ga_lrSelection_R, crossover=gareal_waCrossover_R, mutation=gareal_rsMutation_R,
                 popSize=200, pcrossover = 0.8, pmutation=0.1,
                 keepBest = TRUE, elitism=floor(0.5*200),
                 suggestions = x_initial,
                 maxiter=10, run=10, maxFitness=1, parallel = num_cores)


save(GA_results, file=paste0("code/", pathname, "/GA_results_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_", "_.RData"))


# stopCluster(cl_for_ga)


# type="real-valued"
# fitness=fitness_function
# prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers
# N_row_inbag = nrow(data_train_y_x_inbag)
# label_real_inbag = data_train_y_x_inbag$Y
# num_combination = ncol(class_combinations)
# prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers
# N_row_outbag = nrow(data_train_y_x_outbag)
# label_real_outbag = data_train_y_x_outbag$Y
# lower = x_lower
# upper = x_upper
# population=gareal_Population_R
# selection=ga_lrSelection_R
# crossover=gareal_waCrossover_R
# mutation=gareal_rsMutation_R
# popSize=200
# pcrossover = 0.8
# pmutation=0.1
# keepBest = TRUE
# elitism=1
# suggestions = x_initial
# maxiter=2
# run=2
# maxFitness=0.95
# parallel = num_cores


# updatePop = FALSE
# postFitness = NULL
# names = NULL
# optim = FALSE
# optimArgs = list(method = "L-BFGS-B",
#                  poptim = 0.05,
#                  pressel = 0.5,
#                  control = list(fnscale = -1, maxit = 100))
# monitor = if(interactive()) gaMonitor else FALSE
# seed = NULL




# callArgs <- ga(type="real-valued", fitness=fitness_function,
#                  prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers,
#                  N_row_inbag = nrow(data_train_y_x_inbag),
#                  label_real_inbag = data_train_y_x_inbag$Y,
#                  num_combination = num_combination,
#                  prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers,
#                  N_row_outbag = nrow(data_train_y_x_outbag),
#                  label_real_outbag = data_train_y_x_outbag$Y,
#                  lower = x_lower, upper = x_upper,
#                  population=gareal_Population_R, selection=ga_lrSelection_R, crossover=gareal_waCrossover_R, mutation=gareal_rsMutation_R,
#                  popSize=5, pcrossover = 0.8, pmutation=0.1,
#                  keepBest = TRUE, elitism=1,
#                  suggestions = x_initial,
#                  maxiter=2, run=2, maxFitness=0.95, parallel = 2, functionTest = TRUE)

# aaa <- ga(type="real-valued", fitness=fitness_function,
#                prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers,
#                N_row_inbag = nrow(data_train_y_x_inbag),
#                label_real_inbag = data_train_y_x_inbag$Y,
#                num_combination = num_combination,
#                prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers,
#                N_row_outbag = nrow(data_train_y_x_outbag),
#                label_real_outbag = data_train_y_x_outbag$Y,
#                lower = cost_lower, upper = cost_upper,
#                population=gareal_Population_R, selection=ga_lrSelection_R, crossover=gareal_waCrossover_R, mutation=gareal_rsMutation_R,
#                popSize=5, pcrossover = 0.8, pmutation=0.1,
#                keepBest = TRUE, elitism=1,
#                suggestions = cost_initial,
#                maxiter=2, run=2, maxFitness=0.95, parallel = 2, functionTest = F)


print('GA search optimal cost Done \n')
tictoc::toc()


# if(nrow(GA_results@solution)>1){
#   solution_id <- sample(1:nrow(GA_results@solution), 1)
#   x_values_optimal <- GA_results@solution[solution_id, ]
# } else {
#   x_values_optimal <- GA_results@solution
# }



##### obtain the prediction values of train dataset and test dataset
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")

  idx_best_parameter_model_combination <- idx_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  
  prediction_trainset_AllBinaryClassifiers <- c()
  prediction_testset_AllBinaryClassifiers <- c()
  best_model_names <- c()
  for (combination_i in seq_len(length(idx_combinations))) {
    cat("combination_i ", combination_i, "\n")
    
    idx_best_combination <- idx_combinations[combination_i]
    idx_best_model <- idx_models[combination_i]
    idx_best_parameter <- idx_best_parameter_model_combination[idx_best_model, idx_best_combination]
    
    best_model_names <- c(best_model_names, paste0("combination_", idx_best_combination, "_", model_names[idx_best_model]))
    
    # model_fit <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$model[[idx_best_model]][[idx_best_parameter]]
    
    model_fit <- model_storage_list[[bootstrap_i]][[idx_best_combination]]

    if(is.element(model_names[idx_best_model], c("RandomForest"))){
      prediction_inbag <- predict(model_fit, data=data_train_y_x, probability = TRUE)$predictions
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      prediction_outbag <- predict(model_fit, data=NEW_X, probability = TRUE)$predictions
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("SVM"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x, probability = TRUE)
      prediction_inbag <- attr(prediction_inbag, "probabilities")
      prediction_inbag <- -1 + prediction_inbag[, 1]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X, probability = TRUE)
      prediction_outbag <- attr(prediction_outbag, "probabilities")
      prediction_outbag <- -1 + prediction_outbag[, 1]*2
    } else if(is.element(model_names[idx_best_model], c("naivebayes"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("C50"))) {
      x_train_inbag <- as.matrix(data_train_y_x[, names_x])
      prediction_inbag <- predict(model_fit, newdata=x_train_inbag, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      x_train_outbag <- as.matrix(NEW_X[, names_x])
      prediction_outbag <- predict(model_fit, newdata=x_train_outbag, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("ctree"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x, type = "prob")
      prediction_inbag <- do.call(rbind, prediction_inbag)
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X, type = "prob")
      prediction_outbag <- do.call(rbind, prediction_outbag)
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("wsrf"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x, type = "prob")
      prediction_inbag <- prediction_inbag$prob
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X, type = "prob")
      prediction_outbag <- prediction_outbag$prob
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("KNNreg"))) {
      x_train_inbag <- as.matrix(data_train_y_x[, names_x])
      prediction_inbag <- predict(model_fit, newdata = x_train_inbag)
      
      x_train_outbag <- as.matrix(NEW_X[, names_x])
      prediction_outbag <- predict(model_fit, newdata = x_train_outbag)
    } else if(is.element(model_names[idx_best_model], c("KNNclassification"))) {
      x_train_inbag <- as.matrix(data_train_y_x[, names_x])
      prediction_inbag <- predict(model_fit, newdata = x_train_inbag)
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      x_train_outbag <- as.matrix(NEW_X[, names_x])
      prediction_outbag <- predict(model_fit, newdata = x_train_outbag)
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("LASSO"))){
      x_train_inbag <- as.matrix(data_train_y_x[, names_x])
      prediction_inbag <- c(predict(model_fit, newx=x_train_inbag, s = "lambda.min"))
      
      x_train_outbag <- as.matrix(NEW_X[, names_x])
      prediction_outbag <- c(predict(model_fit, newx=x_train_outbag, s = "lambda.min"))
    } else if(is.element(model_names[idx_best_model], c("AdaBoost"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x)$prob
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X)$prob
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("C4.5"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x, type = "probability")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X, type = "probability")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("CART"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } else if(is.element(model_names[idx_best_model], c("MultilayerPerception"))){
      x_train_inbag <- as.matrix(data_train_y_x[, names_x])
      prediction_inbag <- c(predict(model_fit, newdata = x_train_inbag))
      
      x_train_outbag <- as.matrix(NEW_X[, names_x])
      prediction_outbag <- c(predict(model_fit, newdata = x_train_outbag))
    } else {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x)
      prediction_outbag <- predict(model_fit, newdata=NEW_X)
    }

    prediction_trainset_AllBinaryClassifiers <- cbind(prediction_trainset_AllBinaryClassifiers, prediction_inbag)
    prediction_testset_AllBinaryClassifiers <- cbind(prediction_testset_AllBinaryClassifiers, prediction_outbag)

  }

  prediction_trainset_AllBinaryClassifiers <- as.data.frame(prediction_trainset_AllBinaryClassifiers)
  colnames(prediction_trainset_AllBinaryClassifiers) <- best_model_names

  prediction_testset_AllBinaryClassifiers <- as.data.frame(prediction_testset_AllBinaryClassifiers)
  colnames(prediction_testset_AllBinaryClassifiers) <- best_model_names
}

prediction_trainset_AllBinaryClassifiers <- prediction_trainset_AllBinaryClassifiers[, sort.int(idx_combinations, index.return =T)$ix]
prediction_testset_AllBinaryClassifiers <- prediction_testset_AllBinaryClassifiers[, sort.int(idx_combinations, index.return =T)$ix]

for(s_i in order(GA_results@fitness, decreasing = TRUE)){
  if(inherits(try(
    results_final <- fitness_function(x = GA_results@population[s_i,],
                                      prediction_matrix_inbag = prediction_trainset_AllBinaryClassifiers,
                                      N_row_inbag = nrow(prediction_trainset_AllBinaryClassifiers),
                                      label_real_inbag = data_train_y_x$Y,
                                      num_combination = ncol(class_combinations),
                                      prediction_matrix_outbag = prediction_testset_AllBinaryClassifiers,
                                      N_row_outbag = nrow(prediction_testset_AllBinaryClassifiers),
                                      label_real_outbag = NULL)
  ), "try-error")){
    next
  } else {
    results_final <- fitness_function(x = GA_results@population[s_i,],
                                      prediction_matrix_inbag = prediction_trainset_AllBinaryClassifiers,
                                      N_row_inbag = nrow(prediction_trainset_AllBinaryClassifiers),
                                      label_real_inbag = data_train_y_x$Y,
                                      num_combination = ncol(class_combinations),
                                      prediction_matrix_outbag = prediction_testset_AllBinaryClassifiers,
                                      N_row_outbag = nrow(prediction_testset_AllBinaryClassifiers),
                                      label_real_outbag = NULL)
    break
  }
}

save(list=ls(), envir = environment(), file=paste0("code/", pathname, "/ATM_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))

print("ATM done")

return(list(class_prediction_train=results_final$class_prediction_inbag, class_prediction_test=results_final$class_prediction_outbag,
            prediction_trainset_AllBinaryClassifiers=prediction_trainset_AllBinaryClassifiers, prediction_testset_AllBinaryClassifiers=prediction_testset_AllBinaryClassifiers))
  
}







