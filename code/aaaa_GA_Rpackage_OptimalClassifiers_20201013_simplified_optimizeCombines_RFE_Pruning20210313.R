
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

Adaptive_threshold_moving <- function(X, Y, NEW_X, num_bootstrap=10, pathname = "directory_temp", fold_i=fold_i, rep_i=rep_i, 
                                      rfe_select=FALSE, select_backward = FALSE, select_backward_wilcoxon = TRUE, 
                                      names_x_fixed=NULL, subsets_rfe=c(10, 20, 40, 60, 80, 160, 240, 320, 400), 
                                      parallel_combinations=FALSE, parallel_AdaBoost=FALSE, sub_features=TRUE, ratio_sub_features=0.8, fit_model_binary=T, fit_model_OVA=F, rescale_prediction=T,
                                      plot_scatter=FALSE, plot_boxplot_prediction_InbagAndOutbag=TRUE, plot_boxplot_prediction_TrainAndTest=TRUE){


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
  
  names_x_NoFixed <- names_x[!is.element(names_x, names_x_fixed)]
  
  data_train_y_x <- cbind(Y, X)
  data_train_y_x <- as.data.frame(data_train_y_x)
  colnames(data_train_y_x) <- c("Y", names_x)
  
  # model_names <- c("RandomForest", "SVM", "naivebayes", "C50", "ctree", "wsrf", "KNNreg", "KNNclassification", "LASSO", "AdaBoost", "C4.5", "CART", "MultilayerPerception")
  
  # model_names <- c("RandomForest", "SVM", "naivebayes", "ctree", "wsrf", "LASSO", "AdaBoost", "CART")
  # model_names <- c("RandomForest", "SVM", "LASSO", "AdaBoost")
  model_names <- c("RandomForest", "SVM")
  # model_names <- c("LASSO")
  
  num_sub_features=floor(ratio_sub_features*(length(names_x_NoFixed)))
  
  # num_bootstrap <- 10
  options(warn = 0)
  tictoc::tic()
  print('fit binary classifiers Start \n')  
  library(foreach)
  library(doParallel)
  cores <- detectCores()
  num_cores = floor(0.2*cores)
  
  if(parallel_combinations){
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    
    `%DO_1%` <- `%do%`
    `%DO_2%` <- `%dopar%`
  } else {
    `%DO_1%` <- `%do%`
    `%DO_2%` <- `%do%`
  }

  auc_model_list <- list()     ## Stack1 Way1: normal for loop

  # auc_model_list <- foreach (bootstrap_i = 1:num_bootstrap) %dopar% {       ## Stack1 Way2: dopar loop
  seed_rfe <- 123
  # subsets_rfe <- seq(1, length(names_x), 200)
#  subsets_rfe <- c(10, 20, 30, seq(40, 200, 40))
#  subsets_rfe <- subsets_rfe * 2
  subsets_backward <- subsets_rfe
  subsets_backward <- subsets_backward[subsets_backward<=num_sub_features]
  idx_inbag <- list()
  idx_outbag <- list()
  names_x_NoFixed_sub_bootstrap <- list()
  bootstrap_i <- 1
  combination_i <- 1
  for(bootstrap_i in 1:num_bootstrap){    ## Stack1 Way1: normal for loop
    cat("bootstrap_i: ", bootstrap_i, "\n")

    idx_inbag_temp <- c()
    for (K_i in 1:K) {
      idx_K_i <- c(1:length(Y))[is.element(Y, K_i)]

      set.seed(bootstrap_i)
      # idx_inbag_K_i <- sample(idx_K_i, floor(0.6*length(idx_K_i)),  replace = FALSE)
      idx_inbag_K_i <- sample(idx_K_i, floor(1*length(idx_K_i)),  replace = TRUE)

      idx_inbag_temp <- c(idx_inbag_temp, idx_inbag_K_i)
    }
    idx_outbag_temp <- c(1:length(Y))[!is.element(c(1:length(Y)), idx_inbag_temp)]
    
    if(sub_features){
      set.seed(bootstrap_i)
      names_x_NoFixed_sub_temp <- names_x_NoFixed[sample(1:length(names_x_NoFixed), num_sub_features, replace = FALSE)]
    } else {
      names_x_NoFixed_sub_temp <- names_x_NoFixed
    }
    

    data_train_y_x_inbag <- data_train_y_x[idx_inbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]
    data_train_y_x_outbag <- data_train_y_x[idx_outbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]
    
    idx_inbag[[bootstrap_i]] <- idx_inbag_temp
    idx_outbag[[bootstrap_i]] <- idx_outbag_temp
    
    names_x_NoFixed_sub_bootstrap[[bootstrap_i]] <- names_x_NoFixed_sub_temp

    save(data_train_y_x_inbag, file=paste0("code/", pathname, "/data_train_y_x_inbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
    save(data_train_y_x_outbag, file=paste0("code/", pathname, "/data_train_y_x_outbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))

    # auc_model_list_bootstrap <- list()     ### Stack2 Way1: normal for loop
    # for(combination_i in c(1:ncol(class_combinations))){     ### Stack2 Way1: normal for loop

    ##!!!!!!!!!!!!!!!!!!!!!!!
    auc_model_list_bootstrap <- foreach(combination_i = c(1:ncol(class_combinations))) %DO_1% {       ### Stack2 Way2: dopar loop

      cat("================combination_i ", combination_i, "==============\n")
      
      auc_calculator <- function(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp){
        class_binary <- c(class_1, class_2)
        class_others <- unique(label_All_classes)[!is.element(unique(label_All_classes), class_binary)]
        class_others <- sort(as.numeric(as.character(class_others)))
        
        roc_list <- list()
        
        idx_temp <- is.element(label_All_classes, c(class_1, class_2))
        Y_temp <- label_All_classes[idx_temp]
        Y_temp <- as.numeric(as.character(Y_temp))
        Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        Y_temp <- factor(Y_temp)
        roc_binary <- pROC::roc(Y_temp, prediction_All_classes[idx_temp], direction="<", quiet=T)
        name_component_temp <- paste0("roc_", class_1, "_", class_2)
        roc_list[[name_component_temp]] <- roc_binary
        auc_binary <- roc_binary$auc
        
        auc_others_1 <- c()
        for (class_others_i in class_others) {
          idx_temp <- is.element(label_All_classes, c(class_1, class_others_i))
          Y_temp <- label_All_classes[idx_temp]
          Y_temp <- as.numeric(as.character(Y_temp))
          Y_temp <- ifelse(Y_temp==class_1, 1, -1)
          Y_temp <- factor(Y_temp)
          name_component_temp <- paste0("roc_", class_1, "_", class_others_i)
          roc_list[[name_component_temp]] <- pROC::roc(Y_temp, prediction_All_classes[idx_temp], direction="<", quiet=T)
          auc_others_1 <- c(auc_others_1, pROC::roc(Y_temp, prediction_All_classes[idx_temp], direction="<", quiet=T)$auc)
        }
        
        auc_others_2 <- c()
        for (class_others_i in class_others) {
          idx_temp <- is.element(label_All_classes, c(class_2, class_others_i))
          Y_temp <- label_All_classes[idx_temp]
          Y_temp <- as.numeric(as.character(Y_temp))
          Y_temp <- ifelse(Y_temp==class_2, -1, 1)
          Y_temp <- factor(Y_temp)
          name_component_temp <- paste0("roc_", class_2, "_", class_others_i)
          roc_list[[name_component_temp]] <- pROC::roc(Y_temp, prediction_All_classes[idx_temp], direction="<", quiet=T)
          auc_others_2 <- c(auc_others_2, pROC::roc(Y_temp, prediction_All_classes[idx_temp], direction="<", quiet=T)$auc)
        }
        
        
        list(auc_binary=auc_binary, auc_others_1=auc_others_1, auc_others_2=auc_others_2, roc_list=roc_list)
        
      }
      

      ## extract binary dataset
      class_1 <- class_combinations[1, combination_i]
      class_2 <- class_combinations[2, combination_i]
      
      class_others <- unique(data_train_y_x_inbag$Y)[!is.element(unique(data_train_y_x_inbag$Y), c(class_1, class_2))]
      class_others <- sort(as.numeric(as.character(class_others)))

      ###### inbag
      ### binary
      data_train_y_x_inbag_class1 <- data_train_y_x_inbag[data_train_y_x_inbag$Y==class_1, ]
      data_train_y_x_inbag_class2 <- data_train_y_x_inbag[data_train_y_x_inbag$Y==class_2, ]
      
      if(fit_model_binary){
        data_train_y_x_inbag_binary <- rbind(data_train_y_x_inbag_class1, data_train_y_x_inbag_class2)
        data_train_y_x_inbag_binary$Y_binary_numerical <- ifelse(data_train_y_x_inbag_binary$Y==class_1, 1, -1)
        data_train_y_x_inbag_binary$Y_binary_factor <- as.factor(data_train_y_x_inbag_binary$Y_binary_numerical)
      }

      if(fit_model_OVA){
        ### OVA1
        data_train_y_x_inbag_OVA1 <- data_train_y_x_inbag
        data_train_y_x_inbag_OVA1$Y_binary_numerical <- ifelse(data_train_y_x_inbag_OVA1$Y==class_1, 1, -1)
        data_train_y_x_inbag_OVA1$Y_binary_factor <- as.factor(data_train_y_x_inbag_OVA1$Y_binary_numerical)
        
        ### OVA2
        data_train_y_x_inbag_OVA2 <- data_train_y_x_inbag
        data_train_y_x_inbag_OVA2$Y_binary_numerical <- ifelse(data_train_y_x_inbag_OVA2$Y==class_2, -1, 1)
        data_train_y_x_inbag_OVA2$Y_binary_factor <- as.factor(data_train_y_x_inbag_OVA2$Y_binary_numerical)
      }

      ####### outbag
      data_train_y_x_outbag_class1 <- data_train_y_x_outbag[data_train_y_x_outbag$Y==class_1, ]
      data_train_y_x_outbag_class2 <- data_train_y_x_outbag[data_train_y_x_outbag$Y==class_2, ]
      
      if(fit_model_binary){
        data_train_y_x_outbag_binary <- rbind(data_train_y_x_outbag_class1, data_train_y_x_outbag_class2)
        data_train_y_x_outbag_binary$Y_binary_numerical <- ifelse(data_train_y_x_outbag_binary$Y==class_1, 1, -1)
        data_train_y_x_outbag_binary$Y_binary_factor <- as.factor(data_train_y_x_outbag_binary$Y_binary_numerical)
      }
      
      if(fit_model_OVA){
        data_train_y_x_outbag_OVA1 <- data_train_y_x_outbag
        data_train_y_x_outbag_OVA1$Y_binary_numerical <- ifelse(data_train_y_x_outbag_OVA1$Y==class_1, 1, -1)
        data_train_y_x_outbag_OVA1$Y_binary_factor <- as.factor(data_train_y_x_outbag_OVA1$Y_binary_numerical)
        
        data_train_y_x_outbag_OVA2 <- data_train_y_x_outbag
        data_train_y_x_outbag_OVA2$Y_binary_numerical <- ifelse(data_train_y_x_outbag_OVA2$Y==class_2, -1, 1)
        data_train_y_x_outbag_OVA2$Y_binary_factor <- as.factor(data_train_y_x_outbag_OVA2$Y_binary_numerical)
      }
      
      
      if(fit_model_binary){
        print("wilcoxon binary start")
        results_Wilcoxon <- c()
        for (var_i in names_x_NoFixed_sub_temp) {
          # cat(var_i, "\n")
          Wilcoxon_model <- wilcox.test(as.formula(paste(var_i, "~", name_y_factor)), data = data_train_y_x_inbag_binary,
                                        alternative = c("two.sided"),
                                        mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
                                        conf.int = FALSE, conf.level = 0.95)
          aa <- c(Wilcoxon_model$statistic, Wilcoxon_model$p.value)
          results_Wilcoxon <- rbind(results_Wilcoxon, aa)
        }
        colnames(results_Wilcoxon) <- c("statistics", "p")
        results_Wilcoxon <- cbind(names_x_NoFixed_sub_temp, results_Wilcoxon)
        results_Wilcoxon <- as.data.frame(results_Wilcoxon)
        results_Wilcoxon <- results_Wilcoxon[order(as.numeric(as.character(results_Wilcoxon$p)), decreasing = FALSE), ]
        print("wilcoxon binary end")
      }

      if(fit_model_OVA){
        print("wilcoxon OVA1 start")
        # results_Wilcoxon_OVA1 <- c()
        results_Wilcoxon_OVA1 <- foreach (var_i = names_x_NoFixed_sub_temp, .combine = "rbind") %DO_2% {
          # cat(var_i, "\n")
          Wilcoxon_model <- wilcox.test(as.formula(paste(var_i, "~", name_y_factor)), data = data_train_y_x_inbag_OVA1,
                                        alternative = c("two.sided"),
                                        mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
                                        conf.int = FALSE, conf.level = 0.95)
          aa <- c(Wilcoxon_model$statistic, Wilcoxon_model$p.value)
          aa
          # results_Wilcoxon_OVA1 <- rbind(results_Wilcoxon_OVA1, aa)
        }
        colnames(results_Wilcoxon_OVA1) <- c("statistics", "p")
        results_Wilcoxon_OVA1 <- cbind(names_x_NoFixed_sub_temp, results_Wilcoxon_OVA1)
        results_Wilcoxon_OVA1 <- as.data.frame(results_Wilcoxon_OVA1)
        results_Wilcoxon_OVA1 <- results_Wilcoxon_OVA1[order(as.numeric(as.character(results_Wilcoxon_OVA1$p)), decreasing = FALSE), ]
        # fdr_results <- fdrtool::fdrtool(as.numeric(as.character(results_Wilcoxon_OVA1$p)), statistic = "pvalue", plot=F)
        # results_Wilcoxon_OVA1$qval <- fdr_results$qval
        # results_Wilcoxon_OVA1$lfdr <- fdr_results$lfdr
        print("wilcoxon OVA1 end")
        
        print("wilcoxon OVA2 start")
        # results_Wilcoxon_OVA2 <- c()
        results_Wilcoxon_OVA2 <- foreach(var_i = names_x_NoFixed_sub_temp, .combine = "rbind") %DO_2% {
          # cat(var_i, "\n")
          Wilcoxon_model <- wilcox.test(as.formula(paste(var_i, "~", name_y_factor)), data = data_train_y_x_inbag_OVA2,
                                        alternative = c("two.sided"),
                                        mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
                                        conf.int = FALSE, conf.level = 0.95)
          aa <- c(Wilcoxon_model$statistic, Wilcoxon_model$p.value)
          aa
          # results_Wilcoxon_OVA2 <- rbind(results_Wilcoxon_OVA2, aa)
        }
        colnames(results_Wilcoxon_OVA2) <- c("statistics", "p")
        results_Wilcoxon_OVA2 <- cbind(names_x_NoFixed_sub_temp, results_Wilcoxon_OVA2)
        results_Wilcoxon_OVA2 <- as.data.frame(results_Wilcoxon_OVA2)
        results_Wilcoxon_OVA2 <- results_Wilcoxon_OVA2[order(as.numeric(as.character(results_Wilcoxon_OVA2$p)), decreasing = FALSE), ]
        # fdr_results <- fdrtool::fdrtool(as.numeric(as.character(results_Wilcoxon_OVA2$p)), statistic = "pvalue", plot=F)
        # results_Wilcoxon_OVA2$qval <- fdr_results$qval
        # results_Wilcoxon_OVA2$lfdr <- fdr_results$lfdr
        print("wilcoxon OVA2 end")
      }
      
      
      auc_binary_list_bootstrap_combination <- list()     ### auc_binary_list_combination_bootstrap
      names_x_select_list_bootstrap_combination <- list() 
      threshold_list_bootstrap_combination <- list()
      prediction_temp_temp_list_bootstrap_combination <- list()
      model_list_bootstrap_combination <- list()           ### model_list_combination_boostrap
      prediction_AllClass_inbag_list_bootstrap_combination <- list()
      prediction_AllClass_outbag_list_bootstrap_combination <- list()
      for (model_i in model_names) {
        cat("model_i ", model_i, "\n")
        if(model_i=="RandomForest"){

          # num.trees_c <- seq(100, 1000, 200)
          # min.node.size_c <- c(5, 10, 15)
          # max.depth_c <- c(5, 8, 10)
          
          num.trees_c <- c(500, 1000, 2000)
          min.node.size_c <- c(15, 100, 200)
          min.node.size_c <- seq(10, nrow(data_train_y_x_inbag), 20)
          max.depth_c <- c(3, 5, 10)

          parameters_grid <- expand.grid(num.trees_c=num.trees_c, min.node.size_c=min.node.size_c, max.depth_c=max.depth_c)

          names_x_select_list_temp <- list()
          threshold_list_temp <- list()
          prediction_temp_temp_list <- list()
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          auc_temp_inbag <- c()
          auc_temp_outbag <- c()
          for (row_i in 1:nrow(parameters_grid)) {
            
            cat(row_i, "\n")
          
            if(rfe_select){
              rfRFE <-  list(summary = caret::defaultSummary,
                             fit = function(x, y, first=TRUE, last=FALSE){
                               ranger::ranger(x = x, y = y,
                                              num.trees =parameters_grid$num.trees_c[row_i],
                                              min.node.size = parameters_grid$min.node.size_c[row_i],
                                              max.depth = parameters_grid$max.depth_c[row_i],
                                              importance = "impurity", probability = TRUE)
                             },
                             pred = function(object, x) {
                               prediction_temp <- predict(object, data = x)$predictions
                               prediction_temp <- apply(prediction_temp, 1, which.max)
                               prediction_temp
                             },
                             rank = function(object, x, y) {
                               vimp <- ranger::importance(object)
                               # vimp <- ranger::importance(model_fit)
                               vimp <- vimp[order(vimp, decreasing = TRUE),drop = FALSE]
                               vimp <- as.data.frame(vimp)
                               vimp$var <- rownames(vimp)
                               colnames(vimp) <- c("Overall", "var")
                               vimp
                             },
                             selectSize = caret::pickSizeBest,
                             selectVar = caret::pickVars)
              
              ctrl <- caret::rfeControl(functions = rfRFE)
              #ctrl$returnResamp <- "all"
              set.seed(seed_rfe)
              rfProfile <- caret::rfe(x_rfe, y_rfe, sizes = subsets_rfe, rfeControl = ctrl)
              names_x_select <- rfProfile$optVariables
            } else if(select_backward) {
              modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
              model_fit <- ranger::ranger(formula = modelformula, data = data_train_y_x_inbag_binary,
                                          num.trees =parameters_grid$num.trees_c[row_i],
                                          min.node.size = parameters_grid$min.node.size_c[row_i],
                                          max.depth = parameters_grid$max.depth_c[row_i],
                                          importance = "impurity", probability = TRUE)
              
              vimp <- ranger::importance(model_fit)
              vimp <- vimp[order(vimp, decreasing = TRUE),drop = FALSE]
              vimp <- as.data.frame(vimp)
              vimp$var <- rownames(vimp)
              colnames(vimp) <- c("Overall", "var")
              
              auc_backward <- c()
              for (subset_i in seq_len(length(subsets_backward))) {
                # cat(subset_i, "\n")
                names_x_select_temp <- vimp$var[1:subsets_backward[subset_i]]
                names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
                modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
                model_fit_temp <- ranger::ranger(formula = modelformula, data = data_train_y_x_inbag_binary,
                                            num.trees =parameters_grid$num.trees_c[row_i],
                                            min.node.size = parameters_grid$min.node.size_c[row_i],
                                            max.depth = parameters_grid$max.depth_c[row_i],
                                            importance = "impurity", probability = TRUE)
                
                prediction_temp_temp <- predict(model_fit_temp, data = data_train_y_x_outbag_binary)$predictions
                prediction_temp_temp <- -1 + prediction_temp_temp[, 2]*2
                auc_backward[subset_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp_temp)$auc
              }
              names_x_select <- vimp$var[1:subsets_backward[which.max(auc_backward)]]
              
            } else if(select_backward_wilcoxon) {
              
              ###### binary
              
              if(fit_model_binary){
                
                auc_backward <- c()
                for (subset_i in seq_len(length(subsets_backward))) {
                  names_x_select_temp <- as.character(results_Wilcoxon$names_x_NoFixed_sub_temp[1:subsets_backward[subset_i]])
                  names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
                  modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
                  model_fit_temp <- ranger::ranger(formula = modelformula, data = data_train_y_x_inbag_binary,
                                                   num.trees =parameters_grid$num.trees_c[row_i],
                                                   min.node.size = parameters_grid$min.node.size_c[row_i],
                                                   max.depth = parameters_grid$max.depth_c[row_i],
                                                   importance = "impurity", probability = TRUE, regularization.usedepth=TRUE)
                  
                  prediction_temp_temp <- predict(model_fit_temp, data = data_train_y_x_outbag)$predictions
                  prediction_temp_temp <- prediction_temp_temp[, "1"]
                  if(rescale_prediction&(max(prediction_temp_temp)!=min(prediction_temp_temp))){
                    ratio_temp_temp <- 1/(max(prediction_temp_temp) - min(prediction_temp_temp))
                    prediction_temp_temp <- (prediction_temp_temp - min(prediction_temp_temp)) * ratio_temp_temp
                    prediction_temp_temp <- -1 + prediction_temp_temp*2
                  }
                  
                  if(inherits(try(auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)), "try-error")){
                    auc_backward <- c(auc_backward, 0.5)
                  } else {
                    auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)
                    auc_backward <- c(auc_backward, auc_temp$auc_binary) 
                  }
                }
                
                names_x_select <- results_Wilcoxon$names_x_NoFixed_sub_temp[1:subsets_backward[which.max(auc_backward)]]
                names_x_select <- c(as.character(names_x_select), names_x_fixed)
                
                ###
                names_x_select_list_temp[[row_i]] <- list(names_x_select=names_x_select)
                
                ###
                prediction_temp_temp_list[[row_i]] <- list()
                
                ###
                modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select, collapse = "+")))
                model_fit <- ranger::ranger(formula = modelformula, data = data_train_y_x_inbag_binary,
                                            num.trees =parameters_grid$num.trees_c[row_i],
                                            min.node.size = parameters_grid$min.node.size_c[row_i],
                                            max.depth = parameters_grid$max.depth_c[row_i],
                                            importance = "impurity", probability = TRUE, regularization.usedepth=TRUE)
                
                ###
                model_fit_list_temp[[row_i]] <- list(model_fit=model_fit)
                
                ###
                # all classes inbag
                ### binary
                prediction_AllClass_inbag_temp <- predict(model_fit, data = data_train_y_x_inbag)$predictions
                prediction_AllClass_inbag_temp <- prediction_AllClass_inbag_temp[, "1"]
                if(rescale_prediction&(max(prediction_AllClass_inbag_temp)!=min(prediction_AllClass_inbag_temp))){
                  ratio_inbag_temp <- 1/(max(prediction_AllClass_inbag_temp) - min(prediction_AllClass_inbag_temp))
                  prediction_AllClass_inbag_temp <- (prediction_AllClass_inbag_temp - min(prediction_AllClass_inbag_temp)) * ratio_inbag_temp
                  prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp*2
                }

                auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp)
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                
                # all classes outbag
                ### binary
                prediction_AllClass_outbag_temp <- predict(model_fit, data = data_train_y_x_outbag)$predictions
                prediction_AllClass_outbag_temp <- prediction_AllClass_outbag_temp[, "1"]
                if(rescale_prediction&(max(prediction_AllClass_outbag_temp)!=min(prediction_AllClass_outbag_temp))){
                  ratio_outbag_temp <- 1/(max(prediction_AllClass_outbag_temp) - min(prediction_AllClass_outbag_temp))
                  prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_temp - min(prediction_AllClass_outbag_temp)) * ratio_outbag_temp
                  prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp*2
                }

                auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_AllClass_outbag_temp)
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
                prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
                
                prediction_temp_temp_list[[row_i]] <- list()
                
              } 
              else if(fit_model_OVA){
                
                ###### OVA1
                # prediction_OVA1_temp_temp_matrix <- c()
                # threshold_OVA1_temp_c <- c()
                # auc_OVA1_temp_temp_c <- c()
                prediction_OVA1_temp_temp_matrix <- foreach (subset_i = seq_len(length(subsets_backward)), .combine = 'cbind') %DO_2% {
                  cat(subset_i, "\n")
                  names_x_select_OVA1_temp <- as.character(results_Wilcoxon_OVA1$names_x_NoFixed_sub_temp[1:subsets_backward[subset_i]])
                  names_x_select_OVA1_temp <- c(names_x_select_OVA1_temp, names_x_fixed)
                  modelformula_OVA1_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1_temp, collapse = "+")))
                  model_fit_OVA1_temp <- ranger::ranger(formula = modelformula_OVA1_temp, data = data_train_y_x_inbag_OVA1,
                                                        num.trees =parameters_grid$num.trees_c[row_i],
                                                        min.node.size = parameters_grid$min.node.size_c[row_i],
                                                        max.depth = parameters_grid$max.depth_c[row_i],
                                                        importance = "impurity", probability = TRUE, regularization.usedepth=TRUE)
                  
                  prediction_OVA1_temp_temp <- predict(model_fit_OVA1_temp, data = data_train_y_x_outbag)$predictions
                  prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp[, 2]
                  ratio_OVA1_temp_temp <- 1/(max(prediction_OVA1_temp_temp) - min(prediction_OVA1_temp_temp))
                  prediction_OVA1_temp_temp <- (prediction_OVA1_temp_temp - min(prediction_OVA1_temp_temp)) * ratio_OVA1_temp_temp
                  prediction_OVA1_temp_temp <- -1 + prediction_OVA1_temp_temp*2
                  
                  prediction_OVA1_temp_temp
                  # prediction_OVA1_temp_temp_matrix <- cbind(prediction_OVA1_temp_temp_matrix, prediction_OVA1_temp_temp)
                  
                }
                
                
                ###### OVA2
                # prediction_OVA2_temp_temp_matrix <- c()
                # threshold_OVA2_temp_c <- c()
                # auc_OVA2_temp_temp_c <- c()
                prediction_OVA2_temp_temp_matrix <- foreach (subset_i = seq_len(length(subsets_backward)), .combine = 'cbind') %DO_2% {
                  cat(subset_i, "\n")
                  names_x_select_OVA2_temp <- as.character(results_Wilcoxon_OVA2$names_x_NoFixed_sub_temp[1:subsets_backward[subset_i]])
                  names_x_select_OVA2_temp <- c(names_x_select_OVA2_temp, names_x_fixed)
                  modelformula_OVA2_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2_temp, collapse = "+")))
                  model_fit_OVA2_temp <- ranger::ranger(formula = modelformula_OVA2_temp, data = data_train_y_x_inbag_OVA2,
                                                        num.trees =parameters_grid$num.trees_c[row_i],
                                                        min.node.size = parameters_grid$min.node.size_c[row_i],
                                                        max.depth = parameters_grid$max.depth_c[row_i],
                                                        importance = "impurity", probability = TRUE, regularization.usedepth=TRUE)
                  
                  prediction_OVA2_temp_temp <- predict(model_fit_OVA2_temp, data = data_train_y_x_outbag)$predictions
                  prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp[, 2]
                  ratio_OVA2_temp_temp <- 1/(max(prediction_OVA2_temp_temp) - min(prediction_OVA2_temp_temp))
                  prediction_OVA2_temp_temp <- (prediction_OVA2_temp_temp - min(prediction_OVA2_temp_temp)) * ratio_OVA2_temp_temp
                  prediction_OVA2_temp_temp <- -1 + prediction_OVA2_temp_temp*2
                  prediction_OVA2_temp_temp
                  # prediction_OVA2_temp_temp_matrix <- cbind(prediction_OVA2_temp_temp_matrix, prediction_OVA2_temp_temp)
                  
                }
                
                
                tictoc::tic()
                idx_grid_NoX <- expand.grid(1:length(subsets_backward), 1:length(subsets_backward))
                # idx_grid_NoX <- cbind(1:length(subsets_backward), 1:length(subsets_backward))
                idx_grid_threshold <- expand.grid(seq(-0.98, 0.98, 0.02), seq(-0.98, 0.98, 0.02))
                results_grid_threshold <- foreach::foreach (idx_i = 1:nrow(idx_grid_NoX)) %DO_2% {
                  cat(idx_i, "\n")
                  idx_NoX_OVA1_temp <- idx_grid_NoX[idx_i, 1]
                  idx_NoX_OVA2_temp <- idx_grid_NoX[idx_i, 2]
                  
                  prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp_matrix[, idx_NoX_OVA1_temp]
                  prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp_matrix[, idx_NoX_OVA2_temp]
                  
                  quantile_prediction_OVA1_temp_temp <- aggregate(prediction_OVA1_temp_temp, by = list(data_train_y_x_outbag$Y), FUN = function(x) quantile(x, probs = c(0.05, 0.5,0.95)))
                  threshold_OVA1_upper <- quantile_prediction_OVA1_temp_temp$x[class_1, 2]
                  threshold_OVA1_lower <- max(c(quantile_prediction_OVA1_temp_temp$x[class_1, 1], sort(quantile_prediction_OVA1_temp_temp$x[, 2], decreasing = T)[2]))
                  threshold_OVA1_upper <- (ceiling(threshold_OVA1_upper*100) - ceiling(threshold_OVA1_upper*100) %% 2)/100
                  threshold_OVA1_lower <- (ceiling(threshold_OVA1_lower*100) - ceiling(threshold_OVA1_lower*100) %% 2)/100
                  
                  quantile_prediction_OVA2_temp_temp <- aggregate(prediction_OVA2_temp_temp, by = list(data_train_y_x_outbag$Y), FUN = function(x) quantile(x, probs = c(0.05, 0.5,0.95)))
                  threshold_OVA2_lower <- quantile_prediction_OVA2_temp_temp$x[class_2, 2]
                  threshold_OVA2_upper <- min(c(quantile_prediction_OVA2_temp_temp$x[class_2, 3], sort(quantile_prediction_OVA2_temp_temp$x[, 2], decreasing = F)[2]))
                  threshold_OVA2_upper <- (ceiling(threshold_OVA2_upper*100) - ceiling(threshold_OVA2_upper*100) %% 2)/100
                  threshold_OVA2_lower <- (ceiling(threshold_OVA2_lower*100) - ceiling(threshold_OVA2_lower*100) %% 2)/100
                  
                  if((threshold_OVA1_lower>threshold_OVA1_upper)|(threshold_OVA2_lower>threshold_OVA2_upper)){
                    abnormal_record_temp <- 1
                    idx_grid_threshold_temp <- expand.grid(seq(threshold_OVA1_upper, threshold_OVA1_upper, 0.02), seq(threshold_OVA2_lower, threshold_OVA2_lower, 0.02))
                  } else {
                    abnormal_record_temp <- 0
                    idx_grid_threshold_temp <- expand.grid(seq(threshold_OVA1_lower, threshold_OVA1_upper, 0.02), seq(threshold_OVA2_lower, threshold_OVA2_upper, 0.02))
                  }
                  
                  cat("rows of idx_grid_threshold_temp   ", dim(idx_grid_threshold_temp)[1], "\n")
                  auc_backward_c <- c()
                  for (idx_j in 1:nrow(idx_grid_threshold_temp)) {
                    # cat(idx_j, "\n")
                    threshold_OVA1_temp <- idx_grid_threshold_temp[idx_j, 1]
                    threshold_OVA2_temp <- idx_grid_threshold_temp[idx_j, 2]
                    
                    if(threshold_OVA1_temp<0){
                      prediction_OVA1_temp_temp <- ifelse(prediction_OVA1_temp_temp<threshold_OVA1_temp, 0, prediction_OVA1_temp_temp-threshold_OVA1_temp)
                    } else {
                      prediction_OVA1_temp_temp <- ifelse(prediction_OVA1_temp_temp<threshold_OVA1_temp, 0, prediction_OVA1_temp_temp)
                    }
                    
                    if(threshold_OVA2_temp>0){
                      prediction_OVA2_temp_temp <- ifelse(prediction_OVA2_temp_temp>threshold_OVA2_temp, 0, prediction_OVA2_temp_temp-threshold_OVA2_temp)
                    } else {
                      prediction_OVA2_temp_temp <- ifelse(prediction_OVA2_temp_temp>threshold_OVA2_temp, 0, prediction_OVA2_temp_temp)
                    }
                    
                    prediction_temp_temp <- prediction_OVA1_temp_temp + prediction_OVA2_temp_temp
                    
                    auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)
                    
                    auc_backward_c <- c(auc_backward_c, min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)))
                    
                  }
                  idx_grid_threshold_temp <- cbind(idx_grid_threshold_temp, auc_backward_c)
                  idx_grid_threshold_temp <- merge(idx_grid_threshold, idx_grid_threshold_temp, by=c("Var1", "Var2"), all.x = TRUE)
                  
                  # auc_backward_matrix <- cbind(auc_backward_matrix, idx_grid_threshold_temp$auc_backward_c)
                  
                  results_grid_threshold_temp <- list(auc_backward_matrix_temp=idx_grid_threshold_temp$auc_backward_c, abnormal_record_c=abnormal_record_temp)
                  results_grid_threshold_temp
                }
                tictoc::toc()
                
                auc_backward_matrix <- do.call(cbind, lapply(results_grid_threshold, `[[`, 1))
                abnormal_record <- do.call(c, lapply(results_grid_threshold, `[[`, 2))
                
                idxx_optimal_temp <- which(auc_backward_matrix == max(auc_backward_matrix, na.rm = T), arr.ind = TRUE)
                idx_optimal_NoX <- idxx_optimal_temp[1, 2]
                idx_optimal_threshold <- idxx_optimal_temp[1, 1]
                
                names_x_select_OVA1 <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[idx_grid_NoX[idx_optimal_NoX, 1]]]
                names_x_select_OVA1 <- c(as.character(names_x_select_OVA1), names_x_fixed)
                
                names_x_select_OVA2 <- results_Wilcoxon_OVA2$names_x[1:subsets_backward[idx_grid_NoX[idx_optimal_NoX, 2]]]
                names_x_select_OVA2 <- c(as.character(names_x_select_OVA2), names_x_fixed)
                
                threshold_OVA1 <- idx_grid_threshold[idx_optimal_threshold, 1]
                threshold_OVA2 <- idx_grid_threshold[idx_optimal_threshold, 2]
                
                ###
                names_x_select_list_temp[[row_i]] <- list(names_x_select_OVA1=names_x_select_OVA1, names_x_select_OVA2=names_x_select_OVA2)
                threshold_list_temp[[row_i]] <- list(threshold_OVA1=threshold_OVA1, threshold_OVA2=threshold_OVA2)
                
                ### model OVA1
                modelformula_OVA1 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1, collapse = "+")))
                model_fit_OVA1 <- ranger::ranger(formula = modelformula_OVA1, data = data_train_y_x_inbag_OVA1,
                                                 num.trees =parameters_grid$num.trees_c[row_i],
                                                 min.node.size = parameters_grid$min.node.size_c[row_i],
                                                 max.depth = parameters_grid$max.depth_c[row_i],
                                                 importance = "impurity", probability = TRUE, regularization.usedepth=TRUE)
                
                ### model OVA2
                modelformula_OVA2 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2, collapse = "+")))
                model_fit_OVA2 <- ranger::ranger(formula = modelformula_OVA2, data = data_train_y_x_inbag_OVA2,
                                                 num.trees =parameters_grid$num.trees_c[row_i],
                                                 min.node.size = parameters_grid$min.node.size_c[row_i],
                                                 max.depth = parameters_grid$max.depth_c[row_i],
                                                 importance = "impurity", probability = TRUE, regularization.usedepth=TRUE)
                
                model_fit_list_temp[[row_i]] <- list(model_fit_OVA1=model_fit_OVA1, model_fit_OVA2=model_fit_OVA2)
                
                # all classes inbag
                # ### prediction OVA1
                prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, data = data_train_y_x_inbag)$predictions
                prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, 2]
                ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
                prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
                prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
                
                if(threshold_OVA1==Inf){
                  prediction_AllClass_inbag_OVA1_temp <- rep(0, nrow(data_train_y_x_inbag))
                } else if(threshold_OVA1<0){
                  prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
                } else {
                  prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
                }
                
                # ### prediction OVA2
                prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, data = data_train_y_x_inbag)$predictions
                prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, 2]
                ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
                prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
                prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
                
                if(threshold_OVA2==Inf){
                  prediction_AllClass_inbag_OVA2_temp <- rep(0, nrow(data_train_y_x_inbag))
                } else if(threshold_OVA2>0){
                  prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
                } else {
                  prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
                }
                
                prediction_AllClass_inbag_temp <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp) / 2
                
                auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp)
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                
                # all classes outbag
                # ### prediction OVA1
                prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, data = data_train_y_x_outbag)$predictions
                prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, 2]
                ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
                prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
                prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
                
                if(threshold_OVA1==Inf){
                  prediction_AllClass_outbag_OVA1_temp <- rep(0, nrow(data_train_y_x_outbag))
                } else if(threshold_OVA1<0){
                  prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
                } else {
                  prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
                }
                
                # ### prediction OVA2
                prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, data = data_train_y_x_outbag)$predictions
                prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, 2]
                ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
                prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
                prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
                
                if(threshold_OVA2==Inf){
                  prediction_AllClass_outbag_OVA2_temp <- rep(0, nrow(data_train_y_x_outbag))
                } else if(threshold_OVA2>0){
                  prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
                } else {
                  prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
                }
                
                prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
                
                auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_AllClass_outbag_temp)
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
                prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
                
                prediction_temp_temp_list[[row_i]] <- list()
                
              } 
              else {
                names_x_select <- names_x
                names_x_select_OVA1 <- names_x
                names_x_select_OVA2 <- names_x
                names_x_select_list_temp[[row_i]] <- list(names_x_select=names_x_select)
                
                model_fit <- NULL
                
                prediction_temp_temp_list[[row_i]] <- list()
              }
            }
          }
          
          # colnames(auc_temp_inbag) <- c("minimum_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
          # colnames(auc_temp_outbag) <- c("minimum_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
          
          if(length(class_others)>0) {
            colnames(auc_temp_inbag) <- c("minimum_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", paste0(class_1, collapse = "_"), "_vs_", class_others), paste0("auc_inbag_", paste0(class_2, collapse = "_"), "_vs_", class_others))
            colnames(auc_temp_outbag) <- c("minimum_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", paste0(class_1, collapse = "_"), "_vs_", class_others), paste0("auc_outbag_", paste0(class_2, collapse = "_"), "_vs_", class_others))
          } else {
            colnames(auc_temp_inbag) <- c("minimum_inbag", "auc_binary_temp_inbag")
            colnames(auc_temp_outbag) <- c("minimum_outbag", "auc_binary_temp_outbag")
          }
          
          auc_parameters_grid <- cbind(parameters_grid, cbind(auc_temp_inbag, auc_temp_outbag))

        } else if(model_i=="SVM"){

          # type_c <- c("C-classification")
          # kenel_c <- c("linear", "radial", "polynomial", "sigmoid")
          # degree_c <- c(2, 3)    ## 	parameter needed for kernel of type polynomial (default: 3)
          # # gamma_c <- c(0.001, 0.01, 0.1, 1)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
          # gamma_c <- c(0.001, 1)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
          # coef0_c <- c(0)    ##  parameter needed for kernels of type polynomial and sigmoid (default: 0)
          # cost_c <- c(0.001, 0.01, 0.05, 0.1, 1)  	##  cost of constraints violation (default: 1)—it is the ‘C???-constant of the regularization term in the Lagrange formulation.
          # nu_c <- c(0.1, 0.3, 0.5, 0.7, 1)   ##  parameter needed for nu-classification, nu-regression, and one-classification
          # tolerance_c <- c(0.001)   ##  tolerance of termination criterion (default: 0.001)
          # epsilon_c <- c(0.1)   ##  epsilon in the insensitive-loss function (default: 0.1)
          # shrinking_c <- c("TRUE")   ##  option whether to use the shrinking-heuristics (default: TRUE)
          
          
         type_c <- c("C-classification")
         kenel_c <- c("linear", "polynomial")
         degree_c <- c(2)    ## 	parameter needed for kernel of type polynomial (default: 3)
         # gamma_c <- c(0.001, 0.01, 0.1, 1)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
         gamma_c <- c(0.001)    ##  parameter needed for all kernels except linear (default: 1/(data dimension))
         coef0_c <- c(0)    ##  parameter needed for kernels of type polynomial and sigmoid (default: 0)
         cost_c <- c(0.001, 1)  	##  cost of constraints violation (default: 1)—it is the ‘C???-constant of the regularization term in the Lagrange formulation.
         nu_c <- c(0.1, 0.5, 1)   ##  parameter needed for nu-classification, nu-regression, and one-classification
         tolerance_c <- c(0.001)   ##  tolerance of termination criterion (default: 0.001)
         epsilon_c <- c(0.1)   ##  epsilon in the insensitive-loss function (default: 0.1)
         shrinking_c <- c("TRUE")   ##  option whether to use the shrinking-heuristics (default: TRUE)

          parameters_grid <- expand.grid(type_c=type_c, kenel_c=kenel_c, degree_c=degree_c, gamma_c=gamma_c, coef0_c=coef0_c,
                                         cost_c=cost_c, nu_c=nu_c, tolerance_c=tolerance_c, epsilon_c=epsilon_c, shrinking_c=shrinking_c)

          parameters_grid$degree_c <- ifelse(is.element(parameters_grid$kenel_c, c("polynomial")), parameters_grid$degree_c, 3)
          parameters_grid$gamma_c <- ifelse(is.element(parameters_grid$kenel_c, c("radial", "polynomial", "sigmoid")), parameters_grid$gamma_c, 0.5)
          parameters_grid$coef0_c <- ifelse(is.element(parameters_grid$kenel_c, c("polynomial", "sigmoid")), parameters_grid$coef0_c, 0)
          parameters_grid$nu_c <- ifelse(is.element(parameters_grid$type_c, c("nu-classification", "nu-regression")), parameters_grid$nu_c, 0.5)
          parameters_grid <- dplyr::distinct(parameters_grid)

          names_x_select_list_temp <- list()
          threshold_list_temp <- list()
          model_fit_list_temp <- list()
          prediction_AllClass_inbag_list_temp <- list()
          prediction_AllClass_outbag_list_temp <- list()
          auc_temp_inbag <- c()
          auc_temp_outbag <- c()
          prediction_temp_temp_list <- list()
          for (row_i in 1:nrow(parameters_grid)) {
            cat(row_i, "\n")
            
            if(rfe_select){
              RFE_control <-  list(summary = caret::defaultSummary,
                                   fit = function(x, y, first=TRUE, last=FALSE){
                                     e1071::svm(x, y,
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
                                   },
                                   pred = function(object, x) {
                                     prediction_temp <- predict(object, newdata=x, probability = TRUE)
                                     prediction_temp <- attr(prediction_temp, "probabilities")
                                     prediction_temp <- apply(prediction_temp, 1, which.max)
                                     prediction_temp
                                   },
                                   rank = function(object, x, y) {
                                     vimp <- t(object$coefs)%*%object$SV
                                     # vimp <- t(model_fit$coefs)%*%model_fit$SV
                                     order_vimp <- order(abs(vimp), decreasing = TRUE)
                                     vimp <- as.data.frame(t(vimp))
                                     vimp$var <- rownames(vimp)
                                     colnames(vimp) <- c("Overall", "var")
                                     vimp <- vimp[order_vimp,]
                                     vimp
                                   },
                                   selectSize = caret::pickSizeBest,
                                   selectVar = caret::pickVars)
              
              ctrl <- caret::rfeControl(functions = RFE_control)
              #ctrl$returnResamp <- "all"
              set.seed(seed_rfe)
              rfProfile <- caret::rfe(as.matrix(x_rfe), y_rfe, sizes = subsets_rfe, rfeControl = ctrl)
              names_x_select <- rfProfile$optVariables
              
            } else if(select_backward_wilcoxon) {
              
              ###### binary
              if(fit_model_binary){
                auc_backward <- c()
                for (subset_i in seq_len(length(subsets_backward))) {
                  names_x_select_temp <- as.character(results_Wilcoxon$names_x_NoFixed_sub_temp[1:subsets_backward[subset_i]])
                  names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
                  modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
                  model_fit_temp <-  e1071::svm(modelformula, data = data_train_y_x_inbag_binary,
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
                  
                  prediction_temp_temp <- predict(model_fit_temp, newdata = data_train_y_x_outbag, probability = TRUE)
                  prediction_temp_temp <- attr(prediction_temp_temp, "probabilities")
                  prediction_temp_temp <- prediction_temp_temp[, "1"]
                  if(rescale_prediction&(max(prediction_temp_temp)!=min(prediction_temp_temp))){
                    ratio_temp_temp <- 1/(max(prediction_temp_temp) - min(prediction_temp_temp))
                    prediction_temp_temp <- (prediction_temp_temp - min(prediction_temp_temp)) * ratio_temp_temp
                    prediction_temp_temp <- -1 + prediction_temp_temp*2
                  }
                  
                  if(inherits(try(auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)), "try-error")){
                    auc_backward <- c(auc_backward, 0.5)
                  } else {
                    auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)
                    auc_backward <- c(auc_backward, auc_temp$auc_binary) 
                  }
                  
                }
                
                names_x_select <- results_Wilcoxon$names_x_NoFixed_sub_temp[1:subsets_backward[which.max(auc_backward)]]
                names_x_select <- c(as.character(names_x_select), names_x_fixed)
                
                ###
                names_x_select_list_temp[[row_i]] <- list(names_x_select=names_x_select)
                
                modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select, collapse = "+")))
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
                
                ### Record model
                model_fit_list_temp[[row_i]] <- list(model_fit=model_fit)
                
                # all classes inbag
                ### binary
                prediction_AllClass_inbag_temp <- predict(model_fit, newdata = data_train_y_x_inbag, probability = TRUE)
                prediction_AllClass_inbag_temp <- attr(prediction_AllClass_inbag_temp, "probabilities")
                prediction_AllClass_inbag_temp <- prediction_AllClass_inbag_temp[, "1"]
                if(rescale_prediction&(max(prediction_AllClass_inbag_temp)!=min(prediction_AllClass_inbag_temp))){
                  ratio_inbag_temp <- 1/(max(prediction_AllClass_inbag_temp) - min(prediction_AllClass_inbag_temp))
                  prediction_AllClass_inbag_temp <- (prediction_AllClass_inbag_temp - min(prediction_AllClass_inbag_temp)) * ratio_inbag_temp
                  prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp*2
                }
                
                
                if(any(is.na(prediction_AllClass_inbag_temp))){
                  auc_temp <- list(auc_binary=0, auc_others_1=rep(0, K-2), auc_others_2=rep(0, K-2))
                } else {
                  auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp)
                }
                
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                
                # all classes outbag
                ### binary
                prediction_AllClass_outbag_temp <- predict(model_fit, newdata = data_train_y_x_outbag, probability = TRUE)
                prediction_AllClass_outbag_temp <- attr(prediction_AllClass_outbag_temp, "probabilities")
                prediction_AllClass_outbag_temp <- prediction_AllClass_outbag_temp[, "1"]
                if(rescale_prediction&(max(prediction_AllClass_outbag_temp)!=min(prediction_AllClass_outbag_temp))){
                  ratio_outbag_temp <- 1/(max(prediction_AllClass_outbag_temp) - min(prediction_AllClass_outbag_temp))
                  prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_temp - min(prediction_AllClass_outbag_temp)) * ratio_outbag_temp
                  prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp*2
                }
                
                if(any(is.na(prediction_AllClass_outbag_temp))){
                  auc_temp <- list(auc_binary=0, auc_others_1=rep(0, K-2), auc_others_2=rep(0, K-2))
                } else {
                  auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_AllClass_outbag_temp)
                }
                
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                ### Record the Prediction
                prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
                prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
                
              }
              else if(fit_model_OVA){
                
                ###### OVA1
                # prediction_OVA1_temp_temp_matrix <- c()
                # # threshold_OVA1_temp_c <- c()
                # prediction_OVA1_temp_temp_list <- list()
                # prediction_OVA1_temp_temp_list2 <- list()
                prediction_OVA1_temp_temp_temp <- foreach (subset_i = seq_len(length(subsets_backward))) %DO_2% {
                  cat(subset_i, "\n")
                  names_x_select_OVA1_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
                  names_x_select_OVA1_temp <- c(names_x_select_OVA1_temp, names_x_fixed)
                  modelformula_OVA1_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1_temp, collapse = "+")))
                  
                  model_fit_OVA1_temp <-  e1071::svm(modelformula_OVA1_temp, data = data_train_y_x_inbag_OVA1,
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
                  
                  prediction_OVA1_temp_temp <- predict(model_fit_OVA1_temp, newdata=data_train_y_x_outbag, probability = TRUE)
                  prediction_OVA1_temp_temp <- attr(prediction_OVA1_temp_temp, "probabilities")
                  prediction_OVA1_temp_temp_list <- prediction_OVA1_temp_temp
                  prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp[, "1"]
                  prediction_OVA1_temp_temp_list2 <- prediction_OVA1_temp_temp
                  ratio_OVA1_temp_temp <- 1/(max(prediction_OVA1_temp_temp) - min(prediction_OVA1_temp_temp))
                  prediction_OVA1_temp_temp <- (prediction_OVA1_temp_temp - min(prediction_OVA1_temp_temp)) * ratio_OVA1_temp_temp
                  prediction_OVA1_temp_temp <- -1 + prediction_OVA1_temp_temp*2
                  
                  prediction_OVA1_temp_temp_temp_temp <- list(prediction_OVA1_temp_temp=prediction_OVA1_temp_temp, 
                                                              prediction_OVA1_temp_temp_list=prediction_OVA1_temp_temp_list,
                                                              prediction_OVA1_temp_temp_list2=prediction_OVA1_temp_temp_list2)
                  
                  # prediction_OVA1_temp_temp_matrix <- cbind(prediction_OVA1_temp_temp_matrix, prediction_OVA1_temp_temp)
                }
                
                prediction_OVA1_temp_temp_matrix <- do.call(cbind, lapply(prediction_OVA1_temp_temp_temp, `[[`, 1))
                prediction_OVA1_temp_temp_list <- lapply(prediction_OVA1_temp_temp_temp, `[[`, 2)
                prediction_OVA1_temp_temp_list2 <- lapply(prediction_OVA1_temp_temp_temp, `[[`, 3)
                
                ###### OVA2
                # prediction_OVA2_temp_temp_matrix <- c()
                # # threshold_OVA2_temp_c <- c()
                # prediction_OVA2_temp_temp_list <- list()
                # prediction_OVA2_temp_temp_list2 <- list()
                prediction_OVA2_temp_temp_temp <- foreach (subset_i = seq_len(length(subsets_backward))) %DO_2% {
                  cat(subset_i, "\n")
                  names_x_select_OVA2_temp <- as.character(results_Wilcoxon_OVA2$names_x[1:subsets_backward[subset_i]])
                  names_x_select_OVA2_temp <- c(names_x_select_OVA2_temp, names_x_fixed)
                  modelformula_OVA2_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2_temp, collapse = "+")))
                  
                  model_fit_OVA2_temp <-  e1071::svm(modelformula_OVA2_temp, data = data_train_y_x_inbag_OVA2,
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
                  
                  prediction_OVA2_temp_temp <- predict(model_fit_OVA2_temp, newdata=data_train_y_x_outbag, probability = TRUE)
                  prediction_OVA2_temp_temp <- attr(prediction_OVA2_temp_temp, "probabilities")
                  prediction_OVA2_temp_temp_list <- prediction_OVA2_temp_temp
                  prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp[, "1"]
                  prediction_OVA2_temp_temp_list2 <- prediction_OVA2_temp_temp
                  ratio_OVA2_temp_temp <- 1/(max(prediction_OVA2_temp_temp) - min(prediction_OVA2_temp_temp))
                  prediction_OVA2_temp_temp <- (prediction_OVA2_temp_temp - min(prediction_OVA2_temp_temp)) * ratio_OVA2_temp_temp
                  prediction_OVA2_temp_temp <- -1 + prediction_OVA2_temp_temp*2
                  
                  prediction_OVA2_temp_temp_temp_temp <- list(prediction_OVA2_temp_temp=prediction_OVA2_temp_temp, 
                                                              prediction_OVA2_temp_temp_list=prediction_OVA2_temp_temp_list,
                                                              prediction_OVA2_temp_temp_list2=prediction_OVA2_temp_temp_list2)
                  
                  # prediction_OVA2_temp_temp_matrix <- cbind(prediction_OVA2_temp_temp_matrix, prediction_OVA2_temp_temp)
                }
                
                prediction_OVA2_temp_temp_matrix <- do.call(cbind, lapply(prediction_OVA2_temp_temp_temp, `[[`, 1))
                prediction_OVA2_temp_temp_list <- lapply(prediction_OVA2_temp_temp_temp, `[[`, 2)
                prediction_OVA2_temp_temp_list2 <- lapply(prediction_OVA2_temp_temp_temp, `[[`, 3)
                
                
                tictoc::tic()
                idx_grid_NoX <- expand.grid(1:length(subsets_backward), 1:length(subsets_backward))
                # idx_grid_NoX <- cbind(1:length(subsets_backward), 1:length(subsets_backward))
                idx_grid_threshold <- expand.grid(seq(-0.98, 0.98, 0.02), seq(-0.98, 0.98, 0.02))
                results_grid_threshold <- foreach::foreach (idx_i = 1:nrow(idx_grid_NoX)) %DO_2% {
                  cat(idx_i, "\n")
                  idx_NoX_OVA1_temp <- idx_grid_NoX[idx_i, 1]
                  idx_NoX_OVA2_temp <- idx_grid_NoX[idx_i, 2]
                  
                  prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp_matrix[, idx_NoX_OVA1_temp]
                  prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp_matrix[, idx_NoX_OVA2_temp]
                  
                  if(any(is.na(prediction_OVA1_temp_temp))){
                    prediction_OVA1_temp_temp <- rep(0, length(prediction_OVA1_temp_temp))
                  }
                  
                  if(any(is.na(prediction_OVA2_temp_temp))){
                    prediction_OVA2_temp_temp <- rep(0, length(prediction_OVA2_temp_temp))
                  }
                  
                  quantile_prediction_OVA1_temp_temp <- aggregate(prediction_OVA1_temp_temp, by = list(data_train_y_x_outbag$Y), FUN = function(x) quantile(x, probs = c(0.05, 0.5,0.95)))
                  threshold_OVA1_upper <- quantile_prediction_OVA1_temp_temp$x[class_1, 2]
                  threshold_OVA1_lower <- max(c(quantile_prediction_OVA1_temp_temp$x[class_1, 1], sort(quantile_prediction_OVA1_temp_temp$x[, 2], decreasing = T)[2]))
                  threshold_OVA1_upper <- (ceiling(threshold_OVA1_upper*100) - ceiling(threshold_OVA1_upper*100) %% 2)/100
                  threshold_OVA1_lower <- (ceiling(threshold_OVA1_lower*100) - ceiling(threshold_OVA1_lower*100) %% 2)/100
                  
                  quantile_prediction_OVA2_temp_temp <- aggregate(prediction_OVA2_temp_temp, by = list(data_train_y_x_outbag$Y), FUN = function(x) quantile(x, probs = c(0.05, 0.5,0.95)))
                  threshold_OVA2_lower <- quantile_prediction_OVA2_temp_temp$x[class_2, 2]
                  threshold_OVA2_upper <- min(c(quantile_prediction_OVA2_temp_temp$x[class_2, 3], sort(quantile_prediction_OVA2_temp_temp$x[, 2], decreasing = F)[2]))
                  threshold_OVA2_upper <- (ceiling(threshold_OVA2_upper*100) - ceiling(threshold_OVA2_upper*100) %% 2)/100
                  threshold_OVA2_lower <- (ceiling(threshold_OVA2_lower*100) - ceiling(threshold_OVA2_lower*100) %% 2)/100
                  
                  if((threshold_OVA1_upper<max(quantile_prediction_OVA1_temp_temp$x[, 2]))|(threshold_OVA2_lower>min(quantile_prediction_OVA2_temp_temp$x[, 2]))){
                    abnormal_record_temp <- 1
                    idx_grid_threshold_temp <- expand.grid(seq(threshold_OVA1_upper, threshold_OVA1_upper, 0.02), seq(threshold_OVA2_lower, threshold_OVA2_lower, 0.02))
                  } else {
                    abnormal_record_temp <- 0
                    idx_grid_threshold_temp <- expand.grid(seq(threshold_OVA1_lower, threshold_OVA1_upper, 0.02), seq(threshold_OVA2_lower, threshold_OVA2_upper, 0.02))
                  }
                  
                  cat("rows of idx_grid_threshold_temp   ", dim(idx_grid_threshold_temp)[1], "\n")
                  auc_backward_c <- c()
                  for (idx_j in 1:nrow(idx_grid_threshold_temp)) {
                    # cat(idx_j, "\n")
                    threshold_OVA1_temp <- idx_grid_threshold_temp[idx_j, 1]
                    threshold_OVA2_temp <- idx_grid_threshold_temp[idx_j, 2]
                    
                    if(threshold_OVA1_temp<0){
                      prediction_OVA1_temp_temp <- ifelse(prediction_OVA1_temp_temp<threshold_OVA1_temp, 0, prediction_OVA1_temp_temp-threshold_OVA1_temp)
                    } else {
                      prediction_OVA1_temp_temp <- ifelse(prediction_OVA1_temp_temp<threshold_OVA1_temp, 0, prediction_OVA1_temp_temp)
                    }
                    
                    if(threshold_OVA2_temp>0){
                      prediction_OVA2_temp_temp <- ifelse(prediction_OVA2_temp_temp>threshold_OVA2_temp, 0, prediction_OVA2_temp_temp-threshold_OVA2_temp)
                    } else {
                      prediction_OVA2_temp_temp <- ifelse(prediction_OVA2_temp_temp>threshold_OVA2_temp, 0, prediction_OVA2_temp_temp)
                    }
                    
                    prediction_temp_temp <- prediction_OVA1_temp_temp + prediction_OVA2_temp_temp
                    
                    auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)
                    
                    auc_backward_c <- c(auc_backward_c, min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)))
                    
                  }
                  idx_grid_threshold_temp <- cbind(idx_grid_threshold_temp, auc_backward_c)
                  idx_grid_threshold_temp <- merge(idx_grid_threshold, idx_grid_threshold_temp, by=c("Var1", "Var2"), all.x = TRUE)
                  
                  # auc_backward_matrix <- cbind(auc_backward_matrix, idx_grid_threshold_temp$auc_backward_c)
                  
                  results_grid_threshold_temp <- list(auc_backward_matrix_temp=idx_grid_threshold_temp$auc_backward_c, abnormal_record_c=abnormal_record_temp)
                  results_grid_threshold_temp
                }
                tictoc::toc()
                
                auc_backward_matrix <- do.call(cbind, lapply(results_grid_threshold, `[[`, 1))
                abnormal_record <- do.call(c, lapply(results_grid_threshold, `[[`, 2))
                
                idxx_optimal_temp <- which(auc_backward_matrix == max(auc_backward_matrix, na.rm = T), arr.ind = TRUE)
                idx_optimal_NoX <- idxx_optimal_temp[1, 2]
                idx_optimal_threshold <- idxx_optimal_temp[1, 1]
                
                names_x_select_OVA1 <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[idx_grid_NoX[idx_optimal_NoX, 1]]]
                names_x_select_OVA1 <- c(as.character(names_x_select_OVA1), names_x_fixed)
                
                names_x_select_OVA2 <- results_Wilcoxon_OVA2$names_x[1:subsets_backward[idx_grid_NoX[idx_optimal_NoX, 2]]]
                names_x_select_OVA2 <- c(as.character(names_x_select_OVA2), names_x_fixed)
                
                threshold_OVA1 <- idx_grid_threshold[idx_optimal_threshold, 1]
                threshold_OVA2 <- idx_grid_threshold[idx_optimal_threshold, 2]
                
                ###  Record: features selected, prediction of OVA1 and OVA2
                names_x_select_list_temp[[row_i]] <- list(names_x_select=names_x_select, names_x_select_OVA1=names_x_select_OVA1, names_x_select_OVA2=names_x_select_OVA2)
                threshold_list_temp[[row_i]] <- list(threshold_OVA1=threshold_OVA1, threshold_OVA2=threshold_OVA2)
                
                prediction_temp_temp_list[[row_i]] <- list(prediction_OVA1_temp_temp_list=prediction_OVA1_temp_temp_list, prediction_OVA1_temp_temp_list2=prediction_OVA1_temp_temp_list2,
                                                           prediction_OVA2_temp_temp_list=prediction_OVA2_temp_temp_list, prediction_OVA2_temp_temp_list2=prediction_OVA2_temp_temp_list2,
                                                           prediction_OVA1_temp_temp_matrix=prediction_OVA1_temp_temp_matrix, prediction_OVA2_temp_temp_matrix=prediction_OVA2_temp_temp_matrix,
                                                           abnormal_record=abnormal_record)
                
                
                ### Fit model
                ### model OVA1
                modelformula_OVA1 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1, collapse = "+")))
                model_fit_OVA1 <-  e1071::svm(modelformula_OVA1, data = data_train_y_x_inbag_OVA1,
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
                
                attr(model_fit_OVA1$terms, ".Environment") <- NULL
                
                
                ### model OVA2
                modelformula_OVA2 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2, collapse = "+")))
                model_fit_OVA2 <-  e1071::svm(modelformula_OVA2, data = data_train_y_x_inbag_OVA2,
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
                
                attr(model_fit_OVA2$terms, ".Environment") <- NULL
                
                ### Record model
                model_fit_list_temp[[row_i]] <- list(model_fit_OVA1=model_fit_OVA1, model_fit_OVA2=model_fit_OVA2)
                
                # all classes inbag
                ### prediction OVA1
                prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, newdata=data_train_y_x_inbag, probability = TRUE)
                prediction_AllClass_inbag_OVA1_temp <- attr(prediction_AllClass_inbag_OVA1_temp, "probabilities")
                prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, "1"]
                ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
                prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
                prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
                
                if(threshold_OVA1==Inf){
                  prediction_AllClass_inbag_OVA1_temp <- rep(0, nrow(data_train_y_x_inbag))
                } else if(threshold_OVA1<0){
                  prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
                } else {
                  prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
                }
                
                ### prediction OVA2
                prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, newdata=data_train_y_x_inbag, probability = TRUE)
                prediction_AllClass_inbag_OVA2_temp <- attr(prediction_AllClass_inbag_OVA2_temp, "probabilities")
                prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, "1"]
                ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
                prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
                prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
                
                if(threshold_OVA2==Inf){
                  prediction_AllClass_inbag_OVA2_temp <- rep(0, nrow(data_train_y_x_inbag))
                } else if(threshold_OVA2>0){
                  prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
                } else {
                  prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
                }
                
                prediction_AllClass_inbag_temp <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp)
                
                if(any(is.na(prediction_AllClass_inbag_temp))){
                  auc_temp <- list(auc_binary=0, auc_others_1=rep(0, K-2), auc_others_2=rep(0, K-2))
                } else {
                  auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp)
                }
                
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_inbag <- rbind(auc_temp_inbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                
                # all classes outbag
                ### prediction OVA1
                prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, newdata=data_train_y_x_outbag, probability = TRUE)
                prediction_AllClass_outbag_OVA1_temp <- attr(prediction_AllClass_outbag_OVA1_temp, "probabilities")
                prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, "1"]
                ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
                prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
                prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
                
                if(threshold_OVA1==Inf){
                  prediction_AllClass_outbag_OVA1_temp <- rep(0, nrow(data_train_y_x_outbag))
                } else if(threshold_OVA1<0){
                  prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
                } else {
                  prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
                }
                
                ### prediction OVA2
                prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, newdata=data_train_y_x_outbag, probability = TRUE)
                prediction_AllClass_outbag_OVA2_temp <- attr(prediction_AllClass_outbag_OVA2_temp, "probabilities")
                prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, "1"]
                ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
                prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
                prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
                
                if(threshold_OVA2==Inf){
                  prediction_AllClass_outbag_OVA2_temp <- rep(0, nrow(data_train_y_x_outbag))
                } else if(threshold_OVA2>0){
                  prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
                } else {
                  prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
                }
                
                prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp)
                
                if(any(is.na(prediction_AllClass_outbag_temp))){
                  auc_temp <- list(auc_binary=0, auc_others_1=rep(0, K-2), auc_others_2=rep(0, K-2))
                } else {
                  auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_AllClass_outbag_temp)
                }
                
                
                if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                } else {
                  auc_temp_outbag <- rbind(auc_temp_outbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
                }
                
                ### Record Predictions
                prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
                prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
                
                
              }
              else {
                names_x_select <- names_x
              }
              
              
            } else {
              names_x_select_OVA1 <- names_x
              names_x_select_OVA2 <- names_x
            }
            
          }
          
          # colnames(auc_temp_inbag) <- c("minimum_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
          # colnames(auc_temp_outbag) <- c("minimum_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
          
          if(length(class_others)>0) {
            colnames(auc_temp_inbag) <- c("minimum_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", paste0(class_1, collapse = "_"), "_vs_", class_others), paste0("auc_inbag_", paste0(class_2, collapse = "_"), "_vs_", class_others))
            colnames(auc_temp_outbag) <- c("minimum_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", paste0(class_1, collapse = "_"), "_vs_", class_others), paste0("auc_outbag_", paste0(class_2, collapse = "_"), "_vs_", class_others))
          } else {
            colnames(auc_temp_inbag) <- c("minimum_inbag", "auc_binary_temp_inbag")
            colnames(auc_temp_outbag) <- c("minimum_outbag", "auc_binary_temp_outbag")
          }
          auc_parameters_grid <- cbind(parameters_grid, cbind(auc_temp_inbag, auc_temp_outbag))


        } 
        # else if(model_i=="LASSO"){
        #   # library("glmnet")
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   threshold_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   prediction_temp_temp_list <- list()
        #   
        #   set.seed(318)
        #   ###### OVA1
        #   x_train_OVA1_temp_inbag <- as.matrix(data_train_y_x_inbag_OVA1[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   y_train_OVA1_temp_inbag <- data_train_y_x_inbag_OVA1[, name_y_factor]
        #   model_fit_OVA1 <- glmnet::cv.glmnet(x_train_OVA1_temp_inbag, y_train_OVA1_temp_inbag, family="binomial", maxit = 10000)
        #   
        #   ###### OVA2
        #   x_train_OVA2_temp_inbag <- as.matrix(data_train_y_x_inbag_OVA2[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   y_train_OVA2_temp_inbag <- data_train_y_x_inbag_OVA2[, name_y_factor]
        #   model_fit_OVA2 <- glmnet::cv.glmnet(x_train_OVA2_temp_inbag, y_train_OVA2_temp_inbag, family="binomial", maxit = 10000)
        #   
        #   row_i <- 1
        #   model_fit_list_temp[[row_i]] <- list(model_fit_OVA1=model_fit_OVA1, model_fit_OVA2=model_fit_OVA2)
        #   # plot(cv.fit)
        #   # fit = glmnet(x_radio_train, y_train, family = "binomial", lambda = cv.fit$lambda.min)
        #   
        #   # x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
        #   # y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_numerical]
        #   # prediction_temp <- predict(model_fit, newx = x_train_binary_temp_outbag, s = "lambda.min", type = "response")
        #   # prediction_temp <- -1 + prediction_temp[, 1]*2
        #   # auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #   # auc_parameters_grid <- as.data.frame(auc_temp)
        # 
        #   
        #   x_train_temp_outbag_OVA1 <- as.matrix(data_train_y_x_outbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   x_train_temp_outbag_OVA2 <- as.matrix(data_train_y_x_outbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   
        #   tictoc::tic()
        #   auc_backward_c_OVA1 <- foreach::foreach (lambda_OVA1_temp = model_fit_OVA1$lambda, .combine = "c", .packages = c("glmnet")) %DO_2% {
        #     
        #     prediction_OVA1_temp_temp <- predict(model_fit_OVA1, newx = x_train_temp_outbag_OVA1, s = lambda_OVA1_temp, type = "response")
        #     prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp[, 1]
        #     ratio_outbag_OVA1_temp <- 1/(max(prediction_OVA1_temp_temp) - min(prediction_OVA1_temp_temp))
        #     prediction_OVA1_temp_temp <- (prediction_OVA1_temp_temp - min(prediction_OVA1_temp_temp)) * ratio_outbag_OVA1_temp
        #     prediction_OVA1_temp_temp <- -1 + prediction_OVA1_temp_temp*2
        #     
        #     if(any(is.na(prediction_OVA1_temp_temp))){
        #       prediction_OVA1_temp_temp <- rep(0, length(prediction_OVA1_temp_temp))
        #     }
        #     
        #     auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_OVA1_temp_temp)
        #     
        #     min(c(auc_temp$auc_binary, auc_temp$auc_others_1))
        #   }
        #   tictoc::toc()
        #   
        #   lambda_c_OVA1 <- model_fit_OVA1$lambda[order(auc_backward_c_OVA1, decreasing = T)[1:(0.2*length(auc_backward_c_OVA1))]]
        #   
        #   tictoc::tic()
        #   auc_backward_c_OVA2 <- foreach::foreach (lambda_OVA2_temp = model_fit_OVA2$lambda, .combine = "c", .packages = c("glmnet")) %DO_2% {
        #     
        #     prediction_OVA2_temp_temp <- predict(model_fit_OVA2, newx = x_train_temp_outbag_OVA2, s = lambda_OVA2_temp, type = "response")
        #     prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp[, 1]
        #     ratio_outbag_OVA2_temp <- 1/(max(prediction_OVA2_temp_temp) - min(prediction_OVA2_temp_temp))
        #     prediction_OVA2_temp_temp <- (prediction_OVA2_temp_temp - min(prediction_OVA2_temp_temp)) * ratio_outbag_OVA2_temp
        #     prediction_OVA2_temp_temp <- -1 + prediction_OVA2_temp_temp*2
        #     
        #     if(any(is.na(prediction_OVA2_temp_temp))){
        #       prediction_OVA2_temp_temp <- rep(0, length(prediction_OVA2_temp_temp))
        #     }
        #     
        #     auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_OVA2_temp_temp)
        #     
        #     min(c(auc_temp$auc_binary, auc_temp$auc_others_2))
        #   }
        #   tictoc::toc()
        #   
        #   lambda_c_OVA2 <- model_fit_OVA2$lambda[order(auc_backward_c_OVA2, decreasing = T)[1:(0.2*length(auc_backward_c_OVA2))]]
        #   
        #   tictoc::tic()
        #   grid_lambda <- expand.grid(lambda_c_OVA1, lambda_c_OVA2)
        #   idx_grid_threshold <- expand.grid(seq(-0.98, 0.98, 0.02), seq(-0.98, 0.98, 0.02))
        #   results_grid_threshold <- foreach::foreach (idx_i = 1:nrow(grid_lambda), .packages = c("glmnet")) %DO_2% {
        #     cat(idx_i, "\n")
        #     lambda_OVA1_temp <- grid_lambda[idx_i, 1]
        #     lambda_OVA2_temp <- grid_lambda[idx_i, 2]
        #     
        #     prediction_OVA1_temp_temp <- predict(model_fit_OVA1, newx = x_train_temp_outbag_OVA1, s = lambda_OVA1_temp, type = "response")
        #     prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp[, 1]
        #     ratio_outbag_OVA1_temp <- 1/(max(prediction_OVA1_temp_temp) - min(prediction_OVA1_temp_temp))
        #     prediction_OVA1_temp_temp <- (prediction_OVA1_temp_temp - min(prediction_OVA1_temp_temp)) * ratio_outbag_OVA1_temp
        #     prediction_OVA1_temp_temp <- -1 + prediction_OVA1_temp_temp*2
        #     
        #     prediction_OVA2_temp_temp <- predict(model_fit_OVA2, newx = x_train_temp_outbag_OVA2, s = lambda_OVA2_temp, type = "response")
        #     prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp[, 1]
        #     ratio_outbag_OVA2_temp <- 1/(max(prediction_OVA2_temp_temp) - min(prediction_OVA2_temp_temp))
        #     prediction_OVA2_temp_temp <- (prediction_OVA2_temp_temp - min(prediction_OVA2_temp_temp)) * ratio_outbag_OVA2_temp
        #     prediction_OVA2_temp_temp <- -1 + prediction_OVA2_temp_temp*2
        #     
        #     if(any(is.na(prediction_OVA1_temp_temp))){
        #       prediction_OVA1_temp_temp <- rep(0, length(prediction_OVA1_temp_temp))
        #     }
        #     
        #     if(any(is.na(prediction_OVA2_temp_temp))){
        #       prediction_OVA2_temp_temp <- rep(0, length(prediction_OVA2_temp_temp))
        #     }
        #     
        #     quantile_prediction_OVA1_temp_temp <- aggregate(prediction_OVA1_temp_temp, by = list(data_train_y_x_outbag$Y), FUN = function(x) quantile(x, probs = c(0.05, 0.5,0.95)))
        #     threshold_OVA1_upper <- quantile_prediction_OVA1_temp_temp$x[class_1, 2]
        #     threshold_OVA1_lower <- max(c(quantile_prediction_OVA1_temp_temp$x[class_1, 1], sort(quantile_prediction_OVA1_temp_temp$x[, 2], decreasing = T)[2]))
        #     threshold_OVA1_upper <- (ceiling(threshold_OVA1_upper*100) - ceiling(threshold_OVA1_upper*100) %% 2)/100
        #     threshold_OVA1_lower <- (ceiling(threshold_OVA1_lower*100) - ceiling(threshold_OVA1_lower*100) %% 2)/100
        #     
        #     quantile_prediction_OVA2_temp_temp <- aggregate(prediction_OVA2_temp_temp, by = list(data_train_y_x_outbag$Y), FUN = function(x) quantile(x, probs = c(0.05, 0.5,0.95)))
        #     threshold_OVA2_lower <- quantile_prediction_OVA2_temp_temp$x[class_2, 2]
        #     threshold_OVA2_upper <- min(c(quantile_prediction_OVA2_temp_temp$x[class_2, 3], sort(quantile_prediction_OVA2_temp_temp$x[, 2], decreasing = F)[2]))
        #     threshold_OVA2_upper <- (ceiling(threshold_OVA2_upper*100) - ceiling(threshold_OVA2_upper*100) %% 2)/100
        #     threshold_OVA2_lower <- (ceiling(threshold_OVA2_lower*100) - ceiling(threshold_OVA2_lower*100) %% 2)/100
        #     
        #     if((threshold_OVA1_upper<max(quantile_prediction_OVA1_temp_temp$x[, 2]))|(threshold_OVA2_lower>min(quantile_prediction_OVA2_temp_temp$x[, 2]))){
        #       abnormal_record_temp <- 1
        #       idx_grid_threshold_temp <- expand.grid(seq(threshold_OVA1_upper, threshold_OVA1_upper, 0.02), seq(threshold_OVA2_lower, threshold_OVA2_lower, 0.02))
        #     } else {
        #       abnormal_record_temp <- 0
        #       idx_grid_threshold_temp <- expand.grid(seq(threshold_OVA1_lower, threshold_OVA1_upper, 0.02), seq(threshold_OVA2_lower, threshold_OVA2_upper, 0.02))
        #     }
        #     
        #     cat("rows of idx_grid_threshold_temp   ", dim(idx_grid_threshold_temp)[1], "\n")
        #     auc_backward_c <- c()
        #     for (idx_j in 1:nrow(idx_grid_threshold_temp)) {
        #       # cat(idx_j, "\n")
        #       threshold_OVA1_temp <- idx_grid_threshold_temp[idx_j, 1]
        #       threshold_OVA2_temp <- idx_grid_threshold_temp[idx_j, 2]
        #       
        #       if(threshold_OVA1_temp<0){
        #         prediction_OVA1_temp_temp <- ifelse(prediction_OVA1_temp_temp<threshold_OVA1_temp, 0, prediction_OVA1_temp_temp-threshold_OVA1_temp)
        #       } else {
        #         prediction_OVA1_temp_temp <- ifelse(prediction_OVA1_temp_temp<threshold_OVA1_temp, 0, prediction_OVA1_temp_temp)
        #       }
        #       
        #       if(threshold_OVA2_temp>0){
        #         prediction_OVA2_temp_temp <- ifelse(prediction_OVA2_temp_temp>threshold_OVA2_temp, 0, prediction_OVA2_temp_temp-threshold_OVA2_temp)
        #       } else {
        #         prediction_OVA2_temp_temp <- ifelse(prediction_OVA2_temp_temp>threshold_OVA2_temp, 0, prediction_OVA2_temp_temp)
        #       }
        #       
        #       prediction_temp_temp <- prediction_OVA1_temp_temp + prediction_OVA2_temp_temp
        #       
        #       auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)
        #       
        #       auc_backward_c <- c(auc_backward_c, min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)))
        #       
        #     }
        #     idx_grid_threshold_temp <- cbind(idx_grid_threshold_temp, auc_backward_c)
        #     idx_grid_threshold_temp <- merge(idx_grid_threshold, idx_grid_threshold_temp, by=c("Var1", "Var2"), all.x = TRUE)
        #     
        #     # auc_backward_matrix <- cbind(auc_backward_matrix, idx_grid_threshold_temp$auc_backward_c)
        #     
        #     results_grid_threshold_temp <- list(auc_backward_matrix_temp=idx_grid_threshold_temp$auc_backward_c, abnormal_record_c=abnormal_record_temp)
        #     results_grid_threshold_temp
        #   }
        #   tictoc::toc()
        #   
        #   
        #   auc_backward_matrix <- do.call(cbind, lapply(results_grid_threshold, `[[`, 1))
        #   abnormal_record <- do.call(c, lapply(results_grid_threshold, `[[`, 2))
        #   
        #   idxx_optimal_temp <- which(auc_backward_matrix == max(auc_backward_matrix, na.rm = T), arr.ind = TRUE)
        #   idx_optimal_NoX <- idxx_optimal_temp[1, 2]
        #   idx_optimal_threshold <- idxx_optimal_temp[1, 1]
        #   
        #   names_x_select_OVA1 <- grid_lambda[idx_optimal_NoX, 1]
        #   names_x_select_OVA2 <- grid_lambda[idx_optimal_NoX, 2]
        #   
        #   threshold_OVA1 <- idx_grid_threshold[idx_optimal_threshold, 1]
        #   threshold_OVA2 <- idx_grid_threshold[idx_optimal_threshold, 2]
        #   
        #   row_i <- 1
        #   names_x_select_list_temp[[row_i]] <- list(names_x_select_OVA1=names_x_select_OVA1, names_x_select_OVA2=names_x_select_OVA2)
        #   threshold_list_temp[[row_i]] <- list(threshold_OVA1=threshold_OVA1, threshold_OVA2=threshold_OVA2)
        #   
        #   # all classes inbag
        #   ### prediction OVA1
        #   x_train_temp_inbag_OVA1 <- as.matrix(data_train_y_x_inbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, newx = x_train_temp_inbag_OVA1, s = names_x_select_OVA1, type = "response")
        #   prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, 1]
        #   ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
        #   prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
        #   prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
        #   
        #   ### prediction OVA2
        #   x_train_temp_inbag_OVA2 <- as.matrix(data_train_y_x_inbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, newx = x_train_temp_inbag_OVA2, s = names_x_select_OVA2, type = "response")
        #   prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, 1]
        #   ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
        #   prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
        #   prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
        #   
        #   prediction_AllClass_inbag_temp <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp)
        #   
        #   auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp)
        #   
        #   if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   } else {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   }
        #   
        #   
        #   # all classes outbag
        #   ### prediction OVA1
        #   x_train_temp_outbag_OVA1 <- as.matrix(data_train_y_x_outbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, newx = x_train_temp_outbag_OVA1, s = names_x_select_OVA1, type = "response")
        #   prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, 1]
        #   ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
        #   prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
        #   prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
        #   
        #   
        #   if(threshold_OVA1<0){
        #     prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
        #   } else {
        #     prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
        #   }
        #   
        #   ### prediction OVA2
        #   x_train_temp_outbag_OVA2 <- as.matrix(data_train_y_x_outbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
        #   prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, newx = x_train_temp_outbag_OVA2, s = names_x_select_OVA2, type = "response")
        #   prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, 1]
        #   ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
        #   prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
        #   prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
        #   
        # 
        #   if(threshold_OVA2>0){
        #     prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
        #   } else {
        #     prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
        #   }
        #   
        #   
        #   # prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
        #   prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp)
        #   
        #   auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_AllClass_outbag_temp)
        #   
        #   if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   } else {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(min(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   }
        #   
        #   row_i <- 1
        #   prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
        #   prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
        #   
        #   colnames(auc_temp_inbag) <- c("minimum_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        #   colnames(auc_temp_outbag) <- c("minimum_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        #   
        #   auc_parameters_grid <- cbind(auc_temp_inbag, auc_temp_outbag)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        #   
        # }
        # else if(model_i=="AdaBoost"){
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   
        #   row_i <- 1
        #   
        #   if(rfe_select){
        #     RFE_control <-  list(summary = caret::defaultSummary,
        #                          fit = function(x, y, first=TRUE, last=FALSE){
        #                            adabag::boosting(y~., data = cbind(y, x),
        #                                             boos = parameters_grid$boos_c[row_i],
        #                                             mfinal = parameters_grid$mfinal_c[row_i],
        #                                             coeflearn = "Breiman",    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #                                             control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                            maxdepth = parameters_grid$maxdepth_c[row_i]))
        #                          },
        #                          pred = function(object, x) {
        #                            prediction_temp <- predict(object, newdata=x)$prob
        #                            prediction_temp <- apply(prediction_temp, 1, which.max)
        #                            prediction_temp
        #                          },
        #                          rank = function(object, x, y) {
        #                            vimp <- object$importance
        #                            # vimp <- model_fit$importance
        #                            order_vimp <- order(abs(vimp), decreasing = TRUE)
        #                            vimp <- as.data.frame(vimp)
        #                            vimp$var <- rownames(vimp)
        #                            colnames(vimp) <- c("Overall", "var")
        #                            vimp <- vimp[order_vimp,]
        #                            vimp
        #                          },
        #                          selectSize = caret::pickSizeBest,
        #                          selectVar = caret::pickVars)
        #     
        #     ctrl <- caret::rfeControl(functions = RFE_control)
        #     #ctrl$returnResamp <- "all"
        #     set.seed(seed_rfe)
        #     rfProfile <- caret::rfe(x_rfe, y_rfe, sizes = subsets_rfe, rfeControl = ctrl)
        #     names_x_select <- rfProfile$optVariables
        #   } else if(select_backward_wilcoxon) {
        #     
        #     ###### OVA1
        #     prediction_OVA1_temp_temp_matrix <- c()
        #     for (subset_i in seq_len(length(subsets_backward))) {
        #       cat(subset_i, "\n")
        #       names_x_select_OVA1_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #       names_x_select_OVA1_temp <- c(names_x_select_OVA1_temp, names_x_fixed)
        #       modelformula_OVA1_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1_temp, collapse = "+")))
        #       
        #       model_fit_OVA1_temp <-  caret::train(modelformula_OVA1_temp, data = data_train_y_x_inbag_OVA1, 
        #                                            method = "AdaBoost.M1"    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #       )
        #       
        #       prediction_OVA1_temp_temp <- predict(model_fit_OVA1_temp, newdata=data_train_y_x_outbag)
        #       prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp[, 2]
        #       ratio_OVA1_temp_temp <- 1/(max(prediction_OVA1_temp_temp) - min(prediction_OVA1_temp_temp))
        #       prediction_OVA1_temp_temp <- (prediction_OVA1_temp_temp - min(prediction_OVA1_temp_temp)) * ratio_OVA1_temp_temp
        #       prediction_OVA1_temp_temp <- -1 + prediction_OVA1_temp_temp*2
        #       
        #       prediction_OVA1_temp_temp_matrix <- cbind(prediction_OVA1_temp_temp_matrix, prediction_OVA1_temp_temp)
        #     }
        #     
        #     ###### OVA2
        #     prediction_OVA2_temp_temp_matrix <- c()
        #     for (subset_i in seq_len(length(subsets_backward))) {
        #       cat(subset_i, "\n")
        #       names_x_select_OVA2_temp <- as.character(results_Wilcoxon_OVA2$names_x[1:subsets_backward[subset_i]])
        #       names_x_select_OVA2_temp <- c(names_x_select_OVA2_temp, names_x_fixed)
        #       modelformula_OVA2_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2_temp, collapse = "+")))
        #       
        #       model_fit_OVA2_temp <-  caret::train(modelformula_OVA2_temp, data = data_train_y_x_inbag_OVA2, 
        #                                            method = "AdaBoost.M1"    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #       )
        #       
        #       prediction_OVA2_temp_temp <- predict(model_fit_OVA2_temp, newdata=data_train_y_x_outbag)
        #       prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp[, 2]
        #       ratio_OVA2_temp_temp <- 1/(max(prediction_OVA2_temp_temp) - min(prediction_OVA2_temp_temp))
        #       prediction_OVA2_temp_temp <- (prediction_OVA2_temp_temp - min(prediction_OVA2_temp_temp)) * ratio_OVA2_temp_temp
        #       prediction_OVA2_temp_temp <- -1 + prediction_OVA2_temp_temp*2
        #       
        #       prediction_OVA2_temp_temp_matrix <- cbind(prediction_OVA2_temp_temp_matrix, prediction_OVA2_temp_temp)
        #     }
        #     
        #     auc_backward <- c()
        #     
        #     idx_grid_NoX <- expand.grid(1:length(subsets_backward), 1:length(subsets_backward))
        #     for (idx_i in 1:nrow(idx_grid_NoX)) {
        #       
        #       idx_NoX_OVA1_temp <- idx_grid_NoX[idx_i, 1]
        #       idx_NoX_OVA2_temp <- idx_grid_NoX[idx_i, 2]
        #       
        #       prediction_temp_temp <- (prediction_OVA1_temp_temp_matrix[, idx_NoX_OVA1_temp] + prediction_OVA2_temp_temp_matrix[, idx_NoX_OVA2_temp])/2
        #       
        #       auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)
        #       
        #       auc_backward <- c(auc_backward, auc_temp$auc_binary)
        #       
        #     }
        #     
        #     names_x_select_OVA1 <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[idx_grid_NoX[which.max(auc_backward), 1]]]
        #     names_x_select_OVA1 <- c(as.character(names_x_select_OVA1), names_x_fixed)
        #     
        #     names_x_select_OVA2 <- results_Wilcoxon_OVA2$names_x[1:subsets_backward[idx_grid_NoX[which.max(auc_backward), 1]]]
        #     names_x_select_OVA2 <- c(as.character(names_x_select_OVA2), names_x_fixed)
        #     
        #   } else {
        #     names_x_select_OVA1 <- names_x
        #     names_x_select_OVA2 <- names_x
        #   }
        #   
        #   names_x_select_list_temp[[row_i]] <- list(names_x_select_OVA1=names_x_select_OVA1, names_x_select_OVA2=names_x_select_OVA2)
        #   
        #   ### model OVA1
        #   modelformula_OVA1 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1, collapse = "+")))
        #   model_fit_OVA1 <-  caret::train(modelformula_OVA1, data = data_train_y_x_inbag_OVA1, 
        #                                   method = "AdaBoost.M1"    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #   )
        #   
        #   for(tree_i in seq_len(length(model_fit_OVA1$trees))){
        #     attr(model_fit_OVA1$trees[[tree_i]]$terms, ".Environment") = NULL
        #   }
        #   attr(model_fit_OVA1$formula, ".Environment") <- NULL
        #   attr(model_fit_OVA1$terms, ".Environment") <- NULL
        #   
        #   ### model OVA2
        #   modelformula_OVA2 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2, collapse = "+")))
        #   model_fit_OVA2 <-  caret::train(modelformula_OVA2, data = data_train_y_x_inbag_OVA2, 
        #                                   method = "AdaBoost.M1"    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #   )
        #   
        #   for(tree_i in seq_len(length(model_fit_OVA2$trees))){
        #     attr(model_fit_OVA2$trees[[tree_i]]$terms, ".Environment") = NULL
        #   }
        #   attr(model_fit_OVA2$formula, ".Environment") <- NULL
        #   attr(model_fit_OVA2$terms, ".Environment") <- NULL
        #   
        #   model_fit_list_temp[[row_i]] <- list(model_fit_OVA1=model_fit_OVA1, model_fit_OVA2=model_fit_OVA2)
        #   
        #   
        #   # all classes inbag
        #   ### prediction OVA1
        #   prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, newdata=data_train_y_x_inbag)
        #   prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, 2]
        #   ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
        #   prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
        #   prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
        #   
        #   ### prediction OVA2
        #   prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, newdata=data_train_y_x_inbag)
        #   prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, 2]
        #   ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
        #   prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
        #   prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
        #   
        #   prediction_AllClass_inbag_temp <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp) / 2
        #   
        #   auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp)
        #   
        #   if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   } else {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   }
        #   
        #   
        #   # all classes outbag
        #   ### prediction OVA1
        #   prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, newdata=data_train_y_x_outbag)
        #   prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, 2]
        #   ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
        #   prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
        #   prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
        #   
        #   ### prediction OVA2
        #   prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, newdata=data_train_y_x_outbag)
        #   prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, 2]
        #   ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
        #   prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
        #   prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
        #   
        #   prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
        #   
        #   auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_AllClass_outbag_temp)
        #   
        #   if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   } else {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #   }
        #   
        #   prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
        #   prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
        #   
        #   
        #   colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        #   colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        #   
        #   auc_parameters_grid <- cbind(auc_temp_inbag, auc_temp_outbag)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        #   
        #   # tictoc::toc()
        #   
        # } 
        # else if(model_i=="AdaBoost_adabag"){
        #   library(adabag)
        #   # tictoc::tic()
        #   # boos_c <- c(TRUE, FALSE)
        #   boos_c <- c(TRUE)
        #   # mfinal_c <- seq(50, 150, 30)
        #   mfinal_c <- c(50, 150)
        #   minsplit_c <- c(5, 10)
        #   maxdepth_c <- c(5, 30)
        #   
        #   
        #   #          mfinal_c <- c(50)
        #   #          minsplit_c <- c(5)
        #   #          maxdepth_c <- c(5)
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   parameters_grid <- expand.grid(boos_c=boos_c, mfinal_c=mfinal_c, minsplit_c=minsplit_c, maxdepth_c=maxdepth_c)
        #   # a <- Sys.time()
        #   # auc_temp <- c()
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   for (row_i in 1:nrow(parameters_grid)) {
        #     cat(row_i, "\n")
        #     
        #     if(rfe_select){
        #       RFE_control <-  list(summary = caret::defaultSummary,
        #                            fit = function(x, y, first=TRUE, last=FALSE){
        #                              adabag::boosting(y~., data = cbind(y, x),
        #                                               boos = parameters_grid$boos_c[row_i],
        #                                               mfinal = parameters_grid$mfinal_c[row_i],
        #                                               coeflearn = "Breiman",    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #                                               control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                              maxdepth = parameters_grid$maxdepth_c[row_i]))
        #                            },
        #                            pred = function(object, x) {
        #                              prediction_temp <- predict(object, newdata=x)$prob
        #                              prediction_temp <- apply(prediction_temp, 1, which.max)
        #                              prediction_temp
        #                            },
        #                            rank = function(object, x, y) {
        #                              vimp <- object$importance
        #                              # vimp <- model_fit$importance
        #                              order_vimp <- order(abs(vimp), decreasing = TRUE)
        #                              vimp <- as.data.frame(vimp)
        #                              vimp$var <- rownames(vimp)
        #                              colnames(vimp) <- c("Overall", "var")
        #                              vimp <- vimp[order_vimp,]
        #                              vimp
        #                            },
        #                            selectSize = caret::pickSizeBest,
        #                            selectVar = caret::pickVars)
        #       
        #       ctrl <- caret::rfeControl(functions = RFE_control)
        #       #ctrl$returnResamp <- "all"
        #       set.seed(seed_rfe)
        #       rfProfile <- caret::rfe(x_rfe, y_rfe, sizes = subsets_rfe, rfeControl = ctrl)
        #       names_x_select <- rfProfile$optVariables
        #     } else if(select_backward_wilcoxon) {
        #       
        #       ###### OVA1
        #       prediction_OVA1_temp_temp_matrix <- c()
        #       for (subset_i in seq_len(length(subsets_backward))) {
        #         cat(subset_i, "\n")
        #         names_x_select_OVA1_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #         names_x_select_OVA1_temp <- c(names_x_select_OVA1_temp, names_x_fixed)
        #         modelformula_OVA1_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1_temp, collapse = "+")))
        #         
        #         model_fit_OVA1_temp <-  adabag::boosting(modelformula_OVA1_temp, data = data_train_y_x_inbag_OVA1,
        #                                                  boos = parameters_grid$boos_c[row_i],
        #                                                  mfinal = parameters_grid$mfinal_c[row_i],
        #                                                  coeflearn = "Breiman",    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #                                                  control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                                 maxdepth = parameters_grid$maxdepth_c[row_i]))
        #         
        #         prediction_OVA1_temp_temp <- predict(model_fit_OVA1_temp, newdata=data_train_y_x_outbag)$prob
        #         prediction_OVA1_temp_temp <- prediction_OVA1_temp_temp[, 2]
        #         ratio_OVA1_temp_temp <- 1/(max(prediction_OVA1_temp_temp) - min(prediction_OVA1_temp_temp))
        #         prediction_OVA1_temp_temp <- (prediction_OVA1_temp_temp - min(prediction_OVA1_temp_temp)) * ratio_OVA1_temp_temp
        #         prediction_OVA1_temp_temp <- -1 + prediction_OVA1_temp_temp*2
        #         
        #         prediction_OVA1_temp_temp_matrix <- cbind(prediction_OVA1_temp_temp_matrix, prediction_OVA1_temp_temp)
        #       }
        #       
        #       ###### OVA2
        #       prediction_OVA2_temp_temp_matrix <- c()
        #       for (subset_i in seq_len(length(subsets_backward))) {
        #         cat(subset_i, "\n")
        #         names_x_select_OVA2_temp <- as.character(results_Wilcoxon_OVA2$names_x[1:subsets_backward[subset_i]])
        #         names_x_select_OVA2_temp <- c(names_x_select_OVA2_temp, names_x_fixed)
        #         modelformula_OVA2_temp <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2_temp, collapse = "+")))
        #         
        #         model_fit_OVA2_temp <-  adabag::boosting(modelformula_OVA2_temp, data = data_train_y_x_inbag_OVA2,
        #                                                  boos = parameters_grid$boos_c[row_i],
        #                                                  mfinal = parameters_grid$mfinal_c[row_i],
        #                                                  coeflearn = "Breiman",    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #                                                  control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                                 maxdepth = parameters_grid$maxdepth_c[row_i]))
        #         
        #         prediction_OVA2_temp_temp <- predict(model_fit_OVA2_temp, newdata=data_train_y_x_outbag)$prob
        #         prediction_OVA2_temp_temp <- prediction_OVA2_temp_temp[, 2]
        #         ratio_OVA2_temp_temp <- 1/(max(prediction_OVA2_temp_temp) - min(prediction_OVA2_temp_temp))
        #         prediction_OVA2_temp_temp <- (prediction_OVA2_temp_temp - min(prediction_OVA2_temp_temp)) * ratio_OVA2_temp_temp
        #         prediction_OVA2_temp_temp <- -1 + prediction_OVA2_temp_temp*2
        #         
        #         prediction_OVA2_temp_temp_matrix <- cbind(prediction_OVA2_temp_temp_matrix, prediction_OVA2_temp_temp)
        #       }
        #       
        #       auc_backward <- c()
        #       
        #       idx_grid_NoX <- expand.grid(1:length(subsets_backward), 1:length(subsets_backward))
        #       for (idx_i in 1:nrow(idx_grid_NoX)) {
        #         
        #         idx_NoX_OVA1_temp <- idx_grid_NoX[idx_i, 1]
        #         idx_NoX_OVA2_temp <- idx_grid_NoX[idx_i, 2]
        #         
        #         prediction_temp_temp <- (prediction_OVA1_temp_temp_matrix[, idx_NoX_OVA1_temp] + prediction_OVA2_temp_temp_matrix[, idx_NoX_OVA2_temp])/2
        #         
        #         auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_temp_temp)
        #         
        #         auc_backward <- c(auc_backward, auc_temp$auc_binary)
        #         
        #       }
        #       
        #       names_x_select_OVA1 <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[idx_grid_NoX[which.max(auc_backward), 1]]]
        #       names_x_select_OVA1 <- c(as.character(names_x_select_OVA1), names_x_fixed)
        #       
        #       names_x_select_OVA2 <- results_Wilcoxon_OVA2$names_x[1:subsets_backward[idx_grid_NoX[which.max(auc_backward), 1]]]
        #       names_x_select_OVA2 <- c(as.character(names_x_select_OVA2), names_x_fixed)
        #       
        #     } else {
        #       names_x_select_OVA1 <- names_x
        #       names_x_select_OVA2 <- names_x
        #     }
        #     
        #     names_x_select_list_temp[[row_i]] <- list(names_x_select_OVA1=names_x_select_OVA1, names_x_select_OVA2=names_x_select_OVA2)
        #     
        #     ### model OVA1
        #     modelformula_OVA1 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA1, collapse = "+")))
        #     model_fit_OVA1 <-  adabag::boosting(modelformula_OVA1, data = data_train_y_x_inbag_OVA1,
        #                                         boos = parameters_grid$boos_c[row_i],
        #                                         mfinal = parameters_grid$mfinal_c[row_i],
        #                                         coeflearn = "Breiman",    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #                                         control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                        maxdepth = parameters_grid$maxdepth_c[row_i]))
        #     
        #     for(tree_i in seq_len(length(model_fit_OVA1$trees))){
        #       attr(model_fit_OVA1$trees[[tree_i]]$terms, ".Environment") = NULL
        #     }
        #     attr(model_fit_OVA1$formula, ".Environment") <- NULL
        #     attr(model_fit_OVA1$terms, ".Environment") <- NULL
        #     
        #     ### model OVA2
        #     modelformula_OVA2 <- as.formula(paste(name_y_factor, "~", paste(names_x_select_OVA2, collapse = "+")))
        #     model_fit_OVA2 <-  adabag::boosting(modelformula_OVA2, data = data_train_y_x_inbag_OVA2,
        #                                         boos = parameters_grid$boos_c[row_i],
        #                                         mfinal = parameters_grid$mfinal_c[row_i],
        #                                         coeflearn = "Breiman",    ### different coeflean correspond to different kind of adaboost. see details in help document.
        #                                         control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                        maxdepth = parameters_grid$maxdepth_c[row_i]))
        #     
        #     for(tree_i in seq_len(length(model_fit_OVA2$trees))){
        #       attr(model_fit_OVA2$trees[[tree_i]]$terms, ".Environment") = NULL
        #     }
        #     attr(model_fit_OVA2$formula, ".Environment") <- NULL
        #     attr(model_fit_OVA2$terms, ".Environment") <- NULL
        #     
        #     model_fit_list_temp[[row_i]] <- list(model_fit_OVA1=model_fit_OVA1, model_fit_OVA2=model_fit_OVA2)
        #     
        #     
        #     # all classes inbag
        #     ### prediction OVA1
        #     prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, newdata=data_train_y_x_inbag)$prob
        #     prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, 2]
        #     ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
        #     prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
        #     prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
        #     
        #     ### prediction OVA2
        #     prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, newdata=data_train_y_x_inbag)$prob
        #     prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, 2]
        #     ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
        #     prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
        #     prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
        #     
        #     prediction_AllClass_inbag_temp <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp) / 2
        #     
        #     auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_inbag$Y, prediction_All_classes=prediction_AllClass_inbag_temp)
        #     
        #     if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
        #       auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #     } else {
        #       auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #     }
        #     
        #     
        #     # all classes outbag
        #     ### prediction OVA1
        #     prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, newdata=data_train_y_x_outbag)$prob
        #     prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, 2]
        #     ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
        #     prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
        #     prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
        #     
        #     ### prediction OVA2
        #     prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, newdata=data_train_y_x_outbag)$prob
        #     prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, 2]
        #     ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
        #     prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
        #     prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
        #     
        #     prediction_AllClass_outbag_temp <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
        #     
        #     auc_temp <- auc_calculator(class_1=class_1, class_2=class_2, label_All_classes=data_train_y_x_outbag$Y, prediction_All_classes=prediction_AllClass_outbag_temp)
        #     
        #     if(any(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2) < 0.5)) {
        #       auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #     } else {
        #       auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2)), auc_temp$auc_binary, auc_temp$auc_others_1, auc_temp$auc_others_2))
        #     }
        #     
        #     prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
        #     prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
        #     
        #   }
        #   
        #   colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        #   colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        #   
        #   auc_parameters_grid <- cbind(parameters_grid, cbind(auc_temp_inbag, auc_temp_outbag))
        #   
        #   # tictoc::toc()
        #   
        # } 
        # else if(model_i=="naivebayes"){
        #   library(naivebayes)
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   if(select_backward_wilcoxon) {
        #     
        #     auc_backward <- c()
        #     for (subset_i in seq_len(length(subsets_backward))) {
        #       # cat(subset_i, "\n")
        #       names_x_select_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #       names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
        #       modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
        #       model_fit_temp <-  naivebayes::naive_bayes(modelformula, data = data_train_y_x_inbag_binary)
        # 
        #       prediction_temp_temp <- predict(model_fit_temp, newdata=data_train_y_x_outbag, type = "prob")
        #       prediction_temp_temp <- -1 + prediction_temp_temp[, 2]*2
        #       
        #       class_binary <- c(class_1, class_2)
        #       class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #       class_others <- as.numeric(as.character(class_others))
        #       
        #       auc_others_1 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_1 <- c(auc_others_1, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       auc_others_2 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_2 <- c(auc_others_2, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #       Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #       Y_temp <- factor(Y_temp)
        #       auc_binary_temp <- pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc
        #       
        #       if(any(c(auc_binary_temp, auc_others_1, auc_others_2) < 0.5)) {
        #         auc_backward[subset_i] <- 0.5
        #       } else {
        #         auc_backward[subset_i] <- mean(c(auc_binary_temp, auc_others_1, auc_others_2))
        #       }
        #       
        #     }
        #     names_x_select <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[which.max(auc_backward)]]
        #     names_x_select <- c(as.character(names_x_select), names_x_fixed)
        #     
        #   } else {
        #     names_x_select <- names_x
        #   }
        #   
        #   row_i <- 1
        #   names_x_select_list_temp[[row_i]] <- names_x_select
        #   
        #   
        #   modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
        #   model_fit <-  naivebayes::naive_bayes(modelformula, data = data_train_y_x_inbag_binary)
        #   model_fit_list_temp[[1]] <- model_fit
        #   
        #   # prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
        #   # prediction_temp <- -1 + prediction_temp[, 2]*2
        #   # auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #   # auc_parameters_grid <- as.data.frame(auc_temp)
        #   
        #   # all classes inbag
        #   prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
        #   prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
        #   prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_inbag$Y)[!is.element(unique(data_train_y_x_inbag$Y), class_binary)]
        #   class_others <- as.numeric(as.character(class_others))
        #   
        #   auc_others_1_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_inbag <- c(auc_others_1_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_inbag <- c(auc_others_2_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_inbag <- pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag) < 0.5)) {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   } else {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_1_inbag)), auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   }
        #   
        #   # all classes outbag
        #   prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
        #   prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
        #   prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #   class_others <- sort(as.numeric(as.character(class_others)))
        #   
        #   auc_others_1_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_outbag <- c(auc_others_1_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_outbag <- c(auc_others_2_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_outbag <- pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag) < 0.5)) {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   } else {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag)), auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   }
        # 
        # colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        # colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        # 
        # auc_parameters_grid <- cbind(auc_temp_inbag, auc_temp_outbag)
        # auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        # 
        # } 
        # else if(model_i=="C50"){
        #   library(C50)
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   if(select_backward_wilcoxon) {
        #     
        #     auc_backward <- c()
        #     for (subset_i in seq_len(length(subsets_backward))) {
        #       # cat(subset_i, "\n")
        #       names_x_select_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #       names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
        # 
        #       x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x_select_temp])
        #       y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_factor]
        #       
        #       set.seed(318)
        #       model_fit_temp <- C50::C5.0(x_train_binary_temp_inbag, y_train_binary_temp_inbag)
        #       
        #       x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x_select_temp])
        #       prediction_temp_temp <- predict(model_fit_temp, newdata = x_train_temp_outbag, type = "prob")
        #       prediction_temp_temp <- -1 + prediction_temp_temp[, 2]*2
        #       
        #       class_binary <- c(class_1, class_2)
        #       class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #       class_others <- as.numeric(as.character(class_others))
        #       
        #       auc_others_1 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_1 <- c(auc_others_1, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       auc_others_2 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_2 <- c(auc_others_2, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #       Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #       Y_temp <- factor(Y_temp)
        #       auc_binary_temp <- pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc
        #       
        #       if(any(c(auc_binary_temp, auc_others_1, auc_others_2) < 0.5)) {
        #         auc_backward[subset_i] <- 0.5
        #       } else {
        #         auc_backward[subset_i] <- mean(c(auc_binary_temp, auc_others_1, auc_others_2))
        #       }
        #       
        #     }
        #     names_x_select <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[which.max(auc_backward)]]
        #     names_x_select <- c(as.character(names_x_select), names_x_fixed)
        #     
        #   } else {
        #     names_x_select <- names_x
        #   }
        #   
        #   row_i <- 1
        #   names_x_select_list_temp[[row_i]] <- names_x_select
        #   
        #   x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x_select])
        #   y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_factor]
        #   
        #   set.seed(318)
        #   model_fit <- C50::C5.0(x_train_binary_temp_inbag, y_train_binary_temp_inbag)
        #   model_fit_list_temp[[1]] <- model_fit
        #   
        #   # x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
        #   # y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_factor]
        #   # prediction_temp <- predict(model_fit, newdata = x_train_binary_temp_outbag, type = "prob")
        #   # prediction_temp <- -1 + prediction_temp[, 2]*2
        #   # auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #   # auc_parameters_grid <- as.data.frame(auc_temp)
        #   
        #   # all classes inbag
        #   x_train_temp_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
        #   prediction_AllClass_inbag_temp <- predict(model_fit, newdata = x_train_temp_inbag, type = "prob")
        #   prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
        #   prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_inbag$Y)[!is.element(unique(data_train_y_x_inbag$Y), class_binary)]
        #   class_others <- as.numeric(as.character(class_others))
        #   
        #   auc_others_1_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_inbag <- c(auc_others_1_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_inbag <- c(auc_others_2_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_inbag <- pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag) < 0.5)) {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   } else {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_1_inbag)), auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   }
        #   
        #   
        #   # all classes outbag
        #   x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
        #   prediction_AllClass_outbag_temp <- predict(model_fit, newdata = x_train_temp_outbag, type = "prob")
        #   prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
        #   prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #   class_others <- sort(as.numeric(as.character(class_others)))
        #   
        #   auc_others_1_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_outbag <- c(auc_others_1_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_outbag <- c(auc_others_2_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_outbag <- pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag) < 0.5)) {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   } else {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag)), auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   }
        #   
        #   colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        #   colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        #   
        #   auc_parameters_grid <- cbind(auc_temp_inbag, auc_temp_outbag)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        #   
        # } 
        # else if(model_i=="ctree"){
        #   library(party)
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   if(select_backward_wilcoxon) {
        #     
        #     auc_backward <- c()
        #     for (subset_i in seq_len(length(subsets_backward))) {
        #       # cat(subset_i, "\n")
        #       names_x_select_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #       names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
        #       modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
        #       model_fit_temp <-  party::ctree(modelformula, data = data_train_y_x_inbag_binary)
        #       
        #       prediction_temp_temp <- predict(model_fit_temp, newdata=data_train_y_x_outbag, type = "prob")
        #       prediction_temp_temp <- do.call(rbind, prediction_temp_temp)
        #       prediction_temp_temp <- -1 + prediction_temp_temp[, 2]*2
        #       
        #       class_binary <- c(class_1, class_2)
        #       class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #       class_others <- as.numeric(as.character(class_others))
        #       
        #       auc_others_1 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_1 <- c(auc_others_1, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       auc_others_2 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_2 <- c(auc_others_2, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #       Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #       Y_temp <- factor(Y_temp)
        #       auc_binary_temp <- pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc
        #       
        #       if(any(c(auc_binary_temp, auc_others_1, auc_others_2) < 0.5)) {
        #         auc_backward[subset_i] <- 0.5
        #       } else {
        #         auc_backward[subset_i] <- mean(c(auc_binary_temp, auc_others_1, auc_others_2))
        #       }
        #       
        #     }
        #     names_x_select <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[which.max(auc_backward)]]
        #     names_x_select <- c(as.character(names_x_select), names_x_fixed)
        #     
        #   } else {
        #     names_x_select <- names_x
        #   }
        #   
        #   row_i <- 1
        #   names_x_select_list_temp[[row_i]] <- names_x_select
        #   
        #   
        #   modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
        #   model_fit <-  party::ctree(modelformula, data = data_train_y_x_inbag_binary)
        #   model_fit_list_temp[[1]] <- model_fit
        #   
        #   # prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
        #   # prediction_temp <- do.call(rbind, prediction_temp)
        #   # prediction_temp <- -1 + prediction_temp[, 2]*2
        #   # auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #   # auc_parameters_grid <- as.data.frame(auc_temp)
        #   
        #   # all classes inbag
        #   prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
        #   prediction_AllClass_inbag_temp <- do.call(rbind, prediction_AllClass_inbag_temp)
        #   prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
        #   prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_inbag$Y)[!is.element(unique(data_train_y_x_inbag$Y), class_binary)]
        #   class_others <- as.numeric(as.character(class_others))
        #   
        #   auc_others_1_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_inbag <- c(auc_others_1_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_inbag <- c(auc_others_2_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_inbag <- pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag) < 0.5)) {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   } else {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_1_inbag)), auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   }
        #   
        #   
        #   # all classes outbag
        #   prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
        #   prediction_AllClass_outbag_temp <- do.call(rbind, prediction_AllClass_outbag_temp)
        #   prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
        #   prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #   class_others <- sort(as.numeric(as.character(class_others)))
        #   
        #   auc_others_1_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_outbag <- c(auc_others_1_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_outbag <- c(auc_others_2_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_outbag <- pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag) < 0.5)) {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   } else {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag)), auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   }
        # 
        # colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        # colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        # 
        # auc_parameters_grid <- cbind(auc_temp_inbag, auc_temp_outbag)
        # auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        #   
        # } 
        else if(model_i=="RLT"){
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
          
        } 
        # else if(model_i=="wsrf"){
        #   library(wsrf)
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   if(rfe_select){
        #     RFE_control <-  list(summary = caret::defaultSummary,
        #                          fit = function(x, y, first=TRUE, last=FALSE){
        #                            wsrf::wsrf(x=x, y=y, parallel=FALSE)
        #                          },
        #                          pred = function(object, x) {
        #                            prediction_temp <- predict(object, newdata=x, type = "prob")
        #                            # prediction_temp <- predict(model_fit, newdata=x_rfe, type = "prob")
        #                            prediction_temp <- prediction_temp$prob
        #                            prediction_temp <- apply(prediction_temp, 1, which.max) - 1 
        #                            prediction_temp
        #                          },
        #                          rank = function(object, x, y) {
        #                            vimp <- wsrf::importance(object)
        #                            # vimp <- wsrf::importance(model_fit)
        #                            order_vimp <- order(abs(vimp), decreasing = TRUE)
        #                            vimp <- as.data.frame(vimp)
        #                            vimp$var <- rownames(vimp)
        #                            colnames(vimp) <- c("Overall", "var")
        #                            vimp <- vimp[order_vimp,]
        #                            vimp
        #                          },
        #                          selectSize = caret::pickSizeBest,
        #                          selectVar = caret::pickVars)
        #     
        #     ctrl <- caret::rfeControl(functions = RFE_control)
        #     ctrl$returnResamp <- "all"
        #     set.seed(seed_rfe)
        #     rfProfile <- caret::rfe(x_rfe, y_rfe, sizes = subsets_rfe, rfeControl = ctrl)
        #     names_x_select <- rfProfile$optVariables
        #   } else if(select_backward_wilcoxon) {
        #     
        #     auc_backward <- c()
        #     for (subset_i in seq_len(length(subsets_backward))) {
        #       # cat(subset_i, "\n")
        #       names_x_select_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #       names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
        #       modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
        #       model_fit_temp <-  wsrf::wsrf(modelformula, data = data_train_y_x_inbag_binary, parallel=FALSE)
        #       
        #       prediction_temp_temp <- predict(model_fit_temp, newdata=data_train_y_x_outbag, type = "prob")
        #       prediction_temp_temp <- prediction_temp_temp$prob
        #       prediction_temp_temp <- -1 + prediction_temp_temp[, 2]*2
        #       
        #       class_binary <- c(class_1, class_2)
        #       class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #       class_others <- as.numeric(as.character(class_others))
        #       
        #       auc_others_1 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_1 <- c(auc_others_1, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       auc_others_2 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_2 <- c(auc_others_2, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #       Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #       Y_temp <- factor(Y_temp)
        #       auc_binary_temp <- pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc
        #       
        #       if(any(c(auc_binary_temp, auc_others_1, auc_others_2) < 0.5)) {
        #         auc_backward[subset_i] <- 0.5
        #       } else {
        #         auc_backward[subset_i] <- mean(c(auc_binary_temp, auc_others_1, auc_others_2))
        #       }
        #       
        #     }
        #     names_x_select <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[which.max(auc_backward)]]
        #     names_x_select <- c(as.character(names_x_select), names_x_fixed)
        #     
        #   } else {
        #     names_x_select <- names_x
        #   }
        # 
        #   names_x_select_list_temp[[row_i]] <- names_x_select
        #   
        #   # modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
        #   modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select, collapse = "+")))
        #   model_fit <-  wsrf::wsrf(modelformula, data = data_train_y_x_inbag_binary, parallel=FALSE)
        #   model_fit_list_temp[[1]] <- model_fit
        #   
        #   # prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
        #   # prediction_temp <- prediction_temp$prob
        #   # prediction_temp <- -1 + prediction_temp[, 2]*2
        #   # auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #   # auc_parameters_grid <- as.data.frame(auc_temp)
        #   
        #   # all classes inbag
        #   prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
        #   prediction_AllClass_inbag_temp <- prediction_AllClass_inbag_temp$prob
        #   prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
        #   prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_inbag$Y)[!is.element(unique(data_train_y_x_inbag$Y), class_binary)]
        #   class_others <- as.numeric(as.character(class_others))
        #   
        #   auc_others_1_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_inbag <- c(auc_others_1_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_inbag <- c(auc_others_2_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_inbag <- pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag) < 0.5)) {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   } else {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_1_inbag)), auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   }
        #   
        #   
        #   # all classes outbag
        #   prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
        #   prediction_AllClass_outbag_temp <- prediction_AllClass_outbag_temp$prob
        #   prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
        #   prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #   class_others <- sort(as.numeric(as.character(class_others)))
        #   
        #   auc_others_1_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_outbag <- c(auc_others_1_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_outbag <- c(auc_others_2_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_outbag <- pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag) < 0.5)) {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   } else {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag)), auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   }
        # 
        # 
        #   colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        #   colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        #   
        #   auc_parameters_grid <- cbind(auc_temp_inbag, auc_temp_outbag)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        # 
        #   
        # } 
        # else if(model_i=="KNNreg"){
        #   library(caret)
        # 
        #   x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x])
        #   y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_numerical]
        # 
        #   k_c <- seq(-10, 10, 2) + sqrt(nrow(x_train_binary_temp_inbag))
        #   k_c <- k_c[k_c >= 2]
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   parameters_grid <- as.data.frame(k_c)
        #   auc_temp <- c()
        #   for(row_i in 1:nrow(parameters_grid)){
        #     model_fit <- caret::knnreg(x_train_binary_temp_inbag, y_train_binary_temp_inbag, k=sqrt(nrow(x_train_binary_temp_inbag)))
        #     model_fit_list_temp[[row_i]] <- model_fit
        #     
        #     x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
        #     y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_numerical]
        #     prediction_temp <- predict(model_fit, newdata = x_train_binary_temp_outbag)
        #     auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #     
        #     x_train_temp_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
        #     prediction_AllClass_inbag_temp <- predict(model_fit, newdata = x_train_temp_inbag)
        #     prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
        #     
        #     x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
        #     prediction_AllClass_outbag_temp <- predict(model_fit, newdata = x_train_temp_outbag)
        #     prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
        #   }
        #   auc_parameters_grid <- cbind(auc_temp, parameters_grid)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        # 
        # 
        # } 
        # else if(model_i=="KNNclassification"){
        #   library(caret)
        #   
        #   x_train_binary_temp_inbag <- as.matrix(data_train_y_x_inbag_binary[, names_x])
        #   y_train_binary_temp_inbag <- data_train_y_x_inbag_binary[, name_y_factor]
        #   
        #   k_c <- seq(-10, 10, 2) + sqrt(nrow(x_train_binary_temp_inbag))
        #   k_c <- k_c[k_c >= 2]
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   parameters_grid <- as.data.frame(k_c)
        #   auc_temp <- c()
        #   for(row_i in 1:nrow(parameters_grid)){
        #     model_fit <- caret::knn3(x_train_binary_temp_inbag, y_train_binary_temp_inbag, k=sqrt(nrow(x_train_binary_temp_inbag)))
        #     model_fit_list_temp[[row_i]] <- model_fit
        #     
        #     x_train_binary_temp_outbag <- as.matrix(data_train_y_x_outbag_binary[, names_x])
        #     y_train_binary_temp_outbag <- data_train_y_x_outbag_binary[, name_y_factor]
        #     prediction_temp <- predict(model_fit, newdata = x_train_binary_temp_outbag)
        #     prediction_temp <- -1 + prediction_temp[, 2]*2
        #     auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #     
        #     x_train_temp_inbag <- as.matrix(data_train_y_x_inbag[, names_x])
        #     prediction_AllClass_inbag_temp <- predict(model_fit, newdata = x_train_temp_inbag)
        #     prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
        #     prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
        #     
        #     x_train_temp_outbag <- as.matrix(data_train_y_x_outbag[, names_x])
        #     prediction_AllClass_outbag_temp <- predict(model_fit, newdata = x_train_temp_outbag)
        #     prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
        #     prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
        #   }
        #   auc_parameters_grid <- cbind(auc_temp, parameters_grid)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        #   
        #   
        # } 
        # else if(model_i=="C4.5"){
        #   # load the package
        #   library(RWeka)
        #   
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   if(select_backward_wilcoxon) {
        #     
        #     auc_backward <- c()
        #     for (subset_i in seq_len(length(subsets_backward))) {
        #       # cat(subset_i, "\n")
        #       names_x_select_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #       names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
        #       
        #       modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
        #       model_fit_temp <-  RWeka::J48(modelformula, data = data_train_y_x_inbag_binary)
        #       
        #       prediction_temp_temp <- predict(model_fit_temp, newdata=data_train_y_x_outbag, type = "probability")
        #       prediction_temp_temp <- -1 + prediction_temp_temp[, 2]*2
        #       
        #       
        #       class_binary <- c(class_1, class_2)
        #       class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #       class_others <- as.numeric(as.character(class_others))
        #       
        #       auc_others_1 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_1 <- c(auc_others_1, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       auc_others_2 <- c()
        #       for (class_others_i in class_others) {
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #         Y_temp <- factor(Y_temp)
        #         auc_others_2 <- c(auc_others_2, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #       }
        #       
        #       idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #       Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #       Y_temp <- factor(Y_temp)
        #       auc_binary_temp <- pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc
        #       
        #       if(any(c(auc_binary_temp, auc_others_1, auc_others_2) < 0.5)) {
        #         auc_backward[subset_i] <- 0.5
        #       } else {
        #         auc_backward[subset_i] <- mean(c(auc_binary_temp, auc_others_1, auc_others_2))
        #       }
        #       
        #     }
        #     names_x_select <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[which.max(auc_backward)]]
        #     names_x_select <- c(as.character(names_x_select), names_x_fixed)
        #     
        #   } else {
        #     names_x_select <- names_x
        #   }
        #   
        #   row_i <- 1
        #   names_x_select_list_temp[[row_i]] <- names_x_select
        # 
        #   modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
        #   model_fit <-  RWeka::J48(modelformula, data = data_train_y_x_inbag_binary)
        #   model_fit_list_temp[[1]] <- model_fit
        #   
        #   # prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "probability")
        #   # prediction_temp <- -1 + prediction_temp[, 2]*2
        #   # auc_temp <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #   # auc_parameters_grid <- as.data.frame(auc_temp)
        # 
        #   # all classes inbag
        #   prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "probability")
        #   prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
        #   prediction_AllClass_inbag_list_temp[[1]] <- prediction_AllClass_inbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_inbag$Y)[!is.element(unique(data_train_y_x_inbag$Y), class_binary)]
        #   class_others <- as.numeric(as.character(class_others))
        #   
        #   auc_others_1_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_inbag <- c(auc_others_1_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_inbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_inbag <- c(auc_others_2_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_inbag <- pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag) < 0.5)) {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   } else {
        #     auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_1_inbag)), auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #   }
        #   
        #   # all classes outbag
        #   prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "probability")
        #   prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
        #   prediction_AllClass_outbag_list_temp[[1]] <- prediction_AllClass_outbag_temp
        #   
        #   class_binary <- c(class_1, class_2)
        #   class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #   class_others <- sort(as.numeric(as.character(class_others)))
        #   
        #   auc_others_1_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_1_outbag <- c(auc_others_1_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   auc_others_2_outbag <- c()
        #   for (class_others_i in class_others) {
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #     Y_temp <- factor(Y_temp)
        #     auc_others_2_outbag <- c(auc_others_2_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #   }
        #   
        #   idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #   Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #   Y_temp <- as.numeric(as.character(Y_temp))
        #   Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #   Y_temp <- factor(Y_temp)
        #   auc_binary_temp_outbag <- pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc
        #   
        #   if(any(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag) < 0.5)) {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   } else {
        #     auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag)), auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #   }
        #   
        # 
        #   colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        #   colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        #   
        #   auc_parameters_grid <- cbind(auc_temp_inbag, auc_temp_outbag)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        # 
        # } 
        # else if(model_i=="CART"){
        #   
        # 
        #   # modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x, collapse = "+")))
        #   # model_fit <-  rpart::rpart(modelformula, data = data_train_binary_temp)
        #   # prediction_temp <- predict(model_fit, newdata=data_train_binary_temp, type = "prob")
        #   # prediction_temp <- log(prediction_temp[,2]/prediction_temp[,1])
        # 
        #   minsplit_c <- c(5, 10, 15)
        #   maxdepth_c <- c(5, 8, 10, 20, 30)
        #   cp_c <- seq(0.05, 0.1, 0.01)
        # 
        #   names_x_select_list_temp <- list()
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   parameters_grid <- expand.grid(minsplit_c=minsplit_c, maxdepth_c=maxdepth_c, cp_c=cp_c)
        #   # auc_temp <- c()
        #   auc_temp_inbag <- c()
        #   auc_temp_outbag <- c()
        #   for (row_i in 1:nrow(parameters_grid)) {
        #     
        #     if(rfe_select){
        #       RFE_control <-  list(summary = caret::defaultSummary,
        #                            fit = function(x, y, first=TRUE, last=FALSE){
        #                              rpart::rpart(y~., data = cbind(y, x),
        #                                           parms = list(split="gini"),
        #                                           control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                          maxdepth = parameters_grid$maxdepth_c[row_i]))
        #                            },
        #                            pred = function(object, x) {
        #                              prediction_temp <- predict(object, newdata=x, type = "prob")
        #                              prediction_temp <- apply(prediction_temp, 1, which.max)
        #                              prediction_temp
        #                            },
        #                            rank = function(object, x, y) {
        #                              vimp <- object$variable.importance
        #                              # vimp <- model_fit$variable.importance
        #                              # order_vimp <- order(abs(vimp), decreasing = TRUE)
        #                              vimp <- as.data.frame(vimp)
        #                              vimp$var <- rownames(vimp)
        #                              colnames(vimp) <- c("Overall", "var")
        #                              # vimp <- vimp[order_vimp,]
        #                              vimp
        #                            },
        #                            selectSize = caret::pickSizeBest,
        #                            selectVar = caret::pickVars)
        #       
        #       ctrl <- caret::rfeControl(functions = RFE_control)
        #       #ctrl$returnResamp <- "all"
        #       set.seed(seed_rfe)
        #       rfProfile <- caret::rfe(x_rfe, y_rfe, sizes = subsets_rfe, rfeControl = ctrl)
        #       names_x_select <- rfProfile$optVariables
        #     } else if(select_backward_wilcoxon) {
        #       
        #       auc_backward <- c()
        #       for (subset_i in seq_len(length(subsets_backward))) {
        #         # cat(subset_i, "\n")
        #         names_x_select_temp <- as.character(results_Wilcoxon_OVA1$names_x[1:subsets_backward[subset_i]])
        #         names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
        #         
        #         modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select_temp, collapse = "+")))
        #         model_fit_temp <- rpart::rpart(modelformula, data = data_train_y_x_inbag_binary,
        #                                   method = "class",
        #                                   parms = list(split=1),
        #                                   control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                                  maxdepth = parameters_grid$maxdepth_c[row_i],
        #                                                                  cp = parameters_grid$cp_c[row_i]))
        #         
        #         prediction_temp_temp <- predict(model_fit_temp, newdata=data_train_y_x_outbag, type = "prob")
        #         prediction_temp_temp <- -1 + prediction_temp_temp[, 2]*2
        #         
        #         class_binary <- c(class_1, class_2)
        #         class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #         class_others <- as.numeric(as.character(class_others))
        #         
        #         auc_others_1 <- c()
        #         for (class_others_i in class_others) {
        #           idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #           Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #           Y_temp <- as.numeric(as.character(Y_temp))
        #           Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #           Y_temp <- factor(Y_temp)
        #           auc_others_1 <- c(auc_others_1, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #         }
        #         
        #         auc_others_2 <- c()
        #         for (class_others_i in class_others) {
        #           idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #           Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #           Y_temp <- as.numeric(as.character(Y_temp))
        #           Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #           Y_temp <- factor(Y_temp)
        #           auc_others_2 <- c(auc_others_2, pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc)
        #         }
        #         
        #         idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #         Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #         Y_temp <- as.numeric(as.character(Y_temp))
        #         Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #         Y_temp <- factor(Y_temp)
        #         auc_binary_temp <- pROC::roc(Y_temp, prediction_temp_temp[idx_temp], direction="<")$auc
        #         
        #         if(any(c(auc_binary_temp, auc_others_1, auc_others_2) < 0.5)) {
        #           auc_backward[subset_i] <- 0.5
        #         } else {
        #           auc_backward[subset_i] <- mean(c(auc_binary_temp, auc_others_1, auc_others_2))
        #         }
        #         
        #       }
        #       names_x_select <- results_Wilcoxon_OVA1$names_x[1:subsets_backward[which.max(auc_backward)]]
        #       names_x_select <- c(as.character(names_x_select), names_x_fixed)
        #       
        #     } else {
        #       names_x_select <- names_x
        #     }
        #     
        #     names_x_select_list_temp[[row_i]] <- names_x_select
        #     
        #     modelformula <- as.formula(paste(name_y_factor, "~", paste(names_x_select, collapse = "+")))
        #     model_fit <- rpart::rpart(modelformula, data = data_train_y_x_inbag_binary,
        #                               method = "class",
        #                               parms = list(split=1),
        #                               control = rpart::rpart.control(minsplit = parameters_grid$minsplit_c[row_i],
        #                                                        maxdepth = parameters_grid$maxdepth_c[row_i],
        #                                                        cp = parameters_grid$cp_c[row_i]))
        #     
        #     attr(model_fit$terms, ".Environment") <- NULL
        #     
        #     model_fit_list_temp[[row_i]] <- model_fit
        #     
        #     # prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary, type = "prob")
        #     # prediction_temp <- -1 + prediction_temp[, 2]*2
        #     # auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, prediction_temp, quiet=TRUE)$auc
        #     
        #     # all classes inbag
        #     prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
        #     prediction_AllClass_inbag_temp <- -1 + prediction_AllClass_inbag_temp[, 2]*2
        #     prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
        #     
        #     class_binary <- c(class_1, class_2)
        #     class_others <- unique(data_train_y_x_inbag$Y)[!is.element(unique(data_train_y_x_inbag$Y), class_binary)]
        #     class_others <- as.numeric(as.character(class_others))
        #     
        #     auc_others_1_inbag <- c()
        #     for (class_others_i in class_others) {
        #       idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_others_i))
        #       Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #       Y_temp <- factor(Y_temp)
        #       auc_others_1_inbag <- c(auc_others_1_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #     }
        #     
        #     auc_others_2_inbag <- c()
        #     for (class_others_i in class_others) {
        #       idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_2, class_others_i))
        #       Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #       Y_temp <- factor(Y_temp)
        #       auc_others_2_inbag <- c(auc_others_2_inbag, pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc)
        #     }
        #     
        #     idx_temp <- is.element(data_train_y_x_inbag$Y, c(class_1, class_2))
        #     Y_temp <- data_train_y_x_inbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_binary_temp_inbag <- pROC::roc(Y_temp, prediction_AllClass_inbag_temp[idx_temp], direction="<")$auc
        #     
        #     if(any(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag) < 0.5)) {
        #       auc_temp_inbag <- rbind(auc_temp_inbag, c(0.5, auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #     } else {
        #       auc_temp_inbag <- rbind(auc_temp_inbag, c(mean(c(auc_binary_temp_inbag, auc_others_1_inbag, auc_others_1_inbag)), auc_binary_temp_inbag, auc_others_1_inbag, auc_others_2_inbag))
        #     }
        #     
        #     
        #     # all classes outbag
        #     prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
        #     prediction_AllClass_outbag_temp <- -1 + prediction_AllClass_outbag_temp[, 2]*2
        #     prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
        #     
        #     class_binary <- c(class_1, class_2)
        #     class_others <- unique(data_train_y_x_outbag$Y)[!is.element(unique(data_train_y_x_outbag$Y), class_binary)]
        #     class_others <- sort(as.numeric(as.character(class_others)))
        #     
        #     auc_others_1_outbag <- c()
        #     for (class_others_i in class_others) {
        #       idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_others_i))
        #       Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #       Y_temp <- factor(Y_temp)
        #       auc_others_1_outbag <- c(auc_others_1_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #     }
        #     
        #     auc_others_2_outbag <- c()
        #     for (class_others_i in class_others) {
        #       idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_2, class_others_i))
        #       Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #       Y_temp <- as.numeric(as.character(Y_temp))
        #       Y_temp <- ifelse(Y_temp==class_2, -1, 1)
        #       Y_temp <- factor(Y_temp)
        #       auc_others_2_outbag <- c(auc_others_2_outbag, pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc)
        #     }
        #     
        #     idx_temp <- is.element(data_train_y_x_outbag$Y, c(class_1, class_2))
        #     Y_temp <- data_train_y_x_outbag$Y[idx_temp]
        #     Y_temp <- as.numeric(as.character(Y_temp))
        #     Y_temp <- ifelse(Y_temp==class_1, 1, -1)
        #     Y_temp <- factor(Y_temp)
        #     auc_binary_temp_outbag <- pROC::roc(Y_temp, prediction_AllClass_outbag_temp[idx_temp], direction="<")$auc
        #     
        #     if(any(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag) < 0.5)) {
        #       auc_temp_outbag <- rbind(auc_temp_outbag, c(0.5, auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #     } else {
        #       auc_temp_outbag <- rbind(auc_temp_outbag, c(mean(c(auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag)), auc_binary_temp_outbag, auc_others_1_outbag, auc_others_2_outbag))
        #     }
        #     
        #   }
        #   
        #   colnames(auc_temp_inbag) <- c("mean_inbag", "auc_binary_temp_inbag", paste0("auc_inbag_", class_1, "_vs_", class_others), paste0("auc_inbag_", class_2, "_vs_", class_others))
        #   colnames(auc_temp_outbag) <- c("mean_outbag", "auc_binary_temp_outbag", paste0("auc_outbag_", class_1, "_vs_", class_others), paste0("auc_outbag_", class_2, "_vs_", class_others))
        #   
        #   auc_parameters_grid <- cbind(parameters_grid, cbind(auc_temp_inbag, auc_temp_outbag))
        # 
        # 
        # } 
        # else if(model_i=="MultilayerPerception"){
        # 
        #   library(neuralnet)
        # 
        #   # neuralnet package
        #   # size_c <- c("5, 10, 5", "5, 10, 5", "10, 20, 10")
        #   size_c <- c("5", "5, 10")
        # 
        #   model_fit_list_temp <- list()
        #   prediction_AllClass_inbag_list_temp <- list()
        #   prediction_AllClass_outbag_list_temp <- list()
        #   parameters_grid <- as.data.frame(size_c)
        #   auc_temp <- c()
        #   for(row_i in 1:nrow(parameters_grid)){
        #     set.seed(318)
        #     cat(row_i, "\n")
        # 
        #     size_temp <- as.numeric(strsplit(as.character(parameters_grid$size_c[row_i]), ",")[[1]])
        # 
        #     model_fit <- neuralnet::neuralnet(modelformula, data = data_train_y_x_inbag_binary,
        #                                       hidden = size_temp, linear.output = FALSE)
        #     model_fit_list_temp[[row_i]] <- model_fit
        # 
        #     if(inherits(try(predict(model_fit, newdata=data_train_y_x_outbag_binary)), "try-error")){
        #       auc_temp[row_i] <- 0.5
        #       prediction_temp <- rep(0, nrow(data_train_y_x_outbag_binary))
        #     } else {
        #       prediction_temp <- predict(model_fit, newdata=data_train_y_x_outbag_binary)
        #       auc_temp[row_i] <- pROC::roc(data_train_y_x_outbag_binary$Y_binary_factor, c(prediction_temp), quiet=TRUE)$auc
        #     }
        #     
        #     
        #     if(inherits(try(predict(model_fit, newdata=data_train_y_x_inbag)), "try-error")){
        #       prediction_AllClass_inbag_temp <- rep(0, nrow(data_train_y_x_inbag))
        #     } else {
        #       prediction_AllClass_inbag_temp <- predict(model_fit, newdata=data_train_y_x_inbag)
        #     }
        #     prediction_AllClass_inbag_list_temp[[row_i]] <- prediction_AllClass_inbag_temp
        #     
        #     
        #     if(inherits(try(predict(model_fit, newdata=data_train_y_x_outbag)), "try-error")){
        #       prediction_AllClass_outbag_temp <- rep(0, nrow(data_train_y_x_outbag))
        #     } else {
        #       prediction_AllClass_outbag_temp <- predict(model_fit, newdata=data_train_y_x_outbag)
        #     }
        #     prediction_AllClass_outbag_list_temp[[row_i]] <- prediction_AllClass_outbag_temp
        #   }
        #   # auc_temp <- max(auc_temp)
        #   auc_parameters_grid <- cbind(auc_temp, parameters_grid)
        #   auc_parameters_grid <- as.data.frame(auc_parameters_grid)
        # 
        # }

        # auc_binary_temp <- c(auc_binary_temp, auc_temp)
        auc_binary_list_bootstrap_combination[[match(model_i, model_names)]] <- auc_parameters_grid
        model_list_bootstrap_combination[[match(model_i, model_names)]] <- model_fit_list_temp
        names_x_select_list_bootstrap_combination[[match(model_i, model_names)]] <- names_x_select_list_temp
        threshold_list_bootstrap_combination[[match(model_i, model_names)]] <- threshold_list_temp
        prediction_temp_temp_list_bootstrap_combination[[match(model_i, model_names)]] <- prediction_temp_temp_list
        prediction_AllClass_inbag_list_bootstrap_combination[[match(model_i, model_names)]] <- prediction_AllClass_inbag_list_temp
        prediction_AllClass_outbag_list_bootstrap_combination[[match(model_i, model_names)]] <- prediction_AllClass_outbag_list_temp

      }

      # auc_model_list_bootstrap[[combination_i]] <- auc_binary_list_bootstrap_combination
      # auc_model_list_bootstrap[[combination_i+ncol(class_combinations)]] <- model_list_bootstrap_combination

      auc_model_list_bootstrap_temp <- list(auc = auc_binary_list_bootstrap_combination, names_x_select=names_x_select_list_bootstrap_combination, 
                                            threshold=threshold_list_bootstrap_combination,
                                            prediction_temp_temp_list_bootstrap_combination=prediction_temp_temp_list_bootstrap_combination,
                                            model=model_list_bootstrap_combination, prediction_inbag=prediction_AllClass_inbag_list_bootstrap_combination, 
                                            prediction_outbag=prediction_AllClass_outbag_list_bootstrap_combination)

      # auc_model_list_bootstrap[[combination_i]] <- auc_model_list_bootstrap_temp   ## Stack2 Way1: normal for loop
      auc_model_list_bootstrap_temp ## Stack2 Way2: dopar loop

    }

    auc_model_list[[bootstrap_i]] <- auc_model_list_bootstrap   ## Way1: normal for loop
    # auc_model_list_bootstrap    ## Stack1 Way2: dopar loop

  }

  if(parallel_combinations){
    stopCluster(cl) 
  }

  print('fit binary classifiers Done \n')
  tictoc::toc()

  
  save(idx_inbag, file=paste0("code/", pathname, "/idx_inbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
  save(idx_outbag, file=paste0("code/", pathname, "/idx_outbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
  save(names_x_NoFixed_sub_bootstrap, file=paste0("code/", pathname, "/names_x_NoFixed_sub_bootstrap_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
  save(auc_model_list, file=paste0("code/", pathname, "/auc_model_list_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_", paste0(model_names, collapse="_"), "_.RData"))
  
  
  # load(file=paste0("code/", pathname, "/auc_model_list_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_", paste0(model_names, collapse="_"), "_.RData"))
  # load(file=paste0("code/", pathname, "/data_train_y_x_inbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
  # load(file=paste0("code/", pathname, "/data_train_y_x_outbag_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))



#### 2.2 obtain the best parameter of each binary classifier

# bootstrap_i <- 1
# combination_i <- 3
print("obtain the best parameter of each binary classifier start")
idx_best_parameter_model_combination_bootstrap <- list()
clusterCtriteria_best_parameter_model_combination_bootstrap <- list()
auc_best_parameter_model_combination_bootstrap <- list()
prediction_AllClass_inbag_best_parameter_model_combination_bootstrap <- list()
prediction_AllClass_outbag_best_parameter_model_combination_bootstrap <- list()
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  idx_inbag_temp <- idx_inbag[[bootstrap_i]]
  idx_outbag_temp <- idx_outbag[[bootstrap_i]]
  
  names_x_NoFixed_sub_temp <- names_x_NoFixed_sub_bootstrap[[bootstrap_i]]
  
  data_train_y_x_inbag <- data_train_y_x[idx_inbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]
  data_train_y_x_outbag <- data_train_y_x[idx_outbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]

  idx_best_parameter_model_combination <- c()
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
    auc_best_parameter_model <- c()
    prediction_AllClass_inbag_best_parameter_model <- c()
    prediction_AllClass_outbag_best_parameter_model <- c()
    for (model_i in model_names) {
      auc_parameters_grid <- auc_binary_list_bootstrap_combination[[match(model_i, model_names)]]
      
      if(length(grep("mimimum_outbag", colnames(auc_parameters_grid)))>0){
        colnames(auc_parameters_grid)[grep("mimimum_outbag", colnames(auc_parameters_grid))] <- "minimum_outbag"
      }
      
      if(length(grep("mimimum", colnames(auc_parameters_grid)))>0){
        colnames(auc_parameters_grid)[grep("mimimum", colnames(auc_parameters_grid))] <- "minimum_outbag"
      }
      
      prediction_AllClass_inbag_list_temp <- prediction_AllClass_inbag_list_bootstrap_combination[[match(model_i, model_names)]]
      prediction_AllClass_outbag_list_temp <- prediction_AllClass_outbag_list_bootstrap_combination[[match(model_i, model_names)]]

      idx_best_parameter_temp <- which.max(auc_parameters_grid$minimum_outbag)
      # idx_best_parameter_temp <- which.max(auc_parameters_grid$auc_binary_temp_outbag)
      auc_best_parameter_temp <- max(auc_parameters_grid$minimum_outbag)
      prediction_AllClass_inbag_best_parameter_temp <- prediction_AllClass_inbag_list_temp[[idx_best_parameter_temp]]
      prediction_AllClass_outbag_best_parameter_temp <- prediction_AllClass_outbag_list_temp[[idx_best_parameter_temp]]

      idx_best_parameter_model <- c(idx_best_parameter_model, idx_best_parameter_temp)
      auc_best_parameter_model <- c(auc_best_parameter_model, auc_best_parameter_temp)
      prediction_AllClass_inbag_best_parameter_model <- cbind(prediction_AllClass_inbag_best_parameter_model, prediction_AllClass_inbag_best_parameter_temp)
      prediction_AllClass_outbag_best_parameter_model <- cbind(prediction_AllClass_outbag_best_parameter_model, prediction_AllClass_outbag_best_parameter_temp)
      
    }
    colnames(prediction_AllClass_inbag_best_parameter_model) <- model_names
    colnames(prediction_AllClass_outbag_best_parameter_model) <- model_names
    idx_best_parameter_model_combination <- cbind(idx_best_parameter_model_combination, idx_best_parameter_model)
    auc_best_parameter_model_combination <- cbind(auc_best_parameter_model_combination, auc_best_parameter_model)
    prediction_AllClass_inbag_best_parameter_model_combination[[combination_i]] <- prediction_AllClass_inbag_best_parameter_model
    prediction_AllClass_outbag_best_parameter_model_combination[[combination_i]] <- prediction_AllClass_outbag_best_parameter_model
  
  }
  idx_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- idx_best_parameter_model_combination
  auc_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- auc_best_parameter_model_combination
  prediction_AllClass_inbag_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- prediction_AllClass_inbag_best_parameter_model_combination
  prediction_AllClass_outbag_best_parameter_model_combination_bootstrap[[bootstrap_i]] <- prediction_AllClass_outbag_best_parameter_model_combination

}

print("obtain the best parameter of each binary classifier end")


#### 2.3 obtain the optimal combinations of binary classifiers according to clustering criteria

print("obtain the optimal combinations of binary classifiers according to clustering criteria start")
idx_models <- list()
idx_combinations <- list()
idx_combination_remian <- list()
weight_combinations_bootstrap <- list()
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  auc_best_parameter_model_combination <- auc_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  
  idx_models[[bootstrap_i]] <- apply(auc_best_parameter_model_combination, 2, which.max)
  idx_combinations[[bootstrap_i]] <- c(1:ncol(class_combinations))
  
  print("idx_combinations")
  print(idx_combinations[[bootstrap_i]])
  print("idx_models")
  print(idx_models[[bootstrap_i]])
  
  auc_max <- apply(auc_best_parameter_model_combination, 2, max)
  
  idx_combination_remian[[bootstrap_i]] <- auc_max>0.0
  
  weight_combinations <- 0.5*log(auc_max/(1-auc_max))
  weight_combinations[weight_combinations==Inf] <- 2.5
  
  weight_combinations_bootstrap[[bootstrap_i]] <- weight_combinations
  
  # idx_models[[bootstrap_i]] <- idx_models[[bootstrap_i]][idx_combination_remian[[bootstrap_i]]]
  # idx_combinations[[bootstrap_i]] <- idx_combinations[[bootstrap_i]][idx_combination_remian[[bootstrap_i]]]
  
  print("auc_best_parameter_model_combination")
  print(auc_best_parameter_model_combination)
  
  print("idx_combinations")
  print(idx_combinations[[bootstrap_i]])
  print("idx_models")
  print(idx_models[[bootstrap_i]])
  
  print("weight_combinations")
  print(weight_combinations_bootstrap[[bootstrap_i]])
  
}

print("obtain the optimal combinations of binary classifiers according to clustering criteria end")

# write.table(idx_combinations, file=paste0("code/", pathname, "/idx_combinations_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.txt"))
# write.table(idx_models, file=paste0("code/", pathname, "/idx_models_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.txt"))



#### 2.4 obtain the prediction of each best binary classifier

print("obtain the prediction of each best binary classifier start")
model_storage_list <- vector(mode = "list", length = num_bootstrap)
prediction_train_inbag_AllBinaryClassifiers_bootstrap <- list()
prediction_train_outbag_AllBinaryClassifiers_bootstrap <- list()
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  idx_inbag_temp <- idx_inbag[[bootstrap_i]]
  idx_outbag_temp <- idx_outbag[[bootstrap_i]]
  
  names_x_NoFixed_sub_temp <- names_x_NoFixed_sub_bootstrap[[bootstrap_i]]
  
  data_train_y_x_inbag <- data_train_y_x[idx_inbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]
  data_train_y_x_outbag <- data_train_y_x[idx_outbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]
  
  idx_best_parameter_model_combination <- idx_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  
  # prediction_AllClass_inbag_best_parameter_model_combination <- prediction_AllClass_inbag_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  # prediction_AllClass_outbag_best_parameter_model_combination <- prediction_AllClass_outbag_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  
  # cluster_criteria_bootstrap_combination <- c()
  prediction_train_inbag_AllBinaryClassifiers <- c()
  prediction_train_outbag_AllBinaryClassifiers <- c()
  best_model_names <- c()
  for (combination_i in seq_len(length(idx_combinations[[bootstrap_i]]))) {
    cat("combination_i ", combination_i, "\n")
    
    idx_best_combination <- idx_combinations[[bootstrap_i]][combination_i]
    idx_best_model <- idx_models[[bootstrap_i]][combination_i]
    idx_best_parameter <- idx_best_parameter_model_combination[idx_best_model, idx_best_combination]
    
    
    # prediction_AllClass_inbag_best_parameter_model <- prediction_AllClass_inbag_best_parameter_model_combination[[idx_best_combination]]
    # prediction_AllClass_outbag_best_parameter_model <- prediction_AllClass_outbag_best_parameter_model_combination[[idx_best_combination]]
    
    # prediction_AllClass_inbag_best_parameter <- prediction_AllClass_inbag_best_parameter_model[, idx_best_model]
    # prediction_AllClass_outbag_best_parameter <- prediction_AllClass_outbag_best_parameter_model[, idx_best_model]
    
    # prediction_inbag <- prediction_AllClass_inbag_best_parameter
    # prediction_outbag <- prediction_AllClass_outbag_best_parameter
    
    
    best_model_names <- c(best_model_names, paste0("combination_", idx_best_combination, "_", model_names[idx_best_model]))

    if(fit_model_binary){
      model_fit <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$model[[idx_best_model]][[idx_best_parameter]]$model_fit
      names_x_select <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$names_x_select[[idx_best_model]][[idx_best_parameter]]$names_x_select
      
      model_storage_list[[bootstrap_i]][[idx_best_combination]] <- list(model_fit=model_fit, 
                                                                        names_x_select=names_x_select)
    }
    
    if(fit_model_OVA){
      model_fit_OVA1 <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$model[[idx_best_model]][[idx_best_parameter]]$model_fit_OVA1
      
      model_fit_OVA2 <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$model[[idx_best_model]][[idx_best_parameter]]$model_fit_OVA2
      
      threshold_OVA1 <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$threshold[[idx_best_model]][[idx_best_parameter]]$threshold_OVA1
      threshold_OVA2 <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$threshold[[idx_best_model]][[idx_best_parameter]]$threshold_OVA2
      
      names_x_select_OVA1 <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$names_x_select[[idx_best_model]][[idx_best_parameter]]$names_x_select_OVA1
      names_x_select_OVA2 <- auc_model_list[[bootstrap_i]][[idx_best_combination]]$names_x_select[[idx_best_model]][[idx_best_parameter]]$names_x_select_OVA2
      
      model_storage_list[[bootstrap_i]][[idx_best_combination]] <- list(model_fit_OVA1=model_fit_OVA1, model_fit_OVA2=model_fit_OVA2, 
                                                                        threshold_OVA1=threshold_OVA1, threshold_OVA2=threshold_OVA2, 
                                                                        names_x_select_OVA1=names_x_select_OVA1, names_x_select_OVA2=names_x_select_OVA2)
    }
    

    
    if(is.element(model_names[idx_best_model], c("RandomForest"))){
      
      # all classes inbag
      ### binary
      if(fit_model_binary){
        prediction_inbag <- predict(model_fit, data=data_train_y_x_inbag, probability = TRUE)$predictions
        prediction_inbag <- prediction_inbag[, "1"]
        if(rescale_prediction&(max(prediction_inbag)!=min(prediction_inbag))){
          ratio_inbag_temp <- 1/(max(prediction_inbag) - min(prediction_inbag))
          prediction_inbag <- (prediction_inbag - min(prediction_inbag)) * ratio_inbag_temp
          prediction_inbag <- -1 + prediction_inbag*2
        }
        
        prediction_outbag <- predict(model_fit, data=data_train_y_x_outbag, probability = TRUE)$predictions
        prediction_outbag <- prediction_outbag[, "1"]
        if(rescale_prediction&(max(prediction_outbag)!=min(prediction_outbag))){
          ratio_outbag_temp <- 1/(max(prediction_outbag) - min(prediction_outbag))
          prediction_outbag <- (prediction_outbag - min(prediction_outbag)) * ratio_outbag_temp
          prediction_outbag <- -1 + prediction_outbag*2
        }

      }

      if(fit_model_OVA){
        ### prediction OVA1
        prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, data = data_train_y_x_inbag)$predictions
        prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, 2]
        ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
        prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
        prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
        
        # prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, -1, prediction_AllClass_inbag_OVA1_temp)
        
        if(threshold_OVA1<0){
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
        }
        
        ### prediction OVA2
        prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, data = data_train_y_x_inbag)$predictions
        prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, 2]
        ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
        prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
        prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
        
        # prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA1, 1, prediction_AllClass_inbag_OVA2_temp)
        
        if(threshold_OVA2>0){
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
        }
        
        # prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp) / 2
        prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp)
        
        # all classes outbag
        ### binary
        if(fit_model_binary){
          prediction_outbag <- predict(model_fit, data=data_train_y_x_outbag, probability = TRUE)$predictions
          prediction_outbag <- prediction_outbag[, 2]
          ratio_outbag_temp <- 1/(max(prediction_outbag) - min(prediction_outbag))
          prediction_outbag <- (prediction_outbag - min(prediction_outbag)) * ratio_outbag_temp
          prediction_outbag <- -1 + prediction_outbag*2
        }
        
        ### prediction OVA1
        prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, data = data_train_y_x_outbag)$predictions
        prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, 2]
        ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
        prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
        prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
        
        # prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp>threshold_OVA1, 1, prediction_AllClass_outbag_OVA1_temp)
        
        if(threshold_OVA1<0){
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
        }
        
        ### prediction OVA2
        prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, data = data_train_y_x_outbag)$predictions
        prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, 2]
        ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
        prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
        prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
        
        # prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA1, 1, prediction_AllClass_outbag_OVA2_temp)
        
        if(threshold_OVA2>0){
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
        }
        
        # prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
        prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp)
      }
      
    } else if(is.element(model_names[idx_best_model], c("SVM"))) {
      
      if(fit_model_binary){
        # all classes inbag
        prediction_inbag <- predict(model_fit, newdata = data_train_y_x_inbag, probability = TRUE)
        prediction_inbag <- attr(prediction_inbag, "probabilities")
        prediction_inbag <- prediction_inbag[, "1"]
        if(rescale_prediction&(max(prediction_inbag)!=min(prediction_inbag))){
          ratio_inbag_temp <- 1/(max(prediction_inbag) - min(prediction_inbag))
          prediction_inbag <- (prediction_inbag - min(prediction_inbag)) * ratio_inbag_temp
          prediction_inbag <- -1 + prediction_inbag*2
        }
        
        # all classes outbag
        prediction_outbag <- predict(model_fit, newdata = data_train_y_x_outbag, probability = TRUE)
        prediction_outbag <- attr(prediction_outbag, "probabilities")
        prediction_outbag <- prediction_outbag[, "1"]
        if(rescale_prediction&(max(prediction_outbag)!=min(prediction_outbag))){
          ratio_outbag_temp <- 1/(max(prediction_outbag) - min(prediction_outbag))
          prediction_outbag <- (prediction_outbag - min(prediction_outbag)) * ratio_outbag_temp
          prediction_outbag <- -1 + prediction_outbag*2
        }

      }
      
      if(fit_model_OVA){
        # all classes inbag
        ### prediction OVA1
        prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, newdata = data_train_y_x_inbag, probability = TRUE)
        prediction_AllClass_inbag_OVA1_temp <- attr(prediction_AllClass_inbag_OVA1_temp, "probabilities")
        prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, "1"]
        ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
        prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
        prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
        
        if(threshold_OVA1<0){
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
        }
        
        ### prediction OVA2
        prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, newdata = data_train_y_x_inbag, probability = TRUE)
        prediction_AllClass_inbag_OVA2_temp <- attr(prediction_AllClass_inbag_OVA2_temp, "probabilities")
        prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, "1"]
        ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
        prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
        prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
        
        if(threshold_OVA2>0){
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
        }
        
        prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp)
        
        # all classes outbag
        ### prediction OVA1
        prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, newdata = data_train_y_x_outbag, probability = TRUE)
        prediction_AllClass_outbag_OVA1_temp <- attr(prediction_AllClass_outbag_OVA1_temp, "probabilities")
        prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, "1"]
        ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
        prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
        prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
        
        if(threshold_OVA1<0){
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
        }
        
        ### prediction OVA2
        prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, newdata = data_train_y_x_outbag, probability = TRUE)
        prediction_AllClass_outbag_OVA2_temp <- attr(prediction_AllClass_outbag_OVA2_temp, "probabilities")
        prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, "1"]
        ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
        prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
        prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
        
        if(threshold_OVA2>0){
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
        }
        
        prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp)
        
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
      }
      
    } 
    else if(is.element(model_names[idx_best_model], c("LASSO"))){

      # all classes inbag
      ### prediction OVA1
      x_train_inbag_OVA1 <- as.matrix(data_train_y_x_inbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_inbag_OVA1_temp <- c(predict(model_fit_OVA1, newx=x_train_inbag_OVA1, s = names_x_select_OVA1, type = "response"))
      ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
      prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
      prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
      
      if(threshold_OVA1<0){
        prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
      } else {
        prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
      }
      
      ### prediction OVA2
      x_train_inbag_OVA2 <- as.matrix(data_train_y_x_inbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_inbag_OVA2_temp <- c(predict(model_fit_OVA2, newx=x_train_inbag_OVA2, s = names_x_select_OVA2, type = "response"))
      ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
      prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
      prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
      
      if(threshold_OVA2>0){
        prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
      } else {
        prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
      }
      
      prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp)
      
      # all classes outbag
      ### prediction OVA1
      x_train_outbag_OVA1 <- as.matrix(data_train_y_x_outbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_outbag_OVA1_temp <- c(predict(model_fit_OVA1, newx=x_train_outbag_OVA1, s = names_x_select_OVA1, type = "response"))
      ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
      prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
      prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
      
      if(threshold_OVA1<0){
        prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
      } else {
        prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
      }
      
      ### prediction OVA2
      x_train_outbag_OVA2 <- as.matrix(data_train_y_x_outbag[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_outbag_OVA2_temp <- c(predict(model_fit_OVA2, newx=x_train_outbag_OVA2, s = names_x_select_OVA2, type = "response"))
      ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
      prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
      prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
      
      if(threshold_OVA2>0){
        prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
      } else {
        prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
      }
      
      prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp)
      
    } else if(is.element(model_names[idx_best_model], c("AdaBoost"))){
      
      # all classes inbag
      ### prediction OVA1
      prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, newdata = data_train_y_x_inbag)
      prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, 2]
      ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
      prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
      prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
      
      ### prediction OVA2
      prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, newdata = data_train_y_x_inbag)
      prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, 2]
      ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
      prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
      prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
      
      prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp) / 2
      
      # all classes outbag
      ### prediction OVA1
      prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, newdata = data_train_y_x_outbag)
      prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, 2]
      ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
      prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
      prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
      
      ### prediction OVA2
      prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, newdata = data_train_y_x_outbag)
      prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, 2]
      ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
      prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
      prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
      
      prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
      
    } 
    else if(is.element(model_names[idx_best_model], c("AdaBoost_adabag"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag)$prob
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag)$prob
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("C4.5"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, type = "probability")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, type = "probability")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("CART"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_inbag, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=data_train_y_x_outbag, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("MultilayerPerception"))){
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
  
  prediction_train_inbag_AllBinaryClassifiers <- prediction_train_inbag_AllBinaryClassifiers[, sort.int(idx_combinations[[bootstrap_i]], index.return =T)$ix]
  prediction_train_outbag_AllBinaryClassifiers <- prediction_train_outbag_AllBinaryClassifiers[, sort.int(idx_combinations[[bootstrap_i]], index.return =T)$ix]

  
  # plot_boxplot_prediction_InbagAndOutbag <- TRUE
  if(plot_boxplot_prediction_InbagAndOutbag){
    prediction_train_inbag_AllBinaryClassifiers_boxplot <- cbind(data_train_y_x_inbag$Y, prediction_train_inbag_AllBinaryClassifiers)
    colnames(prediction_train_inbag_AllBinaryClassifiers_boxplot)[1] <- "Y"
    
    for (i in 2:ncol(prediction_train_inbag_AllBinaryClassifiers_boxplot)) {
      cat(i, "\n")
      x_i <- colnames(prediction_train_inbag_AllBinaryClassifiers_boxplot)[i]
      boxplot_age_45 <- ggplot(prediction_train_inbag_AllBinaryClassifiers_boxplot, aes(Y, get(x_i))) + geom_boxplot(aes(fill=Y)) +
        # geom_dotplot(binaxis='y',
        #              binwidth = 1.5,
        #              stackdir='center',
        #              # dotsize = .5,
        #              fill="red") +
        theme(axis.text.x = element_text(angle=0, vjust=0.6)) 
      boxplot_age_45
      
      ggsave(filename = paste0(path,"/results/", pathname,"/boxplot_prediction/", "bootstrap_", bootstrap_i, "_inbag_", x_i, ".png"), boxplot_age_45, height = 10, width = 10)
    }
    
    
    prediction_train_outbag_AllBinaryClassifiers_boxplot <- cbind(data_train_y_x_outbag$Y, prediction_train_outbag_AllBinaryClassifiers)
    colnames(prediction_train_outbag_AllBinaryClassifiers_boxplot)[1] <- "Y"
    
    for (i in 2:ncol(prediction_train_outbag_AllBinaryClassifiers_boxplot)) {
      cat(i, "\n")
      x_i <- colnames(prediction_train_outbag_AllBinaryClassifiers_boxplot)[i]
      boxplot_age_45 <- ggplot(prediction_train_outbag_AllBinaryClassifiers_boxplot, aes(Y, get(x_i))) + geom_boxplot(aes(fill=Y)) +
        # geom_dotplot(binaxis='y',
        #              binwidth = 1.5,
        #              stackdir='center',
        #              # dotsize = .5,
        #              fill="red") +
        theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
        labs(title="Box plot + Dot plot_age 45+",
             x="group",
             y=x_i)
      boxplot_age_45
      
      ggsave(filename = paste0(path,"/results/", pathname,"/boxplot_prediction/", "bootstrap_", bootstrap_i, "_outbag_", x_i, ".jpeg"), boxplot_age_45, height = 10, width = 10)
    }
  }
  
  ### !!!!! weight the prediction
  weights_combination <- weight_combinations_bootstrap[[bootstrap_i]]
  weights_combination_matrix_inbag <- t(replicate(nrow(prediction_train_inbag_AllBinaryClassifiers), weights_combination))
  prediction_train_inbag_AllBinaryClassifiers <- weights_combination_matrix_inbag * prediction_train_inbag_AllBinaryClassifiers
  
  weights_combination_matrix_outbag <- t(replicate(nrow(prediction_train_outbag_AllBinaryClassifiers), weights_combination))
  prediction_train_outbag_AllBinaryClassifiers <- weights_combination_matrix_outbag * prediction_train_outbag_AllBinaryClassifiers
  
  ### !!!!! keep the predictions of remained classifiers
  prediction_train_inbag_AllBinaryClassifiers_bootstrap[[bootstrap_i]] <- prediction_train_inbag_AllBinaryClassifiers[, idx_combination_remian[[bootstrap_i]]]
  prediction_train_outbag_AllBinaryClassifiers_bootstrap[[bootstrap_i]] <- prediction_train_outbag_AllBinaryClassifiers[, idx_combination_remian[[bootstrap_i]]]
  
}

print("obtain the prediction of each best binary classifier end")


save(model_storage_list, file=paste0("code/", pathname, "/model_storage_list_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
save(prediction_AllClass_inbag_list_bootstrap_combination, file=paste0("code/", pathname, "/prediction_AllClass_inbag_list_bootstrap_combination_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
save(prediction_AllClass_outbag_list_bootstrap_combination, file=paste0("code/", pathname, "/prediction_AllClass_outbag_list_bootstrap_combination_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))
#print(ls())
#print(sort(sapply(ls(),function(x){object.size(get(x))})))
rm(list=c("auc_model_list", "auc_model_list_bootstrap", "prediction_AllClass_inbag_list_bootstrap_combination", "prediction_AllClass_outbag_list_bootstrap_combination"), envir = environment())
#print(ls())
#print(sort(sapply(ls(),function(x){object.size(get(x))})))

save(list=ls(), envir = environment(), file=paste0("code/", pathname, "/ATM_saved_test_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))



#### 2.5 obtain the FINAL prediction of each best binary classifier

# prediction_train_inbag_AllBinaryClassifiers_final <- Reduce("+",prediction_train_inbag_AllBinaryClassifiers_bootstrap)/length(prediction_train_inbag_AllBinaryClassifiers_bootstrap)
# prediction_train_outbag_AllBinaryClassifiers_final <- Reduce("+",prediction_train_outbag_AllBinaryClassifiers_bootstrap)/length(prediction_train_outbag_AllBinaryClassifiers_bootstrap)
# 
# plot_boxplot <- TRUE
# if(plot_boxplot){
#   prediction_train_inbag_AllBinaryClassifiers_boxplot <- cbind(data_train_y_x_inbag$Y, prediction_train_inbag_AllBinaryClassifiers_final)
#   colnames(prediction_train_inbag_AllBinaryClassifiers_boxplot)[1] <- "Y"
#   
#   for (i in 2:ncol(prediction_train_inbag_AllBinaryClassifiers_boxplot)) {
#     cat(i, "\n")
#     x_i <- colnames(prediction_train_inbag_AllBinaryClassifiers_boxplot)[i]
#     boxplot_age_45 <- ggplot(prediction_train_inbag_AllBinaryClassifiers_boxplot, aes(Y, get(x_i))) + geom_boxplot(aes(fill=Y)) +
#       # geom_dotplot(binaxis='y',
#       #              binwidth = 1.5,
#       #              stackdir='center',
#       #              # dotsize = .5,
#       #              fill="red") +
#       theme(axis.text.x = element_text(angle=0, vjust=0.6)) 
#     boxplot_age_45
#     
#     ggsave(filename = paste0(path,"/results/", pathname,"/boxplot_prediction/", "inbag_", x_i, "_final.jpeg"), boxplot_age_45, height = 10, width = 10)
#   }
#   
#   
#   prediction_train_outbag_AllBinaryClassifiers_boxplot <- cbind(data_train_y_x_outbag$Y, prediction_train_outbag_AllBinaryClassifiers_final)
#   colnames(prediction_train_outbag_AllBinaryClassifiers_boxplot)[1] <- "Y"
#   
#   for (i in 2:ncol(prediction_train_outbag_AllBinaryClassifiers_boxplot)) {
#     cat(i, "\n")
#     x_i <- colnames(prediction_train_outbag_AllBinaryClassifiers_boxplot)[i]
#     boxplot_age_45 <- ggplot(prediction_train_outbag_AllBinaryClassifiers_boxplot, aes(Y, get(x_i))) + geom_boxplot(aes(fill=Y)) +
#       # geom_dotplot(binaxis='y',
#       #              binwidth = 1.5,
#       #              stackdir='center',
#       #              # dotsize = .5,
#       #              fill="red") +
#       theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
#       labs(title="Box plot + Dot plot_age 45+",
#            x="group",
#            y=x_i)
#     boxplot_age_45
#     
#     ggsave(filename = paste0(path,"/results/", pathname,"/boxplot_prediction/", "outbag_", x_i, "_final.jpeg"), boxplot_age_45, height = 10, width = 10)
#   }
# }
# 
# idx_combination_remian_final <- Reduce("&", idx_combination_remian)
# 
# prediction_train_inbag_AllBinaryClassifiers_final <- prediction_train_inbag_AllBinaryClassifiers_final[, idx_combination_remian_final]
# prediction_train_outbag_AllBinaryClassifiers_final <- prediction_train_outbag_AllBinaryClassifiers_final[, idx_combination_remian_final]

############################################################################################################################################
### primary code: K-means ensemble
############################################################################################################################################
# prediction_train_inbag_AllBinaryClassifiers_boxplot <- cbind(data_train_y_x_inbag$Y, prediction_train_inbag_AllBinaryClassifiers)
# colnames(prediction_train_inbag_AllBinaryClassifiers_boxplot)[1] <- "Y"
# 
# 
# centroid_train_outbag <- aggregate(. ~ Y, data = prediction_train_outbag_AllBinaryClassifiers_boxplot, mean, na.rm = TRUE)
# 
# euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
# 
# library(foreach)
# dist_matrix_train_inbag <- c()
# for (class_i in 1:K) {
#   
#   dist_temp <- foreach(i = 1:nrow(prediction_train_inbag_AllBinaryClassifiers), .combine = c ) %do% euc.dist(prediction_train_inbag_AllBinaryClassifiers[i,], centroid_train_inbag[class_i, 2:ncol(centroid_train_inbag)])
#   
#   dist_matrix_train_inbag <- cbind(dist_matrix_train_inbag, dist_temp)
# }
# 
# 
# class_prediction_train_inbag <- apply(dist_matrix_train_inbag, 1, which.min)
# 
# table(data_train_y_x_inbag$Y, class_prediction_train_inbag)
# 
# multiclass.roc(data_train_y_x_inbag$Y, class_prediction_train_inbag)
# F1(data_train_y_x_inbag$Y, class_prediction_train_inbag)
# 
# 
# dist_matrix_train_outbag <- c()
# for (class_i in 1:K) {
#   
#   dist_temp <- foreach(i = 1:nrow(prediction_train_outbag_AllBinaryClassifiers), .combine = c ) %do% euc.dist(prediction_train_outbag_AllBinaryClassifiers[i,], centroid_train_outbag[class_i, 2:ncol(centroid_train_outbag)])
#   
#   dist_matrix_train_outbag <- cbind(dist_matrix_train_outbag, dist_temp)
# }
# 
# 
# class_prediction_train_outbag <- apply(dist_matrix_train_outbag, 1, which.min)
# 
# table(data_train_y_x_outbag$Y, class_prediction_train_outbag)
# 
# multiclass.roc(data_train_y_x_outbag$Y, class_prediction_train_outbag)
# 
# F1(data_train_y_x_outbag$Y, class_prediction_train_outbag)
# 
# 
# ### test
# dist_matrix_test <- c()
# for (class_i in 1:K) {
#   
#   dist_temp <- foreach(i = 1:nrow(prediction_testset_AllBinaryClassifiers), .combine = c ) %do% euc.dist(prediction_testset_AllBinaryClassifiers[i,], centroid_train_outbag[class_i, 2:ncol(centroid_train_outbag)])
#   
#   dist_matrix_test <- cbind(dist_matrix_test, dist_temp)
# }
# 
# 
# class_prediction_test <- apply(dist_matrix_test, 1, which.min)
# 
# table(data_test_y, class_prediction_test)
# 
# multiclass.roc(data_test_y, class_prediction_test)
# 
# F1(data_test_y, class_prediction_test)
# 
# AU1U(data_test_y, class_prediction_test)
# 
# AUNU(data_test_y, class_prediction_test)
# 
# AR(data_test_y, class_prediction_test)
# 
# AAR(data_test_y, class_prediction_test)
############################################################################################################################################



#### 3.1 optimize the cost using GA
#### 遗传算法优化cost

fitness_function <- function(x, M, prediction_matrix_inbag, N_row_inbag, label_real_inbag, num_combination, prediction_matrix_outbag, N_row_outbag, label_real_outbag){
  
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
  
  prediction_matrix_inbag <- as.matrix(prediction_matrix_inbag) + adjust_values_matrix_inbag
  
  ## part 3: obtain the predicted class of each instance according to respect loss
  
  ### part 3.2: inbag
  class_prediction_inbag_temp <- c()
  Margin_inbag <- c()
  for (i in 1:N_row_inbag) {
    # cat(i, "  ", "\n")
    loss_i <- -M%*%as.numeric(prediction_matrix_inbag[i, ])

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
  
  prediction_matrix_outbag <- as.matrix(prediction_matrix_outbag) + adjust_values_matrix_outbag
  
  ## part 3: obtain the predicted class of each instance according to respect loss
  
  ### part 3.2: outbag
  class_prediction_outbag_temp <- c()
  Margin_outbag <- c()
  for (i in 1:N_row_outbag) {
    # cat(i, "  ", "\n")
    loss_i <- -M%*%as.numeric(prediction_matrix_outbag[i, ])
    
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




GA_results_bootstrap <- list()
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  source(paste0(path, '/code/1_function_OVO_code_matrix_generator.R'))
  M_coding <- OVO_matrix(K=K)
  
  M_coding_temp <- M_coding[, idx_combination_remian[[bootstrap_i]]]
  
  idx_inbag_temp <- idx_inbag[[bootstrap_i]]
  idx_outbag_temp <- idx_outbag[[bootstrap_i]]
  
  names_x_NoFixed_sub_temp <- names_x_NoFixed_sub_bootstrap[[bootstrap_i]]
  
  data_train_y_x_inbag <- data_train_y_x[idx_inbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]
  data_train_y_x_outbag <- data_train_y_x[idx_outbag_temp, c("Y", names_x_fixed, names_x_NoFixed_sub_temp)]
  
  prediction_train_inbag_AllBinaryClassifiers <- prediction_train_inbag_AllBinaryClassifiers_bootstrap[[bootstrap_i]]
  prediction_train_outbag_AllBinaryClassifiers <- prediction_train_outbag_AllBinaryClassifiers_bootstrap[[bootstrap_i]]
  
  print("test fitness_function start")
  x = c(0, 1, max(table(data_train_y_x_inbag$Y))/table(data_train_y_x_inbag$Y))
  # x = c(40, 2, 30, 80, 20, 1)
  # x = c(4, 1, 1.214286, 1.100000, 1.478261)
  # x = c(4, 1, 1.184286, 1.100000, 1.478261)
  # prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers
  # N_row_inbag = nrow(data_train_y_x_inbag)
  # label_real_inbag = data_train_y_x_inbag$Y
  # num_combination = ncol(M)
  # prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers
  # N_row_outbag = nrow(data_train_y_x_outbag)
  # label_real_outbag = data_train_y_x_outbag$Y
  
  
  while (inherits( try(
    aa <- fitness_function(x = x,
                           M = M_coding_temp,
                           prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers,
                           N_row_inbag = nrow(data_train_y_x_inbag),
                           label_real_inbag = data_train_y_x_inbag$Y,
                           num_combination = ncol(M_coding_temp),
                           prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers,
                           N_row_outbag = nrow(data_train_y_x_outbag),
                           label_real_outbag = data_train_y_x_outbag$Y)
    
  ), "try-error")){
    x = c(runif(1, 0, 10), 1, max(table(data_train_y_x_inbag$Y))/table(data_train_y_x_inbag$Y))
  }
  
  
  
  print("test fitness_function end")
  
  # plot_scatter <- FALSE
  if(plot_scatter){
    library(plotly)
    
    # prediction_plot_inbag <- prediction_matrix_inbag
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
  num_cores = floor(0.1*cores)
  # num_cores = 10
  
  # DELTA_initial <- c(5)
  # DELTA_lower <- c(0)
  # DELTA_upper <- c(8)
  
  DELTA_initial <- median(aa$Margin_outbag)
  DELTA_lower <- ifelse(min(aa$Margin_outbag)<0, min(aa$Margin_outbag), 0)
  DELTA_upper <- floor(1.5*max(aa$Margin_outbag))
  
  GAMMA_initial <- c(max(table(data_train_y_x_inbag$Y))/table(data_train_y_x_inbag$Y))
  GAMMA_lower <- 0.1*GAMMA_initial
  GAMMA_upper <- 20*GAMMA_initial
  
  LAMBDA_initial <- c(1)
  LAMBDA_lower <- c(1)
  LAMBDA_upper <- c(10)
  
  x_initial <- c(DELTA_initial, LAMBDA_initial, GAMMA_initial)
  x_lower <- c(DELTA_lower, LAMBDA_lower, GAMMA_lower)
  x_upper <- c(DELTA_upper, LAMBDA_upper, GAMMA_upper)
  
  num_combination <- ncol(M_coding_temp)
  
  GA_results_bootstrap[[bootstrap_i]] <- ga(type="real-valued", fitness=fitness_function,
                                 M=M_coding_temp,
                                 prediction_matrix_inbag = prediction_train_inbag_AllBinaryClassifiers,
                                 N_row_inbag = nrow(data_train_y_x_inbag),
                                 label_real_inbag = data_train_y_x_inbag$Y,
                                 num_combination = num_combination,
                                 prediction_matrix_outbag = prediction_train_outbag_AllBinaryClassifiers,
                                 N_row_outbag = nrow(data_train_y_x_outbag),
                                 label_real_outbag = data_train_y_x_outbag$Y,
                                 lower = x_lower, upper = x_upper,
                                 population=gareal_Population_R, selection=ga_lrSelection_R, crossover=gareal_waCrossover_R, mutation=gareal_rsMutation_R,
                                 popSize=500, pcrossover = 0.8, pmutation=0.1,
                                 keepBest = TRUE, elitism=floor(0.5*500),
                                 suggestions = x_initial,
                                 maxiter=5, run=5, maxFitness=1, parallel = num_cores)
  
  

  
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
  
  
  # if(nrow(GA_results_bootstrap@solution)>1){
  #   solution_id <- sample(1:nrow(GA_results_bootstrap@solution), 1)
  #   x_values_optimal <- GA_results_bootstrap@solution[solution_id, ]
  # } else {
  #   x_values_optimal <- GA_results_bootstrap@solution
  # }
}

save(GA_results_bootstrap, file=paste0("code/", pathname, "/GA_results_bootstrap_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_", "_.RData"))




##### 3.2 obtain the prediction values of train dataset and test dataset
print('obtain the prediction values of train dataset and test dataset start')
prediction_trainset_AllBinaryClassifiers_bootstrap <- list()
prediction_testset_AllBinaryClassifiers_bootstrap <- list()
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  names_x_NoFixed_sub_temp <- names_x_NoFixed_sub_bootstrap[[bootstrap_i]]
  
  data_train_y_x_sub <- data_train_y_x[, c(names_x_fixed, names_x_NoFixed_sub_temp)]
  NEW_X_sub <- NEW_X[, c(names_x_fixed, names_x_NoFixed_sub_temp)]
  
  idx_best_parameter_model_combination <- idx_best_parameter_model_combination_bootstrap[[bootstrap_i]]
  
  prediction_trainset_AllBinaryClassifiers <- c()
  prediction_testset_AllBinaryClassifiers <- c()
  best_model_names <- c()
  for (combination_i in seq_len(length(idx_combinations[[bootstrap_i]]))) {
    cat("combination_i ", combination_i, "\n")
    
    idx_best_combination <- idx_combinations[[bootstrap_i]][combination_i]
    idx_best_model <- idx_models[[bootstrap_i]][combination_i]
    idx_best_parameter <- idx_best_parameter_model_combination[idx_best_model, idx_best_combination]
    
    best_model_names <- c(best_model_names, paste0("combination_", idx_best_combination, "_", model_names[idx_best_model]))
    
    if(fit_model_binary){
      model_fit <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$model_fit
      names_x_select <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$names_x_select
    }

    if(fit_model_OVA){
      model_fit_OVA1 <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$model_fit_OVA1
      model_fit_OVA2 <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$model_fit_OVA2
      
      threshold_OVA1 <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$threshold_OVA1
      threshold_OVA2 <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$threshold_OVA2
      
      names_x_select_OVA1 <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$names_x_select_OVA1
      names_x_select_OVA2 <- model_storage_list[[bootstrap_i]][[idx_best_combination]]$names_x_select_OVA2
    }
    
    if(is.element(model_names[idx_best_model], c("RandomForest"))){
      
      if(fit_model_binary){
        # all classes inbag
        ### binary
        prediction_inbag <- predict(model_fit, data = data_train_y_x_sub)$predictions
        prediction_inbag <- prediction_inbag[, "1"]
        if(rescale_prediction&(max(prediction_inbag)!=min(prediction_inbag))){
          ratio_inbag_temp <- 1/(max(prediction_inbag) - min(prediction_inbag))
          prediction_inbag <- (prediction_inbag - min(prediction_inbag)) * ratio_inbag_temp
          prediction_inbag <- -1 + prediction_inbag*2
        }

        # all classes outbag
        ### binary
        prediction_outbag <- predict(model_fit, data = NEW_X_sub)$predictions
        prediction_outbag <- prediction_outbag[, "1"]
        if(rescale_prediction&(max(prediction_outbag)!=min(prediction_outbag))){
          ratio_outbag_temp <- 1/(max(prediction_outbag) - min(prediction_outbag))
          prediction_outbag <- (prediction_outbag - min(prediction_outbag)) * ratio_outbag_temp
          prediction_outbag <- -1 + prediction_outbag*2
        }

      }
      
      if(fit_model_OVA){
        ### all classes inbag
        # ### prediction OVA1
        prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, data = data_train_y_x_sub)$predictions
        prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, 2]
        ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
        prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
        prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
        
        if(threshold_OVA1<0){
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
        }
        
        
        # ### prediction OVA2
        prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, data = data_train_y_x_sub)$predictions
        prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, 2]
        ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
        prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
        prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
        
        if(threshold_OVA2>0){
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
        }

        prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp) / 2
        
        
        ### all classes outbag
        # ### prediction OVA1
        prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, data = NEW_X_sub)$predictions
        prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, 2]
        ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
        prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
        prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
        
        if(threshold_OVA1<0){
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
        }
        
        # ### prediction OVA2
        prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, data = NEW_X_sub)$predictions
        prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, 2]
        ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
        prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
        prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
        
        if(threshold_OVA2>0){
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
        }
        
        prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
      }
      
      
    } else if(is.element(model_names[idx_best_model], c("SVM"))) {
      
      if(fit_model_binary){
        # all classes inbag
        ### prediction OVA1
        prediction_inbag <- predict(model_fit, newdata = data_train_y_x_sub, probability = TRUE)
        prediction_inbag <- attr(prediction_inbag, "probabilities")
        prediction_inbag <- prediction_inbag[, "1"]
        if(rescale_prediction&(max(prediction_inbag)!=min(prediction_inbag))){
          ratio_inbag_temp <- 1/(max(prediction_inbag) - min(prediction_inbag))
          prediction_inbag <- (prediction_inbag - min(prediction_inbag)) * ratio_inbag_temp
          prediction_inbag <- -1 + prediction_inbag*2
        }

        
        # all classes outbag
        ### prediction OVA1
        prediction_outbag <- predict(model_fit, newdata = NEW_X_sub, probability = TRUE)
        prediction_outbag <- attr(prediction_outbag, "probabilities")
        prediction_outbag <- prediction_outbag[, "1"]
        if(rescale_prediction&(max(prediction_outbag)!=min(prediction_outbag))){
          ratio_outbag_temp <- 1/(max(prediction_outbag) - min(prediction_outbag))
          prediction_outbag <- (prediction_outbag - min(prediction_outbag)) * ratio_outbag_temp
          prediction_outbag <- -1 + prediction_outbag*2
        }
      }
      
      if(fit_model_OVA){
        # all classes inbag
        ### prediction OVA1
        prediction_AllClass_inbag_OVA1_temp <- predict(model_fit_OVA1, newdata = data_train_y_x_sub, probability = TRUE)
        prediction_AllClass_inbag_OVA1_temp <- attr(prediction_AllClass_inbag_OVA1_temp, "probabilities")
        prediction_AllClass_inbag_OVA1_temp <- prediction_AllClass_inbag_OVA1_temp[, "1"]
        ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
        prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
        prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
        
        if(threshold_OVA1<0){
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
        }
        
        ### prediction OVA2
        prediction_AllClass_inbag_OVA2_temp <- predict(model_fit_OVA2, newdata = data_train_y_x_sub, probability = TRUE)
        prediction_AllClass_inbag_OVA2_temp <- attr(prediction_AllClass_inbag_OVA2_temp, "probabilities")
        prediction_AllClass_inbag_OVA2_temp <- prediction_AllClass_inbag_OVA2_temp[, "1"]
        ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
        prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
        prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
        
        if(threshold_OVA2>0){
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
        }
        
        prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp)
        
        # all classes outbag
        ### prediction OVA1
        prediction_AllClass_outbag_OVA1_temp <- predict(model_fit_OVA1, newdata = NEW_X_sub, probability = TRUE)
        prediction_AllClass_outbag_OVA1_temp <- attr(prediction_AllClass_outbag_OVA1_temp, "probabilities")
        prediction_AllClass_outbag_OVA1_temp <- prediction_AllClass_outbag_OVA1_temp[, "1"]
        ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
        prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
        prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
        
        if(threshold_OVA1<0){
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
        } else {
          prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
        }
        
        ### prediction OVA2
        prediction_AllClass_outbag_OVA2_temp <- predict(model_fit_OVA2, newdata = NEW_X_sub, probability = TRUE)
        prediction_AllClass_outbag_OVA2_temp <- attr(prediction_AllClass_outbag_OVA2_temp, "probabilities")
        prediction_AllClass_outbag_OVA2_temp <- prediction_AllClass_outbag_OVA2_temp[, "1"]
        ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
        prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
        prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
        
        if(threshold_OVA2>0){
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
        } else {
          prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
        }
        
        prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp)
      }
      
    } 
    else if(is.element(model_names[idx_best_model], c("naivebayes"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_sub, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X_sub, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("C50"))) {
      x_train_inbag <- as.matrix(data_train_y_x_sub[, names_x])
      prediction_inbag <- predict(model_fit, newdata=x_train_inbag, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      x_train_outbag <- as.matrix(NEW_X_sub[, names_x])
      prediction_outbag <- predict(model_fit, newdata=x_train_outbag, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("ctree"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_sub, type = "prob")
      prediction_inbag <- do.call(rbind, prediction_inbag)
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X_sub, type = "prob")
      prediction_outbag <- do.call(rbind, prediction_outbag)
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("wsrf"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_sub, type = "prob")
      prediction_inbag <- prediction_inbag$prob
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X_sub, type = "prob")
      prediction_outbag <- prediction_outbag$prob
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("KNNreg"))) {
      x_train_inbag <- as.matrix(data_train_y_x_sub[, names_x])
      prediction_inbag <- predict(model_fit, newdata = x_train_inbag)
      
      x_train_outbag <- as.matrix(NEW_X_sub[, names_x])
      prediction_outbag <- predict(model_fit, newdata = x_train_outbag)
    } 
    else if(is.element(model_names[idx_best_model], c("KNNclassification"))) {
      x_train_inbag <- as.matrix(data_train_y_x_sub[, names_x])
      prediction_inbag <- predict(model_fit, newdata = x_train_inbag)
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      x_train_outbag <- as.matrix(NEW_X_sub[, names_x])
      prediction_outbag <- predict(model_fit, newdata = x_train_outbag)
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("LASSO"))){
      
      # all classes inbag
      ### prediction OVA1
      x_train_inbag_OVA1 <- as.matrix(data_train_y_x_sub[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_inbag_OVA1_temp <- c(predict(model_fit_OVA1, newx=x_train_inbag_OVA1, s = names_x_select_OVA1, type = "response"))
      ratio_inbag_OVA1_temp <- 1/(max(prediction_AllClass_inbag_OVA1_temp) - min(prediction_AllClass_inbag_OVA1_temp))
      prediction_AllClass_inbag_OVA1_temp <- (prediction_AllClass_inbag_OVA1_temp - min(prediction_AllClass_inbag_OVA1_temp)) * ratio_inbag_OVA1_temp
      prediction_AllClass_inbag_OVA1_temp <- -1 + prediction_AllClass_inbag_OVA1_temp*2
      
      if(threshold_OVA1<0){
        prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp-threshold_OVA1)
      } else {
        prediction_AllClass_inbag_OVA1_temp <- ifelse(prediction_AllClass_inbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_inbag_OVA1_temp)
      }
      
      ### prediction OVA2
      x_train_inbag_OVA2 <- as.matrix(data_train_y_x_sub[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_inbag_OVA2_temp <- c(predict(model_fit_OVA2, newx=x_train_inbag_OVA2, s = names_x_select_OVA2, type = "response"))
      ratio_inbag_OVA2_temp <- 1/(max(prediction_AllClass_inbag_OVA2_temp) - min(prediction_AllClass_inbag_OVA2_temp))
      prediction_AllClass_inbag_OVA2_temp <- (prediction_AllClass_inbag_OVA2_temp - min(prediction_AllClass_inbag_OVA2_temp)) * ratio_inbag_OVA2_temp
      prediction_AllClass_inbag_OVA2_temp <- -1 + prediction_AllClass_inbag_OVA2_temp*2
      
      if(threshold_OVA2>0){
        prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp-threshold_OVA2)
      } else {
        prediction_AllClass_inbag_OVA2_temp <- ifelse(prediction_AllClass_inbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_inbag_OVA2_temp)
      }
      
      prediction_inbag <- (prediction_AllClass_inbag_OVA1_temp + prediction_AllClass_inbag_OVA2_temp) / 2
      
      # all classes outbag
      ### prediction OVA1
      x_train_outbag_OVA1 <- as.matrix(NEW_X_sub[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_outbag_OVA1_temp <- c(predict(model_fit_OVA1, newx=x_train_outbag_OVA1, s = names_x_select_OVA1, type = "response"))
      ratio_outbag_OVA1_temp <- 1/(max(prediction_AllClass_outbag_OVA1_temp) - min(prediction_AllClass_outbag_OVA1_temp))
      prediction_AllClass_outbag_OVA1_temp <- (prediction_AllClass_outbag_OVA1_temp - min(prediction_AllClass_outbag_OVA1_temp)) * ratio_outbag_OVA1_temp
      prediction_AllClass_outbag_OVA1_temp <- -1 + prediction_AllClass_outbag_OVA1_temp*2
      
      if(threshold_OVA1<0){
        prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp-threshold_OVA1)
      } else {
        prediction_AllClass_outbag_OVA1_temp <- ifelse(prediction_AllClass_outbag_OVA1_temp<threshold_OVA1, 0, prediction_AllClass_outbag_OVA1_temp)
      }
      
      ### prediction OVA2
      x_train_outbag_OVA2 <- as.matrix(NEW_X_sub[, c(names_x_fixed, names_x_NoFixed_sub_temp)])
      prediction_AllClass_outbag_OVA2_temp <- c(predict(model_fit_OVA2, newx=x_train_outbag_OVA2, s = names_x_select_OVA2, type = "response"))
      ratio_outbag_OVA2_temp <- 1/(max(prediction_AllClass_outbag_OVA2_temp) - min(prediction_AllClass_outbag_OVA2_temp))
      prediction_AllClass_outbag_OVA2_temp <- (prediction_AllClass_outbag_OVA2_temp - min(prediction_AllClass_outbag_OVA2_temp)) * ratio_outbag_OVA2_temp
      prediction_AllClass_outbag_OVA2_temp <- -1 + prediction_AllClass_outbag_OVA2_temp*2
      
      if(threshold_OVA2>0){
        prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp-threshold_OVA2)
      } else {
        prediction_AllClass_outbag_OVA2_temp <- ifelse(prediction_AllClass_outbag_OVA2_temp>threshold_OVA2, 0, prediction_AllClass_outbag_OVA2_temp)
      }
      
      prediction_outbag <- (prediction_AllClass_outbag_OVA1_temp + prediction_AllClass_outbag_OVA2_temp) / 2
      
    } else if(is.element(model_names[idx_best_model], c("AdaBoost"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_sub)$prob
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X_sub)$prob
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("C4.5"))) {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_sub, type = "probability")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X_sub, type = "probability")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("CART"))){
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_sub, type = "prob")
      prediction_inbag <- -1 + prediction_inbag[, 2]*2
      
      prediction_outbag <- predict(model_fit, newdata=NEW_X_sub, type = "prob")
      prediction_outbag <- -1 + prediction_outbag[, 2]*2
    } 
    else if(is.element(model_names[idx_best_model], c("MultilayerPerception"))){
      x_train_inbag <- as.matrix(data_train_y_x_sub[, names_x])
      prediction_inbag <- c(predict(model_fit, newdata = x_train_inbag))
      
      x_train_outbag <- as.matrix(NEW_X_sub[, names_x])
      prediction_outbag <- c(predict(model_fit, newdata = x_train_outbag))
    } 
    else {
      prediction_inbag <- predict(model_fit, newdata=data_train_y_x_sub)
      prediction_outbag <- predict(model_fit, newdata=NEW_X_sub)
    }
    
    prediction_trainset_AllBinaryClassifiers <- cbind(prediction_trainset_AllBinaryClassifiers, prediction_inbag)
    prediction_testset_AllBinaryClassifiers <- cbind(prediction_testset_AllBinaryClassifiers, prediction_outbag)
    
  }
  
  prediction_trainset_AllBinaryClassifiers <- as.data.frame(prediction_trainset_AllBinaryClassifiers)
  colnames(prediction_trainset_AllBinaryClassifiers) <- best_model_names
  
  prediction_testset_AllBinaryClassifiers <- as.data.frame(prediction_testset_AllBinaryClassifiers)
  colnames(prediction_testset_AllBinaryClassifiers) <- best_model_names
  
  prediction_trainset_AllBinaryClassifiers <- prediction_trainset_AllBinaryClassifiers[, sort.int(idx_combinations[[bootstrap_i]], index.return =T)$ix]
  prediction_testset_AllBinaryClassifiers <- prediction_testset_AllBinaryClassifiers[, sort.int(idx_combinations[[bootstrap_i]], index.return =T)$ix]

  
  # plot_boxplot_prediction_TrainAndTest <- TRUE
  if(plot_boxplot_prediction_TrainAndTest){
    prediction_trainset_AllBinaryClassifiers_boxplot <- cbind(data_train_y_x$Y, prediction_trainset_AllBinaryClassifiers)
    colnames(prediction_trainset_AllBinaryClassifiers_boxplot)[1] <- "Y"
    
    for (i in 2:ncol(prediction_trainset_AllBinaryClassifiers_boxplot)) {
      cat(i, "\n")
      x_i <- colnames(prediction_trainset_AllBinaryClassifiers_boxplot)[i]
      boxplot_age_45 <- ggplot(prediction_trainset_AllBinaryClassifiers_boxplot, aes(Y, get(x_i))) + geom_boxplot(aes(fill=Y)) +
        # geom_dotplot(binaxis='y',
        #              binwidth = 1.5,
        #              stackdir='center',
        #              # dotsize = .5,
        #              fill="red") +
        theme(axis.text.x = element_text(angle=0, vjust=0.6)) 
      boxplot_age_45
      
      ggsave(filename = paste0(path,"/results/", pathname,"/boxplot_prediction/", "bootstrap_", bootstrap_i, "_trainset_", x_i, ".jpeg"), boxplot_age_45, height = 10, width = 10)
    }
    
    
    prediction_testset_AllBinaryClassifiers_boxplot <- cbind(factor(data_test_y_numerical), prediction_testset_AllBinaryClassifiers)
    colnames(prediction_testset_AllBinaryClassifiers_boxplot)[1] <- "Y"
    
    for (i in 2:ncol(prediction_testset_AllBinaryClassifiers_boxplot)) {
      cat(i, "\n")
      x_i <- colnames(prediction_testset_AllBinaryClassifiers_boxplot)[i]
      boxplot_age_45 <- ggplot(prediction_testset_AllBinaryClassifiers_boxplot, aes(Y, get(x_i))) + geom_boxplot(aes(fill=Y)) +
        # geom_dotplot(binaxis='y',
        #              binwidth = 1.5,
        #              stackdir='center',
        #              # dotsize = .5,
        #              fill="red") +
        theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
        labs(title="Box plot + Dot plot_age 45+",
             x="group",
             y=x_i)
      boxplot_age_45
      
      ggsave(filename = paste0(path,"/results/", pathname,"/boxplot_prediction/", "bootstrap_", bootstrap_i, "_testset_", x_i, ".jpeg"), boxplot_age_45, height = 10, width = 10)
    }
  }
  
  ### !!!!! weight the prediction
  weights_combination <- weight_combinations_bootstrap[[bootstrap_i]]
  weights_combination_matrix_trainset <- t(replicate(nrow(prediction_trainset_AllBinaryClassifiers), weights_combination))
  prediction_trainset_AllBinaryClassifiers <- weights_combination_matrix_trainset * prediction_trainset_AllBinaryClassifiers
  
  weights_combination_matrix_testset <- t(replicate(nrow(prediction_testset_AllBinaryClassifiers), weights_combination))
  prediction_testset_AllBinaryClassifiers <- weights_combination_matrix_testset * prediction_testset_AllBinaryClassifiers
  
  ### !!!!! keep the predictions of remained classifiers
  prediction_trainset_AllBinaryClassifiers_bootstrap[[bootstrap_i]] <- prediction_trainset_AllBinaryClassifiers[, idx_combination_remian[[bootstrap_i]]]
  prediction_testset_AllBinaryClassifiers_bootstrap[[bootstrap_i]] <- prediction_testset_AllBinaryClassifiers[, idx_combination_remian[[bootstrap_i]]]
  
  
}

print('obtain the prediction values of train dataset and test dataset Done')

# prediction_trainset_AllBinaryClassifiers <- Reduce("+",prediction_trainset_AllBinaryClassifiers_bootstrap)/length(prediction_trainset_AllBinaryClassifiers_bootstrap)
# prediction_testset_AllBinaryClassifiers <- Reduce("+",prediction_testset_AllBinaryClassifiers_bootstrap)/length(prediction_testset_AllBinaryClassifiers_bootstrap)

# idx_combination_remian <- Reduce("&", idx_combination_remian)

# prediction_trainset_AllBinaryClassifiers <- prediction_trainset_AllBinaryClassifiers[, idx_combination_remian]
# prediction_testset_AllBinaryClassifiers <- prediction_testset_AllBinaryClassifiers[, idx_combination_remian]


##### 3.3 obtain the prediction of classes in each bootstrap

print('obtain the prediction of classes in each bootstrap start')
results_final <- list()
class_prediction_inbag_bootstrap <- c()
class_prediction_outbag_bootstrap <- c()
for (bootstrap_i in 1:num_bootstrap) {
  cat(bootstrap_i, "\n")
  
  source(paste0(path, '/code/1_function_OVO_code_matrix_generator.R'))
  M_coding <- OVO_matrix(K=K)
  
  M_coding_temp <- M_coding[, idx_combination_remian[[bootstrap_i]]]
  
  for(s_i in order(GA_results_bootstrap[[bootstrap_i]]@fitness, decreasing = TRUE)){
    # cat(s_i, "\n")
    if(inherits(try(
      results_final[[bootstrap_i]] <- fitness_function(x = GA_results_bootstrap[[bootstrap_i]]@population[s_i,],
                                                       M = M_coding_temp,
                                                       prediction_matrix_inbag = prediction_trainset_AllBinaryClassifiers_bootstrap[[bootstrap_i]],
                                                       N_row_inbag = nrow(prediction_trainset_AllBinaryClassifiers_bootstrap[[bootstrap_i]]),
                                                       label_real_inbag = data_train_y_x$Y,
                                                       num_combination = ncol(M_coding_temp),
                                                       prediction_matrix_outbag = prediction_testset_AllBinaryClassifiers_bootstrap[[bootstrap_i]],
                                                       N_row_outbag = nrow(prediction_testset_AllBinaryClassifiers_bootstrap[[bootstrap_i]]),
                                                       label_real_outbag = NULL)
    ), "try-error")){
      next
    } else {
      # results_final[[bootstrap_i]] <- fitness_function(x = GA_results_bootstrap[[bootstrap_i]]@population[s_i,],
      #                                                  prediction_matrix_inbag = prediction_trainset_AllBinaryClassifiers_bootstrap[[bootstrap_i]],
      #                                                  N_row_inbag = nrow(prediction_trainset_AllBinaryClassifiers_bootstrap[[bootstrap_i]]),
      #                                                  label_real_inbag = data_train_y_x$Y,
      #                                                  num_combination = ncol(M),
      #                                                  prediction_matrix_outbag = prediction_testset_AllBinaryClassifiers_bootstrap[[bootstrap_i]],
      #                                                  N_row_outbag = nrow(prediction_testset_AllBinaryClassifiers_bootstrap[[bootstrap_i]]),
      #                                                  label_real_outbag = NULL)
      class_prediction_inbag_bootstrap <- cbind(class_prediction_inbag_bootstrap, results_final[[bootstrap_i]]$class_prediction_inbag)
      class_prediction_outbag_bootstrap <- cbind(class_prediction_outbag_bootstrap, results_final[[bootstrap_i]]$class_prediction_outbag)
      break
    }
  }
}

##### obtain the final prediction of class using vote strategy
class_prediction_inbag_final <- apply(class_prediction_inbag_bootstrap, 1, function(x) names(which.max(table(x))))
class_prediction_outbag_final <- apply(class_prediction_outbag_bootstrap, 1, function(x) names(which.max(table(x))))

class_prediction_inbag_final <- as.numeric(class_prediction_inbag_final)
class_prediction_outbag_final <- as.numeric(class_prediction_outbag_final)

print('obtain the prediction of classes in each bootstrap Done')

save(list=ls(), envir = environment(), file=paste0("code/", pathname, "/ATM_saved_rep_i_", rep_i, "_fold_i_", fold_i, "_.RData"))

print("ATM done")

return(list(class_prediction_train=class_prediction_inbag_final, class_prediction_test=class_prediction_outbag_final))
  
}







