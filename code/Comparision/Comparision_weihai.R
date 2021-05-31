

# if (!requireNamespace("openxlsx", quietly = TRUE))
#   install.packages("openxlsx", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("dplyr", quietly = TRUE))
#   install.packages("dplyr", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("ggplot2", quietly = TRUE))
#   install.packages("ggplot2", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("data.table", quietly = TRUE))
#   install.packages("data.table", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("ranger", quietly = TRUE))
#   install.packages("ranger", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("pROC", quietly = TRUE))
#   install.packages("pROC", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("e1071", quietly = TRUE))
#   install.packages("e1071", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("adabag", quietly = TRUE))
#   install.packages("adabag", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("metaheuristicOpt", quietly = TRUE))
#   install.packages("metaheuristicOpt", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("foreach", quietly = TRUE))
#   install.packages("foreach", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("doParallel", quietly = TRUE))
#   install.packages("doParallel", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("caret", quietly = TRUE))
#   install.packages("caret", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("glmnet", quietly = TRUE))
#   install.packages("glmnet", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("GA", quietly = TRUE))
#   install.packages("GA", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("tictoc", quietly = TRUE))
#   install.packages("tictoc", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("matlabr", quietly = TRUE))
#   install.packages("matlabr", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
# 
# if (!requireNamespace("CVXR", quietly = TRUE))
#   install.packages("CVXR", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")


path <- "/home/jtw/biyelunwen"

# path <- "F:/biyelunwen"

# path <- "D:/山大课题/毕业论文"

setwd(path)

# options(matlab.path='/usr/lib64/matlab2019b/bin/')

library(CVXR)
library(openxlsx)
# library(dplyr)
library(ggplot2)
library(data.table)
library(ranger)
library(adabag)

# source(paste0(path,'/code/1_function_adaptive_threshold_moving_bootstrap??.R', echo=TRUE))
# source(paste0(path,'/code/1_function_adaptive_threshold_moving_????_??ɸѡ??_???ι???.R', echo=TRUE))
# source(paste0(path,'/code/1_function_adaptive_threshold_moving_????_??ɸѡ??_???ι滮_PSO.R', echo=TRUE))

print("0")
# source(paste0(path,'/code/test1.R'))
print("1")
source(paste0(path,'/code/1_function_metrics_unbalance_classification.R'))
print("2")
source(paste0(path,'/code/1_function_benchmarks(Multi-Imbalance_Matlab).R'))
print("3")
source(paste0(path,'/code/aaaa_GA_Rpackage_OptimalClassifiers_20201013_simplified_optimizeCombines_RFE_Pruning.R'))
source(paste0(path,'/code/aaaa_auc_calculator.R'))
# source(paste0(path,'/code/aaaa_GA_No_Optimazition_20200930.R'))

### ????data
library(ranger)
library(pROC)

pathname <-  "weihai"

data_all <- openxlsx::read.xlsx(paste0(path,"/data/", pathname,"/whct_radiomics_data_clean.xlsx"))

# table(data_all$lex, data_all$fx, useNA = "ifany")
# table(data_all$fx_new1, useNA = "ifany")
# table(data_all$buwei, useNA = "ifany")

data_all <- subset(data_all, !is.na(data_all$fx_new1))

name_y_data_all <- "fx_new1"
names_x_data_all_radiomics <- colnames(data_all)[grepl("original|log|wavelet", colnames(data_all))]
names_x_data_all <- c(c("age", "gender1"), names_x_data_all_radiomics)

data_all[, names_x_data_all_radiomics] <- scale(data_all[, names_x_data_all_radiomics], scale = TRUE, center = FALSE)

for (i in seq_len(length(names_x_data_all_radiomics))) {
  cat(i, "\n")
  x_i <- names_x_data_all_radiomics[i]
  var_i <- data_all[, x_i]
  Quartile <- quantile(var_i)[4] - quantile(var_i)[2]
  Median <- quantile(var_i)[3]
  Bound_upper <- Median + Quartile
  Bound_lower <- Median - Quartile
  
  data_all[, x_i][var_i > Bound_upper] <- Bound_upper
  data_all[, x_i][var_i < Bound_lower] <- Bound_lower

}

# data_all_y <- data_all[, name_y_data_all]
# data_all_x <- data_all[, names_x_data_all]
# data_all_x <- as.matrix(data_all_x)



if(!dir.exists(paste0("data/", pathname))) dir.create(paste0("data/", pathname), recursive = TRUE)
if(!dir.exists(paste0("code/", pathname))) dir.create(paste0("code/", pathname), recursive = TRUE)
if(!dir.exists(paste0("code/QuadraticProgramming/", pathname))) dir.create(paste0("code/QuadraticProgramming/", pathname), recursive = TRUE)
if(!dir.exists(paste0("results/", pathname))) dir.create(paste0("results/", pathname), recursive = TRUE)


#########################################
### statistical description
#library(Publish)

#(var_lx_mean <- paste0("CLASS~",paste0(names_x_data_all,collapse="+")))
#mean_sd <- summary(univariateTable(as.formula(var_lx_mean), data=data_all, summary.format = "mean(x) ?? sd(x)", Q.format = "median(x) [range(x)]",digits = 5))

#(var_lx_median <- paste0("CLASS~",paste0("Q(",names_x_data_all,")",collapse="+")))
#median_range <- summary(univariateTable(as.formula(var_lx_median), data=data_all, summary.format = "mean(x) ?? sd(x)", Q.format = "median(x) [range(x)]",digits = 5))

#(var_fl_count <- paste0("CLASS~",paste0(names_x_data_all,collapse="+")))
#count_percent <- summary(univariateTable(as.formula(var_fl_count), data=data_all, freq.format = "count(x) (percent(x))", column.percent = TRUE, digits=2))





#########################################

num_rep <- 5   # ʵ???ظ?????
num_Kfold <- 5  # K?۽?????֤?е?K

# names_methods <- c("ATM", "DECOC", "DOVO", "AdaBoostM1", "SAMME", "AdaBoostNC", "AdaC2M1", "PIBoost",
#                    "MCHDDT", "HDDTECOC", "HDDTOVA", "imECOCdense", "imECOCOVA", "imECOCsparse", "fuzzyImbECOC",
#                    "MultiImAO", "MultiImOAHO", "MultiImOVA", "MultiImOVO")

# names_methods <- c("ATM", "AdaBoostM1", "SAMME", "AdaBoostNC", "AdaC2M1", "PIBoost",
#                    "MCHDDT", "HDDTECOC", "HDDTOVA", "imECOCdense", "imECOCOVA", "imECOCsparse", "fuzzyImbECOC",
#                    "MultiImAO", "MultiImOAHO", "MultiImOVA", "MultiImOVO")

# names_methods <- c("original_RF", "over_sampling_RF", "down_sampling_RF", "cost_sensitive_RF")
# names_methods <- c("down_sampling_RF", "cost_sensitive_RF")
names_methods <- c("ATM", "AdaBoostM1", "SAMME")
names_methods <- c("ATM")
select_backward_wilcoxon <- TRUE
names_x_fixed = c("age")
subsets_backward <- c(20, 40, 60, 80, 160, 240, 320, 350, 400, 450, 500, 550, 600, 700)
subsets_backward_adaboost <- c(20, 160, 400)

rep_i <- 2
fold_i <- 2
prediction_performance <- c()
prediction_performance_average <- c()
for(rep_i in 1:num_rep){
  
  table(data_all[, name_y_data_all])
  set.seed(rep_i)
  idx_Kfold <- caret::createFolds(factor(data_all[, name_y_data_all]), k = num_Kfold)  ## ?ֲ?????
  class_prediction_test_AllMethods <- c()
  y_test_stack <- c()
  
  for(fold_i in 1:num_Kfold){
    # class_prediction_temp <- c()
    cat("-----------------------------------------", rep_i, fold_i, "----------------------------------", "\n")
    
    idx_test <- idx_Kfold[[fold_i]]
    idx_train <- c(1:nrow(data_all))[!is.element(c(1:nrow(data_all)), idx_test)]
    
    data_train <- data_all[idx_train, ]
    data_test <- data_all[idx_test, ]
    
    data_train_x <- data_train[, names_x_data_all]
    data_train_y <- data_train[, name_y_data_all]
    data_train_y_numerical <- as.numeric(as.character(data_train_y))
    
    data_test_x <- data_test[, names_x_data_all]
    data_test_y <- data_test[, name_y_data_all]
    data_test_y_numerical <- as.numeric(as.character(data_test_y))
    
    data_train <- cbind(data_train_y, data_train_x)
    data_train$data_train_y <- factor(data_train$data_train_y)
    if(any(!is.element(names_methods, "ATM"))){
      results_Wilcoxon <- c()
      print("wilcoxon multiclasses start")
      for (var_i in names_x_data_all_radiomics) {
        # cat(var_i, "\n")
        # x_wilcoxon <- data_train_y_x_inbag_binary[]
        Wilcoxon_model <- kruskal.test(as.formula(paste(var_i, "~", "data_train_y")), data = data_train)
        aa <- c(Wilcoxon_model$statistic, Wilcoxon_model$p.value)
        results_Wilcoxon <- rbind(results_Wilcoxon, aa)
      }
      colnames(results_Wilcoxon) <- c("statistics", "p")
      results_Wilcoxon <- cbind(names_x_data_all_radiomics, results_Wilcoxon)
      results_Wilcoxon <- as.data.frame(results_Wilcoxon)
      results_Wilcoxon <- results_Wilcoxon[order(as.numeric(as.character(results_Wilcoxon$p)), decreasing = FALSE), ]
      colnames(results_Wilcoxon)[1] <- "names_x"
      print("wilcoxon multiclasses end")
    }
    
    if(!dir.exists(paste0("data/", pathname))) dir.create(paste0("data/", pathname), recursive = TRUE)
    # write.xlsx(data_train_x, file = paste0("data/", pathname, "/data_train_x.xlsx"), row.names=FALSE, col.names=FALSE)
    # write.xlsx(data_train_y_numerical, file = paste0("data/", pathname, "/data_train_y.xlsx"), row.names=FALSE, col.names=FALSE)
    # write.xlsx(data_test_x, file = paste0("data/", pathname, "/data_test_x.xlsx"), row.names=FALSE, col.names=FALSE)
    # write.xlsx(data_test_y_numerical, file = paste0("data/", pathname, "/data_test_y.xlsx"), row.names=FALSE, col.names=FALSE)
    
    y_test_stack <- c(y_test_stack, data_test_y)
    
    class_prediction_test_AllMethods_fold_i <- c()
    
    ### method 0: Adaptivie Threshold Moving (ATM)
    if(is.element("ATM", names_methods)){
      results_ATM <- Adaptive_threshold_moving(X=data_train_x, Y=data_train_y, NEW_X=data_test_x, num_bootstrap=1, pathname = pathname, fold_i=fold_i, rep_i=rep_i, 
                                               rfe_select=FALSE, select_backward = FALSE, select_backward_wilcoxon = TRUE, names_x_fixed = c("age", "gender1"),
                                               subsets_rfe = subsets_backward, parallel_combinations=TRUE, parallel_AdaBoost=FALSE, sub_features=TRUE)
      # results_ATM <- Adaptive_threshold_moving_NoOptimization(X=data_train_x, Y=data_train_y, NEW_X=data_test_x, num_bootstrap=1, pathname = pathname, fold_i=fold_i, rep_i=rep_i)
      
      X=data_train_x
      Y=data_train_y
      NEW_X=data_test_x
      num_bootstrap=3
      pathname = pathname
      rfe_select = FALSE
      select_backward = FALSE
      select_backward_wilcoxon = TRUE
      names_x_fixed = c("age", "gender1")
      subsets_rfe=c(20, 40, 60, 80, 160, 240, 320, 350, 400, 450, 500, 550, 600, 700)
      parallel_combinations=TRUE
      parallel_AdaBoost=FALSE
      sub_features=TRUE
      
      class_prediction_test <- results_ATM$class_prediction_test
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
      
      plot_scatter <- FALSE
      if(plot_scatter){
        print("3d scatter plot trainset")
        library(plotly)
        
        source(paste0(path, '/code/1_function_OVO_code_matrix_generator.R'))
        M <- OVO_matrix(K=length(unique(data_train_y)))
        prediction_plot_trainset <- results_ATM$prediction_trainset_AllBinaryClassifiers
        prediction_plot_trainset <- as.data.frame(prediction_plot_trainset)
        colnames(prediction_plot_trainset) <- c("c1", "c2", "c3")
        prediction_plot_trainset$label <- data_train_y
        prediction_plot_trainset$class <- 1
        aaa <- cbind(M, c(1, 2, 3), c(2, 2, 2))
        colnames(aaa) <- colnames(prediction_plot_trainset)
        prediction_plot_trainset <- rbind(aaa, prediction_plot_trainset)
        
        prediction_plot_trainset$label <- factor(prediction_plot_trainset$label)
        prediction_plot_trainset$class <- factor(prediction_plot_trainset$class)
        
        fig <- plot_ly(prediction_plot_trainset, x=~c1, y=~c2, z=~c3, color = ~label, symbol = ~class, symbols = c('circle', 'x'))
        fig <- fig %>% add_markers()
        fig <- fig %>% layout(scene = list(xaxis = list(title = 'classifier_1'),
                                           yaxis = list(title = 'classifier_2'),
                                           zaxis = list(title = 'classifier_3')))
        # fig
        
        htmlwidgets::saveWidget(as_widget(fig), file=paste0(path, "/results/", pathname, "/3D_ScatterPlot_trainset_rep_i_", rep_i, "_fold_i_", fold_i, "_.html"))
        
        
        print("3d scatter plot testset")
        prediction_plot_testset <- results_ATM$prediction_testset_AllBinaryClassifiers
        prediction_plot_testset <- as.data.frame(prediction_plot_testset)
        colnames(prediction_plot_testset) <- c("c1", "c2", "c3")
        prediction_plot_testset$label <- data_test_y
        prediction_plot_testset$class <- 1
        aaa <- cbind(M, c(1, 2, 3), c(2, 2, 2))
        colnames(aaa) <- colnames(prediction_plot_testset)
        prediction_plot_testset <- rbind(aaa, prediction_plot_testset)
        
        prediction_plot_testset$label <- factor(prediction_plot_testset$label)
        prediction_plot_testset$class <- factor(prediction_plot_testset$class)
        
        fig <- plot_ly(prediction_plot_testset, x=~c1, y=~c2, z=~c3, color = ~label, symbol = ~class, symbols = c('circle', 'x'))
        fig <- fig %>% add_markers()
        fig <- fig %>% layout(scene = list(xaxis = list(title = 'classifier_1'),
                                           yaxis = list(title = 'classifier_2'),
                                           zaxis = list(title = 'classifier_3')))
        # fig
        
        htmlwidgets::saveWidget(as_widget(fig), file=paste0(path, "/results/", pathname, "/3D_ScatterPlot_testset_rep_i_", rep_i, "_fold_i_", fold_i, "_.html"))
      }
      
    }

    
    ### method 1: DECOC
    if(is.element("DECOC", names_methods)){
      DECOC(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE),
            train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE),
            test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE),
            test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE),
            output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_DECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
            output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_DECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
            matlab_code_file_directory=paste0(path, "/code/", pathname))
      
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_DECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_DECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    ### method 2: DOVO
    if(is.element("DOVO", names_methods)){
      DOVO(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE),
           train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE),
           test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE),
           test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE),
           output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_DOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
           output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_DOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
           matlab_code_file_directory=paste0(path, "/code/", pathname))
      
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_DOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_DOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 3: AdaBoostM1
    matlab_code_file_directory=paste0(path, "/code/", pathname)
    if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
    
    
    # rmat_to_matlab_mat(data_train_x, matname = "trainData", transpose = FALSE)
    # rvec_to_matlab(data_train_y, matname = "trainLabel")
    # rmat_to_matlab_mat(data_test_x, matname = "testData", transpose = FALSE)
    # rvec_to_matlab(data_test_y, matname = "testLabel")
    
    # tictoc::tic()
    # output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE)
    # output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE)
    # 
    # code <- c("clear",
    #           paste0("cd '", path, "/code/", pathname, "'"),
    #           "javaaddpath('weka.jar')",
    #           "p = genpath(pwd)",
    #           "addpath(p, '-begin')",
    #           rmat_to_matlab_mat(data_train_x, matname = "trainData", transpose = FALSE),
    #           rvec_to_matlab(data_train_y, matname = "trainLabel"),
    #           rmat_to_matlab_mat(data_test_x, matname = "testData", transpose = FALSE),
    #           rvec_to_matlab(data_test_y, matname = "testLabel"),
    #           "[trainTime,testTime,predictedResults_train] = adaBoostCartM1(trainData, trainLabel, trainData, 20)",
    #           "[trainTime,testTime,predictedResults_test] = adaBoostCartM1(trainData, trainLabel, testData, 20)",
    #           paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    #           paste0("xlswrite('", output_test_file, "', predictedResults_test)")
    #           )
    # run_matlab_code(code = code)
    # tictoc::toc()
    
    
    if(is.element("AdaBoostM1", names_methods)){
      
      ## matlab code
      # tictoc::tic()
      # AdaBoostM1(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
      #            train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
      #            test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
      #            test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
      #            output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
      #            output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
      #            matlab_code_file_directory=paste0(path, "/code/", pathname))
      # tictoc::toc()
      # 
      # class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      # class_prediction_train <- class_prediction_train$X1
      # 
      # class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      # class_prediction_test <- class_prediction_test$X1
      
      ## R code
#      tictoc::tic()
#      data_train <- cbind(data_train_y, data_train_x)
#      data_train$data_train_y <- factor(data_train$data_train_y)
#      modelformula <- as.formula(paste("data_train_y", "~", paste(colnames(data_train_x), collapse = "+")))
#      model_fit <-  adabag::boosting(modelformula, data = data_train, 
#                                     coeflearn = "Breiman"    ### different coeflean correspond to different kind of adaboost. see details in help document.
#                                     )
#
#      class_prediction_train <- predict(model_fit, newdata=data_train_x)$class
#      class_prediction_test <- predict(model_fit, newdata=data_test_x)$class
#      tictoc::toc()


      tictoc::tic()
      
      library(foreach)
      library(doParallel)
      cores <- detectCores()
      num_cores = floor(0.7*cores)
      
      cl <- makeCluster(num_cores)
      registerDoParallel(cl)
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward_adaboost))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward_adaboost[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <-  caret::train(modelformula, data = data_train, 
                                     method = "AdaBoost.M1"    ### different coeflean correspond to different kind of adaboost. see details in help document.
          )
          
          prediction_temp_temp <- predict(model_fit_temp, newdata = data_train)
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward_adaboost[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      # data_train <- cbind(data_train_y, data_train_x)
      # data_train$data_train_y <- factor(data_train$data_train_y)
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <-  caret::train(modelformula, data = data_train, 
                                     method = "AdaBoost.M1"    ### different coeflean correspond to different kind of adaboost. see details in help document.
                                     )

      class_prediction_train <- predict(model_fit, newdata=data_train_x)
      class_prediction_test <- predict(model_fit, newdata=data_test_x)
      
      stopCluster(cl)
      
      save.image(file=paste0("code/", pathname, "/AdaBoost.M1_", rep_i, "_fold_i_", fold_i, "_.RData"))
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 4: SAMME
    if(is.element("SAMME", names_methods)){
      # SAMME(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
      #       train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
      #       test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
      #       test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
      #       output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
      #       output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
      #       matlab_code_file_directory=paste0(path, "/code/", pathname))
      # class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      # class_prediction_train <- class_prediction_train$X1
      # 
      # class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      # class_prediction_test <- class_prediction_test$X1
      
      ## R code
      tictoc::tic()
      data_train <- cbind(data_train_y, data_train_x)
      data_train$data_train_y <- factor(data_train$data_train_y)
      modelformula <- as.formula(paste("data_train_y", "~", paste(colnames(data_train_x), collapse = "+")))
      model_fit <-  adabag::boosting(modelformula, data = data_train, 
                                     coeflearn = "Zhu"    ### different coeflean correspond to different kind of adaboost. see details in help document.
      )
      
      class_prediction_train <- predict(model_fit, newdata=data_train_x)$class
      class_prediction_test <- predict(model_fit, newdata=data_test_x)$class
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    
    ### method 19_1: original (RF)
    if(is.element("original_RF", names_methods)){
      
      ## R code
      tictoc::tic()
      # data_train <- cbind(data_train_y, data_train_x)
      # data_train$data_train_y <- as.factor(data_train$data_train_y)
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <- ranger::ranger(formula = modelformula, data = data_train)
          
          prediction_temp_temp <- predict(model_fit_temp, data = data_train)$predictions
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <- ranger::ranger(formula = modelformula, data = data_train)
      
      class_prediction_train <- predict(model_fit, data=data_train_x)$predictions
      class_prediction_test <- predict(model_fit, data=data_test_x)$predictions
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    
    ### method 19_2: original (SVM)
    if(is.element("original_SVM", names_methods)){
      
      ## R code
      tictoc::tic()

      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <-  e1071::svm(modelformula, data = data_train)
          
          prediction_temp_temp <- predict(model_fit_temp, newdata = data_train)
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <-  e1071::svm(modelformula, data = data_train)
      
      class_prediction_train <- predict(model_fit, newdata=data_train_x)
      class_prediction_test <- predict(model_fit, newdata=data_test_x)
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    
    ### method 20_1: over sampling (RF)
    if(is.element("over_sampling_RF", names_methods)){
      
      ## R code
      tictoc::tic()
      # data_train <- cbind(data_train_y, data_train_x)
      data_train_upSampling <- caret::upSample(x = data_train_x,
                                               y = as.factor(data_train_y))
      colnames(data_train_upSampling) <- c(colnames(data_train_x), "data_train_y")
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <- ranger::ranger(formula = modelformula, data = data_train_upSampling)
          
          prediction_temp_temp <- predict(model_fit_temp, data = data_train)$predictions
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <- ranger::ranger(formula = modelformula, data = data_train_upSampling)
      
      class_prediction_train <- predict(model_fit, data=data_train_x)$predictions
      class_prediction_test <- predict(model_fit, data=data_test_x)$predictions
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    ### method 20_2: over sampling (SVM)
    if(is.element("original_SVM", names_methods)){
      
      ## R code
      tictoc::tic()
      
      data_train_upSampling <- caret::upSample(x = data_train_x,
                                               y = as.factor(data_train_y))
      colnames(data_train_upSampling) <- c(colnames(data_train_x), "data_train_y")
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <-  e1071::svm(modelformula, data = data_train_upSampling)
          
          prediction_temp_temp <- predict(model_fit_temp, newdata = data_train)
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <-  e1071::svm(modelformula, data = data_train_upSampling)
      
      class_prediction_train <- predict(model_fit, newdata=data_train_x)
      class_prediction_test <- predict(model_fit, newdata=data_test_x)
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    
    ### method 21_1: under sampling (RF)
    if(is.element("down_sampling_RF", names_methods)){
      
      ## R code
      tictoc::tic()
      # data_train <- cbind(data_train_y, data_train_x)
      data_train_downSampling <- caret::downSample(x = data_train_x,
                                               y = as.factor(data_train_y))
      colnames(data_train_downSampling) <- c(colnames(data_train_x), "data_train_y")
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <- ranger::ranger(formula = modelformula, data = data_train_downSampling)
          
          prediction_temp_temp <- predict(model_fit_temp, data = data_train)$predictions
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <- ranger::ranger(formula = modelformula, data = data_train_downSampling)
      
      class_prediction_train <- predict(model_fit, data=data_train_x)$predictions
      class_prediction_test <- predict(model_fit, data=data_test_x)$predictions
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    
    ### method 21_2: under sampling (SVM)
    if(is.element("original_SVM", names_methods)){
      
      ## R code
      tictoc::tic()
      
      data_train_downSampling <- caret::downSample(x = data_train_x,
                                               y = as.factor(data_train_y))
      colnames(data_train_downSampling) <- c(colnames(data_train_x), "data_train_y")
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <-  e1071::svm(modelformula, data = data_train_downSampling)
          
          prediction_temp_temp <- predict(model_fit_temp, newdata = data_train)
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <-  e1071::svm(modelformula, data = data_train_downSampling)
      
      class_prediction_train <- predict(model_fit, newdata=data_train_x)
      class_prediction_test <- predict(model_fit, newdata=data_test_x)
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    ### method 22_1: cost sensitive (RF)
    if(is.element("cost_sensitive_RF", names_methods)){
      
      ## R code
      tictoc::tic()
      # data_train <- cbind(data_train_y, data_train_x)
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <- ranger::ranger(formula = modelformula, data = data_train, class.weights = 1/(table(data_train_y)/length(data_train_y)))
          
          prediction_temp_temp <- predict(model_fit_temp, data = data_train)$predictions
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <- ranger::ranger(formula = modelformula, data = data_train, class.weights = 1/(table(data_train_y)/length(data_train_y)))
      
      class_prediction_train <- predict(model_fit, data=data_train_x)$predictions
      class_prediction_test <- predict(model_fit, data=data_test_x)$predictions
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    ### method 22_2: cost sensitive (SVM)
    if(is.element("original_SVM", names_methods)){
      
      ## R code
      tictoc::tic()
      
      if(select_backward_wilcoxon) {
        auc_backward <- c()
        for (subset_i in seq_len(length(subsets_backward))) {
          cat(subset_i, "\n")
          names_x_select_temp <- as.character(results_Wilcoxon$names_x[1:subsets_backward[subset_i]])
          names_x_select_temp <- c(names_x_select_temp, names_x_fixed)
          modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select_temp, collapse = "+")))
          model_fit_temp <-  e1071::svm(modelformula, data = data_train, class.weights="inverse")
          
          prediction_temp_temp <- predict(model_fit_temp, newdata = data_train)
          auc_backward[subset_i] <- pROC::multiclass.roc(data_train$data_train_y, as.numeric(as.character(prediction_temp_temp)))$auc
        }
        names_x_select <- as.character(results_Wilcoxon$names_x[1:subsets_backward[which.max(auc_backward)]])
        names_x_select <- c(names_x_fixed, names_x_select)
        
      } else {
        names_x_select <- names_x
      }
      
      modelformula <- as.formula(paste("data_train_y", "~", paste(names_x_select, collapse = "+")))
      model_fit <-  e1071::svm(modelformula, data = data_train, class.weights="inverse")
      
      class_prediction_train <- predict(model_fit, newdata=data_train_x)
      class_prediction_test <- predict(model_fit, newdata=data_test_x)
      tictoc::toc()
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    
    
    ### method 5: AdaBoostNC
    if(is.element("AdaBoostNC", names_methods)){
      AdaBoostNC(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                 train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                 test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                 test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                 output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_AdaBoostNC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                 output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_AdaBoostNC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                 matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_AdaBoostNC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_AdaBoostNC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 6: AdaC2M1
    if(is.element("AdaC2M1", names_methods)){
      AdaC2M1(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
              train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
              test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
              test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
              output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_AdaC2M1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
              output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_AdaC2M1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
              matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_AdaC2M1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_AdaC2M1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 7: PIBoost
    if(is.element("PIBoost", names_methods)){
      PIBoost(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
              train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
              test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
              test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
              output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_PIBoost", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
              output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_PIBoost", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
              matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_PIBoost.", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_PIBoost", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 8: MCHDDT
    if(is.element("MCHDDT", names_methods)){
      MCHDDT(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
             train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
             test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
             test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
             output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_MCHDDT", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
             output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_MCHDDT", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
             matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_MCHDDT", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_MCHDDT", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 9: HDDTECOC
    if(is.element("HDDTECOC", names_methods)){
      HDDTECOC(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
               train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
               test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
               test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
               output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_HDDTECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
               output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_HDDTECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
               matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_HDDTECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_HDDTECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 10: HDDTOVA
    if(is.element("HDDTOVA", names_methods)){
      HDDTOVA(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
              train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
              test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
              test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
              output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_HDDTOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
              output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_HDDTOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
              matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_HDDTOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_HDDTOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 11: imECOCdense
    if(is.element("imECOCdense", names_methods)){
      imECOCdense(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                  train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                  test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                  test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                  output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_imECOCdense", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                  output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_imECOCdense", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                  matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_imECOCdense", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_imECOCdense", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 12: imECOCOVA
    if(is.element("imECOCOVA", names_methods)){
      imECOCOVA(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_AimECOCOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_imECOCOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_imECOCOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_imECOCOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 13: imECOCsparse
    if(is.element("imECOCsparse", names_methods)){
      imECOCsparse(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                   train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                   test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                   test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                   output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_imECOCsparse", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                   output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_imECOCsparse", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                   matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_imECOCsparse", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_imECOCsparse", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 14: fuzzyImbECOC
    if(is.element("fuzzyImbECOC", names_methods)){
      fuzzyImbECOC(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                   train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                   test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                   test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                   output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_fuzzyImbECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                   output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_fuzzyImbECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                   matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_fuzzyImbECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_fuzzyImbECOC", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 15: MultiImAO
    if(is.element("MultiImAO", names_methods)){
      MultiImAO(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_MultiImAO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_MultiImAO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_MultiImAO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_MultiImAO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    ### method 16: MultiImOAHO
    if(is.element("MultiImOAHO", names_methods)){
      MultiImOAHO(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                  train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                  test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                  test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                  output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_MultiImOAHO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                  output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_MultiImOAHO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                  matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_MultiImOAHO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_MultiImOAHO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ## method 17: MultiImOVA
    if(is.element("MultiImOVA", names_methods)){
      MultiImOVA(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                 train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                 test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                 test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                 output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_MultiImOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                 output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_MultiImOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                 matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_MultiImOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_MultiImOVA", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ## method 18: MultiImOVO
    if(is.element("MultiImOVO", names_methods)){
      MultiImOVO(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                 train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                 test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                 test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                 output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_MultiImOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                 output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_MultiImOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                 matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_MultiImOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_MultiImOVO", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }
    
    
    
    prediction_performance_rep_fold <- c()
    for (method_i in 1:ncol(class_prediction_test_AllMethods_fold_i)) {
      
      class_prediction_test_method_i <- class_prediction_test_AllMethods_fold_i[, method_i]
      
      F1_test <- F1(data_test_y, class_prediction_test_method_i)
      
      G_mean_test <- G_mean(data_test_y, class_prediction_test_method_i)
      
      AU1U_test <- AU1U(data_test_y, class_prediction_test_method_i)
      
      AUNU_test <- AUNU(data_test_y, class_prediction_test_method_i)
      
      AR_test <- AR(data_test_y, class_prediction_test_method_i)
      
      AAR_test <- AAR(data_test_y, class_prediction_test_method_i)
      
      KAPPA_test <- KAPPA(data_test_y, class_prediction_test_method_i)
      
      prediction_performance_rep_fold <- rbind(prediction_performance_rep_fold, c(F1_test, G_mean_test, AU1U_test, AUNU_test, AR_test, AAR_test, KAPPA_test))
           
    }
    
    prediction_performance_rep_fold <- as.data.frame(prediction_performance_rep_fold)
    prediction_performance_rep_fold <- cbind(names_methods, prediction_performance_rep_fold)
    prediction_performance_rep_fold <- cbind(cbind(rep(rep_i, ncol(class_prediction_test_AllMethods_fold_i)), rep(fold_i, ncol(class_prediction_test_AllMethods_fold_i))), prediction_performance_rep_fold)
    colnames(prediction_performance_rep_fold) <- c("rep", "fold", "names_methods", "F1_test", "G_mean_test", "AU1U_test", "AUNU_test", "AR_test", "AAR_test", "KAPPA_test")
    # write.xlsx(prediction_performance_rep, file = paste0(path, "/code/", pathname,"/prediction_performance", "_rep_", rep_i, ".xlsx"))
    write.table(prediction_performance_rep_fold, file = paste0(path, "/code/", pathname,"/prediction_performance", "_rep_", rep_i, "_fold_", fold_i, ".txt"))
    
    prediction_performance_average <- rbind(prediction_performance_average, prediction_performance_rep_fold)

    class_prediction_test_AllMethods <- rbind(class_prediction_test_AllMethods, class_prediction_test_AllMethods_fold_i)
    
  }
  
  colnames(class_prediction_test_AllMethods) <- names_methods
  
  write.xlsx(class_prediction_test_AllMethods, file = paste0("data/", pathname, "/class_prediction_test_AllMethods", "_rep_", rep_i, ".xlsx"))
  write.xlsx(y_test_stack, file = paste0("data/", pathname, "/y_test_stack", "_rep_", rep_i, ".xlsx"))
  
  ###  assess prediction performance
  prediction_performance_rep <- c()
  for (method_i in 1:ncol(class_prediction_test_AllMethods)) {
    
    class_prediction_test_method_i <- class_prediction_test_AllMethods[, method_i]
    
    F1_test <- F1(y_test_stack, class_prediction_test_method_i)
    
    G_mean_test <- G_mean(y_test_stack, class_prediction_test_method_i)
    
    AU1U_test <- AU1U(y_test_stack, class_prediction_test_method_i)
    
    AUNU_test <- AUNU(y_test_stack, class_prediction_test_method_i)
    
    AR_test <- AR(y_test_stack, class_prediction_test_method_i)
    
    AAR_test <- AAR(y_test_stack, class_prediction_test_method_i)
    
    KAPPA_test <- KAPPA(y_test_stack, class_prediction_test_method_i)
    
    prediction_performance_rep <- rbind(prediction_performance_rep, c(F1_test, G_mean_test, AU1U_test, AUNU_test, AR_test, AAR_test, KAPPA_test))
    
  }
  
  prediction_performance_rep <- as.data.frame(prediction_performance_rep)
  prediction_performance_rep <- cbind(names_methods, prediction_performance_rep)
  prediction_performance_rep <- cbind(rep(rep_i, ncol(class_prediction_test_AllMethods)), prediction_performance_rep)
  colnames(prediction_performance_rep) <- c("rep", "names_methods", "F1_test", "G_mean_test", "AU1U_test", "AUNU_test", "AR_test", "AAR_test", "KAPPA_test")
  # write.xlsx(prediction_performance_rep, file = paste0(path, "/code/", pathname,"/prediction_performance", "_rep_", rep_i, ".xlsx"))
  write.table(prediction_performance_rep, file = paste0(path, "/code/", pathname,"/prediction_performance", "_rep_", rep_i, ".txt"))
  
  prediction_performance <- rbind(prediction_performance, prediction_performance_rep)
  
}

# write.xlsx(prediction_performance, file = paste0(path,"/code/", pathname,"/prediction_performance4.xlsx"))
write.table(prediction_performance, file = paste0(path,"/code/", pathname,"/prediction_performance4.txt"))
write.table(prediction_performance_average, file = paste0(path,"/code/", pathname,"/prediction_performance_average4.txt"))



