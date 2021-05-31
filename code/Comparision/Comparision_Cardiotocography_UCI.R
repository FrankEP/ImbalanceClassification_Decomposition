

if (!requireNamespace("openxlsx", quietly = TRUE))
install.packages("openxlsx", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("dplyr", quietly = TRUE))
install.packages("dplyr", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("ggplot2", quietly = TRUE))
install.packages("ggplot2", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("data.table", quietly = TRUE))
install.packages("data.table", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("ranger", quietly = TRUE))
install.packages("ranger", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("pROC", quietly = TRUE))
install.packages("pROC", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("e1071", quietly = TRUE))
install.packages("e1071", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("adabag", quietly = TRUE))
install.packages("adabag", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("metaheuristicOpt", quietly = TRUE))
install.packages("metaheuristicOpt", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("foreach", quietly = TRUE))
install.packages("foreach", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("doParallel", quietly = TRUE))
install.packages("doParallel", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("caret", quietly = TRUE))
install.packages("caret", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("glmnet", quietly = TRUE))
install.packages("glmnet", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("GA", quietly = TRUE))
install.packages("GA", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("tictoc", quietly = TRUE))
install.packages("tictoc", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("matlabr", quietly = TRUE))
install.packages("matlabr", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")

if (!requireNamespace("CVXR", quietly = TRUE))
  install.packages("CVXR", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN")
  
  


path <- "/home/jtw/biyelunwen"

# path <- "F:/biyelunwen"

setwd(path)

# options(matlab.path='/usr/lib64/matlab2019b/bin/')


library(openxlsx)
library(dplyr)
library(ggplot2)
library(data.table)
library(ranger)

# source(paste0(path,'/code/1_function_adaptive_threshold_moving_bootstrap??.R', echo=TRUE))
# source(paste0(path,'/code/1_function_adaptive_threshold_moving_????_??ɸѡ??_???ι�?R', echo=TRUE))
# source(paste0(path,'/code/1_function_adaptive_threshold_moving_????_??ɸѡ??_???ι滮_PSO.R', echo=TRUE))

print("0")
source(paste0(path,'/code/test1.R'))
print("1")
source(paste0(path,'/code/1_function_metrics_unbalance_classification.R'))
print("2")
# source(paste0(path,'/code/1_function_?????�?Multi-Imbalance Matlabcode).R'))
print("3")
source(paste0(path,'/code/aaaa_GA_Rpackage_20200910.R'))

### ????data
library(ranger)
library(pROC)

pathname <-  "UCI_Cardiotocography"

# data_all <- read.xlsx(paste0(path,"/data_open/", pathname,"/UCI_cardiotocography.xlsx", sheetIndex=1))

data_all <- openxlsx::read.xlsx(paste0(path,"/data_open/", pathname,"/UCI_cardiotocography.xlsx"))

name_y_data_all <- "CLASS"
names_x_data_all <- colnames(data_all)[c(1:21, 23)]


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

names_methods <- c("ATM")
# names_methods <- c("AdaBoostM1", "SAMME", "AdaBoostNC")
# names_methods <- c("ATM")

rep_i <- 1
fold_i <- 1
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
    
    if(!dir.exists(paste0("data/", pathname))) dir.create(paste0("data/", pathname), recursive = TRUE)
    #write.xlsx(data_train_x, file = paste0("data/", pathname, "/data_train_x.xlsx"), row.names=FALSE, col.names=FALSE)
    #write.xlsx(data_train_y_numerical, file = paste0("data/", pathname, "/data_train_y.xlsx"), row.names=FALSE, col.names=FALSE)
    #write.xlsx(data_test_x, file = paste0("data/", pathname, "/data_test_x.xlsx"), row.names=FALSE, col.names=FALSE)
    #write.xlsx(data_test_y_numerical, file = paste0("data/", pathname, "/data_test_y.xlsx"), row.names=FALSE, col.names=FALSE)
    
    y_test_stack <- c(y_test_stack, data_test_y)
    
    class_prediction_test_AllMethods_fold_i <- c()
    
    ### method 0: Adaptivie Threshold Moving (ATM)
    if(is.element("ATM", names_methods)){
      results_ATM <- Adaptive_threshold_moving(X=data_train_x, Y=data_train_y, NEW_X=data_test_x, num_bootstrap=1, pathname = pathname, fold_i=fold_i, rep_i=rep_i)
      X=data_train_x
      Y=data_train_y
      NEW_X=data_test_x
      num_bootstrap=1
      pathname = pathname
      class_prediction_test <- results_ATM$class_prediction_test
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
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
    if(is.element("AdaBoostM1", names_methods)){
      AdaBoostM1(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
                 train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
                 test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
                 test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
                 output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
                 output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
                 matlab_code_file_directory=paste0(path, "/code/", pathname))
      
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_AdaBoostM1", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
      class_prediction_test_AllMethods_fold_i <- cbind(class_prediction_test_AllMethods_fold_i, class_prediction_test)
    }

    
    
    ### method 4: SAMME
    if(is.element("SAMME", names_methods)){
      SAMME(train_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_x.xlsx"), fixed=TRUE), 
            train_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_train_y.xlsx"), fixed=TRUE), 
            test_x_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_x.xlsx"), fixed=TRUE), 
            test_y_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/data_test_y.xlsx"), fixed=TRUE), 
            output_train_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_train_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE), 
            output_test_file=gsub("/", "\\", paste0(path, "/data/", pathname, "/", "class_prediction_test_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), fixed=TRUE),
            matlab_code_file_directory=paste0(path, "/code/", pathname))
      class_prediction_train <- read.xlsx(paste0("data/", pathname, "/class_prediction_train_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_train <- class_prediction_train$X1
      
      class_prediction_test <- read.xlsx(paste0("data/", pathname, "/class_prediction_test_SAMME", "_rep_", rep_i, "_fold_", fold_i, ".xlsx"), colNames = FALSE)
      class_prediction_test <- class_prediction_test$X1
      
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
    # write.xlsx(prediction_performance_rep, file = paste0(path, "/results/", pathname,"/prediction_performance", "_rep_", rep_i, ".xlsx"))
    write.table(prediction_performance_rep_fold, file = paste0(path, "/results/", pathname,"/prediction_performance", "_rep_", rep_i, "_fold_", fold_i, ".txt"))
    
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
  # write.xlsx(prediction_performance_rep, file = paste0(path, "/results/", pathname,"/prediction_performance", "_rep_", rep_i, ".xlsx"))
  write.table(prediction_performance_rep, file = paste0(path, "/results/", pathname,"/prediction_performance", "_rep_", rep_i, ".txt"))
  
  prediction_performance <- rbind(prediction_performance, prediction_performance_rep)
  
}

# write.xlsx(prediction_performance, file = paste0(path,"/results/", pathname,"/prediction_performance4.xlsx"))
write.table(prediction_performance, file = paste0(path,"/results/", pathname,"/prediction_performance4.txt"))
write.table(prediction_performance_average, file = paste0(path,"/results/", pathname,"/prediction_performance_average4.txt"))



