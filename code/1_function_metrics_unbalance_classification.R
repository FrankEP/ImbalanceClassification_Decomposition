# path <- "D:/山大课题/毕业论文"

# setwd(path)

# load(file = "result_adaptive_threshold_moving.RData")


##### 1. F1
# label_true <- yvarall_im_scale_train$jb
# label_prediction <- class_prediction_train
# 
# label_true <- yvarall_im_scale_test$jb
# label_prediction <- class_prediction_test

F1 <- function(label_true, label_prediction){
  confusion_matrix <- table(factor(label_prediction, levels = c(1:length(table(label_true)))), label_true)
  sum_row <- apply(confusion_matrix, 1, sum)
  sum_col <- apply(confusion_matrix, 2, sum)
  PRECISION <- diag(confusion_matrix)/sum_row
  PRECISION <- ifelse(is.na(PRECISION), 0, PRECISION)
  RECALL <- diag(confusion_matrix)/sum_col
  Fi <- 2*PRECISION*RECALL/(PRECISION+RECALL)
  Fi <- ifelse(is.na(Fi), 0, Fi)
  F1 <- mean(Fi)
  
  return(F1)
}


##### 2. G_mean
# label_true <- yvarall_im_scale_train$jb
# label_prediction <- class_prediction_train
# 
# label_true <- yvarall_im_scale_test$jb
# label_prediction <- class_prediction_test

G_mean <- function(label_true, label_prediction){
  confusion_matrix <- table(factor(label_prediction, levels = c(1:length(table(label_true)))), label_true)
  sum_row <- apply(confusion_matrix, 1, sum)
  sum_col <- apply(confusion_matrix, 2, sum)
  # PRECISION <- diag(confusion_matrix)/sum_row
  # PRECISION <- ifelse(is.na(PRECISION), 0, PRECISION)
  RECALL <- diag(confusion_matrix)/sum_col
  G_mean <- prod(RECALL)^(1/length(RECALL))
  
  return(G_mean)
}

##### 3. MAUC
library(pROC)

# MAUC <- multiclass.roc(label_true, label_prediction)$auc
# 
# label_true <- yvarall_im_scale_train$jb
# label_prediction <- class_prediction_train
# 
# label_true <- yvarall_im_scale_test$jb
# label_prediction <- class_prediction_test

AU1U <- function(label_true, label_prediction){
  
  class_combinations <- combn(c(1:length(table(label_true))), 2)
  
  AUC_class <- c()
  for (combination_i in 1:ncol(class_combinations)) {
    cat(combination_i, "\n")
    class_1 <- class_combinations[1, combination_i]
    class_2 <- class_combinations[2, combination_i]
    
    label_true_binary <- label_true[is.element(label_true, c(class_1, class_2))]
    label_prediction_binary <- label_prediction[is.element(label_true, c(class_1, class_2))]
    
    label_true_binary <- factor(as.numeric(as.character(label_true_binary)))
    label_prediction_binary <- as.numeric(as.character(label_prediction_binary))
    
    AUC_class[combination_i] <- pROC::roc(label_true_binary, label_prediction_binary)$auc
    
  }
  
  AU1U <- mean(AUC_class)
  
  return(AU1U)
}

AUNU <- function(label_true, label_prediction){
  
  AUC_class <- c()
  for (class_i in 1:length(table(label_true))) {
    cat(class_i, "\n")
    label_true_binary <- ifelse(label_true==class_i, 1, 0)
    label_prediction_binary <- ifelse(label_prediction==class_i, 1, 0)
    
    AUC_class[class_i] <- pROC::roc(label_true_binary, label_prediction_binary)$auc
    
  }
  
  AUNU <- mean(AUC_class)
  
  return(AUNU)
}


##### 4. average recall (AR)
# label_true <- yvarall_im_scale_train$jb
# label_prediction <- class_prediction_train
# 
# label_true <- yvarall_im_scale_test$jb
# label_prediction <- class_prediction_test

AR <- function(label_true, label_prediction){
  confusion_matrix <- table(factor(label_prediction, levels = c(1:length(table(label_true)))), label_true)
  sum_row <- apply(confusion_matrix, 1, sum)
  sum_col <- apply(confusion_matrix, 2, sum)
  # PRECISION <- diag(confusion_matrix)/sum_row
  # PRECISION <- ifelse(is.na(PRECISION), 0, PRECISION)
  RECALL <- diag(confusion_matrix)/sum_col
  AR <- mean(RECALL)
  
  return(AR)
}


##### 5. average accuracy rate (AAR)
# label_true <- yvarall_im_scale_train$jb
# label_prediction <- class_prediction_train
# 
# label_true <- yvarall_im_scale_test$jb
# label_prediction <- class_prediction_test

AAR <- function(label_true, label_prediction){
  
  accuracy_class <- c()
  for (class_i in 1:length(table(label_true))) {
    cat(class_i, "\n")
    label_true_binary <- ifelse(label_true==class_i, 1, 0)
    label_prediction_binary <- ifelse(label_prediction==class_i, 1, 0)
    
    confusion_matrix <- table(factor(label_prediction, levels = c(1:length(table(label_true)))), label_true)
    accuracy_class[class_i] <- sum(diag(confusion_matrix))/length(label_true)
    
  }
  
  AAR <- mean(accuracy_class)
  
  return(AAR)
}


##### 6. Kappa
# label_true <- yvarall_im_scale_train$jb
# label_prediction <- class_prediction_train
# 
# label_true <- yvarall_im_scale_test$jb
# label_prediction <- class_prediction_test

KAPPA <- function(label_true, label_prediction){
  
  ABC <- c()
  TP <- c()
  for (class_i in 1:length(table(label_true))) {
    cat(class_i, "\n")
    label_true_binary <- ifelse(label_true==class_i, 1, 2)
    label_prediction_binary <- ifelse(label_prediction==class_i, 1, 2)
    
    confusion_matrix <- table(factor(label_prediction, levels = c(1:length(table(label_true)))), label_true)
    sum_row <- apply(confusion_matrix, 1, sum)
    sum_col <- apply(confusion_matrix, 2, sum)
    
    TP[class_i] <- diag(confusion_matrix)[1]
    
    ABC[class_i] <- sum_row[1]*sum_col[1]
    
  }
  
  KAPPA <- (length(label_true)*sum(TP)-sum(ABC)) / (length(label_true)^2-sum(ABC))
  
  return(KAPPA)
}














