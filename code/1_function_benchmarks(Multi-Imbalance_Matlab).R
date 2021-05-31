

##################  model 1: DECOC  ##################

DECOC <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "[trainTime, testTime, predictedResults_train] = DECOC(trainData, trainLabel, trainData,'sparse',1)",
    "[trainTime, testTime, predictedResults_test] = DECOC(trainData, trainLabel, testData,'sparse',1)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_DECOC.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_DECOC.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_DECOC.m"))
}


##################  model 2: DOVO  ##################
DOVO <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "[trainTime, testTime, predictedResults_train] = DOVO([trainData,trainLabel],trainData,trainLabel,5)",
    "[trainTime, testTime, predictedResults_test] = DOVO([trainData,trainLabel],testData,testLabel,5)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_DOVO.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_DOVO.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_DOVO.m"))
}



##################  model 3: AdaBoostM1  ##################
AdaBoostM1 <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "[trainTime,testTime,predictedResults_train] = adaBoostCartM1(trainData, trainLabel, trainData, 20)",
    "[trainTime,testTime,predictedResults_test] = adaBoostCartM1(trainData, trainLabel, testData, 20)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_AdaBoostM1.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_AdaBoostM1.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_AdaBoostM1.m"))
  
}



##################  model 4: SAMME  ##################
SAMME <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "[trainTime,testTime,predictedResults_train] = adaBoostCartM1(trainData, trainLabel, trainData, 20)",
    "[trainTime,testTime,predictedResults_test] = adaBoostCartM1(trainData, trainLabel, testData, 20)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_SAMME.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_SAMME.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_SAMME.m"))
}




##################  model 5: AdaBoostNC  ##################
AdaBoostNC <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "[trainTime,testTime,predictedResults_train] = adaBoostCartNC(trainData, trainLabel, trainData, 20, 2)",
    "[trainTime,testTime,predictedResults_test] = adaBoostCartNC(trainData, trainLabel, testData, 20, 2)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_AdaBoostNC.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_AdaBoostNC.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_AdaBoostNC.m"))
}



##################  model 6: AdaC2M1  ##################
AdaC2M1 <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = adaC2CartM1(trainData, trainLabel, trainData, 20, C0)",
    "[trainTime,testTime,predictedResults_test] = adaC2CartM1(trainData, trainLabel, testData, 20, C0)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_AdaC2M1.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_AdaC2M1.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_AdaC2M1.m"))
}



##################  model 7: PIBoost  ##################
PIBoost <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = PIBoostCart(trainData, trainLabel, trainData, 20)",
    "[trainTime,testTime,predictedResults_test] = PIBoostCart(trainData, trainLabel, testData, 20)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_PIBoost.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_PIBoost.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_PIBoost.m"))
}



##################  model 8: MCHDDT  ##################
MCHDDT <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = MCHDDT(trainData, trainLabel, trainData, trainLabel)",
    "[trainTime,testTime,predictedResults_test] = MCHDDT(trainData, trainLabel, testData, testLabel)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_MCHDDT.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_MCHDDT.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_MCHDDT.m"))
}




##################  model 9: HDDTECOC  ##################
HDDTECOC <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = HDDTECOC(trainData, trainLabel, trainData, trainLabel)",
    "[trainTime,testTime,predictedResults_test] = HDDTECOC(trainData, trainLabel, testData, testLabel)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_HDDTECOC.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_HDDTECOC.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_HDDTECOC.m"))
}



##################  model 10: HDDTOVA  ##################
HDDTOVA <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = HDDTOVA(trainData, trainLabel, trainData, trainLabel)",
    "[trainTime,testTime,predictedResults_test] = HDDTOVA(trainData, trainLabel, testData, testLabel)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_HDDTOVA.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_HDDTOVA.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_HDDTOVA.m"))
}



##################  model 11: imECOCdense  ##################
imECOCdense <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = imECOC(trainData, trainLabel, trainData,'dense',1)",
    "[trainTime,testTime,predictedResults_test] = imECOC(trainData, trainLabel, testData,'dense',1)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_imECOCdense.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_imECOCdense.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_imECOCdense.m"))
}



##################  model 12: imECOCOVA  ##################
imECOCOVA <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = imECOC(trainData, trainLabel, trainData,'OVA',1)",
    "[trainTime,testTime,predictedResults_test] = imECOC(trainData, trainLabel, testData,'OVA',1)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_imECOCOVA.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_imECOCOVA.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_imECOCOVA.m"))
}



##################  model 13: imECOCsparse  ##################
imECOCsparse <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = imECOC(trainData, trainLabel, trainData,'sparse',1)",
    "[trainTime,testTime,predictedResults_test] = imECOC(trainData, trainLabel, testData,'sparse',1)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_imECOCsparse.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_imECOCsparse.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_imECOCsparse.m"))
}




##################  model 14: fuzzyImbECOC  ##################
fuzzyImbECOC <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = fuzzyImbECOC(trainData, trainLabel, trainData, trainLabel, 'w6', 0.1)",
    "[trainTime,testTime,predictedResults_test] = fuzzyImbECOC(trainData, trainLabel, testData, testLabel, 'w6', 0.1)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_fuzzyImbECOC.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_fuzzyImbECOC.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_fuzzyImbECOC.m"))
}



##################  model 15: MultiImAO  ##################
MultiImAO <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = classAandO(trainData, trainLabel, trainData)",
    "[trainTime,testTime,predictedResults_test] = classAandO(trainData, trainLabel, testData)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_MultiImAO.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_MultiImAO.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_MultiImAO.m"))
}



##################  model 16: MultiImOAHO  ##################
MultiImOAHO <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = classOAHO(trainData, trainLabel, trainData)",
    "[trainTime,testTime,predictedResults_test] = classOAHO(trainData, trainLabel, testData)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_MultiImOAHO.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_MultiImOAHO.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_MultiImOAHO.m"))
}



##################  model 17: MultiImOVA  ##################
MultiImOVA <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = classOVA(trainData, trainLabel, trainData)",
    "[trainTime,testTime,predictedResults_test] = classOVA(trainData, trainLabel, testData)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_MultiImOVA.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_MultiImOVA.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_MultiImOVA.m"))
}




##################  model 18: MultiImOVO  ##################
MultiImOVO <- function(train_x_file, train_y_file, test_x_file, test_y_file, output_train_file, output_test_file, matlab_code_file_directory){
  matlab.lines <- c(
    "clear",
    paste0("cd '", path, "/code/Multi_Imbalance_examples'"),
    "javaaddpath('weka.jar')",
    "p = genpath(pwd)",
    "addpath(p, '-begin')",
    paste0("trainData = xlsread('", train_x_file, "');"),
    paste0("trainLabel = xlsread('", train_y_file, "');"),
    paste0("testData = xlsread('", test_x_file, "');"),
    paste0("testLabel = xlsread('", test_y_file, "');"),
    "C0=testGA(trainData, trainLabel);",
    "[trainTime,testTime,predictedResults_train] = classOAO(trainData, trainLabel, trainData)",
    "[trainTime,testTime,predictedResults_test] = classOAO(trainData, trainLabel, testData)",
    paste0("xlswrite('", output_train_file, "', predictedResults_train)"),
    paste0("xlswrite('", output_test_file, "', predictedResults_test)")
  )
  
  #create a MATLAB script containing all the commands in matlab.lines
  if(!dir.exists(matlab_code_file_directory)) dir.create(matlab_code_file_directory, recursive = TRUE)
  writeLines(matlab.lines, con=paste0(matlab_code_file_directory, "/matlab_script_MultiImOVO.m"))
  
  #run our MATLAB script
  # method 1
  # system("matlab -nodisplay -r \"run('F:/毕业论文/程序/pathname/matlab_script_MultiImOVO.m'); exit\"")
  
  # method 2
  library(matlabr)
  run_matlab_script(paste0(matlab_code_file_directory, "/matlab_script_MultiImOVO.m"))
}






