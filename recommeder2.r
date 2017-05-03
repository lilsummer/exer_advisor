sex = "1"
age = "Age 35 to 44"
height = '168'
weight = '68'
min = '45'
freq = '3'
type = 'Weight_lifting'
healthC = 'Yes'

recommender2 <- function(sex,age,height, weight,min,freq, type, healthC) {
    library(caret)
    library(randomForest)
    set.seed(126)
    
    ###########
    data = readRDS(file = './Data/sample_data_test_rf.rds')
    
    # F1 <- createDataPartition(data$X_PAINDX1_F, p = 0.5, groups = 2)
    # data_sub = data[F1$Resample1,]
    # F2 <- createDataPartition(data_sub$X_PAINDX1_F, p = 0.8, groups = 2)
    # 
    # select = c('X_PAINDX1_F_logic', 'X_HCVU651',
    #            'cutPADUR', 'cutPAFRE', 'X_AGE_G',
    #            'SEX',  'EXRACT11_N',
    #            'X_BMI5CAT')
    # 
    # select_col = c()
    # for (i in 1:length(names(data))) {
    #     if (names(data)[i] %in% select) {
    #         select_col = c(select_col, i)
    #     }
    # }
    # 
    # train = data_sub[F2$Resample1,select_col]
    # test = data_sub[-F2$Resample1,select_col]
    # y = 'X_PAINDX1_F_logic'
    # x = select[2:8]
    # fmla <- paste(y, paste(x, collapse = "+"), sep = '~')
    # model <- glm(fmla, data = train, family = binomial(link = 'logit'))
    ###########
    model <- readRDS('./Data/rf-model.rds')
    
    # sex = "2"
    # age = "Age 18 to 24"
    # height = '170'
    # weight = '60'
    # min = '30'
    # freq = '2'
    # type = 'Bicycling'
    # healthC = 'Yes'
    #
    ############################ ###########
    sex_t = as.numeric(sex)
    
    ################
    healthC_t = healthC == "1"
    healthC = ifelse(healthC_t, 1, 2)
    ################
    freq = as.numeric(freq)
    freq_cut = c(10, 16, 26, 150)
    freq_dist = freq - freq_cut
    freq_num = which(abs(freq_dist) == min(abs(freq_dist)))
    freq = levels(data$cutPAFRE)[freq_num]
    ###################
    min = as.numeric(min)
    min_cut = c(30, 45, 60, 270)
    min_dist = min - min_cut
    min_num = which(abs(min_dist) == min(abs(min_dist)))
    min = levels(data$cutPADUR)[min_num]
    ###########################
    Age <- readRDS('./Data/age.rds')
    age <- Age$ID[which(Age$NAME == age)]
    ####################
    weight = as.numeric(weight)
    height = as.numeric(height)
    BMI = weight / (height)^2 * 10000
    BMI_cat = c(18.5, 25, 30, 99)
    BMI_dist = BMI - BMI_cat
    bmi = which(abs(BMI_dist) == min(abs(BMI_dist))) + 1
    ###################
    Type <- readRDS('./Data/exercise.rds')
    type <- Type$ID[which(Type$NAME == type)]
    ##################
    if (type %in% c(34, 60, 67, 69)) {
        test = data
        test$SEX[1] = as.factor(sex)
        test$X_HCVU651[1] = as.factor(healthC)
        test$cutPADUR[1] = as.factor(min)
        test$cutPAFRE[1] = as.factor(freq)
        test$X_AGE_G[1] = as.factor(age)
        test$EXRACT11_N[1] = as.factor(levels(data$EXRACT11_N)[1])
        test$X_BMI5CAT[1] = as.factor(bmi)
        
        
        
        #test = test[,select_col]
        #lapply(test, factor)
        test$pred = predict(model, newdata = test[1,], type = 'response')
        
        if(test$pred[1] == FALSE ) {
            print('Increase your strength training level')}
        
        else{
            print('You did well in strength training! Try to work on your cardio exercise!')}
        
    }
    else {
        print('You selected a cardio exercise! Try to select a cardio exercise for better recommendation.')
        
       
        
        
    }
}
