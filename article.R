rm(list = ls())

library(skimr)
library(tableone)
library(randomForest)
library(xgboost)
library(shapviz)
library(ggplot2)
library(caret)
library(MASS)
library(pROC)
library(e1071)
library(nnet)
library(rpart)
library(rpart.plot)
library(ROSE)
library(jsonlite)
library(dcurves)
library(gtsummary)
library(tidyverse)
library(mlr3verse)
library(tidyverse)
library(data.table)
library(iml)
library(DALEX)
library(DALEXtra)
library(yardstick)
library(tidyr)
library(dplyr)
library(mlr3)
library(fastshap)
library(openxlsx)
library(rms)
{
#read the data
file_data <-  "data file path"
data <- read.csv(file_data,header = T)
skim(data)
names(data)
str(data)

for (i in names(data)[c(4:17)]){data[,i] <- as.factor(data[,i])}

data$Medical.insurance.type<-factor(data$Medical.insurance.type,labels=c("Residents","Employees"))
data$Pathologictype<-factor(data$Pathologictype,labels=c("SCC","Adenocarcinoma or othes"))
data$Stage<-factor(data$Stage,labels=c("I-II","III-IV"))
data$Chemotherapy<-factor(data$Chemotherapy,labels=c("NO","YES"))
data$Radiotherapy<-factor(data$Radiotherapy,labels=c("NO","YES"))
data$Targetedtherapy<-factor(data$Targetedtherapy,labels=c("NO","YES"))
data$Immunotherapy<-factor(data$Immunotherapy,labels=c("NO","YES"))
data$BMI<-factor(data$BMI,labels=c("18.5-23.9","24-27.9","≥28","<18.5"))
data$Marriage<-factor(data$Marriage,labels=c("Married","Unmarried/divorced and others"))
data$Surgerytypes<-factor(data$Surgerytypes,labels=c("Open surgery","Laparoscopic surgery","Robotic surgery"))
data$WBC<-factor(data$WBC,labels=c("≤10",">10"))
data$PLT<-factor(data$PLT,labels=c("≤300",">300"))
data$D.dimer<-factor(data$D.dimer,labels=c("≤0.5",">0.5"))

#Randomly split the data.
set.seed(828)
trains<-createDataPartition(y=data$VTE,p=0.7,list = F)
train<-data[trains,]
vali<-data[-trains,]
prop.table(table(train$VTE))
prop.table((table(vali$VTE)))


#baseline description
train$group=1
vali$group=2
datasum <- rbind(train,vali)
skim(datasum)
summary(datasum)

# variables in Table 1
myVars <- c("Age","BMI","Marriage","Pathologictype","Stage","Surgerytypes","WBC",
            "PLT","Radiotherapy","Chemotherapy","Targetedtherapy","Immunotherapy",
            "D.dimer","Microglobulin","NLR","Surgerytime")

#categorical variable
catVars <- c("BMI","Marriage","Pathologictype","Stage","Surgerytypes",
             "Radiotherapy","Chemotherapy","Targetedtherapy","Immunotherapy","WBC",
             "PLT","D.dimer")

nonvar <- c("Age","Microglobulin","NLR","Surgerytime")

# table 
table1<- CreateTableOne(vars = myVars,
                        factorVars = catVars,
                        strata = "VTE",
                        data = datasum,
                        addOverall = TRUE) 

table1 <- print(table1,
                nonnormal = nonvar,
                #exact = exactvars,#Fisher
                catDigits = 2, contDigits = 3, pDigits = 4,
                showAllLevels=TRUE, 
                quote = FALSE, 
                noSpaces = TRUE, 
                printToggle = TRUE) 
write.csv(table1, file = "table/table1.csv",fileEncoding="UTF-8")
table2<- CreateTableOne(vars = myVars,
                        factorVars = catVars,
                        strata = "group", 
                        data = datasum,
                        addOverall = TRUE)
table2 <- print(table2,
                nonnormal = nonvar,
                #exact = exactvars,
                catDigits = 2, contDigits = 3, pDigits = 4,
                showAllLevels=TRUE, 
                quote = FALSE, 
                noSpaces = TRUE,
                printToggle = TRUE)
write.csv(table2, file = "table/table2.csv")

table3<- CreateTableOne(vars = myVars,
                        factorVars = catVars,
                        strata = "VTE",
                        data = train,
                        addOverall = TRUE)

table3 <- print(table3,
                nonnormal = nonvar,
                #exact = exactvars,
                catDigits = 2, contDigits = 3, pDigits = 4,
                showAllLevels=TRUE, 
                quote = FALSE, 
                noSpaces = TRUE,
                printToggle = TRUE)
write.csv(table3, file = "table/table3.csv")

#3.select variable
Outcome <- "VTE"
CandidateVariables <- c("Age","BMI","Marriage","Pathologictype","Stage","Surgerytypes",
                        "Radiotherapy","Chemotherapy","Targetedtherapy","Immunotherapy","WBC",
                        "PLT","D.dimer","Microglobulin","NLR","Surgerytime")

# create a formula
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))


# fit a model with all candidate varaibles
model.full <- glm(Formula, data=train, family=binomial)
summary(model.full)

# 1.1 stepwise selection
model.step <- stepAIC(model.full,data=train,direction="both")
summary(model.step)

library(autoReg)
library(export)
fit2<-autoReg(model.full,uni=TRUE,final=T) 
myft(fit2)
table2doc(fit2,"table/table4.docx")


# scale
head(train)
variables_to_standardize <- c("Age", "Microglobulin",'NLR')


# max and min in the train
train_max <- train %>% select(all_of(variables_to_standardize)) %>% summarise_all(max)
train_min <- train %>% select(all_of(variables_to_standardize)) %>% summarise_all(min)

# scale train
train1<-train %>% mutate(Age=(Age-train_min[[1]])/(train_max[[1]]-train_min[[1]]),
                         Microglobulin=(Microglobulin-train_min[[2]])/(train_max[[2]]-train_min[[2]]),
                         NLR=(NLR-train_min[[3]])/(train_max[[3]]-train_min[[3]]))

# scale vali
vali1<-vali %>% mutate(Age=(Age-train_min[[1]])/(train_max[[1]]-train_min[[1]]),
                         Microglobulin=(Microglobulin-train_min[[2]])/(train_max[[2]]-train_min[[2]]),
                         NLR=(NLR-train_min[[3]])/(train_max[[3]]-train_min[[3]]))

train<-train1
vali=vali1
newtrain<-train

summary(model.step)
variablename<-c("Age","Pathologictype",
                "Stage","Chemotherapy","D.dimer","NLR")

train <- train[,c("VTE",variablename)]
vali <- vali[,c("VTE",variablename)]
newtrain<-data.frame(VTE=train$VTE) %>% bind_cols(model.matrix(VTE~., data = train)[,-1])
names(newtrain)<-c("VTE","Age","Pathologictype","Stage","Chemotherapy","D.dimer","NLR")
vali<-data.frame(VTE=vali$VTE) %>% bind_cols(model.matrix(VTE~.,
                                                          data = vali)[,-1])
names(vali)<-c("VTE","Age","Pathologictype","Stage","Chemotherapy","D.dimer","NLR")

head(newtrain)
str(newtrain)

custom_color <- c("#79BDA4","#F0945C","#F5EF80",
                  "#E34041","#C8C2E0","#9FD5C9","#C286BB")
custom_models_name <- c('LR','RF','NN','XGBoost','GBM','SVM','Adaboost')
}
#10 CV, tune hyperparameter
# 10 CV
tsk_newtrain <- as_task_classif(newtrain, target = 'VTE')
tsk_newtrain$set_col_roles('VTE', c('target', 'stratum'))
rsmp_cv10 = rsmp("cv",
                 folds = 10)
rsmp_cv10$instantiate(tsk_newtrain)

prop.table(table(tsk_newtrain$data(rows = rsmp_cv10$test_set(1), cols = 'VTE')))
prop.table(table(tsk_newtrain$data(rows = rsmp_cv10$test_set(5), cols = 'VTE')))
prop.table(table(tsk_newtrain$data(rows = rsmp_cv10$test_set(6), cols = 'VTE')))

cv_log <- function(data)
{
  metric_log_cv <- c()
  for (i in 1:10) {
    train_temp <- data[-rsmp_cv10$test_set(i),]
    vali_temp <- data[rsmp_cv10$test_set(i),]
    set.seed(999)
    m1 <- glm(VTE~.,data = train_temp,family = "binomial")
    
    #AUC
    prob1 <- predict(m1,newdata = vali_temp,type="response")
    roc1 <- roc(response = vali_temp$VTE, predictor = prob1,ci=T, quiet=TRUE)
    metric_log_cv <- c(metric_log_cv,as.numeric(roc1$auc))
  }
  return(metric_log_cv)
}
log_cv_auc <- c(mean(cv_log(newtrain)), sd(cv_log(newtrain)))
log_cv_auc <- as.data.frame(t(log_cv_auc))
names(log_cv_auc) <- c('ROC', 'SD')
write.xlsx(log_cv_auc, 'table/log_hyperparameter_cv_10.xlsx')

#train optimal model
set.seed(999)
model.log <- glm(VTE~.,data = newtrain,family = "binomial")
summary(model.log)

#residual
par(mfrow=c(2,2))
plot(model.log)

library(ROCR)
#ROC of test
log.vali.prob <- predict(model.log, newdata = vali, type = "response")
log_pred <- prediction(log.vali.prob,vali$VTE)
log_perf <- performance(log_pred, "tpr","fpr")
log_auc <- round(performance(log_pred, "auc")@y.values[[1]],digits = 4)
log_auc#auc

png("figure/log.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(log_perf,lwd=2,col=custom_color[1], cex.axis = 1.5, cex.lab=1.5)
abline(0,1,lty=2)
legend("bottomright", legend=paste("Test Dataset AUC: ",log_auc),
       col=custom_color[1], lwd=2,bty = "n", 
       # text.col = 'black', 
       cex=2
       )
title(list('ROC Curve of LR Model', cex=1.5))
dev.off()

#PR
log_perf_2 <- performance(log_pred, "prec","rec")
log_aupr <- round(performance(log_pred, "aucpr")@y.values[[1]],digits = 4)
log_aupr

png("figure/log.vali.prc.png",units = "in",width = 10,height = 7,res = 600)
plot(log_perf_2,lwd=2,col=custom_color[1],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
legend("bottomright", legend=paste("Test Dataset AUPR: ",log_aupr),
       col=custom_color[1], lwd=2,bty = "n", cex=2)
title(list('PR Curve of LR Model', cex=1.5))
dev.off()


#calibration
png("figure/log.vali.calibration.png",units = "in",width = 10,
    height = 7,res = 600)
log_calibration<- val.prob(log.vali.prob,vali$VTE,logistic.cal = T,
         pl=T, legendloc =c(0.55, 0.27), statloc = F)
title('LR Model Calibration Curves', col.main = '#213012')
dev.off()

#dca
data_lr_dca_vali<-data.frame(Outcome=vali$VTE,
                                     predictors=log.vali.prob,model="LR")
png("figure/log.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
log_dca_data <- dcurves::dca(data=data_lr_dca_vali,Outcome~predictors,
             label = list(predictors='LR')) %>% plot(smooth=T)+
  ggplot2::labs(title = 'LR Model DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                'LR'=custom_color[1]))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
log_dca_data
dev.off()

#lr model interpretation
log_explain <- DALEX::explain(model.log,
                              data = newtrain[,-1],
                              y=newtrain$VTE,
                              label = 'LR')

#diagnosis residual
#continuous y
log_md <- model_diagnostics(log_explain)
plot(log_md)

#feature importance
log_mp <- model_parts(log_explain,
                      type='raw',
                      B=50,
                      # variable_groups=list(aaa=c('D.dimer','Stage')),
                      N=NULL)
log_featurer_importance <- plot(log_mp)+
  facet_wrap(~label, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c('LR'='LR model')))+
  labs(subtitle = ""
       # tag = 'a',
       # caption = 'd'
       )+
  scale_color_manual(values = custom_color[1])+
  # ylab("")+
  # xlab("")+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/log.vali.feature.importance.png",plot = log_featurer_importance,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png) 

#pd
categories <- 'D.dimer'
pd_variables <- 'Age'
pd_breaks <- ifelse(pd_variables %in% categories, 
                     list(c(0, 1)), 
                     list(c(0.00, 0.25, 0.50, 0.75, 1.00))) [[1]]
pd_group <- 'Pathologictype'

#Age NLR
pd_trans_id <- match(pd_variables, names(train_max))
pd_variable_label <- round(c(0.00, 0.25, 0.50, 0.75, 1.00)*(train_max- train_min)[[pd_trans_id]] +train_min[[pd_trans_id]],
                        0)
pd_variable_label
#D.dimer
pd_variable_label <- c('≤0.5','>0.5')

# pd_legend_label <- c('Stage:I-II', 'Stage: III-IV')
# pd_legend_label <- c('Chemotherapy: No', 'Chemotherapy: Yes')
pd_legend_label <- c('Pathologic type: SCC', 'Pathologic type: Others')
# pd_legend_label <- c('D.dimer:≤0.5', 'D.dimer:>0.5')
pd_title <- paste0('Partial Dependence profile for ', pd_variables)
pd_title_group <- paste0(pd_title, ' grouped by ',pd_group)


log_pd <- model_profile(log_explain,
                        type = 'partial',
                        N=NULL)
log_pd$color <- custom_color[1]
log_pd_figure <- plot(log_pd, variables=pd_variables)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title =pd_title, 
       subtitle = "LR Model"
       # tag = 'a',
       # caption = 'd'
  )+
  # ylab("")+
  xlab("")+
  scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(breaks = pd_breaks,
                     labels = pd_variable_label)+
  # scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00), 
  #                    labels = pd_variable_label)+ 
  theme(panel.background = element_rect(fill = "white", color = NA),
        # plot.subtitle = element_text(colour = 'red'),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/log.vali.pd_figure.png",plot = log_pd_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png) 
plot(log_pd, variables = pd_variables, geom = "profiles") +
  ggtitle(paste0("Ceteris-paribus and partial-dependence profiles for ", pd_variables))


##group pd, cp figure
##group stage
log_pd_stage <- model_profile(log_explain, 
                              variables = pd_variables,
                              groups = pd_group, 
                              N=NULL)
log_pd_Stage_figure <- plot(log_pd_stage, variables = pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  scale_color_manual(name = "", 
                     values = DALEX::colors_discrete_drwhy(2),
                     labels = pd_legend_label)+
  labs(title = pd_title_group,
       subtitle = "LR Model"
       # tag = 'a',
       # caption = 'd'
  )+
  # ylab("")+
  xlab("")+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12))
ggsave("figure/log.vali.pd_Stage_figure.png",plot = log_pd_Stage_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
log_pd_Stage_figure

{
  ##cluster pd figure
  log_pd_clu <- model_profile(log_explain,
                              # variables = 'Age',
                              k=2,
                              N=NULL)
  plot(log_pd_clu, geom='profiles')
  log_pd_clu

#group sex
# non_data <- data[trains,]
# non_marry_data <- newtrain[non_data$Stage != 'I-II',]
# log_explain_male <- DALEX::explain(model.log,
#                               data = non_marry_data[,-1],
#                               y=non_marry_data$VTE,
#                               label = 'LR_male')
# log_pd_male <- model_profile(log_explain_male,
#                              N=NULL,
#                              variables = 'Age')
# marry_data <- newtrain[non_data$Stage == 'I-II',]
# log_explain_female <- DALEX::explain(model.log,
#                               data = marry_data[,-1],
#                               y=marry_data$VTE,
#                               label = 'LR_female')
# log_pd_female <- model_profile(log_explain_female,
#                              N=NULL,
#                              variables = 'Age')
# plot(log_pd_male, log_pd_female)

#comparable pd figure

#LD figure
log_ld <- model_profile(log_explain,
                        type='conditional',
                        N=NULL)
plot(log_ld, variables=c('Age'))
#al figure
log_al <- model_profile(log_explain,
                        type='accumulated',
                        N=NULL
                        # groups = 'Stage'
                        )
plot(log_al, variables=c('Age'))
#combined
log_pd$agr_profiles$`_label_`='LR partial dependence'
log_ld$agr_profiles$`_label_`='LR local dependence'
log_al$agr_profiles$`_label_`='LR accumulated local'
plot(log_pd, log_ld, log_al, variables='Age')
}
#instance interpretation###############
#bd figure
log_ins_bd <- predict_parts(log_explain,
                         new_observation = newtrain[1,-1],
                         type = 'break_down',
                         order=c('Age','D.dimer','Pathologictype',
                                 'Stage','Chemotherapy','NLR'))
log_ins_bd <- predict_parts(log_explain,
                         new_observation = newtrain[1,-1],
                         type = 'break_down',
                         order=c('D.dimer','NLR','Pathologictype',
                                 'Chemotherapy','Stage','Age')
                         # keep_distribution=TRUE
                         )
plot(log_ins_bd)
plot(log_ins_bd, plot_distributions=TRUE)

#interaction bd figure
log_ins_inbd <- DALEX::predict_parts(log_explain,
                            new_observation = newtrain[1,-1],
                            type  = "break_down_interactions")
                            # keep_distribution=TRUE
plot(log_ins_inbd)

#shap bd figure
log_ins_shap <- predict_parts(log_explain,
                         new_observation = newtrain[1,-1],
                         type = 'shap',
                         B=25)
plot(log_ins_shap)
plot(log_ins_shap, show_boxplots=F)

#cp figure
log_ins_cp <- predict_profile(log_explain,
                              new_observation = newtrain[1,-1],
                              variables = names(newtrain)[-1])

plot(log_ins_cp, variables='NLR')
# plot(log_ins_cp, variables='D.dimer',
#      variable_type = "categorical", 
#      categorical_type = "bars")

# compare instance figure
# cp_titanic_rf2 <- predict_profile(explainer = explain_rf,
#                                   new_observation = rbind(henry, johnny_d),
#                                   variable_splits = variable_splits)
# plot(cp_titanic_rf2, color = "_ids_", variables = c("age", "fare")) +
#   scale_color_manual(name = "Passenger:", breaks = 1:2,
#                      values = c("#4378bf", "#8bdcbe"),
#                      labels = c("henry" , "johny_d"))

# cp_titanic_rf <- predict_profile(explain_rf, henry)
# cp_titanic_lmr <- predict_profile(explain_lmr, henry)
# plot(cp_titanic_rf, cp_titanic_lmr, color = "_label_",  
#      variables = c("age", "fare")) +
#   ggtitle("Ceteris-paribus profiles for Henry", "") 

#cp shock
log_ins_shockcp <- predict_parts(explainer = log_explain,
                                      new_observation = newtrain[1,-1],
                                      type = "oscillations_uni")
plot(log_ins_shockcp)

#local stability
log_explain <- DALEX::explain(model.log,
                              data = newtrain[,-1],
                              y=newtrain$VTE,
                              residual_function = 
                              label = 'LR')

log_ins_part <- predict_diagnostics(log_explain,
                                    new_observation = newtrain[1,-1],
                                    neighbors = 50,
                                    variables = 'D.dimer')
# log_ins_part <- predict_diagnostics(log_explain,
                                    # new_observation = newtrain[1,-1],
                                    # neighbors = 5)
plot(log_ins_part)

#RandomForest======================================================================================================
# 10 CV tune hyperparameter
# Set Training Control
myTrainingControl <- trainControl(method = "cv", 
                                  number = 10, 
                                  savePredictions = TRUE, 
                                  classProbs = TRUE, 
                                  verboseIter = FALSE,
                                  search='random',
                                  summaryFunction = twoClassSummary)
# gird search
# rf_tuneGrid = expand.grid(mtry = c(1:5), min.node.size = c(20,25,30),
#                           splitrule=c('gini','extratrees'))
# Train RF
set.seed(999)
fit_RF <- caret::train(factor(VTE,labels = c('no','yes')) ~ .,   
                       data = newtrain, 
                       method = "ranger", 
                       # tuneGrid =rf_tuneGrid,
                       tuneLength = 10,
                       importance = "permutation",
                       trControl = myTrainingControl,
                       num.trees = 10,
                       metric='ROC')

print(fit_RF$bestTune)
#final model
fit_RF$final_model
rf_cv_auc <- fit_RF$results
write.xlsx(rf_cv_auc, 'table/rf_hyperparameter_cv_10.xlsx')



rf.vali.prob <- predict(fit_RF ,newdata=vali,type = "prob")[,2]
rf_pred <- prediction(rf.vali.prob,vali$VTE) 
rf_perf <- performance(rf_pred, "tpr","fpr")
rf_auc <- round(performance(rf_pred, "auc")@y.values[[1]],digits = 4)
rf_auc#auc

png("figure/rf.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(rf_perf,lwd=2,col=custom_color[2],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
abline(0,1,lty=2)
legend("bottomright", legend=paste("Test Dataset AUC: ",rf_auc), 
       col=custom_color[2], lwd=2,bty = "n", cex=2)
title(list('ROC Curves For RF Model', cex=1.5))
dev.off()

#PR
rf_perf_2 <- performance(rf_pred, "prec","rec")
rf_aupr <- round(performance(rf_pred, "aucpr")@y.values[[1]],digits = 4)
rf_aupr

png("figure/rf.vali.prc.png",units = "in",width = 10,height = 7,res = 600)
plot(rf_perf_2,lwd=2,col=custom_color[2],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
legend("bottomright", legend=paste("Test Dataset AUPR: ",rf_aupr), 
       col=custom_color[2], lwd=2,bty = "n", cex=2)
title(list('PR Curves For RF Model', cex=1.5))
dev.off()
#calibration

png("figure/rf.vali.calibration.png",units = "in",width = 10,height = 7,res = 600)
rf_calibration<- val.prob(rf.vali.prob,vali$VTE,logistic.cal = T,
         pl=T, legendloc =c(0.55, 0.27), statloc = F)
title('RF Model Calibration Curves', col.main = '#213012')
dev.off()

#dca
data_rf_dca_vali<-data.frame(Outcome=vali$VTE,
                                     predictors=rf.vali.prob,model="RF")

png("figure/rf.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
rf_dca_data <- dcurves::dca(data=data_rf_dca_vali,Outcome~predictors,
             label = list(predictors='RF')) %>% plot(smooth=T)+
  ggplot2::labs(title = 'RF Model DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                'RF'=custom_color[2]))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
rf_dca_data
dev.off()

#rf model interpretation####
rf_explain <- DALEX::explain(fit_RF,
                              data = newtrain[,-1],
                              y=newtrain$VTE,
                              label = 'RF')

#feature importance
set.seed(999)
rf_mp <- model_parts(rf_explain,
                      type='raw',
                      B=50,
                      N=NULL)
rf_featurer_importance <- plot(rf_mp)+
  facet_wrap(~label, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c('RF'='RF model')))+
  labs(subtitle = "")+
  scale_color_manual(values = custom_color[2])+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/rf.vali.feature.importance.png",plot = rf_featurer_importance,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#pd
RF_pd <- model_profile(rf_explain,
                        type = 'partial',
                        N=NULL)
RF_pd$color <- custom_color[2] 
RF_pd_figure <- plot(RF_pd, variables= pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = pd_title, 
       subtitle = "RF Model")+
  scale_y_continuous(limits = c(0,1))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/rf.vali.pd_figure.png",plot = RF_pd_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
RF_pd_figure
# plot(RF_pd, variables='Age', geom = "profiles") + 
#   ggtitle("Ceteris-paribus and partial-dependence profiles for age")


##group pd, cp figure
##group stage
RF_pd_stage <- model_profile(rf_explain, 
                              variables = pd_variables,
                              groups = pd_group, 
                              N=NULL)
rf_pd_Stage_figure <- plot(RF_pd_stage, variables = pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  scale_color_manual(name = "", 
                     values = DALEX::colors_discrete_drwhy(2),
                     labels = pd_legend_label)+
  labs(title = pd_title_group, subtitle = "RF Model")+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12))
ggsave("figure/rf.vali.pd_Stage_figure.png",plot = rf_pd_Stage_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
rf_pd_Stage_figure
#BP-NetWork======================================================================================================
# 10 CV tune hyperparameter
set.seed(999)
fit_bp_model <- caret::train(
  factor(VTE ,labels = c('no','yes'))~ .,
  data = newtrain,
  method = "nnet",
  trControl = myTrainingControl,
  tuneLength=10,
  metric='ROC')

nnet_cv_auc <- fit_bp_model$results
write.xlsx(nnet_cv_auc, 'table/nnet_hyperparameter_cv_10.xlsx')

# ROC
nnet.vali.prob <- predict(fit_bp_model ,newdata=vali,type = "prob")[,2]
nnet_pred <- prediction(nnet.vali.prob,vali$VTE) 
nnet_perf <- performance(nnet_pred, "tpr","fpr")
nnet_auc <- round(performance(nnet_pred, "auc")@y.values[[1]],digits = 4)
nnet_auc#auc

png("figure/nnet.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(nnet_perf,lwd=2,col=custom_color[3],cex.axis = 1.5, cex.lab=1.5)
abline(0,1,lty=2)
legend("bottomright", legend=paste("Test Dataset AUC: ",nnet_auc),
       col=custom_color[3], cex=2, lwd=2,bty = "n")
title(list('ROC Curves For NN Model', cex=1.5))
dev.off()

#PR
nnet_perf_2 <- performance(nnet_pred, "prec","rec")
nnet_aupr <- round(performance(nnet_pred, "aucpr")@y.values[[1]],digits = 4)
nnet_aupr

png("figure/nnet.vali.pr.png",units = "in",width = 10,height = 7,res = 600)
plot(nnet_perf_2,lwd=2,col=custom_color[3],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
legend("bottomright", legend=paste("Test Dataset AUPR: ", nnet_aupr),
       col=custom_color[3], cex=2, lwd=2,bty = "n")
title(list('PR Curves For NN Model', cex=1.5))
dev.off()

#calibration
png("figure/nnet.vali.calibration.png",units = "in",width = 10,height = 7,res = 600)
nnet_calibration <- val.prob(nnet.vali.prob,vali$VTE,logistic.cal = T,
         pl=T, legendloc =c(0.55, 0.27), statloc = F)
title('NN Model Calibration Curves', col.main = '#213012')
dev.off()

#dca

data_nnet_dca_vali<-data.frame(Outcome=vali$VTE,
                                     predictors=nnet.vali.prob)

png("figure/nnet.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
nnet_dca_data <- dcurves::dca(data=data_nnet_dca_vali,Outcome~predictors,
             label = list(predictors='NN')) %>% plot(smooth=T)+
  ggplot2::labs(title = 'NN Model DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                'NN'=custom_color[3]))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
nnet_dca_data
dev.off()


#nnetmodel interpretation####
nnet_explain <- DALEX::explain(fit_bp_model,
                             data = newtrain[,-1],
                             y=newtrain$VTE,
                             label = 'NN')

#feature importance
set.seed(999)
nnet_mp <- model_parts(nnet_explain,
                     type='raw',
                     B=50,
                     N=NULL)
nnet_featurer_importance <- plot(nnet_mp)+
  facet_wrap(~label, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c('NN'='NN model')))+
  labs(subtitle = "")+
  scale_color_manual(values = custom_color[3])+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/nnet.vali.feature.importance.png",plot = nnet_featurer_importance,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#pd
nnet_pd <- model_profile(nnet_explain,
                       type = 'partial',
                       N=NULL)
nnet_pd$color <- custom_color[3]
nnet_pd_figure <- plot(nnet_pd, variables=pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = pd_title, 
       subtitle = "NN Model")+
  scale_y_continuous(limits = c(0,1))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/nnet.vali.pd_figure.png",plot = nnet_pd_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
nnet_pd_figure
# plot(nnet_pd, variables='Age', geom = "profiles") + 
#   ggtitle("Ceteris-paribus and partial-dependence profiles for age")


##group pd, cp figure
##group stage
nnet_pd_stage <- model_profile(nnet_explain, 
                             variables = pd_variables,
                             groups = pd_group, 
                             N=NULL)
nnet_pd_Stage_figure <- plot(nnet_pd_stage, variables = pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  scale_color_manual(name = "", 
                     values = DALEX::colors_discrete_drwhy(2),
                     labels = pd_legend_label)+
  labs(title = pd_title_group, subtitle = "NN Model")+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12))
ggsave("figure/nnet.vali.pd_Stage_figure.png",plot = nnet_pd_Stage_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
nnet_pd_Stage_figure

#XGBoost=========================================================================================================
# 10 CV tune hyperparameter
set.seed(999)
fit_xgb_model<-train(factor(VTE ,labels = c('no','yes'))~ .,
                     data=newtrain,
                     method='xgbTree',
                     trControl=myTrainingControl,
                     tuneLength=10,
                     metric='ROC')

xgb_cv_auc <- fit_xgb_model$results
write.xlsx(xgb_cv_auc, 'table/xgb_hyperparameter_cv_10.xlsx')

# ROC
xgb.vali.prob <- predict(fit_xgb_model ,newdata=vali,type = "prob")[,2]
xgb_pred <- prediction(xgb.vali.prob,vali$VTE) 
xgb_perf <- performance(xgb_pred, "tpr","fpr")
xgb_auc <- round(performance(xgb_pred, "auc")@y.values[[1]],digits = 4)
xgb_auc#auc

png("figure/xgb.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(xgb_perf,lwd=2,col=custom_color[4],cex.axis = 1.5, cex.lab=1.5)
abline(0,1,lty=2)
legend("bottomright", legend=paste("Test Dataset AUC: ",xgb_auc), 
       col=custom_color[4], cex=2, lwd=2,bty = "n")
title(list('ROC Curves For XGBoost Model', cex=1.5))
dev.off()

#PR
xgb_perf_2 <- performance(xgb_pred, "prec","rec")
xgb_aupr <- round(performance(xgb_pred, "aucpr")@y.values[[1]],digits = 4)
xgb_aupr

png("figure/xbg.vali.pr.png",units = "in",width = 10,height = 7,res = 600)
plot(xgb_perf_2,lwd=2,col=custom_color[4],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
legend("bottomright", legend=paste("Test Dataset AUPR: ", xgb_aupr), 
       col=custom_color[4], cex=2, lwd=2,bty = "n")
title(list('PR Curves For XGBoost Model', cex=1.5))
dev.off()

#calibration
png("figure/xgb.vali.calibration.png",units = "in",width = 10,height = 7,res = 600)
xgb_calibration<- val.prob(xgb.vali.prob, vali$VTE,logistic.cal = T,
         pl=T, legendloc =c(0.55, 0.27), statloc = F)
title('XGBoost Model Calibration Curves', col.main = '#213012')
dev.off()
#dca

data_xgb_dca_vali<-data.frame(Outcome=vali$VTE,
                                       predictors=xgb.vali.prob)
#xgboost验证集dca曲线
png("figure/xgb.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
xgb_dca_data <- dcurves::dca(data=data_xgb_dca_vali,Outcome~predictors,
             label = list(predictors='XGBoost')) %>% plot(smooth=T)+
  ggplot2::labs(title = 'XGBoost Model DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                'XGBoost'=custom_color[4]))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
xgb_dca_data
dev.off()

#xgboostmodel interpretation####
xgb_explain <- DALEX::explain(fit_xgb_model,
                               data = newtrain[,-1],
                               y=newtrain$VTE,
                               label = 'XGBoost')

#feature importance
set.seed(999)
xgb_mp <- model_parts(xgb_explain,
                       type='raw',
                       B=50,
                       N=NULL)
xgb_featurer_importance <- plot(xgb_mp)+
  facet_wrap(~label, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c('XGBoost'='XGBoost model')))+
  labs(subtitle = "")+
  scale_color_manual(values = custom_color[4])+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/xgboost.vali.feature.importance.png",plot = xgb_featurer_importance,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#pd
xgb_pd <- model_profile(xgb_explain,
                         type = 'partial',
                         N=NULL)
xgb_pd$color <- custom_color[4]
xgb_pd_figure <- plot(xgb_pd, variables=pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = pd_title, 
       subtitle = "XGBoost Model")+
  scale_y_continuous(limits = c(0,1))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/xgb.vali.pd_figure.png",plot = xgb_pd_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
xgb_pd_figure
# plot(xgb_pd, variables='Age', geom = "profiles") + 
#   ggtitle("Ceteris-paribus and partial-dependence profiles for age")


##group pd, cp figure
##group stage
xgb_pd_stage <- model_profile(xgb_explain, 
                               variables = pd_variables,
                               groups = pd_group, 
                               N=NULL)
xgb_pd_Stage_figure <- plot(xgb_pd_stage, variables=pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  scale_color_manual(name = "", 
                     values = DALEX::colors_discrete_drwhy(2),
                     labels = pd_legend_label)+
  labs(title = pd_title_group, subtitle = "XGBoost Model")+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12))
ggsave("figure/xgb.vali.pd_Stage_figure.png",plot = xgb_pd_Stage_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
xgb_pd_Stage_figure

#GBM==================================================================================================
# 10 CV tune hyperparameter
set.seed(999)
fit_gbm_model<-train(factor(VTE ,labels = c('no','yes'))~ .,
                     data=newtrain,
                     method='gbm',
                     trControl=myTrainingControl,
                     verbose=FALSE,
                     tuneLength =10,
                     metric='ROC')

gbm_cv_auc <- fit_gbm_model$results
write.xlsx(gbm_cv_auc, 'table/gbm_hyperparameter_cv_10.xlsx')

# ROC
gbm.vali.prob <- predict(fit_gbm_model ,newdata=vali,type = "prob")[,2]
gbm_pred <- prediction(gbm.vali.prob,vali$VTE) 
gbm_perf <- performance(gbm_pred, "tpr","fpr")
gbm_auc <- round(performance(gbm_pred, "auc")@y.values[[1]],digits = 4)
gbm_auc#auc

png("figure/gbm.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(gbm_perf,lwd=2,col=custom_color[5],cex.axis = 1.5, cex.lab=1.5)
abline(0,1,lty=2)
legend("bottomright", legend=paste("Test Dataset AUC: ",xgb_auc),
       col=custom_color[5], cex=2, lwd=2,bty = "n")
title(list('ROC Curves For GBM Model', cex=1.5))
dev.off()

#PR
gbm_perf_2 <- performance(gbm_pred, "prec","rec")
gbm_aupr <- round(performance(gbm_pred, "aucpr")@y.values[[1]],digits = 4)
gbm_aupr

png("figure/gbm.vali.pr.png",units = "in",width = 10,height = 7,res = 600)
plot(gbm_perf_2,lwd=2,col=custom_color[5],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
legend("bottomright", legend=paste("Test Dataset AUPR: ", gbm_aupr),
       col=custom_color[5], cex=2, lwd=2,bty = "n")
title(list('PR Curves For GBM Model', cex=1.5))
dev.off()

#calibration
png("figure/gbm.vali.calibration.png",units = "in",width = 10,height = 7,res = 600)
gbm_calibration<- val.prob(gbm.vali.prob, vali$VTE,logistic.cal = T,
         pl=T, legendloc =c(0.55, 0.27), statloc = F)
title('GBM Model Calibration Curves', col.main = '#213012')
dev.off()

#dca

data_gbm_dca_vali<-data.frame(Outcome=vali$VTE,
                                      predictors=gbm.vali.prob)

png("figure/gbm.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
gbm_dca_data <- dcurves::dca(data=data_gbm_dca_vali,Outcome~predictors,
             label = list(predictors='GBM')) %>% plot(smooth=T)+
  ggplot2::labs(title = 'GBM Model DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                'GBM'=custom_color[5]))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
gbm_dca_data
dev.off()

#gbmmodel interpretation####
gbm_explain <- DALEX::explain(fit_gbm_model,
                              data = newtrain[,-1],
                              y=newtrain$VTE,
                              label = 'GBM')

#feature importance
set.seed(999)
gbm_mp <- model_parts(gbm_explain,
                      type='raw',
                      B=50,
                      N=NULL)
gbm_featurer_importance <- plot(gbm_mp)+
  facet_wrap(~label, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c('GBM'='GBM model')))+
  labs(subtitle = "")+
  scale_color_manual(values = custom_color[5])+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/gbm.vali.feature.importance.png",plot = gbm_featurer_importance,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#pd
gbm_pd <- model_profile(gbm_explain,
                        type = 'partial',
                        N=NULL)
gbm_pd$color <- custom_color[5]
gbm_pd_figure <- plot(gbm_pd, variables=pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = pd_title, 
       subtitle = "GBM Model")+
  scale_y_continuous(limits = c(0,1))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/gbm.vali.pd_figure.png",plot = gbm_pd_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
gbm_pd_figure
# plot(gbm_pd, variables='Age', geom = "profiles") + 
#   ggtitle("Ceteris-paribus and partial-dependence profiles for age")

##group pd, cp figure
##group stage
gbm_pd_stage <- model_profile(gbm_explain, 
                              variables = pd_variables,
                              groups = pd_group, 
                              N=NULL)
gbm_pd_Stage_figure <- plot(gbm_pd_stage, variables = pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  scale_color_manual(name = "", 
                     values = DALEX::colors_discrete_drwhy(2),
                     labels = pd_legend_label)+
  labs(title = pd_title_group, subtitle = "GBM Model")+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12))
ggsave("figure/gbm.vali.pd_Stage_figure.png",plot = gbm_pd_Stage_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
gbm_pd_Stage_figure
#svm==============================================================================================
set.seed(999)
fit_svm_model <- train(factor(VTE ,labels = c('no','yes'))~ .,	
                       data=newtrain, 
                       method = 'svmRadial',	
                       trControl = myTrainingControl,
                       verbose=FALSE,
                       tuneLength =10,
                       metric='ROC')

svm_cv_auc <- fit_svm_model$results
write.xlsx(svm_cv_auc, 'table/svm_hyperparameter_cv_10.xlsx')


# ROC
svm.vali.prob <- predict(fit_svm_model ,newdata=vali,type = "prob")[,2]
svm_pred <- prediction(svm.vali.prob,vali$VTE) 
svm_perf <- performance(svm_pred, "tpr","fpr")
svm_auc <- round(performance(svm_pred, "auc")@y.values[[1]],digits = 4)
svm_auc#auc

png("figure/svm.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(svm_perf,lwd=2,col=custom_color[6],cex.axis = 1.5, cex.lab=1.5)
abline(0,1,lty=2)
legend("bottomright", legend=paste("Test Dataset AUC: ",svm_auc),
       col=custom_color[6], cex=2, lwd=2,bty = "n")
title(list('ROC Curves For SVM Model', cex=1.5))
dev.off()

#PR
svm_perf_2 <- performance(svm_pred, "prec","rec")
svm_aupr <- round(performance(svm_pred, "aucpr")@y.values[[1]],digits = 4)
svm_aupr

png("figure/svm.vali.pr.png",units = "in",width = 10,height = 7,res = 600)
plot(svm_perf_2,lwd=2,col=custom_color[6],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
legend("bottomright", legend=paste("Test Dataset AUPR: ", svm_aupr),
       col=custom_color[6], cex=2, lwd=2,bty = "n")
title(list('PR Curves For SVM Model', cex=1.5))
dev.off()

#calibration
png("figure/svm.vali.calibration.png",units = "in",width = 10,height = 7,res = 600)
svm_calibration<- val.prob(svm.vali.prob, vali$VTE,logistic.cal = T,
         pl=T, legendloc =c(0.55, 0.27), statloc = F)
title('SVM Model Calibration Curves', col.main = '#213012')
dev.off()
#dca

data_svm_dca_vali<-data.frame(Outcome=vali$VTE,
                                      predictors=svm.vali.prob)

png("figure/svm.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
svm_dca_data <- dcurves::dca(data=data_svm_dca_vali,Outcome~predictors,
             label = list(predictors='SVM')) %>% plot(smooth=T)+
  ggplot2::labs(title = 'SVM Model DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                'SVM'=custom_color[6]))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
svm_dca_data
dev.off()

#svmmodel interpretation####
svm_explain <- DALEX::explain(fit_svm_model,
                              data = newtrain[,-1],
                              y=newtrain$VTE,
                              label = 'SVM')

#feature importance
set.seed(999)
SVM_mp <- model_parts(svm_explain,
                      type='raw',
                      B=50,
                      N=NULL)
SVM_featurer_importance <- plot(SVM_mp)+
  facet_wrap(~label, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c('SVM'='SVM model')))+
  labs(subtitle = "")+
  scale_color_manual(values = custom_color[6])+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/svm.vali.feature.importance.png",plot = SVM_featurer_importance,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#pd
svm_pd <- model_profile(svm_explain,
                        type = 'partial',
                        N=NULL)
svm_pd$color <- custom_color[6]
svm_pd_figure <- plot(svm_pd, variables=pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = pd_title,
       subtitle = "SVM Model")+
  scale_y_continuous(limits = c(0,1))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/svm.vali.pd_figure.png",plot = svm_pd_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
svm_pd_figure
# plot(svm_pd, variables='Age', geom = "profiles") + 
#   ggtitle("Ceteris-paribus and partial-dependence profiles for age")


##group pd, cp figure
##group stage
svm_pd_stage <- model_profile(svm_explain, 
                              variables = pd_variables,
                              groups = pd_group, 
                              N=NULL)
svm_pd_Stage_figure <- plot(svm_pd_stage, variables = pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  scale_color_manual(name = "", 
                     values = DALEX::colors_discrete_drwhy(2),
                     labels = pd_legend_label)+
  labs(title = pd_title_group, subtitle = "SVM Model")+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12))
ggsave("figure/svm.vali.pd_Stage_figure.png",plot = svm_pd_Stage_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
svm_pd_Stage_figure

#adaboost==============================================================================================
# devtools::install_github("souravc83/fastAdaboost")
set.seed(999)
fit_ada_model <- train(factor(VTE ,labels = c('no','yes'))~ .,	
                       data=newtrain, 
                       method = 'adaboost',	
                       trControl = myTrainingControl,
                       verbose=FALSE,
                       tuneLength =10,
                       metric='ROC')

ada_cv_auc <- fit_ada_model$results
write.xlsx(ada_cv_auc, 'table/ada_hyperparameter_cv_10.xlsx')

# ROC
ada.vali.prob <- predict(fit_ada_model ,newdata=vali,type = "prob")[,2]
ada_pred <- prediction(ada.vali.prob,vali$VTE) 
ada_perf <- performance(ada_pred, "tpr","fpr")
ada_auc <- round(performance(ada_pred, "auc")@y.values[[1]],digits = 4)
ada_auc#auc

png("figure/adaboost.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(ada_perf,lwd=2,col=custom_color[7],cex.axis = 1.5, cex.lab=1.5)
abline(0,1,lty=2)
legend("bottomright", legend=paste("Test Dataset AUC: ",ada_auc), 
       col=custom_color[7], cex=2, lwd=2,bty = "n")
title(list('ROC Curves For Adaboost Model', cex=1.5))
dev.off()

#PR
ada_perf_2 <- performance(ada_pred, "prec","rec")
ada_aupr <- round(performance(ada_pred, "aucpr")@y.values[[1]],digits = 4)
ada_aupr

png("figure/adaboost.vali.pr.png",units = "in",width = 10,height = 7,res = 600)
plot(ada_perf_2,lwd=2,col=custom_color[7],ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.5)
legend("bottomright", legend=paste("Test Dataset AUPR: ", ada_aupr),
       col=custom_color[7], cex=2, lwd=2,bty = "n")
title(list('PR Curves For Adaboost Model', cex=1.5))
dev.off()

#calibration
png("figure/adaboost.vali.calibration.png",units = "in",width = 10,height = 7,res = 600)
ada_calibration <- val.prob(ada.vali.prob, vali$VTE,logistic.cal = T,
         pl=T, legendloc =c(0.55, 0.27), statloc = F)
title('Adaboost Model Calibration Curves', col.main = '#213012')
dev.off()
ada_calibration
#dca

data_ada_dca_vali<-data.frame(Outcome=vali$VTE,
                                      predictors=ada.vali.prob)

png("figure/adaboost.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
ada_dca_data <- dcurves::dca(data=data_ada_dca_vali,Outcome~predictors,
             label = list(predictors='Adaboost')) %>% plot(smooth=T)+
  ggplot2::labs(title = 'Adaboost Model DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                'Adaboost'=custom_color[7]))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
ada_dca_data
dev.off()

#adaboostmodel interpretation####
ada_explain <- DALEX::explain(fit_ada_model,
                              data = newtrain[,-1],
                              y=newtrain$VTE,
                              label = 'Adaboost')

#feature importance
set.seed(999)
ada_mp <- model_parts(ada_explain,
                      type='raw',
                      B=50,
                      N=NULL)
ada_featurer_importance <- plot(ada_mp)+
  facet_wrap(~label, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c('Adaboost'='Adaboost model')))+
  labs(subtitle = "")+
  scale_color_manual(values = custom_color[7])+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/ada.vali.feature.importance.png",plot = ada_featurer_importance,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#pd
ada_pd <- model_profile(ada_explain,
                        type = 'partial',
                        N=NULL)
ada_pd$color <- custom_color[7]
ada_pd_figure <- plot(ada_pd, variables=pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = pd_title,
       subtitle = "Adaboost Model")+
  scale_y_continuous(limits = c(0,1))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/ada.vali.pd_figure.png",plot = ada_pd_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
ada_pd_figure
# plot(ada_pd, variables='Age', geom = "profiles") + 
#   ggtitle("Ceteris-paribus and partial-dependence profiles for age")


##group pd, cp figure
##group stage
ada_pd_stage <- model_profile(ada_explain, 
                              variables = pd_variables,
                              groups = pd_group, 
                              N=NULL)
ada_pd_Stage_figure <- plot(ada_pd_stage, variables = pd_variables)+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  scale_color_manual(name = "", 
                     values = DALEX::colors_discrete_drwhy(2),
                     labels = pd_legend_label)+
  labs(title = pd_title_group, subtitle = "Adaboost Model")+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 12))
ggsave("figure/ada.vali.pd_Stage_figure.png",plot = ada_pd_Stage_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
ada_pd_Stage_figure
# plot(ada_pd_stage, log_pd_stage, variables=pd_variables)

###summary########
test_collect <- function(model, auc, aupr, calibration, digit=4){
  metric <- t(c(model, round(c(auc, aupr, calibration[c(11:13)]), digit)))
  dimnames(metric)[[2]][1:3] <- c('Model', 'ROC', 'AUPR')
  metric <- as.data.frame(metric)
  return(metric)
}
test_performance <- rbind(
  test_collect('LR', log_auc, log_aupr, log_calibration),
  test_collect('RF', rf_auc, rf_aupr, rf_calibration),
  test_collect('NNET', nnet_auc, nnet_aupr, nnet_calibration),
  test_collect('XGBoost', xgb_auc, xgb_aupr, xgb_calibration),
  test_collect('GBM', gbm_auc, gbm_aupr, gbm_calibration),
  test_collect('SVM', svm_auc, svm_aupr, svm_calibration),
  test_collect('Adaboost', ada_auc, ada_aupr, ada_calibration)
)
write.xlsx(test_performance, 'table/test dataset.xlsx')

#roc set
png("figure/all.vali.roc.png",units = "in",width = 10,height = 7,res = 600)
plot(log_perf,lwd=2,col="#79BDA4",ylim=c(0, 1),cex.axis = 1.5, cex.lab=1.4)
plot(rf_perf,lwd=2,col="#F0945C",ylim=c(0, 1), add=T)
plot(nnet_perf,lwd=2,col="#F5EF80",ylim=c(0, 1), add=T)
plot(xgb_perf,lwd=2,col="#E34041",ylim=c(0, 1), add=T)
plot(gbm_perf,lwd=2,col="#C8C2E0",ylim=c(0, 1), add=T)
plot(svm_perf,lwd=2,col="#9FD5C9",ylim=c(0, 1), add=T)
plot(ada_perf,lwd=2,col="#C286BB",ylim=c(0, 1), add=T)
abline(0,1,lty=2)
legend("bottomright",
       
       legend = c("    LR       (AUC: 0.7617)", 
                  "    RF       (AUC: 0.7922)", 
                  "    NN       (AUC: 0.7672)",
                  "XGBoost  (AUC: 0.7657)",
                  "    GBM    (AUC: 0.7853)",
                  "    SVM    (AUC: 0.6559)",
                  "AdaBoost (AUC: 0.7191)"
                  ) ,
       col = c("#79BDA4","#F0945C","#F5EF80",
               "#E34041","#C8C2E0","#9FD5C9","#C286BB"), 
       lty = 1, lwd = 2, bty = 'n',
       cex =1.5)
title(list('ROC Curves For Test Dataset', cex=1.5))
dev.off()

#delong's test
roc_log <- roc(vali$VTE, log.vali.prob)
roc_rf <- roc(vali$VTE, rf.vali.prob)
roc_nnet <- roc(vali$VTE, nnet.vali.prob)
roc_xgb <- roc(vali$VTE, xgb.vali.prob)
roc_gbm <- roc(vali$VTE, gbm.vali.prob)
roc_svm <- roc(vali$VTE, svm.vali.prob)
roc_ada <- roc(vali$VTE, ada.vali.prob)

#pairwise comparison
p_values <- c(
  roc.test(roc_log, roc_rf)$p.value,
  roc.test(roc_log, roc_nnet)$p.value,
  roc.test(roc_log, roc_xgb)$p.value,
  roc.test(roc_log, roc_gbm)$p.value,
  roc.test(roc_log, roc_svm)$p.value,
  roc.test(roc_log, roc_ada)$p.value,
  roc.test(roc_rf, roc_nnet)$p.value,
  roc.test(roc_rf, roc_xgb)$p.value,
  roc.test(roc_rf, roc_gbm)$p.value,
  roc.test(roc_rf, roc_svm)$p.value,
  roc.test(roc_rf, roc_ada)$p.value,
  roc.test(roc_nnet, roc_xgb)$p.value,
  roc.test(roc_nnet, roc_gbm)$p.value,
  roc.test(roc_nnet, roc_svm)$p.value,
  roc.test(roc_nnet, roc_ada)$p.value,
  roc.test(roc_xgb, roc_gbm)$p.value,
  roc.test(roc_xgb, roc_svm)$p.value,
  roc.test(roc_xgb, roc_ada)$p.value,
  roc.test(roc_gbm, roc_svm)$p.value,
  roc.test(roc_gbm, roc_ada)$p.value,
  roc.test(roc_svm, roc_ada)$p.value
)

# Bonferroni adjust
p.adjust(p_values, method = "bonferroni")
# Benjamini-Hochberg adjust
p.adjust(p_values, method = "BH")
# Holm adjust
p.adjust(p_values, method = "holm")
p.adjust(p_values, method = "hochberg")

p_data <- as.data.frame(t(p_values))
names(p_data) <- apply(combn(custom_models_name, 2), 2, 
                       function(x) paste(x, collapse = "——"))
p_data <- pivot_longer(p_data,
                       cols = 1:21,
                       names_to = 'Model',
                       values_to = 'P'
                       )
p_data$P.bonferroni <- p.adjust(p_values, method = "bonferroni")
p_data$BH <- p.adjust(p_values, method = "BH")
p_data$holm <- p.adjust(p_values, method = "holm")
p_data$hochberg <- p.adjust(p_values, method = "hochberg")
write.xlsx(p_data, 'table/delong_test.xlsx')

# PR set
png("figure/all.vali.pr.png",units = "in",width = 10,height = 7,res = 600)
plot(log_perf_2,lwd=2,col="#79BDA4",ylim=c(0, 1))
plot(rf_perf_2,lwd=2,col="#F0945C",ylim=c(0, 1), add=T)
plot(nnet_perf_2,lwd=2,col="#F5EF80",ylim=c(0, 1), add=T)
plot(xgb_perf_2,lwd=2,col="#E34041",ylim=c(0, 1), add=T)
plot(gbm_perf_2,lwd=2,col="#C8C2E0",ylim=c(0, 1), add=T)
plot(svm_perf_2,lwd=2,col="#9FD5C9",ylim=c(0, 1), add=T)
plot(ada_perf_2,lwd=2,col="#C286BB",ylim=c(0, 1), add=T)
legend("topright",
       
       legend = c("    LR       (AUC: 0.2382)", 
                  "    RF       (AUC: 0.4084)", 
                  "    NN       (AUC: 0.2487)",
                  "XGBoost  (AUC: 0.4066)",
                  "    GBM    (AUC: 0.3614)",
                  "    SVM    (AUC: 0.1971)",
                  "AdaBoost (AUC: 0.3528)"
       ) ,
       col = c("#79BDA4","#F0945C","#F5EF80",
               "#E34041","#C8C2E0","#9FD5C9","#C286BB"), 
       lty = 1, lwd = 2, bty = 'n',
       cex =1.1)
title('PR Curves For Test Dataset')
dev.off()

#DCA
all_dca <- cbind(data_lr_dca_vali[,c(1,2)], data_rf_dca_vali[,2],
                 data_nnet_dca_vali[,2],data_xgb_dca_vali[,2],
                 data_gbm_dca_vali[,2],data_svm_dca_vali[,2],
                 data_ada_dca_vali[,2])
names(all_dca) <- c('Outcome',custom_models_name)
png("figure/all.vali.dca.png",units = "in",width = 10,height = 7,res = 600)
all_dca_data <- dca(data=all_dca, Outcome~.) %>% plot(smooth=T)+
  ggplot2::labs(title = 'DCA Curves')+
  scale_color_manual(values = c('Treat All'= rgb(068,004,090, maxColorValue = 255),
                                'Treat None'=rgb(145,213,066, maxColorValue = 255),
                                 'LR'=custom_color[1],
                                 'RF'=custom_color[2],
                                 'NN'=custom_color[3],
                                 'XGBoost'=custom_color[4],
                                 'GBM'=custom_color[5],
                                 'SVM'=custom_color[6],
                                 'Adaboost'=custom_color[7]
                                ))+
  theme(legend.position = c(0.9, 0.5),
        legend.background = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
all_dca_data
dev.off()
head(all_dca_data$data)


# png("figure/all_feature_importance.png",units = "in",width = 10,height = 7,res = 600)
# plot(log_mp, rf_mp, nnet_mp,
#      xgb_mp, gbm_mp, SVM_mp,
#      ada_mp)
# dev.off()

all_pd_figure_age <- plot(log_pd, RF_pd, nnet_pd, xgb_pd,
     gbm_pd, svm_pd, ada_pd, variables=pd_variables)+
  scale_y_continuous(limits = c(0,1))+
  scale_color_manual(name="", values =  custom_color[order(custom_models_name)])+
  scale_x_continuous(breaks = pd_breaks, 
                     labels = pd_variable_label)+
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = pd_title,
       subtitle = "")+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("figure/all_pd_figure_age.png",plot = all_pd_figure_age,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
all_pd_figure_age
###appropriate model：RF#####
#instanve interpretation###############
#bd figure, difference order variable
ins_id <- 1
rf_ins_bd <- predict_parts(rf_explain,
                            new_observation = newtrain[ins_id,-1],
                            type = 'break_down',
                            order=c('Age','Stage','Pathologictype',
                                    'D.dimer','Chemotherapy','NLR'))
plot(rf_ins_bd)
rf_ins_bd <- predict_parts(rf_explain,
                            new_observation = newtrain[ins_id,-1],
                            type = 'break_down',
                            order=c('D.dimer','NLR','Age',
                                    'Chemotherapy','Pathologictype','Stage'))
rf_ins_bp_figure <- plot(rf_ins_bd, 
                         vnames=c('intercept','D.dimer: > 0.5','NLR: 2.10',
                                  'Age: 72','Chemotherapy: NO', 'Pathologictype: SCC',
                                  'Stage: III-IV', 'prediction'))+
  facet_wrap(~label, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('RF'='')))+
  labs(title = paste0('ID:', ins_id), subtitle = paste0('Break-down plot for ID', ins_id))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, colour = 'tomato'))
rf_ins_bp_figure
ggsave("figure/rf_ins_bp_figure.png",plot = rf_ins_bp_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
#interaction bd
ins_id <- 1
rf_ins_inbd <- DALEX::predict_parts(rf_explain,
                                     new_observation = newtrain[ins_id,-1],
                                     type  = "break_down_interactions")
rf_ins_ibp_figure <- plot(rf_ins_inbd)
rf_ins_ibp_figure
#shap bd figure
set.seed(999)
rf_ins_shap <- predict_parts(rf_explain,
                              new_observation = newtrain[ins_id,-1],
                              type = 'shap',
                              B=25)
rf_ins_shap <- rf_ins_shap %>%
  mutate(variable = case_when(
    variable == "Age = 0.7536" ~ 'Age: 72',
    variable == "Chemotherapy = 0" ~ 'Chemotherapy: NO',
    variable == "D.dimer = 1" ~ 'D.dimer: > 0.5',
    variable == "NLR = 0.06005" ~ 'NLR: 2.10',
    variable == "Pathologictype = 0" ~ 'Pathologictype: SCC',
    variable == "Stage = 1" ~ 'Stage: III-IV',
    TRUE ~ 'NA_real_' 
  ))

rf_ins_shap_figure <- plot(rf_ins_shap)+
  facet_wrap(~label, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('RF'='')))+
  labs(title = paste0('ID:', ins_id), subtitle = paste0('Average attributions for ID', ins_id, '-Based Shapley Values'))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15, colour = 'tomato'))
rf_ins_shap_figure
ggsave("figure/rf_ins_shapbp_figure.png",plot = rf_ins_shap_figure,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#cp
{
  categories <- 'D.dimer'
  cp_variables <- 'D.dimer'
  cp_breaks <- ifelse(cp_variables %in% categories, 
                      list(c(0, 1)), 
                      list(c(0.00, 0.25, 0.50, 0.75, 1.00)))[[1]]
  # Age NLR
  cp_trans_id <- match(cp_variables, names(train_max))
  cp_variable_label <- round(c(0.00, 0.25, 0.50, 0.75, 1.00)*(train_max- train_min)[[cp_trans_id]] +train_min[[cp_trans_id]],
                             2)
  # D.dimer
  # cp_variable_label <- c('≤0.5','>0.5')
  cp_variable_label
  cp_title <- paste0('Ceteris Paribus profile for ', cp_variables)
}
rf_ins_cp <- predict_profile(rf_explain,
                              new_observation = newtrain[ins_id,-1],
                              variables = cp_variables)

rf_ins_cp_dimer<- plot(rf_ins_cp, variables = cp_variables)+
  scale_x_continuous(breaks = cp_breaks, 
                     labels = cp_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = paste0('ID:', ins_id), subtitle = cp_title)+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 15, colour = 'tomato'))
rf_ins_cp_dimer
ggsave(paste0("figure/rf_ins_cp",cp_variables ,"_figure.png"),
       plot = rf_ins_cp_dimer,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)
# #cp shock
# rf_ins_shockcp <- predict_parts(explainer = rf_explain,
#                                 new_observation = newtrain[ins_id,-1],
#                                 type = "oscillations_uni")
# plot(rf_ins_shockcp)

#local stability
rf_explain_part <- DALEX::explain(fit_RF,
                             data = newtrain[,-1],
                             y=newtrain$VTE,
                             # residual_function = 
                               label = 'RF')

rf_ins_part_dimer <- predict_diagnostics(rf_explain_part,
                                   new_observation = newtrain[ins_id,-1],
                                   neighbors = 15,
                                   variables = cp_variables)

rf_ins_cppart_dimer<- plot(rf_ins_part_dimer)+
  scale_x_continuous(breaks = cp_breaks, 
                     labels = cp_variable_label)+ 
  facet_wrap(~`_vname_`, ncol = 1, scales = "free_y",
             labeller = as_labeller(c('Age'='','Chemotherapy'='',
                                      'D.dimer'='','NLR'='',
                                      'Pathologictype'='','Stage'='')))+
  labs(title = paste0('ID:', ins_id), 
       subtitle = 'Local stability plot created for the RF model')+
  xlab(cp_variables)+
  scale_y_continuous(limits = c(0,1))+
  guides(color= guide_legend(override.aes = list(size = 15)))+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 15, colour = 'tomato'))
rf_ins_cppart_dimer
ggsave(paste0("figure/rf_ins_localstability",cp_variables ,"_figure.png"),
       plot = rf_ins_cppart_dimer,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)

#fastshap
pfun <- function(object, newdata) {  # prediction wrapper
  unname(predict(object ,newdata=newdata,type = "prob")[,2])
}
ex.total<- fastshap::explain(fit_RF, X = newtrain[,-1], 
                   pred_wrapper = pfun, 
                   nsim = 50, 
                   adjust = F,
                   shap_only = FALSE)
tibble::as_tibble(ex.total$shapley_values)
shapv.global <- shapviz(ex.total)
sv_importance(shapv.global) 
undebug(sv_importance)
viridis_args =list(begin=0.25,end=0.85,option="inferno")
shap_beeswarm <- sv_importance(shapv.global, kind = "beeswarm")+
  scale_color_gradientn(colors = c("#4378bf", "#8bdcbe"))+
  theme_bw()+
  labs(title = 'SHAP Values for Random Forest Model')+
  theme(legend.position = c(0.85, 0.25),
        legend.background = element_blank())
  # ggplot2::theme(legend.box.spacing = grid::unit(0, "pt"))+
  # shapviz:::.get_color_scale(viridis_args = viridis_args, 
  #                  bar = T, ncol = 493)
ggsave("figure/rf_shap_beeswarm.png",plot = shap_beeswarm,
       width = 5.5,height = 4,units = "in",dpi = 600,device = png)



#cutoff values decision####
#Youden index
rf.train.prob <- predict(fit_RF ,newdata=newtrain,type = "prob")[,2]
rf_pred_train <- prediction(rf.train.prob,newtrain$VTE) 
rf_perf_train <- performance(rf_pred_train, "tpr","fpr")
plot(rf_perf_train)

rf_perf_train@y.values[[1]][1:4]
rf_perf_train@alpha.values[[1]][1:4]
rf_perf_train@y.name
rf_perf_train@alpha.name
rf_perf_train@x.name
#youden index
#cutoff value by train dataset
cutoff = rf_perf_train@alpha.values[[1]][which.max(rf_perf_train@y.values[[1]] -
                                                    rf_perf_train@x.values[[1]])]
tf_train_lable_pre <- ifelse(rf.train.prob>= cutoff,1,0)
cutoff 
table(tf_train_lable_pre, newtrain$VTE)


#cutoff for vali dataset
rf_pre_label <- ifelse(rf.vali.prob>= cutoff, 1,0)

rf_data_pred_label <- data.frame(p = factor(rf_pre_label,levels = c('0','1')),
                                 l = as.factor(vali$VTE))
cmrf <- conf_mat(rf_data_pred_label,
                 l, p)
png("figure/rf.confusionmatrix.yueden.png",units = "in",width = 10,height = 7,res = 600)
autoplot(cmrf, type = "heatmap") +
  scale_fill_gradient(high = "#8ccbea", low = "#D6EAF8")+
  scale_x_discrete(position = "top", breaks=c(0,1),
                   label=c('Non-VTE','VTE')) +
  scale_y_discrete(breaks=c(0,1), label=c('Non-VTE','VTE'))+
  labs(title = 'The ConfusionMatrix of RF Model',
       subtitle = paste0('cutoff value: ', round(cutoff, 4)))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(face='bold'))
dev.off()

cmrf_vali <- confusionMatrix(as.factor(rf_pre_label), as.factor(vali$VTE),
                             positive = '1')
cmrf_vali$byClass
tp=18
fp=56
fn=11
tn=267
dim(vali)
#nb
(tp-(fp*cutoff/(1-cutoff)))/352
0.62068966+0.82662539-1



#maximize F1 in the train dataset
rf_f <- performance(rf_pred_train, 'f', α=0.5)
rf_f@y.name
rf_f@x.name
which.max(rf_f@y.values[[1]])
rf_f@y.values[[1]][45]
max(rf_f@y.values[[1]], na.rm = T)
cutoff = rf_f@x.values[[1]][45]
cutoff

rf_pre_label <- ifelse(rf.vali.prob>= cutoff, 1,0)

rf_data_pred_label <- data.frame(p = as.factor(rf_pre_label),
                                 l = as.factor(vali$VTE))
cmrf <- conf_mat(rf_data_pred_label,
                 l, p)
png("figure/rf.confusionmatrix.F1.png",units = "in",width = 10,height = 7,res = 600)
autoplot(cmrf, type = "heatmap") +
  scale_fill_gradient(high = "#f3a17c", low = "#f4f1ea")+
  scale_x_discrete(position = "top", breaks=c(0,1),
                   label=c('Non-VTE','VTE')) +
  scale_y_discrete(breaks=c(0,1), label=c('Non-VTE','VTE'))+
  labs(title = 'The ConfusionMatrix of RF Model',
       subtitle = paste0('cutoff value: ', round(cutoff, 4)))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(face='bold'))
dev.off()

cmrf_vali <- confusionMatrix(as.factor(rf_pre_label), as.factor(vali$VTE),
                             positive = '1')
cmrf_vali$byClass
tp=14
fp=23
fn=15
tn=300
dim(vali)
(tp-(fp*cutoff/(1-cutoff)))/352
0.48275862+0.92879257-1
#distribution
hist(rf.vali.prob[data_rf_dca_vali$Outcome==0])
hist(rf.vali.prob[data_rf_dca_vali$Outcome==1])

ggplot(data_rf_dca_vali, aes(x=rf.vali.prob, color=as.factor(Outcome)))+
  geom_histogram()

#priori information: prevalence
prop.table(table(newtrain$VTE))
0.0754
#cutoff=0.07
#RF model
cutoff =  0.0754 
cutoff
rf_pre_label <- ifelse(rf.vali.prob>= cutoff, 1,0)

rf_data_pred_label <- data.frame(p = as.factor(rf_pre_label),
                                 l = as.factor(vali$VTE))
cmrf <- conf_mat(rf_data_pred_label,
                 l, p)
png("figure/rf.confusionmatrix.prior.png",units = "in",width = 10,height = 7,res = 600)
autoplot(cmrf, type = "heatmap") +
  scale_fill_gradient(high = "#78a040", low = "#c1e0da")+
  scale_x_discrete(position = "top", breaks=c(0,1),
                   label=c('Non-VTE','VTE')) +
  scale_y_discrete(breaks=c(0,1), label=c('Non-VTE','VTE'))+
  labs(title = 'The ConfusionMatrix of RF Model',
       subtitle = paste0('cutoff value: ', round(cutoff, 4)))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(face='bold'))
dev.off()

cmrf_vali <- confusionMatrix(as.factor(rf_pre_label), as.factor(vali$VTE),
                             positive = '1')
cmrf_vali$byClass

tp=19
fp=83
fn=10
tn=240
(tp-fp*(cutoff/(1-cutoff)))/352
0.65517241+0.74303406-1
# (tp*0.08239-fp*(cutoff/(1-cutoff))*(1-0.08239))
# levels(pre_lable)


save(list = c('fit_RF', 'rf_explain','cutoff', 'train_max', 'train_min'), file = 'rf.RData')
