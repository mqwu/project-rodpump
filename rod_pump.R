
#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:/project/RodPump")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)
library(randomForest)
library(party)
library(rpart)
library(survival)


source("tools.R")


#--------------------------------------------------------
# Paths
#-------------------------------------------------------- 
d_path = "./data/RodPumpData.csv"


#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
d <- read.csv(d_path, header=TRUE, na.strings="N/A")

# correct names
colnames(d) <- gsub("\\.+", ".", colnames(d)) # replace multiple . with one .
colnames(d) <- gsub("\\.$", "", colnames(d))  # rm last .

# select vars
d <- d %>% select(Well.Name:Pump.Set.Depth.ft.TVD)  # 166 x 21

# correct var type
d$Well.Name <- as.character(d$Well.Name)
d$Failure.Date <- as.Date(d$Failure.Date)
d$Well.BOL <- as.Date(d$Well.BOL)
d$Pump.Set.Depth.ft.TVD <- as.numeric(d$Pump.Set.Depth.ft.TVD)


## regroups 
# run vs no run
d.run.n <- d %>% filter(Currently.Running=="N")  # 153 x 21
d.run.y <- d %>% filter(Currently.Running=="Y")  # 13 x 21

# Bad actor 2 groups (combine N and Alumi) vs Y
d.bad2 <- cbind(d, Bad=ifelse(d$Bad.Actor=="Y", "Y", "N"))  # (combine N and Alumi)  vs Y
d.bad2.complete <- d.bad2 %>% select(Bad, Target.Sand, GLR, Max.DLS) %>% filter(complete.cases(.))  # 102

# no run
# complete case vs. non complete
d.bad <- d.run.n %>% select(Bad.Actor, Max.DLS, Target.Sand, GLR)  # 153 cases
d.bad.complete <- d.bad %>% filter(complete.cases(.))  # 93 complete cases

# d.grp0 <- d.bad.actor.complete  # three classes
# d.grp1 <- cbind(d.bad.actor.complete, Bad=ifelse(d.bad.actor.complete$Bad.Actor=="N", "N", "Y"))  # (combine Y and Alummi) vs N
# d.grp2 <- cbind(d.bad.actor.complete, Bad=ifelse(d.bad.actor.complete$Bad.Actor=="Y", "Y", "N"))  # (combine N and Alumi)  vs Y
# d.grp2.all <- cbind(d.bad.actor, Bad=ifelse(d.bad.actor$Bad.Actor=="Y", "Y", "N"))  # (combine N and Alumi)  vs Y

#### uniqure wells (some wells have multiple runs)
uniq.well <- d %>% distinct(Well.Name)  #78 wells
# Bad actor 2 groups (combine N and Alumi) vs Y
uniq.well.bad2 <- cbind(uniq.well, Bad=ifelse(uniq.well$Bad.Actor=="Y", "Y", "N"))  # (combine N and Alumi)  vs Y
uniq.well.bad2 <- uniq.well.bad2 %>% select(Bad, Target.Sand, GLR, Max.DLS)
uniq.well.bad2.imputated <- rfImpute(Bad ~ ., uniq.well.bad2)

uniq.well.bad2.complete <- uniq.well.bad2 %>% select(Bad, Target.Sand, GLR, Max.DLS) %>% filter(complete.cases(.))  # 47



#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 
# summary stats
summary(d)
summary(d.run.n)
summary(d.run.y)

# missing values
sapply(d, function(x) sum(is.na(x)))  # count
sapply(d, function(x) sum(is.na(x))/nrow(d))  # proportion

## numerical vars
# hist
plot_HistDensity(dat$Max.DLS, "Max DLS")
plot_HistDensity(dat$GLR, "GLR")
plot_HistDensity(dat$Failure.Runtime, "Failure Runtime")
plot_HistDensity(dat$Feet.of.Sinker.Bars, "Feet of Sinker Bars")
plot_HistDensity(dat$Pump.Set.Depth.ft.TVD, "Pump Set Depth")


# boxplot
plot_Box(dat, y="Max.DLS", title="Max DLS")
plot_Box(dat, y="GLR", title="GLR")
plot_Box(dat, y="Failure.Runtime", title="Failure Runtime")
plot_Box(dat, y="Feet.of.Sinker.Bars", title="Feet of Sinker Bars")
plot_Box(dat, y="Pump.Set.Depth.ft.TVD", title="Pump Set Depth")



## bad actor EDA
# boxplot of Max.DLS and GLR group by bad.actor
plot_Box(d, x="Bad.Actor", y="Max.DLS", title="Max DLS vs. Bad Actor")
plot_Box(d, x="Bad.Actor", y="GLR", title="GLR vs. Bad Actor")

# stacking plot group by Target.Sand
plot_StackingProp (d, x="Target.Sand", y="Bad.Actor", title="Bad Actor")
# bar count plot group by Target.Sand
plot_BarCount(d, x="Target.Sand", y="Bad.Actor", title="Bad Actor")


## bad actor EDA (unique wells)
plot_Box(uniq.well.bad , x="Bad.Actor", y="Max.DLS", title="Max DLS vs. Bad Actor")
plot_Box(uniq.well, x="Bad.Actor", y="GLR", title="GLR vs. Bad Actor")
# stacking plot group by Target.Sand
plot_StackingProp (uniq.well, x="Target.Sand", y="Bad.Actor", title="Bad Actor")
# bar count plot group by Target.Sand
plot_BarCount(uniq.well, x="Target.Sand", y="Bad.Actor", title="Bad Actor")



##########################
## bad actor 2 groups EDA
##########################
# boxplot of Max.DLS and GLR group by bad.actor
plot_Box(d.bad2, x="Bad", y="Max.DLS", title="Max DLS vs. Bad Actor (combine N and Alumi)")
plot_Box(d.bad2, x="Bad", y="GLR", title="GLR vs. Bad Actor (combine N and Alumi)")

# stacking plot group by Target.Sand
plot_StackingProp (d.bad2, x="Target.Sand", y="Bad", title="Bad Actor (combine N and Alumi)")
# bar count plot group by Target.Sand
plot_BarCount(d.bad2, x="Target.Sand", y="Bad", title="Bad Actor (combine N and Alumi)")


## unique wells
# boxplot of Max.DLS and GLR group by bad.actor
plot_Box(uniq.well.bad2, x="Bad", y="Max.DLS", title="Max DLS vs. Bad Actor (combine N and Alumi)")
plot_Box(uniq.well.bad2, x="Bad", y="GLR", title="GLR vs. Bad Actor (combine N and Alumi)")

# stacking plot group by Target.Sand
plot_StackingProp (uniq.well.bad2, x="Target.Sand", y="Bad", title="Bad Actor (combine N and Alumi)")
# bar count plot group by Target.Sand
plot_BarCount(uniq.well.bad2, x="Target.Sand", y="Bad", title="Bad Actor (combine N and Alumi)")




#############################
## Runtime EDA (Not running)
#############################
# boxplot of runtime group by bad.actor
plot_Box(d.run.n, x="Bad.Actor", y="Failure.Runtime", title="Failure Runtime vs. Bad Actor")
plot_Box(d.run.n, x="Cap.String", y="Failure.Runtime", title="Failure Runtime vs. Cap String")
plot_Box(d.run.n, x="Coated.Tubing", y="Failure.Runtime", title="Failure Runtime vs. Coated Tubing")
plot_Box(d.run.n, x="Taper.Design", y="Failure.Runtime", title="Failure Runtime vs. Taper Design")
plot_Box(d.run.n, x="Rod.Type", y="Failure.Runtime", title="Failure Runtime vs. Rod Type")
plot_Box(d.run.n, x="Guide.Design", y="Failure.Runtime", title="Failure Runtime vs. Guide Design")
plot_Box(d.run.n, x="Tubing.Pump", y="Failure.Runtime", title="Failure Runtime vs. Tubing Pump")
plot_Box(d.run.n, x="Desander", y="Failure.Runtime", title="Failure Runtime vs. Desander")
plot_Box(d.run.n, x="Sinker.Bars", y="Failure.Runtime", title="Failure Runtime vs. Sinker Bars")
plot_Box(d.run.n, x="Stabilizer.Location", y="Failure.Runtime", title="Failure Runtime vs. Stabilizer Location")

# scatter plots
plot_Scatter(d.run.n, x="Feet.of.Sinker.Bars", y="Failure.Runtime", title="Failure Runtime vs. Feet of sinker bars")
plot_Scatter(d.run.n, x="Pump.Set.Depth.ft.TVD", y="Failure.Runtime", title="Failure Runtime vs. Pump Set Depth")



# plot_Box(d.grp2.all, x="Bad", y="Max.DLS", title="Max DLS vs. Bad Actor (combine N and Alumi)")
# plot_Box(d.grp2.all, x="Bad", y="GLR", title="GLR vs. Bad Actor (combine N and Alumi)")


#--------------------------------------------------------
# Modelling
#-------------------------------------------------------- 
##################################################
# random forest model for bad.actor prediction
##################################################

# Bad Actor modelling
set.seed(777)
id <- sample(nrow(d.bad2.complete), nrow(d.bad2.complete)*0.667)  # split training and test 2/3 vs 1/3
train <- d.bad2.complete[id,]
test <- d.bad2.complete[-id,]

bad.mod1.rf <- randomForest(Bad ~ Target.Sand + Max.DLS + GLR, data = train, importance = TRUE)
bad.mod2.rf <- randomForest(Bad ~ Max.DLS + GLR, data = train, importance = TRUE)
varImpPlot(bad.mod2.rf, sort=TRUE)

test.pred <- predict(bad.mod1.rf, test)
table(obs=test$Bad, pred=test.pred)

test.pred2 <- predict(bad.mod2.rf, test)
table(obs=test$Bad, pred=test.pred2)

set.seed(777)
bad.mod.rf <- randomForest(Bad ~ Max.DLS + GLR, data = d.bad2.complete, importance = TRUE)
partialPlot(bad.mod.rf, d.bad2.complete, Max.DLS, "Y")
partialPlot(bad.mod.rf, d.bad2.complete, GLR, "Y")
hist(d.bad2.complete$GLR, nclass=40)
hist(d.bad2.complete$Max.DLS, nclass=40)
#partialPlot(bad.mod2.rf, train, Max.DLS, "Y")
#partialPlot(bad.mod2.rf, train, GLR, "Y")

#------------------------------------------------------
# Bad Actor modelling for unique wells
bad1 <- uniq.well.bad2.imputated %>% filter(Bad=="Y")
bad0 <- uniq.well.bad2.imputated %>% filter(Bad=="N")
set.seed(777)
bad1.train <- bad1 %>% sample_n(12, replace = F)
bad0.train <- bad0 %>% sample_frac(0.8, replace = F)

train <- union(bad1.train,bad0.train)  # 37
test  <- setdiff(uniq.well.bad2.imputated, train)  # 10


set.seed(777)
id <- sample(nrow(uniq.well.bad2.complete), nrow(uniq.well.bad2.complete)*0.667)  # split training and test 2/3 vs 1/3
train <- uniq.well.bad2.complete[id,]
test <- uniq.well.bad2.complete[-id,]

bad.mod1.rf <- randomForest(Bad ~ Target.Sand + Max.DLS + GLR, data = train, importance = TRUE)
bad.mod2.rf <- randomForest(Bad ~ Max.DLS + GLR, data = train, importance = TRUE)
varImpPlot(bad.mod2.rf, sort=TRUE)
plotRFVarImp(bad.mod1.rf)

test.pred <- predict(bad.mod1.rf, test)
table(obs=test$Bad, pred=test.pred)

test.pred2 <- predict(bad.mod2.rf, test)
table(obs=test$Bad, pred=test.pred2)








# visualize trees results
bad.mod.vis <- rpart(Bad ~ Max.DLS  + GLR, data = d.bad2.complete)
plot(bad.mod.vis)
text(bad.mod.vis)

bad.mod.rf <- randomForest(Bad ~ Target.Sand + Max.DLS + GLR, data = d.bad2.complete, importance = TRUE)
plotRFVarImp(bad.mod.rf)


#####################################################
# random forest model for failure runtime prediction
#####################################################
dat <- d.run.n %>% select(-Well.Name, -(Currently.Running:GLR), -Failure.Date, -Well.BOL, -VSP)
dat2 <- dat %>% select(-Bad.Actor)

# split data into training and test 2/3 vs 1/3
id <- sample(nrow(dat), nrow(dat)*0.667)  
train <- dat[id,]
test  <- dat[-id,]

run.mod.rf <- randomForest(Failure.Runtime ~ ., data = train, importance = TRUE)

varImpPlot(run.mod.rf, sort=TRUE)
test.pred <- predict(run.mod.rf, test)
sol <- data.frame(obs=test$Failure.Runtime, pred=test.pred)









train2 <- d.grp2[id,]
test2 <- d.grp2[-id,]


mod0.rf <- randomForest(Bad.Actor ~ Max.DLS + Target.Sand + GLR, data = train0, importance = TRUE)
varImpPlot(mod0.rf, sort=TRUE)
test0.pred <- predict(mod0.rf, test0)
table(obs=test0$Bad.Actor, pred=test0.pred)

mod1.rf <- randomForest(Bad ~ Max.DLS + Target.Sand + GLR, data = train1, importance = TRUE)
varImpPlot(mod1.rf, sort=TRUE)
test1.pred <- predict(mod1.rf, test1)
table(obs=test1$Bad, pred=test1.pred)

mod2.rf <- randomForest(Bad ~ Max.DLS + GLR, data = train2, importance = TRUE)
varImpPlot(mod2.rf, sort=TRUE)
test2.pred <- predict(mod2.rf, test2)
table(obs=test2$Bad, pred=test2.pred)



mod.rf <- randomForest(Bad.Actor ~ Max.DLS + GLR, data = d.bad.actor.complete, importance = TRUE)
varImpPlot(mod.rf, sort=TRUE)

# visualize trees results
mod.rf2 <- rpart(Bad.Actor ~ Max.DLS  + GLR, data = d.bad.actor.complete)
plot(mod.rf2)
text(mod.rf2)


##################################################
# logistic regression
##################################################
mod.logit <- glm(Bad ~  GLR+Max.DLS, data=train, family = binomial(link = 'logit'))
summary(mod.logit)

# prediction on test data
pred <- predict(mod.logit, newdata = test, type = "response")
pred.sol <- as.factor(ifelse(pred>0.5, "Y", "N"))
table(pred.sol, test$Bad)



##################################################
# Survival Analysis
##################################################
dat <- d %>% 
        mutate(Sensor=ifelse(Currently.Running=="Y", 1, 0)) %>% 
        mutate(TaperD.86=ifelse(d$Taper.Design=="86", 1, 0)) %>% 
        mutate(TaperD.87=ifelse(d$Taper.Design=="87", 1, 0)) %>% 
        mutate(TaperD.88=ifelse(d$Taper.Design=="88", 1, 0)) %>%
        mutate(TaperD.CordSE=ifelse(d$Taper.Design=="Corod SE", 1, 0)) %>% 
        mutate(TaperD.Fiberglass=ifelse(d$Taper.Design=="Fiberglass", 1, 0)) %>%
        mutate(Rod.Corod=ifelse(d$Rod.Type=="Corod", 1, 0)) %>%
        mutate(Rod.Fiberglass=ifelse(d$Rod.Type=="Fiberglass", 1, 0)) %>%
        mutate(Rod.KD=ifelse(d$Rod.Type=="KD", 1, 0)) %>%
        mutate(Rod.Misc=ifelse(d$Rod.Type=="Misc", 1, 0)) %>%
        mutate(Rod.T66=ifelse(d$Rod.Type=="T66", 1, 0)) %>%
        mutate(Guide.Full=ifelse(d$Guide.Design=="Fully guided", 1, 0)) %>%
        #mutate(Guide.None=ifelse(d$Guide.Design=="None", 1, 0)) %>%
        mutate(Guide.Some=ifelse(d$Guide.Design=="Some", 1, 0)) %>%  
        mutate(Stabilizer.None=ifelse(d$Stabilizer.Location=="None", 1, 0)) %>% 
        select(Failure.Runtime, Cap.String, Coated.Tubing, TaperD.86, TaperD.87, TaperD.CordSE, TaperD.CordSE, TaperD.Fiberglass, TaperD.88, 
               Rod.Corod, Rod.Fiberglass, Rod.KD, Rod.Misc, Rod.T66, Guide.Full,  Guide.Some,   #Guide.None, 
               Tubing.Pump, Desander, Feet.of.Sinker.Bars, Sinker.Bars, Stabilizer.None, Pump.Set.Depth.ft.TVD, Sensor, GLR)


is.fact <- sapply(dat, is.factor)
lapply(dat[,is.fact], table)
        

dat1 <- dat %>%
            filter(Coated.Tubing=="N", Cap.String=="N") %>% # always fix
            filter(TaperD.86==1) %>% 
            filter(Rod.KD==1|Rod.T66==1)
                   #Tubing.Pump=="N", Desander=="N", Stabilizer.None==1, Guide.None==0)


dat1$Stabilizer.None <- as.factor(dat1$Stabilizer.None)
plot_Box(dat1, x="Sinker.Bars", y="Failure.Runtime", title="Failure Runtime vs. Sinker Bars")
plot_Box(dat1, x="Stabilizer.None", y="Failure.Runtime", title="Failure Runtime vs. No Stabilizer")
plot_Box(dat1, x="Desander", y="Failure.Runtime", title="Failure Runtime vs. Desander")
plot_Box(dat1, x="Tubing.Pump", y="Failure.Runtime", title="Failure Runtime vs. Tubing Pump")

dat1.guide <- dat1 %>% filter(Guide.Full==1|Guide.Some==1) %>% mutate(Guide=ifelse(Guide.Full==1, "Full", "Some"))
plot_Box(dat1.guide, x="Guide", y="Failure.Runtime", title="Failure Runtime vs. Guide")

dat1.rod <- dat1 %>% filter(Rod.KD==1|Rod.T66==1) %>% mutate(Rod=ifelse(Rod.KD==1, "KD", "T66"))
plot_Box(dat1.rod, x="Rod", y="Failure.Runtime", title="Failure Runtime vs. Rod")

Gaussian.Mod <- survreg(Surv(Failure.Runtime, Sensor)~
                          #TaperD.86+TaperD.Fiberglass+
                          Rod.KD+Rod.T66+ #Rod.Fiberglass+
                          Tubing.Pump+Desander+Guide.Full+Guide.Some+ # Guide.None
                          Sinker.Bars + Stabilizer.None+#+#Feet.of.Sinker.Bars+
                          Pump.Set.Depth.ft.TVD,#+GLR, 
                        data=dat1, na.action=na.omit, dist="gaussian")

plot(dat1$Failure.Runtime, log(dat1$Failure.Runtime), xlab="Runtime", ylab="Log(Runtime)")

Gaussian.Mod2 <- survreg(Surv(Failure.Runtime, Sensor)~
                          #TaperD.86+TaperD.Fiberglass+
                          Rod.KD+Rod.T66+ #Rod.Fiberglass+
                          Tubing.Pump+Desander+Guide.Full+Guide.Some+ # Guide.None
                          Sinker.Bars + Stabilizer.None,#+#+#Feet.of.Sinker.Bars+
                          #Pump.Set.Depth.ft.TVD,#+GLR, 
                        data=dat1, na.action=na.omit, dist="gaussian")

Lognormal.Mod <- survreg(Surv(Failure.Runtime, Sensor)~
                          #TaperD.86+TaperD.Fiberglass+
                          Rod.KD+Rod.T66+ #Rod.Fiberglass+
                          Tubing.Pump+Desander+Guide.Full+Guide.Some+ # Guide.None
                          Sinker.Bars + Stabilizer.None+#+#Feet.of.Sinker.Bars+
                          Pump.Set.Depth.ft.TVD,#+GLR, 
                        data=dat1, na.action=na.omit, dist="lognormal")



Exp.Mod <- survreg(Surv(Failure.Runtime, Sensor)~
                           #TaperD.86+TaperD.Fiberglass+
                           Rod.KD+Rod.T66+ #Rod.Fiberglass+
                           Tubing.Pump+Desander+Guide.Full+Guide.Some+ # Guide.None
                           Sinker.Bars + Stabilizer.None+#+#Feet.of.Sinker.Bars+
                           Pump.Set.Depth.ft.TVD,#+GLR, 
                         data=dat1, na.action=na.omit, dist="exponential")
anova(Lognormal.Mod, Exp.Mod, Gaussian.Mod)

anova(Gaussian.Mod, Gaussian.Mod2)


imp <- coef(Gaussian.Mod)[-c(1,3)]
imp <- exp(imp)
imp.scale <- ifelse(imp<1, 1/imp, imp)
imp <- as.data.frame(imp)
imp <- imp[order(imp$`exp(imp)`),]
imp <- sort(abs(imp))
anova(Gaussian.Mod, Gaussian.Mod1)





dat2 <- dat %>% filter(Coated.Tubing=="N", Cap.String=="N", TaperD.88==0, Rod.Corod==0, Rod.Misc==0)

dat3 <- dat %>% filter(Coated.Tubing=="N", Cap.String=="N", TaperD.86==1)
  

LogNormal.Mod <- survreg(Surv(Failure.Runtime, Sensor)~TaperD.86+TaperD.87+TaperD.CordSE+TaperD.CordSE+TaperD.Fiberglass+
                                                   Rod.Corod+Rod.Fiberglass+Rod.KD+Rod.Misc+Rod.T66+Guide.None+Tubing.Pump+Desander+ #Guide.Full+Guide.None+Guide.Some
                                                   Feet.of.Sinker.Bars+Sinker.Bars+Stabilizer.None+Pump.Set.Depth.ft.TVD+GLR, 
                                                   data=dat2, na.action=na.omit, dist="lognormal")


Exp.Mod <- survreg(Surv(Failure.Runtime, Sensor)~TaperD.86+TaperD.87+TaperD.CordSE+TaperD.CordSE+TaperD.Fiberglass+
                       Rod.Corod+Rod.Fiberglass+Rod.KD+Rod.Misc+Rod.T66+Guide.None+Tubing.Pump+Desander+ #Guide.Full+Guide.None+Guide.Some
                       Feet.of.Sinker.Bars+Sinker.Bars+Stabilizer.None+Pump.Set.Depth.ft.TVD+GLR, 
                       data=dat2, na.action=na.omit, dist="exponential")
  

Gaussian.Mod <- survreg(Surv(Failure.Runtime, Sensor)~TaperD.86+TaperD.87+TaperD.CordSE+TaperD.CordSE+TaperD.Fiberglass+
                          #Rod.Fiberglass+Rod.KD+Rod.T66+Guide.None+Tubing.Pump+Desander+ #Guide.Full+Guide.None+Guide.Some
                          Guide.None+Tubing.Pump+Desander+ #Guide.Full+Guide.None+Guide.Some
                          Feet.of.Sinker.Bars+Sinker.Bars+Stabilizer.None+Pump.Set.Depth.ft.TVD+GLR, 
                          data=dat2, na.action=na.omit, dist="gaussian")

Gaussian.Mod <- survreg(Surv(Failure.Runtime, Sensor)~
                          Rod.Fiberglass+Rod.KD+Rod.T66+Guide.None+Tubing.Pump+Desander+ #Guide.Full+Guide.None+Guide.Some
                          Guide.None+Tubing.Pump+Desander+ #Guide.Full+Guide.None+Guide.Some
                          Feet.of.Sinker.Bars+Sinker.Bars+Stabilizer.None+Pump.Set.Depth.ft.TVD+GLR, 
                        data=dat2, na.action=na.omit, dist="gaussian")





anova(LogNormal.Mod, Exp.Mod, Gaussian.Mod)




summary(Gaussian.Mod)
coef <- coef(Gaussian.Mod)[-c(1, length(coef(Gaussian.Mod)))]
sort(exp(coef), decreasing=T)










survfit(Surv(Failure.Runtime, Sensor)~1, data=dat) -> out0
names(out0)
plot(out0, ylab='S(t)', xlab='t')

survfit(Surv(Failure.Runtime, Sensor)~factor(Bad), data=dat) -> out1
plot(out1, col=1:2, ylab='S(t)', xlab='t')

# cox proportion model
coxph(Surv(Failure.Runtime, Sensor)~Cap.String + Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+VSP+Desander+Feet.of.Sinker.Bars+ Sinker.Bars+factor(Stabilizer.Location)+Pump.Set.Depth.ft.TVD+Bad, data=dat)->out1.cox
coxph(Surv(Failure.Runtime, Sensor)~Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+VSP+Desander+ Sinker.Bars+factor(Stabilizer.Location)+Bad, data=dat)->out2.cox
coxph(Surv(Failure.Runtime, Sensor)~ factor(Taper.Design), data=dat)->out3.cox

LogNormalReg <- survreg(Surv(Failure.Runtime, Sensor)~Cap.String + Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+Desander+Feet.of.Sinker.Bars+ Sinker.Bars+factor(Stabilizer.Location)+Pump.Set.Depth.ft.TVD, data=dat, na.action=na.omit, dist="lognormal")
WeibullReg   <- survreg(Surv(Failure.Runtime, Sensor)~Cap.String + Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+Desander+Feet.of.Sinker.Bars+ Sinker.Bars+factor(Stabilizer.Location)+Pump.Set.Depth.ft.TVD, data=dat, na.action=na.omit, dist="weibull")
expReg   <- survreg(Surv(Failure.Runtime, Sensor)~Cap.String + Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+Desander+Feet.of.Sinker.Bars+ Sinker.Bars+factor(Stabilizer.Location)+Pump.Set.Depth.ft.TVD, data=dat, na.action=na.omit, dist="exponential")
gaussianReg   <- survreg(Surv(Failure.Runtime, Sensor)~Cap.String + Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+Desander+Feet.of.Sinker.Bars+ Sinker.Bars+factor(Stabilizer.Location)+Pump.Set.Depth.ft.TVD, data=dat, na.action=na.omit, dist="gaussian")
loglogiReg   <- survreg(Surv(Failure.Runtime, Sensor)~Cap.String + Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+Desander+Feet.of.Sinker.Bars+ Sinker.Bars+factor(Stabilizer.Location)+Pump.Set.Depth.ft.TVD, data=dat, na.action=na.omit, dist="loglogistic")
#logiReg   <- survreg(Surv(Failure.Runtime, Sensor)~Cap.String + Coated.Tubing + factor(Taper.Design) + factor(Rod.Type) + factor(Guide.Design)+Tubing.Pump+Desander+Feet.of.Sinker.Bars+ Sinker.Bars+factor(Stabilizer.Location)+Pump.Set.Depth.ft.TVD+Bad, data=dat, na.action=na.omit, dist="logistic")

anova(LogNormalReg, WeibullReg, expReg, gaussianReg, loglogiReg)
anova(LogNormalReg, expReg, gaussianReg, loglogiReg)

summary(gaussianReg)
coef <- sort(exp(coef(gaussianReg)[-1]), decreasing=T)


imp.dat <- data.frame(
  Var = factor(names(var), levels=names(var)),
  Importance = var
)

plot(log(gaussianReg$Failure.Runtime),log(-log(gaussianReg$surv)), type='n', xlab=expression(log(t)), ylab=expression(log(-log(S(t)))))

ggplot(data=imp.dat[-c(1,23),], aes(x=Var, y=Importance, fill=Importance)) +
  geom_bar(stat="identity")
