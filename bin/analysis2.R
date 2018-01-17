library(tidyverse)
library(data.table)
library(colorspace)
library(corrplot)
library(glmnet)

#Read in as DT
#Skip 2 for codec
DT <- fread("../data/survey2.csv", skip=2)[
  MTurkCode != "", ,] #Filter

#Read in codec
codec <- as.data.table(t(fread("../data/survey2.csv", nrows=1)), keep.rownames=TRUE)
colnames(codec) <- c("Variable", "Description")







"
----------------------------------------------------------------------
## Initialization
----------------------------------------------------------------------
"
"
### Reverse (1-7 Likert) target responses
"
#--Select target columns
#Personality: 1_24 2_135; SDT: 1_246 2_246; Preference: -2
targetColIndex <- grep("(^Person.+((1_[24])|(2_[135]))$)|(^SDT.+_[246]$)|(^Pref.-2)", names(DT), value=TRUE)


#--Reverse 1-7 likert
reversed <- 8 - DT[, targetColIndex, with=FALSE]


#--Assign back to DT
#Use parenthesis since the synax does not allow with=FALSE here
DT[, (targetColIndex) := reversed]




"
### Combine sub-items
"
#--personalities (5 constructs; 2 items each)
#Computation
subColIndex_1 <- grep("^Person.+1_\\d$", names(DT))
subColIndex_2 <- grep("^Person.+2_\\d$", names(DT))
personalities <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

#Substitution
newColName <- gsub("1_", "", grep("^Person.+1_\\d$", names(DT), value=TRUE))
DT[, (newColName) := personalities]

#Update codec
codec <- rbind(codec, list("PersonXY-Z",
                           "Combined personality measurement.
                           X = {In: in-game, Out: real, Id: ideal, Ste: stereotype}
                           Y = {S: self-version, F: fellow-version}
                           Z = {1: extraversion, 2: agreeableness, 3: conscientiousness, 4: emotion stability, 5: openness, sum: summation}"))


#--SDT (3 constructs; 4 items each)
#Computation
subColIndex_1 <- grep("^SDT.+1_[135]$", names(DT))
subColIndex_2 <- grep("^SDT.+1_[246]$", names(DT))
subColIndex_3 <- grep("^SDT.+2_[135]$", names(DT))
subColIndex_4 <- grep("^SDT.+2_[246]$", names(DT))
SDTs <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE] + DT[, subColIndex_3, with=FALSE] + DT[, subColIndex_4, with=FALSE]) / 4

#Substitution
newColName <- gsub("1_", "", grep("^SDT.+1_[123]$", names(DT), value=TRUE))
DT[, (newColName) := SDTs]

#Update codec
codec <- rbind(codec, list("SDTX-Z",
                           "Combined SDT measurement.
                           X = {In: in-game, Out: real, Id: ideal}
                           Z = {1: autonomy, 2: relatedness, 3: competence, sum: summation}"))


#--Preference (5 items)
DT[, "PrefS-a1" := rowMeans(.SD), .SDcols=grep("^PrefS-\\d$", names(DT))] #All 5 measures
DT[, "PrefS-a2" := rowMeans(.SD), .SDcols=grep("^PrefS-[1234]$", names(DT))] #Except play frequency
DT[, "PrefF-a1" := rowMeans(.SD), .SDcols=grep("^PrefF-\\d$", names(DT))]
DT[, "PrefF-a2" := rowMeans(.SD), .SDcols=grep("^PrefF-[1234]$", names(DT))]

#Update codec
codec <- rbind(codec, list("PrefX-aZ",
                           "Combined preference measurement.
                           X = {S: self-version, F: fellow-version}
                           Z = {1: all 5 measures, 2: exclude play frequency}"))




"
### Compute gaps and sums
"
#--Personality
colIndex_InF <- grep("^PersonInF-\\d$", names(DT))
colIndex_OutF <- grep("^PersonOutF-\\d$", names(DT))
colIndex_InS <- grep("^PersonInS-\\d$", names(DT))
colIndex_OutS <- grep("^PersonOutS-\\d$", names(DT))
colIndex_IdS <- grep("^PersonIdS-\\d$", names(DT))

#InF - OutF (original and absolute)
InFOutF <- DT[, colIndex_InF, with=FALSE] - DT[, colIndex_OutF, with=FALSE]
newColName <- gsub("InF", "InFOutF", grep("^PersonInF-\\d$", names(DT), value=TRUE))
DT[, (newColName) := InFOutF]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(InFOutF)]

#InS - OutS (original and absolute)
InSOutS <- DT[, colIndex_InS, with=FALSE] - DT[, colIndex_OutS, with=FALSE]
newColName <- gsub("InS", "InSOutS", grep("^PersonInS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := InSOutS]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(InSOutS)]

#IdS - InS (original and absolute)
IdSInS <- DT[, colIndex_IdS, with=FALSE] - DT[, colIndex_InS, with=FALSE]
newColName <- gsub("InS", "IdSInS", grep("^PersonInS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdSInS]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(IdSInS)]

#IdS - OutS (original and absolute)
IdSOutS <- DT[, colIndex_IdS, with=FALSE] - DT[, colIndex_OutS, with=FALSE]
newColName <- gsub("InS", "IdSOutS", grep("^PersonInS-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdSOutS]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(IdSOutS)]

#Gap sum
DT[, "PersonInFOutF-sum" := rowSums(.SD), .SDcols=grep("^PersonInFOutF-\\d$", names(DT))]
DT[, "PersonInSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonInSOutS-\\d$", names(DT))]
DT[, "PersonIdSInS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSInS-\\d$", names(DT))]
DT[, "PersonIdSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSOutS-\\d$", names(DT))]

#Gap absolute sum
DT[, "PersonInFOutF-absum" := rowSums(.SD), .SDcols=grep("^PersonInFOutF-ab\\d$", names(DT))]
DT[, "PersonInSOutS-absum" := rowSums(.SD), .SDcols=grep("^PersonInSOutS-ab\\d$", names(DT))]
DT[, "PersonIdSInS-absum" := rowSums(.SD), .SDcols=grep("^PersonIdSInS-ab\\d$", names(DT))]
DT[, "PersonIdSOutS-absum" := rowSums(.SD), .SDcols=grep("^PersonIdSOutS-ab\\d$", names(DT))]

#InS, OutS, IdS, SteS sum
DT[, "PersonInF-sum" := rowSums(.SD), .SDcols=grep("^PersonInF-\\d$", names(DT))]
DT[, "PersonOutF-sum" := rowSums(.SD), .SDcols=grep("^PersonOutF-\\d$", names(DT))]
DT[, "PersonInS-sum" := rowSums(.SD), .SDcols=grep("^PersonInS-\\d$", names(DT))]
DT[, "PersonOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonOutS-\\d$", names(DT))]
DT[, "PersonIdS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdS-\\d$", names(DT))]
DT[, "PersonSteS-sum" := rowSums(.SD), .SDcols=grep("^PersonSteS-\\d$", names(DT))]

#Update codec
codec <- rbind(codec, list("PersonOO-Z",
                           "Personality gaps.
                           OO = {InFOutF, InSOutS, IdSInS, IdSOutS; eg. IdSInS: ideal(self-version) - in-game(self-version)}
                           Z = {1: extraversion, 2: agreeableness, 3: conscientiousness, 4: emotion stability, 5: openness, sum: summation, ab(prefix): absolute}"))


#--SDT
colIndex_In <- grep("^SDTIn-\\d$", names(DT))
colIndex_Out <- grep("^SDTOut-\\d$", names(DT))
colIndex_Id <- grep("^SDTId-\\d$", names(DT))

#In - Out (original and absolute)
InOut <- DT[, colIndex_In, with=FALSE] - DT[, colIndex_Out, with=FALSE]
newColName <- gsub("In", "InOut", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := InOut]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(InOut)]

#Id - In (original and absolute)
IdIn <- DT[, colIndex_Id, with=FALSE] - DT[, colIndex_In, with=FALSE]
newColName <- gsub("In", "IdIn", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdIn]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(IdIn)]

#Id - Out (original and absolute)
IdOut <- DT[, colIndex_Id, with=FALSE] - DT[, colIndex_Out, with=FALSE]
newColName <- gsub("In", "IdOut", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdOut]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(IdOut)]

#Gap sum
DT[, "SDTInOut-sum" := rowSums(.SD), .SDcols=grep("^SDTInOut-\\d$", names(DT))]
DT[, "SDTIdIn-sum" := rowSums(.SD), .SDcols=grep("^SDTIdIn-\\d$", names(DT))]
DT[, "SDTIdOut-sum" := rowSums(.SD), .SDcols=grep("^SDTIdOut-\\d$", names(DT))]

#Gap absolute sum
DT[, "SDTInOut-absum" := rowSums(.SD), .SDcols=grep("^SDTInOut-ab\\d$", names(DT))]
DT[, "SDTIdIn-absum" := rowSums(.SD), .SDcols=grep("^SDTIdIn-ab\\d$", names(DT))]
DT[, "SDTIdOut-absum" := rowSums(.SD), .SDcols=grep("^SDTIdOut-ab\\d$", names(DT))]

#In, Out, Id sum
DT[, "SDTIn-sum" := rowSums(.SD), .SDcols=grep("^SDTIn-\\d$", names(DT))]
DT[, "SDTOut-sum" := rowSums(.SD), .SDcols=grep("^SDTOut-\\d$", names(DT))]
DT[, "SDTId-sum" := rowSums(.SD), .SDcols=grep("^SDTId-\\d$", names(DT))]

#Update codec
codec <- rbind(codec, list("SDTOO-Z",
                           "SDT gaps.
                           OO = {InOut, IdIn, IdOut; eg. IdIn: ideal - in-game}
                           Z = {1: autonomy, 2: relatedness, 3: competence, sum: summation, ab(prefix): absolute}"))


"
### Clean temp vars and save the environment
"
rm(list=ls()[which(ls() != "DT" & ls() != "codec")]) #Preserve only DT and codec

save.image()








"
----------------------------------------------------------------------
## Exploration
----------------------------------------------------------------------
"
"
### Distribution comparison
"
#Function for distribution comparison
dist_compare <- function(construct, types, item, gap=0) {
  #A map for construct and item code and str pairs
  strCodec <- list(
    "Person"=list(
      item=c("1"="Extraversion", "2"="Agreeableness", "3"="Conscientiousness", "4"="Emotion stability", "5"="Openness", "sum"="Summation",
             "ab1"="Extraversion (absolute)", "ab2"="Agreeableness (absolute)", "ab3"="Conscientiousness (absolute)", "ab4"="Emotion stability (absolute)", "ab5"="Openness (absolute)", "absum"="Summation (absolute)"),
      type=c("InS"="In-game (self)", "OutS"="Real (self)", "IdS"="Ideal (self)", "InF"="In-game (fellow)", "OutF"="Real (fellow)", "SteS"="Stereotype (self)",
             "InSOutS"="In-game - real", "IdSInS"="Ideal - in-game", "IdSOutS"="Ideal - real")
    ),
    "SDT"=list(
      item=c("1"="Autonomy", "2"="Relatedness", "3"="Competence", "sum"="Summation",
             "ab1"="Autonomy (absolute)", "ab2"="Relatedness (absolute)", "ab3"="Competence (absolute)", "absum"="Summation (absolute)"),
      type=c("In"="In-game", "Out"="Real", "Id"="Ideal",
             "InOut"="In-game - real", "IdIn"="Ideal - in-game", "IdOut"="Ideal - real")
    )
  )
  
  #Decide scales according to item and gap
  #Complication is bad!!!!!
  itemNo <- c("Person"=5, "SDT"=3)[construct]
  scales <- list(
    binwidth=if (item == "sum" | item == "absum") 0.5 * itemNo else 0.5,
    limits=if (gap == 1) {
      if (item == "sum" | item == "absum") c(-6 * itemNo - 0.5 * itemNo, 6 * itemNo + 0.5 * itemNo) else c(-6.5, 6.5)
    } else if (gap == 0) {
      if (item == "sum") c(1 * itemNo - 0.5 * itemNo, 7 * itemNo + 0.5 * itemNo) else c(0.5, 7.5)
    },
    breaks=if (gap == 1) {
      if (item == "sum" | item == "absum") seq(-6 * itemNo, 6 * itemNo, itemNo) else seq(-6, 6)
    } else if (gap == 0) {
      if (item == "sum") seq(1 * itemNo, 7 * itemNo, itemNo) else seq(1, 7)
    }
  )
  
  #Make individual hist
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("%s%s-%s", construct, type, item)), fill=toString(which(types == type))),
                   binwidth=scales$binwidth, alpha=0.6)
  }
  
  #Make hist list of all items
  geom_hists <- lapply(types, make_hist)
  
  #Use the list to add ggplot components
  ggplot(data=DT) +
    geom_hists +
    scale_x_continuous(breaks=scales$breaks, minor_breaks=NULL, labels=scales$breaks, limits=scales$limits) +
    labs(x="score", title=strCodec[[construct]]$item[toString(item)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(strCodec[[construct]]$type[unlist(types)])) + #labels does not accept names vector
    theme_minimal()
}

#Function call
#Options refer to the strCodec
dist_compare("Person", list("InS", "OutS", "IdS"), "sum", gap=0)
dist_compare("Person", list("IdSOutS", "IdSInS"), "sum", gap=1)
dist_compare("SDT", list("In", "Out", "Id"), 1, gap=0)
dist_compare("SDT", list("IdOut", "IdIn"), "sum", gap=1)




"
### Distribution and description
"
#Function for dist
dist_gen <- function (targetColName) {
  ggplot(data=DT[, targetColName, with=FALSE]) +
    geom_histogram(mapping=aes_(x=as.name(targetColName)),
                   bins=nrow(table(DT[, targetColName, with=FALSE])), binwidth=1, alpha=0.65) +
    labs(title=targetColName) +
    theme_minimal()
}
lapply(c("Demo-1", "Demo-2", "GProfile-1"), dist_gen)

#Description
summary(DT[, c("Demo-1", "Demo-2", "GProfile-1"), with=FALSE])




"
### Cor table
"
#Use index or name for columns
targetColIndex <- grep("(^Person.+((1_[24])|(2_[135]))$)|(^SDT.+_[246]$)|(^Pref.-2)", names(DT), value=TRUE)
targetColName <- c("PersonInS-sum", "SDTId-2")

corrplot(cor(DT[, targetColName, with=FALSE]),
         method="color", type="upper", addCoef.col="black", diag=FALSE, tl.srt=45, tl.cex=0.8, tl.col="black",
         cl.pos="r", col=colorRampPalette(diverge_hcl(3))(100)) #From the palette, how many color to extrapolate




"
### Scatter plot
"
#Use name for columns
targetColName <- c("SDTInOut-sum", "PersonInSOutS-sum")

#Filter by criteria
#Potential filters: PrefS-5, PrefS-a1, PrefS-a2, GProfile-2, GProfile-4, GProfile-135, GProfile-10 11
criteria <- quote(get("PrefS-a1") > 0)

#Common mapping
p <- ggplot(mapping=aes_(x=as.name(targetColName[1]), y=as.name(targetColName[2])))

#Use filtered row number decide if add additional layers
if(DT[eval(criteria), .N,]) p <- p + geom_point(data=DT[eval(criteria), targetColName, with=FALSE], mapping=aes(color="g1"))
if(DT[!eval(criteria), .N,]) p <- p + geom_point(data=DT[!eval(criteria), targetColName, with=FALSE], mapping=aes(color="g2"))

#Plotting
p + scale_color_discrete(name="Group", labels=c("g1"="PrefS-a1 > 5", "g2"="PrefS-a1 < 5"))








"
----------------------------------------------------------------------
## Analysis
----------------------------------------------------------------------
"
"
### T test (paired)
"
tTest <- function(construct, types, item) {
  col1 <- sprintf("%s%s-%s", construct, types[1], item)
  col2 <- sprintf("%s%s-%s", construct, types[2], item)
  
  #DT does not accept as.name (symbol); it requires object
  testOutput <- t.test(DT[, get(col1)], DT[, get(col2)], paired=TRUE)
  
  #Rename the caption of output table
  testOutput$data.name <- paste(col1, "and", col2, sep=" ")
  
  return(testOutput)
}
tTest("Person", list("InS", "OutS"), "sum")




"
### Double Lasso selection (+ simple lm)
"
#--Function for updating lambda used in selection
#n = number of observation; p = number of independent variables; se = standard error of residual or dependent variable
updateLambda <- function(n, p, se) {se * (1.1 / sqrt(n)) * qnorm(1 - (.1 / log(n)) / (2 * p))}


#--Function for acquiring the indices of the selected variables in df_x
#df_x = matrix with only variables to be tested; y = dependent variable or treatment variables; lambda = the initial lambda computed in advance 
acquireBetaIndices <- function(df_x, y, lambda, n, p) {
  #glmnet accept only matrix not df
  df_x <- as.matrix(df_x)
  
  #Update lambda k times, k is selected based on literature
  k <- 1
  while(k < 15) {
    model_las <- glmnet(x=df_x, y=y, alpha=1, lambda=lambda, standardize=TRUE)
    beta <- coef(model_las)
    residual.se <- sd(y - predict(model_las, df_x))
    lambda <- updateLambda(n=n, p=p, se=residual.se)
    k <- k + 1
  }
  
  #Return the variable indices with absolute value of beta > 0
  return(which(abs(beta) > 0))
}


#--Function to perform double lasso selection
#output = a new df_yx with variables selected from df_yx
lassoSelect <- function(df_yx, df_ytreatment, df_test, outcomeVar) {
  #--Setting up
  #Df af all variables
  df_yx <- cbind(df_ytreatment, df_test)
  
  #The number of observations
  n <- nrow(df_test)
  
  #The number of variables to be tested
  p <- ncol(df_test)
  
  
  #--Select vars that predict outcome
  #Lambda is initialized as the se of residuals of a simple linear using only treatments predicting dependent variable
  #If the treatment var is NULL, use the se pf dependent var to initiate
  residual.se <- if(ncol(df_ytreatment) == 1) {sd(df_yx[[outcomeVar]])} else {sd(residuals(lm(as.formula(sprintf("`%s` ~ .", outcomeVar)), data=df_ytreatment)))}
  lambda <- updateLambda(n=n, p=p, se=residual.se)
  
  #by Lasso model: dependent variable ~ test variables
  betaIndices <- acquireBetaIndices(df_x=df_test, y=df_yx[[outcomeVar]], lambda=lambda, n=n, p=p)
  
  
  #--Select vars that predict treatments
  #Each column of the treatment variables as the y in the Lasso selection
  #Starting from 2 because 1 is the dependent variable
  if(ncol(df_ytreatment) != 1) { #Run only when treatment vars not NULL
    for(i in seq(2, ncol(df_ytreatment))) {
      #Acquire target treatment variable
      treatment <- df_ytreatment[[i]]
      
      #Lambda is initialized as the se of the target treatment variable
      treatment.se <- sd(treatment)
      lambda <- updateLambda(n=n, p=p, se=treatment.se)
      
      #Acquire the indices and union the result indices of each treatment variable
      betaIndices <- union(betaIndices, acquireBetaIndices(df_x=df_test, y=treatment, lambda=lambda, n=n, p=p))
    }
  }
  
  
  #Process the result indices to remove the first term (the intercept term)
  betaIndices <- setdiff((betaIndices - 1), 0)
  
  #Bind the selected variables with dependent and treatment variables
  df_yx_selected <- if(nrow(df_test[, ..betaIndices]) == 0) df_ytreatment else cbind(df_ytreatment, df_test[, ..betaIndices])
  
  #Return a new df_yx with variables selected
  return(df_yx_selected)
}


#--Use the function to acquire the selected dfs (the new dfs can be fed into the simple linear model)
df_ytreatment <- DT[, c("GProfile-1", "PrefS-a1")]
df_test <- DT[, c("Demo-1", "Demo-2", "Demo-4", "PrefF-1")]
outcomeVar <- "PrefS-a1"

DT_select <- lassoSelect(df_yx=DT, df_ytreatment=df_ytreatment, df_test=df_test, outcomeVar=outcomeVar)


#--Simple lm implementation
model_lm <- lm(as.formula(sprintf("`%s` ~ .", outcomeVar)), data=DT_select)
summary(model_lm)
