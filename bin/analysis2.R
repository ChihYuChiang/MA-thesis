library(tidyverse)
library(data.table)
library(colorspace)
library(corrplot)

#Read in as DT
#Skip 2 for codec
DT <- fread("../../data/survey2.csv", skip=2)[
  MTurkCode != "", ,] #Filter

#Read in codec
codec <- as.data.table(t(fread("../../data/survey2.csv", nrows=1)), keep.rownames=TRUE)
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
colIndex_InS <- grep("^PersonInS-\\d$", names(DT))
colIndex_OutS <- grep("^PersonOutS-\\d$", names(DT))
colIndex_IdS <- grep("^PersonIdS-\\d$", names(DT))

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
DT[, "PersonInSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonInSOutS-\\d$", names(DT))]
DT[, "PersonIdSInS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSInS-\\d$", names(DT))]
DT[, "PersonIdSOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdSOutS-\\d$", names(DT))]

#Gap absolute sum
DT[, "PersonInSOutS-absum" := rowSums(.SD), .SDcols=grep("^PersonInSOutS-ab\\d$", names(DT))]
DT[, "PersonIdSInS-absum" := rowSums(.SD), .SDcols=grep("^PersonIdSInS-ab\\d$", names(DT))]
DT[, "PersonIdSOutS-absum" := rowSums(.SD), .SDcols=grep("^PersonIdSOutS-ab\\d$", names(DT))]

#InS, OutS, IdS sum
DT[, "PersonInS-sum" := rowSums(.SD), .SDcols=grep("^PersonInS-\\d$", names(DT))]
DT[, "PersonOutS-sum" := rowSums(.SD), .SDcols=grep("^PersonOutS-\\d$", names(DT))]
DT[, "PersonIdS-sum" := rowSums(.SD), .SDcols=grep("^PersonIdS-\\d$", names(DT))]

#Update codec
codec <- rbind(codec, list("PersonOO-Z",
                           "Personality gaps.
                           OO = {InSOutS, IdSInS, IdSOutS; eg. IdSInS: ideal(self-version) - in-game(self-version)}
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
DT[, (newColName) := abs(InSOutS)]

#Id - In (original and absolute)
IdIn <- DT[, colIndex_Id, with=FALSE] - DT[, colIndex_In, with=FALSE]
newColName <- gsub("In", "IdIn", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdIn]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(InSOutS)]

#Id - Out (original and absolute)
IdOut <- DT[, colIndex_Id, with=FALSE] - DT[, colIndex_Out, with=FALSE]
newColName <- gsub("In", "IdOut", grep("^SDTIn-\\d$", names(DT), value=TRUE))
DT[, (newColName) := IdOut]
newColName <- gsub("(\\d)", "ab\\1", newColName)
DT[, (newColName) := abs(InSOutS)]

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
### Clean temp vars
"
rm(list=ls()[which(ls() != "DT" & ls() != "codec")]) #Preserve only DT and codec








"
----------------------------------------------------------------------
## Exploration
----------------------------------------------------------------------
"
"
### Distribution comparison
"
#--Personality
#Function for distribution
dist_personality <- function(DT, personality, types){
  #A map for personality code and str pairs
  personaCodec <- c("1"="Extraversion", "2"="Agreeableness", "3"="Conscientiousness;", "4"="Emotion stability", "5"="Openness")
  typeCodec <- c("InS"="In-game / Self", "OutS"="Real / Self", "IdS"="Ideal / Self", "InF"="In-game / Fellow", "OutF"="Real / Fellow", "SteS"="Stereotype / Public")
  
  #Acquire specific columns of that personality
  targetColIndex <- matches(sprintf("^Person.+-%s$", personality), vars=names(DT))
  
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("Person%s-%s", type, personality)), fill=toString(which(types == type))),
                   binwidth=0.5, alpha=0.6)
  }
  geom_hists <- lapply(types, make_hist)
  
  #Use a list to add ggplot components
  ggplot(data=DT[, targetColIndex, with=FALSE]) +
    geom_hists +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7), limits=c(0.5, 7.5)) +
    labs(x="score", title=personaCodec[toString(personality)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(typeCodec[unlist(types)])) +
    theme_minimal()
}

#Function call
#InS OutS IdS InF OutF SteS
dist_personality(DT, 4, list("IdS"))


#--SDT
#Function for distribution
dist_SDT <- function(DT, SDT, types){
  #A map for personality code and str pairs
  SDTCodec <- c("1"="Autonomy", "2"="Relatedness", "3"="Competence")
  typeCodec <- c("In"="In-game", "Out"="Real", "Id"="Ideal")
  
  #Acquire specific columns of that personality
  targetColIndex <- matches(sprintf("^SDT.+-%s$", SDT), vars=names(DT))
  
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("SDT%s-%s", type, SDT)), fill=toString(which(types == type))),
                   binwidth=0.5, alpha=0.6)
  }
  geom_hists <- lapply(types, make_hist)
  
  #Use a list to add ggplot components
  ggplot(data=DT[, targetColIndex, with=FALSE]) +
    geom_hists +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7), limits=c(0.5, 7.5)) +
    labs(x="score", title=SDTCodec[toString(SDT)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(typeCodec[unlist(types)])) +
    theme_minimal()
}

#Function call
#In Out Id
dist_SDT(DT, 3, list("In", "Out"))




"
### Distribution
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
### T test
"
t.test(DT$`PersonIdSInS-1`, DT$`PersonIdSOutS-1`, paired=TRUE)
t.test(DT$`PersonIdSInS-2`, DT$`PersonIdSOutS-2`, paired=TRUE)
t.test(DT$`PersonIdSInS-3`, DT$`PersonIdSOutS-3`, paired=TRUE)
t.test(DT$`PersonIdSInS-4`, DT$`PersonIdSOutS-4`, paired=TRUE)
t.test(DT$`PersonIdSInS-5`, DT$`PersonIdSOutS-5`, paired=TRUE)