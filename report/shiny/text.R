"
### Filter
"
FILTER <- list(
  F1_1="Casual or Core Gamer",
  F1_2="GProfile-3_1",
  F1_3="(casual gamer = 1, core gamer = 7)",
  
  F2_1="Preference for the Focal Game",
  F2_2="PrefS-a2",
  F2_3="(casual gamer = 1, core gamer = 7)",
  
  F3_1="The Focal Game fit the taste",
  F3_2="PrefS-5",
  F3_3="(low = 1, high = 7)",
  
  F4_1="How familiar with the fellow player",
  F4_2="Relation-8",
  F4_3="(low = 1, high = 7)",
  
  F5_1="In video games, as myself or a different person",
  F5_2="GProfile-10_2",
  F5_3="(myself = 1, different person = 7)",
  
  F6_1="In video games, as usual self or a better version of self",
  F6_2="GProfile-11_2",
  F6_3="(usual self = 1, better version of self = 7)"
)




"
### Instructions
"
#--Double Lasso selection
DLS <- list(
  T1="Model (with Double Lasso Selection)",
  T2="(O. Urminsky, C. Hansen, and V. Chernozhukov, 2018)",
  
  C1="Select and update variables for each construct, outcome, treatment, and covariate.",
  C2="Treatments are variables will be included in the final model.",
  C3="Covariates are variables to be selected by the double Lasso selection process.",
  C4="Refer to \"Codec\" section for additional information of the variables.",
  C5="Select a type pf model to implement.",
  C6="Click the \"IMPLEMENT\" button to proceed.",
  C7="This app supports saving variable combinations. You can output them as a file or retrieve them later on.",
  C8="This app supports batch processing. You can implement the selected model on all var combinations at once.",
  
  N1="Select the number of variables as instructed.",
  N2="Please allow some time (should be < 10 secs) for the app to process.",
  N3="If the result is not display, try click the button again.",
  
  PH1="Select only 1 variable as outcome",
  PH2="Select 1+ variables as treatments",
  PH3="Select 2+ variables as covariates"
)


#--Personality
PERSON <- list(
  T1="Distribution of Personality Versions",
  
  C1="Select any number of versions to be displayed.",
  C2="If more than 1 version is selected, they are marked by different colors and can be compared.",
  C3="If exactly 2 versions are selected, the result of paired t-tests is also reported.",
  C4="If comparing the primary versions (e.g. Real and Ideal), the app produces 6 plots corresponding to the Big five's 5 items plus their summation.",
  C5="If comparing the gap versions (e.g. Ideal - Read and Ideal - In-game), the app produces the above 6 plots as well as their abolute versions.",
  C6="Click the \"DRAW DISTRIBUTION\" button to proceed.",
  
  N1="The absolute summation is acquired by first having the absolute value of each gap of each item and then doing the total.",
  N2="You can't and there's no reason to compare across primary versions and gap versions.",
  N3="Please allow some time (should be < 10 secs) for the app to process.",
  N4="If the result is not display, try click the button again.",
  
  ST1="Primary",
  ST2="Gap"
)


#--SDT
SDT <- list(
  T1="Distribution of SDT Versions",
  
  C1="Select any number of versions to be displayed.",
  C2="If more than 1 version is selected, they are marked by different colors and can be compared.",
  C3="If exactly 2 versions are selected, the result of paired t-tests is also reported.",
  C4="If comparing the primary versions (e.g. Real and Ideal), the app produces 3 plots corresponding to the SDT's 3 items plus their summation.",
  C5="If comparing the gap versions (e.g. Ideal - Read and Ideal - In-game), the app produces the above 3 plots as well as their abolute versions.",
  C6="Click the \"DRAW DISTRIBUTION\" button to proceed.",
  
  N1="The absolute summation is acquired by first having the absolute value of each gap of each item and then doing the total.",
  N2="You can't and there's no reason to compare across primary versions and gap versions.",
  N3="Please allow some time (should be < 10 secs) for the app to process.",
  N4="If the result is not display, try click the button again.",
  
  ST1="Primary",
  ST2="Gap"
)


#--Description
DESC <- list(
  T1="Description of Individual Variable",
  
  C1="Select any number of variables to be described.",
  C2="For each selected variable, the app produces 1) basic descrptive stats and 2) distribution histogram.",
  C3="Refer to \"Codec\" section for additional information of the variables.",
  C4="Click the \"DESCRIBE VARIABLE\" button to proceed.",
  C5="Click the \"CLEAR SELECTION\" button to remove current selections.",
  
  N1="For better display, select no more than 16 variables a time.",
  N2="Please allow some time (should be < 10 secs) for the app to process.",
  N3="If the result is not display, try click the button again."
)


#--Cor table
COR <- list(
  T1="Correlation Table",
  
  C1="Select 2 or more variables to examine the correlations.",
  C2="Refer to \"Codec\" section for additional information of the variables.",
  C3="If exactly 2 variables are selected, the scatter plot is also provided.",
  C4="Click the \"DESCRIBE VARIABLE\" button to proceed.",
  C5="Click the \"CLEAR SELECTION\" button to remove current selections.",
  
  N1="For better display, select more than 5 variables a time.",
  N2="Please allow some time (should be < 10 secs) for the app to process.",
  N3="If the result is not display, try click the button again."
)


#--Text responses
TEXT <- list(
  T1="Text Response",
  
  C1="This table shows results of primary survey items with text response.",
  C2="It includes the particular games they played, the main benefit of playing, and some general feedback regarding this survey.",
  C3="Please allow some time (should be < 5 secs) for loading the table.",
  C4="If the table is not displayed, try scroll the browser window, resize the window, or switch to other tags and try again.
  nd back."
)


#--Codec
CODEC <- list(
  T1="Variable Code Book",
  
  C1="This table shows primary items (variables) acquired in the survey.",
  C2="It includes the variable names and the actual prompts asked during the survey.",
  C3="Please allow some time (should be < 5 secs) for the app to process.",
  C4="If the table is not displayed, try scroll the browser window, resize the window, or switch to other tags and try again.
  nd back."
)