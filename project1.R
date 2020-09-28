#my pre-write up of the project to test functions

#In order to predict the reliability of large sets of software, engineers and computer scientists developed
#several algorithms. These 4 algorithms measure 4 different attributes of a given file. If these attributes acceed
#a given upper bound, different for each measurement, the algorithm predicts that file to have a defect. This data
#is stored in SWDEFECTS.csv.

swdefects.tab <- read.csv("SWDEFECTS.csv")
head(swdefects.tab)

#predict.vg.10 = cyclomatic complexity test
#predict.evg.14.5 = essential complexity test
#predict.ivg.9.2 = design complexity test
#predict.loc.50 = lines of code test

#number of defects predicted
defects.pd = with(swdefects.tab, sum((predict.vg.10 == "yes") | (predict.evg.14.5 == "yes")
                                                  | (predict.ivg.9.2 == "yes") | (predict.loc.50 == "yes")))
defects.pd

#number of non-defects predicted
notdefects.pd = with(swdefects.tab, sum((predict.vg.10 == "no") & (predict.evg.14.5 == "no")
                                        & (predict.ivg.9.2 == "no") & (predict.loc.50 == "no")))
notdefects.pd

nrow(swdefects.tab) == defects.pd + notdefects.pd

#total two-way summary table
predictions <- with(swdefects.tab, (predict.vg.10 == "yes") | (predict.evg.14.5 == "yes")
                                      | (predict.ivg.9.2 == "yes") | (predict.loc.50 == "yes"))
predictions
#takes a 4x4 table and a grand total and creates a two way summary table
twowaysum <- function(tab, total)
{
  mat = matrix(c(tab[1, 1], tab[1, 2], I(tab[1,1] + tab[1,2])
                 ,tab[2, 1], tab[2,2], I(tab[2,1] + tab[2, 2])
                 , I(tab[1,1] + tab[2,1]), I(tab[1,2] + tab[2,2]), total)
               , ncol=3,byrow=TRUE)
  colnames(mat) = c("No Defect", "Defect", "Row Totals")
  rownames(mat) = c("Not Predicted", "Predicted", "Column Totals")
  mat
}

totals.tab <- table(predictions, swdefects.tab$defect)

twowaysum(totals.tab, nrow(swdefects.tab))

##functions for probability measures.
#function for accuracy
acc=function(a,b,c,d){

  accuracy = (a+d)/(a+b+c+d)
  accuracy
}

#function for detection rate
detect=function(b,d){

  detection=d/(b+d)
  detection
}

#function for false alarm rate
falarm=function(a,c){

  false_alarm=c/(a+c)
  false_alarm
}

#function for precision
prec=function(c,d){

  precision=d/(c+d)
  precision
}

#tables for each predictive factor

#loc v defect

loc.tab <- with(swdefects.tab, table(predict.loc.50, defect))



twowaysum(loc.tab, nrow(swdefects.tab))

#cyclomatic complexity v defect
cyclo.tab <- with(swdefects.tab, table(predict.vg.10, defect))

twowaysum(cyclo.tab, nrow(swdefects.tab))

#essential complexity v defect
evg.tab <- with(swdefects.tab, table(predict.evg.14.5, defect))

twowaysum(evg.tab, nrow(swdefects.tab))

#design complexity v defect
ivg.tab <- with(swdefects.tab, table(predict.ivg.9.2, defect))

twowaysum(ivg.tab, nrow(swdefects.tab))

#Lines of Code through functions

#function to calculate accuracy, detection rate,
#false alarm rate, precision
prob_measure=function(summary.tab)
{
  a=summary.tab[1,1]
  b=summary.tab[1,2]
  c=summary.tab[2,1]
  d=summary.tab[2,2]

  accuracy=acc(a, b, c, d)
  detection=detect(b, d)
  false_alarm=falarm(a, c)
  precision=prec(c, d)

  as.list(c(accuracy, detection, false_alarm, precision))
}

loc_measures <- prob_measure(loc.tab)
cyclo_measures <-prob_measure(cyclo.tab)
ivg_measures <- prob_measure(ivg.tab)
evg_measures <- prob_measure(evg.tab)

#table to store all values
measures <- matrix(c("Method", "Accuracy", "Detection Rate"
                     ,"False Alarm Rate", "Precision"
  , "Lines of Code",loc_measures, "Cyclomatic Complexity", cyclo_measures
  , "Essential Complexity", ivg_measures
  , "Design Complexity", evg_measures)
                   ,nrow=5, ncol=5, byrow=TRUE)
measures
