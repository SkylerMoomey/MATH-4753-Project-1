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

totals.tab <- table(predictions, swdefects.tab$defect)
colnames(totals.tab) <- c("No Defect", "Defect")
totals.tab

#tables for each predictive factor

#loc v defect

loc.tab <- with(swdefects.tab, table(predict.loc.50, defect))

mat = matrix(c(loc.tab[1, 1], loc.tab[1, 2], I(loc.tab[1,1] + loc.tab[1,2])
        ,loc.tab[2, 1], loc.tab[2,2], I(loc.tab[2,1] + loc.tab[2, 2])
        , I(loc.tab[1,1] + loc.tab[2,1]), I(loc.tab[1,2] + loc.tab[2,2]), nrow(swdefects.tab))
        , ncol=3,byrow=TRUE)
colnames(mat) = c("No Defect", "Defect", "Row Totals")
rownames(mat) = c("Not Predicted", "Predicted", "Column Totals")
mat

addmargins(loc.tab)

#cyclomatic complexity v defect
cyclo.tab <- with(swdefects.tab, table(predict.vg.10, defect))
cyclo.tab

#essential complexity v defect
evg.tab <- with(swdefects.tab, table(predict.evg.14.5, defect))
evg.tab

#design complexity v defect
ivg.tab <- with(swdefects.tab, table(predict.ivg.9.2, defect))
ivg.tab

