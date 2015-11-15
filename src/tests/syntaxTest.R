library(RUnit)
setwd("..")
source("syntaxChecker.R")


#############################
"
Programmer: Shane Caldwell

Goal of Code: Testing Suite for Syntax Checking Functions.

Last Updated: 10/19/2015
"
############################

test.countCharOccurences <- function(){
  checkEquals(countCharOccurrences("s", "selfless"), 3)
  checkEquals(countCharOccurrences(".", "..."), 3)
  preDefinedString <- "....+ajridsklwl ++=.djzzz"
  checkEquals(countCharOccurrences("q", preDefinedString), 0)
}

#testsuite.countCharOcurrences <- defineTestSuite("countCharOcurrences,
#                                                 ")
