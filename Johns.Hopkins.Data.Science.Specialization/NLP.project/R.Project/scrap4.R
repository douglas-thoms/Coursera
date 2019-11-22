library(data.table)

testDT <- as.data.table(unigram)

x <- testDT[1:100,]

#test mutating regex

orig.sentence <- "I like Barry Manilow"

testDT[, regex := paste("^_",name,"$",sep="")] 

test.bigram.DT <- as.data.table(bigram)

y <- test.bigram.DT[name %like% ".*_shirt$"]