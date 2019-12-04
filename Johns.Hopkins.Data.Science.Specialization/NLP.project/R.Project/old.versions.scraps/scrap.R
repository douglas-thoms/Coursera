#setting up kneser ney model

#https://rpubs.com/pferriere/dscapreport

#The unigram Kneser-Ney probability is the number of unique words the unigram 
#follows divided by all bigrams. The Kneser-Ney unigram probability can be
#extended to k-grams, where 1 <= k < 

#formula

#Porbability formula

#recurse

#d if more 3 or greater 0.75, if 1 0.446, if 2 1.26

d5 = if count >= 3, 0.75, count == 2 then 1.26, count ==1 then 0.446

prob.unigram = proceeding.names.type.unigram/name.type.bigram.all

backoff.weight.quad = d5(proceeding.names.quadgram/preceeding.names.quadgram)

Prob.penta = max(frequency(pentagram - d5)| 0) + backoff.weight.quad*prob.quad

#so to start