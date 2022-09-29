# Install if not already installed
# installed.packages("stringr", dependencies = TRUE)
# install.packages("tokenizers")
# install.packages("dplyr")
library(stringr)
totlibrary(tokenizers)
library(dplyr)

# Read text. Retrieved from http://cy.wikisource.org/wiki/Pwyll_Pendeuic_Dyuet
# Text included here corrects a punctuation mark 'adwaenwn' instead of 'adwa.enwn' 
# To apply to any other text change the file name cyntaf2.txt in the line below
# E.g. text <- readChar("ail.txt", file.info("ail.txt")$size)
text <- readChar("cyntaf2.txt", file.info("cyntaf2.txt")$size)



# Clean text
text2<- stringr::str_replace_all(text,"<[^>]+>", " ")
text2 <- stringr::str_replace_all(text2,"[\\s+]", " ")

# Split text into sentences
sentencelist<-str_split(text2, "\\.")
sentences<-sentencelist[[1]]

# Tokenize
text_tokens<- stringr::str_replace_all(text2,"[^a-zA-Z0-9\\s]", " ")
tokens_list<- tokenize_words(text_tokens)
tokens<- tokens_list[[1]]    # Tokens list
num_tokens<-length(tokens)   # Total number of tokens

# Word types
word_types1<-table(tokens)
word_types<- as.data.frame(table(tokens))
word_types<-rename(word_types, word_form=tokens, freq=Freq)

# Zipf's distribution
orderfreq<-arrange(word_types, desc(freq))
plot(orderfreq$freq)

#Sentece arrangement
mediumval<-array() 
mediumval_sq<-array() 
mediumval_nusq<-array() 
mediumval_hnu<-array() 
mediumval_sq_hnu<-array() 
  
#Solve sentences #
sentence_tokens<- stringr::str_replace_all(sentences,"[^a-zA-Z0-9\\s]", " ")

for(i in 1:length(sentences)){  
tokens_sentence<- tokenize_words(sentence_tokens[i])

tokens_in_sentence<- tokens_sentence[[1]]

t<-length(tokens_in_sentence) # number of tokens in sentence

freqs_sentence<-word_types1[tokens_in_sentence]   # absolute freq for each token in sentence

relative<-freqs_sentence/num_tokens   # relative freqs for each token in sentence

valueunits<-sum(relative)

mediumval[i]<-(valueunits)/t

mediumval_sq[i]<-(valueunits^2)/(t)  # not exact, though close

mediumval_nusq[i]<-(valueunits)/(t^2) # not exact, though close

mediumval_hnu[i]<-(valueunits)/(t^6)  # not exact, though close

mediumval_sq_hnu[i]<-(valueunits^2)/(t^6)  # not exact, though close
}

results<-data.frame(sentence=sentences, mediumval=mediumval, mediumval_sq=mediumval_sq, mediumval_nusq=mediumval_nusq, mediumval_hnu=mediumval_hnu, mediumval_sq_hnu=mediumval_sq_hnu)
results<-na.omit(results) 

#Sort sentences by preferences
wf_more<-arrange(results,desc(mediumval))
wf_highest<-arrange(results,desc(mediumval_sq))
sent_more<-arrange(results,desc(mediumval_nusq))
sent_highest<-arrange(results,desc(mediumval_hnu)) 
sent_wf<-arrange(results,desc(mediumval_sq_hnu))

print("Word frequency in context highest relevancy")
print (wf_highest[1:5,1])
print("Word frequency more relevant that sentence complexity")
print (wf_more[1:5,1])
print("Sentence complexity more relevant, word frequency highly relevant")
print (sent_more[1:5,1])
print("Sentence complexity more relevant")
print (sent_wf[1:5,1])
print("Sentence complexity highest relevancy")
print (sent_highest[1:5,1])

# Type tokens to see the full list of tokens
# Type orderfreq to see a list ot word types and their frequencies
# Type results$sentence to see the list of sentences ordered as in text
# Type sent_highest$sentence to see a list of sentences ordered by sentence lenght as most relevant criterion

