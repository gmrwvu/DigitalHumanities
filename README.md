# DigitalHumanities
This application of LDA works on a corpus to compare the topical distribution of each document in a corpus with that of the other documents.
A posterior analysis is done on the target against the topic model formed from corpora sets in the original corpus.
Statistics are calculated on the topical distribution of the target.
Once all documents have had the statistics calculated they are compared.
In this prototype of the system, LDA is employed as a classifier to determine topical similarity among documents in a corpus.
The application uses R and needs the following libraries:<br/>
install.packages("ggraph")<br/>
install.packages("gutenbergr")<br/>
install.packages("rlist")<br/>
install.packages("tidytext")<br/>
install.packages("tidyr")<br/>
install.packages("tm")<br/>
install.packages("dplyr")<br/>
install.packages("scales")<br/>
install.packages("topicmodels")<br/>
install.packages("stringr")<br/>
