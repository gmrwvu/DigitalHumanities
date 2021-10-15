# DigitalHumanities
This application of LDA works on a corpus to compare the topical distribution of each document in that corpus against all the other documents.
Each document becomes the target document while the others are formed into a topic model using LDA.
A posterior analysis is done on the target against the topic model formed from the others.
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
