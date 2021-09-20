# DigitalHumanities
rotationalLDA works on a corpus to compare the topical distribution of each document in that corpus against all the other documents.
Each document becomes the target document while the others are formed into a topic model using LDA.
A posterior analysis is done on the target against the topic model formed from the others.
Statistics are calculated on the topical distribution of the target.
Once all documents have had the statistics calculated they are compared.
In this prototype of the system, rotationalLDA is employed as a classifier to determine topical similarity among documents in a corpus.
