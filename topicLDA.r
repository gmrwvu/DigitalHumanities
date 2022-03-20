library(ggraph)
library(gutenbergr)
library(rlist)
library(tidytext)
library(tidyr)
library(tm)
library(dplyr)
library(scales)
library(topicmodels)
library(stringr)

#==================================
# FUNCTIONS
#==================================
#1.)SOURCE IDENTIFIERS
#============================================================
#the books and their source id are put in char vector here
#============================================================
G_ids<-function() {
   Num_Books<-c(8347:8373)
   Num_Books_loop<-c(8349:8373)
   book_names<-c("mt","mr","lk","jn","act","rm","fcr","scr", "ga", "eph", "pp", "co", "fth", "sth","ftm","stm","ti","ph","hb","jam","fpt","spt", "fjn","sjn","tjn","jud","rev")  

   book_map<- book_names
   names(book_map)<- Num_Books

   book_mapN<- Num_Books
   names(book_mapN)<- book_names

   retList<-list("Num_Books"= Num_Books, "Num_Books_loop" = Num_Books_loop, "book_map" = book_map, "book_mapN" = book_mapN)
   return(retList)
}

#2.) READ SOURCE
#=============================================================
#Read each NT book from Gutenberg project - get rid of commentary
#aleph goes down form time to time
#if so use the mirror
#mirror = "http://mirrors.xmission.com/gutenberg/"
#https://github.com/ropensci/gutenbergr/issues/8
#=============================================================
read_source<-function(source) {
  print(source)
  i=8347
  #gmr1<-as.data.frame(list(gutenberg_download(c(i))))
  gmr1<-as.data.frame(list(gutenberg_download(c(i),mirror = "http://mirrors.xmission.com/gutenberg/")))

  i=8348
  #gmr2<-as.data.frame(list(gutenberg_download(c(i))))
  gmr2<-as.data.frame(list(gutenberg_download(c(i),mirror = "http://mirrors.xmission.com/gutenberg/")))

  x1<-commentary[1]
  x2<-commentary[2]
  x<-2
  ntlist<-list(gmr1[-(1:x1),],gmr2[-(1:x2),])
  for (j in G_ids()$Num_Books_loop) {
    x<-x+1
    #gmrX<-as.data.frame(list(gutenberg_download(c(j))))
    gmrX<-as.data.frame(list(gutenberg_download(c(j),mirror = "http://mirrors.xmission.com/gutenberg/")))
    print(j)
    print(gmrX)
    ntlist<-list.append(ntlist,gmrX[-(1:x),])
  }
  return(ntlist)
}

#3.) CONVERT SOURCE TEXT TO TIDY
#=============================================================
#convert list of books just read into a structure used in program
#=============================================================
convert_tidy<-function(nt_books) {
  #group df by gutenberg-id and add chpater, linenumber
  tidy_bks <- nt_books %>%
    group_by(gutenberg_id) %>%
    mutate(
      linenumber = row_number(),
      chapter = cumsum(str_detect(text, 
                                regex("^.*chapter [\\divxlc]", 
                                      ignore_case = TRUE))))  
  #after ch and line, get ungroups version of that
  tidy_books<- tidy_bks %>% ungroup()

  return(tidy_books)
}

#4.) GET_PROPORTIONS
get_proportions<-function(target_book, K, top_topics, tidy_books) {
   #fit model
   gmr_lda<-get_lda(K, tidy_books, target_book)

   #get book-topic-porportion set from return but only those with a proportion of content > 10%
   #topics df is for corpus
   topics_df<-data.frame()
   sort_gamma<-arrange(gmr_lda$books_gamma, topic, desc(gamma))
   for(i in 1:length(sort_gamma$gamma)) {
    if(sort_gamma[i,]$gamma > .1){
       topics_df<-rbind(topics_df, unname(sort_gamma[i,]))
    }
   } 
   #appropriately name the columns in topics_df of corpus
   names(topics_df)<-c("document","topic","gamma")

   #split the 26 book in this iteration of the corpus into Paul, Not Paul
   paul_split<-split_paul()

   #get names of nt book associated with gutenberg_id
   pauline_books<-unname(G_ids()$book_map[as.character(paul_split$pauline)])

   #now work on posterior results, ie target distribution here 
   post_topics<-gmr_lda$post$topics
   gmr1<-names(post_topics[1,]) #get names
   gmr2<-unname(post_topics[1,]) #get values
   post_df<-cbind(gmr1,gmr2)
   post_df<-as.data.frame(post_df)
  
   #sort the target distrinbution in desc order so top rows have highest content proportion
   names(post_df)<-c("Topic","Gamma")
   sort_post<-arrange(post_df,desc(as.numeric(Gamma)))

   #only use the top x number of topics - those with heaviest weight, x held in constant top_topics
   target_topics<-head(sort_post,n=top_topics)

   #Find topics in target dist that also generated paul
   return(inspect_target(target_topics, topics_df, target, pauline_books))
}

#5.) GET_LDA
get_lda<-function(gmrK, fn_bks, target_fn) {
   #get corpus as dtm
   nt_dtm<-tidy_up(fn_bks)

   #create topic model
   nt_lda <- LDA(nt_dtm, k = gmrK, control = list(seed = 1234))
   gmrPerplex<-perplexity(nt_lda)


   #This is for later research on what the 42 topics mean 
   nt_topics <- tidy(nt_lda, matrix = "beta")
   top_terms <- nt_topics %>%
     group_by(topic) %>%
     slice_max(beta, n = 30) %>% #magic cookie
     ungroup() %>%
     arrange(topic, -beta)

   if(target_fn == "NA"){
      return(top_terms)
   }

   #Back to author study: which topics are associated with each document.
   books_gamma <- tidy(nt_lda, matrix = "gamma")

   #next major step: get target as dtm
   tar_dtm<-tidy_up(target_fn)
   
   post_probs <- topicmodels::posterior(nt_lda, tar_dtm)

   retvar<-list("perplexity" = gmrPerplex, "topic_terms" = top_terms, "books_gamma" = books_gamma, "post"=post_probs)
   return(retvar)
}

#6.) TIDY_UP
#row by row format
tidy_up<-function(fn_bks){
   gmr_word <- fn_bks %>%
     unnest_tokens(word, text)

   gmr_word_counts <- gmr_word %>%
     anti_join(stop_words) %>%
     dplyr::count(document, word, sort = TRUE) %>%
     ungroup()

   gmr_dtm <- gmr_word_counts %>%
     cast_dtm(document, word, n)

   return(gmr_dtm)
}

#==================================
# END OF FUNCTIONS
#==================================

#==================================
#  MAIN 
#==================================
#==================================
#make Paul corpus and other corpus
#==================================
#the location of Paul's book in the list of NT books
paul_numbers<-c(8352, 8354, 8355, 8356, 8357, 8358, 8359, 8360, 8361, 8362, 8363, 8364)
paul_ntlist_idx<-c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)

#not Paul except Hebrews.
other_numbers<-c(8347, 8348, 8349, 8350, 8351, 8366, 8367, 8368, 8369, 8370, 8371, 8372, 8373)
other_ntlist_idx<-c(1, 2, 3, 4, 5, 20, 21, 22, 23, 24, 25, 26, 27)

NTLEN<- length(G_ids()$Num_Books) 
n <- NTLEN
commentary<-rep(46,27)
#NUMBER OF TOPICS	
K<-50
top_topics<- 7
ntlist<-read_source("gutenberg")



#collect all the paul books into a dataframe so can renumber guten_id 
paul_ntlist_idx<-c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
Paul_Corpus<-rbind(ntlist[[6]], ntlist[[7]])
for (j in paul_ntlist_idx) {
   print(c(j, length(ntlist[[j]]$text)))
   Paul_Corpus<- rbind(Paul_Corpus, ntlist[[j]])
}
#use the print from for to validate the end dataframe as having correct length
#need to make sure the follwoing is all 8352
Paul_Corpus$gutenberg_id

#collect all the non-paul books into a dataframe so can renumber guten_id 
other_ntlist_idx<-c(3, 4, 5, 20, 21, 22, 23, 24, 25, 26, 27)
Other_Corpus<-rbind(ntlist[[1]], ntlist[[2]])
for (j in other_ntlist_idx) {
   print(c(j, length(ntlist[[j]]$text)))
   Other_Corpus<- rbind(Other_Corpus, ntlist[[j]])
}

#NEED TO USE PAUL$GUTEN AS ID
size<-length(Paul_Corpus$gutenberg_id)
for (k in 1:size){
   Paul_Corpus$gutenberg_id[k]<-8352
   Paul_Corpus$document[k]<-"paul"
}

#NEED TO USE OTHER$GUTEN AS ID
size<-length(Other_Corpus$gutenberg_id)
for (k in 1:size){
   Other_Corpus$gutenberg_id[k]<-8347
   Other_Corpus$document[k]<-"other"
}

#Combine into an NT_Corpus
NT_Corpus<-rbind(Paul_Corpus, Other_Corpus)

#change to tidy format - linenumber chapter added
tidy_books<-convert_tidy(NT_Corpus)
#hebrews is book 19 in NT 4365 in Guten
#target <- 8365


#get corpus as dtm
nt_dtm<-tidy_up(tidy_books)


#get Perplex - k needs to be integre of atleast 2 by direction of LDA function
for (K in 2:50){ 
  nt_lda <- LDA(nt_dtm, k = K, control = list(seed = 1234))
  gmrPerplex<-perplexity(nt_lda)
  print(c(K, gmrPerplex))
}


K <-3

#create topic model
nt_lda <- LDA(nt_dtm, k = K, control = list(seed = 1234))
gmrPerplex<-perplexity(nt_lda)

books_gamma <- tidy(nt_lda, matrix = "gamma")

#get the array of paulene proportions for each of the topics
paulProbs<-books_gamma$gamma[books_gamma$document == "paul"]

#now loop through NT books and write out each posterior result:
book_names<-c("mt","mr","lk","jn","act","rm","fcr","scr", "ga", "eph", "pp", "co", "fth", "sth","ftm","stm","ti","ph","hb","jam","fpt","spt", "fjn","sjn","tjn","jud","rev")
for (j in 1:27){

   target<- ntlist[[j]]
   size<-length(target$gutenberg_id)
   for (k in 1:size){
      target$document[k]<-book_names[j]
   }
   tidy_target<-convert_tidy(target)

   #next major step: get target as dtm
   tar_dtm<-tidy_up(tidy_target)

   #thern get posterior 
   post_probs <- topicmodels::posterior(nt_lda, tar_dtm)

   #this is a check to be sure the topics distrubtion sums to 1 or 100%
   sumPost <- 0
   for (k in 1:K){
      sumPost <- sumPost + post_probs$topics[[k]]
   }

   #get the weighted average of paulene topics for a book
   #this is the equivalent of a sumproduct of top[ics in the document times the paulene proportiopn in that dpocument
   sumPaul<-0
   for (k in 1:K){
      sumPaul <- sumPaul + (paulProbs[[k]] * post_probs$topics[[k]])
   }

   output<- c(target$document[[1]], sumPaul)
   print(output)

}
