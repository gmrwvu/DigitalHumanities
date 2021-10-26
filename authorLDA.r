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

#===============================================
#Objects created:
#     1.) ntlist is a listy of the 27 books, each element the text of the book
#     2.) nt_books is a df with 2 columns guten_id and text and the 20552 rows of nt
#     3.) tidy_bks is df grouped by guten-id with chap and line#
#     4.) tidy_books is an ungrouped tidy_bks
#     5a.) num_books is a vector of the gutenberg_id of the NT books
#     5b.) num_books_loop does not have 8347 and 8349 that are used to start corpus union
#     6.) book_names is a vector of abbreviations for the NT books
#     7.) book_map is a named vector that has gutenberg_id as index and book abbreviation as value
#     8.) book_mapN has book abbrv as index and gutn_id as value
#     9.) topic_df holds the book-topic-proportion rows where propotion > .1 (magic cookie)
#===============================================

#===============================================
#Functions section
#===============================================
#1.) APPEND_BOOKS
#utility to put NT books into a list
append_books<-function(gmr1, gmr2){
     return(union(gmr1, gmr2))
}

#2.) TIDY_UP
#row by row format
tidy_up<-function(fn_bks){
   gmr_word <- fn_bks %>%
     unnest_tokens(word, text)

   #remove verse numbering here
   tfl<-length(gmr_word$word)
   for (m in tfl:1) {
      if(substr(gmr_word$word[m],1,1) %in% c(1,2,3,4,5,6,7,8,9,0)){
       #if it is 1 then remove from hb.trifreq
       print(c(m,gmr_word$word[m]))
       print(gmr_word[m,])
       gmr_word<-gmr_word[-m,]

      }
   } 

   #this joins by word
   gmr_word_counts <- gmr_word %>%
     anti_join(stop_words) %>%
     dplyr::count(document, word, sort = TRUE) %>%
     ungroup()

   gmr_dtm <- gmr_word_counts %>%
     cast_dtm(document, word, n)

   return(gmr_dtm)
}

#3.) GET_LDA
get_lda<-function(gmrK, fn_bks, target_fn) {
   #get corpus as dtm
   nt_dtm<-tidy_up(fn_bks)

   #create topic model
   nt_lda <- LDA(nt_dtm, k = gmrK, control = list(seed = 1234))
   gmrPerplex<-perplexity(nt_lda)


   #This is for later research on what the 38 topics mean 
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

#4.) SPLIT_BOOKS
target_split<-function(target, tidy_books){
   #divide corpus into target and all others
   target_book<- subset(tidy_books,gutenberg_id==target)
   nt_bks<- subset(tidy_books,!gutenberg_id==target) 

   nt_bks$document<-" "
   target_book$document<- " "

   #assign title to books
   for(i in G_ids()$Num_Books) {
    if(i == target) {
       target_book$document[target_book$gutenberg_id == as.character(i)]<-unname(G_ids()$book_map[as.character(i)])
    } else {
       nt_bks$document[nt_bks$gutenberg_id == as.character(i)]<-unname(G_ids()$book_map[as.character(i)])
    }
   }
   gmrRet<-list("target_book" = target_book, "nt_bks" = nt_bks)
   return(gmrRet)
}

#5.) SPLIT_PAUL
split_paul<-function(){
   pauline<-vector()
   notpaul<-vector()
   pauline_idx<-c(6:18)
   other_idx<-c(1:5, 19:27)
   for(j in 1:length(G_ids()$Num_Books)) {
     if(G_ids()$Num_Books[j] == target) {
       print(target)
     } else {
       if(G_ids()$Num_Books[j] %in% G_ids()$Num_Books[pauline_idx]) {
         pauline<-append(pauline,G_ids()$Num_Books[j])
       } else {
         if(G_ids()$Num_Books[j] %in% G_ids()$Num_Books[other_idx]) {
           notpaul<-append(notpaul, G_ids()$Num_Books[j])
         }
       }
     } 
   }
   gmrRet<-list("pauline" = pauline, "notpaul" = notpaul)
}

#6.) INSPECT_TARGET dsitribution for ...
inspect_target<-function(target_topics, topics_df, target, pauline_books) {
   #show the books related to target
   #loop through target book distribution over topics
   ret_df<-data.frame()
   for(l in 1:length(target_topics$Topic)){
      #get the topic, gamma and book(s) 
      tt<-target_topics[l,]$Topic 
      tg<-percent(as.numeric(target_topics[l,]$Gamma))
      #what if more than one book per topic
      tb<-topics_df$document[topics_df$topic==tt]
      # if there are no books in corpus data frame for that topics - the target is the book
      if (length(tb)<1) {
         tb<-unname(G_ids()$book_map[as.character(target)])
      }
      else { # if intersect of tb with paul is 0, ie this topic did not generate a Pauline book
         if(length(tb)>1){
            x<-intersect(tb,pauline_books)
            if(length(x)> 0){ #here we have a topic that does generate a book
              tb<-x
            } else {
              tb<-tb[1]
         }
       }
      }
      #create return aas a vector
      t_ret<-c(tb,tt,tg)

      ret_df<-rbind(ret_df,t_ret)
   }

   return(ret_df)
}

#7.) GET_PROPORTIONS
get_proportions<-function(target, K, top_topics, tidy_books) {
   #Each nt book comes through as target and corpus is all other books
   tar_nto<-target_split(target, tidy_books)

   #fit model
   gmr_lda<-get_lda(K, tar_nto$nt_bks, tar_nto$target_book)

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

#8.)SOURCE IDENTIFIERS
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

#9.) READ SOURCE
#=============================================================
#Read each NT book from Gutenberg project - get rid of commentary
#aleph goes down once a week
#if so uncomment the mirror and comment default
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

#10.) CONVERT SOURCE TEXT TO TIDY
#=============================================================
#convert list of books just read into a structure used in program
#=============================================================
convert_tidy<-function(ntlist) {
  i = 1
  nt_books<-as.data.frame(ntlist[[i]])
  for (i in 2:NTLEN){
    gmrY<-as.data.frame(ntlist[[i]])
    nt_books<-append_books(nt_books, gmrY)
  }
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
#=================================================
#end functions
#=================================================

#=============================================================
#define constants
#=============================================================
NTLEN<- length(G_ids()$Num_Books) 
n <- NTLEN
commentary<-rep(46,27)
#NUMBER OF TOPICS	
K<-38
top_topics<- 5

#============================================================
#Main procesing loop for Hebrews Authorship
#============================================================
ntlist<-read_source("gutenberg")
tidy_books<-convert_tidy(ntlist)

paul_books<-c("rm","fcr","scr", "ga", "eph", "pp", "co", "fth", "sth","ftm","stm","ti","ph")
Process_Table<-data.frame()

for(m in 1:length(G_ids()$Num_Books)){
   target<-G_ids()$Num_Books[m]
   results<-get_proportions(target, K, top_topics, tidy_books)
   names(results)<-c("Book","Topic","Gamma")
   paul_cnt<-length(intersect(results$Book, paul_books))

   Paul_Topic_Percentage<-percent(paul_cnt/top_topics)
   Target_Book<-unname(G_ids()$book_map[as.character(target)])

   Process_Table <-rbind(Process_Table,c(Target_Book,Paul_Topic_Percentage))
}

names(Process_Table)<-c("Target Book", "Pecentage of Topics Pauline")
