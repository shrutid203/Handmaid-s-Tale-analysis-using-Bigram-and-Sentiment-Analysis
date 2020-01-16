#Download PDF Tools to read pdf data 
install.packages("pdftools")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("textreadr")
install.packages("textdata")
install.packages("SnowballC")
install.packages ("RColorBrewer")
install.packages("SemNetCleaner")
install.packages("textstem")
install.packages("topicmodels")
install.packages("plotly")



setwd("C:/Users/shruti.diwakar/Desktop")
library(pdftools)
library(tidyverse)
library(stringr)
library(tidytext)
library(textreadr)
library(textdata)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(textstem)
library(topicmodels)
library(plotly)
library(widyr)
library(syuzhet)
library(tm)


data("stop_words")


hmaid <- read_pdf("The Handmaid's Tale_analysis.pdf")
head(hmaid) 


#Label Chapter numbers by reading the word "chapter" 
j=0
for (i in 1:nrow(hmaid)){
 if(length(grep('CHAPTER',hmaid$text[i]))==1){
     j=j+1}
  hmaid$chapter_num[i]=j
} 
clean_hmaid<- hmaid[!grepl("CHAPTER",hmaid$text),] %>% unnest_tokens(word,text)


# Cleaning the pdf
clean_hmaid$word <-str_replace(clean_hmaid$word,"'","'")
clean_hmaid$word <-str_replace(clean_hmaid$word,"'s","")
clean_hmaid$word <-str_replace(clean_hmaid$word,"'d","")
clean_hmaid$word<-gsub('[0-9]+', '', clean_hmaid$word)
#Lemmatizing the words
clean_hmaid$word<-lemmatize_words(clean_hmaid$word)


#Remove stop words
hmaid_stopwords<- clean_hmaid%>% anti_join(stop_words)
head(hmaid_stopwords)


# Now we will try to create word count
hmaid_wc<-hmaid_stopwords %>% count(word, sort = TRUE)
hmaid_wc<-data.frame(hmaid_wc,stringsAsFactors = F)
hmaid_wc$word<-factor(hmaid_wc$word,levels=unique(hmaid_wc$word)[order(hmaid_wc$n)])

# plot the 50 most common words
 plot_ly(x=hmaid_wc$n[1:20],y=hmaid_wc$word[1:20],type='bar',orientation='h', color="red",plot_bgcolor = "rgb(182, 215, 168)")

#####################Topic modellin using LDA###################
# 
# #converting into document term matrix
# hmaid_dtm<- hmaid_stopwords %>% count(chapter_num,word) %>% cast_dtm(document = chapter_num,term = word,value = n,weighting = tm::weightTf)
# 
# #We will classify the words into four topics
# hmaid_lda<-LDA(hmaid_dtm,k=4,method = 'Gibbs',control = list(seed=0))
# hmaid_lda
# 
# hmaid_betas<-tidy(hmaid_lda,matrix="beta")
# hmaid_betas
# 
# #Top words for Topic 1 and representation
# hmaid_topic_1<- hmaid_betas %>% filter(topic==1) %>% group_by(topic) %>% arrange(topic,-beta)
# hmaid_topic_1 %>% with(wordcloud(term,beta,random.order = FALSE,max.words = 20,colors = pal))
# 
# #Top words for Topic 2 and representation
# hmaid_topic_2<- hmaid_betas %>% filter(topic==2) %>% group_by(topic) %>% arrange(topic,-beta)
# hmaid_topic_2 %>% with(wordcloud(term,beta,random.order = FALSE,max.words = 20,colors = pal))
# 
# #Top words for Topic 3 and representation
# hmaid_topic_3<- hmaid_betas %>% filter(topic==3) %>% group_by(topic) %>% arrange(topic,-beta)
# hmaid_topic_3 %>% with(wordcloud(term,beta,random.order = FALSE,max.words = 20,colors = pal))
# 
# 
# #Top words for Topic 4 and representation
# hmaid_topic_4<- hmaid_betas %>% filter(topic==4) %>% group_by(topic) %>% arrange(topic,-beta)
# hmaid_topic_4 %>% with(wordcloud(term,beta,random.order = FALSE,max.words = 20,colors = pal))
# 
# #Trying to associate Topics with chapters
# hmaid_chapters<- tidy(hmaid_lda,matrix="gamma")
# hmaid_chapters %>% filter(document==8)

###################### Bigrams Analysis #########################
 hmaid_bigram<- hmaid[!grepl("CHAPTER",hmaid$text),] %>% unnest_tokens(word,text,token="ngrams",n=2)
 
 # Some NAs were generated. Removing them
 hmaid_bigram<-na.omit(hmaid_bigram)
 
 # Clean the pdf reading by replacing ' with '
 hmaid_bigram$word <-str_replace(hmaid_bigram$word,"'","'")
 hmaid_bigram$word <-str_replace(hmaid_bigram$word,"'s","")
 
 
 # Separate bigrams and remove stop words
 hmaid_bigrams_sep <- hmaid_bigram %>%
   separate(word, c("word1", "word2"), sep = " ") %>% filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word)
 
 
 #Lemmatizing the words
 hmaid_bigrams_sep$word1<-lemmatize_words(hmaid_bigrams_sep$word1)
 hmaid_bigrams_sep$word2<-lemmatize_words(hmaid_bigrams_sep$word2)
 
 
 bigram_graph_woman<-hmaid_stopwords %>% filter(word1=="woman"|word2=="woman")%>% filter(n>1) %>% graph_from_data_frame()
 
 set.seed(2019)
 
 a<-grid::arrow(type="closed",length=unit(.15,"inches"))
 ggraph(bigram_graph_woman, layout = "fr") +
   geom_edge_link(aes(edge_alpha=n),show.legend = F,arrow=a,end_cap=circle(.05,'inches')) +
   geom_node_point(color="light blue",size=5) +
   geom_node_text(aes(label = name), vjust = 1, hjust = 1)+theme_void()
 
 bigram_graph_time<-hmaid_stopwords %>% filter(word1=="feel"|word2=="feel")%>% graph_from_data_frame()
 
 set.seed(2019)
 
 a<-grid::arrow(type="closed",length=unit(.15,"inches"))
 ggraph(bigram_graph_time, layout = "fr") +
   geom_edge_link(aes(edge_alpha=n),show.legend = F,arrow=a,end_cap=circle(.07,'inches')) +
   geom_node_point(color="green",size=5) +
   geom_node_text(aes(label = name), vjust = 1, hjust = 1)+theme_void()
 
 
 bigram_graph<-hmaid_stopwords %>% filter(n>4) %>% graph_from_data_frame()
 
 set.seed(2019)
 
 a<-grid::arrow(type="closed",length=unit(.15,"inches"))
 ggraph(bigram_graph, layout = "fr") +
   geom_edge_link(aes(edge_alpha=n),show.legend = F,arrow=a,end_cap=circle(.07,'inches')) +
   geom_node_point(color="orange",size=5) +
   geom_node_text(aes(label = name), vjust = 1, hjust = 1)+theme_void()

 
#  ##################correlation of words ###########################
#  
#  
#  # count words co-occuring within sections
  word_pairs <- hmaid_stopwords %>% group_by(word) %>% filter(n()>=20) %>% pairwise_cor(word,page_id,sort=TRUE)
 head(word_pairs) 
 word_pairs %>%
   filter(item1 %in% c("commander","lydia","gilead","woman")) %>%
   group_by(item1) %>%
   top_n(6) %>%
   ungroup() %>%
   mutate(item2 = reorder(item2, correlation)) %>%
   ggplot(aes(item2, correlation)) +
   geom_bar(stat = "identity") +
   facet_wrap(~ item1, scales = "free") +
   coord_flip()
 
# set.seed(2016)
# 
# word_pairs %>%
#   filter(correlation > .3) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
#   geom_node_point(color = "lightblue", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_void()



############Emotion analysis weighted by tfidf ranking ######################################3
hmaid_wc_2<-hmaid_stopwords %>% group_by(word,chapter_num) %>% summarise(n=n())
hmaid_wc_2<-hmaid_wc_2[!is.numeric(hmaid_wc_2$word),]
hmaid_stopwords_2 <- hmaid_wc_2 %>% bind_tf_idf(word,chapter_num, n)
emotions<-get_nrc_sentiment(hmaid_stopwords_2$word)

#emotions<-get_nrc_sentiment(hmaid_stopwords$word)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
emp_sum<-emo_sum[!(emo_sum$emotion %in% c("positive","negative")),]
plot_ly(emp_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion") 

#hmaid_emoanalysis<-cbind(hmaid_stopwords,emotions)
hmaid_stopwords_2<-data.frame(hmaid_stopwords_2)
hmaid_emoanalysis<-cbind(hmaid_stopwords_2,emotions)
hmaid_emoanalysis$anger<-hmaid_emoanalysis$anger*hmaid_emoanalysis$tf_idf
hmaid_emoanalysis$anticipation<-hmaid_emoanalysis$anger*hmaid_emoanalysis$tf_idf
hmaid_emoanalysis$disgust<-hmaid_emoanalysis$disgust*hmaid_emoanalysis$tf_idf
hmaid_emoanalysis$fear<-hmaid_emoanalysis$fear*hmaid_emoanalysis$tf_idf
hmaid_emoanalysis$joy<-hmaid_emoanalysis$joy*hmaid_emoanalysis$tf_idf
hmaid_emoanalysis$sadness<-hmaid_emoanalysis$sadness*hmaid_emoanalysis$tf_idf
hmaid_emoanalysis$surprise<-hmaid_emoanalysis$surprise*hmaid_emoanalysis$tf_idf
hmaid_emoanalysis$trust<-hmaid_emoanalysis$trust*hmaid_emoanalysis$tf_idf

emo_bar=colSums(hmaid_emoanalysis[,c(7:14)])
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
emp_sum<-emo_sum[!(emo_sum$emotion %in% c("positive","negative")),]
plot_ly(emp_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
 layout(xaxis=list(title=""), showlegend=FALSE,
        title="Distribution of emotion") 

hmaid_chapter<-hmaid_emoanalysis %>% group_by(chapter_num) %>% summarise(trust=sum(trust),anticipation=sum(anticipation),joy=sum(joy),sadness=sum(fear),anger=sum(anger),surprise=sum(surprise),disgust=sum(disgust))
hmaid_chapter$total<-rowSums(hmaid_chapter[,c(2:8)])
hmaid_chapter$trust<-hmaid_chapter$trust/hmaid_chapter$total
hmaid_chapter$anticipation<-hmaid_chapter$anticipation/hmaid_chapter$total
hmaid_chapter$joy<-hmaid_chapter$joy/hmaid_chapter$total
hmaid_chapter$sadness<-hmaid_chapter$sadness/hmaid_chapter$total
hmaid_chapter$anger<-hmaid_chapter$anger/hmaid_chapter$total
hmaid_chapter$surprise<-hmaid_chapter$surprise/hmaid_chapter$total
hmaid_chapter$disgust<-hmaid_chapter$disgust/hmaid_chapter$total

plot_ly(hmaid_chapter, x = ~chapter_num, y = ~trust, name = 'trust', type = 'bar') %>%
  add_trace(y = ~anticipation, name = 'anticipation') %>%
  add_trace(y = ~joy, name = 'joy') %>%
add_trace(y = ~sadness, name = 'sadness')%>%
add_trace(y = ~anger, name = 'anger')%>%
add_trace(y = ~surprise, name = 'surprise')%>%
add_trace(y = ~disgust, name = 'disgust')%>%
layout(yaxis = list(title = ''), barmode = 'stack')


## Finding out the most common words for each 

hmaid_trust=hmaid_emoanalysis%>% filter(trust>0)%>%group_by(word)%>%summarise(count=n())

## Lets try to find if aunt is used in a good connotation

chapter=clean_hmaid%>% filter(word=="commander")
chapter=unique(clean_hmaid[,c("chapter_num","element_id")])
aunt_chapter= merge(hmaid_emoanalysis,chapter)

#Removing "aunt" lets see what the reaction is
aunt_chapter=aunt_chapter[!(aunt_chapter$word %in% "commander"),]
aunt_chapter_emo=colSums(aunt_chapter[,c(7:14)])
aunt_chapter_emosum = data.frame(count=aunt_chapter_emo, emotion=names(aunt_chapter_emo))
aunt_chapter_emosum$emotion = factor(aunt_chapter_emosum$emotion, levels=aunt_chapter_emosum$emotion[order(aunt_chapter_emosum$count, decreasing = TRUE)])

plot_ly(aunt_chapter_emosum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion") 

b=aunt_chapter %>% filter(trust>0)%>%group_by(word)%>%summarise(count=n())
b=aunt_chapter %>% filter(anticipation>0) %>%group_by(word)%>%summarise(count=n())



################
chapter_analysis=hmaid_emoanalysis[hmaid_emoanalysis$chapter_num==15,]
b=hmaid_emoanalysis %>% filter(trust>0)%>%filter(chapter_num==15)%>% group_by(word)%>%summarise(count=n())


