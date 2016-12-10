## Warning! This file contains foul language and may be NSFW

## read data
library(RJSONIO)                ## parse JSON file
library(plyr)                   ## reshape dataframe
library(tm)                     ## removeSparseTerms
library(slam)
library(RColorBrewer)           ## change pal()
library(wordcloud)
# library(qdap)                   ## strip function 
library(ggplot2)

#AFINN <- read.csv("AFINN-111.txt", sep="\t", header=FALSE)
#
#unk_json <- fromJSON("unk_words_output_new1.txt", nullValue=NA)
#term <- names(unk_json)
#value <- unname(unk_json)
#unk <- data.frame(cbind(term, value))
#unk$value <- as.integer(unk$value)
        # write.table(unk, "unknown_df.csv")
#

## Load tweet list 
setwd("/Users/kairstenfay/Documents/dataScience/projects/cyberbullying")
tweets_json <- fromJSON("one_line_tweets_mentions_only.txt", nullValue=NA)
dat <- lapply(tweets_json, function(j) {
        as.data.frame(replace(j, sapply(j, is.list), NA))
})
df <- rbind.fill(dat)
df$text <- as.character(df$text)
df[,14] <- tolower(df[,14]) ## coerce all screen names to lowercase 
str(df)
Sys.setlocale('LC_ALL','C') 
write.csv(df, "tweet_frame.csv", row.names = FALSE, na = "NA")

## Split tweets into neutral/positive/negative ##
length(df[,1])
neu_df <- subset(df, sentiment==0); length(neu_df[,1])
pos_df <- subset(df, sentiment>0); length(pos_df[,1])
neg_df <- subset(df, sentiment<0); length(neg_df[,1])

## Concatenate all data frames into a vector for future for-looping 
frames_list <- list(df, neu_df, pos_df, neg_df)
names(frames_list) <- c("df", "neu_df", "pos_df", "neg_df")

## Create a custom `removeMostPunctuation` function for our purposes.
## Thanks MrFlick from stackoverflow.com for code
removeMostPunctuation <- 
        function (x) {
                x <- gsub("([@])|[[:punct:]]", "\\1", x)
                return(x)
        }


## Create a custom `term_doc_df` function that creates term document matrices and converts
## them to dataframes. 
## Code adapted from tutorial at https://www.r-bloggers.com/word-cloud-in-r/
term_doc_df <- function(x) {
        corpus <- Corpus(DataframeSource(data.frame(x[,3]))) 
        #corpus <- tm_map(corpus, content_transformer(tolower), lazy=T)
        corpus <- tm_map(corpus, content_transformer(function(x) removeWords(x, stopwords("english"))), lazy=T)
        corpus <- tm_map(corpus, content_transformer(removeMostPunctuation), lazy=T)
       # corpus <- tm_map(corpus, PlainTextDocument)
        tdm <- TermDocumentMatrix(corpus)
        tdm <- removeSparseTerms(tdm, .9999)
        m <- as.matrix(tdm)
        v <- sort(rowSums(m), decreasing=TRUE)
        d_pregrep <- data.frame(word=names(v), freq=v)
        d <- d_pregrep[!grepl(".*http.*|.*[<>].*|.*@{2,}.*|^like$|^just$|^amp$|^via$|
                              |@youtube|@c0nvey", d_pregrep$word), ] ## clean up 
}

##         corpus <- tm_map(corpus, PlainTextDocument)


## Create a custom `avg_sent` (average sentiment) function for our purposes. 
avg_sent <- function(x) {
        avg <- mean(x$sentiment)
        stdev <- sd(x$sentiment)
        paste("The average sentiment score was", avg, 
              "sentiment points per tweet with a standard deviation of", stdev, 
              sep=" ")
}


## Make term document matrices and convert them to dataframes
d_frames_list <- lapply(frames_list, term_doc_df)
names(d_frames_list) <- names(frames_list)
for (i in 1:length(d_frames_list)) {
        my_df <- data.frame(d_frames_list[i])
        print(head(my_df, 10))
}
write.csv(d_frames_list$df, "d_frame_df.csv", row.names=FALSE, na="NA")
write.csv(d_frames_list$neu_df, "d_frame_neu_df.csv", row.names=FALSE, na="NA")
write.csv(d_frames_list$pos_df, "d_frame_pos_df.csv", row.names=FALSE, na="NA")
write.csv(d_frames_list$neg, "d_frame_neg_df.csv", row.names=FALSE, na="NA")

## Assign colors for each wordcloud to be constructed 
df.pal <- neu_df.pal <- brewer.pal(9, "BuGn"); df.pal <- neu_df.pal <- df.pal[-(1:2)]
pos_df.pal <- brewer.pal(9, "GnBu"); pos_df.pal <- pos_df.pal[-(1:2)]
neg_df.pal <- brewer.pal(9, "OrRd"); neg_df.pal <- neg_df.pal[-(1:2)]
cols_list <- list(df.pal, neu_df.pal, pos_df.pal, neg_df.pal)

## Create wordclouds and save them locally
save_wordcloud <- function(x, y, z=NULL) {
        for(i in 1:length(x)) {
                png(paste(names(x[i]), "_wordcloud.png", sep=""), width=1280, height=800)
                set.seed(1)
                par(bg=z)
                wordcloud(x[[i]]$word, x[[i]]$freq, scale=c(8,.3), min.freq=2, max.words=100,
                          random.order=TRUE, rot.per=.15, colors=y[[i]], 
                          vfont=c("sans serif", "plain"))
                dev.off()
                }
        }
save_wordcloud(d_frames_list, cols_list)

## Calculate average sentiment score per tweet 
lapply(frames_list, avg_sent) 

t.test(neg_df$sentiment, df$sentiment, alternative="less")
## significantly different

t.test(pos_df$sentiment, df$sentiment, alternative="greater")




## Find the people mentioned the most in each dataframe
lapply(d_frames_list, function(x) {
        mentions <- x[grepl("^@", x$word),]
        my_df <- data.frame(mentions)
        print(head(my_df, 10))
})

## Who used a bigger vocabulary?
## Calculate average number of unique words per tweet 
## We remove any unigrams that begin with '@' or a number and unigrams that have a number 
## within them or a character that repeats 3 times or more 

remove_non_words <- function(x) {
        d.vocab <- x[!grepl('(\\w)\\1{2,}|.*@.*|.*http.*|.*[0-9].*', 
                            x$word, perl=TRUE),]
        d.vocab[,1] <- as.character(d.vocab[,1])
        d.vocab <- subset(d.vocab, nchar(d.vocab[[1]]) > 2)
        return(d.vocab)
}
avg_unique_words <- function(x,y) {
        for (i in 1:length(x)) {
                num_words <- length(rownames(x[[i]]))
                num_tweets <- length(rownames(y[[i]]))
                print(paste("The average number of unique words per tweet in", names(x[i]), 
                        "was", num_words/num_tweets, sep=" "))
        }
}
d_frames_list <- lapply(d_frames_list, remove_non_words)
avg_unique_words(d_frames_list, frames_list)
        ## across the board, 0.2 unique words/tweet
        ## neutral tweets had the highest rate at 2.5
        ## positive tweets had a lower rate at 0.3
        ## negative tweets had a rate of 2.0 

## Who was more verbose? 
## Calculate average number of words per tweet 
avg_num_words <- function(x, y) {
        for (i in 1:length(x)) {
                total_words <- sum(x[[i]]$freq)
                num_tweets <- length(rownames(y[[i]]))
                print(paste("The average number of words per tweet in", names(x[i]), 
                            "was", total_words/num_tweets, sep=" "))
        }
}

for (i in 1:length(d_frames_list)) {
        d_frames_list[[i]]$portion <- NULL
        d_frames_list[[i]]$portion <- 
                d_frames_list[[i]]$freq/length(frames_list[[i]][,1])
}



avg_num_words(d_frames_list, frames_list)

for (i in 1:length(d_top_rep_frames_list)) {
        d_top_rep_frames_list[[i]]$portion <- NULL
        d_top_rep_frames_list[[i]]$portion <- 
                d_top_rep_frames_list[[i]]$freq/
                length(top_rep_frames_list[[i]][,1])
}


## across the board, 5.3 words per tweet
## neutral tweets were the shortest at 4.9
## positive tweets were at 5.4
## negative tweets were the longest at 5.8

## Top tweet recipients
## Look at the top recipients of tweets using the 'in_reply_to_screen_name' variable (14)
## This differs from the previous example where we calculated mentions, because we are
## focusing specifically on tweets that begin with someone's screen name 

## Begin by subsetting each dataframe so that it contains a value in column 14
replies_frames_list <- lapply(frames_list, function(x) {
        with_reply_df <- subset(x, !is.na(x[,14]))
})
names(replies_frames_list) <- names(frames_list)
lens_of_rep_frames <- NULL
for (i in 1:length(replies_frames_list)) {
        lens_of_rep_frames[[i]] <- length(replies_frames_list[[i]][,1])
}
names(lens_of_rep_frames) <- names(frames_list)
lens <- data.frame(x=names(lens_of_rep_frames), y=lens_of_rep_frames)


qplot(x=lens$x, y=lens$y)

ggplot(data=lens, aes(x=lens$x, fill=)) + geom_bar(stat="identity", position="fill")

qplot(factor(x), data=lens, geom="bar", fill=factor(x))
####### work on this 
my_pie <- ggplot(lens, aes(x=factor(1), fill=factor(y))) + geom_bar(width=1)
my_pie + coord_polar(theta="y")









## What were the most common words in tweets with replies to specific people?
## Similar, i.e. boring
#d_rep_frames_list <- lapply(replies_frames_list, term_doc_df)
#names(d_rep_frames_list) <- names(frames_list)
#for (i in 1:length(d_rep_frames_list)) {
#        my_df <- data.frame(d_rep_frames_list[i])
#        print(head(my_df, 10))
#}
top_recipients <- lapply(replies_frames_list, function(x) {
        temp <- data.frame(table(x[,14]))
        temp <- head(arrange(temp, desc(Freq)), 10)
})
names(top_recipients) <- names(frames_list)
for (i in 1:length(top_recipients)) {
        print(top_recipients[i])
}
## Store top 10 names as a character vector 
top10rep_list <- lapply(top_recipients, function(x) {
        top10replies <- as.character(x[,1])
})

## Subset each dataframe in frames_list by the top recipients of tweets in each one
subset_by_top10reps <- function(x,y) {
        df_list <- NULL 
        for (i in 1:length(x)) {
                temp <- subset(x[[i]], x[[i]][,14] %in% y[[i]])
                df_list[[i]] <- temp
        } 
        return(df_list)
}
top10rep_frames_list <- subset_by_top10reps(replies_frames_list, top10rep_list)
names(top10rep_frames_list) <- names(frames_list)
for (i in 1:length(top10rep_frames_list)) {
        print(length(top10rep_frames_list[[i]][,1]))
}

## What words appear in these tweets aimed at the top 10 recipients for each dataframe?
d_top_rep_frames_list <- lapply(top_rep_frames_list, term_doc_df)
d_top_rep_frames_list <- lapply(d_top_rep_frames_list, function(x) {
        remove_mentions <- x[!grepl("^@|trump|hillary", x$word),]
})
names(d_top_rep_frames_list) <- names(frames_list)
lapply(d_top_rep_frames_list, function(x) {
        head(remove_mentions,10)
}) 


write.csv(d_top_rep_frames_list$df, "d_top_rep_df.csv", row.names=FALSE, na="NA")
write.csv(d_top_rep_frames_list$neu_df, "d_top_rep_neu_df.csv", row.names=FALSE, na="NA")
write.csv(d_top_rep_frames_list$pos_df, "d_top_rep_pos_df.csv", row.names=FALSE, na="NA")
write.csv(d_top_rep_frames_list$neg_df, "d_top_rep_neg_df.csv", row.names=FALSE, na="NA")


## wordcloud? ## remove hashtags?  
##
## 

## Calculate average sentiment of tweets sent at the top 10 recipients for each dataframe 
lapply(top10rep_frames_list, avg_sent)
## all together, more positive (7.3 -> 10.2)
## positive more positive (9.9 -> 13.2)
## negative more negative (-4.9 -> -7.1)

##########################################
## comparing left and right sentiment   ##
## realdonaldtrump vs. hillaryclinton   ##
## foxnews vs. cnn                      ##
##########################################

## Create new dataframes where the recipients tweets are either affiliated with the political
## left or right. 
left <- c("hillaryclinton", "cnn")
right <- c("realdonaldtrump", "foxnews")

## Subset the replies dataframes by tweets aimed at hillaryclinton or cnn
left_rep_frames_list <- lapply(replies_frames_list, function(x) {
        tweets_at_left <- subset(x, x[,14] %in% left)
}) 
names(left_rep_frames_list) <- c("left_aimed_df", "left_aimed_neu_df", 
                                 "left_aimed_pos_df", "left_aimed_neg_df")
for (i in 1:length(left_rep_frames_list)) {
        print(length(left_rep_frames_list[[i]][,1]))
} 
## Subset the replies dataframes by tweets aimed at realdonaldtrump or foxnews 
right_rep_frames_list <- lapply(replies_frames_list, function(x) {
        tweets_at_right <- subset(x, x[,14] %in% right)
}) 
names(right_rep_frames_list) <- c("right_aimed_df", "right_aimed_neu_df", 
                                 "right_aimed_pos_df", "right_aimed_neg_df")
for (i in 1:length(right_rep_frames_list)) {
        print(length(right_rep_frames_list[[i]][,1]))
}
len_of_frames_in_list <- function(x) {
        total_len <- NULL
        for (i in 1:length(x)) {
                l <- length(x[[i]][,1])
                total_len[[i]] <- l
        }
        return(sum(total_len))
}
len_of_frames_in_list(left_rep_frames_list)
len_of_frames_in_list(right_rep_frames_list)
        ## more tweets aimed at right 

lapply(left_rep_frames_list, avg_sent)
        ## decrease overall to 1.0
        ## positive to 8.25
        ## negative to -8.25

lapply(right_rep_frames_list, avg_sent)
        ## decrease overall to 3.5
        ## pos to 8.5
        ## neg to -5.6 

## ggplot() ? 

d_left_frames_list <- lapply(left_frames_list, term_doc_df)
d_right_frames_list <- lapply(right_frames_list, term_doc_df)

write.csv(d_left_frames_list[[1]], "d_left_frame_df.csv", row.names=FALSE, na="NA")
write.csv(d_left_frames_list[[2]], "d_left_frame_neu_df.csv", row.names=FALSE, na="NA")
write.csv(d_left_frames_list[[3]], "d_left_frame_pos_df.csv", row.names=FALSE, na="NA")
write.csv(d_left_frames_list[[4]], "d_left_frame_neg_df.csv", row.names=FALSE, na="NA")

write.csv(d_right_frames_list[[1]], "d_right_frame_df.csv", row.names=FALSE, na="NA")
write.csv(d_right_frames_list[[2]], "d_right_frame_neu_df.csv", row.names=FALSE, na="NA")
write.csv(d_right_frames_list[[3]], "d_right_frame_pos_df.csv", row.names=FALSE, na="NA")
write.csv(d_right_frames_list[[4]], "d_right_frame_neg_df.csv", row.names=FALSE, na="NA")





## Remove mentions of either US Presidential Candidates 2016 candidates 
d_left_rep_frames_list <- lapply(d_left_rep_frames_list, function(x) {
        remove_mentions <- x[!grepl("^@|trump|hillary", x$word),]
}) 
for (i in 1:length(d_left_rep_frames_list)) {
        print(head(d_left_rep_frames_list[[i]],10))
}

d_right_rep_frames_list <- lapply(d_right_rep_frames_list, function(x) {
        remove_mentions <- x[!grepl("^@|donald|trump|hillary|clinton", x$word),]
})
for (i in 1:length(d_right_rep_frames_list)) {
        print(head(d_right_rep_frames_list[[i]],10))
}

## Create wordclouds and save them locally
save_wordcloud(d_left_rep_frames_list, cols_list, z="#E4F2FF")
save_wordcloud(d_right_rep_frames_list, cols_list, z="#FEDEDE")

## What were the most common words aimed at both left and right? 
political_tweets <- NULL
for (i in 1:length(left_rep_frames_list)) {
        political_tweets[[i]] <- rbind(left_rep_frames_list[[i]], 
                                       right_rep_frames_list[[i]])
}
names(political_tweets) <- names(frames_list)
d_political_tweets <- lapply(political_tweets, term_doc_df)
d_political_tweets <- lapply(d_political_tweets, function(x) {
        remove_mentions <- x[!grepl("^@|donald|trump|hillary|clinton", x$word),]
})
for (i in 1:length(d_political_tweets)) {
        print(head(d_political_tweets[[i]],10))
}

lapply(political_tweets, avg_sent)
        ## overall: 2.7
        ## pos: 8.4
        ## neg: -6.6

## all political tweets
grep_vocab <- lapply(d_political_tweets, remove_non_words)
avg_unique_words(grep_vocab, political_tweets)
        ## overall increased from 0.2 -> 5.3 unique words/tweet
        ## neu: 2.5 -> 6
        ## pos: 0.3 -> 6
        ## neg: 2.0 -> 5.9 
avg_num_words(grep_vocab, political_tweets)
        ## across the board, 5.3 words per tweet
        ## neutral tweets were the shortest at 4.9
        ## positive tweets were at 5.4
        ## negative tweets were the longest at 5.8

## all tweets aimed at left 
grep_vocab <- lapply(d_left_rep_frames_list, remove_non_words)
avg_unique_words(grep_vocab, left_rep_frames_list)
        ## across the board, 6.8 unique words per tweet 
        ## neu: 6.5
        ## pos: 7.4
        ## neg: 7.0
avg_num_words(grep_vocab, left_rep_frames_list)
        ## across the board, 8.4 words per tweet
        ## neu: 6.5
        ## pos: 9.0
        ## neg: 7.8 

## all tweets aimed at right
grep_vocab <- lapply(d_right_rep_frames_list, remove_non_words)
avg_unique_words(grep_vocab, right_rep_frames_list)
        ## across the board, 5.7 unique words per tweet
        ## neu: 6.0
        ## pos: 6.3
        ## neg: 6.0 
avg_num_words(grep_vocab, right_rep_frames_list)
        ## across the board, 7.8 words per tweet
        ## neu: 6.0
        ## pos: 8.2
        ## neg: 7.2 

## t.test? 

