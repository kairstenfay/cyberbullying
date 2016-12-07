import json
import sys
import string
import operator
import re 



def main():
	sent_file = open(sys.argv[1])
	tweet_file = open(sys.argv[2])
	#insults_file = open(sys.argv[3]) 
	
	# Make sent_file usable
	scores = {}     
	for line in sent_file:
		term, score  = line.split("\t") 	 				# The file is tab-delimited. 
		term = term.translate(None, string.punctuation) 	# Remove punctuation marks 
		scores[term] = int(score) 		 					# Convert the score to an integer.
	
	# Make insults_file usable        
	#insults_raw = insults_file.readlines()
	#insults_list = insults_raw[0].split(",")
	#insults = set()
	#for insult in insults_list:
#		insult = insult.translate(None, string.punctuation) # Remove punctuation marks 
#		insults.add(insult.strip().lower())
	
	# Make tweet_file usable
	parsed_file = load_tweets(tweet_file)							## Code chunk 1
	
	# Begin filtering 
	eng_tweet_list = english_checker(parsed_file)					## Code chunk 2
	original_tweets = remove_RTs(eng_tweet_list)					## Code chunk 2.5
	#scoreable_tweet_list = score_checker(original_tweets, scores) 	## Code chunk 3 -- removed 
	#mention_tweet_list = mention_checker(scoreable_tweet_list)		## Code chunk 4 -- removed
	#bullying_tweet_list = bully_checker(mention_tweet_list, insults)## Code chunk 5 -- removed 
	tweet_sent_list, unk_words =  sentimentTuples(eng_tweet_list, scores) ## Code chunk 6 
	unk_list = assignWordScore(tweet_sent_list, unk_words)			## Code chunk 7

	# Make unknown scores usable 
	unk_scores = {}
	for tuplee in unk_list:
		unk_scores[tuplee[0]] = int(tuplee[1])

	sentiment_scores =  sentimentScore(eng_tweet_list, scores, unk_scores)	
																	## Code chunk 8
	print json.dumps(sentiment_scores)
	# print json.dumps(sentiment_scores)

	#for word in unk_words:
	#	print json.dumps(word)

	#print json.dumps(unk_scores)
   
		
############################          
## User defined functions ##
############################

# Code chunk 1 
# Parse the JSON file
def load_tweets(fp):
	json_objs = []
	for line in fp:
		json_objs.append(json.loads(line))
	return json_objs

# Code chunk 2
# Return tweets in English
def english_checker(parsed_tweets):
	english_tweets = []
	for tweet in parsed_tweets:
		if 'lang' in tweet:
			language = tweet['lang']
			if language == "en":
				english_tweets.append(tweet)
	return english_tweets

# Code chunk 2.5
# Filter out RTs
def remove_RTs(parsed_tweets):
	printable = set(string.printable)
	original_content = []
	for tweet in parsed_tweets:
		if 'text' in tweet:
			tweet_text = tweet['text'].encode('utf-8')
			tweet_text_ascii = filter(lambda x: x in printable, tweet_text)
			matchObj = re.search("^RT", tweet_text_ascii)
			if not matchObj:
				original_content.append(tweet)
	return original_content

# Code chunk 3
# Return tweets that are score-able using the AFINN file 
#def score_checker(parsed_tweets, scores):
#	printable = set(string.printable)
#	scoreable_tweets = []
#	for tweet in parsed_tweets:
#		if 'text' in tweet:
#			tweet_text = tweet['text'].encode('utf-8')
#			tweet_text_ascii = filter(lambda x: x in printable, tweet_text)			# remove non-ascii
#			tweet_text_strp = tweet_text_ascii.translate(None, string.punctuation)	# remove punctuation
#			tweet_text_split = tweet_text_strp.split(' ')
#			for word in tweet_text_split:
#				word = word.lower().strip() 
#				if word in scores:
#					if tweet not in scoreable_tweets:
#						scoreable_tweets.append(tweet)
#						break
#	return scoreable_tweets
 

# Code chunk 4
# Return tweets that have a user mentioned 
#def mention_checker(parsed_tweets):
#	mention_tweets = []
#	for i in range(0, len(parsed_tweets)):
#		mentions_attr = parsed_tweets[i]['entities']['user_mentions']
#		if len(mentions_attr) != 0:
#			# mentioned_person = mentions_attr['screen_name'].split(",") ## use for later?
#			mention_tweets.append(parsed_tweets[i])
#	return mention_tweets

# Code chunk 5
# Return tweets that have an insult from the insults_file 
#def bully_checker(mention_tweet_lst, insults):
#	printable = set(string.printable)
#	punct_marks = set(string.punctuation)
#	bullying_tweets = []
#	for tweet in mention_tweet_lst:
#		if 'text' in tweet:
#			tweet_text = tweet['text'].encode("utf-8")
#			tweet_text_ascii = filter(lambda x: x in printable, tweet_text)
#			tweet_text_strp = tweet_text_ascii.translate(None, string.punctuation)
#			tweet_text_split = tweet_text_strp.split(' ')
#			for word in tweet_text_split:
#				word = word.lower()
#				if len(word) > 1:
#					if word in insults:
#						bullying_tweets.append(tweet)
#						break
#	return bullying_tweets

# Code chunk 6
# Returns a 2-tuple. First item is a list of tuples, where the first
# item in each tuple is the text of a tweet, and the second item is its score
# The second item is a set of words that we want to find the sentiment of. 
# The point of this function is to use the output to impute word scores to 
# unknown words in the next function 
def sentimentTuples(tweet_list, scores):
	printable = set(string.printable)
	sentiment = []
	unknown_words = set()
	for tweet in tweet_list:
		count = 0
		if 'text' in tweet:
			tweet_text = tweet['text'].encode("utf-8")
			tweet_text_ascii = filter(lambda x: x in printable, tweet_text)
			tweet_text_split = tweet_text_ascii.split(' ')
			for word in tweet_text_split:
				word = word.lower().strip() 
				matchObj = re.search(r'(.)\1{2,}|@|[0-9]|http|^&amp', word)
				if matchObj:
					word_strp = "" 
				elif not matchObj:
					word_strp = word.translate(None, string.punctuation).strip() 
					if len(word_strp)>2:
						if word_strp in scores:
							count += scores[word_strp]
						else:
							unknown_words.add(word_strp)
		tweet_score = tweet_text, count
		sentiment.append(tweet_score)
	return sentiment, unknown_words


# Code chunk 7 
# For every word in unknown words, average the sentiment of all the tweets that contain 
# that word.
# tweet_sentiment_list is a list of tuples with the tweet text and its score according to 
# the AFINN file. unknown_words is the list of words that do not appear in the AFINN file
# but are found in at least one tweet. 
def assignWordScore(tweet_sentiment_list, unknown_words):
	unknown_list = []
	for word in unknown_words:
		totalSent = 0
		numTweets = 0
		for tuplee in tweet_sentiment_list:
			tuplee_strp = tuplee[0].translate(None, string.punctuation).lower()
			tuplee_split = tuplee_strp.split(' ') 
			if word in tuplee_split:
				totalSent += tuplee[1]
				numTweets += 1
		if numTweets != 0:
			avg_score = float(totalSent)/numTweets
			unknown_tuple = word, avg_score
			unknown_list.append(unknown_tuple)
	return unknown_list
	  

# Code chunk 8
# Return the sentiment score of a tweet according to the AFINN file 
def sentimentScore(tweet_list, scores, unk_scores):
	printable = set(string.printable)
	sentiment = []
	for tweet in tweet_list:
		count = 0
		tweet_text = tweet['text'].encode("utf-8")
		tweet_text_ascii = filter(lambda x: x in printable, tweet_text)
		tweet_text_strp = tweet_text_ascii.translate(None, string.punctuation)
		tweet_text_split = tweet_text_strp.split(' ')
		for word in tweet_text_split:
			word = word.lower().strip()  
			if word in scores:
				count += scores[word]
			elif word in unk_scores:
				count += unk_scores[word]
		tweet['sentiment'] = count
		sentiment.append(count)
	return tweet_list	


if __name__ == '__main__':
	main()
