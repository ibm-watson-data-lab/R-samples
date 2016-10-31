
options(warn=-1) #turn off warnings globally for the notebook

library("twitteR")
library("plyr")
library("stringr")
library("httr")


library("couchDB")

library(xtable)

sqlContext <- sparkRSQL.init(sc)

myconn <- couch_http_connection(host = "XXXX-bluemix.cloudant.com", port = 443, https = TRUE,
service = "cloudant", user = "XXXX-bluemix", password = "XXXX")

couch_list_databases(myconn)

print(myconn)


results <- couch_fetch(myconn, database = "tweets2/_all_docs", key = NULL, myOpts = NULL)
results_df <- data.frame(results)
df <- createDataFrame(sqlContext, results_df)

typeof(results)

keys_list <- data.frame(results)
print(keys_list[,'total_rows'])
#print(keys_list[,'rows.key.3'])
rows_df_2 <- data.frame()

    #for (i in 1:(keys_list[,'total_rows'] - 1) ){
    	
    	   for (i in 1:100 ){
        #print(i)
        key <- paste('rows.key.',i,sep="")
        docs <- couch_fetch(myconn, database = "tweets2", key = keys_list[,key], myOpts = NULL)   
 
        rows_df <- data.frame(docs)  
        rows_df_2 <- rbind(rows_df_2,rows_df)
    
    }

df2 <- createDataFrame(sqlContext, rows_df_2)
#printSchema(df2)
#showDF(df2)


registerTempTable(df2,"tweets")


negative <- c("Anger","Disgust","Fear","Sadness","Tentative","Emotional Range")
positive <- c("Joy","Analytical","Confident","Openness","Conscientiousness","Extraversion","Agreeableness")


toneanalyzer_url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19"
username = "XXXX" # your own here 
password = "XXXX"  # your own here 
username_password = paste(username,":",password,sep="")


tone_analyzer <- function(text){
	
	response <- POST(url=toneanalyzer_url,authenticate(username,password),add_headers("Content-Type"="text/plain","charset"="utf-8" ),body = text)

	data <- content(response)

return(data)
}


compute_score <- function(data){
	tonedata <- as.data.frame(data$document_tone$tone_categories)

    scorelist <- list()
	scorenames <- list()

	score <- "tones.score"
	tone_name <- "tones.tone_name"

	scorelist[1] <- tonedata[score] * 100
	scorenames[1] <- toString(tonedata[1,tone_name])
		
	for(i in 2:13){ 

			score <- str_replace(paste('tones.score.',(i-1)), ' ', '') 
			tone_name <- str_replace(paste('tones.tone_name.',(i-1)), ' ', '') 
		
			#print(tonedata[score]) 
			#print(tonedata[tone_name])
			scorelist[i] <- tonedata[score] * 100
			scorenames[i] <- toString(tonedata[1,tone_name])
		}
		
	df_scorelist <- as.data.frame(scorelist)
	scorenames <-unlist(scorenames)
		
	colnames(df_scorelist) <- scorenames	

return(df_scorelist)
	
}

test_tone <- function(df_scorelist){
	# valid values are neutral, +ve and -ve
	# if either 3 or more emotions are greater than 50% in negative or positive group count as that group
	# if either emotion is greater than 85% counts as that group
	# if both +ve and -ve are true that tweet is neutral

test_result <- "NEUTRAL"

j <- 0

for(i in 1:length(negative)){
	
	if(df_scorelist[negative[i]] > 85){
		#print("Emotion > 85% is ")
		#print(negative[i])
		test_result <- "NEGATIVE"
	}else if(df_scorelist[negative[i]] > 50){
		j <- j + 1
	}
}
if(j > 3){
	test_result <- "NEGATIVE"
}

j <- 0

for(i in 1:length(positive)){
	
	
			if(df_scorelist[positive[i]] > 85){
				#print("Emotion > 85% is ")
				#print(positive[i])
				if(str_detect(test_result,"NEUTRAL"))	{
				test_result <- "POSITIVE"
				break
				}else if(str_detect(test_result,"NEGATIVE")){
					test_result <- "NEUTRAL" # both -ve and +ve are quite high
					break
				}
			}else if(df_scorelist[positive[i]] > 50){
			j <- j + 1
			}
		
}

if(j > 3){

if(str_detect(test_result,"NEUTRAL"))	{
	
	test_result <- "POSITIVE"
     }else if(str_detect(test_result,"NEGATIVE")){
  	
  	test_result <- "NEUTRAL" # both -ve and +ve are quite high
	}
 }
j <- 0

return(test_result)		
}



printSchema(df2)

authors <- as.data.frame(select(df2,"userid"))
typeof(authors)

nrow(authors)
nrow(df2)

print(authors)

str_replace(paste("from:",authors[3,1]), ' ','')

setup_twitter_oauth("XXXX","XXXX",access_token="XXXX",access_secret="XXXX")

#searchstring <- str_replace(paste("from:",authors[3,1]), ' ','')

searchstring <- str_replace(paste("from:","kittentacos"), ' ','')
print(searchstring)
tweets <- searchTwitter(searchstring, n=10, retryOnRateLimit = 1000)
typeof(tweets)
length(tweets)

stddev <- function(dfclassify){
    lastrow <- nrow(dfclassify)
    summary <- data.frame(dfclassify[1,1],dfclassify[lastrow,3],dfclassify[lastrow,4],dfclassify[lastrow,5])
    colnames(summary) <- c('Author','Positive','Negative','Neutral')
    
    for(i in 6:18){
     sdvalue <- sd(dfclassify[,i])
     meanvalue <- mean(dfclassify[,i])   
        #print(dfclassify[i])
        #print(sdvalue)
     s <- data.frame(sdvalue)
     colnames(s) <- paste("SD",colnames(dfclassify[i]))
     summary <- cbind(summary,s)   
     
     m <- data.frame(meanvalue)
     colnames(m) <- paste("Mean",colnames(dfclassify[i]))
     summary <- cbind(summary,m)     
    }
    
    return(summary)
}

classify <- data.frame()
summarytw <- data.frame()

for(j in 1:10){

searchstring <- str_replace(paste("from:",authors[j,1]), ' ','')

#print(searchstring)
tweets <- searchTwitter(searchstring, n=2)
typeof(tweets)
length(tweets)

if(length(tweets) > 0){    
textTweets <- laply(tweets, function(t) t$getText())

#print(textTweets)
pos <- 0
neg <- 0
neut <- 0

    #classify <- data.frame(matrix(NA, nrow=length(tweets), ncol=18))
   
    classifytw <- data.frame()
for(i in 1:length(tweets)){
	
	#print(textTweets[i])
    
	data <- tone_analyzer(textTweets[i])

	df_scorelist <- compute_score(data)
	#print(df_scorelist)
	
	tone_result <- test_tone(df_scorelist)
	#print(tone_result)
	
	if(str_detect(tone_result,"NEUTRAL")){ neut <- neut + 1 }
	if(str_detect(tone_result,"NEGATIVE")){ neg <- neg + 1 }
	if(str_detect(tone_result,"POSITIVE")){ pos <- pos + 1 }

    classifytweet <- data.frame(authors[j,1],textTweets[i], pos,neg,neut,df_scorelist)
    classifytw <- rbind(classifytw,classifytweet)
	}

    #print("Neutral Negative Positive are ")
	#print(neut)
	#print(neg)
	#print(pos)
   
   
    
   colnames(classifytw) <- c('Author','Tweet','Positive','Negative','Neutral', colnames(df_scorelist))   
   classify <- rbind(classify,classifytw)
   summarytw <- rbind(summarytw,stddev(classifytw))
 }   
}    

classify.table <- xtable(classify)
print.xtable(classify.table, type="html", file = "")
                    
print("Summary")                    
summarytw.table <- xtable(summarytw)
print.xtable(summarytw.table, type="html", file = "")

print("<p>Summary</p>",type="html",file = "")

print(textTweets[i])
print(i)
print(length(tweets))
print(tweets)
typeof(tweets)
length(tweets)
something <- laply(tweets, function(t) t$getText())
    something
    
 if(length(tweets) == 0){print("Equal ")   }

getwd()

list.files('/gpfs/global_fs01/sym_shared/YPProdSpark/user/s9d7-8f4b908f520f55-4137fa4057f6/notebook/work')

print(classify)

classify

unlink('/gpfs/global_fs01/sym_shared/YPProdSpark/user/s9d7-8f4b908f520f55-4137fa4057f6/notebook/work/ClassifyTweets.html')


