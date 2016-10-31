
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


pi_url="https://gateway.watsonplatform.net/personality-insights/api/v2/profile"
pi_username = "XXXX" # your own here 
pi_password = "XXXX"  # your own here 
pi_username_password = paste(pi_username,":",pi_password,sep="")

personality_insights <- function(textTweets){
    
    response <- POST(url=pi_url,authenticate(pi_username,pi_password),add_headers("Content-Type"="text/plain","charset"="utf-8" ),body = textTweets)

    data <- content(response)
    
return(data)    
}

#Unfold JSON response of Personality Insights

plot_pi <- function(pidata){

big5 <- data.frame()
needs <- data.frame()
values <- data.frame()
openness <- data.frame()
conscientiousness <- data.frame()
extraversion <- data.frame()
agreeableness <- data.frame()
emotional_range <- data.frame()

i = 10

while(i < 184){

	pval <- pidata[1,(i+2)] * 100	
	#print(pval)	
	
	
		
		if((i == 10) || (i == 45) || (i == 80) || (i == 115) || (i == 150)) { #Big5
			
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(big5) == 0)
		big5 <- temp
		else
		big5 <- cbind(big5, temp)
		}
		
	
	
	if((i > 10) && (i < 45)){ #Openness
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(openness) == 0)
		openness <- temp
		else
		openness <- cbind(openness, temp)
	
		}
	
	if((i > 45) && (i < 80)){ #Conscientiousness
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(conscientiousness) == 0)
		conscientiousness <- temp
		else
		conscientiousness <- cbind(conscientiousness, temp)
	
		}

	if((i > 80) && (i < 115)){ #Extraversion
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(extraversion) == 0)
		extraversion <- temp
		else
		extraversion <- cbind(extraversion, temp)
	
		}
	

	if((i > 115) && (i < 150)){ #Agreeableness
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(agreeableness) == 0)
		agreeableness <- temp
		else
		agreeableness <- cbind(agreeableness, temp)
	
		}
	

	if((i > 150) && (i < 185)){ #Emotional Range
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(emotional_range) == 0)
		emotional_range <- temp
		else
		emotional_range <- cbind(emotional_range, temp)
	
		}
	

	
	
i <- i + 5	
}		
i = 191
while(i < 251){ #Needs
	pval <- pidata[1,(i+2)] * 100	
	#print("needs")
	#print(pval)	
	
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(needs) == 0)
		needs <- temp
		else
		needs <- cbind(needs, temp)
		

i <- i + 5	
}	

i = 257
while(i < 280){ #Values
	pval <- pidata[1,(i+2)] * 100	
	#print("values")
	#print(pval)	
	
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(values) == 0)
		values <- temp
		else
		values <- cbind(values, temp)
		

i <- i + 5	
}	

par(las = 2, mfrow = c(3,3)) #print x labels perpendicular to the x-axis

bp <- barplot(unlist(big5),cex.axis=0.5,cex.names=0.5,main = "Big 5 Personality", ylab = "Score in Percentage%", ylim = c(0,100),col='deepskyblue3')
text(bp,0,round(unlist(big5),1), pos = 3)

bp <- barplot(unlist(needs),cex.axis=0.5,cex.names=0.5,main = "Needs", ylab = "Score in Percentage%", ylim = c(0,100),col='seagreen3')
text(bp,0,round(unlist(needs),1), pos = 3)

bp <- barplot(unlist(values),cex.axis=0.5,cex.names=0.5,main = "Values", ylab = "Score in Percentage%", ylim = c(0,100),col='orangered3')
text(bp,0,round(unlist(values),1), pos = 3)

bp <- barplot(unlist(openness),cex.axis=0.5,cex.names=0.5,main = "Openness", ylab = "Score in Percentage%", ylim = c(0,100),col='deepskyblue')
text(bp,0,round(unlist(openness),1), pos = 3)

bp <- barplot(unlist(conscientiousness),cex.axis=0.5,cex.names=0.5,main = "Conscientiousness", ylab = "Score in Percentage%", ylim = c(0,100),col='deepskyblue')
text(bp,0,round(unlist(conscientiousness),1), pos = 3)

bp <- barplot(unlist(extraversion),cex.axis=0.5,cex.names=0.5,main = "Extraversion", ylab = "Score in Percentage%", ylim = c(0,100),col='deepskyblue')
text(bp,0,round(unlist(extraversion),1), pos = 3)

bp <- barplot(unlist(agreeableness),cex.axis=0.5,cex.names=0.5,main = "Agreeableness", ylab = "Score in Percentage%", ylim = c(0,100),col='deepskyblue')
text(bp,0,round(unlist(agreeableness),1), pos = 3)

bp <- barplot(unlist(emotional_range),cex.axis=0.5,cex.names=0.5,main = "Emotional Range", ylab = "Score in Percentage%", ylim = c(0,100),col='deepskyblue')
text(bp,0,round(unlist(emotional_range),1), pos = 3)

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

for(j in 1:2){

searchstring <- str_replace(paste("from:",authors[j,1]), ' ','')

#print(searchstring)
tweets <- searchTwitter(searchstring, n=100)
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
 

    #Call Personality Insights for each Twitter author
                    
    personality_data <- personality_insights(textTweets)
    plot_pi(as.data.frame(personality_data["tree"]))   
}
                   
}    

classify.table <- xtable(classify)
print.xtable(classify.table, type="html", file = "")
                    
print("Summary")                    
summarytw.table <- xtable(summarytw)
print.xtable(summarytw.table, type="html", file = "")

perosonality_data

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


