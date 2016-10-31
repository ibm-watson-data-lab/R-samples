library("twitteR")
library("plyr")
library("stringr")
library("httr")
library("jsonlite")

setup_twitter_oauth("XXXX","XXXX",access_token="XXXX",access_secret="XXXX")

pi_url="https://gateway.watsonplatform.net/personality-insights/api/v2/profile"
username = "XXXX" # your own here 
password = "XXXX"  # your own here 
username_password = paste(username,":",password,sep="")

personality_insights <- function(textTweets){
	print(textTweets)
	response <- POST(url=pi_url,authenticate(username,password),add_headers("Content-Type"="text/plain","charset"="utf-8" ),body = textTweets)

	data <- content(response)
return(data)
}

plot_pi <- function(pidata,user){
	
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
	print(pval)	
	
	
		
		if((i == 10) || (i == 45) || (i == 80) || (i == 115) || (i == 150)) {
			
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(big5) == 0)
		big5 <- temp
		else
		big5 <- cbind(big5, temp)
		}
		
	
	
	if((i > 10) && (i < 45)){
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(openness) == 0)
		openness <- temp
		else
		openness <- cbind(openness, temp)
	
		}
	
	if((i > 45) && (i < 80)){
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(conscientiousness) == 0)
		conscientiousness <- temp
		else
		conscientiousness <- cbind(conscientiousness, temp)
	
		}

	if((i > 80) && (i < 115)){
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(extraversion) == 0)
		extraversion <- temp
		else
		extraversion <- cbind(extraversion, temp)
	
		}
	

	if((i > 115) && (i < 150)){
		
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(agreeableness) == 0)
		agreeableness <- temp
		else
		agreeableness <- cbind(agreeableness, temp)
	
		}
	

	if((i > 150) && (i < 185)){
		
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
while(i < 251){
	pval <- pidata[1,(i+2)] * 100	
	print("needs")
	print(pval)	
	
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(needs) == 0)
		needs <- temp
		else
		needs <- cbind(needs, temp)
		

i <- i + 5	
}	

i = 257
while(i < 280){
	pval <- pidata[1,(i+2)] * 100	
	print("values")
	print(pval)	
	
		temp <- data.frame(pval)
		colnames(temp) <- pidata[1,i]
		
		if(length(values) == 0)
		values <- temp
		else
		values <- cbind(values, temp)
		

i <- i + 5	
}	

par(las = 2, mfrow = c(3,3)) #print x labels perpendicular to the x-axis

bp <- barplot(unlist(big5),cex.axis=0.5,cex.names=0.5,main = paste("Big 5 Personality for ",user), ylab = "Score in Percentage%", ylim = c(0,100),col='deepskyblue3')
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

tweets <- searchTwitter("from:realDonaldTrump", n=100)
typeof(tweets)
length(tweets)

textTweets <- laply(tweets, function(t) t$getText())

personality_data <- personality_insights(textTweets)
plot_pi(as.data.frame(personality_data["tree"]), "Donald Trump")   
 
#str_replace(textTweets,' \"', '') 

#for(i in 1:50){
		#print(paste('Tweet is ',tweets[i]))
		#typeof(tweets[i])
		#print(tweets[i])
#		write(tweets[i],file="somedata", append=TRUE, sep = " `")
#}


#j = 1
# for(i in 1:280){ 
	# if(str_detect(colnames(pidata[i]),"category")){
		 # print(pidata[i-1])
		 # print(pidata[1,i+1] * 100)
		 # pi_rownames[j] <- pidata[i-1]
		 # pi_results[j] <- (pidata[1,(i+1)] * 100)
		 # j <- j + 1
		 # } 
		 
# }



