

count_word <- function(str){
  
  n <- lengths(gregexpr("\\W+",str))-1
  return(n)
}




get_author <- function(df,author_str)
{
  df <- df%>%filter(author == author_str)
  return(df)
}


senti_plot <- function(sent_data,author){
  sentences <- syuzhet::get_sentences(sent_data$sentence_str)
  emos<-matrix(emotion(sentences)$emotion, 
               nrow =length(sentences), 
               byrow=T)
  colnames(emos)=emotion(sentences)$emotion_type[1:16]
  df<-data.frame(emos)
  df<-df%>%select(anger,
                  anticipation,
                  disgust,
                  fear,
                  joy,
                  sadness,
                  surprise,
                  trust)
  df$domEmo <- apply(df,1,which.max)
  df$domEmoV <- apply(df[,1:8],1,max)
  df$domEmo[df$domEmoV<0.01]=0
  df$domEmo =df$domEmo+1
  
  temp=df$domEmoV 
  df$domEmoV [temp<0.05]=1
  word_count <- count_word(sentences)
  df$sent_id <- c(1:length(sentences))
  df$word_count <- word_count
  cols <- c("deeppink","coral","darkmagenta","darkgreen","yellow","cornflowerblue","darkcyan","brown1")
  col_sel <- alpha(cols[df$domEmo],sqrt(df$domEmoV))
  plot(df$sent_id, df$word_count,col=col_sel,type = "h",main=author)
}




