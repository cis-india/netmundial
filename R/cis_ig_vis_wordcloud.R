# Created by Sumandro Chattapadhyay
# <http://ajantriks.net/>
# Shared under Creative Commons Attribution-ShareAlike 2.5 India license

# Load libraries
library(tm)
library(SnowballC)
library(wordcloud)

# Open file and convert to UTF 8
d <- readLines("X.txt")
View(d)
d1 <- iconv(d, to = "utf-8", sub = "")
View(d1)

# Create corpus
d2 <- Corpus(VectorSource(d1), readerControl = list(languge="English"))
inspect(d2)

# Remove common english words and convert into a matrix (of terms and frequencies of appearance)
d3 <- tm_map(d2, removeWords, stopwords("english"))
inspect(d3)

tdm <- TermDocumentMatrix(d3)
m <- as.matrix(tdm)
View(m)

# Sort the terms in terms of frequency (from most frequent to least) and converting into a dataframe (table)
v <- sort(rowSums(m), decreasing=TRUE)
df <- data.frame(word = names(v), freq = v)
View(df)

# Remove more common words
df$drop <- 0
View(df)

df$drop[df$word == "the"] <- 1
df$drop[df$word == "must"] <- 1
df$drop[df$word == "will"] <- 1
df$drop[df$word == "can"] <- 1
df$drop[df$word == "also"] <- 1
df$drop[df$word == "this"] <- 1
df$drop[df$word == "one"] <- 1
df$drop[df$word == "two"] <- 1
df$drop[df$word == "may"] <- 1
df$drop[df$word == "within"] <- 1
df$drop[df$word == "based"] <- 1
df$drop[df$word == "without"] <- 1
df$drop[df$word == "many"] <- 1
df$drop[df$word == "including"] <- 1
df$drop[df$word == "following"] <- 1
df$drop[df$word == "related"] <- 1
df$drop[df$word == "able"] <- 1
df$drop[df$word == "set"] <- 1
df$drop[df$word == "making"] <- 1
df$drop[df$word == "make"] <- 1
df$drop[df$word == "these"] <- 1
df$drop[df$word == "way"] <- 1
df$drop[df$word == "well"] <- 1
df$drop[df$word == "even"] <- 1
df$drop[df$word == "part"] <- 1
df$drop[df$word == "general"] <- 1
df$drop[df$word == "made"] <- 1
df$drop[df$word == "they"] <- 1
df$drop[df$word == "however"] <- 1
df$drop[df$word == "means"] <- 1
df$drop[df$word == "among"] <- 1
df$drop[df$word == "for"] <- 1
df$drop[df$word == "take"] <- 1
df$drop[df$word == "http"] <- 1
df$drop[df$word == "www"] <- 1
df$drop[df$word == "org"] <- 1
df$drop[df$word == "2011"] <- 1
View(df)

df$drop[df$word == "internet"] <- 1
df$drop[df$word == "governance"] <- 1
df$drop[df$word == "web"] <- 1
df$drop[df$word == "global"] <- 1
df$drop[df$word == "international"] <- 1
df$drop[df$word == "principle"] <- 1
df$drop[df$word == "principles"] <- 1
df$drop[df$word == "roadmap"] <- 1
View(df)

df2 <- subset(df, df$drop == 0)
View(df2)

# Create word cloud
png("wordcloud.png", width = 1000, height = 600, units = "px", res = 280, type = c("cairo"))
wordcloud(df2$word, df2$freq, max.words = 100, scale = c(2,0.02), rot.per = 0, colors = brewer.pal(5, "Set2"), use.r.layout = FALSE, fixed.asp = FALSE)
dev.off()

# Save terms frequency table
df2$drop <- NULL
View(df2)
write.csv(df2, file = "X_freq.csv")

