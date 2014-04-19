# Written by Sumandro Chattapadhyay
# <http://ajantriks.net/>
# Shared under Creative Commons Attribution-ShareAlike 2.5 India license

# Opening and preparing data for each types of organisation
# Note: To see how these files were generated, go to: <https://github.com/ajantriks/netmundial/blob/master/R/cis_ig_vis_wordcloud.R>

ac <- read.csv("data/cis_ig_vis_word_cloud_academia_freq.csv")
ac$sector <- 1
View(ac)

cs <- read.csv("data/cis_ig_vis_word_cloud_civil_society_freq.csv")
cs$sector <- 2
View(cs)

go <- read.csv("data/cis_ig_vis_word_cloud_government_freq.csv")
go$sector <- 3
View(go)

ot <- read.csv("data/cis_ig_vis_word_cloud_other_freq.csv")
ot$sector <- 4
View(ot)

ps <- read.csv("data/cis_ig_vis_word_cloud_private_sector_freq.csv")
ps$sector <- 5
View(ps)

tc <- read.csv("data/cis_ig_vis_word_cloud_technical_community_freq.csv")
tc$sector <- 6
View(tc)

# Combining data

d <- rbind(ac, cs, go, ot, ps, tc)
View(d)

# Generate average frequency of the words across contributions from different types of organisation

d$avg.freq <- with(d, ave(freq, word))
View(d)

# Sorting the rows in terms of words first, and then in terms of average frequencies, and saving the table

ds <- d[order(-d$avg.freq, d$word) , ]
View(ds)
write.csv(ds, file = "data/cis_ig_vis_word_heatmap.csv")

# Removing certain words

ds$drop <- 0
ds$drop[ds$word == "aspen"] <- 1
ds$drop[ds$word == "issues"] <- 1
ds$drop[ds$word == "new"] <- 1
ds$drop[ds$word == "use"] <- 1
ds$drop[ds$word == "world"] <- 1
ds$drop[ds$word == "organizations"] <- 1
ds$drop[ds$word == "model"] <- 1
ds$drop[ds$word == "need"] <- 1
ds$drop[ds$word == "role"] <- 1
ds$drop[ds$word == "states"] <- 1
ds$drop[ds$word == "government"] <- 1
ds$drop[ds$word == "meeting"] <- 1
ds$drop[ds$word == "important"] <- 1
ds$drop[ds$word == "support"] <- 1
ds$drop[ds$word == "ensure"] <- 1
ds$drop[ds$word == "existing"] <- 1
ds$drop[ds$word == "group"] <- 1
ds$drop[ds$word == "groups"] <- 1
ds$drop[ds$word == "different"] <- 1
ds$drop[ds$word == "level"] <- 1
ds$drop[ds$word == "promote"] <- 1
ds$drop[ds$word == "current"] <- 1
ds$drop[ds$word == "following"] <- 1
ds$drop[ds$word == "wif"] <- 1
d2 <- subset(ds, ds$drop == "0")
d2$drop <- NULL

# Selecting the top fifty most frequent words across all the contributions

d3 <- subset(d2[1:300 , ])
View(d3)

# Naming the sectors

d3$sector.name[d3$sector == "1"] <- "academia"
d3$sector.name[d3$sector == "2"] <- "civil society"
d3$sector.name[d3$sector == "3"] <- "government"
d3$sector.name[d3$sector == "4"] <- "other"
d3$sector.name[d3$sector == "5"] <- "private sector"
d3$sector.name[d3$sector == "6"] <- "technical community"
View(d3)

# Entering total cotributions by each types of organisation

d3$contributions[d3$sector == "1"] <- "21"
d3$contributions[d3$sector == "2"] <- "59"
d3$contributions[d3$sector == "3"] <- "28"
d3$contributions[d3$sector == "4"] <- "20"
d3$contributions[d3$sector == "5"] <- "43"
d3$contributions[d3$sector == "6"] <- "16"
d3$contributions <- as.numeric(d3$contributions)
View(d3)

# Generating heatmap - absolute

library(ggplot2)
library(scales)
ggplot(d3, aes(x = sector.name, y = word, fill = freq)) + geom_tile() + scale_fill_gradient(low = "#edf8fb", high = "#006d2c", name = "Word Frequency:") + xlab("") + ylab("") + theme(legend.title = element_text(size = "13", colour = "#333333"), legend.text = element_text(size = "12", colour = "#333333"), axis.text = element_text(size = "13", colour = "#333333"), panel.background = element_rect(fill = "white"), legend.title.align = "0")

# Generating weighted frequency of words by dividing frequency by number of contributions of each types of organisation

d3$rel.freq <- with(d3, d3$freq / d3$contributions)
View(d3)

# Generating heatmap - relative

ggplot(d3, aes(x = sector.name, y = word, fill = rel.freq)) + geom_tile() + scale_fill_gradient(low = "#edf8fb", high = "#006d2c", name = "Word Frequency:") + xlab("") + ylab("") + theme(legend.title = element_text(size = "13", colour = "#333333"), legend.text = element_text(size = "12", colour = "#333333"), axis.text = element_text(size = "13", colour = "#333333"), panel.background = element_rect(fill = "white"), legend.title.align = "0")

