# see https://github.com/ceboylan/got_crawl_explore repo

#see http://structuraltopicmodel.com/ for more information on stm package used for this Topic Model
got_all <- read.csv("https://raw.githubusercontent.com/ceboylan/got_crawl_explore/master/got_all.csv", header = TRUE)

# only look at works in English
got <- got_all[which(got_all$language=="English"),]

# convert to date format
got$date_updated <- as.Date(got$date_updated._source, "%d %b %Y")

# convert chapters
temp <- strsplit(as.character(got$chapters),"/", fixed=FALSE)
temp <- matrix(unlist(temp), ncol=2, byrow=TRUE)
temp <- as.data.frame(temp)
colnames(temp) <- c("chaps_writ", "chap_total")

got <- cbind(got, temp) # add chaps writ and chaps total 


# Basic text properties correlated with Kudos -----------------------------------

#bookmarks, hits, and kudos are all really collinear, so just use kudos
bookmarkCor <- cor(got$kudos, got$bookmarks._source, use = "na.or.complete", method = "kendall")
hitsCor <- cor(got$kudos, got$hits._source, use = "na.or.complete", method = "kendall")
bookmarks_hitsCor <- cor(got$hits, got$bookmarks, use = "na.or.complete", method = "kendall")

# see how well various properties of the text predict the number of kudos

# number of words
wordsCor <- cor(got$kudos, got$words._source, use = "na.or.complete", method = "kendall")

# average length of chapters
got$chap_length = as.numeric(got$words)/as.numeric(got$chaps_writ)
chaplengthCor <- cor(got$kudos, got$chap_length, use = "na.or.complete", method = "kendall")

# number of chapters
chapsCor <- cor(got$kudos, as.numeric(got$chaps_writ), use = "na.or.complete", method = "kendall")

# Topics Model ------------------------------------------------------------

# install.packages("stm")
# install.packages("Rtsne")
# install.packages("geometry")
# install.packages("tm")
# install.packages("SnowballC")
library("stm")
library("Rtsne")
library("geometry")
library("tm")
library("SnowballC")
#stemming/stopword removal, etc.
processed <- textProcessor(got$tags, metadata=got)
#structure and index for usage in the stm model. Verify no-missingness.
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
#output will have object meta, documents, and vocab
#corpus should now have 2316 documents, 2414 terms, and 56940 tokens.

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# # plot the number of words and documents removed for given threshold
# plotRemoved(processed$documents, lower.thresh=seq(1,200, by=100))

#Estimation with topical prevalence parameter - in this case, we use Kudos
# got_tagsPrevFit <- stm(out$documents,out$vocab,K=0, prevalence =~ s(kudos), max.em.its=75, data=out$meta, init.type="Spectral", seed=151030)
got_tagsNoPrevFit <- stm(out$documents,out$vocab,K=0, max.em.its=75, data=out$meta, init.type="Spectral", seed=15103031)
# got_tagsPrevFit <- stm(out$documents,out$vocab,K=79, prevalence =~ s(kudos), max.em.its=75, data=out$meta, seed=151031)
# got_tagsNoPrevFit <- stm(out$documents,out$vocab,K=20, max.em.its=75, data=out$meta, seed=151031)

got_tagsSelect <- selectModel(out$documents,out$vocab,K=0, max.em.its=75, data=out$meta, init.type="Spectral", runs=20, seed=15103031)
plotModels(got_tagsSelect)
# choose the second model, as it has highest semantic coherence and exclusivity (rightmost and uppermost of models)
got_tagsFit<-got_tagsSelect$runout[[2]] 

labelTopics(got_tagsFit, c(1,2,4,8,9,10,18,23,25,28,29,32,34,35,36,37,44,53,54,56,59,63,68,70,75))
labelTopics(got_tagsFit, 44)
labelTopics(got_tagsFit, 75)


# First plot, top topics in corpus and concentration of kudpos by  --------
par(mfrow=c(2,1)) 
# expected proportion of the corpus that belongs to each topic
plot.STM(got_tagsFit,type="summary", topics = c(1,2,4,8,9,10,18,23,25,28,29,32,34,35,36,37,44,53,54,56,59,63,68,70,75), xlim=c(0,.3))

# Estimate effect of kudos, comparing two topics
meta$kudos<-as.numeric(meta$kudos)
prep <- estimateEffect(1:75 ~ s(kudos),got_tagsFit, meta=meta, uncertainty = "Global")
# plot.estimateEffect(prep, covariate ="kudos", topics = c(75,9), model=got_tagsFit, method="continuous", xlab="Fewer Kudos ... More Kudos", main="Kudos by Topic", labeltype = "custom", custom.labels = c('Sandor Clegane', 'Ramsay Bolton'))
plot.estimateEffect(prep, covariate ="kudos", topics = c(75,44), model=got_tagsFit, method="continuous", xlab="Fewer Kudos ... More Kudos", main="Kudos by Topic, Red: Topic 75 (Sandor), Blue: Topic 44 (Daenerys)", labeltype = "custom", custom.labels = c('75', '44'))
# plot.estimateEffect(prep, covariate ="kudos", topics = c(32, 75, 37,9,68), model=got_tagsFit, method="continuous", xlab="Fewer Kudos ... More Kudos", main="Kudos by Topic", labeltype = "custom", custom.labels = c('Ned Stark', 'Sandor Clegane','Arya Stark', 'Ramsay Bolton', 'Lyanna Stark'))


# Second plot, wordcloud- and term-based -----------------------------------
# install.packages("wordcloud")
library("wordcloud")
# par(mfrow=c(3,1)) 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
# Compare term contributions across two similar topics (Jorah and Daenerys, Drogo and Daenerys)
plot.STM(got_tagsFit,type="perspectives", topics=c(44,4))

# Word clouds

cloud(got_tagsFit, topic=44, max.words=20, main="test")
cloud(got_tagsFit, topic=4, max.words=20)

