#Tuto et code disponibles sur: http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir

# Packages à installer
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")

# Charger les packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#____________________________________________________________________________
#     Charger le texte: 
#____________________________________________________________________________

filepath = "D:/Users/Jeanne/Documents/R_stage_CIRED/Singh_2017.txt"
text = readLines(filepath)
docs = Corpus(VectorSource(text))

#____________________________________________________________________________
#   Transformation, nettoyage du texte: 
#____________________________________________________________________________


# Remplacer les caractères spéciaux non utiles par des espaces:
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, "\\|")

# Convertir le texte en minuscules:
docs = tm_map(docs, content_transformer(tolower))

#Supprimer les nombres:
docs = tm_map(docs, removeNumbers)

#Supprimer les mots vides (anglais):
docs = tm_map(docs, removeWords, stopwords("english"))

#Supprimer sa propre liste de mots:
docs = tm_map(docs, removeWords, c("blabla1", "blabla2"))

#Supprimer la ponctuation:
docs = tm_map(docs, removePunctuation)

#Supprimerles espaces vides supplémentaires 
docs = tm_map(docs, stripWhitespace)

# Text stemming (= garder seulement les radicaux des mots, le résultat est un peu 
#bizarre (genre cities -> citi, ça semble poser problème pour certains mots))
docs = tm_map(docs, stemDocument)

inspect(docs) #Pour accéder au texte transformé

#____________________________________________________________________________
#  Construction de la matrice de mots 
#____________________________________________________________________________

# C'est une table contenant la fréquence des mots.
dtm = TermDocumentMatrix(docs) 
m = as.matrix(dtm)
v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq=v)
head(d, 10) # On fait apparaitre les 10 mots les plus fréquents ainsi que leur nombre d'apparitions

#____________________________________________________________________________
#  Construction du nuage de mots
#____________________________________________________________________________

set.seed(1234)
wordcloud(words = d$word, freq=d$freq, min.freq = 1,
          max.words=200, random.order = FALSE, rot.per = 0.35,
          color = brewer.pal(8, "Dark2"))

#____________________________________________________________________________
#  Autres fonctions intéressantes
#____________________________________________________________________________

# Explorer les termes les plus fréquents:

findFreqTerms(dtm, lowfreq = 4)

# Analyser l'association entre les mots (= leur corrélation = la distance entre eux dans le texte?):

findAssocs(dtm, terms = "vulner", corlimit = 0.8)


#Plotter la fréquence des 10 premiers mots:

barplot(d[1:10,]$freq, las=2, names.arg=d[1:10,]$word, 
        col = "lightblue", main = "Most frequent words", ylab = "Word frequencies")



