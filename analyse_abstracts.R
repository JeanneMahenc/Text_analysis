library("tm")

filepath = getwd()
liste_textes = dir(paste(getwd(), "/textes_a_analyser", sep=""))


for(file in liste_textes){
 # text = ""
 # docs = Corpus(VectorSource(text))
  texte = readLines(paste(getwd(),"/textes_a_analyser/",file, sep=""))
  docs = Corpus(VectorSource(texte))
  
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
  
  dtm = TermDocumentMatrix(docs) 
  m = as.matrix(dtm)
  v = sort(rowSums(m), decreasing = TRUE)
  d = data.frame(word = names(v), freq=v)
  x = paste("freq_words_", file, sep="")
  value = head(d,10)
  eval(call("<-", as.name(x), value))
  
}
