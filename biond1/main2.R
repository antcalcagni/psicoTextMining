# Spazio di lavoro --------------------------------------------------------
rm(list=ls())
setwd("~/Dropbox (unipd)/TESI/Biondini_Arianna/")
library(quanteda); library(tm); library(RDRPOSTagger); library(TextWiller);library(topicmodels); library(ldatuning)
source("functions.R")


# Caricamento dei testi ---------------------------------------------------
## testi
t1<-iconv(x = readLines(con = paste0(getwd(),"/t1.txt"),ok = TRUE,encoding = "windows-1252"),from = "windows-1252",to = "UTF-8")
t2<-iconv(readLines(con = paste0(getwd(),"/t2.txt"),ok = TRUE,encoding = "windows-1252"),from = "windows-1252",to = "UTF-8")
t3<-iconv(readLines(con = paste0(getwd(),"/t3.txt"),ok = TRUE,encoding = "windows-1252"),from = "windows-1252",to = "UTF-8")
t4<-iconv(readLines(con = paste0(getwd(),"/t4.txt"),ok = TRUE,encoding = "windows-1252"),from = "windows-1252",to = "UTF-8")

## locuzioni e modi di dire
locs<-iconv(x = readLines(con = paste0(getwd(),"/modi_di_dire_italiani_e_locuzioni.txt"), ok = TRUE,encoding = "windows-1252"),from = "windows-1252",to = "UTF-8") 

## preposizioni articolate
prep_artic = as.matrix(read.table(file = paste0(getwd(),"/preposizioni_artic.csv"),encoding = "UTF-8",header = FALSE,sep = ","))

## stopwords specifiche
stpw_specific = iconv(readLines(con = paste0(getwd(),"/stopwords specifiche.txt"), ok = TRUE,encoding = "windows-1252"),from="windows-1252",to="UTF-8")
stpw_specific = normalize_text(stpw_specific,preps.art = prep_artic,locs = locs)
stpw_specific = unlist(strsplit(stpw_specific,split = " "))


# Preparazione dei testi --------------------------------------------------
## normalizza le locuzioni
locs = normalize_locs(locs = locs,preps.art = prep_artic)

## normalizza i testi
t1 = normalize_text(text = t1,preps.art = prep_artic,locs = locs)
t2 = normalize_text(text = t2,preps.art = prep_artic,locs = locs)
t3 = normalize_text(text = t3,preps.art = prep_artic,locs = locs)
t4 = normalize_text(text = t4,preps.art = prep_artic,locs = locs)

## creazione del corpus finale
corpora = corpus(texts(x = c(t1,t2,t3,t4)))
summary(corpora)

## stop-words da eliminare dai testi
stpw<-stopwords(kind = "ita")
stpw_tot<-c(stpw,stpw_specific)

# eliminazione delle stopwords (fixed matching among tokens and stop-words)
corpora_tok<-tokens_select(x = tokens(corpora,remove_numbers=TRUE,remove_punct=TRUE), pattern = stpw_tot,verbose=TRUE,padding=FALSE,selection = "remove",valuetype = "fixed")


# DFM e dizionari ---------------------------------------------------------
dfm_t1_t4 <- dfm(corpora_tok)
dfm_t1_t4[, 1:5]

freq.terms = data.frame(apply(dfm_t1_t4,2,sum),row.names = NULL)
dictionary = data.frame(colnames(dfm_t1_t4),freq.terms); names(dictionary) = c("term","freq") #dictionary of corpora
dictionary = dictionary[order(dictionary$freq,decreasing = TRUE),] #sort dictionary

dictionaries = list()
for(i in 1:4){
  x = as.vector(dfm_t1_t4[i,])
  freq.terms = data.frame(x[x>0],row.names = NULL)
  dictionaries[[i]] = data.frame(colnames(dfm_t1_t4[i,])[x>0],freq.terms); names(dictionaries[[i]]) = c("term","freq") #dictionary of corpora
  dictionaries[[i]] = dictionaries[[i]][order(dictionaries[[i]]$freq,decreasing = TRUE),] #sort dictionary
  colnames(dictionaries[[i]]) = c("terms","freq")
}



# Stats descrittive su DFM ------------------------------------------------
ttrs = textstat_lexdiv(x = dfm_t1_t4)
length_texts = sapply(X = dictionaries,FUN=nrow)
hapaxs = mapply(function(i)sum(dictionaries[[i]]$freq==1)/length_texts[i],1:4)

## POS distinto per testo
t1_pos = rdr_pos(x = paste(corpora_tok$text1,collapse = " "),object = rdr_model(language = "Italian", annotation = "UniversalPOS"))
t2_pos = rdr_pos(x = paste(corpora_tok$text2,collapse = " "),object = rdr_model(language = "Italian", annotation = "UniversalPOS"))
t3_pos = rdr_pos(x = paste(corpora_tok$text3,collapse = " "),object = rdr_model(language = "Italian", annotation = "UniversalPOS"))
t4_pos = rdr_pos(x = paste(corpora_tok$text4,collapse = " "),object = rdr_model(language = "Italian", annotation = "UniversalPOS"))

## POS su dizionario complessivo
t1_t4_pos = rdr_pos(x = paste(dictionary$term,collapse = " "),object = rdr_model(language = "Italian", annotation = "UniversalPOS"))

pos_texts = matrix(NA,4,3)
pos_texts[1,1] = sum(t1_pos$pos=="VERB")/max(t1_pos$token_id); pos_texts[1,2] = sum(t1_pos$pos=="ADJ")/max(t1_pos$token_id); pos_texts[1,3] = sum(t1_pos$pos=="NOUN")/max(t1_pos$token_id)
pos_texts[2,1] = sum(t2_pos$pos=="VERB")/max(t2_pos$token_id); pos_texts[2,2] = sum(t2_pos$pos=="ADJ")/max(t2_pos$token_id); pos_texts[2,3] = sum(t2_pos$pos=="NOUN")/max(t2_pos$token_id)
pos_texts[3,1] = sum(t3_pos$pos=="VERB")/max(t3_pos$token_id); pos_texts[3,2] = sum(t3_pos$pos=="ADJ")/max(t3_pos$token_id); pos_texts[3,3] = sum(t3_pos$pos=="NOUN")/max(t3_pos$token_id)
pos_texts[4,1] = sum(t4_pos$pos=="VERB")/max(t4_pos$token_id); pos_texts[4,2] = sum(t4_pos$pos=="ADJ")/max(t4_pos$token_id); pos_texts[4,3] = sum(t4_pos$pos=="NOUN")/max(t4_pos$token_id)
colnames(pos_texts) = c("VERB","ADJ","NOUN")

## Sentiment Analysis
sa_texts = matrix(NA,4,2); colnames(sa_texts) = c("neg","pos")
sa_texts[1,] = table(TextWiller::sentiment(t1_pos$token[t1_pos$pos=="ADJ"]))[c(1,3)]
sa_texts[2,] = table(TextWiller::sentiment(t2_pos$token[t1_pos$pos=="ADJ"]))[c(1,3)]
sa_texts[3,] = table(TextWiller::sentiment(t3_pos$token[t3_pos$pos=="ADJ"]))[c(1,3)]
sa_texts[4,] = table(TextWiller::sentiment(t4_pos$token[t4_pos$pos=="ADJ"]))[c(1,3)]
sa_texts = prop.table(sa_texts,margin = 1)

plot(sa_texts[,1],type="b",bty="n",col=2,ylim=c(0,1),ylab="SA%"); points(sa_texts[,2],type="b",bty="n",col=4)

## Dataframe di sintesi
stats_texts = data.frame(txt=paste0("t",rep(1:4)),lengths=length_texts,hapax=hapaxs,ttr=ttrs$TTR,pos_texts,sa_texts)
print(stats_texts)

## Comunalità e unicità del dizionario
comun_texts = compute_communality(dfmx = dfm_t1_t4)
print(comun_texts)

## Comunalità rispetto a aggettivi e verbi
dfm_t1_t4_verb = dfm_keep(x=dfm_t1_t4, pattern=t1_t4_pos$token[t1_t4_pos$pos=="VERB"])
dfm_t1_t4_adj = dfm_keep(x=dfm_t1_t4, pattern=t1_t4_pos$token[t1_t4_pos$pos=="ADJ"])

comun_texts_verb = compute_communality(dfmx = dfm_t1_t4_verb)
print(comun_texts_verb)

comun_texts_adj = compute_communality(dfmx = dfm_t1_t4_adj)
print(comun_texts_adj)



# Ridefinizione DFM -------------------------------------------------------
# eliminiamo parole non interpretatibili (es.: avverbi, pronomi)
dfm_t1_t4_refined = dfm_keep(x=dfm_t1_t4, pattern=t1_t4_pos$token[t1_t4_pos$pos%in%c("VERB","ADJ","NOUN","PROPN","NUM")])

# riduciamo la sparsità
dfm_t1_t4_trimmed = dfm_trim(dfm_t1_t4_refined, min_termfreq = 20)



# Wordcloud ---------------------------------------------------------------

# complessivo
x11();textplot_wordcloud(dfm_t1_t4_trimmed, min_count = 8, random_order = FALSE, rotation = .25, color = c("deepskyblue","dodgerblue3","darkblue"))

# per singoli testi
x11();textplot_wordcloud(dfm_t1_t4_trimmed[1,], min_count = 8, random_order = FALSE, rotation = .25, color = c("deepskyblue","dodgerblue3","darkblue")) # t1
x11();textplot_wordcloud(dfm_t1_t4_trimmed[2,], min_count = 8, random_order = FALSE, rotation = .25, color = c("deepskyblue","dodgerblue3","darkblue")) # t2
x11();textplot_wordcloud(dfm_t1_t4_trimmed[3,], min_count = 8, random_order = FALSE, rotation = .25, color = c("deepskyblue","dodgerblue3","darkblue")) # t3
x11();textplot_wordcloud(dfm_t1_t4_trimmed[4,], min_count = 8, random_order = FALSE, rotation = .25, color = c("deepskyblue","dodgerblue3","darkblue")) # t4

# comparativo
x11();textplot_wordcloud(dfm_t1_t4_trimmed, min_count = 8, random_order = FALSE, rotation = .25, color = c("deepskyblue","dodgerblue3","darkblue"),comparison = TRUE)


# CA ----------------------------------------------------------------------
ca_t1_t4 = textmodel_ca(x = dfm_t1_t4_trimmed)
print(ca_t1_t4)

#quanteda::textmodel_lsa(x = dfm_t1_t4_trimmed) #lsa??

# Varianza spiegata dalla edue dimensioni: 80% circa (ok)
factoextra::get_eigenvalue(ca_t1_t4) 

dat_ca <- data.frame(dim1 = coef(ca_t1_t4, doc_dim = 1)$coef_document, 
                    dim2 = coef(ca_t1_t4, doc_dim = 2)$coef_document)

dat_ca2 <- data.frame(dim1 = ca_t1_t4$colcoord[,1], dim2 = ca_t1_t4$colcoord[,2])


plot(1, xlim = c(-3, 3), ylim = c(-3, 3), type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2',bty="n"); grid(); abline(h = 0,v = 0,lty=2)
text(dat_ca2$dim1, dat_ca2$dim2, labels = rownames(dat_ca2), cex = 0.8, col = rgb(0, 0, 0, 0.7))
text(dat_ca$dim1, dat_ca$dim2, labels = paste0("t",rep(1:4)), cex = 1, col = 2)





# Associazione tra parole -------------------------------------------------
kws = c("testosterone","barba","operazione","chirurgo","madre","problema","nuovo","cambiare","coming_out")
#dfm_t1_t4_tm = convert(dfm_t1_t4_trimmed,to="tm")
#kws_cors = tm::findAssocs(terms = kws,x = dfm_t1_t4_tm,corlimit=0.5)

text_topfeat = names(topfeatures(dfm_t1_t4_trimmed, n=80)) 
text_topfeat = c(text_topfeat,kws);
text_topfeat = unique(text_topfeat)

g1 = textplot_network(fcm_select(fcm(dfm_t1_t4_trimmed[1,]), pattern = text_topfeat), edge_alpha = 1,omit_isolated = TRUE,edge_color = "gray",min_freq = 800)
g2 = textplot_network(fcm_select(fcm(dfm_t1_t4_trimmed[2,]), pattern = text_topfeat), edge_alpha = 1,omit_isolated = TRUE,edge_color = "gray",min_freq = 800)
g3 = textplot_network(fcm_select(fcm(dfm_t1_t4_trimmed[3,]), pattern = text_topfeat), edge_alpha = 1,omit_isolated = TRUE,edge_color = "gray",min_freq = 800)
g4 = textplot_network(fcm_select(fcm(dfm_t1_t4_trimmed[4,]), pattern = text_topfeat), edge_alpha = 1,omit_isolated = TRUE,edge_color = "gray",min_freq = 800)

x11(); cowplot::plot_grid(g1,g2,g3,g4,labels = paste0("t",1:4))


