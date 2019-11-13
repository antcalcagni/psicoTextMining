normalize_locs = function(locs=NULL,preps.art=NULL,stpw=c("il","lo","la","i","gli","le","l","d","c","un","una","uno")){
  #0: normalize locs
  t0_vec = sapply(1:length(locs),function(x)tolower(locs[x]))
  
  #1: eliminiamo gli apostrofi da articoli e preposizioni articolate
  for(i in 1:length(t0_vec)){
    t0_vec[i] = paste(strsplit(x = t0_vec[i],split = "'",fixed = TRUE)[[1]],collapse = " ") 
  }
  
  #2: riduciamo prep articolate in prep semplici
  for(k in 1:length(t0_vec)){
    x = unlist(strsplit(t0_vec[k],split = " "))
    for(i in 1:NROW(preps.art)){ #preposizioni articolate -> semplici
      for(j in 1:NCOL(preps.art)){
        x[which(preps.art[i,j]==x)] = preps.art[i,1]
        t0_vec[k] = paste(x,collapse = " ")
      }
    }
  }
  
  #4: eliminiamo articoli e lettere vaganti
  for(k in 1:length(t0_vec)){
    x = unlist(strsplit(t0_vec[k],split = " "))
    for(i in 1:length(stpw)){ 
      x[x==stpw[i]] = ""
    }
    t0_vec[k] = paste(x,collapse = " ")
  }
  
  #5: stripWhitespace
  for(k in 1:length(t0_vec)){
    t0_vec[k] = tm::stripWhitespace(t0_vec[k])
  }
  
  return(t0_vec)
}


normalize_text = function(text=NULL,preps.art=NULL,stpw=c("il","lo","la","i","gli","le","l","d","c","un","una","uno"),locs=NULL,stemming=TRUE,prop.matching=1){
  
  #0: tolower su tutto il testo
  t0_vec = unlist(strsplit(x = text,split = " "))
  t0_vec = sapply(1:length(t0_vec),function(x)tolower(t0_vec[x]))
  
  #1: eliminiamo gli apostrofi da articoli e preposizioni articolate
  #t0_vec = unlist(strsplit(x = ,split = " "))
  for(i in 1:length(t0_vec)){
    t0_vec[i] = paste(strsplit(x = t0_vec[i],split = "'",fixed = TRUE)[[1]],collapse = " ") 
  }
  t0 = paste(t0_vec,collapse = " ")
  
  #2: riduciamo prep articolate in prep semplici
  t0_vec = unlist(strsplit(x = t0,split = " "))
  for(i in 1:NROW(preps.art)){ #preposizioni articolate -> semplici
    for(j in 1:NCOL(preps.art)){
      t0_vec[which(preps.art[i,j]==t0_vec)] = preps.art[i,1]
    }
  }
  t0 = paste(t0_vec,collapse = " ")
  
  #3: normalizziamo locuzioni e modi di dire
  source("find_replance_sentences.R") #non può contenere apostrofi
  t0 = find_replace_sentences(text = t0,locs = locs,stemming = stemming,prop.matching = prop.matching)
  
  #4: eliminiamo articoli e lettere vaganti
  t0_vec = unlist(strsplit(x = t0,split = " "))
  for(i in 1:length(stpw)){ 
    t0_vec[t0_vec==stpw[i]] = ""
  }
  t0 = paste(t0_vec,collapse = " ")
  return(t0)
}

find_replace_sentences = function(text=NULL,locs=NULL,stemming=TRUE,prop.matching=1){
  
  t1_vec = unlist(strsplit(x = text,split = " "))
  iid = sort(apply(as.matrix(locs),1,nchar),index.return=TRUE,decreasing=TRUE)$ix
  locs = locs[iid] #sort vector of locs
  
  for(j in 1:length(locs)){
    cat(".")
    locx = locs[j]
    locy = gsub(pattern = " ",replacement = "_",x = locx)
    
    loc_vec = unlist(strsplit(x = locx,split = " ",fixed = TRUE))
    loc_iid = which.max(apply(X = as.matrix(loc_vec),MARGIN = 1,FUN = nchar))[1]
    loc_tosearch = loc_vec[loc_iid]
    loc_tosearch_iid = which(t1_vec == loc_tosearch)
    steps = 1:length(loc_vec)-loc_iid
    
    if(length(loc_tosearch_iid)>0){
      for(i in 1:length(loc_tosearch_iid)){
        #print(i)
        x = loc_tosearch_iid[i]
        iid = x+steps
        tryCatch(
          {
            if(stemming==TRUE){
              x1 = quanteda::char_tolower(quanteda::char_wordstem(language = "ita",x=t1_vec[iid]))
              x2 = quanteda::char_wordstem(language = "ita",x=loc_vec)
            }else{
              x1 = quanteda::char_tolower(t1_vec[iid])
              x2 = loc_vec
            }
          },error = function(e){}
        )
        
        xmatch = round(sum(x1==x2)/length(x2),1)
        if(xmatch>=prop.matching){ 
          iid = iid[-which(iid==x)]
          t1_vec[iid] = ""
          t1_vec[x] = paste(loc_vec,collapse = "_")
        }
      }
    }
  }
  text1 = paste(t1_vec,collapse = " ")
  return(text1)
}


compute_communality = function(dfmx = NULL){
  m = NROW(dfmx)
  Y=as.matrix(dfmx); 
  Y[Y>1] = 1
  x = mapply(function(j)sum(Y[,j]==rep(1,m)),1:NCOL(Y))
  comun_overall = sum(x==m)/length(x)
  
  X = matrix(NA,m,m); rownames(X) = colnames(X) = paste0("t",rep(1:m))
  for(i in 1:m){
    for(j in 1:m){
      x = mapply(function(k)sum(Y[c(i,j),k]==rep(1,2)),1:NCOL(Y))
      X[i,j] = sum(x==2)/length(x[x>0])
    }
  }
  
  return(list(X=X,overall=comun_overall))
}


