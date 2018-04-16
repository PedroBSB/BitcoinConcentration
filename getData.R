#install.packages("httr")

library(httr)

#install.packages("jsonlite")

library(jsonlite)

#install.packages("rjson")

library("rjson")
install.packages("googledrive")
library(googledrive)
library(Rcurl)
library(RJSONIO)
library(data.table)

base_dados_blockexplorer <- drive_get(as_id("1LMQf-itbULwKX6VLBA_cKTHLpQBG92Qn"))
base_dados_blockexplorer<- readRDS("base_dados_blockexplorer.rds")
arquivo_google_com_blocos<- drive_get(as_id("1QKoYlImQhb4sx3OL26ZrNEM9lnsUsiI-"))
arquivo_google_com_blocos<- readRDS("arquivo_google_com_blocos.rds")

############# TOKEN 
Token <- "245432532532"

header_type <- "applcation/json"

full_token <- paste0("Bearer ", Token)

####### BLOCOS QUE O DEVEMOS PEGAR.

blocos <- "https://blockexplorer.com/api/blocks"

blocos_padrao <- GET(blocos, add_headers(Authorization = full_token, Accept = header_type), timeout(120), verbose())

blocos_texto <- content(blocos_padrao, type = 'text', encoding = "UTF-8")

blocos_finais <- fromJSON(blocos_texto)

data_frames_finais <- NULL

transacoes_finais_i <- NULL


i <- 1
for(i in 1:lengths(blocos_finais)[1]){
  if(!(blocos_finais$blocks[[i]]$height %in% arquivo_google_com_blocos$bloco)){
    
    blocoi <- paste0("https://blockexplorer.com/api/block/",blocos_finais$blocks[[i]]$hash)
    
    blocoi_transacoes <- GET(blocoi, add_headers(Authorization = full_token, Accept = header_type), timeout(120), verbose())
    
    text_json <- content(blocoi_transacoes, type = 'text', encoding = "UTF-8")
    
    bloco_i_final <- fromJSON(text_json)
    
    for(j in 4:lengths(bloco_i_final)[6]){
      tx_i <- paste0("https://blockexplorer.com/api/tx/",bloco_i_final$tx[j])
      
      
      #while(tx_i_final){
      txi <- GET(tx_i, add_headers(Authorization = full_token, Accept = header_type), timeout(120), verbose())
      
      
      conteudo <- content(txi, type = 'text', encoding = "UTF-8")
      
      tryCatch({
        tx_i_final <- rjson::fromJSON(getURL(tx_i))
        
        
        #}
        
        #tx_i_final  <- transpose(as.data.frame(unlist(tx_i_final)))
        tx_i <- cbind(bloco_i_final$height,tx_i_final$time,tx_i_final$txid,ifelse(is.null(tx_i_final$valueOut),NA,tx_i_final$valueOut),
                      ifelse(is.null(tx_i_final$valueIn),NA,tx_i_final$valueIn),
                      ifelse(length(tx_i_final$vin)>=1 ,ifelse(is.null(tx_i_final$vin[[1]]$addr),NA,tx_i_final$vin[[1]]$addr),NA),
                      ifelse(length(tx_i_final$vin)>=1 ,ifelse(is.null(tx_i_final$vin[[1]]$value),NA,tx_i_final$vin[[1]]$value),NA),
                      ifelse(length(tx_i_final$vin)>=2 ,ifelse(is.null(tx_i_final$vin[[2]]$addr),NA,tx_i_final$vin[[2]]$addr),NA),
                      ifelse(length(tx_i_final$vin)>=2, ifelse(is.null(tx_i_final$vin[[2]]$value),NA,tx_i_final$vin[[2]]$value),NA),
                      ifelse(length(tx_i_final$vin)>=3 ,ifelse(is.null(tx_i_final$vin[[3]]$addr),NA,tx_i_final$vin[[3]]$addr),NA),
                      ifelse(length(tx_i_final$vin)>=3 ,ifelse(is.null(tx_i_final$vin[[3]]$value),NA,tx_i_final$vin[[3]]$value),NA),
                      ifelse(length(tx_i_final$vin)>=4 ,ifelse(is.null(tx_i_final$vin[[4]]$addr),NA,tx_i_final$vin[[4]]$addr),NA),
                      ifelse(length(tx_i_final$vin)>=4 ,ifelse(is.null(tx_i_final$vin[[4]]$value),NA,tx_i_final$vin[[4]]$value),NA),
                      ifelse(length(tx_i_final$vin)>=5 ,ifelse(is.null(tx_i_final$vin[[5]]$addr),NA,tx_i_final$vin[[5]]$addr),NA),
                      ifelse(length(tx_i_final$vin)>=5 ,ifelse(is.null(tx_i_final$vin[[5]]$value),NA,tx_i_final$vin[[5]]$value),NA),
                      ifelse(length(tx_i_final$vin)>=6 ,ifelse(is.null(tx_i_final$vin[[6]]$addr),NA,tx_i_final$vin[[6]]$addr),NA),
                      ifelse(length(tx_i_final$vin)>=6 ,ifelse(is.null(tx_i_final$vin[[6]]$value),NA,tx_i_final$vin[[6]]$value),NA),
                      ifelse(length(tx_i_final$vin)>=7 ,ifelse(is.null(tx_i_final$vin[[7]]$addr),NA,tx_i_final$vin[[7]]$addr),NA),
                      ifelse(length(tx_i_final$vin)>=7 ,ifelse(is.null(tx_i_final$vin[[7]]$value),NA,tx_i_final$vin[[7]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=1 ,ifelse(is.null(tx_i_final$vout[[1]]$value),NA,tx_i_final$vout[[1]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=1 ,ifelse(is.null(tx_i_final$vout[[1]]$scriptPubKey$addresses),NA,tx_i_final$vout[[1]]$scriptPubKey$addresses),NA),
                      ifelse(length(tx_i_final$vou)>=2 ,ifelse(is.null(tx_i_final$vout[[2]]$value),NA,tx_i_final$vout[[2]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=2 ,ifelse(is.null(tx_i_final$vout[[2]]$scriptPubKey$addresses),NA,tx_i_final$vout[[2]]$scriptPubKey$addresses),NA),
                      ifelse(length(tx_i_final$vou)>=3 ,ifelse(is.null(tx_i_final$vout[[3]]$value),NA,tx_i_final$vout[[3]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=3 ,ifelse(is.null(tx_i_final$vout[[3]]$scriptPubKey$addresses),NA,tx_i_final$vout[[3]]$scriptPubKey$addresses),NA),
                      ifelse(length(tx_i_final$vou)>=4 ,ifelse(is.null(tx_i_final$vout[[4]]$value),NA,tx_i_final$vout[[4]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=4 ,ifelse(is.null(tx_i_final$vout[[4]]$scriptPubKey$addresses),NA,tx_i_final$vout[[4]]$scriptPubKey$addresses),NA),
                      ifelse(length(tx_i_final$vou)>=5 ,ifelse(is.null(tx_i_final$vout[[5]]$value),NA,tx_i_final$vout[[5]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=5 ,ifelse(is.null(tx_i_final$vout[[5]]$scriptPubKey$addresses),NA,tx_i_final$vout[[5]]$scriptPubKey$addresses),NA),
                      ifelse(length(tx_i_final$vou)>=6 ,ifelse(is.null(tx_i_final$vout[[6]]$value),NA,tx_i_final$vout[[6]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=6 ,ifelse(is.null(tx_i_final$vout[[6]]$scriptPubKey$addresses),NA,tx_i_final$vout[[6]]$scriptPubKey$addresses),NA),
                      ifelse(length(tx_i_final$vou)>=7 ,ifelse(is.null(tx_i_final$vout[[7]]$value),NA,tx_i_final$vout[[7]]$value),NA),
                      ifelse(length(tx_i_final$vou)>=7 ,ifelse(is.null(tx_i_final$vout[[7]]$scriptPubKey$addresses),NA,tx_i_final$vout[[7]]$scriptPubKey$addresses),NA))
        
        
        
        transacoes_finais_i <- rbind(transacoes_finais_i,tx_i)
        
        #############salva onde parou ###################
        print(i)
        print(j)
        Sys.sleep(0.4)
      },error = function(err) {
        #Print the error:
        print(paste("MY_ERROR:  ",err))
        
        #Decrease your counter to try again
        j<- (j-1)
        
        #Wait some time before try again...
        Sys.sleep(2) 
      })
    } 
    print("ATUALIZANDO BLOCOS RASPADOS E BANCO DE DADOS NO GOOGLE")
    #ATUALIZANDO OS BLOCOS FEITOS
    arquivo_google_com_blocos<- rbind(arquivo_google_com_blocos,c(bloco_i_final$height,lengths(bloco_i_final)[6],j))
    saveRDS(arquivo_google_com_blocos,"arquivo_google_com_blocos.rds")
    drive_update(as_id("1QKoYlImQhb4sx3OL26ZrNEM9lnsUsiI-"),media="arquivo_google_com_blocos.rds")
    
    transacoes_finais_i<- as.data.frame(transacoes_finais_i)
    names(transacoes_finais_i) <- c("bloco","tempo","id_transicao","Valor_saida","Valor_entrada","id_recebedor_a","qnt_recebedor_a",
                                    "id_recebedor_b","qnt_recebedor_b","id_recebedor_c","qnt_recebedor_c","id_recebedor_d","qnt_recebedor_d",
                                    "id_recebedor_e","qnt_recebedor_e","id_recebedor_f","qnt_recebedor_f","id_recebedor_g","qnt_recebedor_g",
                                    "qnt_pagador_a","id_pagador_a","qnt_pagador_b","id_pagador_b","qnt_pagador_c","id_pagador_c",
                                    "qnt_pagador_d","id_pagador_d","qnt_pagador_e","id_pagador_e","qnt_pagador_f","id_pagador_f","qnt_pagador_g","id_pagador_g")
    
    base_dados_blockexplorer<- rbind(base_dados_blockexplorer,transacoes_finais_i)
    saveRDS(base_dados_blockexplorer,"base_dados_blockexplorer.rds")
    drive_update(as_id("1LMQf-itbULwKX6VLBA_cKTHLpQBG92Qn"),media="base_dados_blockexplorer.rds")
    
    
    ### lembrando o nome de todos data.frames criados por blocos 
  }else{
    print("Bloco coletado")
  }  
  # data_frames_finais <-c(data_frames_finais,paste("transacoes_finais_bloco_",blocos_finais$blocks[[i]]$height, sep="")) 
}



