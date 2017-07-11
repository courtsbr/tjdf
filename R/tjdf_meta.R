#' Function tjdf_meta
#'
#' This function retrieves the metadata related to the high court decisions according to a search.
#'
#' @param BuscaLivre a word or an expression to searched for.
#' @param quoted TRUE. It will treat an expression between quotes.
#' @keywords Courts, Decisions, Jurimetry, Webscraping
#' @export
#' @examples
#' tjdf_meta("dano moral coletivo", quoted=TRUE)

tjdf_meta<-function(BuscaLivre,quoted=TRUE){
  
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
  BuscaLivre<-str_replace_all(BuscaLivre,"\\s+","+")
  
  if(quoted==TRUE) BuscaLivre<-paste0("%22",BuscaLivre,"%22")
  
  url1<-"https://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?argumentoDePesquisa=indulto&visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=buscaLivre&comando=pesquisar&internet=1&camposSelecionados=ESPELHO&COMMAND=ok&quantidadeDeRegistros=20&tokenDePaginacao=1"
  
  url1<-urltools::param_set(url1,"argumentoDePesquisa",BuscaLivre)
  
  hits<-GET(url1) %>% 
    content("parsed") %>% 
    xml_find_all("//*[@class='conteudoComRotulo']") %>%
    xml_text(trim=T) %>% 
    .[3] %>% 
    as.numeric()
  
  paginas<-hits %>% 
    '/'(20) %>% 
    ceiling()
  
  url2 <-"http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=buscaLivre2&buscaPorQuery=1&baseSelecionada=BASE_ACORDAO_TODAS&ramoJuridico=&baseDados=[BASE_ACORDAOS,%20TURMAS_RECURSAIS,%20BASE_ACORDAO_PJE,%20BASE_HISTORICA]&argumentoDePesquisa=indulto&desembargador=&indexacao=&tipoDeNumero=&tipoDeRelator=&camposSelecionados=[ESPELHO]&numero=&tipoDeData=&dataFim=&dataInicio=&ementa=&orgaoJulgador=&legislacao=&baseSelecionada=BASE_ACORDAO_TODAS&numeroDaPaginaAtual=&quantidadeDeRegistros=20&totalHits=" %>% 
    urltools::param_set("argumentoDePesquisa",BuscaLivre) %>% 
    urltools::param_set("totalHits",hits)
  
  df<-tibble()
  for(i in 1:paginas){
    tryCatch({
      url2<-urltools::param_set(url2,"numeroDaPaginaAtual",i)
      
      codigo<-url2 %>% 
        GET() %>% 
        content() %>% 
        xml_find_all("//*[@id='id_link_abrir_dados_acordao']") %>% xml_text()
      
      url3<-paste0("https://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&controladorId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.ControladorBuscaAcordao&visaoAnterior=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=resultado&comando=abrirDadosDoAcordao&enderecoDoServlet=sistj&historicoDePaginas=buscaLivre&quantidadeDeRegistros=20&baseSelecionada=BASE_ACORDAO_TODAS&numeroDaUltimaPagina=1&buscaIndexada=1&mostrarPaginaSelecaoTipoResultado=false&totalHits=1&internet=1&numeroDoDocumento=",codigo)
      
      b1<-map(url3,function(x){
        x %>% GET() %>% 
          content()
        
      })
      
      processo<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Classe')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
            str_trim()
        })
      
      registro<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Registro')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
            str_trim()
        })
      
      data_julgamento<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Julgamento')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
            str_trim()
        })
      
      orgao_julgador<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Julgador')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
            str_trim()
        })
      
      
      relator<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Relator:')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
          str_trim()
        })
      
      data_publicacao<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Intimação')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
            str_extract("\\d+/\\d+/\\d+")
        })
      
      
      
      decisao<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Decisão')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
            str_trim()
        })
      
      ementa<- b1 %>%
        map_chr(function(x){
          x %>% xml_find_all("//*[@class='rotulo'][contains(.,'Ementa')]/ancestor::div[1]/div[@class='conteudoComRotulo']") %>% 
            xml_text() %>% 
            str_trim()
        })
      
      df1<-tibble(processo,registro,data_julgamento,data_publicacao,relator,orgao_julgador,ementa, decisao,pagina=i)
      df<-rbind(df,df1)
    }, error=function(m){
      m
    }, finally={
      next
    })
    
  }
  return(df)
}



