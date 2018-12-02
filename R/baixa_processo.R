#' Baixa processo de primeira instância do TJDFT
#'
#' @param x número unificado do processo (20 dígitos) com ou sem separador.
#' @param diretorio Default para o atual.
#'
#' @return Baixa as páginas html.
#' @export
#'
#' @examples
baixar_processo <- function(x,diretorio="."){
  
  x <- abjutils::clean_id(x)
  
  arquivos <- paste0(format(Sys.Date(),"%Y%m%d_"),x,".html")
  
  urls <- paste0("http://www.tjdft.jus.br/submitConsulta?url=http%3A%2F%2Fcache-internet.tjdft.jus.br%2Fcgi-bin%2Ftjcgi1&NXTPGM=tjhtml101&ORIGEM=INTER&SELECAO=1&CIRC=ZZ&CHAVE=",x,"&CHAVE1=&submit=ok")
  
  furrr::future_map2(urls,x,possibly(~{
    
    httr::GET(.x) %>% 
      httr::content("text") %>% 
      stringr::str_extract("(?<=URL\\=)\\X+\\d{10,}") %>% 
      httr::GET(httr::write_disk(paste0(format(Sys.Date(),"%Y%m%d_"),.y,".html"),overwrite = TRUE))
    
  },NULL),.progress=TRUE)
  
}