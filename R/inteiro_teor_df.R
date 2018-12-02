#' Function inteiro_teor_df
#'
#' This function downloads the pdfs from Brazilian Distrito Federal second degreee decisions.
#'
#' @param registro String of registro's numbers to search for. You get this number by using the tjdf_meta function.
#' @keywords Courts, Decisions, Jurimetry, Webscraping
#' @export
#' @examples
#' registro<-1005250
#' inteiro_teor_df(registro)


inteiro_teor_df<-function(acordao){

for(i in seq_along(acordao)){
  tryCatch({
    
u <- "http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&controladorId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.ControladorBuscaAcordao&visaoAnterior=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=resultado&comando=abrirDadosDoAcordao&enderecoDoServlet=sistj&historicoDePaginas=buscaLivre&quantidadeDeRegistros=20&baseSelecionada=BASE_ACORDAO_TODAS&numeroDaUltimaPagina=1&buscaIndexada=1&mostrarPaginaSelecaoTipoResultado=false&totalHits=1&internet=1&numeroDoDocumento="
url<-paste0(u,acordao[i])
# usei html session pra economizar codigo de escrever o form inteiro
s <- rvest::html_session(url)
form <- rvest::html_form(s)[[1]] %>% 
  rvest::set_values(comando = 'downloadInteiroTeor')
s <- rvest::submit_form(s, form)

# pulo do gato tá aqui: o download vem de uma GET request com um codigo gerado na pagina da resposta do form
idd <- s$response %>% 
  httr::content('text') %>% 
  stringr::str_extract('idd=[0-9a-zA-Z]+')

# agora é só correr pro abraço
u_teor <- paste0(
  "http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/infra/Download.jsp?",
  idd
)

binario<-httr::GET(u_teor)

ifelse(binario$headers$`content-type`=="application/msword",
       writeBin(binario$content,paste0(acordao[i],".doc")),
       writeBin(binario$content,paste0(acordao[i],".pdf")))
  }, error = function(m) {
    m
  }, finally = {
    next
  })
}
}
