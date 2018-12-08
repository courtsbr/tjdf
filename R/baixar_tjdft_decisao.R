#' Baixar inteiro teor da consulta jurisprudencia do TJDFT
#'
#' Baixa inteiro teor dos acórdaos
#'
#' @param registro String of registro's numbers to search for. You get this number by using the tjdf_meta function.
#' @keywords Courts, Decisions, Jurimetry, Webscraping
#' @export

baixar_tjdft_decisao<-function(registro){

for(i in seq_along(registro)){
  tryCatch({

u <- "http://pesquisajuris.tjdft.jus.br/Indexadorregistros-web/sistj?visaoId=tjdf.sistj.registroeletronico.buscaindexada.apresentacao.VisaoBuscaregistro&controladorId=tjdf.sistj.registroeletronico.buscaindexada.apresentacao.ControladorBuscaregistro&visaoAnterior=tjdf.sistj.registroeletronico.buscaindexada.apresentacao.VisaoBuscaregistro&nomeDaPagina=resultado&comando=abrirDadosDoregistro&enderecoDoServlet=sistj&historicoDePaginas=buscaLivre&quantidadeDeRegistros=20&baseSelecionada=BASE_registro_TODAS&numeroDaUltimaPagina=1&buscaIndexada=1&mostrarPaginaSelecaoTipoResultado=false&totalHits=1&internet=1&numeroDoDocumento="
url<-paste0(u,registro[i])
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
  "http://pesquisajuris.tjdft.jus.br/Indexadorregistros-web/infra/Download.jsp?",
  idd
)

binario<-httr::GET(u_teor)

ifelse(binario$headers$`content-type`=="application/msword",
       writeBin(binario$content,paste0(registro[i],".doc")),
       writeBin(binario$content,paste0(registro[i],".pdf")))
  }, error = function(m) {
    m
  }, finally = {
    next
  })
}
}
