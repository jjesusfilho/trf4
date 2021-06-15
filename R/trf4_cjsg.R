#' Obtêm os ids dos processos a partir da busca livre
#'
#' @param livre Busca livre.
#' @param data_inicial Data inicial no formato dd/mm/yyyy
#' @param data_final Data final no formato dd/mm/yyyy
#' @param tipo_data Buscar por data de decisão "DEC" ou data de
#'     publicação "PUB".
#'
#' @details Após obter os ids, use a função trf4_baixar_cjsg para 
#'     baixar por ids. Limite a busca por data inicial e final
#'
#' @return Vetor de ids
#' @export
#'
trf4_cjsg <- function (livre = "",
                                 data_inicial = "",
                                 data_final = "",
                                 tipo_data  = c("DEC","PUB")
                       )
{
 
  
  tipo_data <- tipo_data[[1]] %>% 
               toupper()

  
  url <- "https://jurisprudencia.trf4.jus.br/pesquisa/resultado_pesquisa.php"
  
  body <- list(rdoTipo = "1", rdoCampoPesquisa = "I", textoPesqLivre = livre, 
               chkAcordaos = "on", chkDecMono = "on", numProcesso = "", 
               cboRelator = "", dataIni = data_inicial, dataFim = data_final, 
               tipodata = tipo_data, docsPagina = "20", hdnAcao = "nova_pesquisa", 
               arrclasses = "", arrorgaos = "")
  

   
  conteudo <- httr::POST(url, body = body, encode = "form") %>% 
              httr::content()
  
  excesso <- conteudo %>% 
    xml2::xml_find_first("boolean(//b[contains(text(),'Foram encontrados')])")
  
  if (excesso){
    
     conteudo %>% 
      xml2::xml_find_first("//b[contains(text(),'Foram encontrados')]") %>% 
      xml2::xml_text() %>% 
      paste0(" Delimite a busca por data") %>% 
      return()
    
  }
  
  ids <- conteudo %>% 
        xml2::xml_find_first("//input[@id='vetPaginacao']") %>% 
        xml2::xml_attr("value") %>% 
        stringr::str_split("[,#]") %>% 
        unlist()
 
  ids 
}
  
  