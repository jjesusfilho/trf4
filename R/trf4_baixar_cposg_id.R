#' Baixar cposg conforme id
#'
#' @param id Id do processo. Pode ser completo ou seomente os números.
#' @param diretorio Diretório
#'
#' @details Há uma sequência de ids que podem ser baixados. Vc pode formá-la, se quiser.
#'
#' @return htmls
#' @export
#'
trf4_baixar_cposg_id <- function(id = NULL, diretorio = "."){


    pb <- progress::progress_bar$new(total = length(id))

  purrr::walk(id,purrr::possibly(~{


    pb$tick()

    if (str_detect(.x,"TRF4",negate=TRUE)){

    .x <- .x %>%
      stringr::str_pad(8,"left","0") %>%
      stringr::str_c("TRF4",.)
    }


    url <- "https://jurisprudencia.trf4.jus.br/pesquisa/resultado_pesquisa.php"

    body <- list(
      parcial = "aqui",
      pesquisaLivre = "",
      registrosSelecionados = "",
      vetPaginacao = .x,
      paginaAtual = "0",
      totalRegistros = "",
      rdoCampoPesquisa = "I",
      chkAcordaos = "on",
      chkDecMono = "on",
      textoPesqLivre = "",
      chkDocumentosSelecionados = "",
      numProcesso = "",
      cboRelator = "",
      dataIni = "",
      dataFim = "",
      tipodata = "DEC",
      docsPagina = "",
      arrorgaos = "",
      arrclasses = "",
      hdnAcao = "",
      hdnTipo = "1"
    )


  url1 <-  httr::POST(url,body= body, encode="form") %>%
         httr:content() %>%
    xml2::xml_find_first("//td[contains(b,'Processo:')]/a") %>%
    xml2::xml_attr("href")



    arquivo <- file.path(diretorio,paste0("cposg_",.x,".html"))

    httr::GET(url1, httr::write_disk(arquivo,overwrite = TRUE))


  },NULL))

}
