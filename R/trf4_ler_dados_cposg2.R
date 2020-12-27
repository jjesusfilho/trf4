#' Ler dados da consulta processual do 2º grau do TRF4
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório onde se encontram os arquivos
#'
#' @return tibble
#' @export

trf4_ler_dados_cposg2 <- function (arquivos = NULL, diretorio = ".")
{

  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, pattern = "html$",
                           full.names = TRUE)
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    id <- stringr::str_extract(.x, "TRF4\\d+")

    x <- xml2::read_html(.x)

    var <- x %>%
      xml2::xml_find_all("//strong[contains(.,'Assuntos:')]/preceding-sibling::strong") %>%
      xml2::xml_text()

    val <- x %>%
      xml2::xml_find_all("//strong[contains(.,'Assuntos:')]/preceding-sibling::strong/following-sibling::text()[1]") %>%
      xml2::xml_text()

    val <- c(var[1],val)

    var[1]<-"classe_processo"

    val[2]<- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Origin\u00e1rio')]/following-sibling::a") %>%
      xml2::xml_text(trim = T) %>%
      .[1]

    assunto <- x %>% xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Assuntos')]/following-sibling::hr[1]") %>%
      xml2::xml_find_all(".//preceding-sibling::text()") %>%
      xml2::xml_text(trim = T) %>% stringr::str_subset("^\\d+[.]\\s") %>%
      stringr::str_c(collapse = "\n")


    tibble::tibble(id, assunto, variavel = var, valor = val) %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>% dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id) %>%
      janitor::clean_names()


  },NULL))

}
