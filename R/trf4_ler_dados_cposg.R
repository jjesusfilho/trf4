#' Ler dados da consulta processual do 2º grau do TRF4
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório onde se encontram os arquivos
#'
#' @return tibble
#' @export
#'
trf4_ler_dados_cposg <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern = "html$", full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    id <- stringr::str_extract(.x,"TRF4\\d+")

    x <- xml2::read_html("data-raw/a.html")

    classe <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[1]") %>%
      xml2::xml_text()

    origem <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Origin\u00e1rio')]/following-sibling::text()") %>%
      xml2::xml_text(trim=T) %>%
      .[1]

    data_autuacao <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Data de autua\u00e7\u00e3o:')]/following-sibling::text()[1]") %>%
      xml2::xml_text() %>%
      lubridate::parse_date_time("%d/%m/%Y %H:%M:%S") %>%
      as.Date()

    relator<- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Relator:')]/following-sibling::text()[1]") %>%
      xml2::xml_text()

    orgao_julgador <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'\u00d3rg\u00e3o Julgador:')]/following-sibling::text()[1]") %>%
      xml2::xml_text()

    orgao_atual <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'\u00d3rg\u00e3o Atual:')]/following-sibling::text()[1]") %>%
      xml2::xml_text()

    situacao <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Situa\u00e7\u00e3o:')]/following-sibling::text()[1]") %>%
      xml2::xml_text()

    competencia <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Compet\u00eancia:')]/following-sibling::text()[1]") %>%
      xml2::xml_text()


    assunto <- x %>%
      xml2::xml_find_all("//div[@id='spnCabecalho']//following-sibling::strong[contains(.,'Assuntos')]/following-sibling::hr[1]") %>%
      xml2::xml_find_all(".//preceding-sibling::text()") %>%
      xml2::xml_text(trim=T) %>%
      stringr::str_subset("^\\d+[.]\\s") %>%
      stringr::str_c(collapse = "\n")

    tibble::tibble(
      id = id,
      classe  = classe,
      origem = origem,
      data_autuacao = data_autuacao,
      relator = relator,
      orgao_julgador = orgao_julgador,
      orgao_atual = orgao_atual,
      situacao = situacao,
      competencia = competencia,
      assunto = assunto
    )

  },NULL))

}
