#' Ler partes da consulta processual do 2º grau do TRF4
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório onde se encontram os arquivos
#'
#' @return tibble
#' @export
#'
trf4_ler_partes_cposg <- function (arquivos = NULL, diretorio = ".")
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
    parte_nome <- x %>% xml2::xml_find_all("//div//a[contains(text(),'todas as partes')]") %>%
      xml2::xml_find_all("./preceding-sibling::hr/following-sibling::text()") %>%
      xml2::xml_text(trim = TRUE) %>% stringr::str_subset("^[A-Z]{3,}") %>%
      stringr::str_remove(":")

    parte <- x %>% xml2::xml_find_all("//div//a[contains(text(),'todas as partes')]") %>%
      xml2::xml_find_all("./preceding-sibling::hr/following-sibling::text()/following-sibling::strong") %>%
      xml2::xml_text(trim = TRUE) %>%
      .[1:length(parte_nome)]


    tibble::tibble(id = id, parte = parte, parte_nome = parte_nome) %>%
      dplyr::filter(stringr::str_detect(parte,"^\\d+/",negate=TRUE))
  }, NULL))
}
