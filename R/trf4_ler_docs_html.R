#' Lê julgados
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se arquivos não forem informados
#'
#' @return tibble
#' @export
#'
trf4_ler_docs_html <- function (arquivos = NULL, diretorio = ".")
{
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, full.names = TRUE)
  }
  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    id <- stringr::str_extract(.x, "TRF4\\d+")

    x <- .x %>%
      xml2::read_html(encoding="latin1")



    processo <- x %>%
      xml2::xml_find_first("//span[@data-numero_processo]") %>%
      xml2::xml_text() %>%
      stringr::str_remove_all("\\D+")

    if (is.na(processo)){

      p <- x %>%
        xml2::xml_find_first("//div[@class='identProcesso']") %>%
        xml2::xml_text()

      processo <- stringr::str_extract(p,"\\d{7}.+?(?=/)")

      origem <- stringr::str_extract(p,"\\w+$")

      classe <- stringr::str_extract(p,".+?(?=\\sN\\u00ba)")

      classe_codigo = NA_character_

    } else {


      classe <- x %>%
        xml2::xml_find_first("//span[@data-classe_processo]") %>%
        xml2::xml_text(trim = TRUE)

      classe_codigo <- x %>%
        xml2::xml_find_first("//span[@data-classe_processo]") %>%
        xml2::xml_attr("data-classe_processo")

      origem <- x %>%
        xml2::xml_find_first("//span[@data-origem_processo]") %>%
        xml2::xml_text()

    }

    titulo <- x %>%
      xml2::xml_find_first("//p[@class='titulo']|//div[@class='titulo']") %>%
      xml2::xml_text(trim = TRUE)

    julgado <- x %>%
      xml2::xml_find_all("//body") %>% xml2::xml_text(trim = TRUE) %>%
      stringr::str_c(collapse = "\n")

    tibble::tibble(id, classe, classe_codigo, processo, origem,
                   titulo, julgado)
  }, NULL))
}

