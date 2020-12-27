#' Baixa inteiro teor de acórdãos ou decisões monocráticas
#'
#' @param id Id do processo. Pode ser completo ou seomente os números.
#' @param url Url do processo
#' @param diretorio Diretório onde armazenar os arquivos
#'
#' @return html ou pdf
#' @export
#'
trf4_baixar_inteiro_teor <- function(id = NULL, url = NULL, diretorio = NULL){



  pb <- progress::progress_bar$new(total = length(id))


  purrr::walk2(id, url,purrr::possibly(~{

    pb$tick()

    resposta <- httr::GET(.y)

    tipo <- purrr::pluck(resposta,'headers','content-type')

    if (stringr::str_detect(tipo,"text/html")){

      arquivo <- file.path(diretorio, paste0(stringr::str_replace_all(Sys.time(),'\\D',"_"),'_',.x,'.html'))

    } else {

      arquivo <- file.path(diretorio, paste0(stringr::str_replace_all(Sys.time(),'\\D',"_"),'_',.x,'.pdf'))


    }


    writeBin(resposta$content,arquivo)

  },NULL))



}
