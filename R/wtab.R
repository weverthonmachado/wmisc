#--------------------------------------------------------------------
# ## Weighted tab com dplyr
#
# ## Weverthon Machado
#--------------------------------------------------------------------


#' Weighted tabulation
#'
#' `wtab` produces one-way, two-way and three-way tabulations with weighted data.
#'
#' This is actually a wrapper function that calls other ones. `wtab` is compatible
#' with the `%>%` pipe operator
#'
#' @param dat A data frame
#' @param row Row variable
#' @param col Column variable
#' @param by Grouping (super row) variable
#' @param wt Weight variable
#'
#' @export
#' @importFrom magrittr %>%
wtab <- function(dat, row, col, by, wt, ...){
    if (missing(wt)) {
      dat <- dat %>% dplyr::mutate(wt=1)
      wt <- "wt"
      wt <- dplyr::quo(wt)
    } else {
      wt <- dplyr::enquo(wt)
    }

    if (!missing(row) & missing(col) & missing(by)) {
        row <- dplyr::enquo(row)
        result <- wtab_1way(dat, !! row, !! wt, ...)
    } else if (!missing(row) & !missing(col) & missing(by)) {
        row <- dplyr::enquo(row)
        col <- dplyr::enquo(col)
        result <- wtab_2way(dat, !! row, !! col, wt= !! wt, ...)
    } else if (!missing(row) & !missing(col) & !missing(by)) {
        row <- dplyr::enquo(row)
        col <- dplyr::enquo(col)
        by <- dplyr::enquo(by)
        result <- wtab_3way(dat, !! row, !! col, !! by, wt= !! wt, ...)
    }
    result
}


#' One-way Weighted tabulation
#'
#' wtab_1way produces one-way with weighted data.
#'
#' @param dat A data frame
#' @param row Row variable
#' @param wt Weight variable
wtab_1way <- function(dat, row, wt) {
    if (missing(wt)) {
      dat <- dat %>% dplyr::mutate(wt=1)
      wt <- "wt"
      wt <- dplyr::quo(wt)
    } else {
      wt <- dplyr::enquo(wt)
    }

    row <- dplyr::enquo(row)
    x <- dplyr::select(dat, !! row, !! wt)

    result<- x %>%
            dplyr::group_by(!! row) %>%
            dplyr::summarise(sum =sum(!! wt))

    result
}

#' @export
wtab_2way <- function(dat, row, col, wt, stat="freq") {
    if (missing(wt)) {
      dat <- dat %>% dplyr::mutate(wt=1)
      wt <- "wt"
      wt <- dplyr::quo(wt)
    } else {
      wt <- dplyr::enquo(wt)
    }

    row <- dplyr::enquo(row)
    col <- dplyr::enquo(col)
    x <- dplyr::select(dat, !! row, !! col, !! wt)

    if (stat=="freq") {
            result<- x %>%
            dplyr::group_by(!! row, !! col) %>%
            dplyr::summarise(wtd.freq = sum(!! wt))
    } else if (stat=="row") {
             result<- x %>%
             dplyr::group_by(!! row) %>%
             dplyr::mutate(rowtotal = sum(!! wt)) %>%
             dplyr::group_by(!! row, !! col, rowtotal) %>%
             dplyr::summarise(wtd.freq=sum(!! wt)) %>%
             dplyr::mutate(row.prop= wtd.freq/rowtotal*100) %>%
             dplyr::select(-rowtotal, -wtd.freq)
    } else if (stat=="col") {
             result<- x %>%
             dplyr::group_by(!! col) %>%
             dplyr::mutate(coltotal = sum(!! wt)) %>%
             dplyr::group_by(!! row, !! col, coltotal) %>%
             dplyr::summarise(wtd.freq=sum(!! wt)) %>%
             dplyr::mutate(col.prop= wtd.freq/coltotal*100) %>%
             dplyr::select(-coltotal, -wtd.freq)
    } else if (stat=="cell") {
      result<- x %>%
        dplyr::mutate(total = sum(!! wt)) %>%
        dplyr::group_by(!! row, !! col, total) %>%
        dplyr::summarise(wtd.freq=sum(!! wt)) %>%
        dplyr::mutate(cell.prop= wtd.freq/total*100) %>%
        dplyr::select(-total, -wtd.freq)
    }
    result
    structure(result, class = c("wtab_2way", class(result)))
}

#' @export
wtab_3way <- function(dat, row, col, by, wt, ...) {
    if (missing(wt)) {
      dat <- dat %>% dplyr::mutate(wt=1)
      wt <- "wt"
      wt <- dplyr::quo(wt)
    } else {
      wt <- dplyr::enquo(wt)
    }

    row <- dplyr::enquo(row)
    col <- dplyr::enquo(col)
    by <- dplyr::enquo(by)
    x <- dplyr::select(dat, !! row, !! col, !! by, !! wt)

    rowlab <- attributes(x[[1]])$label
    collab <- attributes(x[[2]])$label
    bylab  <- attributes(x[[3]])$label


    t <- x %>%
      dplyr::group_by(!! by) %>%
      dplyr::do(tab = wtab_2way(., !! row, !! col, !! wt, ...))

    attributes(t)$bylab <- bylab
    attributes(t)$collab <- collab
    attributes(t)$rowlab <- rowlab

    structure(t, class = c("wtab_3way", class(t)))
}


#' @export
print.wtab_3way <- function(x) {
    group_name  <- names(x)[1]
    names(x)[1] <- "group_value"
    p <- function(group_value, tab){
        colname <- names(tab)[2]
        statname <- names(tab)[3]
        tab <- tab %>%
               tidyr::spread(colname, statname) %>%
               dplyr::ungroup() %>%
               dplyr::select_all(.funs = funs(paste0("&nbsp;\n", .)))
        col2_old <- names(tab)[2]
        col2_new <- paste0(colname, col2_old)
        tab <- tab %>% rename(!! col2_new := !! col2_old)
        cat("\n")
        print(cli::rule(line = 1, left = paste0(group_name, " = ", as.character(group_value))))
        pander::pandoc.table(tab, justify = "right", keep.line.breaks=T, plain.ascii=T, digits=2, keep.trailing.zeros=T, round=2)
        cat("\n")
    }
    rowname <- names(x[2][[1]][[1]])[1]
    colname <- names(x[2][[1]][[1]])[2]

    cat("\n")
    cat(paste0(rowname, ": ", attributes(x)$rowlab, "\n"))
    cat(paste0(colname, ": ", attributes(x)$collab, "\n"))
    cat(paste0(group_name, ": ", attributes(x)$bylab, "\n"))


    purrr::pmap(x, p)
    invisible(x)
}

#' @export
print.wtab_2way <- function(x) {
    if (length(x)==3) {

      tab <- x

      #checa se vars têm label
      if ("label" %in% names(attributes(tab[[1]]))){
        rowlab <- attributes(tab[[1]])[1][[1]]
      }
      if ("label" %in% names(attributes(tab[[2]]))){
        collab <- attributes(tab[[2]])[1][[1]]
      }
      # se existe pelo menos uma label, criar outra como " "
      if (exists("rowlab") & !exists("collab")) {
        collab<- " "
      }
      if (!exists("rowlab") & exists("collab")) {
        rowlab<- " "
      }


      rowname <- names(tab)[1]
      colname <- names(tab)[2]
      statname <- names(tab)[3]
      tab <- tab %>%
        tidyr::spread(colname, statname) %>%
        dplyr::ungroup() %>%
        dplyr::select_all(.funs = funs(paste0("&nbsp;\n", .)))
      col2_old <- names(tab)[2]
      col2_new <- paste0(colname, col2_old)
      tab <- tab %>% rename(!! col2_new := !! col2_old)

      cat("\n")
      if (exists("collab")){
        cat(paste0(colname, ": ", collab, "\n"))
        cat(paste0(rowname, ": ", rowlab))
      }
      cat("\n")
      pander::pandoc.table(tab, justify = "right", keep.line.breaks=T, plain.ascii=T, keep.trailing.zeros=T, round=2)
      cat("\n")
    } else {
      print(dplyr::as_data_frame(x))
    }
    invisible(x)
}




