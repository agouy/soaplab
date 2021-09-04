## global code
library(bs4Dash)
library(readxl)
library(DT)
library(dplyr)
library(plotly)

metrics_names <- c("hardness", "cleansing", "longevity", "conditioning", "bubbliness", "creaminess")

getRemoveButton <- function(n, idS = "", lab = "Pit") {
  if (stringr::str_length(idS) > 0) idS <- paste0(idS, "-")
  ret <- shinyInput(actionButton, n,
                    'button_', label = "Remove",
                    onclick = sprintf('Shiny.onInputChange(\"%sremove_button_%s\",  this.id)' ,idS, lab))
  return (ret)
}

shinyInput <- function(FUN, n, id, ses, ...) {
  as.character(FUN(paste0(id, n), ...))
}

master_df <- readxl::read_xlsx("./www/oils.xlsx")

get_met <- function(df, props = NULL) {
  if(is.null(props)) props <- rep(1, nrow(df))
  df_out <- df %>% 
    mutate(hardness = lauric + myristic + palmitic + stearic) %>% 
    mutate(cleansing = lauric + myristic) %>% 
    mutate(longevity = palmitic + stearic) %>% 
    mutate(conditioning = oleic + ricinoleic + linoleic + linolenic) %>% 
    mutate(bubbliness = lauric + myristic + ricinoleic) %>% 
    mutate(creaminess = palmitic + stearic + ricinoleic) %>%
    summarise_at(
      vars(metrics_names),
      funs(weighted.mean(., props)))
  return(df_out)
}

get_met(master_df[1:100,])

get_hardness <- function(df) df$lauric + df$myristic + df$palmitic + df$stearic
get_cleansing <- function(df) df$lauric + df$myristic
get_longevity <- function(df) df$palmitic + df$stearic
get_conditioning <- function(df) df$oleic + df$ricinoleic + df$linoleic + df$linolenic
get_bubbliness <- function(df) df$lauric + df$myristic + df$ricinoleic
get_creaminess <- function(df) df$palmitic + df$stearic + df$ricinoleic

get_c <- function(thetas) return(1 / (sum(sapply(thetas, exp)) + 1))

get_x <- function(thetas) {
  cst <- get_c(thetas)
  xs <- cst * exp(thetas)
  return(c(xs, cst))
}

optim_fn <- function(par, df_tmp, ideal) {
  props <- get_x(par)
  res <- unlist(get_met(df_tmp, props))
  SS <- sum((res - ideal)^2)
  return(SS)
}

get_mixture <- function(oil_names) {
  
  oil <- sort(oil_names)
  df_tmp <-   master_df %>% filter(name_fr %in% oil)
  
  low <- c(29, 12, 25, 44, 14, 16)
  med <- c(54, 22, 50, 69, 46, 48)
  ideal <- (low + med) / 2
  
  n_props <- length(oil)
  par <- rep(1 / n_props, n_props)
  
  thetas <- log(par[- n_props] / par[n_props])
  
  optim_par <- optim(thetas, fn=optim_fn, gr=NULL, df_tmp, ideal, method=c("L-BFGS-B"))
  
  best_val <- get_x(optim_par$par)
  names(best_val) <- oil

  return(best_val)
}


barplot_metrics <- function(data) {
  data <- data %>% arrange(param)
  fig <- plot_ly(data, x = ~param)
  fig <- fig %>% add_trace(
    y = ~low,
    marker = list(color = "white"),
    type = 'bar',
    name = 'bad',
    showlegend=FALSE
  )
  fig <- fig %>% add_trace(
    y = ~med,
    marker = list(color = "lightblue"),
    type = 'bar',
    name = 'good',
    showlegend=FALSE
  )
  fig <- fig %>% add_trace(
    y = ~high,
    marker = list(color = "white"),
    type = 'bar',
    name = 'bad',
    showlegend=FALSE
  )
  
  fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
  
  fig <- fig %>% add_trace(
    y = ~obs, name = 'mon savon',
    marker=list(color = "chocolate"), type = 'scatter', mode = "lines+markers"
  )
  return(fig)
}


get_metrics <- function(oil_names, weights) {
  filtered <- master_df %>% filter(name_fr %in% oil_names)
  perc <- weights/sum(weights)
  
  res <- get_met(filtered, perc)
  
  param <- metrics_names
  low <- c(29, 12, 25, 44, 14, 16)
  med <- c(54, 22, 50, 69, 46, 48) - low
  high <- 100 - (med + low)
  
  data <- data.frame(param, low, med, high)
  data$obs <- unlist(res[, data$param])
  data$obs[is.na(data$obs)] <- 0
  return(data)
}