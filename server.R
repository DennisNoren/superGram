library(shiny)
library(stringr)
library(dplyr)
library(tidyr)
library(RWeka)
library(tm)

vars <- reactiveValues(chat=NULL, users=NULL)
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to Shiny Chat!"
}
linePrefix <- function(){
  if (is.null(isolate(vars$chat))) {
    return("")
  }
  return("<br />")
}

ngram <- readRDS('ng_twitter.rds') %>%
  separate(term, c('gram1', 'gram2', 'gram3'), sep = ' ', extra = 'drop', remove = FALSE) %>%
  mutate(resp = ifelse(n == 1, gram1, ifelse(n == 2, gram2, gram3)),
         contexto = ifelse(n == 1, NA, ifelse(n == 2, gram1, paste(gram1, gram2)))) %>%
  select(n, term, contexto, resp, freq) %>%
  filter(!term %in% c('<s>', '</s>')) %>%
  arrange(desc(freq))

#------------------------------------------------------------------------------
# MODEL

rm_accent <- function(x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))

preparar_teste <- function(txt) {
  txt %>%
    VectorSource() %>%
    VCorpus() %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
    tm_map(content_transformer(rm_accent)) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    data_frame(txt = sapply(., `[[`, 1)) %>%
    select(txt) %>%
    mutate(txt = sprintf('<s> %s </s>', str_trim(txt))) %>%
    with(txt)
}

ngrams_teste <- function(txt, ngram, n_min = 1, n_max = 3) {
  w_control <- Weka_control(min = n_min, max = n_max)
  tok <- function(x) NGramTokenizer(x, w_control)
  ctrl <- list(tokenize = tok)
  voc_ng <- ngram %>% filter(n == 1) %>% with(term)
  voc_txt <- unique(scan_tokenizer(txt))
  voc_txt <- voc_txt[!voc_txt %in% voc_ng]
  voc_txt <- paste(sprintf('( %s )', voc_txt), collapse = '|')
  if(voc_txt == '') voc_txt <- '@'
  txt %>%
    preparar_teste() %>%
    str_replace_all(voc_txt, ' <unk> ') %>%
    VectorSource() %>%
    VCorpus() %>%
    `[[`(1) %>%
    termFreq(control = ctrl) %>%
    data_frame(term = names(.), freq = as.integer(.)) %>%
    filter(!str_detect(term, '</s> <s>')) %>%
    select(term, freq) %>%
    mutate(n = str_count(term, ' ')) %>%
    filter(str_detect(term, '</s>'), term != '</s>') %>%
    mutate(term = str_trim(gsub('</s>', '', term)))
}

predict_pkn <- function(txt, ngram, k = 5) {
  op <- ngrams_teste(txt, ngram)
  d <- ngram %>% 
    filter(contexto %in% op$term) %>%
    arrange(desc(freq)) %>%
    filter(resp != '</s>') %>%
    head(1)
  
  
  res <- d$resp
  res[res == '</s>'] = '.'
  if(length(res) == 0) {
    res <- ngram %>%
      filter(n == 1) %>%
      sample_n(1) %>%
      with(term)
    print('aquia')
  }
  res
}

#------------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  sessionVars <- reactiveValues(username = "")
  init <- FALSE
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, 
                     paste0(linePrefix(), 
                            tags$span(class="user-exit", 
                                      sessionVars$username, 
                                      "left the room.")))
    })
  })
  
  observe({
    input$user
    if(!init) {
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else {
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          return()
        }
        vars$users <- vars$users[vars$users != sessionVars$username]
        vars$chat <<- c(vars$chat, 
                        paste0(linePrefix(),
                               tags$span(class="user-change",
                                         sessionVars$username,
                                         " changed name to ",
                                         input$user)))
        sessionVars$username <- input$user
      })
    }
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  observe({
    updateTextInput(session, "user", value=sessionVars$username)    
  })
  
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  observe({
    if(input$send < 1){
      return()
    }
    isolate({
      entry <- gsub(' +', ' ', input$entry)
      if(entry %in% c(' ', '')) return()
      vars$chat <<- c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), 
                                                 sessionVars$username)),
                             ": ",
                             tagList(input$entry)))
    })
    updateTextInput(session, "entry", value="")
  })
  
  output$chat <- renderUI({
    if (length(vars$chat) > 16){
      vars$chat <- vars$chat[(length(vars$chat)-16):(length(vars$chat))]
    }
    saveRDS(vars$chat, "chat.Rds")
    HTML(vars$chat)
  })
  
  observe({
    if(is.null(input$entry)) return()
    entry <- gsub(' +', ' ', input$entry)
    if(str_sub(entry, -1L) != ' ') return()
    entry <- str_trim(entry)
    if(entry %in% c(' ', '')) return()
    
    predicoes <- predict_pkn(entry, ngram)
    isolate({
      updateSelectInput(session, inputId = 'opcoes', label = '',
                        choices = predicoes, selected = predicoes)
    })
  })
  
})
