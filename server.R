#################################################
#         tidy sentiments                       #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")
library("DT")
library("reshape2")
library("wordcloud")
library("plotly")

#--------------------------------------------#


shinyServer(function(input, output,session) {
  set.seed=2092014   
  
lexicon_data<-read.csv('sentiments.csv',stringsAsFactors=FALSE)# read lexcicons from app folder
  
  output$dictionary <- renderUI({
    if (input$lexicon != 'userdefined') return(NULL)    
    fileInput("user_dict", "Upload User-Defined Dictionary")
      })
  
  userdictionary = reactive({
    if (input$lexicon != 'userdefined') return(NULL)
    userdictionary = read.csv(input$user_dict$datapath,header=TRUE, sep = ",", stringsAsFactors = F)
      })

## +++++++++++++
  
  dataset <- reactive({
        if (is.null(input$file)) {return(NULL)}
    else {

     if(file_ext(input$file$datapath)=="txt"){
        Document = readLines(input$file$datapath)        
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id=Doc.id, text = Document)
        colnames(calib) <- c("Doc.id","text")
        print(input$file$name)
        return(calib)} else if(file_ext(input$file$datapath)=="pdf")
      {          
        pdf_text0 <- pdftools::pdf_text(input$file$datapath)                
        pdf_text1 <- str_replace_all(pdf_text0, 
                                     pattern = "([.!?])\n(\\w)", 
                                     replacement = "\\1\n\n\\2") 
  
        # Collapse multiple repetitions of newline into a paragraph break
        pdf_text1 <- gsub("\n{2,}", "\n\n", pdf_text1)
        pdf_text1 <- gsub("\n\\s{2,}", " ", pdf_text1)
  
        # Combine text from all pages while preserving line breaks
        pdf_text1 <- paste(pdf_text1, collapse = "\n\n")
        pdf_text2 <- str_split(pdf_text1, pattern = "\n\n")
          Doc.id <- seq(1, length(pdf_text2[[1]]))
          calib <- data.frame(docID = Doc.id, text = pdf_text2[[1]])
          colnames(calib) <- c("Doc.id","Documents")
          print(input$file$name)
          return(calib)} else
      {
      Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
      Document[,1] <- str_to_title(Document[,1])
      Document[,1] <- make.names(Document[,1], unique=TRUE)
      Document[,1] <- tolower(Document[,1])
      Document[,1] <- str_replace_all(Document[,1],"\\.","_")
      Document<-Document[complete.cases(Document), ]
      Document <- Document[!(duplicated(Document[,1])), ]
      rownames(Document) <- Document[,1]
      return(Document)      }      
    }
  })

  cols <- reactive({colnames(dataset())})
  y_col <- reactive({
      x <- match(input$x, cols())
      y_col <- cols()[-x]
      return(y_col)     })
  
  output$id_var <- renderUI({ print(cols())
    selectInput("x","Select ID Column",choices = cols())   })
  
  output$doc_var <- renderUI({ selectInput("y","Select Text Column",choices = y_col())   })
 
 output$up_size <- renderPrint({ size <- dim(dataset())
    paste0("Dimensions of uploaded data: ",size[1]," (rows) X ", size[2]," (Columns)") })

  dataset1 <- reactive({ y1 <- match(input$y, cols())
    df0 <- data.frame(text = dataset()[,y1]); return(df0) })

   output$up_size <- renderPrint({
    size <- dim(dataset())
    paste0("Dimensions of uploaded data: ",size[1]," (rows) X ", size[2]," (Columns)")
  })

  text_summ <- eventReactive(input$apply,{summary(quanteda::corpus(dataset()[,input$y]))})
  quant_mod <- eventReactive(input$apply,{quanteda::corpus(dataset()[,input$y])})
    
  output$text01 <- renderUI({
        req(input$file$datapath)
        str1 <- paste("Total no of documents:", nrow(dataset()))
        str2 <- paste("Range of sentences per document: ",min(text_summ()$Sentences),"-",max(text_summ()$Sentences))
        #str3 <- paste("Maximum number of sentence: ",)
        str4 <- paste("Average number of sentences per document: ",round(mean(text_summ()$Sentences),2))
        HTML(paste(str1, str2,str4, sep = '<br/>'))
    })

    output$text02 <- renderUI({
        req(input$file$datapath)
        str2 <- paste("Range of words per document: ",min(text_summ()$Tokens),'-',max(text_summ()$Tokens))
        #str3 <- paste("range of words per document:: ",max(text_summ()$Tokens))
        str4 <- paste("Average number of words: ",round(mean(text_summ()$Tokens),2))
        HTML(paste(str2,str4, sep = '<br/>'))
    })
  output$samp_data <- DT::renderDataTable({
    DT::datatable(dataset(), rownames = FALSE)
  })
  
  ## +++++
    stopw = eventReactive(input$apply,{
        stopwords = data_frame(text = input$stopw) |>
        mutate(linenumber = row_number()) |>
        ungroup() |>
        unnest_tokens(word, text)
        stopwords
    })
  
    sent.df = eventReactive(input$apply,{
      textdf = dataset1() #[,input$y]
      
      if (input$lexicon == 'userdefined'){
          sent = textdf |>
          mutate(linenumber = row_number()) |>
          ungroup() |> 
          unnest_tokens(word, text) |>
          anti_join(stopw(), by="word") |>
          inner_join(tbl_df(userdictionary()))
        }    else {
        sent = textdf |>
          mutate(linenumber = row_number()) |>
          ungroup() |>
          unnest_tokens(word, text) |> 
          anti_join(stopw(), by="word") |> 
          inner_join(lexicon_data%>%filter(lexicon==input$lexicon)) 
        }
      return(sent)
    })
  
  sentiments_cdf =  eventReactive(input$apply,{
    if (input$lexicon %in% c('userdefined',"AFINN")){
      sentiments_cdf = sent.df() |>
        group_by(index = linenumber %/% 1) |>
        summarise(sentiment = sum(score)) |> 
        mutate(method = input$lexicon)          
    }  else {
      sentiments_cdf = sent.df()[,-c(4,5)] |>
        count(sentiment, index = linenumber %/% 1, sort = TRUE) |>
        mutate(method = input$lexicon)      
    }    
    return(sentiments_cdf)   }) # reactive ends

  dat = reactive({
    
    dat1 = sentiments_cdf()[(sentiments_cdf()$sentiment %in% c("positive","negative") ),]
    dat2 = sentiments_cdf()[(sentiments_cdf()$sentiment %in% c("uncertainty","litigious","constraining","superfluous") ),]
    dat3 = sentiments_cdf()[(sentiments_cdf()$sentiment %in% c("joy","trust","surprise","anticipation") ),]
    dat4 = sentiments_cdf()[(sentiments_cdf()$sentiment %in% c("anger","disgust","fear", "sadness") ),]
    
    if (input$lexicon %in% c("AFINN","userdefined")) {
      out = list(sentiments_cdf())
    } else if (input$lexicon == "nrc") {
      out = list(dat3,dat4) # ,dat1
    } else if (input$lexicon == "bing") {
      out = list(dat1)
    } else if (input$lexicon == "loughran") {
      out = list(dat2,dat1)
    }
    return(out)  }) # dat reactive
  
    output$sent.plots <- renderUI({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      
      if (input$lexicon %in% c("AFINN","userdefined")) {
        k = 1
      } else if (input$lexicon == "nrc") {
        k = 3
      } else if (input$lexicon == "bing") {
        k = 1
      } else if (input$lexicon == "loughran") {
        k = 2
      }
        plot_output_list <- lapply(1:k, function(i) {
        plotname <- paste("plot", i, sep="")
        plotlyOutput(plotname, height = 700, width = 700)  }) # lapply ends
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    }
  })
  
  max_plots = 3  
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      
      my_i <- i 
      plotname <- paste("plot", my_i, sep="")
      
        output[[plotname]] <- renderPlotly({
          if (input$lexicon %in% c("AFINN",'userdefined')) {
            plot_ly(dat()[[my_i]],x = ~index, y = ~ sentiment,type = "bar")
            # plot_ly(sentiments,x = ~index, y = ~ sentiment,type = "bar")
            #      aes(index, sentiment)) +     # index is x col, n is y col. fill=?
            # geom_bar(alpha = 1, stat = "identity", position = "identity", show.legend = FALSE)      # stat=?
          } else {
            
          ggplot(dat()[[my_i]], 
            aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
            geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
            facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.    
          }
        })
      })
  }
    
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  
  output$word.cloud <- renderPlot({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
    
    if (input$lexicon  %in% c("nrc","bing","loughran")) {
      sent.df() |>
      count(word, sentiment, sort = TRUE) |>
      acast(word ~ sentiment, value.var = "n", fill = 0) |>
       comparison.cloud( #colors = c("#F8766D", "#00BFC4"),
                       max.words = 300)
    } else {
      
        sent.df() |>
        count(word, score, sort = TRUE) |>
        acast(word ~ score, value.var = "n", fill = 0) |>
        comparison.cloud( #colors = c("#F8766D", "#00BFC4"),
        max.words = 100)
    }
    }
  })

  output$count <- renderDataTable({    
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {      
    if (input$lexicon %in% c("nrc","bing","loughran")) {
        wc = sent.df() |>
        count(word, sentiment, sort = TRUE) |>
        acast(word ~ sentiment, value.var = "n", fill = 0) 
      
    } else {
      
        wc = sent.df() |>
        count(word, score, sort = TRUE) |>
        acast(word ~ score, value.var = "n", fill = 0) 
    }
    
    wc1 = data.frame(wc)

    if (input$lexicon %in% c("AFINN","userdefined")){
     neg =grep("\\.",colnames(wc1))
     vec = c(rep("neg_",length(neg)), rep("pos_",(length(colnames(wc1))-length(neg))))
     cnames = paste(vec,gsub("X|X\\.","",colnames(wc1)))
     colnames(wc1) = cnames
    }
    wc1 = wc1[order(wc1[,1], decreasing = T),]
    return(wc1)
    }
  })
  
  #----------------------------------------------------#
  
  sentiments.index =  eventReactive(input$apply,{
    textdf = data.frame(text=dataset1()[input$index]) |> # as.character() |> as.data.frame() |> 
          unnest_tokens(text, text, token = "sentences")
  
    if (input$lexicon == "nrc") {
        sent = textdf |>
        mutate(linenumber = row_number()) |>
        ungroup() |>
        unnest_tokens(word, text) |>
        anti_join(stopw(), by="word") |>
        inner_join(lexicon_data |> 
        filter(lexicon=='nrc')) |> 
        count(sentiment, Sentence.No = linenumber %/% 1, sort = TRUE) |>
        mutate(method = "nrc")
    }
    
    if (input$lexicon == "bing") {
      sent = textdf |>
        mutate(linenumber = row_number()) |>
        ungroup() |>
        unnest_tokens(word, text) |>
        anti_join(stopw(), by="word") |>
        inner_join(lexicon_data |> filter(lexicon=='bing')) |> 
        count(sentiment, Sentence.No = linenumber %/% 1, sort = TRUE) |>
        mutate(method = "bing")
    }
    
    if (input$lexicon == "AFINN") {
      sent = textdf |>
        mutate(linenumber = row_number()) |>
        ungroup() |>
        unnest_tokens(word, text) |>
        anti_join(stopw(), by="word") |>
        inner_join(lexicon_data |> filter(lexicon=='AFINN')) |>
        group_by(Sentence.No = linenumber %/% 1) |> 
        summarise(sentiment = sum(score)) |>
        mutate(method = "afinn")
    }
    
    if (input$lexicon == "loughran") {
      sent = textdf |>
        mutate(linenumber = row_number()) |>
        ungroup() |>
        unnest_tokens(word, text) |>
        anti_join(stopw(), by="word") |>
        inner_join(lexicon_data%>%filter(lexicon=='loughran')) |>
        count(sentiment, Sentence.No = linenumber %/% 1, sort = TRUE) |>
        mutate(method = "loughran")
    }
    
    if (input$lexicon == "userdefined") {
        sent = textdf |>
        mutate(linenumber = row_number()) |>
        ungroup() |>
        unnest_tokens(word, text) |>
        anti_join(stopw(), by="word") |>
        inner_join(tbl_df(userdictionary())) |>
        group_by(Sentence.No = linenumber %/% 1) |>
        summarise(sentiment = sum(score)) |>
        mutate(method = "userdefined")
    }    
    return(sent)  
  })
  
  t1 = reactive({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      
      textdf = dataset1()
      
      if (input$lexicon == 'userdefined'){
          worddf = textdf |>
          mutate(linenumber = row_number()) |>
          ungroup() |>
          unnest_tokens(word, text) |>
          anti_join(stopw(), by="word") |>
          inner_join(tbl_df(userdictionary())) |>
          unique()
      }    else {
          worddf = textdf |>
          mutate(linenumber = row_number()) |>
          ungroup() |>
          unnest_tokens(word, text) |>
          anti_join(stopw(), by="word") |>
          inner_join(lexicon_data |> filter(lexicon==input$lexicon)) |>
          unique()
      }
      
      worddf = data.frame(worddf)
      
      if (input$lexicon %in%  c("nrc","bing","loughran")) {
        wdf = data.frame(NULL)
        for (i in unique(worddf$linenumber)) {
          tempd = worddf[worddf$linenumber == i,]
          se = unique(tempd$sentiment)
          se = se[order(se)]
          for (s in se){
            t = paste(tempd[tempd$sentiment == s,'word'],collapse = ", ")
            dft = data.frame(index = i, sentiment = s, words = t)
            wdf = rbind(wdf, dft)
          }
        } 
      }   else {
        wdf = data.frame(NULL)
        for (i in unique(worddf$linenumber)) {
          tempd = worddf[worddf$linenumber == i,]
          se = unique(tempd$score)
          se = se[order(se)]
          for (s in se){
            t = paste(tempd[tempd$score == s,'word'],collapse = ", ")
            dft = data.frame(index = i, sentiment = s, words = t)
            wdf = rbind(wdf, dft)
          }
        }        
      }
      wdf1 = wdf[wdf$index == input$index,]
      return(wdf1)
    } # else ends    
  })
  
  output$table <- renderDataTable({
    datatable(t1(), rownames = F)
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))

#  t2 = reactive({
#    if (is.null(input$file)|input$apply==0) {return(NULL)} 
#    else {
#      tb = sentiments.index()
#      tx = dataset1()[input$index, 1] |> as.data.frame() |>
#          unnest_tokens(text, text, token = "sentences")      
#      y1 = data.frame(tx, Sentence.No = 1:nrow(tx))    
#      #test = merge(tb, y1, by.x ="Sentence.No", by.y= "Sentence.No", all.y=T)
#      #return(test)
#      return(y1)    
#    }    
#  })
  
  output$table2 <- renderDataTable({
    # datatable(t2(), rownames = F)
    datatable(sentiments.index(), rownames = F)
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
  #----------------------------------------------------#
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "Sentiments Scores.csv" },
    content = function(file) {
      write.csv(sentiments_cdf(), file, row.names=F)
    })
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "User Defined Dictionary.csv" },
    content = function(file) {
      write.csv(read.csv("data/user defined dictionary.csv",stringsAsFactors = F), file, row.names=F)
    })
  
  output$downloadData4 <- downloadHandler(
    filename = function() { "OnePlus.txt" },
    content = function(file) {
       writeLines(readLines("data/onePlus8T_reviews.txt.txt "), file)
    })
  
  output$downloadData5 <- downloadHandler(
    filename = function() { "Bahubali_reviews.txt" },
    content = function(file) {
       writeLines(readLines("data/Bahubali_reviews.txt "), file)
    })  

})
