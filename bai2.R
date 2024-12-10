library(shiny)
library(rvest)
library(wordcloud2)
library(dplyr)
library(stringr)
library(tm)

# Hàm crawl bài viết từ VnExpress theo từ khóa tìm kiếm
crawl_vnexpress <- function(topic, num_pages = 2) {
  base_url <- "https://timkiem.vnexpress.net"
  search_url <- paste0(base_url, "/?q=", URLencode(topic))
  articles <- data.frame(Title = character(), Content = character(), stringsAsFactors = FALSE)
  
  for (page in 1:num_pages) {
    url <- paste0(search_url, "&page=", page)
    page_html <- tryCatch(read_html(url), error = function(e) NULL)
    if (is.null(page_html)) next
    
    # Lấy danh sách bài viết
    article_links <- page_html %>%
      html_nodes(".title-news a") %>%
      html_attr("href") %>%
      unique()
    
    # Crawl từng bài viết
    for (link in article_links) {
      if (!startsWith(link, "https://")) {
        link <- paste0(base_url, link)  # Thêm base_url nếu link không đầy đủ
      }
      
      article_html <- tryCatch(read_html(link, user_agent = "Mozilla/5.0"), error = function(e) NULL)
      if (!is.null(article_html)) {
        title <- article_html %>% html_node("h1") %>% html_text(trim = TRUE)
        
        # Chỉnh sửa phần này để lấy đúng nội dung bài viết
        content <- article_html %>%
          html_nodes("article.fck_detail") %>%  # Sửa lại selector để lấy đúng nội dung
          html_text(trim = TRUE) %>%
          paste(collapse = " ")
        
        # Nếu không lấy được nội dung, thông báo lỗi
        if (content == "") {
          content <- "Nội dung không có sẵn."
        }
        
        articles <- rbind(articles, data.frame(Title = title, Content = content, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Lưu kết quả vào file CSV
  write.csv(articles, file = "articles_data.csv", row.names = FALSE)
  return(articles)  # Đảm bảo trả về một data frame đúng
}

# Hàm tạo Word Cloud
generate_wordcloud <- function(text_data, stopwords) {
  if (length(text_data) == 0 || nchar(paste(text_data, collapse = " ")) == 0) {
    return(NULL)
  }
  
  # Làm sạch văn bản
  text_data <- tolower(text_data)
  text_data <- removePunctuation(text_data)
  text_data <- removeNumbers(text_data)
  text_data <- removeWords(text_data, c(stopwords, stopwords("en")))
  
  # Chuyển dữ liệu sang dạng ma trận từ
  text_corpus <- Corpus(VectorSource(text_data))
  term_matrix <- TermDocumentMatrix(text_corpus)
  term_matrix <- as.matrix(term_matrix)
  
  if (nrow(term_matrix) == 0) {
    return(NULL)
  }
  
  word_freq <- sort(rowSums(term_matrix), decreasing = TRUE)
  data <- data.frame(word = names(word_freq), freq = word_freq)
  wordcloud2(data, size = 1)
}

# UI
ui <- fluidPage(
  titlePanel("Word Cloud Visualization from News Articles"),
  sidebarLayout(
    sidebarPanel(
      textInput("topic", "Enter Topic:", "giáo dục"),
      numericInput("num_pages", "Number of Pages to Crawl:", 2, min = 1, max = 10),
      actionButton("crawl", "Crawl Data"),
      hr(),
      h5("Crawl Summary:"),
      textOutput("summary"),
      downloadButton("download", "Download CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", wordcloud2Output("wordcloud")),
        tabPanel("Articles", tableOutput("articles"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  articles_data <- reactiveVal(data.frame(Title = character(), Content = character()))
  
  observeEvent(input$crawl, {
    req(input$topic, input$num_pages)
    articles <- crawl_vnexpress(input$topic, input$num_pages)
    
    # Kiểm tra nếu dữ liệu là valid
    if (nrow(articles) == 0) {
      showNotification("No articles found. Please try a different topic or increase the number of pages.", type = "error")
      return()
    }
    
    articles_data(articles)
    output$summary <- renderText({
      paste0("Crawled ", nrow(articles), " articles related to '", input$topic, "'.")
    })
  })
  
  output$wordcloud <- renderWordcloud2({
    req(articles_data())  # Kiểm tra nếu dữ liệu có sẵn
    
    # Kiểm tra nội dung trước khi xử lý
    contents <- paste(articles_data()$Content, collapse = " ")
    
    wordcloud <- generate_wordcloud(contents, stopwords = c("và", "là", "của", "có", "cho", "với", "một", "các", "được", "trong", "những", "này", "đó", "khi"))
    
    if (is.null(wordcloud)) {
      showNotification("Không có dữ liệu để tạo Word Cloud.", type = "error")
    } else {
      wordcloud
    }
  })
  
  output$articles <- renderTable({
    req(articles_data())  # Kiểm tra nếu dữ liệu có sẵn
    articles_data()  # Hiển thị bài viết dưới dạng bảng
  })
  
  # Tạo tính năng tải về tệp CSV
  output$download <- downloadHandler(
    filename = function() {
      paste("articles_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(articles_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
