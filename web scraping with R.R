library(dplyr)
##web scraping "Specphone"
url<-"https://specphone.com/OPPO-A17k.html"

##read_html
specphone<-read_html(url)


##retrive phone name
att<-specphone%>%
  html_nodes("div.topic")%>%
  html_text2()
 

value<-specphone%>%
  html_nodes("div.detail")%>%
  html_text2()


df<-data.frame(attribte=att,detail=value)

View(df)


###retrive all smart phone

samsung_url<-read_html("https://specphone.com/brand/Samsung")

##get link all samsung smartphone
links<-samsung_url%>%
  html_nodes("li.mobile-brand-item a")%>%
  html_attr("href")

full_link<-paste0("https://specphone.com",links)


result<-data.frame()

for (link in full_link[1:10]){
  ss_names<-link%>%
    read_html()%>%
    html_nodes("h1.page-topic")%>%
    html_text2()
  
  ss_topic<-link%>%
    read_html()%>%
    html_nodes("div.topic") %>%
    html_text2()
  
  ss_detail<-link%>%
    read_html()%>%
    html_nodes("div.detail") %>%
    html_text2()
    temp<-data.frame(item=ss_names,attribute=ss_topic,
                   value=ss_detail)
    result<-bind_rows(result,temp)
    print("process...")
}


View(result)

write_csv(result,"result_samsung_phone.csv")


head(result)
































