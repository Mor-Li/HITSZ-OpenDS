---
title: "Tutorial-1"
author: "Zhenghui Feng"
date: '2022-06-15'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


指定随机数种子
```{r}
set.seed(100)
```


产生100个U(0,1)随机数
```{r}
runif(100,0,1)
x1<-runif(100,0,1)
```

画散点图和直方图
```{r}
plot(x1)
hist(x1)
```

产生100个Binomial(1,0.5)随机数
```{r}
rbinom(10,1,0.5)
x2<-rbinom(10,1,0.5)
plot(x2)
hist(x2)
```

产生100个Binomial(20,0.5)随机数
```{r}
rbinom(100,20,0.5)
x3<-rbinom(100,20,0.5)
plot(x3)
hist(x3)
```


