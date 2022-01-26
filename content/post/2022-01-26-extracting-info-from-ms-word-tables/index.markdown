---
title: Extracting Info from MS Word Tables
author: Timothy Liew
date: '2022-01-26'
slug: extracting-info-from-ms-word-tables
categories: []
tags: [programming]
---

# Introduction

In my work as a lecturer, one of the more tedious tasks we need to deal with is extracting the scores/marks from students' exam paper submission. The typical workflow is as follow:

1. Lecturers mark student's exam paper (in MS Word), allocating scores in a table within the document.
2. This is then checked and moderated by an assigned moderator. 
3. Lecturers then need to extract the score for _each_ question in _each_ student's exam paper, and then transfer that into an MS Excel file for record purposes. 

As you can imagine, it can get very tedious if a class has many students or the exam has many questions, which can lead to numerous data transfer errors. 

Luckily, I knew we could manipulate files using R, and so I set about writing a script that would automate the entire data transfer process. 

# The Exam Paper Format

Okay before we get into the code, we need to clarify the "format" of the exam paper. This is obviously going to be very different for different departments, so keep that in mind when you go through the codes. 

For my department's exam paper format, there are 3 important characteristics:

1. All exam files are in MS Word.
2. Because the documents are uploaded on an elearning platform (e.g., Moodle), the site will use a specific way of naming the files. For example,

`1622257272 - -, SAM BURLEY 2158_SAM_BURLEY_-_B1111111_-_PSY348_188715_1659232754`

The name of the (fictional) student here is Sam Burley, and their student ID is B1111111. This is important because we will pull this information into our code. 

3. Exam scores are entered into a pre-defined table within the MS Word document. Let's call it the **Marks Table**, which looks something like the following: 

![](sam_burley_screenshot.png)

Now that we are clear on the "parameters" of the exam paper documents, we can proceed with the R script.

# The R Script

As usual, we will load in our requisite libraries.


```r
library(docxtractr) # To wrangle data from MS Word files
library(stringr) # For some string manipulation
library(rebus) # I like using this package for simplified regex shenanigans
library(tidyverse) # Good old tidyverse!
```

Next, we need to define some patterns to help extract the student name and ID from the name of the file.


```r
# We'll use a pattern to extract student name and student ID
## This reads as any number of characters that is preceded by "- -, " and followed by a space and a digit
name_pattern <- "(?<=- -, ).+(?= \\d)"

## This reads as starting with either B, C, or E, then 7 digits
id_pattern = or("B", "C", "E", "b", "c", "e") %R% repeated(DIGIT,7)
```

The start of the script is the following code, which is essentially a function that will import the MS Word file into R, extracts the name and student ID, and then extracts the scores for each exam question from the Marks Table.


```r
# Define a function to import file
exam_import <- function(file){
  # Note: This reads in docx files only. You will get an error if the file is not a docx file
  script <- read_docx(file)
  
  # This will extract the student name and ID
  student_name <- str_extract(script$path, pattern = name_pattern)
  student_id <- str_extract(script$path, pattern = id_pattern)
  
  # Extracts the scores. We assume that the first table contains the scores
  exam_scores <- docx_extract_tbl(docx = script, tbl_number = 1) %>% 
  # Sometimes lecturers will use a SUM formula in Word to sum the scores
  # This will cause problems when we import to R, so we do some wrangling to remove the extra words
    mutate(Total = str_extract(.[ncol(.)], pattern = one_or_more(DGT))) %>% 
    select(-(ncol(.)-1)) %>% 
    mutate(name = student_name,
           id = student_id)
  return(exam_scores)
}
```

The hard part is now over. The only tasks left is to mass import the files into R, loop them through the `exam_import()` function, and then write the data frame into a csv file. 


```r
# Now to import the files en masse 
## Identify the word doc files
my_files <- list.files(pattern = "*.docx")

## Read in the files into a dataframe
exam_files <- my_files %>% 
  map_dfr(.f = exam_import)

# Exporting the file

## And now to export the table to a csv file. Make sure you name the file to whatever suits you.
## Note: When naming the file, make sure that i) the file name is followed by ".csv" and ii) the whole name is enclosed in quotation marks
write.csv(x = exam_files, file = "potatochips.csv")
```

The final result should look something like this:

![](exam_scores.png)

And there you have it, a quick and easy way of extracting exam scores. Writing this script was really enjoyable, because it was a fond reminder that I can use R to simplify and automate so many different tasks. 
