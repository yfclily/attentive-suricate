library(dplyr)
list_student<-c(
  'Shen-Chin-CHIA',
  '簡睫',
  'YA-FANG CHENG',
  'Ling-Lan HSU',
  'Yen-Ting CHEN',
  'Yu-Wen CHEN',
  'Kong-Mei SHUET',
  'Wu-Chiao LIAO',
  'Mariana REYES',
  'Balamurugan ANANTHAKRISHNAN',
  'Chiao-Jung HAN',
  'Min-Rou  JIANG', 
  'Hsun-Yi  HUANG',
  'San-Pei LEE')
  
sample(list_student, 1, replace = TRUE)
# Visualize source code: View(sample) OR getAnywhere(sample())