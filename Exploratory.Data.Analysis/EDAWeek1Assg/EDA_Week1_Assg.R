##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  EDA_week1_Assg.R
##  Date:       28Feb2019
##
##Computes:
## 
## Args: None
##         
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

#check if data.table installed
if(!require(data.table)){
  
  install.packages("data.table")
  
}

library(data.table)


#read selective rows of file
data.df <- (fread("household_power_consumption.txt", header=T, sep=";")[-c(1:66636,69517:2075259),])

#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.
#Create a separate R code file (\color{red}{\verb|plot1.R|}plot1.R, \color{red}{\verb|plot2.R|}plot2.R, etc.) that constructs the corresponding plot, i.e. code in \color{red}{\verb|plot1.R|}plot1.R constructs the \color{red}{\verb|plot1.png|}plot1.png plot. Your code file should include code for reading the data so that the plot can be fully reproduced. You must also include the code that creates the PNG file.
#Add the PNG file and R code file to the top-level folder of your git repository (no need for separate sub-folders)