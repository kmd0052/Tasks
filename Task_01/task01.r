library(swirl)

swirl()
Kylie Dickinson
2
2
swirl()
Kylie Dickinson
1
no2
2
1
2
getwd()
ls()
x <- 9
ls()
list.files()
?list.files
args(list.files)
old.dir <- getwd()
dir.create("testdir")
setwd("testdir")
file.create("mytest.R")
list.files()
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R", "mytest2.R")
file.copy("mytest2.R", "mytest3.R")
file.path("mytest3.R")
file.("folder1", "folder2")
file.path("folder1", "folder2")
?dir.create
dir.create(file.path("testdir2", "testdir3"), recursive = TRUE)
setwd(old.dir)
2
1
3
1:20
pi:10
15:1
?':'
seq()
seq(1, 20)
seq(0, 10, by=0.5)
my_seq <- seq(5, 10, length=30)
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0, times = 40)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)
1