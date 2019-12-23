library(tidyverse)

digits <- function(x) { as.numeric(unlist(strsplit(as.character(x),""))) }

fromdigits <- function(x) {as.numeric(paste(as.character(x),collapse="")) }

decode <- function(x) { ifelse(x > 4, digits(x) %>% tail(2) %>% fromdigits, x) }

modes <- function(x) {
  if (x > 4) {
    digits(x) %>% head(-2) %>% rev
  } else {
    vector()
  }
  # ifelse gives strange results, don't know what I'm doing wrong
  #ifelse(x > 4, digits(x) %>% head(-2) %>% rev, vector())
}
ops5 <- c(3,225,1,225,6,6,1100,1,238,225,104,0,2,106,196,224,101,-1157,224,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1002,144,30,224,1001,224,-1710,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,101,82,109,224,1001,224,-111,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1102,10,50,225,1102,48,24,224,1001,224,-1152,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1102,44,89,225,1101,29,74,225,1101,13,59,225,1101,49,60,225,1101,89,71,224,1001,224,-160,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1101,27,57,225,102,23,114,224,1001,224,-1357,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,1001,192,49,224,1001,224,-121,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,81,72,225,1102,12,13,225,1,80,118,224,1001,224,-110,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,226,224,102,2,223,223,1005,224,329,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,359,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,389,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,404,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,449,1001,223,1,223,107,226,226,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,479,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,509,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1108,677,226,224,1002,223,2,223,1006,224,539,101,1,223,223,1108,677,677,224,102,2,223,223,1005,224,554,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,569,101,1,223,223,8,677,677,224,102,2,223,223,1005,224,584,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,599,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,1008,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,644,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,659,101,1,223,223,1007,677,226,224,1002,223,2,223,1005,224,674,1001,223,1,223,4,223,99,226)

intcodecomputer <- function(ops, inputs) {
  inputs_pointer <- 1
  pointer <- 1
  res <- 0
  p <- FALSE
  while (!is.null(res)) {
    o <- ops[pointer]
    op <- decode(o)
    m <- modes(o)
    p1 <-
      ifelse(length(m) > 0 &
               m[1] == 1, ops[pointer + 1], ops[ops[pointer + 1] + 1])
    p2 <-
      ifelse(length(m) > 1 &
               m[2] == 1, ops[pointer + 2], ops[ops[pointer + 2] + 1])
    
    res <- switch (op,
                   { # 1 add
                     if (p) { print(c(op,p1,p2)) }
                     dest <- ops[pointer + 3] + 1
                     ops[dest] <- (p1 + p2)
                     pointer <- pointer + 4
                     1
                   },
                   { # 2 multiply
                     if (p) { print(c(op,p1,p2)) }
                     dest <- ops[pointer + 3] + 1
                     ops[dest] <- (p1 * p2)
                     pointer <- pointer + 4
                     2
                   },
                   { # 3 take input
                     dest <- ops[pointer + 1] + 1
                     ops[dest] <- inputs[inputs_pointer]
                     if (p) { print(c(op,dest,ops[dest])) }
                     inputs_pointer <- inputs_pointer + 1
                     pointer <- pointer + 2
                     3
                   },
                   { # 4 print output
                     print(c(op, p1))
                     pointer <- pointer + 2
                     4
                   },
                   { # Opcode 5 is jump-if-true: if the first parameter is non-zero, 
                     # it sets the instruction pointer to the value from the second 
                     # parameter. Otherwise, it does nothing.
                     if (p) { print(c(op,p1,p2)) }
                     ifelse(p1!=0, pointer<-(p2+1),pointer<-pointer+3)
                     5
                   },
                   { # Opcode 6 is jump-if-false: if the first parameter is zero, 
                     # it sets the instruction pointer to the value from the second 
                     # parameter. Otherwise, it does nothing.
                     if (p) { print(c(op,p1,p2)) }
                     ifelse(p1==0, pointer<-(p2+1),pointer<-pointer+3)
                     6
                   },
                   { # Opcode 7 is less than: if the first parameter is less than the second 
                     # parameter, it stores 1 in the position given by the third parameter. 
                     # Otherwise, it stores 0.
                     if (p) { print(c(op,p1,p2)) }
                     dest <- ops[pointer + 3] + 1
                     ifelse(p1<p2,ops[dest]<-1,ops[dest]<-0)
                     pointer <- pointer + 4
                     7
                   },
                   { # Opcode 8 is equals: if the first parameter is equal to the second 
                     # parameter, it stores 1 in the position given by the third parameter. 
                     # Otherwise, it stores 0.
                     if (p) { print(c(op,p1,p2)) }
                     dest <- ops[pointer + 3] + 1
                     ifelse(p1==p2,ops[dest]<-1,ops[dest]<-0)
                     pointer <- pointer + 4
                     8
                   })
  }
  
}
inputs5a <- c(1)
result5a <- intcodecomputer(ops5, inputs5a)

opstest1 <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
intcodecomputer(opstest1, 79)

inputs5b <-5
result5b <- intcodecomputer(ops5, inputs5b)
