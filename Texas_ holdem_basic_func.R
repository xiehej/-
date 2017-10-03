#点数如果是14，返回“Ace”
Ace = function(num){
    if(num==14){
        return("Ace")
    }
    else{
        return(as.character(num))
    }
}

#7选5
#input 是一个list, 如temp = list(spadeAce,heart13,clubAce,spade10,spade11,heart8, diamond6)
fiveInSeven = function(sevencards){
    if(length(sevencards)!=7){
        stop("input is not a 7 cards list")
    }
    name1 = paste(sevencards[[1]]$tag,Ace(sevencards[[1]]$num), sep = "")
    name2 = paste(sevencards[[2]]$tag,Ace(sevencards[[2]]$num), sep = "")
    name3 = paste(sevencards[[3]]$tag,Ace(sevencards[[3]]$num), sep = "")
    name4 = paste(sevencards[[4]]$tag,Ace(sevencards[[4]]$num), sep = "")
    name5 = paste(sevencards[[5]]$tag,Ace(sevencards[[5]]$num), sep = "")
    name6 = paste(sevencards[[6]]$tag,Ace(sevencards[[6]]$num), sep = "")
    name7 = paste(sevencards[[7]]$tag,Ace(sevencards[[7]]$num), sep = "")
    name = c(name1,name2,name3,name4,name5,name6,name7)
    
    fiveCardsCombination = list(1:21)
    i = 1
    for(a in 1:3){
        for(b in (a+1):4){
            for(c in (b+1):5){
                for(d in (c+1):6){
                    for(e in (d+1):7){
                        fiveCardsCombination[[i]] = list(sevencards[[a]],
                                                         sevencards[[b]],
                                                         sevencards[[c]],
                                                         sevencards[[d]],
                                                         sevencards[[e]])
                        names(fiveCardsCombination[[i]]) = c(name[a],
                                                             name[b],
                                                             name[c],
                                                             name[d],
                                                             name[e])
                        i = i+1
                    }
                }
            }
        }
    }
    
    return(fiveCardsCombination)
}

#把个位数变成两位的character，如1变成“01”
twonum = function(num){
    if(num<2||num>14){
        stop("input must be 2 - 14, 14 represent Ace")
    }
    if(num<10){
        return(paste("0",num, sep = ""))
    }
    
    if(num>=10){
        return(num)
    }
}


#判断5张牌的价值
ValueFive = function(fivecards){
    if(length(fivecards)!=5){
        stop("input must be a lit of 5 cards")
    }
    card1 = fivecards[[1]]
    card2 = fivecards[[2]]
    card3 = fivecards[[3]]
    card4 = fivecards[[4]]
    card5 = fivecards[[5]]

    tag = c(card1$tag,
            card2$tag,
            card3$tag,
            card4$tag,
            card5$tag)
    
    number = c(card1$num,
               card2$num,
               card3$num,
               card4$num,
               card5$num)
    
    sortNum = sort(number)
    
    singleValue = paste(twonum(sortNum[5]),
                        twonum(sortNum[4]),
                        twonum(sortNum[3]),
                        twonum(sortNum[2]),
                        twonum(sortNum[1]),sep = "")
   
    
    #start two check pairs
    numPairs = 0
    
    for(a in 1:4){
        for(b in (a+1):5){
            if(number[a]==number[b]){
                numPairs = numPairs + 1
            }
        }
    }
    
    if(numPairs==6){
        return(list(combo = "Four of a Kind",
                    value = as.numeric(paste("17",singleValue,sep = ""))))
    }
    
    else if(numPairs==4){
        return(list(combo = "Full House, 3+2",
                    value = as.numeric(paste("16",singleValue,sep = ""))))
    }
    
    else if(numPairs==3){
        return(list(combo = "Three of a Kind",
                    value = as.numeric(paste("13",singleValue,sep = ""))))
    }
    
    else if(numPairs==2){
        return(list(combo = "Two Pairs",
                    value = as.numeric(paste("12",singleValue,sep = ""))))
    }
    
    else if(numPairs==1){
        return(list(combo = "One Pair",
                    value = as.numeric(paste("11",singleValue,sep = ""))))
    }

    
    
    #check straight and flush
    isFlush = FALSE
    isStraight = FALSE
    
    if(length(unique(tag))==1){
        isFlush = TRUE
    }
    
    
    if(sortNum[2]==sortNum[1]+1
       &&sortNum[3]==sortNum[2]+1
       &&sortNum[4]==sortNum[3]+1
       &&sortNum[5]==sortNum[4]+1){
        isStraight = TRUE
    }
    if(sortNum[1]==2
       &&sortNum[2]==3
       &&sortNum[3]==4
       &&sortNum[4]==5
       &&sortNum[5]==14){
        isStraight = TRUE
    }
    
    
    
    if(isStraight&&isFlush){
        return(list(combo = "Straight Flush",
                    value = as.numeric(paste("18",singleValue,sep = ""))))
    }
    
    else if(isStraight){
        return(list(combo = "Straight",
                    value = as.numeric(paste("14",singleValue,sep = ""))))
    }
    
    else if(isFlush){
        return(list(combo = "Flush",
                    value = as.numeric(paste("15",singleValue,sep = ""))))
    }
    
    else{
        return(list(combo = "High Cards, no combo",
                    value = as.numeric(paste("10",singleValue,sep = ""))))
    }
    
}


#给定7张牌，计算最大组合的价值和combo名称
ValueSeven = function(sevencards){
    scorelist = rep(0,21)
    combolist = rep(0,21)
    
    fiveCardsCombination = fiveInSeven(sevencards)
    
    for(i in 1:21){
        result = ValueFive(fiveCardsCombination[[i]])
        scorelist[i] = result$value
        combolist[i] = result$combo
    }
    
    maxscore = max(scorelist)
    place = match(maxscore,scorelist)
    combo = combolist[place]
    
    return(list(combo = combo,
                value = maxscore))
}
