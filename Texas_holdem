creatCardsAndDeck=function(nothing){
    spadeAce = list(tag = "spade", num = 14, tag1 = 4)
    spade2 = list(tag = "spade", num = 2, tag1 = 4)
    spade3 = list(tag = "spade", num = 3, tag1 = 4)
    spade4 = list(tag = "spade", num = 4, tag1 = 4)
    spade5 = list(tag = "spade", num = 5, tag1 = 4)
    spade6 = list(tag = "spade", num = 6, tag1 = 4)
    spade7 = list(tag = "spade", num = 7, tag1 = 4)
    spade8 = list(tag = "spade", num = 8, tag1 = 4)
    spade9 = list(tag = "spade", num = 9, tag1 = 4)
    spade10 = list(tag = "spade", num = 10, tag1 = 4)
    spade11 = list(tag = "spade", num = 11, tag1 = 4)
    spade12 = list(tag = "spade", num = 12, tag1 = 4)
    spade13 = list(tag = "spade", num = 13, tag1 = 4)
    
    heartAce = list(tag = "heart", num = 14, tag1 = 3)
    heart2 = list(tag = "heart", num = 2, tag1 = 3)
    heart3 = list(tag = "heart", num = 3, tag1 = 3)
    heart4 = list(tag = "heart", num = 4, tag1 = 3)
    heart5 = list(tag = "heart", num = 5, tag1 = 3)
    heart6 = list(tag = "heart", num = 6, tag1 = 3)
    heart7 = list(tag = "heart", num = 7, tag1 = 3)
    heart8 = list(tag = "heart", num = 8, tag1 = 3)
    heart9 = list(tag = "heart", num = 9, tag1 = 3)
    heart10 = list(tag = "heart", num = 10, tag1 = 3)
    heart11 = list(tag = "heart", num = 11, tag1 = 3)
    heart12 = list(tag = "heart", num = 12, tag1 = 3)
    heart13 = list(tag = "heart", num = 13, tag1 = 3)
    
    clubAce = list(tag = "club", num = 14, tag1 = 2)
    club2 = list(tag = "club", num = 2, tag1 = 2)
    club3 = list(tag = "club", num = 3, tag1 = 2)
    club4 = list(tag = "club", num = 4, tag1 = 2)
    club5 = list(tag = "club", num = 5, tag1 = 2)
    club6 = list(tag = "club", num = 6, tag1 = 2)
    club7 = list(tag = "club", num = 7, tag1 = 2)
    club8 = list(tag = "club", num = 8, tag1 = 2)
    club9 = list(tag = "club", num = 9, tag1 = 2)
    club10 = list(tag = "club", num = 10, tag1 = 2)
    club11 = list(tag = "club", num = 11, tag1 = 2)
    club12 = list(tag = "club", num = 12, tag1 = 2)
    club13 = list(tag = "club", num = 13, tag1 = 2)
    
    diamondAce = list(tag = "diamond", num = 14, tag1 = 1)
    diamond2 = list(tag = "diamond", num = 2, tag1 = 1)
    diamond3 = list(tag = "diamond", num = 3, tag1 = 1)
    diamond4 = list(tag = "diamond", num = 4, tag1 = 1)
    diamond5 = list(tag = "diamond", num = 5, tag1 = 1)
    diamond6 = list(tag = "diamond", num = 6, tag1 = 1)
    diamond7 = list(tag = "diamond", num = 7, tag1 = 1)
    diamond8 = list(tag = "diamond", num = 8, tag1 = 1)
    diamond9 = list(tag = "diamond", num = 9, tag1 = 1)
    diamond10 = list(tag = "diamond", num = 10, tag1 = 1)
    diamond11 = list(tag = "diamond", num = 11, tag1 = 1)
    diamond12 = list(tag = "diamond", num = 12, tag1 = 1)
    diamond13 = list(tag = "diamond", num = 13, tag1 = 1)
    
    
    deck = list(spadeAce,
                spade2,
                spade3,
                spade4,
                spade5,
                spade6,
                spade7,
                spade8,
                spade9,
                spade10,
                spade11,
                spade12,
                spade13,
                heartAce,
                heart2,
                heart3,
                heart4,
                heart5,
                heart6,
                heart7,
                heart8,
                heart9,
                heart10,
                heart11,
                heart12,
                heart13,
                clubAce,
                club2,
                club3,
                club4,
                club5,
                club6,
                club7,
                club8,
                club9,
                club10,
                club11,
                club12,
                club13,
                diamondAce,
                diamond2,
                diamond3,
                diamond4,
                diamond5,
                diamond6,
                diamond7,
                diamond8,
                diamond9,
                diamond10,
                diamond11,
                diamond12,
                diamond13)
    
    names(deck) = c("spadeAce",paste("spade",2:13,sep = ""),
                    "heartAce",paste("heart",2:13,sep = ""),
                    "clubAce",paste("club",2:13,sep = ""),
                    "diamondAce",paste("diamond",2:13,sep = ""))
}



# newdeck = sample(deck)
# names(newdeck)


# newdeck$"spade2" = NULL
# newdeck$diamond3 = NULL

#计算各牌的位置，便于从初始顺序卡组里抽出
calculatePlace = function(card){
    tag = card$tag
    num = card$num
    if(num == 14){
        num = 1
    }
    
    if(tag == "spade"){
        return(num)
    }
    
    else if(tag == "heart"){
        return(13+num)
    }
    
    else if(tag == "club"){
        return(26+num)
    }
    
    else if(tag == "diamond"){
        return(39+num)
    }
}

#根据已有牌，用蒙特卡罗模拟计算出各种组合的概率
calculateProbability = function(flop, hand, numSimulation){
    numflop = length(flop)
    if(numflop == 5){
        return("All cards are dealt")
    }
    # if(numflop<0||numflop==1||numflop==2||numflop>5){
    #     stop("incorrect number of flop")
    # }
    
    newdeck = deck
    sevenCards = hand
    #把已发的牌抽出来，放进sevenCards里
    if(numflop!=0){
        for(i in 1:numflop){
            tag = flop[[i]]$tag
            num = flop[[i]]$num
            place = 0
            for(j in 1:length(newdeck)){
                if(newdeck[[j]]$num==num&&newdeck[[j]]$tag==tag){
                    place = j
                    break
                }
            }
        
            newdeck[[place]] = NULL
        
            sevenCards[[2+i]] = flop[[i]]
        }
    }
    #把手牌从牌库抽出来
    for(i in 1:2){
        tag = hand[[i]]$tag
        num = hand[[i]]$num
        place = 0
        for(j in 1:length(newdeck)){
            if(newdeck[[j]]$num==num&&newdeck[[j]]$tag==tag){
                place = j
                break
            }
        }
        newdeck[[place]] = NULL
    }
    
    
    
    #start simulation
    remaining = 5-numflop #random deal this num of cards
    already = numflop + 2
    

    
    simulateComboList = rep(0,9)
    simulateScoreList = rep(0,numSimulation)
        
    for(m in 1:numSimulation){
        simulateDeck = sample(newdeck)#create new deck
        for(i in 1:remaining){
            sevenCards[[already+i]] = simulateDeck[[i]]
        }
        
        #
        result = ValueSeven(sevenCards)
        simulateScoreList[m] = result$value
        combo = result$combo
        if(combo=="High Cards, no combo"){
            simulateComboList[1] = simulateComboList[1] + 1
        }
        else if(combo=="One Pair"){
            simulateComboList[2] = simulateComboList[2] + 1
        }
        else if(combo=="Two Pairs"){
            simulateComboList[3] = simulateComboList[3] + 1
        }
        else if(combo=="Three of a Kind"){
            simulateComboList[4] = simulateComboList[4] + 1
        }
        else if(combo=="Straight"){
            simulateComboList[5] = simulateComboList[5] + 1
        }
        else if(combo=="Flush"){
            simulateComboList[6] = simulateComboList[6] + 1
        }
        else if(combo=="Full House, 3+2"){
            simulateComboList[7] = simulateComboList[7] + 1
        }
        else if(combo=="Four of a Kind"){
            simulateComboList[8] = simulateComboList[8] + 1
        }
        else if(combo=="Straight Flush"){
            simulateComboList[9] = simulateComboList[9] + 1
        }
        
        else{
            stop("unknown error")
        }
                                            
        # for(i in 1:remaining){
        #     simulateDeck[[1]] = NULL
        # }
        
        # combination = fiveInSeven(sevenCards)
        # 
        # scorelist = rep(0,21)
        # 
        # for(i in 1:21){
        #     scorelist[i] = ValueFive(combination[[i]])$value   
        # }
        # 
        # maxValue = max(scorelist)
        # simulateScoreList[m] = maxValue
        # 
        # if(maxValue<110000000000&&maxValue>100000000000){
        #     # simulateComboList[m] = "High Cards, no combo"
        #     simulateComboList[1] = simulateComboList[1] + 1
        # }
        # 
        # else if(maxValue<120000000000&&maxValue>110000000000){
        #     # simulateComboList[m] = "One Pair"
        #     simulateComboList[2] = simulateComboList[2] + 1
        # }
        # 
        # else if(maxValue<130000000000&&maxValue>120000000000){
        #     # simulateComboList[m] = "Two Pair"
        #     simulateComboList[3] = simulateComboList[3] + 1
        # }
        # 
        # else if(maxValue<140000000000&&maxValue>130000000000){
        #     # simulateComboList[m] = "Three of a Kind"
        #     simulateComboList[4] = simulateComboList[4] + 1
        # }
        # 
        # else if(maxValue<150000000000&&maxValue>140000000000){
        #     # simulateComboList[m] = "Straight"
        #     simulateComboList[5] = simulateComboList[5] + 1
        # }
        # 
        # else if(maxValue<160000000000&&maxValue>150000000000){
        #     # simulateComboList[m] = "Flush"
        #     simulateComboList[6] = simulateComboList[6] + 1
        # }
        # 
        # else if(maxValue<170000000000&&maxValue>160000000000){
        #     # simulateComboList[m] = "Full House, 3+2"
        #     simulateComboList[7] = simulateComboList[7] + 1
        # }
        # 
        # else if(maxValue<180000000000&&maxValue>170000000000){
        #     # simulateComboList[m] = "Four of a Kind"
        #     simulateComboList[8] = simulateComboList[8] + 1
        # }
        # 
        # else if(maxValue>180000000000){
        #     # simulateComboList[m] = "Straight Flush"
        #     simulateComboList[9] = simulateComboList[9] + 1
        # }
        # 
        # else{
        #     stop("unknown error")
        # }
        
    }
    
        
    quantileScore = quantile(simulateScoreList,c(.1,.25,.5,.75,.9))
    percentageOfCombo = as.matrix(t(simulateComboList/numSimulation))
    colnames(percentageOfCombo) = c("High Cards, no combo",
                                    "One Pair",
                                    "Two Pairs",
                                    "Three of a Kind",
                                    "Straight",
                                    "Flush",
                                    "Full House, 3+2",
                                    "Four of a Kind",
                                    "Straight Flush")
    return(list(quantileScore = quantileScore,
                meanScore = mean(simulateScoreList),
                simulateComboList = simulateComboList,
                percentageOfCombo = percentageOfCombo))
}


flop = list()
hand = list(spade4, heart5)
numSimulation = 10000

calculateProbability(flop, hand, numSimulation)


#通过蒙特卡罗模拟，计算当前手牌获胜概率，given一定数量的玩家
WinSimulation = function(numplayer, flop, hand, numSimulation){
    
    #create deck and deal cards to ourselves
    numflop = length(flop)
    if(numflop == 5){
        return("All cards are dealt")
    }
   
    newdeck = deck
    sevenCards = hand
    
    if(numflop!=0){
        for(i in 1:numflop){
            tag = flop[[i]]$tag
            num = flop[[i]]$num
            place = 0
            for(j in 1:length(newdeck)){
                if(newdeck[[j]]$num==num&&newdeck[[j]]$tag==tag){
                    place = j
                    break
                }
            }
            
            newdeck[[place]] = NULL
            
            sevenCards[[2+i]] = flop[[i]]
        }
    }
    for(i in 1:2){
        tag = hand[[i]]$tag
        num = hand[[i]]$num
        place = 0
        for(j in 1:length(newdeck)){
            if(newdeck[[j]]$num==num&&newdeck[[j]]$tag==tag){
                place = j
                break
            }
        }
        newdeck[[place]] = NULL
    }
    
    
    
    
    remaining = 5-numflop #random deal this num of cards
    already = numflop + 2
    
    numWins = 0
    winScore = rep(0,numSimulation)
    winCombo = rep(0,9)
    #start simulation
    for(a in 1:numSimulation){
        simulateDeck = sample(newdeck)
        finalFlop = flop
        
        #deal remaining flop, at the same time form the seven cards of ourselves
        for(i in 1:remaining){
            finalFlop[[numflop+i]] = simulateDeck[[i]]
            sevenCards[[already+i]] = simulateDeck[[i]]
        }    
        
        
        
        #track the number of cards dealt in each simulation
        dealCardsCount = remaining
        
        #deal hand cards for other player
        numOther = numplayer-1
        
        sevenCardsOfother = list()
        for(i in 1:numOther){
            sevenCardstemp = finalFlop
            sevenCardstemp[[6]] = simulateDeck[[dealCardsCount+1]]
            sevenCardstemp[[7]] = simulateDeck[[dealCardsCount+2]]
            dealCardsCount = dealCardsCount + 2
            
            sevenCardsOfother[[i]] = sevenCardstemp
        }
        
        
        #计算各玩家的最大分，皇城PK
        resultList = list()
        resultList[[1]] = ValueSeven(sevenCards)
        for(i in 1:numOther){
            resultList[[i+1]] = ValueSeven(sevenCardsOfother[[i]])
        }
        
        playerScore = rep(0,numplayer)
        playerCombo = rep(0,numplayer)
        for(i in 1:numplayer){
            playerScore[i] = resultList[[i]]$value
            playerCombo[i] = resultList[[i]]$combo
        }
        
        if(max(playerScore)==playerScore[1]){
            numWins = numWins + 1
            winScore[a] = playerScore[1]
            combo = playerCombo[[1]]
            if(combo=="High Cards, no combo"){
                winCombo[1] = winCombo[1] + 1
            }
            else if(combo=="One Pair"){
                winCombo[2] = winCombo[2] + 1
            }
            else if(combo=="Two Pairs"){
                winCombo[3] = winCombo[3] + 1
            }
            else if(combo=="Three of a Kind"){
                winCombo[4] = winCombo[4] + 1
            }
            else if(combo=="Straight"){
                winCombo[5] = winCombo[5] + 1
            }
            else if(combo=="Flush"){
                winCombo[6] = winCombo[6] + 1
            }
            else if(combo=="Full House, 3+2"){
                winCombo[7] = winCombo[7] + 1
            }
            else if(combo=="Four of a Kind"){
                winCombo[8] = winCombo[8] + 1
            }
            else if(combo=="Straight Flush"){
                winCombo[9] = winCombo[9] + 1
            }
            
            else{
                stop("unknown error")
            }
        }
        else{
            winScore[a] = NA 
        }
    }
    
    winScore = na.omit(winScore)
    
    quantileScore = quantile(winScore,c(.1,.25,.5,.75,.9))
    percentageOfCombo = as.matrix(t(winCombo/numSimulation))
    colnames(percentageOfCombo) = c("High Cards, no combo",
                                    "One Pair",
                                    "Two Pairs",
                                    "Three of a Kind",
                                    "Straight",
                                    "Flush",
                                    "Full House, 3+2",
                                    "Four of a Kind",
                                    "Straight Flush")
    return(list(numWins = numWins,
                percentageWins = numWins/numSimulation,
                quantileScore = quantileScore,
                meanScore = mean(winScore),
                winCombo = winCombo,
                percentageOfCombo = percentageOfCombo))
    
    
}


flop = list(spade2,club9,club10)
hand = list(spade6, heart11)
numSimulation = 10000

WinSimulation(numplayer = 3, flop, hand, numSimulation)


#通过蒙特卡罗模拟，计算当前手牌获胜概率超过多少的时候才跟注
#given一定数量的玩家和已有flop #由于需要模拟次数太多，该函数还未做完
OptimalProbabilitySimulation = 
    function(numplayer,
             numflop,
             numSimulationGame,
             numSimulationBet,
             prob){
    
    remaining = 5-numflop #random deal this num of cards
    already = numflop + 2
    
    # numWins = 0
    # winScore = rep(0,numSimulation)
    # winCombo = rep(0,9)
    
    #start simulation of existing cards
    for(a in 1:numSimulationGame){
        simulateDeck = sample(deck)
        flop = list()
        hand = list()
        #deal flop, at the same time form the seven cards of ourselves
        for(i in 1:numflop){
            flop[[i]] = simulateDeck[[i]]
        }
        
        hand[[1]] = simulateDeck[[numflop+1]]
        hand[[2]] = simulateDeck[[numflop+2]]
        
        winProbability = 
            WinSimulation(numplayer, flop, hand, 50000)$percentageWins
        
        #if winprobability is small, discard and do not bet
        if(winProbability < prob) next
        
        
        #start to simulate bet
        for(i in 1:remaining){
            finalFlop[[numflop+i]] = simulateDeck[[i]]
            sevenCards[[already+i]] = simulateDeck[[i]]
        }    
        
        
        
        #track the number of cards dealt in each simulation
        dealCardsCount = remaining
        
        #deal hand cards for other player
        numOther = numplayer-1
        
        sevenCardsOfother = list()
        for(i in 1:numOther){
            sevenCardstemp = finalFlop
            sevenCardstemp[[6]] = simulateDeck[[dealCardsCount+1]]
            sevenCardstemp[[7]] = simulateDeck[[dealCardsCount+2]]
            dealCardsCount = dealCardsCount + 2
            
            sevenCardsOfother[[i]] = sevenCardstemp
        }
        
        
        #计算各玩家的最大分，皇城PK
        resultList = list()
        resultList[[1]] = ValueSeven(sevenCards)
        for(i in 1:numOther){
            resultList[[i+1]] = ValueSeven(sevenCardsOfother[[i]])
        }
        
        playerScore = rep(0,numplayer)
        playerCombo = rep(0,numplayer)
        for(i in 1:numplayer){
            playerScore[i] = resultList[[i]]$value
            playerCombo[i] = resultList[[i]]$combo
        }
        
        if(max(playerScore)==playerScore[1]){
            numWins = numWins + 1
            winScore[a] = playerScore[1]
            combo = playerCombo[[1]]
            if(combo=="High Cards, no combo"){
                winCombo[1] = winCombo[1] + 1
            }
            else if(combo=="One Pair"){
                winCombo[2] = winCombo[2] + 1
            }
            else if(combo=="Two Pairs"){
                winCombo[3] = winCombo[3] + 1
            }
            else if(combo=="Three of a Kind"){
                winCombo[4] = winCombo[4] + 1
            }
            else if(combo=="Straight"){
                winCombo[5] = winCombo[5] + 1
            }
            else if(combo=="Flush"){
                winCombo[6] = winCombo[6] + 1
            }
            else if(combo=="Full House, 3+2"){
                winCombo[7] = winCombo[7] + 1
            }
            else if(combo=="Four of a Kind"){
                winCombo[8] = winCombo[8] + 1
            }
            else if(combo=="Straight Flush"){
                winCombo[9] = winCombo[9] + 1
            }
            
            else{
                stop("unknown error")
            }
        }
        else{
            winScore[a] = NA 
        }
    }
    
    winScore = na.omit(winScore)
    
    quantileScore = quantile(winScore,c(.1,.25,.5,.75,.9))
    percentageOfCombo = as.matrix(t(winCombo/numSimulation))
    colnames(percentageOfCombo) = c("High Cards, no combo",
                                    "One Pair",
                                    "Two Pairs",
                                    "Three of a Kind",
                                    "Straight",
                                    "Flush",
                                    "Full House, 3+2",
                                    "Four of a Kind",
                                    "Straight Flush")
    return(list(numWins = numWins,
                percentageWins = numWins/numSimulation,
                quantileScore = quantileScore,
                meanScore = mean(winScore),
                winCombo = winCombo,
                percentageOfCombo = percentageOfCombo))
    
    
}
