lend amount balance = 
    let reserve = 100
        newBalance = balance - amount
    in if balance < reserve
        then Nothing
        else Just newBalance

lend2 amount balance =
    if balance < reserve 
        then Nothing 
        else Just newBalance
    where 
        reserve = 100 
        newBalance = balance - amount