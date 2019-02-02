```haskell
cv = ("2019", "Haskell MaximizeDecentralizationDelivery") :
    [
        ("2016-2018", "Opensource Bitshares Telegram  Vuex Actors Crypto"),
        ("2014-2016", "Golang Instagram ReverseEngeneering"),
        ("2012-2014", "Enterprise PHP ProjectManagement"),
        -- 2 year of
        ("2007-2012", "ComputerScience")
        -- Got BS and Master, no
    ]
    
-- Or skills in ascending order
historical = map (\ (_, tags) -> words tags)  $ foldl (flip (:)) [] cv
```
