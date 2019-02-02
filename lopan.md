
#### Primary Timeline 
 
```haskell
roadmap = ("2019", "Haskell MaximizeDecentralizationDelivery") : -- Going straight to achieve
    [
        ("2016-2018", "Opensource Bitshares Telegram VueJs Actors Distributed"), -- Fresh Ideas and Perception
        ("2014-2016", "Golang Instagram ReverseEngeneering NoSQL"), -- Startups mostly 
        ("2012-2014", "Enterprise PHP SQL ProjectManagement Linux"), -- Large ERP and Educational platforms
        ("2007-2012", "ComputerScience") -- Mostly learning, some practice with C++
    ]
    
-- Or skills in ascending order
historical = map (\ (_, tags) -> words tags)  $ foldl (flip (:)) [] cv
```

#### Todo: cover leter