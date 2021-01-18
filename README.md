
``` r
# Load the libraries
library(rvest)
```

    ## Loading required package: xml2

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(stringr)
library(ggplot2)
```

This is a project combined with web scraping and data cleaning. The data
sets are from two different website. One is called `serebii.net`that
collects Pokemon stats and info and the other website is named `IGN`, a
famous games review website.

![](https://assets.pokemon.com/assets/cms2/img/video-games/_tiles/strategy/sword-shield/starters-guide/01/sword-shield-starters-guide-169-en.jpg)

## Web Scraping

``` r
link <- "https://www.serebii.net/swordshield/galarpokedex.shtml"
pokemon_ss <- read_html(link) %>%  # web scraping
  html_nodes("table.tab")%>%   # CSS selector
  html_table(fill=TRUE)%>%.[[2]] 
head(pokemon_ss)
```

    ##     X1   X2   X3                 X4                    X5         X6         X7
    ## 1  No.  Pic Name          Abilities                  Type Base Stats Base Stats
    ## 2  No.  Pic Name          Abilities                  Type         HP        Att
    ## 3 #001              Grookeyサルノリ Overgrow Grassy Surge                    50
    ## 4      <NA> <NA>               <NA>                  <NA>       <NA>       <NA>
    ## 5 #002           Thwackeyバチンキー Overgrow Grassy Surge                    70
    ## 6      <NA> <NA>               <NA>                  <NA>       <NA>       <NA>
    ##           X8         X9        X10        X11 X12
    ## 1 Base Stats Base Stats Base Stats Base Stats  NA
    ## 2        Def      S.Att      S.Def        Spd  NA
    ## 3         65         50         40         40  65
    ## 4       <NA>       <NA>       <NA>       <NA>  NA
    ## 5         85         70         55         60  80
    ## 6       <NA>       <NA>       <NA>       <NA>  NA

The pokemon\_ss data set looks pretty bad and we are going to do some
data wrangling soon. This data does not have the type column since
serebii.net used pictures, and R web scraping cannot transform pictures
into text, however the IGN has another data set which includes the type
column. Thus, we can use Pokemon ID to merge two data sets together.

``` r
link_2 <- "https://www.ign.com/wikis/pokemon-sword-shield/List_of_Pokemon_(Pokedex)#Galarian_Pokedex"
pokemon_type <- read_html(link_2) %>%  # web scraping
  html_nodes("table")%>%   # CSS selector
  html_table(fill=TRUE)%>%.[[1]]
head(pokemon_type)
```

    ##   #   Pokemon  Type Location(s) Found
    ## 1 1   Grookey Grass   Starter Pokemon
    ## 2 2  Thwackey Grass    Evolve Grookey
    ## 3 3 Rillaboom Grass   Evolve Thwackey
    ## 4 4 Scorbunny  Fire   Starter Pokemon
    ## 5 5    Raboot  Fire  Evolve Scorbunny
    ## 6 6 Cinderace  Fire  Evolve Cinderace

## Data Cleaning

``` r
colnames(pokemon_ss) # check column names 
```

    ##  [1] "X1"  "X2"  "X3"  "X4"  "X5"  "X6"  "X7"  "X8"  "X9"  "X10" "X11" "X12"

``` r
pokemon_ss <- pokemon_ss[,-6]   # remove sixth column
pokemon_ss <- pokemon_ss[,-2:-3] # remove second, third column

colnames(pokemon_ss) <- c("No.","Name","Abilities","HP","Attack","Defence","Sp_Attack","Sp.Defence","Speed")
# change column name

pokemon_ss <- pokemon_ss[-1:-2,] # remove first, second rows

pokemon_ss <- na.omit(pokemon_ss) # remove all the NA rows 
```

``` r
pokemon_ss$No. <-  str_replace_all(pokemon_ss$No.,"\\#","") # remove # at the beginning of the No. column

pokemon_ss$No. <- as.numeric(pokemon_ss$No.) # change into numeric
```

``` r
unique(pokemon_ss$No.)   # there are 400 unique observations
```

    ##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
    ##  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
    ##  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
    ##  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
    ##  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
    ##  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
    ## [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
    ## [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
    ## [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
    ## [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
    ## [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
    ## [199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
    ## [217] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234
    ## [235] 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
    ## [253] 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270
    ## [271] 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288
    ## [289] 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306
    ## [307] 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324
    ## [325] 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342
    ## [343] 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360
    ## [361] 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378
    ## [379] 379 380 381 382 383 384 385 386 387 388 389 390 391 392 393 394 395 396
    ## [397] 397 398 399 400

``` r
unique(pokemon_type$`#`) # there are 400 unique observations
```

    ##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
    ##  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
    ##  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
    ##  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
    ##  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
    ##  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
    ## [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
    ## [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
    ## [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
    ## [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
    ## [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
    ## [199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
    ## [217] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234
    ## [235] 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
    ## [253] 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270
    ## [271] 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288
    ## [289] 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306
    ## [307] 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324
    ## [325] 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342
    ## [343] 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360
    ## [361] 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378
    ## [379] 379 380 381 382 383 384 385 386 387 388 389 390 391 392 393 394 395 396
    ## [397] 397 398 399 400

``` r
pokemon_list <- merge(pokemon_type, pokemon_ss, by.x = "#", by.y = "No.")  # merge two data sets together

colnames(pokemon_list) <- tolower(colnames(pokemon_list)) # make column names lower case

pokemon_list <- pokemon_list%>%
  rename(No.="#")  # rename column name
  
pokemon_list <- pokemon_list%>%  
  rename(locations_found="location(s) found")# rename the column name

summary(as.factor(pokemon_list$No.)) # use factor we can see which # has duplicates No.182 and No.327
```

    ##     182     327       1       2       3       4       5       6       7       8 
    ##       2       2       1       1       1       1       1       1       1       1 
    ##       9      10      11      12      13      14      15      16      17      18 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      19      20      21      22      23      24      25      26      27      28 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      29      30      31      32      33      34      35      36      37      38 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      39      40      41      42      43      44      45      46      47      48 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      49      50      51      52      53      54      55      56      57      58 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      59      60      61      62      63      64      65      66      67      68 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      69      70      71      72      73      74      75      76      77      78 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      79      80      81      82      83      84      85      86      87      88 
    ##       1       1       1       1       1       1       1       1       1       1 
    ##      89      90      91      92      93      94      95      96      97 (Other) 
    ##       1       1       1       1       1       1       1       1       1     301

``` r
# we can pull No.182 and No.327 out 

pokemon_list[pokemon_list$No.==182,]
```

    ##     No.         pokemon   type         locations_found           name
    ## 182 182 Galarian Meowth  Steel Route 4Max Raid Battles Meowthニャース
    ## 183 182          Meowth Normal               NPC Trade Meowthニャース
    ##                      abilities hp attack defence sp_attack sp.defence speed
    ## 182 Pickup Tough Claws Unnerve 50     65      55        40         40    40
    ## 183 Pickup Tough Claws Unnerve 50     65      55        40         40    40

``` r
pokemon_list[pokemon_list$No.==327,]
```

    ##     No.         pokemon        type         locations_found           name
    ## 328 327 Galarian Yamask GroundGhost Route 6Max Raid Battles Yamaskデスマス
    ## 329 327   Unovan Yamask       Ghost               NPC Trade Yamaskデスマス
    ##            abilities hp attack defence sp_attack sp.defence speed
    ## 328 Wandering Spirit 38     55      85        30         65    30
    ## 329 Wandering Spirit 38     55      85        30         65    30

``` r
#Both Pokemons have different types although they do have the same stats, we still decide to keep them in the data set
```

``` r
colnames(pokemon_list) <- c("No.","pokemon_name","type","locations_found","name",
                             "abilities","hp","attack", "defence","sp_attack",
                             "sp_defence","speed") 

pokemon_list <- pokemon_list[,-5] # remove duplicate column

col_order <- c("No.","pokemon_name","type",
               "abilities","locations_found","hp","attack", 
               "defence","sp_attack",
               "sp_defence","speed")  # set up correct column order

pokemon_list <- pokemon_list[, col_order] # organize the data set with correct column order

pokemon_list <- pokemon_list%>%
  mutate(No.=as.character(No.),
         type=as.factor(type),
         abilities=as.factor(abilities),
         hp=as.numeric(hp),
         attack=as.numeric(attack),
         defence=as.numeric(defence),
         sp_attack=as.numeric(sp_attack),
         sp_defence=as.numeric(sp_defence),
         speed=as.numeric(speed))

summary(pokemon_list) 
```

    ##      No.            pokemon_name             type    
    ##  Length:402         Length:402         Water   : 25  
    ##  Class :character   Class :character   Fighting: 21  
    ##  Mode  :character   Mode  :character   Psychic : 18  
    ##                                        Grass   : 14  
    ##                                        Normal  : 14  
    ##                                        Ice     : 13  
    ##                                        (Other) :297  
    ##                              abilities   locations_found          hp        
    ##  Levitate                         : 11   Length:402         Min.   :  1.00  
    ##  Big Pecks Super Luck Rivalry     :  3   Class :character   1st Qu.: 50.00  
    ##  Blaze Libero                     :  3   Mode  :character   Median : 65.00  
    ##  Blaze Solar Power                :  3                      Mean   : 66.33  
    ##  Bulletproof Soundproof Overcoat  :  3                      3rd Qu.: 78.00  
    ##  Chlorophyll Early Bird Pickpocket:  3                      Max.   :190.00  
    ##  (Other)                          :376                                      
    ##      attack          defence         sp_attack        sp_defence    
    ##  Min.   : 10.00   Min.   : 15.00   Min.   : 10.00   Min.   : 20.00  
    ##  1st Qu.: 50.00   1st Qu.: 50.00   1st Qu.: 45.00   1st Qu.: 50.00  
    ##  Median : 70.00   Median : 66.50   Median : 60.00   Median : 65.00  
    ##  Mean   : 73.59   Mean   : 71.96   Mean   : 64.97   Mean   : 68.76  
    ##  3rd Qu.: 95.00   3rd Qu.: 90.00   3rd Qu.: 85.00   3rd Qu.: 85.00  
    ##  Max.   :147.00   Max.   :230.00   Max.   :145.00   Max.   :230.00  
    ##                                                                     
    ##      speed       
    ##  Min.   :  5.00  
    ##  1st Qu.: 40.00  
    ##  Median : 60.00  
    ##  Mean   : 61.32  
    ##  3rd Qu.: 80.00  
    ##  Max.   :160.00  
    ## 

``` r
pokemon_list$type <- str_replace_all(pokemon_list$type,
                                     "(?<=[a-z])(?=[A-Z])", #find small letters connected with capital letter                                                             
                                     "\n") # separate the pattern with space
pokemon_list <-  pokemon_list%>%
  separate(col=type,
           into=c("type_1","type_2"), # separate the type column into two columns between space 
           sep="\n")
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 195 rows [1, 2,
    ## 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 16, 21, 22, 24, 25, 29, 30, 34, ...].

``` r
pokemon_list <-  pokemon_list%>%  
  mutate(type_1=as.factor(type_1),  # change type_1 into factor
         type_2=as.factor(type_2)) # change type_2 into factor

print(unique(pokemon_list$type_1))  # we found a typo there is one category named poison and the other named posion
```

    ##  [1] Grass    Fire     Water    Bug      Normal   Flying   Dark     Electric
    ##  [9] Ice      Ground   Psychic  Fighting Steel    Ghost    Poison   Rock    
    ## [17] Fairy    Dragon   Posion  
    ## 19 Levels: Bug Dark Dragon Electric Fairy Fighting Fire Flying Ghost ... Water

``` r
print(unique(pokemon_list$type_2)) # no duplicates in this column, but NA does not help us too much let's change those NAs into None
```

    ##  [1] <NA>     Psychic  Flying   Electric Steel    Normal   Grass    Dark    
    ##  [9] Rock     Ground   Poison   Ghost    Fighting Ice      Fairy    Bug     
    ## [17] Fire     Dragon   Water   
    ## 18 Levels: Bug Dark Dragon Electric Fairy Fighting Fire Flying Ghost ... Water

``` r
pokemon_list$type_1[pokemon_list$type_1=="Posion"] <- "Poison" # change type_1 "posion" into correct name 

pokemon_list <- pokemon_list%>% 
  mutate(type_2=as.character(type_2))%>%  
  mutate(type_2=if_else(is.na(type_2),"None",type_2))%>% # change NAs to None
  mutate(type_2=as.factor(type_2))

summary(pokemon_list$type_2) # double check type_2 summary
```

    ##      Bug     Dark   Dragon Electric    Fairy Fighting     Fire   Flying 
    ##        7        9       16        6       18        9        7       30 
    ##    Ghost    Grass   Ground      Ice     None   Normal   Poison  Psychic 
    ##       16        8       17        8      195        6       12       13 
    ##     Rock    Steel    Water 
    ##        7       11        7

``` r
pokemon_list_long <- pokemon_list%>%  
  pivot_longer(cols=hp:speed,   # transform into long table
               names_to="stats_type",
               values_to="score")

head(pokemon_list_long)
```

    ## # A tibble: 6 x 8
    ##   No.   pokemon_name type_1 type_2 abilities    locations_found stats_type score
    ##   <chr> <chr>        <fct>  <fct>  <fct>        <chr>           <chr>      <dbl>
    ## 1 1     Grookey      Grass  None   Overgrow Gr… Starter Pokemon hp            50
    ## 2 1     Grookey      Grass  None   Overgrow Gr… Starter Pokemon attack        65
    ## 3 1     Grookey      Grass  None   Overgrow Gr… Starter Pokemon defence       50
    ## 4 1     Grookey      Grass  None   Overgrow Gr… Starter Pokemon sp_attack     40
    ## 5 1     Grookey      Grass  None   Overgrow Gr… Starter Pokemon sp_defence    40
    ## 6 1     Grookey      Grass  None   Overgrow Gr… Starter Pokemon speed         65

## Data Analysis

We can look at all the stats use boxplot for all the Pokemon

``` r
ggplot(pokemon_list_long, aes(x = score , y = stats_type)) +
  geom_boxplot() + 
  theme_minimal()+
  facet_wrap(~type_1)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(hrbrthemes)
```

    ## NOTE: Either Arial Narrow or Roboto Condensed fonts are required to use these themes.

    ##       Please use hrbrthemes::import_roboto_condensed() to install Roboto Condensed and

    ##       if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow

``` r
par(mfrow=c(2,2))
# Plot
pokemon_list_long %>%
  ggplot( aes(x=score, y=stats_type,fill=stats_type)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.8, option="A") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12)
    ) +
    ggtitle("All Pokemon in Galar Region Stats Boxplot") +
    xlab("Stats Score")+
    ylab("")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
g3 <- pokemon_list_long %>%
  filter(type_1=="Fairy" | type_2=="Fairy")%>%
  ggplot( aes(x=score, y=stats_type,fill=stats_type)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.8, option="A") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12)
    ) +
    ggtitle("All Fairy Pokemon in Galar Region Stats Boxplot") +
    xlab("Stats Score")+
    ylab("")
```

``` r
g4 <- pokemon_list_long %>%
  filter(type_1=="Dragon" | type_2=="Dragon")%>%
  ggplot( aes(x=score, y=stats_type,fill=stats_type)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.8, option="A") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12)
    ) +
    ggtitle("All Dragon Pokemon in Galar Region Stats Boxplot") +
    xlab("Stats Score")+
    ylab("")
```

``` r
library(ggpubr)
ggarrange(g3, g4,
          ncol = 2, nrow = 1)
```

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database
    
    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family
    ## 'Arial Narrow' not found in PostScript font database

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
primary_type <- pokemon_list%>%
  group_by(type_1)%>%
  summarise(count=n())%>%
  arrange(-count)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ggplot(data=primary_type, aes(x=type_1,y=count,fill=type_1))+
  geom_bar(position="dodge",stat="identity") +
  coord_flip()+
  ggtitle("Primary Type in Galar Region")+
  xlab("Primary Type")+
  ylab("Number of Pokemons")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
secondary_type <- pokemon_list%>%
  group_by(type_2)%>%
  summarise(count=n())%>%
  arrange(-count)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ggplot(data=secondary_type, aes(x=type_2,y=count,fill=type_2))+
  geom_bar(position="dodge",stat="identity") +
  coord_flip()+
  ggtitle("Secondary Type in Galar Region")+
  xlab("Secondary Type")+
  ylab("Number of Pokemons")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(dplyr)
pokemon_stats <- pokemon_list%>%
  select(where(is.numeric))

ggpairs(pokemon_stats) 
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
pokemon_list %>%
  select(where(is.numeric))%>%  # find all the numeric columns 
  cor()%>%
  corrplot(method = "number")
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
