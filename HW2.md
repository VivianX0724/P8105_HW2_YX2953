HW2
================
Vivian Xia
2024-09-30

Problem 1

``` r
# Load necessary libraries
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
# Read the CSV file
data <- read.csv("NYC_Transit_Subway_Entrance_And_Exit_Data.csv")

# Select relevant columns
cleaned_data <- data %>%
  select("Line","Station.Name", "Station.Latitude", "Station.Longitude", c("Route1":"Route11"),"Entry","Vending", "Entrance.Type","ADA") %>%
  
  # Convert Entry to logical (TRUE for "YES", FALSE for "NO")
  mutate(Entry = ifelse(Entry == "YES", TRUE, FALSE))

# View the data
cleaned_data
```

    ##                   Line                            Station.Name Station.Latitude
    ## 1             4 Avenue                                 25th St         40.66040
    ## 2             4 Avenue                                 25th St         40.66040
    ## 3             4 Avenue                                 36th St         40.65514
    ## 4             4 Avenue                                 36th St         40.65514
    ## 5             4 Avenue                                 36th St         40.65514
    ## 6             4 Avenue                                 45th St         40.64894
    ## 7             4 Avenue                                 45th St         40.64894
    ## 8             4 Avenue                                 45th St         40.64894
    ## 9             4 Avenue                                 45th St         40.64894
    ## 10            4 Avenue                                 53rd St         40.64507
    ## 11            4 Avenue                                 53rd St         40.64507
    ## 12            4 Avenue                                 53rd St         40.64507
    ## 13            4 Avenue                                 53rd St         40.64507
    ## 14            4 Avenue                                 53rd St         40.64507
    ## 15            4 Avenue                                 59th St         40.64136
    ## 16            4 Avenue                                 59th St         40.64136
    ## 17            4 Avenue                                 59th St         40.64136
    ## 18            4 Avenue                                 59th St         40.64136
    ## 19            4 Avenue                                 59th St         40.64136
    ## 20            4 Avenue                                 59th St         40.64136
    ## 21            4 Avenue                                 77th St         40.62974
    ## 22            4 Avenue                                 77th St         40.62974
    ## 23            4 Avenue                                 77th St         40.62974
    ## 24            4 Avenue                                 86th St         40.62269
    ## 25            4 Avenue                                 86th St         40.62269
    ## 26            4 Avenue                                 86th St         40.62269
    ## 27            4 Avenue                                 95th St         40.61662
    ## 28            4 Avenue                                 95th St         40.61662
    ## 29            4 Avenue                                 95th St         40.61662
    ## 30            4 Avenue                                 95th St         40.61662
    ## 31            4 Avenue                                 95th St         40.61662
    ## 32            4 Avenue                                  9th St         40.67085
    ## 33            4 Avenue                                  9th St         40.67085
    ## 34            4 Avenue                Atlantic Av-Barclays Ctr         40.68367
    ## 35            4 Avenue                            Bay Ridge Av         40.63497
    ## 36            4 Avenue                            Bay Ridge Av         40.63497
    ## 37            4 Avenue                            Bay Ridge Av         40.63497
    ## 38            4 Avenue                               DeKalb Av         40.69064
    ## 39            4 Avenue                               DeKalb Av         40.69064
    ## 40            4 Avenue                               DeKalb Av         40.69064
    ## 41            4 Avenue                               DeKalb Av         40.69064
    ## 42            4 Avenue                               DeKalb Av         40.69064
    ## 43            4 Avenue                               DeKalb Av         40.69064
    ## 44            4 Avenue                              Pacific St         40.68367
    ## 45            4 Avenue                              Pacific St         40.68367
    ## 46            4 Avenue                             Prospect Av         40.66541
    ## 47            4 Avenue                             Prospect Av         40.66541
    ## 48            4 Avenue                             Prospect Av         40.66541
    ## 49            4 Avenue                                Union St         40.67732
    ## 50            4 Avenue                                Union St         40.67732
    ## 51            4 Avenue                                Union St         40.67732
    ## 52            4 Avenue                                Union St         40.67732
    ## 53     42nd St Shuttle                           Grand Central         40.75277
    ## 54     42nd St Shuttle                           Grand Central         40.75277
    ## 55     42nd St Shuttle                           Grand Central         40.75277
    ## 56     42nd St Shuttle                           Grand Central         40.75277
    ## 57     42nd St Shuttle                           Grand Central         40.75277
    ## 58     42nd St Shuttle                           Grand Central         40.75277
    ## 59     42nd St Shuttle                           Grand Central         40.75277
    ## 60     42nd St Shuttle                            Times Square         40.75598
    ## 61            6 Avenue                                 14th St         40.73823
    ## 62            6 Avenue                                 14th St         40.73823
    ## 63            6 Avenue                                 14th St         40.73823
    ## 64            6 Avenue                                 14th St         40.73823
    ## 65            6 Avenue                                 14th St         40.73823
    ## 66            6 Avenue                                 14th St         40.73823
    ## 67            6 Avenue                                 14th St         40.73823
    ## 68            6 Avenue                                 14th St         40.73823
    ## 69            6 Avenue                                 14th St         40.73823
    ## 70            6 Avenue                                 14th St         40.73823
    ## 71            6 Avenue                                 23rd St         40.74288
    ## 72            6 Avenue                                 23rd St         40.74288
    ## 73            6 Avenue                                 23rd St         40.74288
    ## 74            6 Avenue                                 23rd St         40.74288
    ## 75            6 Avenue                                 23rd St         40.74288
    ## 76            6 Avenue                                 23rd St         40.74288
    ## 77            6 Avenue                                 23rd St         40.74288
    ## 78            6 Avenue                                 23rd St         40.74288
    ## 79            6 Avenue                                  2nd Av         40.72340
    ## 80            6 Avenue                                  2nd Av         40.72340
    ## 81            6 Avenue                                  2nd Av         40.72340
    ## 82            6 Avenue                                  2nd Av         40.72340
    ## 83            6 Avenue                                 34th St         40.74972
    ## 84            6 Avenue                                 34th St         40.74972
    ## 85            6 Avenue                                 34th St         40.74972
    ## 86            6 Avenue                                 34th St         40.74972
    ## 87            6 Avenue                                 34th St         40.74972
    ## 88            6 Avenue                                 34th St         40.74972
    ## 89            6 Avenue                                 34th St         40.74972
    ## 90            6 Avenue                                 34th St         40.74972
    ## 91            6 Avenue                                 34th St         40.74972
    ## 92            6 Avenue                                 34th St         40.74972
    ## 93            6 Avenue                                 42nd St         40.75422
    ## 94            6 Avenue                                 42nd St         40.75422
    ## 95            6 Avenue                                 42nd St         40.75422
    ## 96            6 Avenue                                 42nd St         40.75422
    ## 97            6 Avenue                                 42nd St         40.75422
    ## 98            6 Avenue                                 42nd St         40.75422
    ## 99            6 Avenue                                 42nd St         40.75422
    ## 100           6 Avenue                                 42nd St         40.75422
    ## 101           6 Avenue                                 42nd St         40.75422
    ## 102           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 103           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 104           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 105           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 106           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 107           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 108           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 109           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 110           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 111           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 112           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 113           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 114           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 115           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 116           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 117           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 118           6 Avenue          47-50th Sts Rockefeller Center         40.75866
    ## 119           6 Avenue                                  4th Av         40.67027
    ## 120           6 Avenue                                  4th Av         40.67027
    ## 121           6 Avenue                                 57th St         40.76397
    ## 122           6 Avenue                                 57th St         40.76397
    ## 123           6 Avenue                                 57th St         40.76397
    ## 124           6 Avenue                                 57th St         40.76397
    ## 125           6 Avenue                                 57th St         40.76397
    ## 126           6 Avenue                                 57th St         40.76397
    ## 127           6 Avenue                                 57th St         40.76397
    ## 128           6 Avenue                                 57th St         40.76397
    ## 129           6 Avenue                                  7th Av         40.66627
    ## 130           6 Avenue                                  7th Av         40.66627
    ## 131           6 Avenue                                  7th Av         40.66627
    ## 132           6 Avenue                                  7th Av         40.66627
    ## 133           6 Avenue                                  7th Av         40.66627
    ## 134           6 Avenue                                  7th Av         40.66627
    ## 135           6 Avenue                                  7th Av         40.66627
    ## 136           6 Avenue                                  7th Av         40.66627
    ## 137           6 Avenue                               Bergen St         40.68615
    ## 138           6 Avenue                               Bergen St         40.68615
    ## 139           6 Avenue                               Bergen St         40.68615
    ## 140           6 Avenue                               Bergen St         40.68615
    ## 141           6 Avenue                               Bergen St         40.68615
    ## 142           6 Avenue                               Bergen St         40.68615
    ## 143           6 Avenue                   Broadway-Lafayette St         40.72530
    ## 144           6 Avenue                   Broadway-Lafayette St         40.72530
    ## 145           6 Avenue                   Broadway-Lafayette St         40.72530
    ## 146           6 Avenue                   Broadway-Lafayette St         40.72530
    ## 147           6 Avenue                   Broadway-Lafayette St         40.72530
    ## 148           6 Avenue                              Carroll St         40.68030
    ## 149           6 Avenue                              Carroll St         40.68030
    ## 150           6 Avenue                              Carroll St         40.68030
    ## 151           6 Avenue                              Carroll St         40.68030
    ## 152           6 Avenue                              Carroll St         40.68030
    ## 153           6 Avenue                               Church Av         40.64404
    ## 154           6 Avenue                               Church Av         40.64404
    ## 155           6 Avenue                               Church Av         40.64404
    ## 156           6 Avenue                               Church Av         40.64404
    ## 157           6 Avenue                               Church Av         40.64404
    ## 158           6 Avenue                               Church Av         40.64404
    ## 159           6 Avenue                             Delancey St         40.71861
    ## 160           6 Avenue                             Delancey St         40.71861
    ## 161           6 Avenue                             Delancey St         40.71861
    ## 162           6 Avenue                             Delancey St         40.71861
    ## 163           6 Avenue                           East Broadway         40.71372
    ## 164           6 Avenue                           East Broadway         40.71372
    ## 165           6 Avenue                           East Broadway         40.71372
    ## 166           6 Avenue                           East Broadway         40.71372
    ## 167           6 Avenue                   Fort Hamilton Parkway         40.65078
    ## 168           6 Avenue                   Fort Hamilton Parkway         40.65078
    ## 169           6 Avenue                   Fort Hamilton Parkway         40.65078
    ## 170           6 Avenue                   Fort Hamilton Parkway         40.65078
    ## 171           6 Avenue                                Grand St         40.71827
    ## 172           6 Avenue                                Grand St         40.71827
    ## 173           6 Avenue                                Grand St         40.71827
    ## 174           6 Avenue                     Prospect Park-15 St         40.66036
    ## 175           6 Avenue                     Prospect Park-15 St         40.66036
    ## 176           6 Avenue                     Prospect Park-15 St         40.66036
    ## 177           6 Avenue                     Prospect Park-15 St         40.66036
    ## 178           6 Avenue                     Prospect Park-15 St         40.66036
    ## 179           6 Avenue                     Prospect Park-15 St         40.66036
    ## 180           6 Avenue                            Smith-9th St         40.67358
    ## 181           6 Avenue                                 York St         40.69974
    ## 182        63rd Street                                 21st St         40.75420
    ## 183        63rd Street                                 21st St         40.75420
    ## 184        63rd Street                                 21st St         40.75420
    ## 185        63rd Street                                 21st St         40.75420
    ## 186        63rd Street                                 21st St         40.75420
    ## 187        63rd Street                            Lexington Av         40.76463
    ## 188        63rd Street                            Lexington Av         40.76463
    ## 189        63rd Street                            Lexington Av         40.76463
    ## 190        63rd Street                            Lexington Av         40.76463
    ## 191        63rd Street                            Lexington Av         40.76463
    ## 192        63rd Street                        Roosevelt Island         40.75914
    ## 193           8 Avenue                                103rd St         40.79609
    ## 194           8 Avenue                                116th St         40.80508
    ## 195           8 Avenue                                116th St         40.80508
    ## 196           8 Avenue                                116th St         40.80508
    ## 197           8 Avenue                                116th St         40.80508
    ## 198           8 Avenue                                125th St         40.81111
    ## 199           8 Avenue                                125th St         40.81111
    ## 200           8 Avenue                                125th St         40.81111
    ## 201           8 Avenue                                125th St         40.81111
    ## 202           8 Avenue                                125th St         40.81111
    ## 203           8 Avenue                                125th St         40.81111
    ## 204           8 Avenue                                135th St         40.81789
    ## 205           8 Avenue                                135th St         40.81789
    ## 206           8 Avenue                                135th St         40.81789
    ## 207           8 Avenue                                135th St         40.81789
    ## 208           8 Avenue                                135th St         40.81789
    ## 209           8 Avenue                                135th St         40.81789
    ## 210           8 Avenue                                145th St         40.82478
    ## 211           8 Avenue                                145th St         40.82478
    ## 212           8 Avenue                                145th St         40.82478
    ## 213           8 Avenue                                145th St         40.82478
    ## 214           8 Avenue                                145th St         40.82478
    ## 215           8 Avenue                                145th St         40.82478
    ## 216           8 Avenue                                 14th St         40.74089
    ## 217           8 Avenue                                 14th St         40.74089
    ## 218           8 Avenue                                 14th St         40.74089
    ## 219           8 Avenue                                 14th St         40.74089
    ## 220           8 Avenue                                 14th St         40.74089
    ## 221           8 Avenue                                 14th St         40.74089
    ## 222           8 Avenue                                 14th St         40.74089
    ## 223           8 Avenue                                 14th St         40.74089
    ## 224           8 Avenue                                155th St         40.83052
    ## 225           8 Avenue                                155th St         40.83052
    ## 226           8 Avenue                                155th St         40.83052
    ## 227           8 Avenue                                155th St         40.83052
    ## 228           8 Avenue                                155th St         40.83052
    ## 229           8 Avenue                                155th St         40.83052
    ## 230           8 Avenue                 163rd St - Amsterdam Av         40.83601
    ## 231           8 Avenue                 163rd St - Amsterdam Av         40.83601
    ## 232           8 Avenue                 163rd St - Amsterdam Av         40.83601
    ## 233           8 Avenue           168th St - Washington Heights         40.84072
    ## 234           8 Avenue           168th St - Washington Heights         40.84072
    ## 235           8 Avenue           168th St - Washington Heights         40.84072
    ## 236           8 Avenue           168th St - Washington Heights         40.84072
    ## 237           8 Avenue                                175th St         40.84739
    ## 238           8 Avenue                                175th St         40.84739
    ## 239           8 Avenue                                175th St         40.84739
    ## 240           8 Avenue                                175th St         40.84739
    ## 241           8 Avenue                                175th St         40.84739
    ## 242           8 Avenue                                175th St         40.84739
    ## 243           8 Avenue                                175th St         40.84739
    ## 244           8 Avenue                                181st St         40.85169
    ## 245           8 Avenue                                181st St         40.85169
    ## 246           8 Avenue                                181st St         40.85169
    ## 247           8 Avenue                                181st St         40.85169
    ## 248           8 Avenue                                181st St         40.85169
    ## 249           8 Avenue                                190th St         40.85902
    ## 250           8 Avenue                                190th St         40.85902
    ## 251           8 Avenue                                 23rd St         40.74591
    ## 252           8 Avenue                                 23rd St         40.74591
    ## 253           8 Avenue                                 23rd St         40.74591
    ## 254           8 Avenue                                 23rd St         40.74591
    ## 255           8 Avenue                                 23rd St         40.74591
    ## 256           8 Avenue                                 23rd St         40.74591
    ## 257           8 Avenue                                 23rd St         40.74591
    ## 258           8 Avenue                                 23rd St         40.74591
    ## 259           8 Avenue                                 23rd St         40.74591
    ## 260           8 Avenue                                 23rd St         40.74591
    ## 261           8 Avenue                                 23rd St         40.74591
    ## 262           8 Avenue                                 34th St         40.75229
    ## 263           8 Avenue                                 34th St         40.75229
    ## 264           8 Avenue                                 34th St         40.75229
    ## 265           8 Avenue                                 34th St         40.75229
    ## 266           8 Avenue                                 34th St         40.75229
    ## 267           8 Avenue                                 34th St         40.75229
    ## 268           8 Avenue                                 34th St         40.75229
    ## 269           8 Avenue                                 34th St         40.75229
    ## 270           8 Avenue                                 34th St         40.75229
    ## 271           8 Avenue                                 34th St         40.75229
    ## 272           8 Avenue                                 34th St         40.75229
    ## 273           8 Avenue                                 34th St         40.75229
    ## 274           8 Avenue                                 34th St         40.75229
    ## 275           8 Avenue                                 34th St         40.75229
    ## 276           8 Avenue                                 34th St         40.75229
    ## 277           8 Avenue                                 34th St         40.75229
    ## 278           8 Avenue                                 42nd St         40.75731
    ## 279           8 Avenue                                 42nd St         40.75731
    ## 280           8 Avenue                                 42nd St         40.75731
    ## 281           8 Avenue                                 42nd St         40.75731
    ## 282           8 Avenue                                 42nd St         40.75731
    ## 283           8 Avenue                                 42nd St         40.75731
    ## 284           8 Avenue                                 42nd St         40.75731
    ## 285           8 Avenue                                 42nd St         40.75731
    ## 286           8 Avenue                                 42nd St         40.75731
    ## 287           8 Avenue                                 50th St         40.76246
    ## 288           8 Avenue                                 50th St         40.76246
    ## 289           8 Avenue                                 50th St         40.76246
    ## 290           8 Avenue                                 50th St         40.76246
    ## 291           8 Avenue                                 50th St         40.76246
    ## 292           8 Avenue                                 50th St         40.76246
    ## 293           8 Avenue                                 50th St         40.76246
    ## 294           8 Avenue                                 50th St         40.76246
    ## 295           8 Avenue                                 50th St         40.76246
    ## 296           8 Avenue                                 50th St         40.76246
    ## 297           8 Avenue                                 59th St         40.76830
    ## 298           8 Avenue                                 59th St         40.76830
    ## 299           8 Avenue                                 59th St         40.76830
    ## 300           8 Avenue                                 59th St         40.76830
    ## 301           8 Avenue                                 59th St         40.76830
    ## 302           8 Avenue                                 59th St         40.76830
    ## 303           8 Avenue                                 59th St         40.76830
    ## 304           8 Avenue                                 59th St         40.76830
    ## 305           8 Avenue                                 59th St         40.76830
    ## 306           8 Avenue                                 59th St         40.76830
    ## 307           8 Avenue                                 59th St         40.76830
    ## 308           8 Avenue                                 59th St         40.76830
    ## 309           8 Avenue                                 72nd St         40.77559
    ## 310           8 Avenue                                 72nd St         40.77559
    ## 311           8 Avenue                                 72nd St         40.77559
    ## 312           8 Avenue     81st St - Museum of Natural History         40.78143
    ## 313           8 Avenue     81st St - Museum of Natural History         40.78143
    ## 314           8 Avenue     81st St - Museum of Natural History         40.78143
    ## 315           8 Avenue     81st St - Museum of Natural History         40.78143
    ## 316           8 Avenue                                 86th St         40.78587
    ## 317           8 Avenue                                 86th St         40.78587
    ## 318           8 Avenue                                 86th St         40.78587
    ## 319           8 Avenue                                 86th St         40.78587
    ## 320           8 Avenue                                 86th St         40.78587
    ## 321           8 Avenue                                 96th St         40.79165
    ## 322           8 Avenue                                 96th St         40.79165
    ## 323           8 Avenue                                 96th St         40.79165
    ## 324           8 Avenue                         Broadway-Nassau         40.71020
    ## 325           8 Avenue                         Broadway-Nassau         40.71020
    ## 326           8 Avenue                                Canal St         40.72082
    ## 327           8 Avenue                                Canal St         40.72082
    ## 328           8 Avenue                                Canal St         40.72082
    ## 329           8 Avenue                                Canal St         40.72082
    ## 330           8 Avenue                                Canal St         40.72082
    ## 331           8 Avenue              Cathedral Parkway-110th St         40.80060
    ## 332           8 Avenue              Cathedral Parkway-110th St         40.80060
    ## 333           8 Avenue              Cathedral Parkway-110th St         40.80060
    ## 334           8 Avenue                             Chambers St         40.71411
    ## 335           8 Avenue                             Chambers St         40.71411
    ## 336           8 Avenue                             Chambers St         40.71411
    ## 337           8 Avenue                             Chambers St         40.71411
    ## 338           8 Avenue                             Chambers St         40.71411
    ## 339           8 Avenue                             Chambers St         40.71411
    ## 340           8 Avenue                             Chambers St         40.71411
    ## 341           8 Avenue                             Chambers St         40.71411
    ## 342           8 Avenue                             Chambers St         40.71411
    ## 343           8 Avenue                     Dyckman St-200th St         40.86549
    ## 344           8 Avenue                     Dyckman St-200th St         40.86549
    ## 345           8 Avenue                     Dyckman St-200th St         40.86549
    ## 346           8 Avenue                     Dyckman St-200th St         40.86549
    ## 347           8 Avenue                     Dyckman St-200th St         40.86549
    ## 348           8 Avenue                     Dyckman St-200th St         40.86549
    ## 349           8 Avenue                     Dyckman St-200th St         40.86549
    ## 350           8 Avenue                                 High St         40.69934
    ## 351           8 Avenue                                 High St         40.69934
    ## 352           8 Avenue                                 High St         40.69934
    ## 353           8 Avenue                       Inwood - 207th St         40.86807
    ## 354           8 Avenue                       Inwood - 207th St         40.86807
    ## 355           8 Avenue                       Inwood - 207th St         40.86807
    ## 356           8 Avenue                       Inwood - 207th St         40.86807
    ## 357           8 Avenue                       Inwood - 207th St         40.86807
    ## 358           8 Avenue                       Inwood - 207th St         40.86807
    ## 359           8 Avenue                               Spring St         40.72623
    ## 360           8 Avenue                               Spring St         40.72623
    ## 361           8 Avenue                               Spring St         40.72623
    ## 362           8 Avenue                             West 4th St         40.73234
    ## 363           8 Avenue                             West 4th St         40.73234
    ## 364           8 Avenue                             West 4th St         40.73234
    ## 365           8 Avenue                             West 4th St         40.73234
    ## 366           8 Avenue                             West 4th St         40.73234
    ## 367           8 Avenue                             West 4th St         40.73234
    ## 368           8 Avenue                      World Trade Center         40.71258
    ## 369           8 Avenue                      World Trade Center         40.71258
    ## 370           8 Avenue                      World Trade Center         40.71258
    ## 371           8 Avenue                      World Trade Center         40.71258
    ## 372           8 Avenue                      World Trade Center         40.71258
    ## 373           8 Avenue                      World Trade Center         40.71258
    ## 374           8 Avenue                      World Trade Center         40.71258
    ## 375           8 Avenue                      World Trade Center         40.71258
    ## 376          Archer Av                        Jamaica-Van Wyck         40.70257
    ## 377          Archer Av                        Jamaica-Van Wyck         40.70257
    ## 378          Archer Av                        Jamaica-Van Wyck         40.70257
    ## 379          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 380          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 381          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 382          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 383          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 384          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 385          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 386          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 387          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 388          Archer Av Parsons Blvd-Archer Av - Jamaica Center         40.70215
    ## 389          Archer Av            Sutphin Blvd-Archer Av - JFK         40.70049
    ## 390          Archer Av            Sutphin Blvd-Archer Av - JFK         40.70049
    ## 391          Archer Av            Sutphin Blvd-Archer Av - JFK         40.70049
    ## 392          Archer Av            Sutphin Blvd-Archer Av - JFK         40.70049
    ## 393            Astoria                          30 Av-Grand Av         40.76678
    ## 394            Astoria                          30 Av-Grand Av         40.76678
    ## 395            Astoria                          30 Av-Grand Av         40.76678
    ## 396            Astoria                          30 Av-Grand Av         40.76678
    ## 397            Astoria                     36 Av-Washington Av         40.75680
    ## 398            Astoria                     36 Av-Washington Av         40.75680
    ## 399            Astoria                     36 Av-Washington Av         40.75680
    ## 400            Astoria                          39 Av-Beebe Av         40.75288
    ## 401            Astoria                          39 Av-Beebe Av         40.75288
    ## 402            Astoria                    Astoria Blvd-Hoyt Av         40.77026
    ## 403            Astoria                    Astoria Blvd-Hoyt Av         40.77026
    ## 404            Astoria                    Astoria Blvd-Hoyt Av         40.77026
    ## 405            Astoria                    Astoria Blvd-Hoyt Av         40.77026
    ## 406            Astoria                                Broadway         40.76182
    ## 407            Astoria                                Broadway         40.76182
    ## 408            Astoria                                Broadway         40.76182
    ## 409            Astoria                            Ditmars Blvd         40.77504
    ## 410            Astoria                            Ditmars Blvd         40.77504
    ## 411            Astoria                            Ditmars Blvd         40.77504
    ## 412            Astoria                            Ditmars Blvd         40.77504
    ## 413           Brighton                                  7th Av         40.67705
    ## 414           Brighton                                  7th Av         40.67705
    ## 415           Brighton                             Atlantic Av         40.68446
    ## 416           Brighton                                    Av H         40.62927
    ## 417           Brighton                                    Av H         40.62927
    ## 418           Brighton                                    Av J         40.62504
    ## 419           Brighton                                    Av J         40.62504
    ## 420           Brighton                                    Av J         40.62504
    ## 421           Brighton                                    Av M         40.61762
    ## 422           Brighton                                    Av M         40.61762
    ## 423           Brighton                                    Av U         40.59930
    ## 424           Brighton                              Beverly Rd         40.64403
    ## 425           Brighton                          Brighton Beach         40.57762
    ## 426           Brighton                          Brighton Beach         40.57762
    ## 427           Brighton                          Brighton Beach         40.57762
    ## 428           Brighton                          Brighton Beach         40.57762
    ## 429           Brighton                          Brighton Beach         40.57762
    ## 430           Brighton                          Brighton Beach         40.57762
    ## 431           Brighton                          Brighton Beach         40.57762
    ## 432           Brighton                          Brighton Beach         40.57762
    ## 433           Brighton                               Church Av         40.65053
    ## 434           Brighton                               Church Av         40.65053
    ## 435           Brighton                            Cortelyou Rd         40.64093
    ## 436           Brighton                           Kings Highway         40.60867
    ## 437           Brighton                           Kings Highway         40.60867
    ## 438           Brighton                           Kings Highway         40.60867
    ## 439           Brighton                                 Neck Rd         40.59525
    ## 440           Brighton                              Newkirk Av         40.63508
    ## 441           Brighton                           Ocean Parkway         40.57631
    ## 442           Brighton                           Ocean Parkway         40.57631
    ## 443           Brighton                           Ocean Parkway         40.57631
    ## 444           Brighton                           Ocean Parkway         40.57631
    ## 445           Brighton                           Ocean Parkway         40.57631
    ## 446           Brighton                           Ocean Parkway         40.57631
    ## 447           Brighton                             Parkside Av         40.65529
    ## 448           Brighton                             Parkside Av         40.65529
    ## 449           Brighton                           Prospect Park         40.66161
    ## 450           Brighton                           Prospect Park         40.66161
    ## 451           Brighton                           Prospect Park         40.66161
    ## 452           Brighton                          Sheepshead Bay         40.58690
    ## 453           Brighton                          Sheepshead Bay         40.58690
    ## 454           Brighton                            Stillwell Av         40.57742
    ## 455           Brighton                             West 8th St         40.57613
    ## 456           Brighton                             West 8th St         40.57613
    ## 457           Broadway                                 23rd St         40.74130
    ## 458           Broadway                                 23rd St         40.74130
    ## 459           Broadway                                 23rd St         40.74130
    ## 460           Broadway                                 23rd St         40.74130
    ## 461           Broadway                                 23rd St         40.74130
    ## 462           Broadway                                 23rd St         40.74130
    ## 463           Broadway                                 23rd St         40.74130
    ## 464           Broadway                                 23rd St         40.74130
    ## 465           Broadway                                 28th St         40.74549
    ## 466           Broadway                                 28th St         40.74549
    ## 467           Broadway                                 28th St         40.74549
    ## 468           Broadway                                 28th St         40.74549
    ## 469           Broadway                                 34th St         40.74957
    ## 470           Broadway                                 34th St         40.74957
    ## 471           Broadway                                 34th St         40.74957
    ## 472           Broadway                                 34th St         40.74957
    ## 473           Broadway                                 49th St         40.75990
    ## 474           Broadway                                 49th St         40.75990
    ## 475           Broadway                                 49th St         40.75990
    ## 476           Broadway                                 49th St         40.75990
    ## 477           Broadway                                 49th St         40.75990
    ## 478           Broadway                                 49th St         40.75990
    ## 479           Broadway                                 49th St         40.75990
    ## 480           Broadway                                 57th St         40.76466
    ## 481           Broadway                                 57th St         40.76466
    ## 482           Broadway                                 57th St         40.76466
    ## 483           Broadway                                 57th St         40.76466
    ## 484           Broadway                                 57th St         40.76466
    ## 485           Broadway                                 57th St         40.76466
    ## 486           Broadway                                 57th St         40.76466
    ## 487           Broadway                                 57th St         40.76466
    ## 488           Broadway                                  5th Av         40.76481
    ## 489           Broadway                                  5th Av         40.76481
    ## 490           Broadway                                  5th Av         40.76481
    ## 491           Broadway                                  5th Av         40.76481
    ## 492           Broadway                                  5th Av         40.76481
    ## 493           Broadway                                  5th Av         40.76481
    ## 494           Broadway                                  5th Av         40.76481
    ## 495           Broadway                                  8th St         40.73033
    ## 496           Broadway                                  8th St         40.73033
    ## 497           Broadway                                  8th St         40.73033
    ## 498           Broadway                                  8th St         40.73033
    ## 499           Broadway                                  8th St         40.73033
    ## 500           Broadway                                  8th St         40.73033
    ## 501           Broadway                                  8th St         40.73033
    ## 502           Broadway                                  8th St         40.73033
    ## 503           Broadway                           Canal St (UL)         40.71953
    ## 504           Broadway                           Canal St (UL)         40.71953
    ## 505           Broadway                           Canal St (UL)         40.71953
    ## 506           Broadway                           Canal St (UL)         40.71953
    ## 507           Broadway                           Canal St (UL)         40.71953
    ## 508           Broadway                           Canal St (UL)         40.71953
    ## 509           Broadway                           Canal St (UL)         40.71953
    ## 510           Broadway                               City Hall         40.71328
    ## 511           Broadway                               City Hall         40.71328
    ## 512           Broadway                               City Hall         40.71328
    ## 513           Broadway                            Cortlandt St         40.71067
    ## 514           Broadway                            Cortlandt St         40.71067
    ## 515           Broadway                            Cortlandt St         40.71067
    ## 516           Broadway                            Cortlandt St         40.71067
    ## 517           Broadway                                Court St         40.69410
    ## 518           Broadway                                Court St         40.69410
    ## 519           Broadway                                Court St         40.69410
    ## 520           Broadway                             Lawrence St         40.69218
    ## 521           Broadway                             Lawrence St         40.69218
    ## 522           Broadway                             Lawrence St         40.69218
    ## 523           Broadway                             Lawrence St         40.69218
    ## 524           Broadway                             Lawrence St         40.69218
    ## 525           Broadway                             Lawrence St         40.69218
    ## 526           Broadway                             Lawrence St         40.69218
    ## 527           Broadway                            Lexington Av         40.76266
    ## 528           Broadway                            Lexington Av         40.76266
    ## 529           Broadway                            Lexington Av         40.76266
    ## 530           Broadway                            Lexington Av         40.76266
    ## 531           Broadway                               Prince St         40.72433
    ## 532           Broadway                               Prince St         40.72433
    ## 533           Broadway                               Prince St         40.72433
    ## 534           Broadway                               Prince St         40.72433
    ## 535           Broadway                               Rector St         40.70722
    ## 536           Broadway                               Rector St         40.70722
    ## 537           Broadway                               Rector St         40.70722
    ## 538           Broadway                               Rector St         40.70722
    ## 539           Broadway                               Rector St         40.70722
    ## 540           Broadway                               Rector St         40.70722
    ## 541           Broadway                               Rector St         40.70722
    ## 542           Broadway                               Rector St         40.70722
    ## 543           Broadway                               Rector St         40.70722
    ## 544           Broadway                    Times Square-42nd St         40.75467
    ## 545           Broadway                    Times Square-42nd St         40.75467
    ## 546           Broadway                    Times Square-42nd St         40.75467
    ## 547           Broadway                    Times Square-42nd St         40.75467
    ## 548           Broadway                            Union Square         40.73574
    ## 549           Broadway                            Union Square         40.73574
    ## 550           Broadway                            Union Square         40.73574
    ## 551           Broadway                            Union Square         40.73574
    ## 552           Broadway                            Whitehall St         40.70309
    ## 553           Broadway                            Whitehall St         40.70309
    ## 554           Broadway                            Whitehall St         40.70309
    ## 555           Broadway                            Whitehall St         40.70309
    ## 556           Broadway                            Whitehall St         40.70309
    ## 557           Broadway                            Whitehall St         40.70309
    ## 558   Broadway Jamaica                       104th St-102nd St         40.69518
    ## 559   Broadway Jamaica                       104th St-102nd St         40.69518
    ## 560   Broadway Jamaica                                111th St         40.69742
    ## 561   Broadway Jamaica                                111th St         40.69742
    ## 562   Broadway Jamaica                                121st St         40.70049
    ## 563   Broadway Jamaica                                121st St         40.70049
    ## 564   Broadway Jamaica                                121st St         40.70049
    ## 565   Broadway Jamaica                                121st St         40.70049
    ## 566   Broadway Jamaica                              Alabama Av         40.67699
    ## 567   Broadway Jamaica                              Alabama Av         40.67699
    ## 568   Broadway Jamaica                             Chauncey St         40.68289
    ## 569   Broadway Jamaica                             Chauncey St         40.68289
    ## 570   Broadway Jamaica                            Cleveland St         40.67995
    ## 571   Broadway Jamaica                            Cleveland St         40.67995
    ## 572   Broadway Jamaica                             Crescent St         40.68319
    ## 573   Broadway Jamaica                             Crescent St         40.68319
    ## 574   Broadway Jamaica                           Cypress Hills         40.68994
    ## 575   Broadway Jamaica                           Cypress Hills         40.68994
    ## 576   Broadway Jamaica                           Cypress Hills         40.68994
    ## 577   Broadway Jamaica                    Elderts Lane-75th St         40.69132
    ## 578   Broadway Jamaica                    Elderts Lane-75th St         40.69132
    ## 579   Broadway Jamaica                             Flushing Av         40.70026
    ## 580   Broadway Jamaica                             Flushing Av         40.70026
    ## 581   Broadway Jamaica                             Flushing Av         40.70026
    ## 582   Broadway Jamaica                             Flushing Av         40.70026
    ## 583   Broadway Jamaica                  Forest Parkway-85th St         40.69244
    ## 584   Broadway Jamaica                  Forest Parkway-85th St         40.69244
    ## 585   Broadway Jamaica                                Gates Av         40.68963
    ## 586   Broadway Jamaica                                Gates Av         40.68963
    ## 587   Broadway Jamaica                               Halsey St         40.68637
    ## 588   Broadway Jamaica                               Halsey St         40.68637
    ## 589   Broadway Jamaica                                Hewes St         40.70687
    ## 590   Broadway Jamaica                                Hewes St         40.70687
    ## 591   Broadway Jamaica                            Kosciusko St         40.69334
    ## 592   Broadway Jamaica                            Kosciusko St         40.69334
    ## 593   Broadway Jamaica                              Lorimer St         40.70387
    ## 594   Broadway Jamaica                              Lorimer St         40.70387
    ## 595   Broadway Jamaica                              Lorimer St         40.70387
    ## 596   Broadway Jamaica                              Lorimer St         40.70387
    ## 597   Broadway Jamaica                                Marcy Av         40.70836
    ## 598   Broadway Jamaica                                Marcy Av         40.70836
    ## 599   Broadway Jamaica                                Marcy Av         40.70836
    ## 600   Broadway Jamaica                                Marcy Av         40.70836
    ## 601   Broadway Jamaica                                Marcy Av         40.70836
    ## 602   Broadway Jamaica                                Marcy Av         40.70836
    ## 603   Broadway Jamaica                                Marcy Av         40.70836
    ## 604   Broadway Jamaica                                Marcy Av         40.70836
    ## 605   Broadway Jamaica                               Myrtle Av         40.69721
    ## 606   Broadway Jamaica                               Myrtle Av         40.69721
    ## 607   Broadway Jamaica                              Norwood Av         40.68141
    ## 608   Broadway Jamaica                              Norwood Av         40.68141
    ## 609   Broadway Jamaica                           Van Siclen Av         40.67802
    ## 610   Broadway Jamaica                           Van Siclen Av         40.67802
    ## 611   Broadway Jamaica                          Woodhaven Blvd         40.69388
    ## 612   Broadway Jamaica                          Woodhaven Blvd         40.69388
    ## 613   Broadway Jamaica                          Woodhaven Blvd         40.69388
    ## 614   Broadway-7th Ave                                103rd St         40.79945
    ## 615   Broadway-7th Ave                                103rd St         40.79945
    ## 616   Broadway-7th Ave                                103rd St         40.79945
    ## 617   Broadway-7th Ave                                103rd St         40.79945
    ## 618   Broadway-7th Ave                                103rd St         40.79945
    ## 619   Broadway-7th Ave            116th St-Columbia University         40.80772
    ## 620   Broadway-7th Ave            116th St-Columbia University         40.80772
    ## 621   Broadway-7th Ave            116th St-Columbia University         40.80772
    ## 622   Broadway-7th Ave            116th St-Columbia University         40.80772
    ## 623   Broadway-7th Ave            116th St-Columbia University         40.80772
    ## 624   Broadway-7th Ave                                125th St         40.81558
    ## 625   Broadway-7th Ave                                125th St         40.81558
    ## 626   Broadway-7th Ave                                125th St         40.81558
    ## 627   Broadway-7th Ave                                125th St         40.81558
    ## 628   Broadway-7th Ave                   137th St-City College         40.82201
    ## 629   Broadway-7th Ave                   137th St-City College         40.82201
    ## 630   Broadway-7th Ave                   137th St-City College         40.82201
    ## 631   Broadway-7th Ave                   137th St-City College         40.82201
    ## 632   Broadway-7th Ave                                145th St         40.82655
    ## 633   Broadway-7th Ave                                145th St         40.82655
    ## 634   Broadway-7th Ave                                145th St         40.82655
    ## 635   Broadway-7th Ave                                 14th St         40.73783
    ## 636   Broadway-7th Ave                                 14th St         40.73783
    ## 637   Broadway-7th Ave                                 14th St         40.73783
    ## 638   Broadway-7th Ave                                 14th St         40.73783
    ## 639   Broadway-7th Ave                                 14th St         40.73783
    ## 640   Broadway-7th Ave                                 14th St         40.73783
    ## 641   Broadway-7th Ave                                 14th St         40.73783
    ## 642   Broadway-7th Ave                                 14th St         40.73783
    ## 643   Broadway-7th Ave                                157th St         40.83404
    ## 644   Broadway-7th Ave                                157th St         40.83404
    ## 645   Broadway-7th Ave                                157th St         40.83404
    ## 646   Broadway-7th Ave                                157th St         40.83404
    ## 647   Broadway-7th Ave                                168th St         40.84056
    ## 648   Broadway-7th Ave                                168th St         40.84056
    ## 649   Broadway-7th Ave                                168th St         40.84056
    ## 650   Broadway-7th Ave                                181st St         40.84951
    ## 651   Broadway-7th Ave                                181st St         40.84951
    ## 652   Broadway-7th Ave                                 18th St         40.74104
    ## 653   Broadway-7th Ave                                 18th St         40.74104
    ## 654   Broadway-7th Ave                                 18th St         40.74104
    ## 655   Broadway-7th Ave                                 18th St         40.74104
    ## 656   Broadway-7th Ave                                 18th St         40.74104
    ## 657   Broadway-7th Ave                                 18th St         40.74104
    ## 658   Broadway-7th Ave                                191st St         40.85522
    ## 659   Broadway-7th Ave                                191st St         40.85522
    ## 660   Broadway-7th Ave                                207th St         40.86461
    ## 661   Broadway-7th Ave                                207th St         40.86461
    ## 662   Broadway-7th Ave                                207th St         40.86461
    ## 663   Broadway-7th Ave                                207th St         40.86461
    ## 664   Broadway-7th Ave                                215th St         40.86944
    ## 665   Broadway-7th Ave                                215th St         40.86944
    ## 666   Broadway-7th Ave                                215th St         40.86944
    ## 667   Broadway-7th Ave                                215th St         40.86944
    ## 668   Broadway-7th Ave                                231st St         40.87886
    ## 669   Broadway-7th Ave                                231st St         40.87886
    ## 670   Broadway-7th Ave                                231st St         40.87886
    ## 671   Broadway-7th Ave                                231st St         40.87886
    ## 672   Broadway-7th Ave                                238th St         40.88467
    ## 673   Broadway-7th Ave                                238th St         40.88467
    ## 674   Broadway-7th Ave                                238th St         40.88467
    ## 675   Broadway-7th Ave                                 23rd St         40.74408
    ## 676   Broadway-7th Ave                                 23rd St         40.74408
    ## 677   Broadway-7th Ave                                 23rd St         40.74408
    ## 678   Broadway-7th Ave                                 23rd St         40.74408
    ## 679   Broadway-7th Ave                                 28th St         40.74721
    ## 680   Broadway-7th Ave                                 28th St         40.74721
    ## 681   Broadway-7th Ave                                 28th St         40.74721
    ## 682   Broadway-7th Ave                                 28th St         40.74721
    ## 683   Broadway-7th Ave                                 28th St         40.74721
    ## 684   Broadway-7th Ave                                 28th St         40.74721
    ## 685   Broadway-7th Ave                                 34th St         40.75037
    ## 686   Broadway-7th Ave                                 34th St         40.75037
    ## 687   Broadway-7th Ave                                 34th St         40.75037
    ## 688   Broadway-7th Ave                                 34th St         40.75037
    ## 689   Broadway-7th Ave                                 34th St         40.75037
    ## 690   Broadway-7th Ave                                 34th St         40.75037
    ## 691   Broadway-7th Ave                                 34th St         40.75037
    ## 692   Broadway-7th Ave                                 34th St         40.75037
    ## 693   Broadway-7th Ave                                 34th St         40.75037
    ## 694   Broadway-7th Ave                                 34th St         40.75037
    ## 695   Broadway-7th Ave                                 34th St         40.75037
    ## 696   Broadway-7th Ave                                 50th St         40.76173
    ## 697   Broadway-7th Ave                                 50th St         40.76173
    ## 698   Broadway-7th Ave                                 50th St         40.76173
    ## 699   Broadway-7th Ave                                 50th St         40.76173
    ## 700   Broadway-7th Ave                                 50th St         40.76173
    ## 701   Broadway-7th Ave                                 50th St         40.76173
    ## 702   Broadway-7th Ave                 59th St-Columbus Circle         40.76825
    ## 703   Broadway-7th Ave                 59th St-Columbus Circle         40.76825
    ## 704   Broadway-7th Ave                 59th St-Columbus Circle         40.76825
    ## 705   Broadway-7th Ave                 59th St-Columbus Circle         40.76825
    ## 706   Broadway-7th Ave                 59th St-Columbus Circle         40.76825
    ## 707   Broadway-7th Ave                  66th St-Lincoln Center         40.77344
    ## 708   Broadway-7th Ave                  66th St-Lincoln Center         40.77344
    ## 709   Broadway-7th Ave                  66th St-Lincoln Center         40.77344
    ## 710   Broadway-7th Ave                  66th St-Lincoln Center         40.77344
    ## 711   Broadway-7th Ave                  66th St-Lincoln Center         40.77344
    ## 712   Broadway-7th Ave                  66th St-Lincoln Center         40.77344
    ## 713   Broadway-7th Ave                                 72nd St         40.77845
    ## 714   Broadway-7th Ave                                 72nd St         40.77845
    ## 715   Broadway-7th Ave                                 72nd St         40.77845
    ## 716   Broadway-7th Ave                                 72nd St         40.77845
    ## 717   Broadway-7th Ave                                 79th St         40.78393
    ## 718   Broadway-7th Ave                                 79th St         40.78393
    ## 719   Broadway-7th Ave                                 79th St         40.78393
    ## 720   Broadway-7th Ave                                 79th St         40.78393
    ## 721   Broadway-7th Ave                                 86th St         40.78864
    ## 722   Broadway-7th Ave                                 86th St         40.78864
    ## 723   Broadway-7th Ave                                 86th St         40.78864
    ## 724   Broadway-7th Ave                                 86th St         40.78864
    ## 725   Broadway-7th Ave                                 86th St         40.78864
    ## 726   Broadway-7th Ave                                 96th St         40.79392
    ## 727   Broadway-7th Ave                                 96th St         40.79392
    ## 728   Broadway-7th Ave                                 96th St         40.79392
    ## 729   Broadway-7th Ave                                 96th St         40.79392
    ## 730   Broadway-7th Ave                                 96th St         40.79392
    ## 731   Broadway-7th Ave                                 96th St         40.79392
    ## 732   Broadway-7th Ave                                Canal St         40.72285
    ## 733   Broadway-7th Ave                                Canal St         40.72285
    ## 734   Broadway-7th Ave                                Canal St         40.72285
    ## 735   Broadway-7th Ave                                Canal St         40.72285
    ## 736   Broadway-7th Ave              Cathedral Parkway-110th St         40.80397
    ## 737   Broadway-7th Ave              Cathedral Parkway-110th St         40.80397
    ## 738   Broadway-7th Ave              Cathedral Parkway-110th St         40.80397
    ## 739   Broadway-7th Ave                             Chambers St         40.71548
    ## 740   Broadway-7th Ave                             Chambers St         40.71548
    ## 741   Broadway-7th Ave                             Chambers St         40.71548
    ## 742   Broadway-7th Ave                             Chambers St         40.71548
    ## 743   Broadway-7th Ave                             Chambers St         40.71548
    ## 744   Broadway-7th Ave                          Christopher St         40.73342
    ## 745   Broadway-7th Ave                          Christopher St         40.73342
    ## 746   Broadway-7th Ave                          Christopher St         40.73342
    ## 747   Broadway-7th Ave                          Christopher St         40.73342
    ## 748   Broadway-7th Ave                          Christopher St         40.73342
    ## 749   Broadway-7th Ave                              Dyckman St         40.86053
    ## 750   Broadway-7th Ave                              Dyckman St         40.86053
    ## 751   Broadway-7th Ave                             Franklin St         40.71932
    ## 752   Broadway-7th Ave                             Franklin St         40.71932
    ## 753   Broadway-7th Ave                             Franklin St         40.71932
    ## 754   Broadway-7th Ave                             Franklin St         40.71932
    ## 755   Broadway-7th Ave                             Franklin St         40.71932
    ## 756   Broadway-7th Ave                              Houston St         40.72825
    ## 757   Broadway-7th Ave                              Houston St         40.72825
    ## 758   Broadway-7th Ave                              Houston St         40.72825
    ## 759   Broadway-7th Ave                              Houston St         40.72825
    ## 760   Broadway-7th Ave                              Houston St         40.72825
    ## 761   Broadway-7th Ave                              Houston St         40.72825
    ## 762   Broadway-7th Ave                              Houston St         40.72825
    ## 763   Broadway-7th Ave                              Houston St         40.72825
    ## 764   Broadway-7th Ave                    Marble Hill-225th St         40.87456
    ## 765   Broadway-7th Ave                    Marble Hill-225th St         40.87456
    ## 766   Broadway-7th Ave                               Rector St         40.70751
    ## 767   Broadway-7th Ave                               Rector St         40.70751
    ## 768   Broadway-7th Ave                               Rector St         40.70751
    ## 769   Broadway-7th Ave                               Rector St         40.70751
    ## 770   Broadway-7th Ave                               Rector St         40.70751
    ## 771   Broadway-7th Ave                               Rector St         40.70751
    ## 772   Broadway-7th Ave                             South Ferry         40.70207
    ## 773   Broadway-7th Ave                             South Ferry         40.70207
    ## 774   Broadway-7th Ave                             South Ferry         40.70207
    ## 775   Broadway-7th Ave                             South Ferry         40.70207
    ## 776   Broadway-7th Ave                             South Ferry         40.70207
    ## 777   Broadway-7th Ave                            Times Square         40.75529
    ## 778   Broadway-7th Ave                            Times Square         40.75529
    ## 779   Broadway-7th Ave                            Times Square         40.75529
    ## 780   Broadway-7th Ave                            Times Square         40.75529
    ## 781   Broadway-7th Ave                            Times Square         40.75529
    ## 782   Broadway-7th Ave                            Times Square         40.75529
    ## 783   Broadway-7th Ave                            Times Square         40.75529
    ## 784   Broadway-7th Ave                            Times Square         40.75529
    ## 785   Broadway-7th Ave                            Times Square         40.75529
    ## 786   Broadway-7th Ave             Van Cortlandt Park-242nd St         40.88925
    ## 787   Broadway-7th Ave             Van Cortlandt Park-242nd St         40.88925
    ## 788   Broadway-7th Ave             Van Cortlandt Park-242nd St         40.88925
    ## 789   Broadway-7th Ave             Van Cortlandt Park-242nd St         40.88925
    ## 790           Canarsie                                  1st Av         40.73095
    ## 791           Canarsie                                  1st Av         40.73095
    ## 792           Canarsie                                  1st Av         40.73095
    ## 793           Canarsie                                  1st Av         40.73095
    ## 794           Canarsie                                  3rd Av         40.73285
    ## 795           Canarsie                                  3rd Av         40.73285
    ## 796           Canarsie                                  3rd Av         40.73285
    ## 797           Canarsie                                  3rd Av         40.73285
    ## 798           Canarsie                                  6th Av         40.73734
    ## 799           Canarsie                                  6th Av         40.73734
    ## 800           Canarsie                                  8th Av         40.73978
    ## 801           Canarsie                                  8th Av         40.73978
    ## 802           Canarsie                             Atlantic Av         40.67535
    ## 803           Canarsie                             Atlantic Av         40.67535
    ## 804           Canarsie                             Atlantic Av         40.67535
    ## 805           Canarsie                              Bedford Av         40.71730
    ## 806           Canarsie                              Bedford Av         40.71730
    ## 807           Canarsie                              Bedford Av         40.71730
    ## 808           Canarsie                              Bedford Av         40.71730
    ## 809           Canarsie                             Bushwick Av         40.68283
    ## 810           Canarsie             Canarsie - Rockaway Parkway         40.64665
    ## 811           Canarsie             Canarsie - Rockaway Parkway         40.64576
    ## 812           Canarsie             Canarsie - Rockaway Parkway         40.64596
    ## 813           Canarsie                               DeKalb Av         40.70381
    ## 814           Canarsie                               DeKalb Av         40.70381
    ## 815           Canarsie                               DeKalb Av         40.70381
    ## 816           Canarsie                               DeKalb Av         40.70381
    ## 817           Canarsie                               DeKalb Av         40.70381
    ## 818           Canarsie                               DeKalb Av         40.70381
    ## 819           Canarsie                               DeKalb Av         40.70381
    ## 820           Canarsie                               DeKalb Av         40.70381
    ## 821           Canarsie                           East 105th St         40.65057
    ## 822           Canarsie                           East 105th St         40.65057
    ## 823           Canarsie                               Graham Av         40.71457
    ## 824           Canarsie                               Graham Av         40.71457
    ## 825           Canarsie                               Graham Av         40.71457
    ## 826           Canarsie                               Graham Av         40.71457
    ## 827           Canarsie                                Grand St         40.71193
    ## 828           Canarsie                                Grand St         40.71193
    ## 829           Canarsie                                Grand St         40.71193
    ## 830           Canarsie                                Grand St         40.71193
    ## 831           Canarsie                               Halsey St         40.69560
    ## 832           Canarsie                               Halsey St         40.69560
    ## 833           Canarsie                               Halsey St         40.69560
    ## 834           Canarsie                               Halsey St         40.69560
    ## 835           Canarsie                               Halsey St         40.69560
    ## 836           Canarsie                               Halsey St         40.69560
    ## 837           Canarsie                            Jefferson St         40.70661
    ## 838           Canarsie                            Jefferson St         40.70661
    ## 839           Canarsie                            Jefferson St         40.70661
    ## 840           Canarsie                            Jefferson St         40.70661
    ## 841           Canarsie                            Jefferson St         40.70661
    ## 842           Canarsie                            Jefferson St         40.70661
    ## 843           Canarsie                              Livonia Av         40.66404
    ## 844           Canarsie                              Livonia Av         40.66404
    ## 845           Canarsie                              Lorimer St         40.71406
    ## 846           Canarsie                              Lorimer St         40.71406
    ## 847           Canarsie                             Montrose Av         40.70774
    ## 848           Canarsie                             Montrose Av         40.70774
    ## 849           Canarsie                               Morgan Av         40.70615
    ## 850           Canarsie                               Morgan Av         40.70615
    ## 851           Canarsie                               Morgan Av         40.70615
    ## 852           Canarsie                               Myrtle Av         40.69981
    ## 853           Canarsie                               Myrtle Av         40.69981
    ## 854           Canarsie                               Myrtle Av         40.69981
    ## 855           Canarsie                             New Lots Av         40.65873
    ## 856           Canarsie                               Sutter Av         40.66937
    ## 857           Canarsie                            Union Square         40.73479
    ## 858           Canarsie                            Union Square         40.73479
    ## 859           Canarsie                               Wilson Av         40.68876
    ## 860              Clark                            Borough Hall         40.69322
    ## 861              Clark                            Borough Hall         40.69322
    ## 862              Clark                            Borough Hall         40.69322
    ## 863              Clark                            Borough Hall         40.69322
    ## 864              Clark                                Clark St         40.69747
    ## 865              Clark                                Clark St         40.69747
    ## 866              Clark                               Fulton St         40.70942
    ## 867              Clark                               Fulton St         40.70942
    ## 868              Clark                               Fulton St         40.70942
    ## 869              Clark                               Fulton St         40.70942
    ## 870              Clark                              Park Place         40.71305
    ## 871              Clark                                 Wall St         40.70682
    ## 872              Clark                                 Wall St         40.70682
    ## 873              Clark                                 Wall St         40.70682
    ## 874              Clark                                 Wall St         40.70682
    ## 875              Clark                                 Wall St         40.70682
    ## 876              Clark                                 Wall St         40.70682
    ## 877          Concourse                                155th St         40.83013
    ## 878          Concourse                                167th St         40.83377
    ## 879          Concourse                                167th St         40.83377
    ## 880          Concourse                                167th St         40.83377
    ## 881          Concourse                                167th St         40.83377
    ## 882          Concourse                                167th St         40.83377
    ## 883          Concourse                                167th St         40.83377
    ## 884          Concourse                                170th St         40.83931
    ## 885          Concourse                                170th St         40.83931
    ## 886          Concourse                                170th St         40.83931
    ## 887          Concourse                                170th St         40.83931
    ## 888          Concourse                                170th St         40.83931
    ## 889          Concourse                           174-175th Sts         40.84590
    ## 890          Concourse                           174-175th Sts         40.84590
    ## 891          Concourse                           174-175th Sts         40.84590
    ## 892          Concourse                           174-175th Sts         40.84590
    ## 893          Concourse                         182nd-183rd Sts         40.85609
    ## 894          Concourse                         182nd-183rd Sts         40.85609
    ## 895          Concourse                         182nd-183rd Sts         40.85609
    ## 896          Concourse                         182nd-183rd Sts         40.85609
    ## 897          Concourse                       Bedford Park Blvd         40.87324
    ## 898          Concourse                       Bedford Park Blvd         40.87324
    ## 899          Concourse                       Bedford Park Blvd         40.87324
    ## 900          Concourse                       Bedford Park Blvd         40.87324
    ## 901          Concourse                       Bedford Park Blvd         40.87324
    ## 902          Concourse                              Fordham Rd         40.86130
    ## 903          Concourse                              Fordham Rd         40.86130
    ## 904          Concourse                              Fordham Rd         40.86130
    ## 905          Concourse                              Fordham Rd         40.86130
    ## 906          Concourse                              Fordham Rd         40.86130
    ## 907          Concourse                              Fordham Rd         40.86130
    ## 908          Concourse                          Kingsbridge Rd         40.86698
    ## 909          Concourse                          Kingsbridge Rd         40.86698
    ## 910          Concourse                          Kingsbridge Rd         40.86698
    ## 911          Concourse                          Kingsbridge Rd         40.86698
    ## 912          Concourse                          Kingsbridge Rd         40.86698
    ## 913          Concourse                        Norwood-205th St         40.87481
    ## 914          Concourse                        Norwood-205th St         40.87481
    ## 915          Concourse                        Norwood-205th St         40.87481
    ## 916          Concourse                        Norwood-205th St         40.87481
    ## 917          Concourse                              Tremont Av         40.85041
    ## 918          Concourse                              Tremont Av         40.85041
    ## 919          Concourse                              Tremont Av         40.85041
    ## 920          Concourse                              Tremont Av         40.85041
    ## 921          Concourse                              Tremont Av         40.85041
    ## 922          Concourse                 Yankee Stadium-161st St         40.82791
    ## 923          Concourse                 Yankee Stadium-161st St         40.82791
    ## 924          Concourse                 Yankee Stadium-161st St         40.82791
    ## 925          Concourse                 Yankee Stadium-161st St         40.82791
    ## 926          Concourse                 Yankee Stadium-161st St         40.82791
    ## 927          Concourse                 Yankee Stadium-161st St         40.82791
    ## 928          Concourse                 Yankee Stadium-161st St         40.82791
    ## 929          Concourse                 Yankee Stadium-161st St         40.82791
    ## 930       Coney Island                            Stillwell Av         40.57742
    ## 931       Coney Island                             West 8th St         40.57613
    ## 932       Coney Island                             West 8th St         40.57613
    ## 933          Crosstown                                 21st St         40.74406
    ## 934          Crosstown                                 21st St         40.74406
    ## 935          Crosstown                                 21st St         40.74406
    ## 936          Crosstown                    Bedford-Nostrand Avs         40.68963
    ## 937          Crosstown                    Bedford-Nostrand Avs         40.68963
    ## 938          Crosstown                    Bedford-Nostrand Avs         40.68963
    ## 939          Crosstown                    Bedford-Nostrand Avs         40.68963
    ## 940          Crosstown                    Bedford-Nostrand Avs         40.68963
    ## 941          Crosstown                    Bedford-Nostrand Avs         40.68963
    ## 942          Crosstown                                Broadway         40.70609
    ## 943          Crosstown                                Broadway         40.70609
    ## 944          Crosstown                                Broadway         40.70609
    ## 945          Crosstown                                Broadway         40.70609
    ## 946          Crosstown                              Classon Av         40.68887
    ## 947          Crosstown                              Classon Av         40.68887
    ## 948          Crosstown                              Classon Av         40.68887
    ## 949          Crosstown                  Clinton-Washington Avs         40.68809
    ## 950          Crosstown                  Clinton-Washington Avs         40.68809
    ## 951          Crosstown                  Clinton-Washington Avs         40.68809
    ## 952          Crosstown                  Clinton-Washington Avs         40.68809
    ## 953          Crosstown                  Clinton-Washington Avs         40.68809
    ## 954          Crosstown                  Clinton-Washington Avs         40.68809
    ## 955          Crosstown                  Clinton-Washington Avs         40.68809
    ## 956          Crosstown                  Clinton-Washington Avs         40.68809
    ## 957          Crosstown                             Flushing Av         40.70038
    ## 958          Crosstown                             Flushing Av         40.70038
    ## 959          Crosstown                               Fulton St         40.68712
    ## 960          Crosstown                               Fulton St         40.68712
    ## 961          Crosstown                           Greenpoint Av         40.73135
    ## 962          Crosstown                           Greenpoint Av         40.73135
    ## 963          Crosstown                           Greenpoint Av         40.73135
    ## 964          Crosstown                           Greenpoint Av         40.73135
    ## 965          Crosstown                           Greenpoint Av         40.73135
    ## 966          Crosstown           Long Island City-Court Square         40.74655
    ## 967          Crosstown           Long Island City-Court Square         40.74655
    ## 968          Crosstown           Long Island City-Court Square         40.74655
    ## 969          Crosstown                         Metropolitan Av         40.71279
    ## 970          Crosstown                         Metropolitan Av         40.71279
    ## 971          Crosstown                         Metropolitan Av         40.71279
    ## 972          Crosstown                         Metropolitan Av         40.71279
    ## 973          Crosstown                   Myrtle-Willoughby Avs         40.69457
    ## 974          Crosstown                   Myrtle-Willoughby Avs         40.69457
    ## 975          Crosstown                               Nassau Av         40.72463
    ## 976          Crosstown                               Nassau Av         40.72463
    ## 977          Crosstown                               Nassau Av         40.72463
    ## 978          Crosstown                               Nassau Av         40.72463
    ## 979          Crosstown                               Nassau Av         40.72463
    ## 980          Crosstown                               Nassau Av         40.72463
    ## 981             Culver                                 18th Av         40.62976
    ## 982             Culver                                 18th Av         40.62976
    ## 983             Culver                                 18th Av         40.62976
    ## 984             Culver                                 18th Av         40.62976
    ## 985             Culver                                    Av I         40.62532
    ## 986             Culver                                    Av I         40.62532
    ## 987             Culver                                    Av I         40.62532
    ## 988             Culver                                    Av I         40.62532
    ## 989             Culver                                    Av N         40.61514
    ## 990             Culver                                    Av N         40.61514
    ## 991             Culver                                    Av N         40.61514
    ## 992             Culver                                    Av N         40.61514
    ## 993             Culver                                    Av P         40.60894
    ## 994             Culver                                    Av P         40.60894
    ## 995             Culver                                    Av U         40.59606
    ## 996             Culver                                    Av U         40.59606
    ## 997             Culver                                    Av U         40.59606
    ## 998             Culver                                    Av U         40.59606
    ## 999             Culver                                    Av X         40.58962
    ## 1000            Culver                                    Av X         40.58962
    ## 1001            Culver                     Bay Parkway-22nd Av         40.62077
    ## 1002            Culver                     Bay Parkway-22nd Av         40.62077
    ## 1003            Culver                     Bay Parkway-22nd Av         40.62077
    ## 1004            Culver                               Ditmas Av         40.63612
    ## 1005            Culver                               Ditmas Av         40.63612
    ## 1006            Culver                               Ditmas Av         40.63612
    ## 1007            Culver                               Ditmas Av         40.63612
    ## 1008            Culver                           Kings Highway         40.60322
    ## 1009            Culver                           Kings Highway         40.60322
    ## 1010            Culver                           Kings Highway         40.60322
    ## 1011            Culver                           Kings Highway         40.60322
    ## 1012            Culver                   Neptune Av-Van Siclen         40.58101
    ## 1013            Culver                   Neptune Av-Van Siclen         40.58101
    ## 1014           Dyre Av                           Baychester Av         40.87866
    ## 1015           Dyre Av                           Baychester Av         40.87866
    ## 1016           Dyre Av                     Eastchester-Dyre Av         40.88830
    ## 1017           Dyre Av                             Gun Hill Rd         40.86953
    ## 1018           Dyre Av                             Gun Hill Rd         40.86953
    ## 1019           Dyre Av                             Morris Park         40.85436
    ## 1020           Dyre Av                          Pelham Parkway         40.85898
    ## 1021   Eastern Parkway                Atlantic Av-Barclays Ctr         40.68436
    ## 1022   Eastern Parkway                               Bergen St         40.68083
    ## 1023   Eastern Parkway                               Bergen St         40.68083
    ## 1024   Eastern Parkway                               Bergen St         40.68083
    ## 1025   Eastern Parkway                               Bergen St         40.68083
    ## 1026   Eastern Parkway                               Bergen St         40.68083
    ## 1027   Eastern Parkway         Eastern Parkway-Brooklyn Museum         40.67199
    ## 1028   Eastern Parkway         Eastern Parkway-Brooklyn Museum         40.67199
    ## 1029   Eastern Parkway                             Franklin Av         40.67068
    ## 1030   Eastern Parkway                             Franklin Av         40.67068
    ## 1031   Eastern Parkway                             Franklin Av         40.67068
    ## 1032   Eastern Parkway                             Franklin Av         40.67068
    ## 1033   Eastern Parkway                        Grand Army Plaza         40.67524
    ## 1034   Eastern Parkway                        Grand Army Plaza         40.67524
    ## 1035   Eastern Parkway                        Grand Army Plaza         40.67524
    ## 1036   Eastern Parkway                        Grand Army Plaza         40.67524
    ## 1037   Eastern Parkway                                 Hoyt St         40.69055
    ## 1038   Eastern Parkway                                 Hoyt St         40.69055
    ## 1039   Eastern Parkway                                 Hoyt St         40.69055
    ## 1040   Eastern Parkway                                 Hoyt St         40.69055
    ## 1041   Eastern Parkway                                 Hoyt St         40.69055
    ## 1042   Eastern Parkway                             Kingston Av         40.66940
    ## 1043   Eastern Parkway                             Kingston Av         40.66940
    ## 1044   Eastern Parkway                               Nevins St         40.68825
    ## 1045   Eastern Parkway                               Nevins St         40.68825
    ## 1046   Eastern Parkway                               Nevins St         40.68825
    ## 1047   Eastern Parkway                               Nevins St         40.68825
    ## 1048   Eastern Parkway                             Nostrand Av         40.66985
    ## 1049   Eastern Parkway                             Nostrand Av         40.66985
    ## 1050   Eastern Parkway                                Utica Av         40.66890
    ## 1051   Eastern Parkway                                Utica Av         40.66890
    ## 1052   Eastern Parkway                                Utica Av         40.66890
    ## 1053   Eastern Parkway                                Utica Av         40.66890
    ## 1054   Eastern Parkway                                Utica Av         40.66890
    ## 1055   Eastern Parkway                                Utica Av         40.66890
    ## 1056   Eastern Parkway                                Utica Av         40.66890
    ## 1057          Flushing                                103rd St         40.74986
    ## 1058          Flushing                                103rd St         40.74986
    ## 1059          Flushing                                103rd St         40.74986
    ## 1060          Flushing                                103rd St         40.74986
    ## 1061          Flushing                                111th St         40.75173
    ## 1062          Flushing                                111th St         40.75173
    ## 1063          Flushing                                111th St         40.75173
    ## 1064          Flushing                    45 Rd-Court House Sq         40.74702
    ## 1065          Flushing                    45 Rd-Court House Sq         40.74702
    ## 1066          Flushing                    45 Rd-Court House Sq         40.74702
    ## 1067          Flushing                                  5th Av         40.75382
    ## 1068          Flushing                                  5th Av         40.75382
    ## 1069          Flushing                                  5th Av         40.75382
    ## 1070          Flushing                 82nd St-Jackson Heights         40.74766
    ## 1071          Flushing                 82nd St-Jackson Heights         40.74766
    ## 1072          Flushing                 82nd St-Jackson Heights         40.74766
    ## 1073          Flushing                        90th St Elmhurst         40.74841
    ## 1074          Flushing                        90th St Elmhurst         40.74841
    ## 1075          Flushing                        90th St Elmhurst         40.74841
    ## 1076          Flushing                        Bliss St-46th St         40.74313
    ## 1077          Flushing                        Bliss St-46th St         40.74313
    ## 1078          Flushing                        Bliss St-46th St         40.74313
    ## 1079          Flushing                        Bliss St-46th St         40.74313
    ## 1080          Flushing                        Bliss St-46th St         40.74313
    ## 1081          Flushing                        Bliss St-46th St         40.74313
    ## 1082          Flushing                        Broadway-74th St         40.74685
    ## 1083          Flushing                         Fisk Av-69th St         40.74632
    ## 1084          Flushing                         Fisk Av-69th St         40.74632
    ## 1085          Flushing                        Flushing-Main St         40.75960
    ## 1086          Flushing                        Flushing-Main St         40.75960
    ## 1087          Flushing                        Flushing-Main St         40.75960
    ## 1088          Flushing                        Flushing-Main St         40.75960
    ## 1089          Flushing                        Flushing-Main St         40.75960
    ## 1090          Flushing                        Flushing-Main St         40.75960
    ## 1091          Flushing                        Flushing-Main St         40.75960
    ## 1092          Flushing                        Flushing-Main St         40.75960
    ## 1093          Flushing                        Flushing-Main St         40.75960
    ## 1094          Flushing                        Flushing-Main St         40.75960
    ## 1095          Flushing                   Grand Central-42nd St         40.75143
    ## 1096          Flushing                           Hunters Point         40.74222
    ## 1097          Flushing                           Hunters Point         40.74222
    ## 1098          Flushing                           Hunters Point         40.74222
    ## 1099          Flushing                           Junction Blvd         40.74914
    ## 1100          Flushing                           Junction Blvd         40.74914
    ## 1101          Flushing                           Junction Blvd         40.74914
    ## 1102          Flushing                           Junction Blvd         40.74914
    ## 1103          Flushing                           Junction Blvd         40.74914
    ## 1104          Flushing                      Lincoln Av-52nd St         40.74415
    ## 1105          Flushing                      Lincoln Av-52nd St         40.74415
    ## 1106          Flushing                      Lincoln Av-52nd St         40.74415
    ## 1107          Flushing                      Lincoln Av-52nd St         40.74415
    ## 1108          Flushing                       Lowery St-40th St         40.74378
    ## 1109          Flushing                       Lowery St-40th St         40.74378
    ## 1110          Flushing                       Lowery St-40th St         40.74378
    ## 1111          Flushing                       Lowery St-40th St         40.74378
    ## 1112          Flushing                    Mets - Willets Point         40.75462
    ## 1113          Flushing                    Mets - Willets Point         40.75462
    ## 1114          Flushing                    Mets - Willets Point         40.75462
    ## 1115          Flushing                    Mets - Willets Point         40.75462
    ## 1116          Flushing                        Queensboro Plaza         40.75058
    ## 1117          Flushing                        Queensboro Plaza         40.75058
    ## 1118          Flushing                       Rawson St-33rd St         40.74459
    ## 1119          Flushing                       Rawson St-33rd St         40.74459
    ## 1120          Flushing                       Rawson St-33rd St         40.74459
    ## 1121          Flushing                       Rawson St-33rd St         40.74459
    ## 1122          Flushing                       Rawson St-33rd St         40.74459
    ## 1123          Flushing                       Rawson St-33rd St         40.74459
    ## 1124          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1125          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1126          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1127          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1128          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1129          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1130          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1131          Flushing                  Vernon Blvd-Jackson Av         40.74263
    ## 1132          Flushing                     Woodside Av-61st St         40.74563
    ## 1133          Flushing                     Woodside Av-61st St         40.74563
    ## 1134          Flushing                     Woodside Av-61st St         40.74563
    ## 1135          Franklin                         Botanic Gardens         40.67034
    ## 1136          Franklin                             Franklin Av         40.68060
    ## 1137          Franklin                              Park Place         40.67477
    ## 1138          Franklin                              Park Place         40.67477
    ## 1139            Fulton         Broadway Junction-East New York         40.67833
    ## 1140            Fulton                Clinton & Washington Avs         40.68326
    ## 1141            Fulton                Clinton & Washington Avs         40.68326
    ## 1142            Fulton                Clinton & Washington Avs         40.68326
    ## 1143            Fulton                Clinton & Washington Avs         40.68326
    ## 1144            Fulton                Clinton & Washington Avs         40.68326
    ## 1145            Fulton                               Euclid Av         40.67538
    ## 1146            Fulton                               Euclid Av         40.67538
    ## 1147            Fulton                               Euclid Av         40.67538
    ## 1148            Fulton                               Euclid Av         40.67538
    ## 1149            Fulton                               Euclid Av         40.67538
    ## 1150            Fulton                             Franklin Av         40.68138
    ## 1151            Fulton                     Hoyt & Schermerhorn         40.68848
    ## 1152            Fulton                     Hoyt & Schermerhorn         40.68848
    ## 1153            Fulton                   Jay St - Borough Hall         40.69234
    ## 1154            Fulton                   Jay St - Borough Hall         40.69234
    ## 1155            Fulton                   Jay St - Borough Hall         40.69234
    ## 1156            Fulton                   Jay St - Borough Hall         40.69234
    ## 1157            Fulton                   Jay St - Borough Hall         40.69234
    ## 1158            Fulton                   Jay St - Borough Hall         40.69234
    ## 1159            Fulton                   Jay St - Borough Hall         40.69234
    ## 1160            Fulton                   Jay St - Borough Hall         40.69234
    ## 1161            Fulton                   Jay St - Borough Hall         40.69234
    ## 1162            Fulton                   Jay St - Borough Hall         40.69234
    ## 1163            Fulton                   Jay St - Borough Hall         40.69234
    ## 1164            Fulton                   Jay St - Borough Hall         40.69234
    ## 1165            Fulton                   Jay St - Borough Hall         40.69234
    ## 1166            Fulton                         Kingston-Throop         40.67992
    ## 1167            Fulton                         Kingston-Throop         40.67992
    ## 1168            Fulton                         Kingston-Throop         40.67992
    ## 1169            Fulton                         Kingston-Throop         40.67992
    ## 1170            Fulton                            Lafayette Av         40.68611
    ## 1171            Fulton                            Lafayette Av         40.68611
    ## 1172            Fulton                            Lafayette Av         40.68611
    ## 1173            Fulton                            Lafayette Av         40.68611
    ## 1174            Fulton                            Lafayette Av         40.68611
    ## 1175            Fulton                            Lafayette Av         40.68611
    ## 1176            Fulton                            Lafayette Av         40.68611
    ## 1177            Fulton                            Lafayette Av         40.68611
    ## 1178            Fulton                            Lafayette Av         40.68611
    ## 1179            Fulton                              Liberty Av         40.67454
    ## 1180            Fulton                              Liberty Av         40.67454
    ## 1181            Fulton                              Liberty Av         40.67454
    ## 1182            Fulton                              Liberty Av         40.67454
    ## 1183            Fulton                             Nostrand Av         40.68044
    ## 1184            Fulton                             Nostrand Av         40.68044
    ## 1185            Fulton                             Nostrand Av         40.68044
    ## 1186            Fulton                             Nostrand Av         40.68044
    ## 1187            Fulton                                Ralph Av         40.67882
    ## 1188            Fulton                                Ralph Av         40.67882
    ## 1189            Fulton                                Ralph Av         40.67882
    ## 1190            Fulton                             Rockaway Av         40.67834
    ## 1191            Fulton                             Rockaway Av         40.67834
    ## 1192            Fulton                             Rockaway Av         40.67834
    ## 1193            Fulton                             Rockaway Av         40.67834
    ## 1194            Fulton                             Rockaway Av         40.67834
    ## 1195            Fulton                             Rockaway Av         40.67834
    ## 1196            Fulton                             Shepherd Av         40.67413
    ## 1197            Fulton                             Shepherd Av         40.67413
    ## 1198            Fulton                             Shepherd Av         40.67413
    ## 1199            Fulton                             Shepherd Av         40.67413
    ## 1200            Fulton                                Utica Av         40.67936
    ## 1201            Fulton                                Utica Av         40.67936
    ## 1202            Fulton                                Utica Av         40.67936
    ## 1203            Fulton                                Utica Av         40.67936
    ## 1204            Fulton                           Van Siclen Av         40.67271
    ## 1205            Fulton                           Van Siclen Av         40.67271
    ## 1206            Fulton                           Van Siclen Av         40.67271
    ## 1207            Fulton                           Van Siclen Av         40.67271
    ## 1208            Jerome                                138th St         40.81322
    ## 1209            Jerome                                138th St         40.81322
    ## 1210            Jerome                                138th St         40.81322
    ## 1211            Jerome                149th St-Grand Concourse         40.81838
    ## 1212            Jerome                149th St-Grand Concourse         40.81838
    ## 1213            Jerome                149th St-Grand Concourse         40.81838
    ## 1214            Jerome                149th St-Grand Concourse         40.81838
    ## 1215            Jerome                                167th St         40.83554
    ## 1216            Jerome                                167th St         40.83554
    ## 1217            Jerome                                167th St         40.83554
    ## 1218            Jerome                                167th St         40.83554
    ## 1219            Jerome                                170th St         40.84007
    ## 1220            Jerome                                170th St         40.84007
    ## 1221            Jerome                                170th St         40.84007
    ## 1222            Jerome                                176th St         40.84848
    ## 1223            Jerome                                176th St         40.84848
    ## 1224            Jerome                                183rd St         40.85841
    ## 1225            Jerome                                183rd St         40.85841
    ## 1226            Jerome                                183rd St         40.85841
    ## 1227            Jerome        Bedford Park Blvd-Lehman College         40.87341
    ## 1228            Jerome                             Burnside Av         40.85345
    ## 1229            Jerome                             Burnside Av         40.85345
    ## 1230            Jerome                             Burnside Av         40.85345
    ## 1231            Jerome                             Burnside Av         40.85345
    ## 1232            Jerome                              Fordham Rd         40.86280
    ## 1233            Jerome                              Fordham Rd         40.86280
    ## 1234            Jerome                              Fordham Rd         40.86280
    ## 1235            Jerome                              Fordham Rd         40.86280
    ## 1236            Jerome                          Kingsbridge Rd         40.86776
    ## 1237            Jerome                          Kingsbridge Rd         40.86776
    ## 1238            Jerome                          Kingsbridge Rd         40.86776
    ## 1239            Jerome                         Mosholu Parkway         40.87975
    ## 1240            Jerome                         Mosholu Parkway         40.87975
    ## 1241            Jerome                         Mosholu Parkway         40.87975
    ## 1242            Jerome                         Mosholu Parkway         40.87975
    ## 1243            Jerome                              Mt Eden Av         40.84443
    ## 1244            Jerome                              Mt Eden Av         40.84443
    ## 1245            Jerome                                Woodlawn         40.88604
    ## 1246            Jerome                                Woodlawn         40.88604
    ## 1247            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1248            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1249            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1250            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1251            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1252            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1253            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1254            Jerome                 Yankee Stadium-161st St         40.82799
    ## 1255             Lenox             110th St-Central Park North         40.79908
    ## 1256             Lenox             110th St-Central Park North         40.79908
    ## 1257             Lenox                                116th St         40.80210
    ## 1258             Lenox                                116th St         40.80210
    ## 1259             Lenox                                116th St         40.80210
    ## 1260             Lenox                                116th St         40.80210
    ## 1261             Lenox                                125th St         40.80775
    ## 1262             Lenox                                125th St         40.80775
    ## 1263             Lenox                                125th St         40.80775
    ## 1264             Lenox                                125th St         40.80775
    ## 1265             Lenox                                135th St         40.81423
    ## 1266             Lenox                                135th St         40.81423
    ## 1267             Lenox                                135th St         40.81423
    ## 1268             Lenox                                135th St         40.81423
    ## 1269             Lenox                                145th St         40.82042
    ## 1270             Lenox                                145th St         40.82042
    ## 1271             Lenox                                145th St         40.82042
    ## 1272             Lenox                                145th St         40.82042
    ## 1273             Lenox                         Harlem-148th St         40.82388
    ## 1274         Lexington                                103rd St         40.79060
    ## 1275         Lexington                                103rd St         40.79060
    ## 1276         Lexington                                110th St         40.79502
    ## 1277         Lexington                                110th St         40.79502
    ## 1278         Lexington                                110th St         40.79502
    ## 1279         Lexington                                110th St         40.79502
    ## 1280         Lexington                                116th St         40.79863
    ## 1281         Lexington                                116th St         40.79863
    ## 1282         Lexington                                116th St         40.79863
    ## 1283         Lexington                                116th St         40.79863
    ## 1284         Lexington                                125th St         40.80414
    ## 1285         Lexington                                125th St         40.80414
    ## 1286         Lexington                                125th St         40.80414
    ## 1287         Lexington                                125th St         40.80414
    ## 1288         Lexington                                125th St         40.80414
    ## 1289         Lexington                    14th St-Union Square         40.73467
    ## 1290         Lexington                    14th St-Union Square         40.73467
    ## 1291         Lexington                    14th St-Union Square         40.73467
    ## 1292         Lexington                    14th St-Union Square         40.73467
    ## 1293         Lexington                    14th St-Union Square         40.73467
    ## 1294         Lexington                    14th St-Union Square         40.73467
    ## 1295         Lexington                                 23rd St         40.73986
    ## 1296         Lexington                                 23rd St         40.73986
    ## 1297         Lexington                                 23rd St         40.73986
    ## 1298         Lexington                                 23rd St         40.73986
    ## 1299         Lexington                                 23rd St         40.73986
    ## 1300         Lexington                                 23rd St         40.73986
    ## 1301         Lexington                                 23rd St         40.73986
    ## 1302         Lexington                                 23rd St         40.73986
    ## 1303         Lexington                                 23rd St         40.73986
    ## 1304         Lexington                                 28th St         40.74307
    ## 1305         Lexington                                 28th St         40.74307
    ## 1306         Lexington                                 28th St         40.74307
    ## 1307         Lexington                                 28th St         40.74307
    ## 1308         Lexington                                 28th St         40.74307
    ## 1309         Lexington                                 28th St         40.74307
    ## 1310         Lexington                                 28th St         40.74307
    ## 1311         Lexington                                 33rd St         40.74608
    ## 1312         Lexington                                 33rd St         40.74608
    ## 1313         Lexington                                 33rd St         40.74608
    ## 1314         Lexington                                 33rd St         40.74608
    ## 1315         Lexington                                 33rd St         40.74608
    ## 1316         Lexington                                 33rd St         40.74608
    ## 1317         Lexington                                 33rd St         40.74608
    ## 1318         Lexington                                 33rd St         40.74608
    ## 1319         Lexington                                 33rd St         40.74608
    ## 1320         Lexington                                 51st St         40.75711
    ## 1321         Lexington                                 51st St         40.75711
    ## 1322         Lexington                                 51st St         40.75711
    ## 1323         Lexington                                 51st St         40.75711
    ## 1324         Lexington                                 51st St         40.75711
    ## 1325         Lexington                                 51st St         40.75711
    ## 1326         Lexington                                 51st St         40.75711
    ## 1327         Lexington                                 51st St         40.75711
    ## 1328         Lexington                                 51st St         40.75711
    ## 1329         Lexington                                 59th St         40.76253
    ## 1330         Lexington                                 59th St         40.76253
    ## 1331         Lexington                                 59th St         40.76253
    ## 1332         Lexington                                 59th St         40.76253
    ## 1333         Lexington                                 59th St         40.76253
    ## 1334         Lexington                                 59th St         40.76253
    ## 1335         Lexington                                 59th St         40.76253
    ## 1336         Lexington                  68th St-Hunter College         40.76814
    ## 1337         Lexington                  68th St-Hunter College         40.76814
    ## 1338         Lexington                  68th St-Hunter College         40.76814
    ## 1339         Lexington                  68th St-Hunter College         40.76814
    ## 1340         Lexington                  68th St-Hunter College         40.76814
    ## 1341         Lexington                                 77th St         40.77362
    ## 1342         Lexington                                 77th St         40.77362
    ## 1343         Lexington                                 77th St         40.77362
    ## 1344         Lexington                                 77th St         40.77362
    ## 1345         Lexington                                 77th St         40.77362
    ## 1346         Lexington                                 77th St         40.77362
    ## 1347         Lexington                                 77th St         40.77362
    ## 1348         Lexington                                 77th St         40.77362
    ## 1349         Lexington                                 86th St         40.77949
    ## 1350         Lexington                                 86th St         40.77949
    ## 1351         Lexington                                 86th St         40.77949
    ## 1352         Lexington                                 86th St         40.77949
    ## 1353         Lexington                                 86th St         40.77949
    ## 1354         Lexington                                 86th St         40.77949
    ## 1355         Lexington                                 86th St         40.77949
    ## 1356         Lexington                                 86th St         40.77949
    ## 1357         Lexington                                 96th St         40.78567
    ## 1358         Lexington                                 96th St         40.78567
    ## 1359         Lexington                                 96th St         40.78567
    ## 1360         Lexington                                 96th St         40.78567
    ## 1361         Lexington                             Astor Place         40.73005
    ## 1362         Lexington                             Astor Place         40.73005
    ## 1363         Lexington                             Bleecker St         40.72592
    ## 1364         Lexington                             Bleecker St         40.72592
    ## 1365         Lexington                             Bleecker St         40.72592
    ## 1366         Lexington                             Bleecker St         40.72592
    ## 1367         Lexington                             Bleecker St         40.72592
    ## 1368         Lexington                             Bleecker St         40.72592
    ## 1369         Lexington                             Bleecker St         40.72592
    ## 1370         Lexington                            Borough Hall         40.69240
    ## 1371         Lexington                            Borough Hall         40.69240
    ## 1372         Lexington                            Borough Hall         40.69240
    ## 1373         Lexington                            Borough Hall         40.69240
    ## 1374         Lexington                            Borough Hall         40.69240
    ## 1375         Lexington                           Bowling Green         40.70482
    ## 1376         Lexington                           Bowling Green         40.70482
    ## 1377         Lexington                           Bowling Green         40.70482
    ## 1378         Lexington                           Bowling Green         40.70482
    ## 1379         Lexington                           Bowling Green         40.70482
    ## 1380         Lexington               Brooklyn Bridge-City Hall         40.71287
    ## 1381         Lexington               Brooklyn Bridge-City Hall         40.71272
    ## 1382         Lexington               Brooklyn Bridge-City Hall         40.71233
    ## 1383         Lexington               Brooklyn Bridge-City Hall         40.71186
    ## 1384         Lexington               Brooklyn Bridge-City Hall         40.71182
    ## 1385         Lexington               Brooklyn Bridge-City Hall         40.71381
    ## 1386         Lexington               Brooklyn Bridge-City Hall         40.71389
    ## 1387         Lexington               Brooklyn Bridge-City Hall         40.71307
    ## 1388         Lexington               Brooklyn Bridge-City Hall         40.71307
    ## 1389         Lexington                                Canal St         40.71880
    ## 1390         Lexington                                Canal St         40.71880
    ## 1391         Lexington                                Canal St         40.71880
    ## 1392         Lexington                                Canal St         40.71880
    ## 1393         Lexington                                Canal St         40.71880
    ## 1394         Lexington                                Canal St         40.71880
    ## 1395         Lexington                                Canal St         40.71880
    ## 1396         Lexington                               Fulton St         40.71037
    ## 1397         Lexington                               Fulton St         40.71037
    ## 1398         Lexington                               Fulton St         40.71037
    ## 1399         Lexington                               Fulton St         40.71037
    ## 1400         Lexington                               Fulton St         40.71037
    ## 1401         Lexington                   Grand Central-42nd St         40.75178
    ## 1402         Lexington                   Grand Central-42nd St         40.75178
    ## 1403         Lexington                   Grand Central-42nd St         40.75178
    ## 1404         Lexington                   Grand Central-42nd St         40.75178
    ## 1405         Lexington                   Grand Central-42nd St         40.75178
    ## 1406         Lexington                   Grand Central-42nd St         40.75178
    ## 1407         Lexington                   Grand Central-42nd St         40.75178
    ## 1408         Lexington                   Grand Central-42nd St         40.75178
    ## 1409         Lexington                               Spring St         40.72230
    ## 1410         Lexington                               Spring St         40.72230
    ## 1411         Lexington                               Spring St         40.72230
    ## 1412         Lexington                               Spring St         40.72230
    ## 1413         Lexington                                 Wall St         40.70756
    ## 1414         Lexington                                 Wall St         40.70756
    ## 1415         Lexington                                 Wall St         40.70756
    ## 1416         Lexington                                 Wall St         40.70756
    ## 1417         Lexington                                 Wall St         40.70756
    ## 1418         Lexington                                 Wall St         40.70756
    ## 1419         Lexington                                 Wall St         40.70756
    ## 1420         Lexington                                 Wall St         40.70756
    ## 1421           Liberty                      104th St-Oxford Av         40.68171
    ## 1422           Liberty                      104th St-Oxford Av         40.68171
    ## 1423           Liberty                   111th St-Greenwood Av         40.68433
    ## 1424           Liberty                   111th St-Greenwood Av         40.68433
    ## 1425           Liberty                   111th St-Greenwood Av         40.68433
    ## 1426           Liberty                   111th St-Greenwood Av         40.68433
    ## 1427           Liberty                       80th St-Hudson St         40.67937
    ## 1428           Liberty                       80th St-Hudson St         40.67937
    ## 1429           Liberty                       80th St-Hudson St         40.67937
    ## 1430           Liberty                       80th St-Hudson St         40.67937
    ## 1431           Liberty                         88th St-Boyd Av         40.67984
    ## 1432           Liberty                         88th St-Boyd Av         40.67984
    ## 1433           Liberty                                Grant Av         40.67704
    ## 1434           Liberty                           Lefferts Blvd         40.68595
    ## 1435           Liberty                           Lefferts Blvd         40.68595
    ## 1436           Liberty                           Lefferts Blvd         40.68595
    ## 1437           Liberty                           Lefferts Blvd         40.68595
    ## 1438           Liberty                           Rockaway Blvd         40.68043
    ## 1439           Liberty                           Rockaway Blvd         40.68043
    ## 1440           Liberty                           Rockaway Blvd         40.68043
    ## 1441           Liberty                           Rockaway Blvd         40.68043
    ## 1442            Myrtle                              Central Av         40.69786
    ## 1443            Myrtle                              Central Av         40.69786
    ## 1444            Myrtle                               Forest Av         40.70442
    ## 1445            Myrtle                               Forest Av         40.70442
    ## 1446            Myrtle                           Fresh Pond Rd         40.70619
    ## 1447            Myrtle                           Fresh Pond Rd         40.70619
    ## 1448            Myrtle                        Knickerbocker Av         40.69866
    ## 1449            Myrtle                        Knickerbocker Av         40.69866
    ## 1450            Myrtle                         Metropolitan Av         40.71140
    ## 1451            Myrtle                               Seneca Av         40.70276
    ## 1452            Myrtle                               Seneca Av         40.70276
    ## 1453            Nassau                                  Bowery         40.72028
    ## 1454            Nassau                                  Bowery         40.72028
    ## 1455            Nassau                                Broad St         40.70648
    ## 1456            Nassau                                Broad St         40.70648
    ## 1457            Nassau                                Broad St         40.70648
    ## 1458            Nassau                                Broad St         40.70648
    ## 1459            Nassau                                Broad St         40.70648
    ## 1460            Nassau                                Broad St         40.70648
    ## 1461            Nassau                                Broad St         40.70648
    ## 1462            Nassau                                Broad St         40.70648
    ## 1463            Nassau                                Broad St         40.70648
    ## 1464            Nassau                                Canal St         40.71809
    ## 1465            Nassau                             Chambers St         40.71324
    ## 1466            Nassau                             Chambers St         40.71419
    ## 1467            Nassau                                Essex St         40.71831
    ## 1468            Nassau                                Essex St         40.71831
    ## 1469            Nassau                               Fulton St         40.71037
    ## 1470            Nassau                               Fulton St         40.71037
    ## 1471            Nassau                               Fulton St         40.71037
    ## 1472            Nassau                               Fulton St         40.71037
    ## 1473            Nassau                               Fulton St         40.71037
    ## 1474            Nassau                               Fulton St         40.71037
    ## 1475          New Lots                               Junius St         40.66351
    ## 1476          New Lots                               Junius St         40.66351
    ## 1477          New Lots                             New Lots Av         40.66624
    ## 1478          New Lots                             New Lots Av         40.66624
    ## 1479          New Lots                         Pennsylvania Av         40.66463
    ## 1480          New Lots                         Pennsylvania Av         40.66463
    ## 1481          New Lots                             Rockaway Av         40.66255
    ## 1482          New Lots                             Rockaway Av         40.66255
    ## 1483          New Lots                             Saratoga Av         40.66145
    ## 1484          New Lots                             Saratoga Av         40.66145
    ## 1485          New Lots                             Saratoga Av         40.66145
    ## 1486          New Lots                               Sutter Av         40.66472
    ## 1487          New Lots                               Sutter Av         40.66472
    ## 1488          New Lots                               Sutter Av         40.66472
    ## 1489          New Lots                           Van Siclen Av         40.66545
    ## 1490          New Lots                           Van Siclen Av         40.66545
    ## 1491          Nostrand                              Beverly Rd         40.64510
    ## 1492          Nostrand                              Beverly Rd         40.64510
    ## 1493          Nostrand                               Church Av         40.65084
    ## 1494          Nostrand                               Church Av         40.65084
    ## 1495          Nostrand                               Church Av         40.65084
    ## 1496          Nostrand                               Church Av         40.65084
    ## 1497          Nostrand                               Church Av         40.65084
    ## 1498          Nostrand                               Church Av         40.65084
    ## 1499          Nostrand                               Church Av         40.65084
    ## 1500          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1501          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1502          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1503          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1504          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1505          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1506          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1507          Nostrand            Flatbush Av-Brooklyn College         40.63284
    ## 1508          Nostrand                              Newkirk Av         40.63997
    ## 1509          Nostrand                              Newkirk Av         40.63997
    ## 1510          Nostrand                              Newkirk Av         40.63997
    ## 1511          Nostrand                            President St         40.66788
    ## 1512          Nostrand                            President St         40.66788
    ## 1513          Nostrand                             Sterling St         40.66274
    ## 1514          Nostrand                             Sterling St         40.66274
    ## 1515          Nostrand                             Winthrop St         40.65665
    ## 1516          Nostrand                             Winthrop St         40.65665
    ## 1517            Pelham                        138th St-3rd Ave         40.81048
    ## 1518            Pelham                        138th St-3rd Ave         40.81048
    ## 1519            Pelham                        138th St-3rd Ave         40.81048
    ## 1520            Pelham                        138th St-3rd Ave         40.81048
    ## 1521            Pelham                        138th St-3rd Ave         40.81048
    ## 1522            Pelham                        138th St-3rd Ave         40.81048
    ## 1523            Pelham                        138th St-3rd Ave         40.81048
    ## 1524            Pelham                                Brook Av         40.80757
    ## 1525            Pelham                                Brook Av         40.80757
    ## 1526            Pelham                                Brook Av         40.80757
    ## 1527            Pelham                                Brook Av         40.80757
    ## 1528            Pelham                                Buhre Av         40.84681
    ## 1529            Pelham                                Buhre Av         40.84681
    ## 1530            Pelham                                Buhre Av         40.84681
    ## 1531            Pelham                          Castle Hill Av         40.83425
    ## 1532            Pelham                          Castle Hill Av         40.83425
    ## 1533            Pelham                              Cypress Av         40.80537
    ## 1534            Pelham                              Cypress Av         40.80537
    ## 1535            Pelham                              Cypress Av         40.80537
    ## 1536            Pelham                              Cypress Av         40.80537
    ## 1537            Pelham              East 143rd St-St Mary's St         40.80872
    ## 1538            Pelham              East 143rd St-St Mary's St         40.80872
    ## 1539            Pelham              East 143rd St-St Mary's St         40.80872
    ## 1540            Pelham              East 143rd St-St Mary's St         40.80872
    ## 1541            Pelham                           East 149th St         40.81212
    ## 1542            Pelham                           East 149th St         40.81212
    ## 1543            Pelham                           East 149th St         40.81212
    ## 1544            Pelham                           East 149th St         40.81212
    ## 1545            Pelham                                Elder Av         40.82858
    ## 1546            Pelham                                Elder Av         40.82858
    ## 1547            Pelham                          Hunts Point Av         40.82095
    ## 1548            Pelham                          Hunts Point Av         40.82095
    ## 1549            Pelham                          Hunts Point Av         40.82095
    ## 1550            Pelham                             Longwood Av         40.81610
    ## 1551            Pelham                             Longwood Av         40.81610
    ## 1552            Pelham                             Longwood Av         40.81610
    ## 1553            Pelham                             Longwood Av         40.81610
    ## 1554            Pelham                           Middletown Rd         40.84386
    ## 1555            Pelham                           Middletown Rd         40.84386
    ## 1556            Pelham                Morrison Av-Soundview Av         40.82952
    ## 1557            Pelham                Morrison Av-Soundview Av         40.82952
    ## 1558            Pelham                Morrison Av-Soundview Av         40.82952
    ## 1559            Pelham               Parkchester-East 177th St         40.83323
    ## 1560            Pelham                         Pelham Bay Park         40.85246
    ## 1561            Pelham                         Pelham Bay Park         40.85246
    ## 1562            Pelham                         Pelham Bay Park         40.85246
    ## 1563            Pelham                         Pelham Bay Park         40.85246
    ## 1564            Pelham                         Pelham Bay Park         40.85246
    ## 1565            Pelham                          St Lawrence Av         40.83151
    ## 1566            Pelham                          St Lawrence Av         40.83151
    ## 1567            Pelham      Westchester Square-East Tremont Av         40.83989
    ## 1568            Pelham                             Whitlock Av         40.82652
    ## 1569            Pelham                             Whitlock Av         40.82652
    ## 1570            Pelham                               Zerega Av         40.83649
    ## 1571            Pelham                               Zerega Av         40.83649
    ## 1572  Queens Boulevard                                169th St         40.71047
    ## 1573  Queens Boulevard                                169th St         40.71047
    ## 1574  Queens Boulevard                                169th St         40.71047
    ## 1575  Queens Boulevard                                169th St         40.71047
    ## 1576  Queens Boulevard                                169th St         40.71047
    ## 1577  Queens Boulevard                                169th St         40.71047
    ## 1578  Queens Boulevard                                169th St         40.71047
    ## 1579  Queens Boulevard                                169th St         40.71047
    ## 1580  Queens Boulevard                          23rd St-Ely Av         40.74785
    ## 1581  Queens Boulevard                          23rd St-Ely Av         40.74785
    ## 1582  Queens Boulevard                          23rd St-Ely Av         40.74785
    ## 1583  Queens Boulevard                          23rd St-Ely Av         40.74785
    ## 1584  Queens Boulevard                                 36th St         40.75204
    ## 1585  Queens Boulevard                                 36th St         40.75204
    ## 1586  Queens Boulevard                                 36th St         40.75204
    ## 1587  Queens Boulevard                                 36th St         40.75204
    ## 1588  Queens Boulevard                                 36th St         40.75204
    ## 1589  Queens Boulevard                                 46th St         40.75631
    ## 1590  Queens Boulevard                                 46th St         40.75631
    ## 1591  Queens Boulevard                                 46th St         40.75631
    ## 1592  Queens Boulevard                                 46th St         40.75631
    ## 1593  Queens Boulevard                          5th Av-53rd St         40.76017
    ## 1594  Queens Boulevard                          5th Av-53rd St         40.76017
    ## 1595  Queens Boulevard                          5th Av-53rd St         40.76017
    ## 1596  Queens Boulevard                          5th Av-53rd St         40.76017
    ## 1597  Queens Boulevard                          5th Av-53rd St         40.76017
    ## 1598  Queens Boulevard                    63rd Drive-Rego Park         40.72985
    ## 1599  Queens Boulevard                    63rd Drive-Rego Park         40.72985
    ## 1600  Queens Boulevard                    63rd Drive-Rego Park         40.72985
    ## 1601  Queens Boulevard                    63rd Drive-Rego Park         40.72985
    ## 1602  Queens Boulevard                    63rd Drive-Rego Park         40.72985
    ## 1603  Queens Boulevard                    63rd Drive-Rego Park         40.72985
    ## 1604  Queens Boulevard                    63rd Drive-Rego Park         40.72985
    ## 1605  Queens Boulevard                                 65th St         40.74967
    ## 1606  Queens Boulevard                                 65th St         40.74967
    ## 1607  Queens Boulevard                                 67th Av         40.72652
    ## 1608  Queens Boulevard                                 67th Av         40.72652
    ## 1609  Queens Boulevard                                 67th Av         40.72652
    ## 1610  Queens Boulevard                                 67th Av         40.72652
    ## 1611  Queens Boulevard                                 75th Av         40.71833
    ## 1612  Queens Boulevard                                 75th Av         40.71833
    ## 1613  Queens Boulevard                                 75th Av         40.71833
    ## 1614  Queens Boulevard                                  7th Av         40.76286
    ## 1615  Queens Boulevard                                  7th Av         40.76286
    ## 1616  Queens Boulevard                                  7th Av         40.76286
    ## 1617  Queens Boulevard                                  7th Av         40.76286
    ## 1618  Queens Boulevard                 Briarwood-Van Wyck Blvd         40.70918
    ## 1619  Queens Boulevard                 Briarwood-Van Wyck Blvd         40.70918
    ## 1620  Queens Boulevard                 Briarwood-Van Wyck Blvd         40.70918
    ## 1621  Queens Boulevard                             Elmhurst Av         40.74245
    ## 1622  Queens Boulevard                             Elmhurst Av         40.74245
    ## 1623  Queens Boulevard                             Elmhurst Av         40.74245
    ## 1624  Queens Boulevard                             Elmhurst Av         40.74245
    ## 1625  Queens Boulevard                             Elmhurst Av         40.74245
    ## 1626  Queens Boulevard                    Forest Hills-71st Av         40.72169
    ## 1627  Queens Boulevard                    Forest Hills-71st Av         40.72169
    ## 1628  Queens Boulevard                    Forest Hills-71st Av         40.72169
    ## 1629  Queens Boulevard                    Forest Hills-71st Av         40.72169
    ## 1630  Queens Boulevard                    Forest Hills-71st Av         40.72169
    ## 1631  Queens Boulevard                        Grand Av-Newtown         40.73701
    ## 1632  Queens Boulevard                        Grand Av-Newtown         40.73701
    ## 1633  Queens Boulevard                        Grand Av-Newtown         40.73701
    ## 1634  Queens Boulevard                        Grand Av-Newtown         40.73701
    ## 1635  Queens Boulevard                        Grand Av-Newtown         40.73701
    ## 1636  Queens Boulevard           Jackson Heights-Roosevelt Ave         40.74664
    ## 1637  Queens Boulevard           Jackson Heights-Roosevelt Ave         40.74664
    ## 1638  Queens Boulevard           Jackson Heights-Roosevelt Ave         40.74664
    ## 1639  Queens Boulevard           Jackson Heights-Roosevelt Ave         40.74664
    ## 1640  Queens Boulevard           Jackson Heights-Roosevelt Ave         40.74664
    ## 1641  Queens Boulevard           Jackson Heights-Roosevelt Ave         40.74664
    ## 1642  Queens Boulevard           Jackson Heights-Roosevelt Ave         40.74664
    ## 1643  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1644  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1645  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1646  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1647  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1648  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1649  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1650  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1651  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1652  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1653  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1654  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1655  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1656  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1657  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1658  Queens Boulevard                        Jamaica-179th St         40.71265
    ## 1659  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1660  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1661  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1662  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1663  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1664  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1665  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1666  Queens Boulevard              Kew Gardens-Union Turnpike         40.71444
    ## 1667  Queens Boulevard                    Lexington Av-53rd St         40.75755
    ## 1668  Queens Boulevard                    Lexington Av-53rd St         40.75755
    ## 1669  Queens Boulevard                    Lexington Av-53rd St         40.75755
    ## 1670  Queens Boulevard                    Lexington Av-53rd St         40.75755
    ## 1671  Queens Boulevard                    Lexington Av-53rd St         40.75755
    ## 1672  Queens Boulevard                    Lexington Av-53rd St         40.75755
    ## 1673  Queens Boulevard                    Lexington Av-53rd St         40.75755
    ## 1674  Queens Boulevard                           Northern Blvd         40.75288
    ## 1675  Queens Boulevard                           Northern Blvd         40.75288
    ## 1676  Queens Boulevard                            Parsons Blvd         40.70756
    ## 1677  Queens Boulevard                            Parsons Blvd         40.70756
    ## 1678  Queens Boulevard                            Parsons Blvd         40.70756
    ## 1679  Queens Boulevard                            Parsons Blvd         40.70756
    ## 1680  Queens Boulevard                            Parsons Blvd         40.70756
    ## 1681  Queens Boulevard                            Parsons Blvd         40.70756
    ## 1682  Queens Boulevard                            Queens Plaza         40.74897
    ## 1683  Queens Boulevard                            Queens Plaza         40.74897
    ## 1684  Queens Boulevard                            Queens Plaza         40.74897
    ## 1685  Queens Boulevard                            Queens Plaza         40.74897
    ## 1686  Queens Boulevard                            Queens Plaza         40.74897
    ## 1687  Queens Boulevard                            Queens Plaza         40.74897
    ## 1688  Queens Boulevard                            Queens Plaza         40.74897
    ## 1689  Queens Boulevard                             Steinway St         40.75688
    ## 1690  Queens Boulevard                             Steinway St         40.75688
    ## 1691  Queens Boulevard                             Steinway St         40.75688
    ## 1692  Queens Boulevard                             Steinway St         40.75688
    ## 1693  Queens Boulevard                            Sutphin Blvd         40.70546
    ## 1694  Queens Boulevard                            Sutphin Blvd         40.70546
    ## 1695  Queens Boulevard                            Sutphin Blvd         40.70546
    ## 1696  Queens Boulevard                            Sutphin Blvd         40.70546
    ## 1697  Queens Boulevard                            Sutphin Blvd         40.70546
    ## 1698  Queens Boulevard                          Woodhaven Blvd         40.73311
    ## 1699  Queens Boulevard                          Woodhaven Blvd         40.73311
    ## 1700  Queens Boulevard                          Woodhaven Blvd         40.73311
    ## 1701  Queens Boulevard                          Woodhaven Blvd         40.73311
    ## 1702          Rockaway                      Aqueduct Racetrack         40.67213
    ## 1703          Rockaway               Aqueduct-North Conduit Av         40.66823
    ## 1704          Rockaway               Aqueduct-North Conduit Av         40.66823
    ## 1705          Rockaway                          Beach 105th St         40.58321
    ## 1706          Rockaway                          Beach 105th St         40.58321
    ## 1707          Rockaway                          Beach 105th St         40.58321
    ## 1708          Rockaway                          Beach 105th St         40.58321
    ## 1709          Rockaway                           Beach 25th St         40.60007
    ## 1710          Rockaway                           Beach 25th St         40.60007
    ## 1711          Rockaway                           Beach 25th St         40.60007
    ## 1712          Rockaway                           Beach 36th St         40.59540
    ## 1713          Rockaway                           Beach 36th St         40.59540
    ## 1714          Rockaway                           Beach 36th St         40.59540
    ## 1715          Rockaway                           Beach 36th St         40.59540
    ## 1716          Rockaway                           Beach 44th St         40.59294
    ## 1717          Rockaway                           Beach 44th St         40.59294
    ## 1718          Rockaway                           Beach 44th St         40.59294
    ## 1719          Rockaway                           Beach 44th St         40.59294
    ## 1720          Rockaway                           Beach 60th St         40.59237
    ## 1721          Rockaway                           Beach 60th St         40.59237
    ## 1722          Rockaway                           Beach 60th St         40.59237
    ## 1723          Rockaway                           Beach 60th St         40.59237
    ## 1724          Rockaway                           Beach 67th St         40.59093
    ## 1725          Rockaway                           Beach 67th St         40.59093
    ## 1726          Rockaway                           Beach 67th St         40.59093
    ## 1727          Rockaway                           Beach 67th St         40.59093
    ## 1728          Rockaway                           Beach 90th St         40.58803
    ## 1729          Rockaway                           Beach 90th St         40.58803
    ## 1730          Rockaway                           Beach 90th St         40.58803
    ## 1731          Rockaway                           Beach 90th St         40.58803
    ## 1732          Rockaway                           Beach 98th St         40.58531
    ## 1733          Rockaway                           Beach 98th St         40.58531
    ## 1734          Rockaway                           Beach 98th St         40.58531
    ## 1735          Rockaway                           Broad Channel         40.60838
    ## 1736          Rockaway                    Far Rockaway-Mott Av         40.60399
    ## 1737          Rockaway                            Howard Beach         40.66048
    ## 1738          Rockaway                            Howard Beach         40.66048
    ## 1739          Rockaway                            Howard Beach         40.66048
    ## 1740          Rockaway                            Howard Beach         40.66048
    ## 1741          Rockaway               Rockaway Park-Beach 116th         40.58090
    ## 1742         Sea Beach                                 18th Av         40.62067
    ## 1743         Sea Beach                                 18th Av         40.62067
    ## 1744         Sea Beach                                 20th Av         40.61741
    ## 1745         Sea Beach                                 86th St         40.59272
    ## 1746         Sea Beach                                  8th Av         40.63506
    ## 1747         Sea Beach                                    Av U         40.59747
    ## 1748         Sea Beach                                    Av U         40.59747
    ## 1749         Sea Beach                     Bay Parkway-22nd Av         40.61181
    ## 1750         Sea Beach                     Bay Parkway-22nd Av         40.61181
    ## 1751         Sea Beach                   Fort Hamilton Parkway         40.63139
    ## 1752         Sea Beach                   Fort Hamilton Parkway         40.63139
    ## 1753         Sea Beach                           Kings Highway         40.60392
    ## 1754         Sea Beach                           Kings Highway         40.60392
    ## 1755         Sea Beach                          New Utrecht Av         40.62484
    ## 1756         Sea Beach                          New Utrecht Av         40.62484
    ## 1757          West End                                 18th Av         40.60795
    ## 1758          West End                                 18th Av         40.60795
    ## 1759          West End                                 18th Av         40.60795
    ## 1760          West End                                 20th Av         40.60456
    ## 1761          West End                                 20th Av         40.60456
    ## 1762          West End                                 20th Av         40.60456
    ## 1763          West End                                 20th Av         40.60456
    ## 1764          West End                                 25th Av         40.59770
    ## 1765          West End                                 25th Av         40.59770
    ## 1766          West End                                 25th Av         40.59770
    ## 1767          West End                                 25th Av         40.59770
    ## 1768          West End                                 50th St         40.63626
    ## 1769          West End                                 50th St         40.63626
    ## 1770          West End                                 50th St         40.63626
    ## 1771          West End                                 50th St         40.63626
    ## 1772          West End                                 55th St         40.63144
    ## 1773          West End                                 55th St         40.63144
    ## 1774          West End                                 55th St         40.63144
    ## 1775          West End                                 62nd St         40.62647
    ## 1776          West End                                 62nd St         40.62647
    ## 1777          West End                                 62nd St         40.62647
    ## 1778          West End                                 71st St         40.61959
    ## 1779          West End                                 71st St         40.61959
    ## 1780          West End                                 71st St         40.61959
    ## 1781          West End                                 71st St         40.61959
    ## 1782          West End                                 71st St         40.61959
    ## 1783          West End                                 71st St         40.61959
    ## 1784          West End                                 79th St         40.61350
    ## 1785          West End                                 79th St         40.61350
    ## 1786          West End                                 79th St         40.61350
    ## 1787          West End                                 79th St         40.61350
    ## 1788          West End                                  9th Av         40.64629
    ## 1789          West End                             Bay 50th St         40.58884
    ## 1790          West End                             Bay 50th St         40.58884
    ## 1791          West End                             Bay 50th St         40.58884
    ## 1792          West End                             Bay 50th St         40.58884
    ## 1793          West End                             Bay Parkway         40.60187
    ## 1794          West End                             Bay Parkway         40.60187
    ## 1795          West End                             Bay Parkway         40.60187
    ## 1796          West End                             Bay Parkway         40.60187
    ## 1797          West End                   Fort Hamilton Parkway         40.64091
    ## 1798          West End                   Fort Hamilton Parkway         40.64091
    ## 1799          West End                   Fort Hamilton Parkway         40.64091
    ## 1800          West End                   Fort Hamilton Parkway         40.64091
    ## 1801 White Plains Road                         149th St-3rd Av         40.81611
    ## 1802 White Plains Road                         149th St-3rd Av         40.81611
    ## 1803 White Plains Road                         149th St-3rd Av         40.81611
    ## 1804 White Plains Road                         149th St-3rd Av         40.81611
    ## 1805 White Plains Road                         149th St-3rd Av         40.81611
    ## 1806 White Plains Road                         149th St-3rd Av         40.81611
    ## 1807 White Plains Road                         149th St-3rd Av         40.81611
    ## 1808 White Plains Road                         149th St-3rd Av         40.81611
    ## 1809 White Plains Road                                174th St         40.83729
    ## 1810 White Plains Road                                174th St         40.83729
    ## 1811 White Plains Road                                174th St         40.83729
    ## 1812 White Plains Road                                174th St         40.83729
    ## 1813 White Plains Road                                219th St         40.88390
    ## 1814 White Plains Road                                219th St         40.88390
    ## 1815 White Plains Road                                225th St         40.88802
    ## 1816 White Plains Road                                225th St         40.88802
    ## 1817 White Plains Road                                225th St         40.88802
    ## 1818 White Plains Road                                233rd St         40.89319
    ## 1819 White Plains Road                                233rd St         40.89319
    ## 1820 White Plains Road                      238th St-Nereid Av         40.89838
    ## 1821 White Plains Road                      238th St-Nereid Av         40.89838
    ## 1822 White Plains Road                             Allerton Av         40.86546
    ## 1823 White Plains Road                             Allerton Av         40.86546
    ## 1824 White Plains Road                         Bronx Park East         40.84883
    ## 1825 White Plains Road                         Bronx Park East         40.84883
    ## 1826 White Plains Road                         Bronx Park East         40.84883
    ## 1827 White Plains Road                                Burke Av         40.87136
    ## 1828 White Plains Road                                Burke Av         40.87136
    ## 1829 White Plains Road                           East 180th St         40.84189
    ## 1830 White Plains Road                           East 180th St         40.84189
    ## 1831 White Plains Road           East Tremont Av-West Farms Sq         40.84029
    ## 1832 White Plains Road           East Tremont Av-West Farms Sq         40.84029
    ## 1833 White Plains Road           East Tremont Av-West Farms Sq         40.84029
    ## 1834 White Plains Road           East Tremont Av-West Farms Sq         40.84029
    ## 1835 White Plains Road           East Tremont Av-West Farms Sq         40.84029
    ## 1836 White Plains Road                              Freeman St         40.82999
    ## 1837 White Plains Road                              Freeman St         40.82999
    ## 1838 White Plains Road                              Freeman St         40.82999
    ## 1839 White Plains Road                              Freeman St         40.82999
    ## 1840 White Plains Road                             Gun Hill Rd         40.87785
    ## 1841 White Plains Road                             Gun Hill Rd         40.87785
    ## 1842 White Plains Road                            Intervale Av         40.82218
    ## 1843 White Plains Road                            Intervale Av         40.82218
    ## 1844 White Plains Road                            Intervale Av         40.82218
    ## 1845 White Plains Road                              Jackson Av         40.81649
    ## 1846 White Plains Road                              Jackson Av         40.81649
    ## 1847 White Plains Road                              Jackson Av         40.81649
    ## 1848 White Plains Road                              Jackson Av         40.81649
    ## 1849 White Plains Road                          Pelham Parkway         40.85719
    ## 1850 White Plains Road                          Pelham Parkway         40.85719
    ## 1851 White Plains Road                          Pelham Parkway         40.85719
    ## 1852 White Plains Road                          Pelham Parkway         40.85719
    ## 1853 White Plains Road                          Pelham Parkway         40.85719
    ## 1854 White Plains Road                             Prospect Av         40.81958
    ## 1855 White Plains Road                             Prospect Av         40.81958
    ## 1856 White Plains Road                             Prospect Av         40.81958
    ## 1857 White Plains Road                             Prospect Av         40.81958
    ## 1858 White Plains Road                             Prospect Av         40.81958
    ## 1859 White Plains Road                              Simpson St         40.82407
    ## 1860 White Plains Road                              Simpson St         40.82407
    ## 1861 White Plains Road                              Simpson St         40.82407
    ## 1862 White Plains Road                              Simpson St         40.82407
    ## 1863 White Plains Road                              Simpson St         40.82407
    ## 1864 White Plains Road                      Wakefield-241st St         40.90313
    ## 1865 White Plains Road                      Wakefield-241st St         40.90313
    ## 1866 White Plains Road                      Wakefield-241st St         40.90313
    ## 1867          Flushing                      34 St Hudson Yards         40.75588
    ## 1868          Flushing                      34 St Hudson Yards         40.75588
    ##      Station.Longitude Route1 Route2 Route3 Route4 Route5 Route6 Route7 Route8
    ## 1            -73.99809      R                                               NA
    ## 2            -73.99809      R                                               NA
    ## 3            -74.00355      N      R                                        NA
    ## 4            -74.00355      N      R                                        NA
    ## 5            -74.00355      N      R                                        NA
    ## 6            -74.01001      R                                               NA
    ## 7            -74.01001      R                                               NA
    ## 8            -74.01001      R                                               NA
    ## 9            -74.01001      R                                               NA
    ## 10           -74.01403      R                                               NA
    ## 11           -74.01403      R                                               NA
    ## 12           -74.01403      R                                               NA
    ## 13           -74.01403      R                                               NA
    ## 14           -74.01403      R                                               NA
    ## 15           -74.01788      N      R                                        NA
    ## 16           -74.01788      N      R                                        NA
    ## 17           -74.01788      N      R                                        NA
    ## 18           -74.01788      N      R                                        NA
    ## 19           -74.01788      N      R                                        NA
    ## 20           -74.01788      N      R                                        NA
    ## 21           -74.02551      R                                               NA
    ## 22           -74.02551      R                                               NA
    ## 23           -74.02551      R                                               NA
    ## 24           -74.02840      R                                               NA
    ## 25           -74.02840      R                                               NA
    ## 26           -74.02840      R                                               NA
    ## 27           -74.03088      R                                               NA
    ## 28           -74.03088      R                                               NA
    ## 29           -74.03088      R                                               NA
    ## 30           -74.03088      R                                               NA
    ## 31           -74.03088      R                                               NA
    ## 32           -73.98830      F      G      R                                 NA
    ## 33           -73.98830      F      G      R                                 NA
    ## 34           -73.97881      B      Q      D      N      R      2      3      4
    ## 35           -74.02338      R                                               NA
    ## 36           -74.02338      R                                               NA
    ## 37           -74.02338      R                                               NA
    ## 38           -73.98182      B      Q      R                                 NA
    ## 39           -73.98182      B      Q      R                                 NA
    ## 40           -73.98182      B      Q      R                                 NA
    ## 41           -73.98182      B      Q      R                                 NA
    ## 42           -73.98182      B      Q      R                                 NA
    ## 43           -73.98182      B      Q      R                                 NA
    ## 44           -73.97881      B      Q      D      N      R      2      3      4
    ## 45           -73.97881      B      Q      D      N      R      2      3      4
    ## 46           -73.99287      R                                               NA
    ## 47           -73.99287      R                                               NA
    ## 48           -73.99287      R                                               NA
    ## 49           -73.98311      R                                               NA
    ## 50           -73.98311      R                                               NA
    ## 51           -73.98311      R                                               NA
    ## 52           -73.98311      R                                               NA
    ## 53           -73.97919     GS      4      5      6      7                   NA
    ## 54           -73.97919     GS      4      5      6      7                   NA
    ## 55           -73.97919     GS      4      5      6      7                   NA
    ## 56           -73.97919     GS      4      5      6      7                   NA
    ## 57           -73.97919     GS      4      5      6      7                   NA
    ## 58           -73.97919     GS      4      5      6      7                   NA
    ## 59           -73.97919     GS      4      5      6      7                   NA
    ## 60           -73.98623      A      C      E      N      Q      R     GS      1
    ## 61           -73.99621      F      L      M      1      2      3            NA
    ## 62           -73.99621      F      L      M      1      2      3            NA
    ## 63           -73.99621      F      L      M      1      2      3            NA
    ## 64           -73.99621      F      L      M      1      2      3            NA
    ## 65           -73.99621      F      L      M      1      2      3            NA
    ## 66           -73.99621      F      L      M      1      2      3            NA
    ## 67           -73.99621      F      L      M      1      2      3            NA
    ## 68           -73.99621      F      L      M      1      2      3            NA
    ## 69           -73.99621      F      L      M      1      2      3            NA
    ## 70           -73.99621      F      L      M      1      2      3            NA
    ## 71           -73.99282      F      M                                        NA
    ## 72           -73.99282      F      M                                        NA
    ## 73           -73.99282      F      M                                        NA
    ## 74           -73.99282      F      M                                        NA
    ## 75           -73.99282      F      M                                        NA
    ## 76           -73.99282      F      M                                        NA
    ## 77           -73.99282      F      M                                        NA
    ## 78           -73.99282      F      M                                        NA
    ## 79           -73.98994      F                                               NA
    ## 80           -73.98994      F                                               NA
    ## 81           -73.98994      F                                               NA
    ## 82           -73.98994      F                                               NA
    ## 83           -73.98782      B      D      F      M      N      Q      R     NA
    ## 84           -73.98782      B      D      F      M      N      Q      R     NA
    ## 85           -73.98782      B      D      F      M      N      Q      R     NA
    ## 86           -73.98782      B      D      F      M      N      Q      R     NA
    ## 87           -73.98782      B      D      F      M      N      Q      R     NA
    ## 88           -73.98782      B      D      F      M      N      Q      R     NA
    ## 89           -73.98782      B      D      F      M      N      Q      R     NA
    ## 90           -73.98782      B      D      F      M      N      Q      R     NA
    ## 91           -73.98782      B      D      F      M      N      Q      R     NA
    ## 92           -73.98782      B      D      F      M      N      Q      R     NA
    ## 93           -73.98457      B      D      F      M      7                   NA
    ## 94           -73.98457      B      D      F      M      7                   NA
    ## 95           -73.98457      B      D      F      M      7                   NA
    ## 96           -73.98457      B      D      F      M      7                   NA
    ## 97           -73.98457      B      D      F      M      7                   NA
    ## 98           -73.98457      B      D      F      M      7                   NA
    ## 99           -73.98457      B      D      F      M      7                   NA
    ## 100          -73.98457      B      D      F      M      7                   NA
    ## 101          -73.98457      B      D      F      M      7                   NA
    ## 102          -73.98133      B      D      F      M                          NA
    ## 103          -73.98133      B      D      F      M                          NA
    ## 104          -73.98133      B      D      F      M                          NA
    ## 105          -73.98133      B      D      F      M                          NA
    ## 106          -73.98133      B      D      F      M                          NA
    ## 107          -73.98133      B      D      F      M                          NA
    ## 108          -73.98133      B      D      F      M                          NA
    ## 109          -73.98133      B      D      F      M      7                   NA
    ## 110          -73.98133      B      D      F      M                          NA
    ## 111          -73.98133      B      D      F      M                          NA
    ## 112          -73.98133      B      D      F      M                          NA
    ## 113          -73.98133      B      D      F      M                          NA
    ## 114          -73.98133      B      D      F      M                          NA
    ## 115          -73.98133      B      D      F      M                          NA
    ## 116          -73.98133      B      D      F      M                          NA
    ## 117          -73.98133      B      D      F      M                          NA
    ## 118          -73.98133      B      D      F      M                          NA
    ## 119          -73.98978      F                                               NA
    ## 120          -73.98978      F                                               NA
    ## 121          -73.97745      F                                               NA
    ## 122          -73.97745      F                                               NA
    ## 123          -73.97745      F                                               NA
    ## 124          -73.97745      F                                               NA
    ## 125          -73.97745      F                                               NA
    ## 126          -73.97745      F                                               NA
    ## 127          -73.97745      F                                               NA
    ## 128          -73.97745      F                                               NA
    ## 129          -73.98031      F                                               NA
    ## 130          -73.98031      F                                               NA
    ## 131          -73.98031      F                                               NA
    ## 132          -73.98031      F                                               NA
    ## 133          -73.98031      F                                               NA
    ## 134          -73.98031      F                                               NA
    ## 135          -73.98031      F                                               NA
    ## 136          -73.98031      F                                               NA
    ## 137          -73.99086      F      G                                        NA
    ## 138          -73.99086      F      G                                        NA
    ## 139          -73.99086      F      G                                        NA
    ## 140          -73.99086      F      G                                        NA
    ## 141          -73.99086      F      G                                        NA
    ## 142          -73.99086      F      G                                        NA
    ## 143          -73.99620      B      D      F      M      6                   NA
    ## 144          -73.99620      B      D      F      M      6                   NA
    ## 145          -73.99620      B      D      F      M      6                   NA
    ## 146          -73.99620      B      D      F      M      6                   NA
    ## 147          -73.99620      B      D      F      M      6                   NA
    ## 148          -73.99505      F      G                                        NA
    ## 149          -73.99505      F      G                                        NA
    ## 150          -73.99505      F      G                                        NA
    ## 151          -73.99505      F      G                                        NA
    ## 152          -73.99505      F      G                                        NA
    ## 153          -73.97968      F                                               NA
    ## 154          -73.97968      F                                               NA
    ## 155          -73.97968      F                                               NA
    ## 156          -73.97968      F                                               NA
    ## 157          -73.97968      F                                               NA
    ## 158          -73.97968      F                                               NA
    ## 159          -73.98811      F      J      M      Z                          NA
    ## 160          -73.98811      F      J      M      Z                          NA
    ## 161          -73.98811      F      J      M      Z                          NA
    ## 162          -73.98811      F      J      M      Z                          NA
    ## 163          -73.99017      F                                               NA
    ## 164          -73.99017      F                                               NA
    ## 165          -73.99017      F                                               NA
    ## 166          -73.99017      F                                               NA
    ## 167          -73.97578      F                                               NA
    ## 168          -73.97578      F                                               NA
    ## 169          -73.97578      F                                               NA
    ## 170          -73.97578      F                                               NA
    ## 171          -73.99375      B      D                                        NA
    ## 172          -73.99375      B      D                                        NA
    ## 173          -73.99375      B      D                                        NA
    ## 174          -73.97949      F                                               NA
    ## 175          -73.97949      F                                               NA
    ## 176          -73.97949      F                                               NA
    ## 177          -73.97949      F                                               NA
    ## 178          -73.97949      F                                               NA
    ## 179          -73.97949      F                                               NA
    ## 180          -73.99596      F      G      R                                 NA
    ## 181          -73.98689      F                                               NA
    ## 182          -73.94284      F                                               NA
    ## 183          -73.94284      F                                               NA
    ## 184          -73.94284      F                                               NA
    ## 185          -73.94284      F                                               NA
    ## 186          -73.94284      F                                               NA
    ## 187          -73.96611      F                                               NA
    ## 188          -73.96611      F                                               NA
    ## 189          -73.96611      F                                               NA
    ## 190          -73.96611      F                                               NA
    ## 191          -73.96611      F                                               NA
    ## 192          -73.95326      F                                               NA
    ## 193          -73.96145      B      C                                        NA
    ## 194          -73.95488      B      C                                        NA
    ## 195          -73.95488      B      C                                        NA
    ## 196          -73.95488      B      C                                        NA
    ## 197          -73.95488      B      C                                        NA
    ## 198          -73.95234      A      B      C      D                          NA
    ## 199          -73.95234      A      B      C      D                          NA
    ## 200          -73.95234      A      B      C      D                          NA
    ## 201          -73.95234      A      B      C      D                          NA
    ## 202          -73.95234      A      B      C      D                          NA
    ## 203          -73.95234      A      B      C      D                          NA
    ## 204          -73.94765      B      C                                        NA
    ## 205          -73.94765      B      C                                        NA
    ## 206          -73.94765      B      C                                        NA
    ## 207          -73.94765      B      C                                        NA
    ## 208          -73.94765      B      C                                        NA
    ## 209          -73.94765      B      C                                        NA
    ## 210          -73.94422      A      B      C      D                          NA
    ## 211          -73.94422      A      B      C      D                          NA
    ## 212          -73.94422      A      B      C      D                          NA
    ## 213          -73.94422      A      B      C      D                          NA
    ## 214          -73.94422      A      B      C      D                          NA
    ## 215          -73.94422      A      B      C      D                          NA
    ## 216          -74.00169      A      C      E      L                          NA
    ## 217          -74.00169      A      C      E      L                          NA
    ## 218          -74.00169      A      C      E      L                          NA
    ## 219          -74.00169      A      C      E      L                          NA
    ## 220          -74.00169      A      C      E      L                          NA
    ## 221          -74.00169      A      C      E      L                          NA
    ## 222          -74.00169      A      C      E      L                          NA
    ## 223          -74.00169      A      C      E      L                          NA
    ## 224          -73.94151      C                                               NA
    ## 225          -73.94151      C                                               NA
    ## 226          -73.94151      C                                               NA
    ## 227          -73.94151      C                                               NA
    ## 228          -73.94151      C                                               NA
    ## 229          -73.94151      C                                               NA
    ## 230          -73.93989      C                                               NA
    ## 231          -73.93989      C                                               NA
    ## 232          -73.93989      C                                               NA
    ## 233          -73.93956      A      C                                        NA
    ## 234          -73.93956      A      C                                        NA
    ## 235          -73.93956      A      C                                        NA
    ## 236          -73.93956      A      C                                        NA
    ## 237          -73.93970      A                                               NA
    ## 238          -73.93970      A                                               NA
    ## 239          -73.93970      A                                               NA
    ## 240          -73.93970      A                                               NA
    ## 241          -73.93970      A                                               NA
    ## 242          -73.93970      A                                               NA
    ## 243          -73.93970      A                                               NA
    ## 244          -73.93797      A                                               NA
    ## 245          -73.93797      A                                               NA
    ## 246          -73.93797      A                                               NA
    ## 247          -73.93797      A                                               NA
    ## 248          -73.93797      A                                               NA
    ## 249          -73.93418      A                                               NA
    ## 250          -73.93418      A                                               NA
    ## 251          -73.99804      C      E                                        NA
    ## 252          -73.99804      C      E                                        NA
    ## 253          -73.99804      C      E                                        NA
    ## 254          -73.99804      C      E                                        NA
    ## 255          -73.99804      C      E                                        NA
    ## 256          -73.99804      C      E                                        NA
    ## 257          -73.99804      C      E                                        NA
    ## 258          -73.99804      C      E                                        NA
    ## 259          -73.99804      C      E                                        NA
    ## 260          -73.99804      C      E                                        NA
    ## 261          -73.99804      C      E                                        NA
    ## 262          -73.99339      A      C      E                                 NA
    ## 263          -73.99339      A      C      E                                 NA
    ## 264          -73.99339      A      C      E                                 NA
    ## 265          -73.99339      A      C      E                                 NA
    ## 266          -73.99339      A      C      E                                 NA
    ## 267          -73.99339      A      C      E                                 NA
    ## 268          -73.99339      A      C      E                                 NA
    ## 269          -73.99339      A      C      E                                 NA
    ## 270          -73.99339      A      C      E                                 NA
    ## 271          -73.99339      A      C      E                                 NA
    ## 272          -73.99339      A      C      E                                 NA
    ## 273          -73.99339      A      C      E                                 NA
    ## 274          -73.99339      A      C      E                                 NA
    ## 275          -73.99339      A      C      E                                 NA
    ## 276          -73.99339      A      C      E                                 NA
    ## 277          -73.99339      A      C      E                                 NA
    ## 278          -73.98973      A      C      E      N      Q      R      S      1
    ## 279          -73.98973      A      C      E      N      Q      R      S      1
    ## 280          -73.98973      A      C      E      N      Q      R      S      1
    ## 281          -73.98973      A      C      E      N      Q      R      S      1
    ## 282          -73.98973      A      C      E      N      Q      R      S      1
    ## 283          -73.98973      A      C      E      N      Q      R      S      1
    ## 284          -73.98973      A      C      E      N      Q      R      S      1
    ## 285          -73.98973      A      C      E      N      Q      R      S      1
    ## 286          -73.98973      A      C      E      N      Q      R      S      1
    ## 287          -73.98598      C      E                                        NA
    ## 288          -73.98598      C      E                                        NA
    ## 289          -73.98598      C      E                                        NA
    ## 290          -73.98598      C      E                                        NA
    ## 291          -73.98598      C      E                                        NA
    ## 292          -73.98598      C      E                                        NA
    ## 293          -73.98598      C      E                                        NA
    ## 294          -73.98598      C      E                                        NA
    ## 295          -73.98598      C      E                                        NA
    ## 296          -73.98598      C      E                                        NA
    ## 297          -73.98174      A      B      C      D      1                   NA
    ## 298          -73.98174      A      B      C      D      1                   NA
    ## 299          -73.98174      A      B      C      D      1                   NA
    ## 300          -73.98174      A      B      C      D      1                   NA
    ## 301          -73.98174      A      B      C      D      1                   NA
    ## 302          -73.98174      A      B      C      D      1                   NA
    ## 303          -73.98174      A      B      C      D      1                   NA
    ## 304          -73.98174      A      B      C      D      1                   NA
    ## 305          -73.98174      A      B      C      D      1                   NA
    ## 306          -73.98174      A      B      C      D      1                   NA
    ## 307          -73.98174      A      B      C      D      1                   NA
    ## 308          -73.98174      A      B      C      D      1                   NA
    ## 309          -73.97641      B      C                                        NA
    ## 310          -73.97641      B      C                                        NA
    ## 311          -73.97641      B      C                                        NA
    ## 312          -73.97214      B      C                                        NA
    ## 313          -73.97214      B      C                                        NA
    ## 314          -73.97214      B      C                                        NA
    ## 315          -73.97214      B      C                                        NA
    ## 316          -73.96892      B      C                                        NA
    ## 317          -73.96892      B      C                                        NA
    ## 318          -73.96892      B      C                                        NA
    ## 319          -73.96892      B      C                                        NA
    ## 320          -73.96892      B      C                                        NA
    ## 321          -73.96470      B      C                                        NA
    ## 322          -73.96470      B      C                                        NA
    ## 323          -73.96470      B      C                                        NA
    ## 324          -74.00769      A      C      J      Z      2      3      4      5
    ## 325          -74.00769      A      C      J      Z      2      3      4      5
    ## 326          -74.00523      A      C      E                                 NA
    ## 327          -74.00523      A      C      E                                 NA
    ## 328          -74.00523      A      C      E                                 NA
    ## 329          -74.00523      A      C      E                                 NA
    ## 330          -74.00523      A      C      E                                 NA
    ## 331          -73.95816      B      C                                        NA
    ## 332          -73.95816      B      C                                        NA
    ## 333          -73.95816      B      C                                        NA
    ## 334          -74.00858      A      C      E      2      3                   NA
    ## 335          -74.00858      A      C      E      2      3                   NA
    ## 336          -74.00858      A      C      E      2      3                   NA
    ## 337          -74.00858      A      C      E      2      3                   NA
    ## 338          -74.00858      A      C      E      2      3                   NA
    ## 339          -74.00858      A      C      E      2      3                   NA
    ## 340          -74.00858      A      C      E      2      3                   NA
    ## 341          -74.00858      A      C      E      2      3                   NA
    ## 342          -74.00858      A      C      E      2      3                   NA
    ## 343          -73.92727      A                                               NA
    ## 344          -73.92727      A                                               NA
    ## 345          -73.92727      A                                               NA
    ## 346          -73.92727      A                                               NA
    ## 347          -73.92727      A                                               NA
    ## 348          -73.92727      A                                               NA
    ## 349          -73.92727      A                                               NA
    ## 350          -73.99053      A      C      J      Z      2      3      4      5
    ## 351          -73.99053      A      C      J      Z      2      3      4      5
    ## 352          -73.99053      A      C      J      Z      2      3      4      5
    ## 353          -73.91990      A                                               NA
    ## 354          -73.91990      A                                               NA
    ## 355          -73.91990      A                                               NA
    ## 356          -73.91990      A                                               NA
    ## 357          -73.91990      A                                               NA
    ## 358          -73.91990      A                                               NA
    ## 359          -74.00374      C      E                                        NA
    ## 360          -74.00374      C      E                                        NA
    ## 361          -74.00374      C      E                                        NA
    ## 362          -74.00050      A      B      C      D      E      F      M     NA
    ## 363          -74.00050      A      B      C      D      E      F      M     NA
    ## 364          -74.00050      A      B      C      D      E      F      M     NA
    ## 365          -74.00050      A      B      C      D      E      F      M     NA
    ## 366          -74.00050      A      B      C      D      E      F      M     NA
    ## 367          -74.00050      A      B      C      D      E      F      M     NA
    ## 368          -74.00978      A      C      E      2      3                   NA
    ## 369          -74.00978      A      C      E      2      3                   NA
    ## 370          -74.00978      A      C      E      2      3                   NA
    ## 371          -74.00978      A      C      E      2      3                   NA
    ## 372          -74.00978      A      C      E      2      3                   NA
    ## 373          -74.00978      A      C      E      2      3                   NA
    ## 374          -74.00978      A      C      E      2      3                   NA
    ## 375          -74.00978      A      C      E      2      3                   NA
    ## 376          -73.81686      E                                               NA
    ## 377          -73.81686      E                                               NA
    ## 378          -73.81686      E                                               NA
    ## 379          -73.80111      E      J      Z                                 NA
    ## 380          -73.80111      E      J      Z                                 NA
    ## 381          -73.80111      E      J      Z                                 NA
    ## 382          -73.80111      E      J      Z                                 NA
    ## 383          -73.80111      E      J      Z                                 NA
    ## 384          -73.80111      E      J      Z                                 NA
    ## 385          -73.80111      E      J      Z                                 NA
    ## 386          -73.80111      E      J      Z                                 NA
    ## 387          -73.80111      E      J      Z                                 NA
    ## 388          -73.80111      E      J      Z                                 NA
    ## 389          -73.80797      E      J      Z                                 NA
    ## 390          -73.80797      E      J      Z                                 NA
    ## 391          -73.80797      E      J      Z                                 NA
    ## 392          -73.80797      E      J      Z                                 NA
    ## 393          -73.92148      N      Q                                        NA
    ## 394          -73.92148      N      Q                                        NA
    ## 395          -73.92148      N      Q                                        NA
    ## 396          -73.92148      N      Q                                        NA
    ## 397          -73.92957      N      Q                                        NA
    ## 398          -73.92957      N      Q                                        NA
    ## 399          -73.92957      N      Q                                        NA
    ## 400          -73.93276      N      Q                                        NA
    ## 401          -73.93276      N      Q                                        NA
    ## 402          -73.91784      N      Q                                        NA
    ## 403          -73.91784      N      Q                                        NA
    ## 404          -73.91784      N      Q                                        NA
    ## 405          -73.91784      N      Q                                        NA
    ## 406          -73.92551      N      Q                                        NA
    ## 407          -73.92551      N      Q                                        NA
    ## 408          -73.92551      N      Q                                        NA
    ## 409          -73.91203      N      Q                                        NA
    ## 410          -73.91203      N      Q                                        NA
    ## 411          -73.91203      N      Q                                        NA
    ## 412          -73.91203      N      Q                                        NA
    ## 413          -73.97237      B      Q                                        NA
    ## 414          -73.97237      B      Q                                        NA
    ## 415          -73.97689      B      Q      D      N      R      2      3      4
    ## 416          -73.96164      B      Q                                        NA
    ## 417          -73.96164      B      Q                                        NA
    ## 418          -73.96080      B      Q                                        NA
    ## 419          -73.96080      B      Q                                        NA
    ## 420          -73.96080      B      Q                                        NA
    ## 421          -73.95940      B      Q                                        NA
    ## 422          -73.95940      B      Q                                        NA
    ## 423          -73.95593      B      Q                                        NA
    ## 424          -73.96449      B      Q                                        NA
    ## 425          -73.96138      B      Q                                        NA
    ## 426          -73.96138      B      Q                                        NA
    ## 427          -73.96138      B      Q                                        NA
    ## 428          -73.96138      B      Q                                        NA
    ## 429          -73.96138      B      Q                                        NA
    ## 430          -73.96138      B      Q                                        NA
    ## 431          -73.96138      B      Q                                        NA
    ## 432          -73.96138      B      Q                                        NA
    ## 433          -73.96298      B      Q                                        NA
    ## 434          -73.96298      B      Q                                        NA
    ## 435          -73.96389      B      Q                                        NA
    ## 436          -73.95773      B      Q                                        NA
    ## 437          -73.95773      B      Q                                        NA
    ## 438          -73.95773      B      Q                                        NA
    ## 439          -73.95516      B      Q                                        NA
    ## 440          -73.96279      B      Q                                        NA
    ## 441          -73.96850      Q                                               NA
    ## 442          -73.96850      Q                                               NA
    ## 443          -73.96850      Q                                               NA
    ## 444          -73.96850      Q                                               NA
    ## 445          -73.96850      Q                                               NA
    ## 446          -73.96850      Q                                               NA
    ## 447          -73.96149      B      Q                                        NA
    ## 448          -73.96149      B      Q                                        NA
    ## 449          -73.96225      B      Q     FS                                 NA
    ## 450          -73.96225      B      Q     FS                                 NA
    ## 451          -73.96225      B      Q     FS                                 NA
    ## 452          -73.95416      B      Q                                        NA
    ## 453          -73.95416      B      Q                                        NA
    ## 454          -73.98123      D      F      N      Q                          NA
    ## 455          -73.97594      F      Q                                        NA
    ## 456          -73.97594      F      Q                                        NA
    ## 457          -73.98934      N      R                                        NA
    ## 458          -73.98934      N      R                                        NA
    ## 459          -73.98934      N      R                                        NA
    ## 460          -73.98934      N      R                                        NA
    ## 461          -73.98934      N      R                                        NA
    ## 462          -73.98934      N      R                                        NA
    ## 463          -73.98934      N      R                                        NA
    ## 464          -73.98934      N      R                                        NA
    ## 465          -73.98869      N      R                                        NA
    ## 466          -73.98869      N      R                                        NA
    ## 467          -73.98869      N      R                                        NA
    ## 468          -73.98869      N      R                                        NA
    ## 469          -73.98795      B      D      F      M      N      Q      R     NA
    ## 470          -73.98795      B      D      F      M      N      Q      R     NA
    ## 471          -73.98795      B      D      F      M      N      Q      R     NA
    ## 472          -73.98795      B      D      F      M      N      Q      R     NA
    ## 473          -73.98414      N      Q      R                                 NA
    ## 474          -73.98414      N      Q      R                                 NA
    ## 475          -73.98414      N      Q      R                                 NA
    ## 476          -73.98414      N      Q      R                                 NA
    ## 477          -73.98414      N      Q      R                                 NA
    ## 478          -73.98414      N      Q      R                                 NA
    ## 479          -73.98414      N      Q      R                                 NA
    ## 480          -73.98066      N      Q      R                                 NA
    ## 481          -73.98066      N      Q      R                                 NA
    ## 482          -73.98066      N      Q      R                                 NA
    ## 483          -73.98066      N      Q      R                                 NA
    ## 484          -73.98066      N      Q      R                                 NA
    ## 485          -73.98066      N      Q      R                                 NA
    ## 486          -73.98066      N      Q      R                                 NA
    ## 487          -73.98066      N      Q      R                                 NA
    ## 488          -73.97335      N      Q      R                                 NA
    ## 489          -73.97335      N      Q      R                                 NA
    ## 490          -73.97335      N      Q      R                                 NA
    ## 491          -73.97335      N      Q      R                                 NA
    ## 492          -73.97335      N      Q      R                                 NA
    ## 493          -73.97335      N      Q      R                                 NA
    ## 494          -73.97335      N      Q      R                                 NA
    ## 495          -73.99263      N      R                                        NA
    ## 496          -73.99263      N      R                                        NA
    ## 497          -73.99263      N      R                                        NA
    ## 498          -73.99263      N      R                                        NA
    ## 499          -73.99263      N      R                                        NA
    ## 500          -73.99263      N      R                                        NA
    ## 501          -73.99263      N      R                                        NA
    ## 502          -73.99263      N      R                                        NA
    ## 503          -74.00177      J      N      Q      R      Z      6            NA
    ## 504          -74.00177      J      N      Q      R      Z      6            NA
    ## 505          -74.00177      J      N      Q      R      Z      6            NA
    ## 506          -74.00177      J      N      Q      R      Z      6            NA
    ## 507          -74.00177      J      N      Q      R      Z      6            NA
    ## 508          -74.00177      J      N      Q      R      Z      6            NA
    ## 509          -74.00177      J      N      Q      R      Z      6            NA
    ## 510          -74.00698      R                                               NA
    ## 511          -74.00698      R                                               NA
    ## 512          -74.00698      R                                               NA
    ## 513          -74.01103      R                                               NA
    ## 514          -74.01103      R                                               NA
    ## 515          -74.01103      R                                               NA
    ## 516          -74.01103      R                                               NA
    ## 517          -73.99178      R      2      3      4      5                   NA
    ## 518          -73.99178      R      2      3      4      5                   NA
    ## 519          -73.99178      R      2      3      4      5                   NA
    ## 520          -73.98594      A      C      F      R                          NA
    ## 521          -73.98594      A      C      F      R                          NA
    ## 522          -73.98594      A      C      F      R                          NA
    ## 523          -73.98594      A      C      F      R                          NA
    ## 524          -73.98594      A      C      F      R                          NA
    ## 525          -73.98594      A      C      F      R                          NA
    ## 526          -73.98594      A      C      F      R                          NA
    ## 527          -73.96726      N      Q      R      4      5      6            NA
    ## 528          -73.96726      N      Q      R      4      5      6            NA
    ## 529          -73.96726      N      Q      R      4      5      6            NA
    ## 530          -73.96726      N      Q      R      4      5      6            NA
    ## 531          -73.99770      N      R                                        NA
    ## 532          -73.99770      N      R                                        NA
    ## 533          -73.99770      N      R                                        NA
    ## 534          -73.99770      N      R                                        NA
    ## 535          -74.01334      R                                               NA
    ## 536          -74.01334      R                                               NA
    ## 537          -74.01334      R                                               NA
    ## 538          -74.01334      R                                               NA
    ## 539          -74.01334      R                                               NA
    ## 540          -74.01334      R                                               NA
    ## 541          -74.01334      R                                               NA
    ## 542          -74.01334      R                                               NA
    ## 543          -74.01334      R                                               NA
    ## 544          -73.98675      A      C      E      N      Q      R      S      1
    ## 545          -73.98675      A      C      E      N      Q      R      S      1
    ## 546          -73.98675      A      C      E      N      Q      R      S      1
    ## 547          -73.98675      A      C      E      N      Q      R      S      1
    ## 548          -73.99057      L      N      Q      R      4      5      6     NA
    ## 549          -73.99057      L      N      Q      R      4      5      6     NA
    ## 550          -73.99057      L      N      Q      R      4      5      6     NA
    ## 551          -73.99057      L      N      Q      R      4      5      6     NA
    ## 552          -74.01299      R      1                                        NA
    ## 553          -74.01299      R      1                                        NA
    ## 554          -74.01299      R      1                                        NA
    ## 555          -74.01299      R      1                                        NA
    ## 556          -74.01299      R      1                                        NA
    ## 557          -74.01299      R      1                                        NA
    ## 558          -73.84433      J      Z                                        NA
    ## 559          -73.84433      J      Z                                        NA
    ## 560          -73.83634      J                                               NA
    ## 561          -73.83634      J                                               NA
    ## 562          -73.82829      J      Z                                        NA
    ## 563          -73.82829      J      Z                                        NA
    ## 564          -73.82829      J      Z                                        NA
    ## 565          -73.82829      J      Z                                        NA
    ## 566          -73.89865      J                                               NA
    ## 567          -73.89865      J                                               NA
    ## 568          -73.91046      J      Z                                        NA
    ## 569          -73.91046      J      Z                                        NA
    ## 570          -73.88464      J                                               NA
    ## 571          -73.88464      J                                               NA
    ## 572          -73.87378      J      Z                                        NA
    ## 573          -73.87378      J      Z                                        NA
    ## 574          -73.87255      J                                               NA
    ## 575          -73.87255      J                                               NA
    ## 576          -73.87255      J                                               NA
    ## 577          -73.86714      J      Z                                        NA
    ## 578          -73.86714      J      Z                                        NA
    ## 579          -73.94113      J      M                                        NA
    ## 580          -73.94113      J      M                                        NA
    ## 581          -73.94113      J      M                                        NA
    ## 582          -73.94113      J      M                                        NA
    ## 583          -73.86001      J                                               NA
    ## 584          -73.86001      J                                               NA
    ## 585          -73.92227      J      Z                                        NA
    ## 586          -73.92227      J      Z                                        NA
    ## 587          -73.91656      J                                               NA
    ## 588          -73.91656      J                                               NA
    ## 589          -73.95343      J      M                                        NA
    ## 590          -73.95343      J      M                                        NA
    ## 591          -73.92881      J                                               NA
    ## 592          -73.92881      J                                               NA
    ## 593          -73.94741      J      M                                        NA
    ## 594          -73.94741      J      M                                        NA
    ## 595          -73.94741      J      M                                        NA
    ## 596          -73.94741      J      M                                        NA
    ## 597          -73.95776      J      M      Z                                 NA
    ## 598          -73.95776      J      M      Z                                 NA
    ## 599          -73.95776      J      M      Z                                 NA
    ## 600          -73.95776      J      M      Z                                 NA
    ## 601          -73.95776      J      M      Z                                 NA
    ## 602          -73.95776      J      M      Z                                 NA
    ## 603          -73.95776      J      M      Z                                 NA
    ## 604          -73.95776      J      M      Z                                 NA
    ## 605          -73.93566      J      M      Z                                 NA
    ## 606          -73.93566      J      M      Z                                 NA
    ## 607          -73.88004      J      Z                                        NA
    ## 608          -73.88004      J      Z                                        NA
    ## 609          -73.89169      J      Z                                        NA
    ## 610          -73.89169      J      Z                                        NA
    ## 611          -73.85158      J      Z                                        NA
    ## 612          -73.85158      J      Z                                        NA
    ## 613          -73.85158      J      Z                                        NA
    ## 614          -73.96838      1                                               NA
    ## 615          -73.96838      1                                               NA
    ## 616          -73.96838      1                                               NA
    ## 617          -73.96838      1                                               NA
    ## 618          -73.96838      1                                               NA
    ## 619          -73.96411      1                                               NA
    ## 620          -73.96411      1                                               NA
    ## 621          -73.96411      1                                               NA
    ## 622          -73.96411      1                                               NA
    ## 623          -73.96411      1                                               NA
    ## 624          -73.95837      1                                               NA
    ## 625          -73.95837      1                                               NA
    ## 626          -73.95837      1                                               NA
    ## 627          -73.95837      1                                               NA
    ## 628          -73.95368      1                                               NA
    ## 629          -73.95368      1                                               NA
    ## 630          -73.95368      1                                               NA
    ## 631          -73.95368      1                                               NA
    ## 632          -73.95036      1                                               NA
    ## 633          -73.95036      1                                               NA
    ## 634          -73.95036      1                                               NA
    ## 635          -74.00020      F      L      M      1      2      3            NA
    ## 636          -74.00020      F      L      M      1      2      3            NA
    ## 637          -74.00020      F      L      M      1      2      3            NA
    ## 638          -74.00020      F      L      M      1      2      3            NA
    ## 639          -74.00020      F      L      M      1      2      3            NA
    ## 640          -74.00020      F      L      M      1      2      3            NA
    ## 641          -74.00020      F      L      M      1      2      3            NA
    ## 642          -74.00020      F      L      M      1      2      3            NA
    ## 643          -73.94489      1                                               NA
    ## 644          -73.94489      1                                               NA
    ## 645          -73.94489      1                                               NA
    ## 646          -73.94489      1                                               NA
    ## 647          -73.94013      A      C      1                                 NA
    ## 648          -73.94013      A      C      1                                 NA
    ## 649          -73.94013      A      C      1                                 NA
    ## 650          -73.93360      1                                               NA
    ## 651          -73.93360      1                                               NA
    ## 652          -73.99787      1                                               NA
    ## 653          -73.99787      1                                               NA
    ## 654          -73.99787      1                                               NA
    ## 655          -73.99787      1                                               NA
    ## 656          -73.99787      1                                               NA
    ## 657          -73.99787      1                                               NA
    ## 658          -73.92941      1                                               NA
    ## 659          -73.92941      1                                               NA
    ## 660          -73.91882      1                                               NA
    ## 661          -73.91882      1                                               NA
    ## 662          -73.91882      1                                               NA
    ## 663          -73.91882      1                                               NA
    ## 664          -73.91528      1                                               NA
    ## 665          -73.91528      1                                               NA
    ## 666          -73.91528      1                                               NA
    ## 667          -73.91528      1                                               NA
    ## 668          -73.90483      1                                               NA
    ## 669          -73.90483      1                                               NA
    ## 670          -73.90483      1                                               NA
    ## 671          -73.90483      1                                               NA
    ## 672          -73.90087      1                                               NA
    ## 673          -73.90087      1                                               NA
    ## 674          -73.90087      1                                               NA
    ## 675          -73.99566      1                                               NA
    ## 676          -73.99566      1                                               NA
    ## 677          -73.99566      1                                               NA
    ## 678          -73.99566      1                                               NA
    ## 679          -73.99336      1                                               NA
    ## 680          -73.99336      1                                               NA
    ## 681          -73.99336      1                                               NA
    ## 682          -73.99336      1                                               NA
    ## 683          -73.99336      1                                               NA
    ## 684          -73.99336      1                                               NA
    ## 685          -73.99106      1      2      3                                 NA
    ## 686          -73.99106      1      2      3                                 NA
    ## 687          -73.99106      1      2      3                                 NA
    ## 688          -73.99106      1      2      3                                 NA
    ## 689          -73.99106      1      2      3                                 NA
    ## 690          -73.99106      1      2      3                                 NA
    ## 691          -73.99106      1      2      3                                 NA
    ## 692          -73.99106      1      2      3                                 NA
    ## 693          -73.99106      1      2      3                                 NA
    ## 694          -73.99106      1      2      3                                 NA
    ## 695          -73.99106      1      2      3                                 NA
    ## 696          -73.98385      1                                               NA
    ## 697          -73.98385      1                                               NA
    ## 698          -73.98385      1                                               NA
    ## 699          -73.98385      1                                               NA
    ## 700          -73.98385      1                                               NA
    ## 701          -73.98385      1                                               NA
    ## 702          -73.98193      A      B      C      D      1                   NA
    ## 703          -73.98193      A      B      C      D      1                   NA
    ## 704          -73.98193      A      B      C      D      1                   NA
    ## 705          -73.98193      A      B      C      D      1                   NA
    ## 706          -73.98193      A      B      C      D      1                   NA
    ## 707          -73.98221      1                                               NA
    ## 708          -73.98221      1                                               NA
    ## 709          -73.98221      1                                               NA
    ## 710          -73.98221      1                                               NA
    ## 711          -73.98221      1                                               NA
    ## 712          -73.98221      1                                               NA
    ## 713          -73.98197      1      2      3                                 NA
    ## 714          -73.98197      1      2      3                                 NA
    ## 715          -73.98197      1      2      3                                 NA
    ## 716          -73.98197      1      2      3                                 NA
    ## 717          -73.97992      1                                               NA
    ## 718          -73.97992      1                                               NA
    ## 719          -73.97992      1                                               NA
    ## 720          -73.97992      1                                               NA
    ## 721          -73.97622      1                                               NA
    ## 722          -73.97622      1                                               NA
    ## 723          -73.97622      1                                               NA
    ## 724          -73.97622      1                                               NA
    ## 725          -73.97622      1                                               NA
    ## 726          -73.97232      1      2      3                                 NA
    ## 727          -73.97232      1      2      3                                 NA
    ## 728          -73.97232      1      2      3                                 NA
    ## 729          -73.97232      1      2      3                                 NA
    ## 730          -73.97232      1      2      3                                 NA
    ## 731          -73.97232      1      2      3                                 NA
    ## 732          -74.00628      1                                               NA
    ## 733          -74.00628      1                                               NA
    ## 734          -74.00628      1                                               NA
    ## 735          -74.00628      1                                               NA
    ## 736          -73.96685      1                                               NA
    ## 737          -73.96685      1                                               NA
    ## 738          -73.96685      1                                               NA
    ## 739          -74.00927      1      2      3                                 NA
    ## 740          -74.00927      1      2      3                                 NA
    ## 741          -74.00927      1      2      3                                 NA
    ## 742          -74.00927      1      2      3                                 NA
    ## 743          -74.00927      1      2      3                                 NA
    ## 744          -74.00291      1                                               NA
    ## 745          -74.00291      1                                               NA
    ## 746          -74.00291      1                                               NA
    ## 747          -74.00291      1                                               NA
    ## 748          -74.00291      1                                               NA
    ## 749          -73.92554      1                                               NA
    ## 750          -73.92554      1                                               NA
    ## 751          -74.00689      1                                               NA
    ## 752          -74.00689      1                                               NA
    ## 753          -74.00689      1                                               NA
    ## 754          -74.00689      1                                               NA
    ## 755          -74.00689      1                                               NA
    ## 756          -74.00537      1                                               NA
    ## 757          -74.00537      1                                               NA
    ## 758          -74.00537      1                                               NA
    ## 759          -74.00537      1                                               NA
    ## 760          -74.00537      1                                               NA
    ## 761          -74.00537      1                                               NA
    ## 762          -74.00537      1                                               NA
    ## 763          -74.00537      1                                               NA
    ## 764          -73.90983      1                                               NA
    ## 765          -73.90983      1                                               NA
    ## 766          -74.01378      1                                               NA
    ## 767          -74.01378      1                                               NA
    ## 768          -74.01378      1                                               NA
    ## 769          -74.01378      1                                               NA
    ## 770          -74.01378      1                                               NA
    ## 771          -74.01378      1                                               NA
    ## 772          -74.01366      R      1                                        NA
    ## 773          -74.01366      R      1                                        NA
    ## 774          -74.01366      R      1                                        NA
    ## 775          -74.01366      R      1                                        NA
    ## 776          -74.01366      R      1                                        NA
    ## 777          -73.98749      A      C      E      N      Q      R      S      1
    ## 778          -73.98749      A      C      E      N      Q      R      S      1
    ## 779          -73.98749      A      C      E      N      Q      R      S      1
    ## 780          -73.98749      A      C      E      N      Q      R      S      1
    ## 781          -73.98749      A      C      E      N      Q      R      S      1
    ## 782          -73.98749      A      C      E      N      Q      R      S      1
    ## 783          -73.98749      A      C      E      N      Q      R      S      1
    ## 784          -73.98749      A      C      E      N      Q      R      S      1
    ## 785          -73.98749      A      C      E      N      Q      R      S      1
    ## 786          -73.89858      1                                               NA
    ## 787          -73.89858      1                                               NA
    ## 788          -73.89858      1                                               NA
    ## 789          -73.89858      1                                               NA
    ## 790          -73.98163      L                                               NA
    ## 791          -73.98163      L                                               NA
    ## 792          -73.98163      L                                               NA
    ## 793          -73.98163      L                                               NA
    ## 794          -73.98612      L                                               NA
    ## 795          -73.98612      L                                               NA
    ## 796          -73.98612      L                                               NA
    ## 797          -73.98612      L                                               NA
    ## 798          -73.99679      F      L      M      1      2      3            NA
    ## 799          -73.99679      F      L      M      1      2      3            NA
    ## 800          -74.00258      A      C      E      L                          NA
    ## 801          -74.00258      A      C      E      L                          NA
    ## 802          -73.90310      L                                               NA
    ## 803          -73.90310      L                                               NA
    ## 804          -73.90310      L                                               NA
    ## 805          -73.95687      L                                               NA
    ## 806          -73.95687      L                                               NA
    ## 807          -73.95687      L                                               NA
    ## 808          -73.95687      L                                               NA
    ## 809          -73.90525      L                                               NA
    ## 810          -73.90185      L                                               NA
    ## 811          -73.90250      L                                               NA
    ## 812          -73.90174      L                                               NA
    ## 813          -73.91842      L                                               NA
    ## 814          -73.91842      L                                               NA
    ## 815          -73.91842      L                                               NA
    ## 816          -73.91842      L                                               NA
    ## 817          -73.91842      L                                               NA
    ## 818          -73.91842      L                                               NA
    ## 819          -73.91842      L                                               NA
    ## 820          -73.91842      L                                               NA
    ## 821          -73.89948      L                                               NA
    ## 822          -73.89948      L                                               NA
    ## 823          -73.94405      L                                               NA
    ## 824          -73.94405      L                                               NA
    ## 825          -73.94405      L                                               NA
    ## 826          -73.94405      L                                               NA
    ## 827          -73.94067      L                                               NA
    ## 828          -73.94067      L                                               NA
    ## 829          -73.94067      L                                               NA
    ## 830          -73.94067      L                                               NA
    ## 831          -73.90408      L                                               NA
    ## 832          -73.90408      L                                               NA
    ## 833          -73.90408      L                                               NA
    ## 834          -73.90408      L                                               NA
    ## 835          -73.90408      L                                               NA
    ## 836          -73.90408      L                                               NA
    ## 837          -73.92291      L                                               NA
    ## 838          -73.92291      L                                               NA
    ## 839          -73.92291      L                                               NA
    ## 840          -73.92291      L                                               NA
    ## 841          -73.92291      L                                               NA
    ## 842          -73.92291      L                                               NA
    ## 843          -73.90057      L                                               NA
    ## 844          -73.90057      L                                               NA
    ## 845          -73.95028      G      L                                        NA
    ## 846          -73.95028      G      L                                        NA
    ## 847          -73.93985      L                                               NA
    ## 848          -73.93985      L                                               NA
    ## 849          -73.93315      L                                               NA
    ## 850          -73.93315      L                                               NA
    ## 851          -73.93315      L                                               NA
    ## 852          -73.91159      L      M                                        NA
    ## 853          -73.91159      L      M                                        NA
    ## 854          -73.91159      L      M                                        NA
    ## 855          -73.89923      L                                               NA
    ## 856          -73.90197      L                                               NA
    ## 857          -73.99073      L      N      Q      R      4      5      6     NA
    ## 858          -73.99073      L      N      Q      R      4      5      6     NA
    ## 859          -73.90405      L                                               NA
    ## 860          -73.99000      R      2      3      4      5                   NA
    ## 861          -73.99000      R      2      3      4      5                   NA
    ## 862          -73.99000      R      2      3      4      5                   NA
    ## 863          -73.99000      R      2      3      4      5                   NA
    ## 864          -73.99309      2      3                                        NA
    ## 865          -73.99309      2      3                                        NA
    ## 866          -74.00657      A      C      J      Z      2      3      4      5
    ## 867          -74.00657      A      C      J      Z      2      3      4      5
    ## 868          -74.00657      A      C      J      Z      2      3      4      5
    ## 869          -74.00657      A      C      J      Z      2      3      4      5
    ## 870          -74.00881      A      C      E      1      2      3            NA
    ## 871          -74.00910      2      3                                        NA
    ## 872          -74.00910      2      3                                        NA
    ## 873          -74.00910      2      3                                        NA
    ## 874          -74.00910      2      3                                        NA
    ## 875          -74.00910      2      3                                        NA
    ## 876          -74.00910      2      3                                        NA
    ## 877          -73.93821      B      D                                        NA
    ## 878          -73.91843      B      D                                        NA
    ## 879          -73.91843      B      D                                        NA
    ## 880          -73.91843      B      D                                        NA
    ## 881          -73.91843      B      D                                        NA
    ## 882          -73.91843      B      D                                        NA
    ## 883          -73.91843      B      D                                        NA
    ## 884          -73.91340      B      D                                        NA
    ## 885          -73.91340      B      D                                        NA
    ## 886          -73.91340      B      D                                        NA
    ## 887          -73.91340      B      D                                        NA
    ## 888          -73.91340      B      D                                        NA
    ## 889          -73.91014      B      D                                        NA
    ## 890          -73.91014      B      D                                        NA
    ## 891          -73.91014      B      D                                        NA
    ## 892          -73.91014      B      D                                        NA
    ## 893          -73.90074      B      D                                        NA
    ## 894          -73.90074      B      D                                        NA
    ## 895          -73.90074      B      D                                        NA
    ## 896          -73.90074      B      D                                        NA
    ## 897          -73.88714      B      D                                        NA
    ## 898          -73.88714      B      D                                        NA
    ## 899          -73.88714      B      D                                        NA
    ## 900          -73.88714      B      D                                        NA
    ## 901          -73.88714      B      D                                        NA
    ## 902          -73.89775      B      D                                        NA
    ## 903          -73.89775      B      D                                        NA
    ## 904          -73.89775      B      D                                        NA
    ## 905          -73.89775      B      D                                        NA
    ## 906          -73.89775      B      D                                        NA
    ## 907          -73.89775      B      D                                        NA
    ## 908          -73.89351      B      D                                        NA
    ## 909          -73.89351      B      D                                        NA
    ## 910          -73.89351      B      D                                        NA
    ## 911          -73.89351      B      D                                        NA
    ## 912          -73.89351      B      D                                        NA
    ## 913          -73.87886      D                                               NA
    ## 914          -73.87886      D                                               NA
    ## 915          -73.87886      D                                               NA
    ## 916          -73.87886      D                                               NA
    ## 917          -73.90523      B      D                                        NA
    ## 918          -73.90523      B      D                                        NA
    ## 919          -73.90523      B      D                                        NA
    ## 920          -73.90523      B      D                                        NA
    ## 921          -73.90523      B      D                                        NA
    ## 922          -73.92565      B      D      4                                 NA
    ## 923          -73.92565      B      D      4                                 NA
    ## 924          -73.92565      B      D      4                                 NA
    ## 925          -73.92565      B      D      4                                 NA
    ## 926          -73.92565      B      D      4                                 NA
    ## 927          -73.92565      B      D      4                                 NA
    ## 928          -73.92565      B      D      4                                 NA
    ## 929          -73.92565      B      D      4                                 NA
    ## 930          -73.98123      D      F      N      Q                          NA
    ## 931          -73.97594      F      Q                                        NA
    ## 932          -73.97594      F      Q                                        NA
    ## 933          -73.94972      G                                               NA
    ## 934          -73.94972      G                                               NA
    ## 935          -73.94972      G                                               NA
    ## 936          -73.95352      G                                               NA
    ## 937          -73.95352      G                                               NA
    ## 938          -73.95352      G                                               NA
    ## 939          -73.95352      G                                               NA
    ## 940          -73.95352      G                                               NA
    ## 941          -73.95352      G                                               NA
    ## 942          -73.95031      G                                               NA
    ## 943          -73.95031      G                                               NA
    ## 944          -73.95031      G                                               NA
    ## 945          -73.95031      G                                               NA
    ## 946          -73.96007      G                                               NA
    ## 947          -73.96007      G                                               NA
    ## 948          -73.96007      G                                               NA
    ## 949          -73.96684      G                                               NA
    ## 950          -73.96684      G                                               NA
    ## 951          -73.96684      G                                               NA
    ## 952          -73.96684      G                                               NA
    ## 953          -73.96684      G                                               NA
    ## 954          -73.96684      G                                               NA
    ## 955          -73.96684      G                                               NA
    ## 956          -73.96684      G                                               NA
    ## 957          -73.95023      G                                               NA
    ## 958          -73.95023      G                                               NA
    ## 959          -73.97537      G                                               NA
    ## 960          -73.97537      G                                               NA
    ## 961          -73.95445      G                                               NA
    ## 962          -73.95445      G                                               NA
    ## 963          -73.95445      G                                               NA
    ## 964          -73.95445      G                                               NA
    ## 965          -73.95445      G                                               NA
    ## 966          -73.94383      G                                               NA
    ## 967          -73.94383      G                                               NA
    ## 968          -73.94383      G                                               NA
    ## 969          -73.95142      G      L                                        NA
    ## 970          -73.95142      G      L                                        NA
    ## 971          -73.95142      G      L                                        NA
    ## 972          -73.95142      G      L                                        NA
    ## 973          -73.94905      G                                               NA
    ## 974          -73.94905      G                                               NA
    ## 975          -73.95128      G                                               NA
    ## 976          -73.95128      G                                               NA
    ## 977          -73.95128      G                                               NA
    ## 978          -73.95128      G                                               NA
    ## 979          -73.95128      G                                               NA
    ## 980          -73.95128      G                                               NA
    ## 981          -73.97697      F                                               NA
    ## 982          -73.97697      F                                               NA
    ## 983          -73.97697      F                                               NA
    ## 984          -73.97697      F                                               NA
    ## 985          -73.97613      F                                               NA
    ## 986          -73.97613      F                                               NA
    ## 987          -73.97613      F                                               NA
    ## 988          -73.97613      F                                               NA
    ## 989          -73.97420      F                                               NA
    ## 990          -73.97420      F                                               NA
    ## 991          -73.97420      F                                               NA
    ## 992          -73.97420      F                                               NA
    ## 993          -73.97302      F                                               NA
    ## 994          -73.97302      F                                               NA
    ## 995          -73.97336      F                                               NA
    ## 996          -73.97336      F                                               NA
    ## 997          -73.97336      F                                               NA
    ## 998          -73.97336      F                                               NA
    ## 999          -73.97425      F                                               NA
    ## 1000         -73.97425      F                                               NA
    ## 1001         -73.97526      F                                               NA
    ## 1002         -73.97526      F                                               NA
    ## 1003         -73.97526      F                                               NA
    ## 1004         -73.97817      F                                               NA
    ## 1005         -73.97817      F                                               NA
    ## 1006         -73.97817      F                                               NA
    ## 1007         -73.97817      F                                               NA
    ## 1008         -73.97236      F                                               NA
    ## 1009         -73.97236      F                                               NA
    ## 1010         -73.97236      F                                               NA
    ## 1011         -73.97236      F                                               NA
    ## 1012         -73.97457      F                                               NA
    ## 1013         -73.97457      F                                               NA
    ## 1014         -73.83859      5                                               NA
    ## 1015         -73.83859      5                                               NA
    ## 1016         -73.83083      5                                               NA
    ## 1017         -73.84638      5                                               NA
    ## 1018         -73.84638      5                                               NA
    ## 1019         -73.86050      5                                               NA
    ## 1020         -73.85536      5                                               NA
    ## 1021         -73.97767      B      D      N      Q      R      2      3      4
    ## 1022         -73.97510      2      3                                        NA
    ## 1023         -73.97510      2      3                                        NA
    ## 1024         -73.97510      2      3                                        NA
    ## 1025         -73.97510      2      3                                        NA
    ## 1026         -73.97510      2      3                                        NA
    ## 1027         -73.96438      2      3                                        NA
    ## 1028         -73.96438      2      3                                        NA
    ## 1029         -73.95813     FS      2      3      4      5                   NA
    ## 1030         -73.95813     FS      2      3      4      5                   NA
    ## 1031         -73.95813     FS      2      3      4      5                   NA
    ## 1032         -73.95813     FS      2      3      4      5                   NA
    ## 1033         -73.97105      2      3                                        NA
    ## 1034         -73.97105      2      3                                        NA
    ## 1035         -73.97105      2      3                                        NA
    ## 1036         -73.97105      2      3                                        NA
    ## 1037         -73.98507      2      3                                        NA
    ## 1038         -73.98507      2      3                                        NA
    ## 1039         -73.98507      2      3                                        NA
    ## 1040         -73.98507      2      3                                        NA
    ## 1041         -73.98507      2      3                                        NA
    ## 1042         -73.94216      3                                               NA
    ## 1043         -73.94216      3                                               NA
    ## 1044         -73.98049      2      3      4      5                          NA
    ## 1045         -73.98049      2      3      4      5                          NA
    ## 1046         -73.98049      2      3      4      5                          NA
    ## 1047         -73.98049      2      3      4      5                          NA
    ## 1048         -73.95047      3                                               NA
    ## 1049         -73.95047      3                                               NA
    ## 1050         -73.93294      3      4                                        NA
    ## 1051         -73.93294      3      4                                        NA
    ## 1052         -73.93294      3      4                                        NA
    ## 1053         -73.93294      3      4                                        NA
    ## 1054         -73.93294      3      4                                        NA
    ## 1055         -73.93294      3      4                                        NA
    ## 1056         -73.93294      3      4                                        NA
    ## 1057         -73.86270      7                                               NA
    ## 1058         -73.86270      7                                               NA
    ## 1059         -73.86270      7                                               NA
    ## 1060         -73.86270      7                                               NA
    ## 1061         -73.85533      7                                               NA
    ## 1062         -73.85533      7                                               NA
    ## 1063         -73.85533      7                                               NA
    ## 1064         -73.94526      E      G      M      7                          NA
    ## 1065         -73.94526      E      G      M      7                          NA
    ## 1066         -73.94526      E      G      M      7                          NA
    ## 1067         -73.98196      B      D      F      M      7                   NA
    ## 1068         -73.98196      B      D      F      M      7                   NA
    ## 1069         -73.98196      B      D      F      M      7                   NA
    ## 1070         -73.88370      7                                               NA
    ## 1071         -73.88370      7                                               NA
    ## 1072         -73.88370      7                                               NA
    ## 1073         -73.87661      7                                               NA
    ## 1074         -73.87661      7                                               NA
    ## 1075         -73.87661      7                                               NA
    ## 1076         -73.91844      7                                               NA
    ## 1077         -73.91844      7                                               NA
    ## 1078         -73.91844      7                                               NA
    ## 1079         -73.91844      7                                               NA
    ## 1080         -73.91844      7                                               NA
    ## 1081         -73.91844      7                                               NA
    ## 1082         -73.89139      E      F      M      R      7                   NA
    ## 1083         -73.89640      7                                               NA
    ## 1084         -73.89640      7                                               NA
    ## 1085         -73.83003      7                                               NA
    ## 1086         -73.83003      7                                               NA
    ## 1087         -73.83003      7                                               NA
    ## 1088         -73.83003      7                                               NA
    ## 1089         -73.83003      7                                               NA
    ## 1090         -73.83003      7                                               NA
    ## 1091         -73.83003      7                                               NA
    ## 1092         -73.83003      7                                               NA
    ## 1093         -73.83003      7                                               NA
    ## 1094         -73.83003      7                                               NA
    ## 1095         -73.97604     GS      4      5      6      7                   NA
    ## 1096         -73.94892      7                                               NA
    ## 1097         -73.94892      7                                               NA
    ## 1098         -73.94892      7                                               NA
    ## 1099         -73.86953      7                                               NA
    ## 1100         -73.86953      7                                               NA
    ## 1101         -73.86953      7                                               NA
    ## 1102         -73.86953      7                                               NA
    ## 1103         -73.86953      7                                               NA
    ## 1104         -73.91255      7                                               NA
    ## 1105         -73.91255      7                                               NA
    ## 1106         -73.91255      7                                               NA
    ## 1107         -73.91255      7                                               NA
    ## 1108         -73.92402      7                                               NA
    ## 1109         -73.92402      7                                               NA
    ## 1110         -73.92402      7                                               NA
    ## 1111         -73.92402      7                                               NA
    ## 1112         -73.84562      7                                               NA
    ## 1113         -73.84562      7                                               NA
    ## 1114         -73.84562      7                                               NA
    ## 1115         -73.84562      7                                               NA
    ## 1116         -73.94020      N      Q      7                                 NA
    ## 1117         -73.94020      N      Q      7                                 NA
    ## 1118         -73.93100      7                                               NA
    ## 1119         -73.93100      7                                               NA
    ## 1120         -73.93100      7                                               NA
    ## 1121         -73.93100      7                                               NA
    ## 1122         -73.93100      7                                               NA
    ## 1123         -73.93100      7                                               NA
    ## 1124         -73.95358      7                                               NA
    ## 1125         -73.95358      7                                               NA
    ## 1126         -73.95358      7                                               NA
    ## 1127         -73.95358      7                                               NA
    ## 1128         -73.95358      7                                               NA
    ## 1129         -73.95358      7                                               NA
    ## 1130         -73.95358      7                                               NA
    ## 1131         -73.95358      7                                               NA
    ## 1132         -73.90298      7                                               NA
    ## 1133         -73.90298      7                                               NA
    ## 1134         -73.90298      7                                               NA
    ## 1135         -73.95924     FS      2      3      4      5                   NA
    ## 1136         -73.95583      A     FS                                        NA
    ## 1137         -73.95762     FS                                               NA
    ## 1138         -73.95762     FS                                               NA
    ## 1139         -73.90532      A      C      J      L                          NA
    ## 1140         -73.96584      C                                               NA
    ## 1141         -73.96584      C                                               NA
    ## 1142         -73.96584      C                                               NA
    ## 1143         -73.96584      C                                               NA
    ## 1144         -73.96584      C                                               NA
    ## 1145         -73.87211      A      C                                        NA
    ## 1146         -73.87211      A      C                                        NA
    ## 1147         -73.87211      A      C                                        NA
    ## 1148         -73.87211      A      C                                        NA
    ## 1149         -73.87211      A      C                                        NA
    ## 1150         -73.95685      A      C     FS                                 NA
    ## 1151         -73.98500      A      C      G                                 NA
    ## 1152         -73.98500      A      C      G                                 NA
    ## 1153         -73.98734      A      C      F      R                          NA
    ## 1154         -73.98734      A      C      F      R                          NA
    ## 1155         -73.98734      A      C      F      R                          NA
    ## 1156         -73.98734      A      C      F      R                          NA
    ## 1157         -73.98734      A      C      F      R                          NA
    ## 1158         -73.98734      A      C      F      R                          NA
    ## 1159         -73.98734      A      C      F      R                          NA
    ## 1160         -73.98734      A      C      F      R                          NA
    ## 1161         -73.98734      A      C      F      R                          NA
    ## 1162         -73.98734      A      C      F      R                          NA
    ## 1163         -73.98734      A      C      F      R                          NA
    ## 1164         -73.98734      A      C      F      R                          NA
    ## 1165         -73.98734      A      C      F      R                          NA
    ## 1166         -73.94086      A      C                                        NA
    ## 1167         -73.94086      A      C                                        NA
    ## 1168         -73.94086      A      C                                        NA
    ## 1169         -73.94086      A      C                                        NA
    ## 1170         -73.97395      C                                               NA
    ## 1171         -73.97395      C                                               NA
    ## 1172         -73.97395      C                                               NA
    ## 1173         -73.97395      C                                               NA
    ## 1174         -73.97395      C                                               NA
    ## 1175         -73.97395      C                                               NA
    ## 1176         -73.97395      C                                               NA
    ## 1177         -73.97395      C                                               NA
    ## 1178         -73.97395      C                                               NA
    ## 1179         -73.89655      A      C                                        NA
    ## 1180         -73.89655      A      C                                        NA
    ## 1181         -73.89655      A      C                                        NA
    ## 1182         -73.89655      A      C                                        NA
    ## 1183         -73.95043      A      C                                        NA
    ## 1184         -73.95043      A      C                                        NA
    ## 1185         -73.95043      A      C                                        NA
    ## 1186         -73.95043      A      C                                        NA
    ## 1187         -73.92079      A      C                                        NA
    ## 1188         -73.92079      A      C                                        NA
    ## 1189         -73.92079      A      C                                        NA
    ## 1190         -73.91195      A      C                                        NA
    ## 1191         -73.91195      A      C                                        NA
    ## 1192         -73.91195      A      C                                        NA
    ## 1193         -73.91195      A      C                                        NA
    ## 1194         -73.91195      A      C                                        NA
    ## 1195         -73.91195      A      C                                        NA
    ## 1196         -73.88075      A      C                                        NA
    ## 1197         -73.88075      A      C                                        NA
    ## 1198         -73.88075      A      C                                        NA
    ## 1199         -73.88075      A      C                                        NA
    ## 1200         -73.93073      A      C                                        NA
    ## 1201         -73.93073      A      C                                        NA
    ## 1202         -73.93073      A      C                                        NA
    ## 1203         -73.93073      A      C                                        NA
    ## 1204         -73.89036      A      C                                        NA
    ## 1205         -73.89036      A      C                                        NA
    ## 1206         -73.89036      A      C                                        NA
    ## 1207         -73.89036      A      C                                        NA
    ## 1208         -73.92985      4      5                                        NA
    ## 1209         -73.92985      4      5                                        NA
    ## 1210         -73.92985      4      5                                        NA
    ## 1211         -73.92735      2      4      5                                 NA
    ## 1212         -73.92735      2      4      5                                 NA
    ## 1213         -73.92735      2      4      5                                 NA
    ## 1214         -73.92735      2      4      5                                 NA
    ## 1215         -73.92140      4                                               NA
    ## 1216         -73.92140      4                                               NA
    ## 1217         -73.92140      4                                               NA
    ## 1218         -73.92140      4                                               NA
    ## 1219         -73.91779      4                                               NA
    ## 1220         -73.91779      4                                               NA
    ## 1221         -73.91779      4                                               NA
    ## 1222         -73.91179      4                                               NA
    ## 1223         -73.91179      4                                               NA
    ## 1224         -73.90388      4                                               NA
    ## 1225         -73.90388      4                                               NA
    ## 1226         -73.90388      4                                               NA
    ## 1227         -73.89006      4                                               NA
    ## 1228         -73.90768      4                                               NA
    ## 1229         -73.90768      4                                               NA
    ## 1230         -73.90768      4                                               NA
    ## 1231         -73.90768      4                                               NA
    ## 1232         -73.90103      4                                               NA
    ## 1233         -73.90103      4                                               NA
    ## 1234         -73.90103      4                                               NA
    ## 1235         -73.90103      4                                               NA
    ## 1236         -73.89717      4                                               NA
    ## 1237         -73.89717      4                                               NA
    ## 1238         -73.89717      4                                               NA
    ## 1239         -73.88465      4                                               NA
    ## 1240         -73.88465      4                                               NA
    ## 1241         -73.88465      4                                               NA
    ## 1242         -73.88465      4                                               NA
    ## 1243         -73.91469      4                                               NA
    ## 1244         -73.91469      4                                               NA
    ## 1245         -73.87875      4                                               NA
    ## 1246         -73.87875      4                                               NA
    ## 1247         -73.92583     B       D      4                                 NA
    ## 1248         -73.92583     B       D      4                                 NA
    ## 1249         -73.92583     B       D      4                                 NA
    ## 1250         -73.92583     B       D      4                                 NA
    ## 1251         -73.92583     B       D      4                                 NA
    ## 1252         -73.92583     B       D      4                                 NA
    ## 1253         -73.92583     B       D      4                                 NA
    ## 1254         -73.92583     B       D      4                                 NA
    ## 1255         -73.95182      2      3                                        NA
    ## 1256         -73.95182      2      3                                        NA
    ## 1257         -73.94962      2      3                                        NA
    ## 1258         -73.94962      2      3                                        NA
    ## 1259         -73.94962      2      3                                        NA
    ## 1260         -73.94962      2      3                                        NA
    ## 1261         -73.94549      2      3                                        NA
    ## 1262         -73.94549      2      3                                        NA
    ## 1263         -73.94549      2      3                                        NA
    ## 1264         -73.94549      2      3                                        NA
    ## 1265         -73.94077      2      3                                        NA
    ## 1266         -73.94077      2      3                                        NA
    ## 1267         -73.94077      2      3                                        NA
    ## 1268         -73.94077      2      3                                        NA
    ## 1269         -73.93624      3                                               NA
    ## 1270         -73.93624      3                                               NA
    ## 1271         -73.93624      3                                               NA
    ## 1272         -73.93624      3                                               NA
    ## 1273         -73.93647      3                                               NA
    ## 1274         -73.94748      6                                               NA
    ## 1275         -73.94748      6                                               NA
    ## 1276         -73.94425      6                                               NA
    ## 1277         -73.94425      6                                               NA
    ## 1278         -73.94425      6                                               NA
    ## 1279         -73.94425      6                                               NA
    ## 1280         -73.94162      6                                               NA
    ## 1281         -73.94162      6                                               NA
    ## 1282         -73.94162      6                                               NA
    ## 1283         -73.94162      6                                               NA
    ## 1284         -73.93759      4      5      6                                 NA
    ## 1285         -73.93759      4      5      6                                 NA
    ## 1286         -73.93759      4      5      6                                 NA
    ## 1287         -73.93759      4      5      6                                 NA
    ## 1288         -73.93759      4      5      6                                 NA
    ## 1289         -73.98995      L      N      Q      R      4      5      6     NA
    ## 1290         -73.98995      L      N      Q      R      4      5      6     NA
    ## 1291         -73.98995      L      N      Q      R      4      5      6     NA
    ## 1292         -73.98995      L      N      Q      R      4      5      6     NA
    ## 1293         -73.98995      L      N      Q      R      4      5      6     NA
    ## 1294         -73.98995      L      N      Q      R      4      5      6     NA
    ## 1295         -73.98660      6                                               NA
    ## 1296         -73.98660      6                                               NA
    ## 1297         -73.98660      6                                               NA
    ## 1298         -73.98660      6                                               NA
    ## 1299         -73.98660      6                                               NA
    ## 1300         -73.98660      6                                               NA
    ## 1301         -73.98660      6                                               NA
    ## 1302         -73.98660      6                                               NA
    ## 1303         -73.98660      6                                               NA
    ## 1304         -73.98426      6                                               NA
    ## 1305         -73.98426      6                                               NA
    ## 1306         -73.98426      6                                               NA
    ## 1307         -73.98426      6                                               NA
    ## 1308         -73.98426      6                                               NA
    ## 1309         -73.98426      6                                               NA
    ## 1310         -73.98426      6                                               NA
    ## 1311         -73.98208      6                                               NA
    ## 1312         -73.98208      6                                               NA
    ## 1313         -73.98208      6                                               NA
    ## 1314         -73.98208      6                                               NA
    ## 1315         -73.98208      6                                               NA
    ## 1316         -73.98208      6                                               NA
    ## 1317         -73.98208      6                                               NA
    ## 1318         -73.98208      6                                               NA
    ## 1319         -73.98208      6                                               NA
    ## 1320         -73.97192      E      M      6                                 NA
    ## 1321         -73.97192      E      M      6                                 NA
    ## 1322         -73.97192      E      M      6                                 NA
    ## 1323         -73.97192      E      M      6                                 NA
    ## 1324         -73.97192      E      M      6                                 NA
    ## 1325         -73.97192      E      M      6                                 NA
    ## 1326         -73.97192      E      M      6                                 NA
    ## 1327         -73.97192      E      M      6                                 NA
    ## 1328         -73.97192      E      M      6                                 NA
    ## 1329         -73.96797      N      Q      R      4      5      6            NA
    ## 1330         -73.96797      N      Q      R      4      5      6            NA
    ## 1331         -73.96797      N      Q      R      4      5      6            NA
    ## 1332         -73.96797      N      Q      R      4      5      6            NA
    ## 1333         -73.96797      N      Q      R      4      5      6            NA
    ## 1334         -73.96797      N      Q      R      4      5      6            NA
    ## 1335         -73.96797      N      Q      R      4      5      6            NA
    ## 1336         -73.96387      6                                               NA
    ## 1337         -73.96387      6                                               NA
    ## 1338         -73.96387      6                                               NA
    ## 1339         -73.96387      6                                               NA
    ## 1340         -73.96387      6                                               NA
    ## 1341         -73.95987      6                                               NA
    ## 1342         -73.95987      6                                               NA
    ## 1343         -73.95987      6                                               NA
    ## 1344         -73.95987      6                                               NA
    ## 1345         -73.95987      6                                               NA
    ## 1346         -73.95987      6                                               NA
    ## 1347         -73.95987      6                                               NA
    ## 1348         -73.95987      6                                               NA
    ## 1349         -73.95559      4      5      6                                 NA
    ## 1350         -73.95559      4      5      6                                 NA
    ## 1351         -73.95559      4      5      6                                 NA
    ## 1352         -73.95559      4      5      6                                 NA
    ## 1353         -73.95559      4      5      6                                 NA
    ## 1354         -73.95559      4      5      6                                 NA
    ## 1355         -73.95559      4      5      6                                 NA
    ## 1356         -73.95559      4      5      6                                 NA
    ## 1357         -73.95107      6                                               NA
    ## 1358         -73.95107      6                                               NA
    ## 1359         -73.95107      6                                               NA
    ## 1360         -73.95107      6                                               NA
    ## 1361         -73.99107      6                                               NA
    ## 1362         -73.99107      6                                               NA
    ## 1363         -73.99466      B      D      F      M      6                   NA
    ## 1364         -73.99466      B      D      F      M      6                   NA
    ## 1365         -73.99466      B      D      F      M      6                   NA
    ## 1366         -73.99466      B      D      F      M      6                   NA
    ## 1367         -73.99466      B      D      F      M      6                   NA
    ## 1368         -73.99466      B      D      F      M      6                   NA
    ## 1369         -73.99466      B      D      F      M      6                   NA
    ## 1370         -73.99015      R      2      3      4      5                   NA
    ## 1371         -73.99015      R      2      3      4      5                   NA
    ## 1372         -73.99015      R      2      3      4      5                   NA
    ## 1373         -73.99015      R      2      3      4      5                   NA
    ## 1374         -73.99015      R      2      3      4      5                   NA
    ## 1375         -74.01407      4      5                                        NA
    ## 1376         -74.01407      4      5                                        NA
    ## 1377         -74.01407      4      5                                        NA
    ## 1378         -74.01407      4      5                                        NA
    ## 1379         -74.01407      4      5                                        NA
    ## 1380         -74.00481      J      Z      4      5      6                   NA
    ## 1381         -74.00497      J      Z      4      5      6                   NA
    ## 1382         -74.00439      J      Z      4      5      6                   NA
    ## 1383         -74.00511      J      Z      4      5      6                   NA
    ## 1384         -74.00506      J      Z      4      5      6                   NA
    ## 1385         -74.00391      J      Z      4      5      6                   NA
    ## 1386         -74.00365      J      Z      4      5      6                   NA
    ## 1387         -74.00413      J      Z      4      5      6                   NA
    ## 1388         -74.00413      J      Z      4      5      6                   NA
    ## 1389         -74.00019      J      N      Q      R      Z      6            NA
    ## 1390         -74.00019      J      N      Q      R      Z      6            NA
    ## 1391         -74.00019      J      N      Q      R      Z      6            NA
    ## 1392         -74.00019      J      N      Q      R      Z      6            NA
    ## 1393         -74.00019      J      N      Q      R      Z      6            NA
    ## 1394         -74.00019      J      N      Q      R      Z      6            NA
    ## 1395         -74.00019      J      N      Q      R      Z      6            NA
    ## 1396         -74.00951      A      C      J      Z      2      3      4      5
    ## 1397         -74.00951      A      C      J      Z      2      3      4      5
    ## 1398         -74.00951      A      C      J      Z      2      3      4      5
    ## 1399         -74.00951      A      C      J      Z      2      3      4      5
    ## 1400         -74.00951      A      C      J      Z      2      3      4      5
    ## 1401         -73.97685     GS      4      5      6      7                   NA
    ## 1402         -73.97685     GS      4      5      6      7                   NA
    ## 1403         -73.97685     GS      4      5      6      7                   NA
    ## 1404         -73.97685     GS      4      5      6      7                   NA
    ## 1405         -73.97685     GS      4      5      6      7                   NA
    ## 1406         -73.97685     GS      4      5      6      7                   NA
    ## 1407         -73.97685     GS      4      5      6      7                   NA
    ## 1408         -73.97685     GS      4      5      6      7                   NA
    ## 1409         -73.99714      6                                               NA
    ## 1410         -73.99714      6                                               NA
    ## 1411         -73.99714      6                                               NA
    ## 1412         -73.99714      6                                               NA
    ## 1413         -74.01186      4      5                                        NA
    ## 1414         -74.01186      4      5                                        NA
    ## 1415         -74.01186      4      5                                        NA
    ## 1416         -74.01186      4      5                                        NA
    ## 1417         -74.01186      4      5                                        NA
    ## 1418         -74.01186      4      5                                        NA
    ## 1419         -74.01186      4      5                                        NA
    ## 1420         -74.01186      4      5                                        NA
    ## 1421         -73.83768      A                                               NA
    ## 1422         -73.83768      A                                               NA
    ## 1423         -73.83216      A                                               NA
    ## 1424         -73.83216      A                                               NA
    ## 1425         -73.83216      A                                               NA
    ## 1426         -73.83216      A                                               NA
    ## 1427         -73.85899      A                                               NA
    ## 1428         -73.85899      A                                               NA
    ## 1429         -73.85899      A                                               NA
    ## 1430         -73.85899      A                                               NA
    ## 1431         -73.85147      A                                               NA
    ## 1432         -73.85147      A                                               NA
    ## 1433         -73.86505      A                                               NA
    ## 1434         -73.82580      A                                               NA
    ## 1435         -73.82580      A                                               NA
    ## 1436         -73.82580      A                                               NA
    ## 1437         -73.82580      A                                               NA
    ## 1438         -73.84385      A                                               NA
    ## 1439         -73.84385      A                                               NA
    ## 1440         -73.84385      A                                               NA
    ## 1441         -73.84385      A                                               NA
    ## 1442         -73.92740      M                                               NA
    ## 1443         -73.92740      M                                               NA
    ## 1444         -73.90308      M                                               NA
    ## 1445         -73.90308      M                                               NA
    ## 1446         -73.89588      M                                               NA
    ## 1447         -73.89588      M                                               NA
    ## 1448         -73.91971      M                                               NA
    ## 1449         -73.91971      M                                               NA
    ## 1450         -73.88960      M                                               NA
    ## 1451         -73.90774      M                                               NA
    ## 1452         -73.90774      M                                               NA
    ## 1453         -73.99392      J      Z                                        NA
    ## 1454         -73.99392      J      Z                                        NA
    ## 1455         -74.01106      J      Z                                        NA
    ## 1456         -74.01106      J      Z                                        NA
    ## 1457         -74.01106      J      Z                                        NA
    ## 1458         -74.01106      J      Z                                        NA
    ## 1459         -74.01106      J      Z                                        NA
    ## 1460         -74.01106      J      Z                                        NA
    ## 1461         -74.01106      J      Z                                        NA
    ## 1462         -74.01106      J      Z                                        NA
    ## 1463         -74.01106      J      Z                                        NA
    ## 1464         -73.99989      J      N      Q      R      Z      6            NA
    ## 1465         -74.00340      J      Z      4      5      6                   NA
    ## 1466         -74.00320      J      Z      4      5      6                   NA
    ## 1467         -73.98744      F      J      M      Z                          NA
    ## 1468         -73.98744      F      J      M      Z                          NA
    ## 1469         -74.00758      A      C      J      Z      2      3      4      5
    ## 1470         -74.00758      A      C      J      Z      2      3      4      5
    ## 1471         -74.00758      A      C      J      Z      2      3      4      5
    ## 1472         -74.00758      A      C      J      Z      2      3      4      5
    ## 1473         -74.00758      A      C      J      Z      2      3      4      5
    ## 1474         -74.00758      A      C      J      Z      2      3      4      5
    ## 1475         -73.90245      3                                               NA
    ## 1476         -73.90245      3                                               NA
    ## 1477         -73.88408      3                                               NA
    ## 1478         -73.88408      3                                               NA
    ## 1479         -73.89490      3                                               NA
    ## 1480         -73.89490      3                                               NA
    ## 1481         -73.90895      3                                               NA
    ## 1482         -73.90895      3                                               NA
    ## 1483         -73.91633      3                                               NA
    ## 1484         -73.91633      3                                               NA
    ## 1485         -73.91633      3                                               NA
    ## 1486         -73.92261      3                                               NA
    ## 1487         -73.92261      3                                               NA
    ## 1488         -73.92261      3                                               NA
    ## 1489         -73.88939      3                                               NA
    ## 1490         -73.88939      3                                               NA
    ## 1491         -73.94896      2      5                                        NA
    ## 1492         -73.94896      2      5                                        NA
    ## 1493         -73.94957      2      5                                        NA
    ## 1494         -73.94957      2      5                                        NA
    ## 1495         -73.94957      2      5                                        NA
    ## 1496         -73.94957      2      5                                        NA
    ## 1497         -73.94957      2      5                                        NA
    ## 1498         -73.94957      2      5                                        NA
    ## 1499         -73.94957      2      5                                        NA
    ## 1500         -73.94764      2      5                                        NA
    ## 1501         -73.94764      2      5                                        NA
    ## 1502         -73.94764      2      5                                        NA
    ## 1503         -73.94764      2      5                                        NA
    ## 1504         -73.94764      2      5                                        NA
    ## 1505         -73.94764      2      5                                        NA
    ## 1506         -73.94764      2      5                                        NA
    ## 1507         -73.94764      2      5                                        NA
    ## 1508         -73.94841      2      5                                        NA
    ## 1509         -73.94841      2      5                                        NA
    ## 1510         -73.94841      2      5                                        NA
    ## 1511         -73.95068      2      5                                        NA
    ## 1512         -73.95068      2      5                                        NA
    ## 1513         -73.95085      2      5                                        NA
    ## 1514         -73.95085      2      5                                        NA
    ## 1515         -73.95020      2      5                                        NA
    ## 1516         -73.95020      2      5                                        NA
    ## 1517         -73.92614      6                                               NA
    ## 1518         -73.92614      6                                               NA
    ## 1519         -73.92614      6                                               NA
    ## 1520         -73.92614      6                                               NA
    ## 1521         -73.92614      6                                               NA
    ## 1522         -73.92614      6                                               NA
    ## 1523         -73.92614      6                                               NA
    ## 1524         -73.91924      6                                               NA
    ## 1525         -73.91924      6                                               NA
    ## 1526         -73.91924      6                                               NA
    ## 1527         -73.91924      6                                               NA
    ## 1528         -73.83257      6                                               NA
    ## 1529         -73.83257      6                                               NA
    ## 1530         -73.83257      6                                               NA
    ## 1531         -73.85122      6                                               NA
    ## 1532         -73.85122      6                                               NA
    ## 1533         -73.91404      6                                               NA
    ## 1534         -73.91404      6                                               NA
    ## 1535         -73.91404      6                                               NA
    ## 1536         -73.91404      6                                               NA
    ## 1537         -73.90766      6                                               NA
    ## 1538         -73.90766      6                                               NA
    ## 1539         -73.90766      6                                               NA
    ## 1540         -73.90766      6                                               NA
    ## 1541         -73.90410      6                                               NA
    ## 1542         -73.90410      6                                               NA
    ## 1543         -73.90410      6                                               NA
    ## 1544         -73.90410      6                                               NA
    ## 1545         -73.87916      6                                               NA
    ## 1546         -73.87916      6                                               NA
    ## 1547         -73.89055      6                                               NA
    ## 1548         -73.89055      6                                               NA
    ## 1549         -73.89055      6                                               NA
    ## 1550         -73.89643      6                                               NA
    ## 1551         -73.89643      6                                               NA
    ## 1552         -73.89643      6                                               NA
    ## 1553         -73.89643      6                                               NA
    ## 1554         -73.83632      6                                               NA
    ## 1555         -73.83632      6                                               NA
    ## 1556         -73.87452      6                                               NA
    ## 1557         -73.87452      6                                               NA
    ## 1558         -73.87452      6                                               NA
    ## 1559         -73.86082      6                                               NA
    ## 1560         -73.82812      6                                               NA
    ## 1561         -73.82812      6                                               NA
    ## 1562         -73.82812      6                                               NA
    ## 1563         -73.82812      6                                               NA
    ## 1564         -73.82812      6                                               NA
    ## 1565         -73.86762      6                                               NA
    ## 1566         -73.86762      6                                               NA
    ## 1567         -73.84295      6                                               NA
    ## 1568         -73.88628      6                                               NA
    ## 1569         -73.88628      6                                               NA
    ## 1570         -73.84704      6                                               NA
    ## 1571         -73.84704      6                                               NA
    ## 1572         -73.79360      F                                               NA
    ## 1573         -73.79360      F                                               NA
    ## 1574         -73.79360      F                                               NA
    ## 1575         -73.79360      F                                               NA
    ## 1576         -73.79360      F                                               NA
    ## 1577         -73.79360      F                                               NA
    ## 1578         -73.79360      F                                               NA
    ## 1579         -73.79360      F                                               NA
    ## 1580         -73.94600      E      G      M      7                          NA
    ## 1581         -73.94600      E      G      M      7                          NA
    ## 1582         -73.94600      E      G      M      7                          NA
    ## 1583         -73.94600      E      G      M      7                          NA
    ## 1584         -73.92878      M      R                                        NA
    ## 1585         -73.92878      M      R                                        NA
    ## 1586         -73.92878      M      R                                        NA
    ## 1587         -73.92878      M      R                                        NA
    ## 1588         -73.92878      M      R                                        NA
    ## 1589         -73.91333      M      R                                        NA
    ## 1590         -73.91333      M      R                                        NA
    ## 1591         -73.91333      M      R                                        NA
    ## 1592         -73.91333      M      R                                        NA
    ## 1593         -73.97522      E      M                                        NA
    ## 1594         -73.97522      E      M                                        NA
    ## 1595         -73.97522      E      M                                        NA
    ## 1596         -73.97522      E      M                                        NA
    ## 1597         -73.97522      E      M                                        NA
    ## 1598         -73.86160      M      R                                        NA
    ## 1599         -73.86160      M      R                                        NA
    ## 1600         -73.86160      M      R                                        NA
    ## 1601         -73.86160      M      R                                        NA
    ## 1602         -73.86160      M      R                                        NA
    ## 1603         -73.86160      M      R                                        NA
    ## 1604         -73.86160      M      R                                        NA
    ## 1605         -73.89845      M      R                                        NA
    ## 1606         -73.89845      M      R                                        NA
    ## 1607         -73.85272      M      R                                        NA
    ## 1608         -73.85272      M      R                                        NA
    ## 1609         -73.85272      M      R                                        NA
    ## 1610         -73.85272      M      R                                        NA
    ## 1611         -73.83732      E      F                                        NA
    ## 1612         -73.83732      E      F                                        NA
    ## 1613         -73.83732      E      F                                        NA
    ## 1614         -73.98164      B      D      E                                 NA
    ## 1615         -73.98164      B      D      E                                 NA
    ## 1616         -73.98164      B      D      E                                 NA
    ## 1617         -73.98164      B      D      E                                 NA
    ## 1618         -73.82057      F                                               NA
    ## 1619         -73.82057      F                                               NA
    ## 1620         -73.82057      F                                               NA
    ## 1621         -73.88202      M      R                                        NA
    ## 1622         -73.88202      M      R                                        NA
    ## 1623         -73.88202      M      R                                        NA
    ## 1624         -73.88202      M      R                                        NA
    ## 1625         -73.88202      M      R                                        NA
    ## 1626         -73.84452      E      F      M      R                          NA
    ## 1627         -73.84452      E      F      M      R                          NA
    ## 1628         -73.84452      e      F      M      R                          NA
    ## 1629         -73.84452      E      F      M      R                          NA
    ## 1630         -73.84452      E      F      M      R                          NA
    ## 1631         -73.87722      M      R                                        NA
    ## 1632         -73.87722      M      R                                        NA
    ## 1633         -73.87722      M      R                                        NA
    ## 1634         -73.87722      M      R                                        NA
    ## 1635         -73.87722      M      R                                        NA
    ## 1636         -73.89134      E      F      M      R      7                   NA
    ## 1637         -73.89134      E      F      M      R      7                   NA
    ## 1638         -73.89134      E      F      M      R      7                   NA
    ## 1639         -73.89134      E      F      M      R      7                   NA
    ## 1640         -73.89134      E      F      M      R      7                   NA
    ## 1641         -73.89134      E      F      M      R      7                   NA
    ## 1642         -73.89134      E      F      M      R      7                   NA
    ## 1643         -73.78382      F                                               NA
    ## 1644         -73.78382      F                                               NA
    ## 1645         -73.78382      F                                               NA
    ## 1646         -73.78382      F                                               NA
    ## 1647         -73.78382      F                                               NA
    ## 1648         -73.78382      F                                               NA
    ## 1649         -73.78382      F                                               NA
    ## 1650         -73.78382      F                                               NA
    ## 1651         -73.78382      F                                               NA
    ## 1652         -73.78382      F                                               NA
    ## 1653         -73.78382      F                                               NA
    ## 1654         -73.78382      F                                               NA
    ## 1655         -73.78382      F                                               NA
    ## 1656         -73.78382      F                                               NA
    ## 1657         -73.78382      F                                               NA
    ## 1658         -73.78382      F                                               NA
    ## 1659         -73.83101      E      F                                        NA
    ## 1660         -73.83101      E      F                                        NA
    ## 1661         -73.83101      E      F                                        NA
    ## 1662         -73.83101      E      F                                        NA
    ## 1663         -73.83101      E      F                                        NA
    ## 1664         -73.83101      E      F                                        NA
    ## 1665         -73.83101      E      F                                        NA
    ## 1666         -73.83101      E      F                                        NA
    ## 1667         -73.96905      E      M      6                                 NA
    ## 1668         -73.96905      E      M      6                                 NA
    ## 1669         -73.96905      E      M      6                                 NA
    ## 1670         -73.96905      E      M      6                                 NA
    ## 1671         -73.96905      E      M      6                                 NA
    ## 1672         -73.96905      E      M      6                                 NA
    ## 1673         -73.96905      E      M      6                                 NA
    ## 1674         -73.90601      M      R                                        NA
    ## 1675         -73.90601      M      R                                        NA
    ## 1676         -73.80333      F                                               NA
    ## 1677         -73.80333      F                                               NA
    ## 1678         -73.80333      F                                               NA
    ## 1679         -73.80333      F                                               NA
    ## 1680         -73.80333      F                                               NA
    ## 1681         -73.80333      F                                               NA
    ## 1682         -73.93724      E      M      R                                 NA
    ## 1683         -73.93724      E      M      R                                 NA
    ## 1684         -73.93724      E      M      R                                 NA
    ## 1685         -73.93724      E      M      R                                 NA
    ## 1686         -73.93724      E      M      R                                 NA
    ## 1687         -73.93724      E      M      R                                 NA
    ## 1688         -73.93724      E      M      R                                 NA
    ## 1689         -73.92074      M      R                                        NA
    ## 1690         -73.92074      M      R                                        NA
    ## 1691         -73.92074      M      R                                        NA
    ## 1692         -73.92074      M      R                                        NA
    ## 1693         -73.81071      F                                               NA
    ## 1694         -73.81071      F                                               NA
    ## 1695         -73.81071      F                                               NA
    ## 1696         -73.81071      F                                               NA
    ## 1697         -73.81071      F                                               NA
    ## 1698         -73.86923      M      R                                        NA
    ## 1699         -73.86923      M      R                                        NA
    ## 1700         -73.86923      M      R                                        NA
    ## 1701         -73.86923      M      R                                        NA
    ## 1702         -73.83581      A                                               NA
    ## 1703         -73.83406      A                                               NA
    ## 1704         -73.83406      A                                               NA
    ## 1705         -73.82756      H                                               NA
    ## 1706         -73.82756      H                                               NA
    ## 1707         -73.82756      H                                               NA
    ## 1708         -73.82756      H                                               NA
    ## 1709         -73.76135      A                                               NA
    ## 1710         -73.76135      A                                               NA
    ## 1711         -73.76135      A                                               NA
    ## 1712         -73.76817      A                                               NA
    ## 1713         -73.76817      A                                               NA
    ## 1714         -73.76817      A                                               NA
    ## 1715         -73.76817      A                                               NA
    ## 1716         -73.77601      A                                               NA
    ## 1717         -73.77601      A                                               NA
    ## 1718         -73.77601      A                                               NA
    ## 1719         -73.77601      A                                               NA
    ## 1720         -73.78852      A                                               NA
    ## 1721         -73.78852      A                                               NA
    ## 1722         -73.78852      A                                               NA
    ## 1723         -73.78852      A                                               NA
    ## 1724         -73.79692      A                                               NA
    ## 1725         -73.79692      A                                               NA
    ## 1726         -73.79692      A                                               NA
    ## 1727         -73.79692      A                                               NA
    ## 1728         -73.81364      H                                               NA
    ## 1729         -73.81364      H                                               NA
    ## 1730         -73.81364      H                                               NA
    ## 1731         -73.81364      H                                               NA
    ## 1732         -73.82056      H                                               NA
    ## 1733         -73.82056      H                                               NA
    ## 1734         -73.82056      H                                               NA
    ## 1735         -73.81592      A      H                                        NA
    ## 1736         -73.75540      A                                               NA
    ## 1737         -73.83030      A                                               NA
    ## 1738         -73.83030      A                                               NA
    ## 1739         -73.83030      A                                               NA
    ## 1740         -73.83030      A                                               NA
    ## 1741         -73.83559      H                                               NA
    ## 1742         -73.99041      N                                               NA
    ## 1743         -73.99041      N                                               NA
    ## 1744         -73.98503      N                                               NA
    ## 1745         -73.97823      N                                               NA
    ## 1746         -74.01172      N                                               NA
    ## 1747         -73.97914      N                                               NA
    ## 1748         -73.97914      N                                               NA
    ## 1749         -73.98185      N                                               NA
    ## 1750         -73.98185      N                                               NA
    ## 1751         -74.00535      N                                               NA
    ## 1752         -74.00535      N                                               NA
    ## 1753         -73.98035      N                                               NA
    ## 1754         -73.98035      N                                               NA
    ## 1755         -73.99635      D      N                                        NA
    ## 1756         -73.99635      D      N                                        NA
    ## 1757         -74.00174      D                                               NA
    ## 1758         -74.00174      D                                               NA
    ## 1759         -74.00174      D                                               NA
    ## 1760         -73.99817      D                                               NA
    ## 1761         -73.99817      D                                               NA
    ## 1762         -73.99817      D                                               NA
    ## 1763         -73.99817      D                                               NA
    ## 1764         -73.98683      D                                               NA
    ## 1765         -73.98683      D                                               NA
    ## 1766         -73.98683      D                                               NA
    ## 1767         -73.98683      D                                               NA
    ## 1768         -73.99479      D                                               NA
    ## 1769         -73.99479      D                                               NA
    ## 1770         -73.99479      D                                               NA
    ## 1771         -73.99479      D                                               NA
    ## 1772         -73.99548      D                                               NA
    ## 1773         -73.99548      D                                               NA
    ## 1774         -73.99548      D                                               NA
    ## 1775         -73.99689      D      N                                        NA
    ## 1776         -73.99689      D      N                                        NA
    ## 1777         -73.99689      D      N                                        NA
    ## 1778         -73.99886      D                                               NA
    ## 1779         -73.99886      D                                               NA
    ## 1780         -73.99886      D                                               NA
    ## 1781         -73.99886      D                                               NA
    ## 1782         -73.99886      D                                               NA
    ## 1783         -73.99886      D                                               NA
    ## 1784         -74.00061      D                                               NA
    ## 1785         -74.00061      D                                               NA
    ## 1786         -74.00061      D                                               NA
    ## 1787         -74.00061      D                                               NA
    ## 1788         -73.99432      D                                               NA
    ## 1789         -73.98377      D                                               NA
    ## 1790         -73.98377      D                                               NA
    ## 1791         -73.98377      D                                               NA
    ## 1792         -73.98377      D                                               NA
    ## 1793         -73.99373      D                                               NA
    ## 1794         -73.99373      D                                               NA
    ## 1795         -73.99373      D                                               NA
    ## 1796         -73.99373      D                                               NA
    ## 1797         -73.99430      D                                               NA
    ## 1798         -73.99430      D                                               NA
    ## 1799         -73.99430      D                                               NA
    ## 1800         -73.99430      D                                               NA
    ## 1801         -73.91776      2      5                                        NA
    ## 1802         -73.91776      2      5                                        NA
    ## 1803         -73.91776      2      5                                        NA
    ## 1804         -73.91776      2      5                                        NA
    ## 1805         -73.91776      2      5                                        NA
    ## 1806         -73.91776      2      5                                        NA
    ## 1807         -73.91776      2      5                                        NA
    ## 1808         -73.91776      2      5                                        NA
    ## 1809         -73.88773      2      5                                        NA
    ## 1810         -73.88773      2      5                                        NA
    ## 1811         -73.88773      2      5                                        NA
    ## 1812         -73.88773      2      5                                        NA
    ## 1813         -73.86263      2      5                                        NA
    ## 1814         -73.86263      2      5                                        NA
    ## 1815         -73.86034      2      5                                        NA
    ## 1816         -73.86034      2      5                                        NA
    ## 1817         -73.86034      2      5                                        NA
    ## 1818         -73.85747      2      5                                        NA
    ## 1819         -73.85747      2      5                                        NA
    ## 1820         -73.85438      2      5                                        NA
    ## 1821         -73.85438      2      5                                        NA
    ## 1822         -73.86735      2      5                                        NA
    ## 1823         -73.86735      2      5                                        NA
    ## 1824         -73.86846      2      5                                        NA
    ## 1825         -73.86846      2      5                                        NA
    ## 1826         -73.86846      2      5                                        NA
    ## 1827         -73.86716      2      5                                        NA
    ## 1828         -73.86716      2      5                                        NA
    ## 1829         -73.87349      2      5                                        NA
    ## 1830         -73.87349      2      5                                        NA
    ## 1831         -73.88005      2      5                                        NA
    ## 1832         -73.88005      2      5                                        NA
    ## 1833         -73.88005      2      5                                        NA
    ## 1834         -73.88005      2      5                                        NA
    ## 1835         -73.88005      2      5                                        NA
    ## 1836         -73.89186      2      5                                        NA
    ## 1837         -73.89186      2      5                                        NA
    ## 1838         -73.89186      2      5                                        NA
    ## 1839         -73.89186      2      5                                        NA
    ## 1840         -73.86626      2      5                                        NA
    ## 1841         -73.86626      2      5                                        NA
    ## 1842         -73.89674      2      5                                        NA
    ## 1843         -73.89674      2      5                                        NA
    ## 1844         -73.89674      2      5                                        NA
    ## 1845         -73.90781      2      5                                        NA
    ## 1846         -73.90781      2      5                                        NA
    ## 1847         -73.90781      2      5                                        NA
    ## 1848         -73.90781      2      5                                        NA
    ## 1849         -73.86762      2      5                                        NA
    ## 1850         -73.86762      2      5                                        NA
    ## 1851         -73.86762      2      5                                        NA
    ## 1852         -73.86762      2      5                                        NA
    ## 1853         -73.86762      2      5                                        NA
    ## 1854         -73.90177      2      5                                        NA
    ## 1855         -73.90177      2      5                                        NA
    ## 1856         -73.90177      2      5                                        NA
    ## 1857         -73.90177      2      5                                        NA
    ## 1858         -73.90177      2      5                                        NA
    ## 1859         -73.89306      2      5                                        NA
    ## 1860         -73.89306      2      5                                        NA
    ## 1861         -73.89306      2      5                                        NA
    ## 1862         -73.89306      2      5                                        NA
    ## 1863         -73.89306      2      5                                        NA
    ## 1864         -73.85062      2      5                                        NA
    ## 1865         -73.85062      2      5                                        NA
    ## 1866         -73.85062      2      5                                        NA
    ## 1867         -74.00191      7                                               NA
    ## 1868         -74.00191      7                                               NA
    ##      Route9 Route10 Route11 Entry Vending Entrance.Type   ADA
    ## 1        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 2        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 3        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 4        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 5        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 6        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 7        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 8        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 9        NA      NA      NA  TRUE     YES         Stair FALSE
    ## 10       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 11       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 12       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 13       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 14       NA      NA      NA FALSE      NO         Stair FALSE
    ## 15       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 16       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 17       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 18       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 19       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 20       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 21       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 22       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 23       NA      NA      NA FALSE      NO         Stair FALSE
    ## 24       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 25       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 26       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 27       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 28       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 29       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 30       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 31       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 32       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 33       NA      NA      NA  TRUE      NO         Stair FALSE
    ## 34        5      NA      NA  TRUE     YES      Elevator  TRUE
    ## 35       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 36       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 37       NA      NA      NA FALSE      NO         Stair FALSE
    ## 38       NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 39       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 40       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 41       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 42       NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 43       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 44        5      NA      NA  TRUE     YES         Stair  TRUE
    ## 45        5      NA      NA  TRUE     YES         Stair  TRUE
    ## 46       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 47       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 48       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 49       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 50       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 51       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 52       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 53       NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 54       NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 55       NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 56       NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 57       NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 58       NA      NA      NA FALSE      NO      Easement  TRUE
    ## 59       NA      NA      NA FALSE      NO         Stair  TRUE
    ## 60        2       3       7  TRUE     YES         Stair FALSE
    ## 61       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 62       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 63       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 64       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 65       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 66       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 67       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 68       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 69       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 70       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 71       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 72       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 73       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 74       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 75       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 76       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 77       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 78       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 79       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 80       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 81       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 82       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 83       NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 84       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 85       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 86       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 87       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 88       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 89       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 90       NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 91       NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 92       NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 93       NA      NA      NA  TRUE     YES      Easement FALSE
    ## 94       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 95       NA      NA      NA  TRUE     YES      Easement FALSE
    ## 96       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 97       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 98       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 99       NA      NA      NA  TRUE     YES         Stair FALSE
    ## 100      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 101      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 102      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 103      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 104      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 105      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 106      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 107      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 108      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 109      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 110      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 111      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 112      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 113      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 114      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 115      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 116      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 117      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 118      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 119      NA      NA      NA  TRUE     YES          Door FALSE
    ## 120      NA      NA      NA  TRUE     YES          Door FALSE
    ## 121      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 122      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 123      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 124      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 125      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 126      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 127      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 128      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 129      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 130      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 131      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 132      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 133      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 134      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 135      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 136      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 137      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 138      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 139      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 140      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 141      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 142      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 143      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 144      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 145      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 146      NA      NA      NA FALSE      NO         Stair FALSE
    ## 147      NA      NA      NA FALSE      NO         Stair FALSE
    ## 148      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 149      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 150      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 151      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 152      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 153      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 154      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 155      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 156      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 157      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 158      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 159      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 160      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 161      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 162      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 163      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 164      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 165      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 166      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 167      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 168      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 169      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 170      NA      NA      NA  TRUE     YES          Door FALSE
    ## 171      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 172      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 173      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 174      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 175      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 176      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 177      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 178      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 179      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 180      NA      NA      NA  TRUE     YES          Door FALSE
    ## 181      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 182      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 183      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 184      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 185      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 186      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 187      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 188      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 189      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 190      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 191      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 192      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 193      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 194      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 195      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 196      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 197      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 198      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 199      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 200      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 201      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 202      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 203      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 204      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 205      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 206      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 207      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 208      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 209      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 210      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 211      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 212      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 213      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 214      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 215      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 216      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 217      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 218      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 219      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 220      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 221      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 222      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 223      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 224      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 225      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 226      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 227      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 228      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 229      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 230      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 231      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 232      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 233      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 234      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 235      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 236      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 237      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 238      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 239      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 240      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 241      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 242      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 243      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 244      NA      NA      NA  TRUE     YES          Door FALSE
    ## 245      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 246      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 247      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 248      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 249      NA      NA      NA  TRUE     YES          Door FALSE
    ## 250      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 251      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 252      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 253      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 254      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 255      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 256      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 257      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 258      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 259      NA      NA      NA FALSE      NO         Stair FALSE
    ## 260      NA      NA      NA FALSE      NO         Stair FALSE
    ## 261      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 262      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 263      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 264      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 265      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 266      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 267      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 268      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 269      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 270      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 271      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 272      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 273      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 274      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 275      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 276      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 277      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 278       2       3       7  TRUE     YES      Easement  TRUE
    ## 279       2       3       7  TRUE     YES      Easement  TRUE
    ## 280       2       3       7  TRUE     YES         Stair  TRUE
    ## 281       2       3       7  TRUE     YES         Stair  TRUE
    ## 282       2       3       7  TRUE     YES         Stair  TRUE
    ## 283       2       3       7  TRUE     YES         Stair  TRUE
    ## 284       2       3       7  TRUE     YES         Stair  TRUE
    ## 285       2       3       7 FALSE      NO         Stair  TRUE
    ## 286       2       3       7 FALSE      NO         Stair  TRUE
    ## 287      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 288      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 289      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 290      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 291      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 292      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 293      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 294      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 295      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 296      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 297      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 298      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 299      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 300      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 301      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 302      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 303      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 304      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 305      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 306      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 307      NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 308      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 309      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 310      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 311      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 312      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 313      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 314      NA      NA      NA  TRUE     YES          Door FALSE
    ## 315      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 316      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 317      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 318      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 319      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 320      NA      NA      NA FALSE      NO         Stair FALSE
    ## 321      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 322      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 323      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 324      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 325      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 326      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 327      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 328      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 329      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 330      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 331      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 332      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 333      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 334      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 335      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 336      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 337      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 338      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 339      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 340      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 341      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 342      NA      NA      NA FALSE      NO         Stair FALSE
    ## 343      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 344      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 345      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 346      NA      NA      NA FALSE      NO         Stair FALSE
    ## 347      NA      NA      NA FALSE      NO         Stair FALSE
    ## 348      NA      NA      NA FALSE      NO         Stair FALSE
    ## 349      NA      NA      NA FALSE      NO         Stair FALSE
    ## 350      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 351      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 352      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 353      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 354      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 355      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 356      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 357      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 358      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 359      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 360      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 361      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 362      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 363      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 364      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 365      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 366      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 367      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 368      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 369      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 370      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 371      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 372      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 373      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 374      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 375      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 376      NA      NA      NA  TRUE     YES      Elevator FALSE
    ## 377      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 378      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 379      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 380      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 381      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 382      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 383      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 384      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 385      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 386      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 387      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 388      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 389      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 390      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 391      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 392      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 393      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 394      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 395      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 396      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 397      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 398      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 399      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 400      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 401      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 402      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 403      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 404      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 405      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 406      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 407      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 408      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 409      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 410      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 411      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 412      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 413      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 414      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 415       5      NA      NA  TRUE     YES      Easement  TRUE
    ## 416      NA      NA      NA  TRUE     YES          Door FALSE
    ## 417      NA      NA      NA FALSE      NO         Stair FALSE
    ## 418      NA      NA      NA  TRUE     YES          Door FALSE
    ## 419      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 420      NA      NA      NA FALSE      NO         Stair FALSE
    ## 421      NA      NA      NA  TRUE     YES          Door FALSE
    ## 422      NA      NA      NA FALSE      NO          Door FALSE
    ## 423      NA      NA      NA  TRUE     YES          Door FALSE
    ## 424      NA      NA      NA  TRUE     YES          Door FALSE
    ## 425      NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 426      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 427      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 428      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 429      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 430      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 431      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 432      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 433      NA      NA      NA  TRUE     YES          Door FALSE
    ## 434      NA      NA      NA  TRUE     YES          Door FALSE
    ## 435      NA      NA      NA  TRUE     YES          Door FALSE
    ## 436      NA      NA      NA  TRUE     YES          Door FALSE
    ## 437      NA      NA      NA  TRUE     YES          Door FALSE
    ## 438      NA      NA      NA  TRUE     YES          Door FALSE
    ## 439      NA      NA      NA  TRUE     YES          Door FALSE
    ## 440      NA      NA      NA  TRUE     YES          Door FALSE
    ## 441      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 442      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 443      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 444      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 445      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 446      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 447      NA      NA      NA  TRUE     YES          Door FALSE
    ## 448      NA      NA      NA FALSE      NO         Stair FALSE
    ## 449      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 450      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 451      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 452      NA      NA      NA  TRUE     YES          Door FALSE
    ## 453      NA      NA      NA  TRUE     YES          Door FALSE
    ## 454      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 455      NA      NA      NA  TRUE      NO          Ramp FALSE
    ## 456      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 457      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 458      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 459      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 460      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 461      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 462      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 463      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 464      NA      NA      NA FALSE      NO         Stair FALSE
    ## 465      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 466      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 467      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 468      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 469      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 470      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 471      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 472      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 473      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 474      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 475      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 476      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 477      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 478      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 479      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 480      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 481      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 482      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 483      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 484      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 485      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 486      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 487      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 488      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 489      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 490      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 491      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 492      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 493      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 494      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 495      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 496      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 497      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 498      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 499      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 500      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 501      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 502      NA      NA      NA FALSE      NO         Stair FALSE
    ## 503      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 504      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 505      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 506      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 507      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 508      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 509      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 510      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 511      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 512      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 513      NA      NA      NA FALSE      NO         Stair FALSE
    ## 514      NA      NA      NA FALSE      NO         Stair FALSE
    ## 515      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 516      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 517      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 518      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 519      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 520      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 521      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 522      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 523      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 524      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 525      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 526      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 527      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 528      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 529      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 530      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 531      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 532      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 533      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 534      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 535      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 536      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 537      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 538      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 539      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 540      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 541      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 542      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 543      NA      NA      NA FALSE      NO         Stair FALSE
    ## 544       2       3       7  TRUE     YES         Stair  TRUE
    ## 545       2       3       7  TRUE     YES         Stair  TRUE
    ## 546       2       3       7  TRUE     YES         Stair  TRUE
    ## 547       2       3       7  TRUE     YES         Stair  TRUE
    ## 548      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 549      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 550      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 551      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 552      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 553      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 554      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 555      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 556      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 557      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 558      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 559      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 560      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 561      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 562      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 563      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 564      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 565      NA      NA      NA FALSE      NO         Stair FALSE
    ## 566      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 567      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 568      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 569      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 570      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 571      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 572      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 573      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 574      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 575      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 576      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 577      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 578      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 579      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 580      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 581      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 582      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 583      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 584      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 585      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 586      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 587      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 588      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 589      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 590      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 591      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 592      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 593      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 594      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 595      NA      NA      NA FALSE      NO         Stair FALSE
    ## 596      NA      NA      NA FALSE      NO         Stair FALSE
    ## 597      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 598      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 599      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 600      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 601      NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 602      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 603      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 604      NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 605      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 606      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 607      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 608      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 609      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 610      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 611      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 612      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 613      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 614      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 615      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 616      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 617      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 618      NA      NA      NA FALSE      NO         Stair FALSE
    ## 619      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 620      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 621      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 622      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 623      NA      NA      NA FALSE      NO         Stair FALSE
    ## 624      NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 625      NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 626      NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 627      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 628      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 629      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 630      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 631      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 632      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 633      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 634      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 635      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 636      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 637      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 638      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 639      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 640      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 641      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 642      NA      NA      NA FALSE      NO         Stair FALSE
    ## 643      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 644      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 645      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 646      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 647      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 648      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 649      NA      NA      NA FALSE      NO         Stair FALSE
    ## 650      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 651      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 652      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 653      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 654      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 655      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 656      NA      NA      NA FALSE      NO         Stair FALSE
    ## 657      NA      NA      NA FALSE      NO         Stair FALSE
    ## 658      NA      NA      NA  TRUE     YES          Door FALSE
    ## 659      NA      NA      NA  TRUE     YES          Ramp FALSE
    ## 660      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 661      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 662      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 663      NA      NA      NA FALSE      NO         Stair FALSE
    ## 664      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 665      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 666      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 667      NA      NA      NA FALSE      NO         Stair FALSE
    ## 668      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 669      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 670      NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 671      NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 672      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 673      NA      NA      NA FALSE      NO         Stair FALSE
    ## 674      NA      NA      NA FALSE      NO         Stair FALSE
    ## 675      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 676      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 677      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 678      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 679      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 680      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 681      NA      NA      NA FALSE      NO         Stair FALSE
    ## 682      NA      NA      NA FALSE      NO         Stair FALSE
    ## 683      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 684      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 685      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 686      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 687      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 688      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 689      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 690      NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 691      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 692      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 693      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 694      NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 695      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 696      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 697      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 698      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 699      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 700      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 701      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 702      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 703      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 704      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 705      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 706      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 707      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 708      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 709      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 710      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 711      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 712      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 713      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 714      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 715      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 716      NA      NA      NA FALSE      NO          Door  TRUE
    ## 717      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 718      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 719      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 720      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 721      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 722      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 723      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 724      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 725      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 726      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 727      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 728      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 729      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 730      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 731      NA      NA      NA FALSE     YES          Door  TRUE
    ## 732      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 733      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 734      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 735      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 736      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 737      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 738      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 739      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 740      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 741      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 742      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 743      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 744      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 745      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 746      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 747      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 748      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 749      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 750      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 751      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 752      NA      NA      NA FALSE      NO         Stair FALSE
    ## 753      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 754      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 755      NA      NA      NA FALSE      NO         Stair FALSE
    ## 756      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 757      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 758      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 759      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 760      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 761      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 762      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 763      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 764      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 765      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 766      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 767      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 768      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 769      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 770      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 771      NA      NA      NA FALSE      NO         Stair FALSE
    ## 772      NA      NA      NA  TRUE      NO      Elevator  TRUE
    ## 773      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 774      NA      NA      NA  TRUE      NO     Escalator  TRUE
    ## 775      NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 776      NA      NA      NA FALSE      NO         Stair  TRUE
    ## 777       2       3       7  TRUE     YES      Easement  TRUE
    ## 778       2       3       7  TRUE     YES         Stair  TRUE
    ## 779       2       3       7  TRUE     YES      Easement  TRUE
    ## 780       2       3       7  TRUE     YES         Stair  TRUE
    ## 781       2       3       7  TRUE     YES         Stair  TRUE
    ## 782       2       3       7  TRUE     YES         Stair  TRUE
    ## 783       2       3       7  TRUE     YES      Easement  TRUE
    ## 784       2       3       7  TRUE     YES         Stair  TRUE
    ## 785       2       3       7  TRUE     YES         Stair  TRUE
    ## 786      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 787      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 788      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 789      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 790      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 791      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 792      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 793      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 794      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 795      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 796      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 797      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 798      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 799      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 800      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 801      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 802      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 803      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 804      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 805      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 806      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 807      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 808      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 809      NA      NA      NA  TRUE     YES          Door FALSE
    ## 810      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 811      NA      NA      NA  TRUE      NO          Door  TRUE
    ## 812      NA      NA      NA  TRUE      NO       Walkway  TRUE
    ## 813      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 814      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 815      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 816      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 817      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 818      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 819      NA      NA      NA FALSE      NO         Stair FALSE
    ## 820      NA      NA      NA FALSE      NO         Stair FALSE
    ## 821      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 822      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 823      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 824      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 825      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 826      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 827      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 828      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 829      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 830      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 831      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 832      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 833      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 834      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 835      NA      NA      NA FALSE      NO         Stair FALSE
    ## 836      NA      NA      NA FALSE      NO         Stair FALSE
    ## 837      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 838      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 839      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 840      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 841      NA      NA      NA FALSE      NO         Stair FALSE
    ## 842      NA      NA      NA FALSE      NO         Stair FALSE
    ## 843      NA      NA      NA  TRUE     YES          Door FALSE
    ## 844      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 845      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 846      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 847      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 848      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 849      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 850      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 851      NA      NA      NA FALSE      NO          Door FALSE
    ## 852      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 853      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 854      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 855      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 856      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 857      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 858      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 859      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 860      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 861      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 862      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 863      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 864      NA      NA      NA  TRUE     YES          Door FALSE
    ## 865      NA      NA      NA  TRUE     YES          Door FALSE
    ## 866      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 867      NA      NA      NA FALSE      NO      Easement FALSE
    ## 868      NA      NA      NA FALSE      NO      Easement FALSE
    ## 869      NA      NA      NA FALSE      NO      Easement FALSE
    ## 870      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 871      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 872      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 873      NA      NA      NA  TRUE     YES      Easement FALSE
    ## 874      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 875      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 876      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 877      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 878      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 879      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 880      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 881      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 882      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 883      NA      NA      NA FALSE      NO         Stair FALSE
    ## 884      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 885      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 886      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 887      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 888      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 889      NA      NA      NA  TRUE     YES          Door FALSE
    ## 890      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 891      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 892      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 893      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 894      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 895      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 896      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 897      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 898      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 899      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 900      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 901      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 902      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 903      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 904      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 905      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 906      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 907      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 908      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 909      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 910      NA      NA      NA  TRUE     YES          Door FALSE
    ## 911      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 912      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 913      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 914      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 915      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 916      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 917      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 918      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 919      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 920      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 921      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 922      NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 923      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 924      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 925      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 926      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 927      NA      NA      NA  TRUE     YES          Door  TRUE
    ## 928      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 929      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 930      NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 931      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 932      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 933      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 934      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 935      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 936      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 937      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 938      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 939      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 940      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 941      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 942      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 943      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 944      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 945      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 946      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 947      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 948      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 949      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 950      NA      NA      NA FALSE      NO         Stair FALSE
    ## 951      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 952      NA      NA      NA FALSE      NO         Stair FALSE
    ## 953      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 954      NA      NA      NA FALSE      NO         Stair FALSE
    ## 955      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 956      NA      NA      NA FALSE      NO         Stair FALSE
    ## 957      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 958      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 959      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 960      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 961      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 962      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 963      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 964      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 965      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 966      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 967      NA      NA      NA FALSE      NO      Easement FALSE
    ## 968      NA      NA      NA FALSE      NO         Stair FALSE
    ## 969      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 970      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 971      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 972      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 973      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 974      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 975      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 976      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 977      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 978      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 979      NA      NA      NA  TRUE      NO         Stair FALSE
    ## 980      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 981      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 982      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 983      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 984      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 985      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 986      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 987      NA      NA      NA FALSE      NO         Stair FALSE
    ## 988      NA      NA      NA FALSE      NO         Stair FALSE
    ## 989      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 990      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 991      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 992      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 993      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 994      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 995      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 996      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 997      NA      NA      NA FALSE      NO         Stair FALSE
    ## 998      NA      NA      NA FALSE      NO         Stair FALSE
    ## 999      NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1000     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1001     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1002     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1003     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1004     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1005     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1006     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1007     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1008     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1009     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1010     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1011     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1012     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1013     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1014     NA      NA      NA FALSE      NO          Door FALSE
    ## 1015     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1016     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1017     NA      NA      NA FALSE      NO      Easement FALSE
    ## 1018     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1019     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1020     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1021      5      NA      NA  TRUE     YES      Easement  TRUE
    ## 1022     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1023     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1024     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1025     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1026     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1027     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1028     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1029     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1030     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1031     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1032     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1033     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1034     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1035     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1036     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1037     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1038     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1039     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1040     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1041     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1042     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1043     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1044     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1045     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1046     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1047     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1048     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1049     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1050     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1051     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1052     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1053     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1054     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1055     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1056     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1057     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1058     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1059     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1060     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1061     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1062     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1063     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1064     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1065     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1066     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1067     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1068     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1069     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1070     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1071     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1072     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1073     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1074     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1075     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1076     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1077     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1078     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1079     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1080     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1081     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1082     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1083     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1084     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1085     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1086     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1087     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1088     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1089     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1090     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1091     NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 1092     NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 1093     NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 1094     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1095     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1096     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1097     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1098     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1099     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1100     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1101     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1102     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1103     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1104     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1105     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1106     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1107     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1108     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1109     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1110     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1111     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1112     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1113     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1114     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1115     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1116     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1117     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1118     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1119     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1120     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1121     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1122     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1123     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1124     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1125     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1126     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1127     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1128     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1129     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1130     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1131     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1132     NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 1133     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1134     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1135     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1136     NA      NA      NA  TRUE     YES          Door  TRUE
    ## 1137     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1138     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1139     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1140     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1141     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1142     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1143     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1144     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1145     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1146     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1147     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1148     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1149     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1150     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1151     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1152     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1153     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1154     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1155     NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 1156     NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 1157     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1158     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1159     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1160     NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 1161     NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 1162     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1163     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1164     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1165     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1166     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1167     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1168     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1169     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1170     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1171     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1172     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1173     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1174     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1175     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1176     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1177     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1178     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1179     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1180     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1181     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1182     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1183     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1184     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1185     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1186     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1187     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1188     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1189     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1190     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1191     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1192     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1193     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1194     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1195     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1196     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1197     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1198     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1199     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1200     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1201     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1202     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1203     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1204     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1205     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1206     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1207     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1208     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1209     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1210     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1211     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1212     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1213     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1214     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1215     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1216     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1217     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1218     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1219     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1220     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1221     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1222     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1223     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1224     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1225     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1226     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1227     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1228     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1229     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1230     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1231     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1232     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1233     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1234     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1235     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1236     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1237     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1238     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1239     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1240     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1241     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1242     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1243     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1244     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1245     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1246     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1247     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1248     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1249     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1250     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1251     NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 1252     NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 1253     NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 1254     NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 1255     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1256     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1257     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1258     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1259     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1260     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1261     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1262     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1263     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1264     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1265     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1266     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1267     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1268     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1269     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1270     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1271     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1272     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1273     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1274     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1275     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1276     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1277     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1278     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1279     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1280     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1281     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1282     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1283     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1284     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1285     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1286     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1287     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1288     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1289     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1290     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1291     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1292     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1293     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1294     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1295     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1296     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1297     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1298     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1299     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1300     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1301     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1302     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1303     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1304     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1305     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1306     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1307     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1308     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1309     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1310     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1311     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1312     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1313     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1314     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1315     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1316     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1317     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1318     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1319     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1320     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1321     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1322     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1323     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1324     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1325     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1326     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1327     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1328     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1329     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1330     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1331     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1332     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1333     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1334     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1335     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1336     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1337     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1338     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1339     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1340     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1341     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1342     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1343     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1344     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1345     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1346     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1347     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1348     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1349     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1350     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1351     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1352     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1353     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1354     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1355     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1356     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1357     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1358     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1359     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1360     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1361     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1362     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1363     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1364     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1365     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1366     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1367     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1368     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1369     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1370     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1371     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1372     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1373     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1374     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1375     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1376     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1377     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1378     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1379     NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 1380     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1381     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1382     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1383     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1384     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1385     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1386     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1387     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1388     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1389     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1390     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1391     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1392     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1393     NA      NA      NA FALSE      NO      Elevator  TRUE
    ## 1394     NA      NA      NA FALSE      NO         Stair  TRUE
    ## 1395     NA      NA      NA FALSE      NO         Stair  TRUE
    ## 1396     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1397     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1398     NA      NA      NA FALSE      NO      Easement FALSE
    ## 1399     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1400     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1401     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1402     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1403     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1404     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1405     NA      NA      NA FALSE      NO      Easement  TRUE
    ## 1406     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1407     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1408     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1409     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1410     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1411     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1412     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1413     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1414     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1415     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1416     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1417     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1418     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1419     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1420     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1421     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1422     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1423     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1424     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1425     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1426     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1427     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1428     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1429     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1430     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1431     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1432     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1433     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1434     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1435     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1436     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1437     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1438     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1439     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1440     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1441     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1442     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1443     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1444     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1445     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1446     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1447     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1448     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1449     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1450     NA      NA      NA  TRUE     YES          Door  TRUE
    ## 1451     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1452     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1453     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1454     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1455     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1456     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1457     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1458     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1459     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1460     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1461     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1462     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1463     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1464     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1465     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1466     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1467     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1468     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1469     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1470     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1471     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1472     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1473     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1474     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1475     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1476     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1477     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1478     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1479     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1480     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1481     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1482     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1483     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1484     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1485     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1486     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1487     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1488     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1489     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1490     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1491     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1492     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1493     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1494     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1495     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1496     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1497     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1498     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1499     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1500     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1501     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1502     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1503     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1504     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1505     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1506     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1507     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1508     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1509     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1510     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1511     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1512     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1513     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1514     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1515     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1516     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1517     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1518     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1519     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1520     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1521     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1522     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1523     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1524     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1525     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1526     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1527     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1528     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1529     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1530     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1531     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1532     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1533     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1534     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1535     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1536     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1537     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1538     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1539     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1540     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1541     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1542     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1543     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1544     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1545     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1546     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1547     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1548     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1549     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1550     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1551     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1552     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1553     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1554     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1555     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1556     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1557     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1558     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1559     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1560     NA      NA      NA  TRUE     YES          Door  TRUE
    ## 1561     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1562     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1563     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1564     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1565     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1566     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1567     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1568     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1569     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1570     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1571     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1572     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1573     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1574     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1575     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1576     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1577     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1578     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1579     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1580     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1581     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1582     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1583     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1584     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1585     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1586     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1587     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1588     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1589     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1590     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1591     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1592     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1593     NA      NA      NA  TRUE     YES      Easement FALSE
    ## 1594     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1595     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1596     NA      NA      NA  TRUE      NO      Easement FALSE
    ## 1597     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1598     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1599     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1600     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1601     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1602     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1603     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1604     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1605     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1606     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1607     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1608     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1609     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1610     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1611     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1612     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1613     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1614     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1615     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1616     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1617     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1618     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1619     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1620     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1621     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1622     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1623     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1624     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1625     NA      NA      NA  TRUE      NO         Stair FALSE
    ## 1626     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1627     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1628     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1629     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1630     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1631     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1632     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1633     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1634     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1635     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1636     NA      NA      NA  TRUE     YES          Door  TRUE
    ## 1637     NA      NA      NA  TRUE     YES          Door  TRUE
    ## 1638     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1639     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1640     NA      NA      NA FALSE      NO         Stair  TRUE
    ## 1641     NA      NA      NA FALSE      NO         Stair  TRUE
    ## 1642     NA      NA      NA FALSE      NO         Stair  TRUE
    ## 1643     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1644     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1645     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1646     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1647     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1648     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1649     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1650     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1651     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1652     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1653     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1654     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1655     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1656     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1657     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1658     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1659     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1660     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1661     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1662     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1663     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1664     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1665     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1666     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1667     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1668     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1669     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1670     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1671     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1672     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1673     NA      NA      NA  TRUE     YES      Easement  TRUE
    ## 1674     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1675     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1676     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1677     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1678     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1679     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1680     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1681     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1682     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1683     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1684     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1685     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1686     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1687     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1688     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1689     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1690     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1691     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1692     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1693     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1694     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1695     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1696     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1697     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1698     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1699     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1700     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1701     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1702     NA      NA      NA FALSE      NO          Ramp FALSE
    ## 1703     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1704     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1705     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1706     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1707     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1708     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1709     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1710     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1711     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1712     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1713     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1714     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1715     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1716     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1717     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1718     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1719     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1720     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1721     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1722     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1723     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1724     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1725     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1726     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1727     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1728     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1729     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1730     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1731     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1732     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1733     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1734     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1735     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1736     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1737     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1738     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1739     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1740     NA      NA      NA  TRUE      NO         Stair  TRUE
    ## 1741     NA      NA      NA  TRUE     YES          Door  TRUE
    ## 1742     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1743     NA      NA      NA  TRUE      NO          Door FALSE
    ## 1744     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1745     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1746     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1747     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1748     NA      NA      NA  TRUE      NO          Door FALSE
    ## 1749     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1750     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1751     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1752     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1753     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1754     NA      NA      NA  TRUE      NO          Door FALSE
    ## 1755     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1756     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1757     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1758     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1759     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1760     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1761     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1762     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1763     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1764     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1765     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1766     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1767     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1768     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1769     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1770     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1771     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1772     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1773     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1774     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1775     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1776     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1777     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1778     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1779     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1780     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1781     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1782     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1783     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1784     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1785     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1786     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1787     NA      NA      NA FALSE      NO         Stair FALSE
    ## 1788     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1789     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1790     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1791     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1792     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1793     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1794     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1795     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1796     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1797     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1798     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1799     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1800     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1801     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1802     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1803     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1804     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1805     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1806     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1807     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1808     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1809     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1810     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1811     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1812     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1813     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1814     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1815     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1816     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1817     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1818     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1819     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1820     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1821     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1822     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1823     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1824     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1825     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1826     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1827     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1828     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1829     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1830     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1831     NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 1832     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1833     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1834     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1835     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1836     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1837     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1838     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1839     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1840     NA      NA      NA  TRUE     YES          Door FALSE
    ## 1841     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1842     NA      NA      NA  TRUE     YES     Escalator FALSE
    ## 1843     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1844     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1845     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1846     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1847     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1848     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1849     NA      NA      NA  TRUE     YES     Escalator  TRUE
    ## 1850     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1851     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1852     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1853     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1854     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1855     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1856     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1857     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1858     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1859     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1860     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1861     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1862     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1863     NA      NA      NA  TRUE     YES         Stair  TRUE
    ## 1864     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1865     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1866     NA      NA      NA  TRUE     YES         Stair FALSE
    ## 1867     NA      NA      NA  TRUE     YES      Elevator  TRUE
    ## 1868     NA      NA      NA  TRUE     YES         Stair  TRUE

The dataset provides details about NYC subway entrances and exits. Key
variables include:Line (Subway line), Station Name(Name of the subway
station), Station Latitude/Longitude(Geographical coordinates of the
station), Routes Served (Route1 to Route11), Entry(Logical variable
indicating whether entry is allowed at a specific entrance),
Vending(Indicates whether there are vending machines at the
entrance),Entrance Type(e.g. staircase, elevator), ADA(Whether the
entrance is ADA-compliant)

So far, I have selected relevant columns from the original data set and
converted the Entry variable into a logical format. The resulting
dataset has 1868 rows and 19 columns. This dataset can be considered
tidy, as each variable has its own column, each observation has its own
row, and each value has its own cell.

``` r
# Count distinct stations based on the combination of station name and line
distinct_stations <- cleaned_data %>%
  distinct(Line,Station.Name) %>%
  nrow()

distinct_stations
```

    ## [1] 465

There are 465 stations.

``` r
# Count ADA compliant stations
ada_compliant_stations <- cleaned_data %>%
  filter(ADA == TRUE) %>%
  nrow()

ada_compliant_stations
```

    ## [1] 468

There are 468 stations.

``` r
# Calculate the proportion of entrances without vending that allow entry
proportion_without_vending <- cleaned_data %>%
  filter(Vending == "NO") %>%
  summarise(proportion = mean(Entry))

proportion_without_vending
```

    ##   proportion
    ## 1  0.3770492

The proportion of entrances without vending is approximately 37.7%.

``` r
library(tidyr)
   cleaned_data <- cleaned_data %>%
     mutate(across(Route1:Route11, as.character))
   
# Transfer data to long character format
routes_data <- cleaned_data %>%
  pivot_longer(Route1:Route11, names_to = "RouteNumber", values_to = "Route")
routes_data <- routes_data %>%
  filter(!is.na(Route))

# Check the reformatted data
routes_data
```

    ## # A tibble: 13,198 × 10
    ##    Line     Station.Name Station.Latitude Station.Longitude Entry Vending
    ##    <chr>    <chr>                   <dbl>             <dbl> <lgl> <chr>  
    ##  1 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  2 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  3 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  4 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  5 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  6 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  7 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  8 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ##  9 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ## 10 4 Avenue 25th St                  40.7             -74.0 TRUE  YES    
    ## # ℹ 13,188 more rows
    ## # ℹ 4 more variables: Entrance.Type <chr>, ADA <lgl>, RouteNumber <chr>,
    ## #   Route <chr>

``` r
# Count distinct stations serving the A train
a_train_stations <- routes_data %>%
  filter(Route == "A") %>%
  distinct(Line, Station.Name) %>%
  nrow()

a_train_stations
```

    ## [1] 60

There are 60 distinct stations serving the A train,

``` r
# Count ADA compliant stations serving the A train
ada_compliant_a_train <- routes_data %>%
  filter(Route == "A", ADA == TRUE) %>%
  distinct(Line, Station.Name) %>%
  nrow()

ada_compliant_a_train
```

    ## [1] 17

There 17 ADA compliant stations serving the A train.

Problem 2

``` r
# Load necessary libraries
library(readxl)

# Read Mr. Trash Wheel sheet
mr_trash_wheel <- read_excel("202409 Trash Wheel Collection Data.xlsx",sheet = "Mr. Trash Wheel", skip = 1)
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
# Clean the data by selecting relevant columns and renaming them
mr_trash_wheel_cleaned <- mr_trash_wheel %>%
 select(Dumpster, Month, Year, Date, 'Weight.tons'= 'Weight (tons)', 'Volume.cubicyards' =     'Volume (cubic yards)', PlasticBottles ='Plastic Bottles' , Polystyrene, CigaretteButts = 'Cigarette Butts', GlassBottles = 'Glass Bottles', PlasticBags = 'Plastic Bags', Wrappers, SportsBalls = 'Sports Balls' ,HomesPowered = 'Homes Powered*', fifteen ='...15', sixteen='...16') %>%
  
  # omit rows that do not include dumpster-specific data
  filter(!is.na(Dumpster)) %>%
  # round the number of sports balls to the nearest integer and converts the result to an    integer variable
  mutate(SportsBalls = as.integer(round(SportsBalls))) %>%
  
  # add an additional variable to keep track of Trash Wheel
  mutate(Trash_Wheel = "Mr. Trash Wheel") %>%
  # transfer year to numerical character
  mutate(Year = as.numeric(Year))

 mr_trash_wheel_cleaned
```

    ## # A tibble: 651 × 17
    ##    Dumpster Month  Year Date                Weight.tons Volume.cubicyards
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>             <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31                18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74                13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45                15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                 15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06                18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71                13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                 8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                 16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52                14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76                18
    ## # ℹ 641 more rows
    ## # ℹ 11 more variables: PlasticBottles <dbl>, Polystyrene <dbl>,
    ## #   CigaretteButts <dbl>, GlassBottles <dbl>, PlasticBags <dbl>,
    ## #   Wrappers <dbl>, SportsBalls <int>, HomesPowered <dbl>, fifteen <lgl>,
    ## #   sixteen <lgl>, Trash_Wheel <chr>

``` r
# Read Professor Trash Wheel sheet
prof_trash_wheel <- read_excel("202409 Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", skip = 1)

# Clean
prof_trash_wheel_cleaned <- prof_trash_wheel %>%
  select(Dumpster, Month, Year, Date, 'Weight.tons'= 'Weight (tons)', 'Volume.cubicyards' =     'Volume (cubic yards)', PlasticBottles ='Plastic Bottles' , Polystyrene, CigaretteButts = 'Cigarette Butts', GlassBottles = 'Glass Bottles', PlasticBags = 'Plastic Bags', Wrappers, HomesPowered = 'Homes Powered*') %>%
  
# omit rows that do not include dumpster-specific data
  filter(!is.na(Dumpster)) %>%
  
# add an additional variable to keep track of Trash Wheel
  mutate(Trash_Wheel = "Professor Trash Wheel")

# Repeat for Gwynnda
gwynnda <- read_excel("202409 Trash Wheel Collection Data.xlsx", sheet = "Gwynnda Trash Wheel", skip = 1)

gwynnda_cleaned <- gwynnda %>%
  select(Dumpster, Month, Year, Date, 'Weight.tons'= 'Weight (tons)', 'Volume.cubicyards' =     'Volume (cubic yards)', PlasticBottles ='Plastic Bottles' , Polystyrene, CigaretteButts = 'Cigarette Butts', PlasticBags = 'Plastic Bags', Wrappers, HomesPowered = 'Homes Powered*') %>%
  
filter(!is.na(Dumpster)) %>%

# add an additional variable to keep track of Trash Wheel
  mutate(Trash_Wheel = "Gwynnda Trash Wheel")

# Combine all data set
all_trash_wheel_data <- bind_rows(mr_trash_wheel_cleaned,
                                  prof_trash_wheel_cleaned,
                                  gwynnda_cleaned)

# Check the combined data set
all_trash_wheel_data
```

    ## # A tibble: 1,033 × 17
    ##    Dumpster Month  Year Date                Weight.tons Volume.cubicyards
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>             <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31                18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74                13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45                15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                 15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06                18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71                13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                 8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                 16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52                14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76                18
    ## # ℹ 1,023 more rows
    ## # ℹ 11 more variables: PlasticBottles <dbl>, Polystyrene <dbl>,
    ## #   CigaretteButts <dbl>, GlassBottles <dbl>, PlasticBags <dbl>,
    ## #   Wrappers <dbl>, SportsBalls <int>, HomesPowered <dbl>, fifteen <lgl>,
    ## #   sixteen <lgl>, Trash_Wheel <chr>

The `all_trash_wheel_data` combines the data from three different trash
wheels of Healthy Harbor company from the year 2014 to 2024, if data is
available. The data includes the number of Dumpster, Month, Year, Date,
weight, volumes, different categories of trash collected, and homes
powered.

``` r
# Total weight collected by Professor Trash Wheel
total_weight_professor <- all_trash_wheel_data %>%
  filter(Trash_Wheel == "Professor Trash Wheel") %>%
  summarise(total_weight = sum(Weight.tons, na.rm = TRUE)) %>%
print()
```

    ## # A tibble: 1 × 1
    ##   total_weight
    ##          <dbl>
    ## 1         247.

The total weight collected by Professor Trash Wheel is 246.74 tons.

``` r
# Cigarette butts collected by Gwynnda in June 2022
gwynnda_cigarette_june <- all_trash_wheel_data %>%
  filter(Trash_Wheel == "Gwynnda Trash Wheel", Month == "June", Year == 2022) %>%
  summarise(total_cig_butts = sum(CigaretteButts, na.rm = TRUE))%>%
print()
```

    ## # A tibble: 1 × 1
    ##   total_cig_butts
    ##             <dbl>
    ## 1           18120

The cigarette butts collected by Gwynnda in June 2022 is 18120.

Problem 3

``` r
# Load necessary libraries
library(readr)
library(stringr)

# Read the CSV files
bakers_df <- read_csv("bakers.csv")
```

    ## Rows: 120 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker Name, Baker Occupation, Hometown
    ## dbl (2): Series, Baker Age
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bakes_df <- read_csv("bakes.csv")
```

    ## Rows: 548 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker, Signature Bake, Show Stopper
    ## dbl (2): Series, Episode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
results_df <- read_csv("results.csv")
```

    ## New names:
    ## Rows: 1138 Columns: 5
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (5): ...1, ...2, ...3, ...4, IN = stayed in; OUT = Eliminated; STAR BAKE...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`

``` r
# make the data tidy
results_df <- results_df %>%
  slice(3:n())  %>% #keep the data from the third row
  setNames(c("Series", "Episode", "Baker", "Technical", "Result")) 

# make the data consistent
bakes_df <- bakes_df %>%
  mutate(Series = as.numeric(Series), Episode = as.numeric(Episode))
  
results_df <- results_df %>%
  mutate(Series = as.numeric(Series), Episode = as.numeric(Episode))

bakers_df <- bakers_df %>%
  rename(Baker = `Baker Name`) %>%
  mutate(Series = as.numeric(Series)) %>%
  mutate(Baker = ifelse(Baker == 'Joanne', 'Jo', Baker)) 

bakers_df
```

    ## # A tibble: 120 × 5
    ##    Baker            Series `Baker Age` `Baker Occupation`           Hometown    
    ##    <chr>             <dbl>       <dbl> <chr>                        <chr>       
    ##  1 Ali Imdad             4          25 Charity worker               Saltley, Bi…
    ##  2 Alice Fevronia       10          28 Geography teacher            Essex       
    ##  3 Alvin Magallanes      6          37 Nurse                        Bracknell, …
    ##  4 Amelia LeBruin       10          24 Fashion designer             Halifax     
    ##  5 Andrew Smyth          7          25 Aerospace engineer           Derby / Hol…
    ##  6 Annetha Mills         1          30 Midwife                      Essex       
    ##  7 Antony Amourdoux      9          30 Banker                       London      
    ##  8 Beca Lyne-Pirkis      4          31 Military Wives' Choir Singer Aldershot, …
    ##  9 Ben Frazer            2          31 Graphic Designer             Northampton 
    ## 10 Benjamina Ebuehi      7          23 Teaching assistant           South London
    ## # ℹ 110 more rows

``` r
bakes_df
```

    ## # A tibble: 548 × 5
    ##    Series Episode Baker     `Signature Bake`                      `Show Stopper`
    ##     <dbl>   <dbl> <chr>     <chr>                                 <chr>         
    ##  1      1       1 Annetha   "Light Jamaican Black Cakewith Straw… Red, White & …
    ##  2      1       1 David     "Chocolate Orange Cake"               Black Forest …
    ##  3      1       1 Edd       "Caramel Cinnamon and Banana Cake"    N/A           
    ##  4      1       1 Jasminder "Fresh Mango and Passion Fruit Hummi… N/A           
    ##  5      1       1 Jonathan  "Carrot Cake with Lime and Cream Che… Three Tiered …
    ##  6      1       1 Lea       "Cranberry and Pistachio Cakewith Or… Raspberries a…
    ##  7      1       1 Louise    "Carrot and Orange Cake"              Never Fail Ch…
    ##  8      1       1 Mark      "Sticky Marmalade Tea Loaf"           Heart-shaped …
    ##  9      1       1 Miranda   "Triple Layered Brownie Meringue Cak… Three Tiered …
    ## 10      1       1 Ruth      "Three Tiered Lemon Drizzle Cakewith… Classic Choco…
    ## # ℹ 538 more rows

``` r
results_df
```

    ## # A tibble: 1,136 × 5
    ##    Series Episode Baker     Technical Result
    ##     <dbl>   <dbl> <chr>     <chr>     <chr> 
    ##  1      1       1 Annetha   2         IN    
    ##  2      1       1 David     3         IN    
    ##  3      1       1 Edd       1         IN    
    ##  4      1       1 Jasminder <NA>      IN    
    ##  5      1       1 Jonathan  9         IN    
    ##  6      1       1 Louise    <NA>      IN    
    ##  7      1       1 Miranda   8         IN    
    ##  8      1       1 Ruth      <NA>      IN    
    ##  9      1       1 Lea       10        OUT   
    ## 10      1       1 Mark      <NA>      OUT   
    ## # ℹ 1,126 more rows

``` r
# Check the completeness and correctness
bakers_df <- bakers_df %>%
  mutate(Baker = word (Baker, 1))
```

``` r
# Check the not matching one

resultbakernumber<- anti_join(results_df, bakers_df, by = c("Series", "Baker")) %>%
  nrow()

print(resultbakernumber)
```

    ## [1] 8

``` r
bakerresultnumber <- anti_join(bakers_df, results_df, by = c("Series", "Baker")) %>%
  nrow()

print(bakerresultnumber)
```

    ## [1] 1

``` r
# Merge the bakers and the results
bakers_results <- bakers_df %>%
  left_join(results_df, by = c("Series", "Baker"))

# before merging the bakes and the baker results, checking the not match ones
mb_number <- anti_join(bakers_results, bakes_df, by = c("Series", "Episode", "Baker")) %>%
  nrow()

print(mb_number)
```

    ## [1] 589

``` r
bm_number <- anti_join(bakes_df, bakers_results, by = c("Series", "Episode", "Baker")) %>%
  nrow()

print(bm_number)
```

    ## [1] 8

``` r
# Then merge a final data set

final_data <- bakers_results %>%
  left_join(bakes_df, by = c("Series", "Episode", "Baker")) %>%
  select(Series,Episode,Baker, "Baker Age", "Baker Occupation", Hometown, 
         "Signature Bake", "Show Stopper", Technical, Result)
  
final_data
```

    ## # A tibble: 1,129 × 10
    ##    Series Episode Baker `Baker Age` `Baker Occupation` Hometown `Signature Bake`
    ##     <dbl>   <dbl> <chr>       <dbl> <chr>              <chr>    <chr>           
    ##  1      4       1 Ali            25 Charity worker     Saltley… Rose and Pistac…
    ##  2      4       2 Ali            25 Charity worker     Saltley… Italian Grissini
    ##  3      4       3 Ali            25 Charity worker     Saltley… Coconut, Raspbe…
    ##  4      4       4 Ali            25 Charity worker     Saltley… Apple and Ginge…
    ##  5      4       5 Ali            25 Charity worker     Saltley… <NA>            
    ##  6      4       6 Ali            25 Charity worker     Saltley… <NA>            
    ##  7      4       7 Ali            25 Charity worker     Saltley… <NA>            
    ##  8      4       8 Ali            25 Charity worker     Saltley… <NA>            
    ##  9      4       9 Ali            25 Charity worker     Saltley… <NA>            
    ## 10      4      10 Ali            25 Charity worker     Saltley… <NA>            
    ## # ℹ 1,119 more rows
    ## # ℹ 3 more variables: `Show Stopper` <chr>, Technical <chr>, Result <chr>

``` r
# Export the final data set as a CVS
   write.csv(final_data, "final_data.csv", row.names = FALSE)
```

During the data cleaning process, I set new column names and removed the
first two rows of information in result_df. Then, I transformed the
types of the data in these sets to make them consistent. The final_data
shows all the bakers who have attended the Great British Bake Off.
Through this data set, we can easily get the information of these
bakers, such as their series, episode, names,age,occupations, hometown,
signature bake, show stoppers, technical and results.

``` r
# find the winner
starbakers_winners <- final_data %>%
  filter(Series %in% c("5", "6", "7", "8", "9", "10") & 
         Result %in% c("STAR BAKER", "WINNER")) %>%
  select(Series,Episode,Baker, "Baker Age", "Baker Occupation", Hometown, 
         "Signature Bake", "Show Stopper", Technical, Result)

winners <- starbakers_winners %>%
  filter(Result == "WINNER")

winners
```

    ## # A tibble: 6 × 10
    ##   Series Episode Baker  `Baker Age` `Baker Occupation` Hometown `Signature Bake`
    ##    <dbl>   <dbl> <chr>        <dbl> <chr>              <chr>    <chr>           
    ## 1      7      10 Candi…          31 PE teacher         Barton-… Queen Victoria'…
    ## 2     10      10 David           36 International hea… Whitby   <NA>            
    ## 3      6      10 Nadiya          30 Full-time mother   Leeds /… Cardamom and Al…
    ## 4      5      10 Nancy           60 Retired Practice … Barton-… Apple and Lemon…
    ## 5      9      10 Rahul           30 Research scientist Rotherh… <NA>            
    ## 6      8      10 Sophie          33 Former army offic… West Mo… Spelt Boules, M…
    ## # ℹ 3 more variables: `Show Stopper` <chr>, Technical <chr>, Result <chr>

Most of the winners are in their 30s, expect for Nancy. Additionally,
all of them have up to 2 technical.

``` r
# 10 rows of viewers and calculate the average
viewers <- read_csv("viewers.csv", col_types = cols('Series 1' = col_number(), 
    'Series 5' = col_number()))

head(viewers, 10)
```

    ## # A tibble: 10 × 11
    ##    Episode `Series 1` `Series 2` `Series 3` `Series 4` `Series 5` `Series 6`
    ##      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
    ##  1       1       2.24       3.1        3.85       6.6        8.51       11.6
    ##  2       2       3          3.53       4.6        6.65       8.79       11.6
    ##  3       3       3          3.82       4.53       7.17       9.28       12.0
    ##  4       4       2.6        3.6        4.71       6.82      10.2        12.4
    ##  5       5       3.03       3.83       4.61       6.95       9.95       12.4
    ##  6       6       2.75       4.25       4.82       7.32      10.1        12  
    ##  7       7      NA          4.42       5.1        7.76      10.3        12.4
    ##  8       8      NA          5.06       5.35       7.41       9.02       11.1
    ##  9       9      NA         NA          5.7        7.41      10.7        12.6
    ## 10      10      NA         NA          6.74       9.45      13.5        15.0
    ## # ℹ 4 more variables: `Series 7` <dbl>, `Series 8` <dbl>, `Series 9` <dbl>,
    ## #   `Series 10` <dbl>

``` r
mean_series1 <- mean(viewers$"Series 1", na.rm = TRUE)
mean_series5 <- mean(viewers$"Series 5", na.rm = TRUE)

print(mean_series1)
```

    ## [1] 2.77

``` r
print(mean_series5)
```

    ## [1] 10.0393

The average viewership in Series 1 is 2.77, and for Series 5 is 10,0393.
