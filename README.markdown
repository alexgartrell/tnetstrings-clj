# tnetstrings-clj 
### native tnetstrings in clojure

see: http://www.tnetstrings.org

## Usage

    user=> (tnetstrings.core/dumps "Hello, World!")
    "13:Hello, World!,"

    user=> (tnetstrings.core/loads "13:Hello, World!,")
    "Hello, World!"

    user=> (tnetstrings.core/dumps (list "Hello" "World" "!!!" 111))
    "28:5:Hello,5:World,3:!!!,3:111#]"

    user=> (tnetstrings.core/loads "28:5:Hello,5:World,3:!!!,3:111#]")
    ("Hello" "World" "!!!" 111)

Copyright (C) 2011 Alex Gartrell
