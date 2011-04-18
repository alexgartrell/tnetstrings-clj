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

## Getting tnetstrings

tnetstrings-clj is most conveniently available from the clojars.org
repository and can be added mosty easily by adding the following to your
`project.clj` file

    :dependencies [[tnetstrings/tnetstrings "0.1.0"]]


Copyright (C) 2011 Alex Gartrell
