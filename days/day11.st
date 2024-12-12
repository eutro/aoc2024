Object subclass: RockSet [
    | rocks | rocks [^rocks] rocks: n [rocks:=n asBag]

    size [^rocks size]
    blinkTimes: n [ n timesRepeat: [self blink] ]

    blink
    [ | oldRocks |
      oldRocks := rocks.
      rocks := Bag new.
      oldRocks sortedByCount do: [
          :it | self splitRock: it value multiplicity: it key ]]

    addRock: num times: count
    [ rocks add: num withOccurrences: count ]

    splitRock: num multiplicity: count
    [ | ndigits |
      num = 0 ifTrue: [^self addRock: 1 times: count].
      ndigits := (num + 1) ceilingLog: 10.
      ndigits even ifTrue: [
          | p10 |
          p10 := 10 raisedTo: ndigits // 2.
          self addRock: num // p10 times: count.
          self addRock: num \\ p10 times: count.
          ^nil ].
      self addRock: num * 2024 times: count ]

    printOn: st [st << rocks]
]

AOC input: [ RockSet new rocks:
                 ((stdin nextLine tokenize: ' ')
                      collect: [:it | it asNumber]) ];
    part1: 25; part2: 50 "(cumulative)";
    result: [ :rocks :blinks |
              rocks blinkTimes: blinks.
              rocks size. ];
    finish.
