Object subclass: Monkey [
    | history |
    history: h [history := h]
    lastValue [^history at: history size]

    collectPricesInto: intoTbl seen: seenTbl
    [ | ch prices deltas key newV prevSeen localBest |
      prices := history collect: [:it | it \\ 10].
      deltas := 2 to: history size collect: [
                    :i | (prices at: i) - (prices at: i - 1) + 9].
      ch := 0. "past four deltas as a base-19 integer"
      1 to: 4 do: [:i | ch := 19 * ch + (deltas at: i)].
      "Accumulate the best value globally, (local variable as a
       'compiler' optimisation)."
      localBest := Monkey bestBananas.
      5 to: deltas size do: [
          :i | ch := (19 * ch + (deltas at: i)) \\ 130321.
          "ch is theoretically in the range [0,19^4),
           so add 1 to keep it in array range"
          key := ch + 1.
          (seenTbl at: key) == false ifTrue: [
              "Store a linked list of seen values within the table,
               to make resetting it easier."
              seenTbl at: key put: prevSeen.
              prevSeen := key.
              newV := (prices at: i + 1) + (intoTbl at: key).
              intoTbl at: key put: newV.
              newV > localBest ifTrue: [localBest := newV].
          ]].
      Monkey bestBananas: localBest.
      "Reset the seen table via the linked list."
      [prevSeen notNil] whileTrue: [
          | tmp | tmp := seenTbl at: prevSeen.
          seenTbl at: prevSeen put: false.
          prevSeen := tmp. ]]
]

Monkey class extend [
    | bestV |
    bestBananas: v [bestV := v]
    bestBananas [^bestV]

    initialize [bestV := 0]

    keyDigits [^19]
    keyMax [^130321 "19 raisedTo: 4"]

    stepSecret: initial times: n
    [ | s hist | s := initial.
      hist := 1 to: n collect: [
          :i |
          s := (s bitXor: (s bitShift: 6)) bitAnd: 16777215.
          s := (s bitXor: (s bitShift: -5)) bitAnd: 16777215.
          s := (s bitXor: (s bitShift: 11)) bitAnd: 16777215.
          s ].
      ^Monkey new history: {initial},hist ]
]
Monkey initialize.

AOC input: [ stdin toLines collect: [
               :it | Monkey stepSecret: it asNumber times: 2000] ];
    part1: [ :monkeys | (monkeys collect: [:it | it lastValue]) sum ];
    part2: [ :monkeys | | buyMap seenMap |
             buyMap := Array new: Monkey keyMax withAll: 0.
             seenMap := Array new: Monkey keyMax withAll: false.
             monkeys do: [:it | it collectPricesInto: buyMap seen: seenMap].
             Monkey bestBananas ];
    result: [ :monkeys :part | part value: monkeys ];
    finish.
