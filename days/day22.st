Object subclass: Monkey [
    | history |
    history: h [history := h]
    lastValue [^history at: history size]

    collectPricesInto: intoTbl
    [ | ch table prices deltas chMod |
      table := Dictionary new.
      prices := history collect: [:it | it \\ 10].
      deltas := 2 to: history size collect: [
                        :i | (prices at: i) - (prices at: i - 1) + 9].
      ch := 0.
      1 to: 4 do: [
              :i | ch := 20 * ch + (deltas at: i)].
      chMod := 20 raisedTo: 4.
      5 to: deltas size do: [
          :i | ch := (20 * ch + (deltas at: i)) \\ chMod.
          table at: ch ifAbsentPut: [prices at: i + 1]].
      table keysAndValuesDo: [
          :ch :v | intoTbl at: ch put: v + (intoTbl at: ch ifAbsent: [0])]]
]

Monkey class extend [
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

AOC input: [ stdin toLines collect: [
               :it | Monkey stepSecret: it asNumber times: 2000] ];
    part1: [ :monkeys | (monkeys collect: [:it | it lastValue]) sum ];
    part2: [ :monkeys | | buyMap | buyMap := Dictionary new.
             monkeys do: [:it | it collectPricesInto: buyMap].
             buyMap values maximum ];
    result: [ :monkeys :part | part value: monkeys ];
    finish.
