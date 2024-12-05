Dictionary extend [
    at: key inA: Type [^self at: key ifAbsentPut: [Type new]]
    at: key inA: Type add: elt [ (self at: key inA: Type) add: elt ]
]

Object subclass: Ordering [
    | beforeMap |
    initialize [beforeMap := Dictionary new]

    graphviz "Very elucidating!"
    [ AOC visualizeWithExt: '.dot' do: [
              :stream |
              stream << 'digraph {'.
              beforeMap keysAndValuesDo: [
                  :k :vs | vs do: [
                      :v | stream << k << ' -> ' << v << ';'. ]].
              stream << '}'.
          ]]

    order: page1 before: page2
    [ beforeMap at: page2 inA: Set add: page1 ]
    addRule: ruleStr
    [ ruleStr scanf: '%d|%d' with: [ :x :y | self order: x before: y ] ]

    isValid: man
    [ | mustNotOccur | mustNotOccur := Set new.
      man do: [
          :elt |
          (mustNotOccur includes: elt) ifTrue: [^false].
          mustNotOccur addAll: (beforeMap at: elt inA: Set). ].
      ^true ]

    sort: man
    [ ^man asSortedCollection: [ :x :y | (beforeMap at: y) includes: x ] ]
]

Collection extend [
    middleElement [ ^self at: self size // 2 + 1 ]
]

AOC input: [ (stdin contents tokenize: (String with: $<10> with: $<10>))
                 letArrayInBlock: [
               :rules :seqs | | ord mans |
               ord := Ordering new.
               rules lines do: [ :rule | ord addRule: rule ].
               mans := seqs lines collect: [
                   :seq | seq chain tokenize: ','; collect: [:x | x asNumber] ].
               {ord . mans} ] ];
    part1: [ :ord :mans | mans select: [ :man | ord isValid: man ] ];
    part2: [ :ord :mans | mans chain reject: [ :man | ord isValid: man ];
                  collect: [ :man | ord sort: man ] ];
    result: [ :ordAndMans :part |
              part chain valueWithArguments: ordAndMans;
                          collect: [:man | man middleElement];
                          sum ];
    finish.
