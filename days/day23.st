String extend [
    toPuterId
    [ ^(self inject: 0 into: [:a :c | a * 26 + c asciiValue - 97]) + 1 ]
]
String class extend [
    fromPuterId: x
    [ | d2 d1 t |
      t := x - 1.
      d1 := t // 26.
      d2 := t \\ 26.
      ^String with: (Character asciiValue: d1 + 97)
              with: (Character asciiValue: d2 + 97) ]
]

Object subclass: Puter [
    | id allEdges forwardEdges |
    forwardEdges [^forwardEdges]
    id: i [id := i]

    initialize [allEdges := Set new. forwardEdges := Set new]

    addEdge: o
    [ allEdges add: o.
      id < o ifTrue: [forwardEdges add: o]. ]
]
Puter class extend [
    isChiefHistorians: id [ ^(id - 1 // 26) = 19 "= $t" ]
]

Object subclass: Network [
    | puters biggestParty currentParty |

    puters [^puters]
    initialize
    [ puters := 1 to: 26 * 26 collect: [:id | Puter new id: id] ]
    edges: edges
    [ edges do: [
        :e | (e tokenize: '-') letArrayInBlock: [
            :lhs :rhs | | li ri |
            li := lhs toPuterId. ri := rhs toPuterId.
            (puters at: li) addEdge: ri.
            (puters at: ri) addEdge: li.
        ]]]

    findTriangles
    [ | coll | coll := OrderedCollection new.
      puters keysAndValuesDo: [
          :c1 :p1 |
          p1 forwardEdges do: [
              :c2 | | p2 | p2 := puters at: c2.
              p1 forwardEdges & p2 forwardEdges do: [
                  :c3 | coll add: {c1. c2. c3}]]].
      ^coll ]

    findBiggestParty
    [ currentParty := OrderedCollection new.
      biggestParty := {}.
      puters keysAndValuesDo: [
          :c1 :p1 |
          currentParty add: c1.
          [ self biggestPartyFromSet: p1 forwardEdges
          ] ensure: [ currentParty removeLast ]].
      ^biggestParty ]

    biggestPartyFromSet: set
    [ set do: [
        :cn | | pn | pn := puters at: cn.
        currentParty add: cn.
        [ currentParty size > biggestParty size ifTrue: [
              biggestParty := currentParty asArray ].
          self biggestPartyFromSet: set & pn forwardEdges
        ] ensure: [ currentParty removeLast ]]]

    visualize
    [ AOC visualizeWithExt: '.dot' do: [
        :io |
        io << 'graph{'.
        puters keysAndValuesDo: [
            :c1 :p1 | | n1 | n1 := String fromPuterId: c1.
            p1 forwardEdges do: [
                :c2 | | n2 | n2 := String fromPuterId: c2.
                io << n1 << '--' << n2 << ';'.]].
        io << '}'; nl ]]
]

AOC input: [ Network new edges: stdin lines ];
    part1: [ :net | net chain findTriangles;
                  select: [:it | it anySatisfy: [
                               :c | Puter isChiefHistorians: c]];
                  size ];
    part2: [ :net | net chain findBiggestParty;
                  collect: [:it | String fromPuterId: it];
                  join: ',' ];
    result: [ :net :part | part value: net ];
    finish.
