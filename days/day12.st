Object subclass: UnionFind [
    | root size |
    initialize [ root := self. size := self initSize. ]
    size [^size]
    addSize: n [size := size + n]

    initSize [^1]

    find
    [ ^root == self
        ifTrue: [self]
        ifFalse: [root := root find. root] ]

    union: other
    [ | myroot itroot |
      myroot := self find. itroot := other find.
      myroot == itroot ifTrue: [^myroot].
      ^myroot mergeWith: itroot ]

    mergeWith: other
    [ | child parent |
      size >= other size
        ifTrue: [child := other. parent := self]
        ifFalse: [parent := other. child := self].
      child mergeInto: parent.
      ^parent ]

    mergeInto: parent
    [ root := parent.
      parent addSize: size.
      size := 0 ]
]

UnionFind subclass: Region [
    | regionSet perimeter corners |

    initialize [super initialize. perimeter := 0. corners := 0.]
    regionSet [^regionSet]
    regionSet: n [regionSet:=n]

    area [^size]
    addPerimeter: n [perimeter := perimeter + n]
    addCorners: n [corners := corners + n]

    mergeInto: parent
    [ super mergeInto: parent.
      regionSet ifNotNil: [:it | it remove: self].
      parent addCorners: corners.
      parent addPerimeter: perimeter ]

    fenceCost1 [^self area * perimeter]
    fenceCost2 [^self area * corners]
]

Object subclass: GardenPlot [
    | plantGrid regionGrid regionSet dirs dirsRot1 |

    plants: g
    [ dirs := {Posn up. Posn left. Posn down. Posn right}.
      dirsRot1 := {Posn left. Posn down. Posn right. Posn up}.
      plantGrid := g.
      regionSet := Set new.
      regionGrid := plantGrid collect: [
          :ign | | reg |
          reg := Region new regionSet: regionSet.
          regionSet add: reg.
          reg ].
    ]

    combineRegions [ plantGrid allPosnsDo: [:pos | self combineAt: pos] ]

    combineAt: pos
    [ | plant region |
      plant := plantGrid at: pos.
      region := regionGrid at: pos.
      dirs with: dirsRot1 do: [
          :off :off2 | | offPos oPlant same1 same2 same12 |
          offPos := pos + off.
          oPlant := plantGrid at: offPos.
          same1 := oPlant = plant.
          same2 := (plantGrid at: pos + off2) = plant.
          same12 := (plantGrid at: pos + off + off2) = plant.

          same1
              ifTrue: [ region := region union: (regionGrid at: offPos) ]
              ifFalse: [ region find addPerimeter: 1 ].

          ((same1 not & same2 not "outer corner") |
           (same1 & same2 & same12 not "inner corner"))
              ifTrue: [ region find addCorners: 1 ]
          ]
    ]

    totalFenceCost: block
    [ ^regionSet inject: 0 into: [:acc :it | acc + (block value: it)] ]
]

AOC input: [ | plants | plants := Grid new rows: stdin toLines asArray.
             GardenPlot new plants: plants; combineRegions ];
    part1: [:it | it fenceCost1];
    part2: [:it | it fenceCost2];
    result: [ :plot :cost | plot totalFenceCost: cost ];
    finish.
