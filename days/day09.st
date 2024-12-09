Object subclass: DriveBase [
    | ids |
    ids [^ids]
    diskSize [^ids size]
    idAt: i [^ids at: i]

    isEmpty: i [^(ids at: i ifAbsent: [1]) isNil]
    isFull: i [^(ids at: i ifAbsent: [nil]) isNil not]

    addBlock: id length: len [ len timesRepeat: [ids add: id] ]
    addFreeLength: len [ len timesRepeat: [ids add: nil] ]

    mappingPos [^ids size]
    map: diskMap
    [ | id toPut len |
      ids := OrderedCollection new.
      id := 0. toPut := id.
      diskMap do: [
          :c | len := c digitValue.
          toPut
              ifNil: [self addFreeLength: len.
                      id := id + 1.
                      toPut := id]
              ifNotNil: [self addBlock: id length: len.
                         toPut := nil] ].
      ids := ids asArray ]

    checksum
    [ ^((1 to: self diskSize) collect:
            [ :i | (self idAt: i) ifNil: [0] ifNotNil: [:x | (i - 1) * x] ])
           sum ]

    compress
    [ | end | end := ids size.
      [ self isEmpty: end ] whileTrue: [ end := end - 1 ].
      ids := (1 to: end) collect: [ :i | ids at: i ].
      ^ids ]

    printOn: st
    [ (1 to: self diskSize) do:
          [ :i | (self idAt: i) ifNil: [st << '. '] ifNotNil: [:id | st << id << ' '] ] ]
]

DriveBase subclass: DriveP1 [
    compact
    [ | start end |
      start := 1. end := ids size.
      [ true ] whileTrue: [
          [ self isFull: start ] whileTrue: [ start := start + 1 ].
          [ self isEmpty: end ] whileTrue: [ end := end - 1 ].
          end > start ifFalse: [^self compress].
          ids at: start put: (ids at: end).
          ids at: end put: nil ]]
]

Link subclass: Block [
    | pos len |
    pos: p [pos:=p]
    pos [^pos]
    len: n [len:=n]
    len [^len]

    fillFree: by [pos := pos + by. len := len - by]
    isEmpty [^len = 0]

    printOn: st [st << '#[' << len << ']@' << pos]
]

DriveBase subclass: DriveP2 [
    | freeList idMap lastId |
    map: m
    [ freeList := LinkedList new.
      idMap := Dictionary new.
      super map: m ]

    newBlock: len [^Block new pos: self mappingPos; len: len]

    addBlock: id length: len
    [ | block | block := self newBlock: len.
      idMap at: id put: block.
      lastId := id.
      super addBlock: id length: len ]
    addFreeLength: len
    [ freeList add: (self newBlock: len).
      super addFreeLength: len ]

    compact
    [ (lastId to: 0 by: -1) do: [:id | self compactBlock: id].
      self compress ]

    moveBlock: block to: free
    [ (1 to: block len) do:
          [ :off |
            ids at: free pos + off put: (ids at: block pos + off).
            ids at: block pos + off put: nil.
            "Do not create a free space where the block was moved from
            -- all of the blocks that will be moved are on the left"
          ].
      free fillFree: block len.
      free isEmpty ifTrue: [freeList remove: free] ]

    compactBlock: id
    [ | block | block := idMap at: id.
      freeList do: [
          :free |
          free pos > block pos ifTrue: [^self].
          free len >= block len ifTrue: [
              self moveBlock: block to: free.
              ^self
          ]]]
]

AOC input: [ stdin nextLine ];
    part1: DriveP1;
    part2: DriveP2;
    result: [ :map :Drive | Drive new map: map; compact; checksum ];
    finish.
