Object subclass: AOC []
AOC class extend [
    | savedP1 savedP2 mapper getArg |
    part1: p1Block [ savedP1 := p1Block ]
    part2: p2Block [ savedP2 := p2Block ]
    map: mapBlock [ mapper := mapBlock ]
    arg: argBlock [ getArg := argBlock ]

    output: value part: part
    [ FileStream stdout << 'Part '; << part; << ': '; << value; nl. ]

    runPart: saved part: part arg: arg
    [ saved ifNotNil: [
        | mapped |
        mapped := mapper value: arg value: saved.
        self output: mapped part: part ] ]

    run
    [ | arg |
      arg := getArg value.
      self runPart: savedP1 part: 1 arg: arg.
      self runPart: savedP2 part: 2 arg: arg. ]

    "TODO: save as an image?"
    finish [ self run ]
]

Iterable extend [
    sum [ ^self fold: [ :l :r | l + r ] ]
    toList
    [ | list |
      list := OrderedCollection new.
      self do: [:x | list add: x].
      ^list ]
]

Object subclass: ScannerState [
    | scanner oc dispatch failed |
    init: sc
    [ scanner := sc.
      oc := OrderedCollection new.
      dispatch := nil.
      failed := false. ]

    failed [ ^failed ]

    accept: c on: stream
    [ dispatch ifNotNil:
          [ :dp | dispatch := nil. dp value: c value: stream ] ifNil:
          [ (c = $%) ifTrue:
                [ dispatch := [ :c :stream | self dispatch: c on: stream ] ]
                ifFalse: [ self require: c on: stream ] ] ]

    dispatch: c on: stream [ (scanner dispatchFor: c) value: self value: stream ]
    require: c on: stream
    [ c isSeparator ifTrue:
          [ stream whilePeek: [ :c | c isSeparator ] do: [ :ignored | nil ] ] ifFalse:
          [ failed := (stream peekFor: c) not ] ]
    parseCharOn: stream [ oc add: stream next ]
    badCmd: c [ self error: 'Bad command' ]
    finish [ ^oc asArray ]

    parseIntOn: stream
    [ | d |
      d := 0.
      stream whilePeek: [ :c | c isDigit ] do:
             [ :c | d := d * 10 + c digitValue ].
      oc add: d ]

    parseStringOn: stream
    [ | s | s := OrderedCollection new.
      stream whilePeek: [ :c | c isSeparator not ] do:
             [ :c | s add: c ].
      oc add: s asString ]

    parseCharsetOn: stream [ dispatch := [ :c :stream | self parseCharset: c on: stream set: Set new ] ]
    parseCharset: c on: stream set: set
    [ (c = $^) ifTrue:
          [ dispatch := [
              :c :stream |
              self parseCharset: c on: stream inv: true set: set ] ] ifFalse:
          [ self parseCharset: c on: stream inv: false set: set ] ]
    parseCharset: c on: stream inv: inv set: set
    [ (c = $]) ifTrue:
          [ self parseCharsetOn: stream inv: inv set: set ] ifFalse:
          [ set add: c.
            dispatch := [
              :c :stream |
              self parseCharset: c on: stream inv: inv set: set ] ] ]
    parseCharsetOn: stream inv: inv set: set
    [ | s | s := OrderedCollection new.
      stream whilePeek: [ :c | inv ~= (set includes: c) ] do:
             [ :c | s add: c ].
      oc add: s asString ]
]
ScannerState class extend [
    new: sc [ |r| r := super new. r init: sc. ^r ]
]
Object subclass: Scanner [
    | dispatch |
    init
    [ dispatch := Dictionary new.
      dispatch at: $% put: [ :s :st | s require: $% on: st ].
      dispatch at: $d put: [ :s :st | s parseIntOn: st ].
      dispatch at: $c put: [ :s :st | s parseCharOn: st ].
      dispatch at: $s put: [ :s :st | s parseStringOn: st ].
      dispatch at: $[ put: [ :s :st | s parseCharsetOn: st ]. ]

    dispatchFor: c [ ^dispatch at: c ifAbsent: [[ :s :st | s badCmd: c ]] ]

    scanf: fmt on: stream
    [ | s |
      s := ScannerState new: self.
      fmt do: [ :c | s accept: c on: stream.
                s failed ifTrue: [ ^nil ] ].
      ^s finish ]
]
Scanner class extend [
    | defaultsc |
    default [ ^defaultsc ]
    default: it [ defaultsc := it ]
    new [ |r| r := super new. r init. ^r ]
]
Scanner default: Scanner new.

Object extend [
    memfn: selector [ DirectedMessage receiver: self selector: selector ]
]

Stream extend [
    toLines [ ^self lines toList ]

    scanf: fmt [ ^Scanner default scanf: fmt on: self ]
    scanf: fmt with: block
    [ ^(self scanf: fmt) ifNotNil: [ :it | block valueWithArguments: it ] ]

    whilePeek: pred do: block
    [ [ self peek ifNil: [ false ]
                  ifNotNil: [ :it | pred value: it ] ]
          whileTrue: [ block value: self next ] ]
]

String extend [
    scanf: fmt [ ^(ReadStream on: self) scanf: fmt ]
    scanf: fmt with: block [ ^(ReadStream on: self) scanf: fmt with: block ]
]
