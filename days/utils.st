"--- AOC helper class ---"
Object subclass: AOC []
AOC class extend [
    | savedParts mapper getInput actionDict |

    initialize
    [ savedParts := Array new: 2.
      getInput := OrderedCollection new. ]

    input: inpBlock [ getInput add: inpBlock ]
    part1: partBlock [ savedParts at: 1 put: partBlock ]
    part2: partBlock [ savedParts at: 2 put: partBlock ]
    partN: partBlock [ savedParts := savedParts,{partBlock} ]
    result: mapBlock [ mapper := mapBlock ]

    output: value part: part [ stdout << 'Part ' << part << ': ' << value; nl; flush. ]

    runPart: saved part: part args: args
    [ saved ifNotNil: [
        | mapped |
        mapped := mapper valueWithArguments: args,{saved}.
        self output: mapped part: part ] ]

    run
    [ | inps | inps := getInput asArray collect: [:it | it value].
      savedParts keysAndValuesDo: [
          :part :saved | self runPart: saved part: part args: inps ]]

    action: name do: block
    [ actionDict ifNil: [actionDict := Dictionary new].
      actionDict at: name put: block ]

    "TODO: save as an image?"
    finish
    [ | startTime stopTime action |
      action := (Smalltalk getenv: 'AOC_RUN') ifNil: [
          stdout << 'AOC_RUN not set -- not running.'.
          ^nil ].
      action := actionDict ifNotNil: [ :ad | ad at: action ifAbsent: [nil] ].
      action ifNotNil: [ ^action value ].

      startTime := Time millisecondClock.
      self run.
      stopTime := Time millisecondClock.
      stdout << 'Time elapsed: ' << (stopTime - startTime) << 'ms'; nl. ]

    visualizeWithExt: ext do: block
    [ | file io |
      file := (Smalltalk getenv: 'AOC_VIS') ifNil: [^nil].
      file := (file,ext) asFile.
      file printNl.
      file parent createDirectories.
      io := file writeStream. [block value: io] ensure: [io close] ]
]
AOC initialize.

"--- Scanf ---"
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
    [ | d sf c |
      d := 0. sf := 1. c := stream peek.
      (c = $+) | (c = $-) ifTrue: [
          stream next.
          c = $- ifTrue: [sf := -1]
      ].
      stream whilePeek: [ :c | c isDigit ] do:
             [ :c | d := d * 10 + c digitValue ].
      oc add: (sf * d) ]

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
    initialize
    [ dispatch := Dictionary new.
      dispatch at: $% put: [ :s :st | s require: $% on: st ].
      dispatch at: $d put: [ :s :st | s parseIntOn: st ].
      dispatch at: $c put: [ :s :st | s parseCharOn: st ].
      dispatch at: $s put: [ :s :st | s parseStringOn: st ].
      dispatch at: $[ put: [ :s :st | s parseCharsetOn: st ]. ]

    dispatchFor: c [ ^dispatch at: c ifAbsent: [[ :s :st | s badCmd: c ]] ]

    scanf: fmt on: stream
    [ | s | s := ScannerState new: self.
      fmt do: [ :c | s accept: c on: stream.
                s failed ifTrue: [ ^nil ] ].
      ^s finish ]
]
Scanner class extend [
    | defaultsc |
    default [ ^defaultsc ]
    default: it [ defaultsc := it ]
]
Scanner default: Scanner new.

"--- Extensions ---"
Object extend [
    memfn: selector [ ^DirectedMessage receiver: self selector: selector ]
]

Iterable extend [
    sum [ ^self inject: 0 into: [ :l :r | l + r ] ]
    minimum [ ^self inject: FloatD infinity into: [ :l :r | l min: r ] ]
    maximum [ ^self inject: FloatD negativeInfinity into: [ :l :r | l max: r ] ]

    toList [ ^self toCollection: OrderedCollection ]
    toBag [ ^self toCollection: Bag ]
    toSet [ ^self toCollection: Set ]

    toCollection: clazz [ ^self intoCollection: clazz new ]
    intoCollection: acc [ self do: [:x | acc add: x]. ^acc ]
]

Set extend [
    flip: key
    [ (self includes: key)
          ifTrue: [self remove: key. ^true]
          ifFalse: [self add: key. ^false] ]
]

Iterable extend [
    findFirstElt: block
    [ self do: [:v | (block value: v) ifTrue: [^v]]. ^nil ]
    findFirstElt: block ifAbsent: abs
    [ self do: [:v | (block value: v) ifTrue: [^v]]. ^abs value ]
]

SequenceableCollection extend [
    findFirstElt: block [ ^self at: (self findFirst: block) ]
    findFirstElt: block ifAbsent: abs
    [ ^self at: (self findFirst: block) ifAbsent: abs ]
]

Collection extend [
    transposed
    [ ^(self size <= 0) ifTrue:
           [ #() ] ifFalse:
           [ | selfArr | selfArr := self asArray.
             1 to: (self at: 1) size collect: [
                 :i | selfArr collect: [ :elt | elt at: i ifAbsent: [nil] ] ] ] ]
]

Stream extend [
    toLines [ ^self lines toList ]

    scanf: fmt [ ^Scanner default scanf: fmt on: self ]
    scanf: fmt with: block
    [ ^(self scanf: fmt) ifNotNil: [ :it | block valueWithArguments: it ] ]

    whilePeek: pred do: block
    [ [ self peek ifNil: [ false ] ifNotNil: [ :it | pred value: it ] ]
          whileTrue: [ block value: self next ] ]
]

CharacterArray extend [
    scanf: fmt [ ^(ReadStream on: self) scanf: fmt ]
    scanf: fmt with: block [ ^(ReadStream on: self) scanf: fmt with: block ]

    trim [ ^self copyReplacingRegex: '\s*(.*)\s*' asRegex with: [:m | m at: 1] ]
    trimLines [ ^(self lines collect: [:it | it trim]) join: ' ' ]

    splitWords [ ^self tokenize: ' ' ]
    splitDoubleNl [ ^self tokenize: String doubleNl ]
]
CharacterArray class extend [
    doubleNl [ ^self with: $<10> with: $<10> ]
]

BlockClosure extend [
    asSpreader [ ^[ :args | self valueWithArguments: args ] ]
]

Array extend [
    letArrayInBlock: block [ ^block valueWithArguments: self ]
]

"Pipeline proxy class which forwards onto a boxed value"
nil subclass: MessageChain [
    | val |
    pipelineValue: v [ val := v ]
    yourself [ ^val ]
    doesNotUnderstand: msg [ val := msg reinvokeFor: val. ^val ]
    >* block [ ^block value: val ]
    MessageChain class >> new [ ^self basicNew ]
]
Object extend [ chain [ ^MessageChain new pipelineValue: self ] ]

Object subclass: Posn [
    | x y | x [^x] y [^y] x: n [x:=n] y: n [y:=n]
    x: nx y: ny [x := nx. y := ny]

    + o [ ^Posn x: x + o x y: y + o y ]
    += o [ self x: x + o x; y: y + o y. ]
    - o [ ^Posn x: x - o x y: y - o y ]
    -= o [ self x: x - o x; y: y - o y. ]
    * n [ ^Posn x: x * n y: y * n ]

    rotateCw [ ^Posn x: y y: x negated ]
    rotateCcw [ ^Posn x: y negated y: x ]
    rotate180 [ ^Posn x: x negated y: y negated ]

    < o [ ^(x < o x) & (y < o y) ]
    <= o [ ^(x <= o x) & (y <= o y) ]
    > o [ ^(x > o x) & (y > o y) ]
    >= o [ ^(x >= o x) & (y >= o y) ]
    = o [ ^(x = o x) & (y = o y) ]
    hash [ ^{x. y} hash ]

    printOn: s [ s << '<' << x << ', ' << y << '>'. ]
]
Posn class extend [
    x: x y: y [ ^self new x: x; y: y ]
    up: n [ ^Posn x: 0 y: n negated ]
    down: n [ ^Posn x: 0 y: n ]
    left: n [ ^Posn x: n negated y: 0 ]
    right: n [ ^Posn x: n y: 0 ]
    up [^Posn up: 1]
    down [^Posn down: 1]
    left [^Posn left: 1]
    right [^Posn right: 1]
    zero [^Posn x: 0 y: 0]
]

Object subclass: Mat2 [
    | m11 m12
      m21 m22 |
    m11: n11 m12: n12 m21: n21 m22: n22 [m11:=n11. m12:=n12. m21:=n21. m22:=n22]

    determinant [ ^(m11 * m22) - (m12 * m21) ]
    inverse
    [ | det r rn | det := self determinant.
      det = det zero ifTrue: [^nil].
      r := det reciprocal.
      rn := r negated.
      ^Mat2 m11: r * m22 m12: rn * m12
            m21: rn * m21 m22: r * m11 ]

    *> p [ ^Posn x: (m11 * p x) + (m12 * p y)
                 y: (m21 * p x) + (m22 * p y) ]

    printOn: st [ st << ('{ %1, %2 ; %3, %4 }' % {m11. m12. m21. m22}) ]
]
Mat2 class extend [
    m11: n11 m12: n12 m21: n21 m22: n22
    [ ^self new m11: n11 m12: n12 m21: n21 m22: n22 ]

    col: a col: b
    [ ^self m11: a x m12: b x
            m21: a y m22: b y ]

    row: a row: b
    [ ^self m11: a x m12: a y
            m21: b x m22: b y ]
]

Array extend [ asPosn [ ^Posn x: (self at: 1) y: (self at: 2) ] ]
Iterable extend [ asPosns [ ^self collect: [:it | it asPosn] ] ]

Object subclass: Grid [
    | rows |
    rows: n [rows:=n]
    rows [^rows]

    at: pos [ ^(rows at: pos y ifAbsent: [^nil]) at: pos x ifAbsent: [^nil] ]
    at: pos put: elt
    [ ^(rows at: pos y ifAbsent: [self error: 'Out of bounds'])
           at: pos x put: elt ]

    isInBounds: posn
    [ posn > Posn zero ifFalse: [^false].
      ^posn <= (Posn x: self width y: self height) ]

    height [^rows size] width [^(rows at: 1) size]

    allPosnsDo: block
    [ 1 to: self height do:
          [ :y | 1 to: self width do:
               [ :x | block value: (Posn x: x y: y) ] ] ]
    allPosnsOf: elt
    [ | oc | oc := OrderedCollection new.
      self allPosnsDo: [ :p | elt = (self at: p) ifTrue: [ oc add: p ] ].
      ^oc ]

    collect: block
    [ ^Grid width: self width height: self height initWith: [
               :p | block value: (self at: p) ] ]

    keysAndValuesCollect: block
    [ ^Grid width: self width height: self height initWith: [
               :p | block value: p value: (self at: p) ] ]

    printOn: st [ rows do: [ :r | st << r; nl ] ]

    postCopy [ rows := rows collect: [:it | it copy] ]
]
Grid class extend [
    width: w height: h initWith: block
    [ | rows |
      rows := 1 to: h collect: [
          :y | 1 to: w collect: [
              :x | block value: (Posn x: x y: y)]].
      ^self new rows: rows ]
]

Dictionary extend [
    at: key inA: Type [^self at: key ifAbsentPut: [Type new]]
    at: key inA: Type add: elt [ (self at: key inA: Type) add: elt ]
]

Integer extend [
    ceilingDiv: denom [
        "Answer (self / denom) ceiling."
        ^((self - 1) // denom) + 1
    ]

    "Original code was incorrect:"
    ceilingLog: radix [
        "Answer (self log: radix) ceiling. Optimized to answer an integer."

        <category: 'math methods'>
        | me answer |
        self < self zero ifTrue:
             [^self arithmeticError: 'cannot extract logarithm of a negative number'].
        radix <= radix unity ifTrue: 
              [radix <= radix zero 
                     ifTrue: [^self arithmeticError: 'base of a logarithm cannot be negative'].
               radix = radix unity 
                     ifTrue: [^self arithmeticError: 'base of a logarithm cannot be 1'].
               ^(self floorLog: radix reciprocal) negated].
        radix isInteger ifFalse: [^(radix coerce: self) ceilingLog: radix].
        me := self.
        answer := 1.
        [me > radix] whileTrue: 
            [me := me ceilingDiv: radix. "originally: me // radix."
             answer := answer + 1].
        ^answer
    ]

    egcd: o [
        "Answer {x . y} such that (self * x) + (o * y) = (self gcd: o)."
        | q r0 r s0 s t0 t tmp |
        r0 := self. r := o.
        s0 := 1. s := 0.
        t0 := 0. t := 1.

        [r ~= 0] whileTrue: [
            q := r0 // r.

            tmp := r. r := r0 - (q * r). r0 := tmp.
            tmp := s. s := s0 - (q * s). s0 := tmp.
            tmp := t. t := t0 - (q * t). t0 := tmp.
        ].

        ^{s0 . t0}
    ]
]

Random extend [
    shuffle: coll
    [ 1 to: coll size do: [
          :i | | j tmp |
          j := self between: 1 and: coll size.
          tmp := coll at: i.
          coll at: i put: (coll at: j).
          coll at: j put: tmp.
      ].
      ^coll ]
]

"--- KDTree ---"
Object subclass: KDTree [
    from: min to: max do: block
    [ self subclassResponsibility ]
]
KDTree class extend [
    fromPoints: points
    [ ^self fromShuffledPoints: (Random new shuffle: points asArray) ]

    fromShuffledPoints: points
    [ ^self fromShuffledPoints: points axis: 1 ]

    leafMaxSize [ ^5 ]
    fromShuffledPoints: points axis: ax
    [ | med splits dims nextAx value |
      points size <= self leafMaxSize ifTrue: [^KDLeaf new points: points asArray].
      med := self guessMedian: points axis: ax.
      dims := med size.
      nextAx := ax \\ dims + 1.
      value := med at: ax.
      splits := self splitPoints: points axis: ax on: value.
      ^KDInner new
           left: (self fromShuffledPoints: (splits at: 1) axis: nextAx)
           right: (self fromShuffledPoints: (splits at: 2) axis: nextAx)
           axis: ax
           value: value ]

    splitPoints: points axis: ax on: val
    [ | l r fairLeft |
      l := OrderedCollection new.
      r := OrderedCollection new.
      fairLeft := true.
      points do: [
          :point | | v onLeft |
          v := point at: ax.
          onLeft := (v < val) or: [
              v = val and: [fairLeft := fairLeft not. fairLeft]].
          (onLeft ifTrue: [l] ifFalse: [r]) add: point ].
      ^{l . r} ]

    medianGuessSize [ ^5 ]
    guessMedian: points axis: ax
    [ | arr |
      arr := 1 to: (points size min: self medianGuessSize)
               collect: [:i | points at: i].
      arr sort: [:l :r | (l at: ax) <= (r at: ax)].
      ^arr at: arr size // 2 + 1 ]
]

KDTree subclass: KDLeaf [
    | points |
    points: n [points := n. "self class leafSize: points size"]
    from: min to: max do: block [ points do: block ]

    printOn: st [ st << 'lf' << points ]
]
KDLeaf class extend [
    | leafSizes |
    leafSizes [ ^leafSizes ifNil: [leafSizes := Bag new] ]
    leafSize: sz [ self leafSizes add: sz ]
]

KDTree subclass: KDInner [
    | left right axis value |
    left: l right: r axis: ax value: v
    [left:=l. right:=r. axis:=ax. value:=v]
    from: min to: max do: block
    [ (min at: axis) <= value ifTrue: [ left from: min to: max do: block ].
      (max at: axis) >= value ifTrue: [ right from: min to: max do: block ] ]

    printOn: st [ st << '{' << left << ' <= [' << axis << '] '
                  << value << ' <= ' << right << '}' ]
]
