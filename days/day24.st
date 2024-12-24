Object subclass: Gate [
    | name classif |
    name: nm [name := nm]
    name [^name]
    printOn: st [ st << self class << ' ' << name ]
    eval: cq [self subclassResponsibility]

    inputs [^nil]
    opnm [self subclassResponsibility]
    visualize: st
    [st << name << '[label="' << name << ' ' << self opnm.
     classif ifNotNil: [:c | st << ' (' << c << ')'].
     st << '"];']

    classif: c [classif := c]
    classif [^classif]

    isInput [^false]
]
Gate subclass: InputGate [
    isInput [^true]
]
InputGate subclass: OneGate [opnm [^$1] eval: cq [^true]]
InputGate subclass: ZeroGate [opnm [^$0] eval: cq [^false]]
Gate subclass: LogicGate [
    | lhs rhs cache |
    lhs: l rhs: r [lhs := l. rhs := r]
    eval: cq [^cache ifNil: [cache := self doEval: (cq eval: lhs) and: (cq eval: rhs)]]
    doEval: lv and: rv [self subclassResponsibility]
    printOn: st [ st << self class << $( << name << ': ' << lhs << ', ' << rhs << $) ]

    inputs [^{lhs . rhs}]

    visualize: st
    [super visualize: st.
     st << lhs << '->' << name << ';'.
     st << rhs << '->' << name << ';']
]
LogicGate subclass: AndGate [opnm [^$&] doEval: lv and: rv [^lv & rv]]
LogicGate subclass: OrGate [opnm [^$|] doEval: lv and: rv [^lv | rv]]
LogicGate subclass: XorGate [opnm  [^$^] doEval: lv and: rv [^lv xor: rv]]

Object subclass: Circuit [
    | gates forwardMap swaps locked classMap |
    initialize
    [ gates := LookupTable new.
      swaps := Set new.
      locked := Bag new. ]
    gates [^gates]
    at: nm put: gate [^gates at: nm put: (gate name: nm)]
    at: nm [^gates at: nm]

    eval: nm [^(gates at: nm) eval: self]

    parseInits: inits logic: logic
    [ | optbl |
      optbl := LookupTable new.
      optbl at: 'AND' put: AndGate;
            at: 'OR' put: OrGate;
            at: 'XOR' put: XorGate.
      inits lines do: [
          :ln | ln scanf: '%[^:]: %d'
                   with: [
                  :nm :v |
                  self at: nm put:
                     (v = 1 ifTrue: [OneGate] ifFalse: [ZeroGate])
                         new]].
      logic lines do: [
          :ln | ln scanf: '%s %s %s -> %s'
                   with: [
                  :g1 :op :g2 :nm |
                  self at: nm put: ((optbl at: op) new lhs: g1 rhs: g2)]]]

    gateAssocs
    [ | ord | ord := SortedCollection new.
      gates keysAndValuesDo: [:nm :g | ord add: (nm -> g)].
      ^ord asArray ]

    printOn: st
    [ st << self class << $(; nl.
      self gateAssocs do: [:ass | st tab; << ass value; nl].
      st << $) ]

    visualize
    [ AOC visualizeWithExt: '.dot' do: [
          :st |
          st << 'digraph {'.
          self gateAssocs do: [:ass | ass value visualize: st].
          st << '}'; nl] ]

    swap: nm1 and: nm2
    [ | tmp key undoing |
      key := {nm1. nm2} sort.
      (swaps includes: key) ifTrue: [
          swaps remove: key.
          locked removeAll: {nm1 . nm2}.
      ] ifFalse: [
          swaps size >= 4 ifTrue: [^false].
          ((locked includes: nm1) or: [locked includes: nm2])
              ifTrue: [^false].
          swaps add: key.
          locked addAll: {nm1 . nm2}.
      ].
      tmp := self at: nm1.
      self at: nm1 put: (gates at: nm2).
      self at: nm2 put: tmp.
      ^true ]

    try: block unwind: otherwise
    [ ^block value or: [otherwise value. false] ]

    swapped: nm1 and: nm2 do: block
    [ ^(self swap: nm1 and: nm2) and: [
          self try: block
               unwind: [self swap: nm1 and: nm2]]]
    locked: nm do: block
    [ ^self try: [locked add: nm. block value]
            unwind: [locked remove: nm] ]

    classifyGates
    [ classMap := Dictionary new.
      gates do: [
        :g | | k |
        k := self classify: g.
        g classif: k.
        classMap at: k inA: Set add: g ] ]

    classify: g
    [ | halfGate |
      "half adder:
       - I,  I -> H&, H^
       - C' = H^ ; O = H&"
      "full adder:
       - H^, C -> F&, F^
       - F| <- H&, F&
       - C' = F| ; O = F&"
      g isInput ifTrue: [ ^'I' ].
      halfGate := g inputs allSatisfy: [:i | (self at: i) isInput].
      halfGate ifTrue: [ ^String with: $H with: g opnm ].
      ^String with: $F with: g opnm ]

    withInput: nm
    [ forwardMap ifNil: [
        forwardMap := Dictionary new.
        gates do: [:g | g inputs ifNotNilDo: [
                       :i | forwardMap at: i inA: Set add: g]]].
      ^forwardMap at: nm ifAbsent: [#()] ]
    classifySet: set
    [ | map | map := LookupTable new.
      set do: [:elt | map at: elt classif put: elt].
      ^map ]
    withInputClassify: nm [ ^self classifySet: (self withInput: nm) ]
    withInputClassify: nm1 and: nm2 mustHave: classes
    [ | cls1 cls2 |
      cls1 := self withInputClassify: nm1.
      cls2 := self withInputClassify: nm2.
      cls1 = cls2 ifFalse: [^nil].
      classes do: [:c | cls1 at: c ifAbsent: [^nil]].
      ^cls1 ]
    withInputClassify: nm1 and: nm2 mustHave: classes do: block
    [ (self withInputClassify: nm1 and: nm2 mustHave: classes)
          ifNil: [^false]
          ifNotNil: [:v | ^block value: v] ]

    findFaults
    [ self classifyGates.
      (self findFaults: 0 carry: nil)
          ifFalse: [self error: 'No working swaps']
          ifTrue: [^swaps]. ]

    gate: gate mustHaveName: nm do: block
    [ ^(gate name = nm)
           ifTrue: [block value]
           ifFalse: [self swapped: gate name and: nm do: block] ]

    ioName: c at: idx
    [ ^String with: c
              with: (Character digitValue: idx // 10)
              with: (Character digitValue: idx \\ 10) ]

    maybeRenameGate: gate do: nameBlock
    [ ^(self locked: gate name do: [
          nameBlock value: gate name]) or: [
      (classMap at: gate classif) anySatisfy: [
      :ogate |
      (ogate == gate) not and: [
      (locked includes: ogate name) not and: [
      self swapped: gate name and: ogate name do: [
      nameBlock value: gate name ]]]]]]

    maybeSwap: gate1 and: gate2 do: block
    [ ^block value or: [ self swapped: gate1 name and: gate2 name do: block ] ]

    findFaults: idx carry: carry
    [ | halfAdder zNN |
      "Try and find the adder which outputs zNN.

      This involves finding the half adder, then its full adder,
      ensuring the output bit is to the correct zNN, and possibly
      swapping out each output along the way."
      zNN := self ioName: $z at: idx.
      "inputs can't be wired wrong"
      halfAdder := self withInputClassify: (self ioName: $x at: idx).
      halfAdder isEmpty ifTrue: ["Done!" ^carry = zNN].
      ^self maybeSwap: (halfAdder at: 'H^') and: (halfAdder at: 'H&') do: [
      carry ifNil: [
          self gate: (halfAdder at: 'H^')
               mustHaveName: zNN
               do: [
          self maybeRenameGate: (halfAdder at: 'H&') do: [
          :hAnm | self findFaults: idx + 1 carry: hAnm ]]]
      ifNotNil: [
          self maybeRenameGate: (halfAdder at: 'H^') do: [
          :hXnm | 
          self withInputClassify: carry
               and: hXnm
               mustHave: #('F&' 'F^')
               do: [
          :fullAdder |
          self maybeSwap: (fullAdder at: 'F^') and: (fullAdder at: 'F&') do: [
          self gate: (fullAdder at: 'F^')
               mustHaveName: zNN
               do: [
          self maybeRenameGate: (fullAdder at: 'F&') do: [
          :fAnm |
          self maybeRenameGate: (halfAdder at: 'H&') do: [
          :hAnm |
          self withInputClassify: hAnm
               and: fAnm
               mustHave: #('F|')
               do: [
          :fullOr |
          self maybeRenameGate: (fullOr at: 'F|') do: [
          :fOnm |
          self findFaults: idx + 1 carry: fOnm ]]]]]]]]]]]
]

AOC input: [ stdin contents splitDoubleNl letArrayInBlock: [
               :inits :logic | Circuit new parseInits: inits logic: logic ]];
    part1: [ :cq | | outs res |
             outs := SortedCollection sortBlock: [:l :r | r <= l].
             cq gates keysAndValuesDo: [
                 :nm :i | (nm startsWith: 'z') ifTrue: [outs add: nm]].
             res := 0.
             outs do: [
                 :nm | | bit |
                 bit := (cq eval: nm) ifTrue: [1] ifFalse: [0].
                 res := (res bitShift: 1) bitOr: bit.
             ].
             res ];
    part2: [ :cq | cq findFaults join sort join: ','. ];
    result: [ :cq :part | part value: cq ];
    finish.
