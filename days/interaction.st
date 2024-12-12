Object subclass: ExplorerStream [
    | stream objects | on: aStream
    [ stream := aStream. objects := OrderedCollection new. ]

    at: idx [^objects at: idx ifAbsent: [nil]]

    title: title [ stream << '* ' << title; nl. ]
    subtitle: block [ stream << '- '. block value. stream nl. ]
    text: text [ text displayOn: stream ]
    nl [ stream nl ]
    flush [ stream flush ]

    linkTextTo: obj name: text
    [ objects add: obj. ^'[[aoc2024eutro:objects/%2][%1]]' % {text . objects size} ]

    linkTo: obj name: text [ self text: (self linkTextTo: obj name: text). ]

    section: title do: block [ self text: '** '; text: title; nl. block value ]
    ssection: title do: block [ self text: '*** '; text: title; nl. block value ]
]
ExplorerStream class extend [ on: stream [ ^super new on: stream ] ]

Object extend [
    emacsExploreOn: exs
    [ self basicEmacsExploreOn: exs;
           emacsExploreMembersOn: exs]

    basicEmacsExploreOn: exs
    [ exs title: self;
          subtitle: [
          exs text: 'of class: ';
              linkTo: self class name: self class ] ]

    emacsExploreMembersOn: exs []
]

Object subclass: MessageSource [
    | name | name: n [name:=n] name [^name]

    firstComment [ ^'No source available.' ]
    category [ ^nil ]
    argspec
    [ | i | i := 0.
      ^name asString copyReplacingAllRegex: ':' with: [
               :ignored | i := i + 1. ': arg%1 ' % {i} ]]
]
MessageSource subclass: MessageSourcePresent [
    | source |
    source: n [source:=n]

    quotedRegex: quote
    [ ^('%1((?:[^%1]|%1%1)*?)%1(?!%1)' % {quote}) asRegex ]
    unquoteString: str quote: quote
    [ ^str copyReplacingAllRegex: '%1%1' % {quote}
           with: '%1' % {quote} ]

    findAndUnquote: pattern group: group quote: quote
    [ ^(source searchRegex: pattern)
           ifMatched: [ :m | self unquoteString: (m at: group) quote: quote ]
           ifNotMatched: [ nil ] ]

    firstComment
    [ ^(self findAndUnquote: (self quotedRegex: $") group: 1 quote: $")
           ifNotNil: [:cmt | cmt trimLines]]
    category
    [ | pat | pat := '<category: %1>' % {(self quotedRegex: $') asString}.
      ^self findAndUnquote: pat group: 1 quote: $' ]

    argspec
    [ ^(source searchRegex: '\s*(.*?)\s*\[')
           ifMatched: [:m | (m at: 1) copyReplacingAllRegex: '\s+' with: ' ']
           ifNotMatched: [super argspec] ]
]
MessageSource class extend [
    class: cls selector: name
    [ ^(cls sourceCodeAt: name ifAbsent: [nil])
           ifNil: [MessageSource new name: name]
           ifNotNil: [
               :source |
               MessageSourcePresent new
               name: name; source: source]]
]

ClassDescription extend [
    emacsExploreOn: exs
    [ | methods categories |
      self basicEmacsExploreOn: exs.
      self superclass ifNotNil: [
          :sc | exs subtitle: [
              exs text: 'Superclass: ';
                  linkTo: sc name: sc.
          ] ].
      self category ifNotNil: [
          :cat | exs subtitle: [ exs text: 'Category: '; text: cat ] ].
      self emacsExploreMembersOn: exs.
      exs nl.

      self comment
          ifNotNil: [ :cmnt | exs text: cmnt; nl; nl ]
          ifNil: [ exs text: 'No comment.'; nl; nl ].

      self subclasses size > 0 ifTrue: [
          exs section: 'Class Hierarchy' do: [
                  exs subtitle: [
                      exs text: 'Direct Subclasses: '.
                      self subclasses
                          do: [ :sc | exs linkTo: sc name: sc ]
                          separatedBy: [ exs text: ', ' ]].
                  exs subtitle: [
                      exs text: 'All Subclasses:'.
                      self allSubclasses do: [
                          :sc | exs nl; text: '  - '; linkTo: sc name: sc ]]].
          exs nl.
      ].

      categories := Dictionary new.
      methods := self selectors collect: [
          :sel | MessageSource class: self selector: sel ].
      methods do: [
          :method | | cat | cat := method category ifNil: ['uncategorised'].
          (categories at: cat ifAbsentPut: [
                   SortedCollection sortBlock: [:l :r | l name <= r name]])
              add: method ].
      exs section: 'Own members' do:
          [ categories keysAndValuesDo: [
              :cat :methods |
              exs ssection: cat do: [
                  methods do: [
                  :method |
                  exs subtitle: [
                      exs text: '~%1~' % {method argspec}.
                      method firstComment ifNotNil: [
                          :cmt | exs nl; text: '  '; text: cmt]]
              ]]]]
    ]
]

Metaclass extend [
    emacsExploreMembersOn: exs
    [ exs subtitle: [
        exs text: 'Instance: ';
            linkTo: self instanceClass
            name: self instanceClass ] ]
]

Object subclass: CommandHandler [
    | pattern block |
    pattern: n [pattern:=n]
    block: n [block:=n]
    handleLine: line [ ^line scanf: pattern with: block ]
]
CommandHandler class extend [
    handle: pattern with: block
    [^CommandHandler new pattern: pattern; block: block]
]

Object subclass: Explorer [
    | what obj estream handlers |

    initialize
    [ handlers :=
      {CommandHandler handle: 'explore %s'
                      with: [:it | self exploreName: it].
       CommandHandler handle: 'resolve objects/%d'
                      with: [:idx | self exploreIdx: idx].
       CommandHandler handle: 'list classes' with: [self listClasses].
       CommandHandler handle: 'list selectors' with: [self listSelectors]
      } ]

    explore [ self exploreLoop ]

    exploreLoop
    [ | line |
      [ stdin atEnd ] whileFalse: [
          line := stdin nextLine.
          self handleLine: line ]]

    handleLine: line
    [ handlers do: [:h | (h handleLine: line) ifNotNil: [^self]].
      stdout << ('Bad command: ''%1''' % {line}); nl; flush. ]

    exploreName: what
    [ obj := Smalltalk at: (Symbol intern: what) ifAbsent: [nil].
      stderr << 'Exploring ' << what << '!'; nl; flush.
      obj ifNil: [self reportNotFound: what]
          ifNotNil: [self doExplore] ]
    exploreIdx: idx
    [ obj := estream at: idx.
      stderr << 'Resolving ' << what << '!'; nl; flush.
      self doExplore. ]

    reportNotFound: what
    [ stdout << what << ' is undefined.'; nl; flush ]

    withStream: block
    [ estream := ExplorerStream on: stdout.
      block value.
      estream flush ]

    doExplore
    [ self withStream: [obj emacsExploreOn: estream] ]

    allClassesDo: block
    [ Class allSubclassesDo: [:it | block value: it instanceClass] ]
    listClasses
    [ self withStream: [
        self allClassesDo: [:class | estream linkTo: class name: class; nl]]]

    listSelectors
    [ | sels selsSorted |
      sels := Dictionary new.
      selsSorted := SortedCollection new.
      self allClassesDo: [
          :class | class selectors do: [
              :sel |
              (sels at: sel ifAbsentPut:
                    [ SortedCollection sortBlock: [:l :r | l name <= r name]])
              add: class
          ]].
      selsSorted addAll: sels keys.
      self withStream: [
          selsSorted do: [
          :sel |
          estream text: sel asString; text: ' -- '.
          (sels at: sel) do: [:cls | estream linkTo: cls name: cls]
                         separatedBy: [estream text: ', '].
          estream nl.
      ]]
    ]
]

AOC action: 'explore' do: [ Explorer new explore ]
