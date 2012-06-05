'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 4 June 2012 at 7:46:52 pm'!
'Description Split out from XML-Parser'!
!classDefinition: #XPath category: #'XPath-XPath'!
Object subclass: #XPath
	instanceVariableNames: 'instructions literals path source block'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XPath-XPath'!
!classDefinition: 'XPath class' category: #'XPath-XPath'!
XPath class
	instanceVariableNames: ''!

!classDefinition: #XPathContext category: #'XPath-XPath'!
Object subclass: #XPathContext
	instanceVariableNames: 'root path parameters locals stack marker pc results done'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XPath-XPath'!
!classDefinition: 'XPathContext class' category: #'XPath-XPath'!
XPathContext class
	instanceVariableNames: ''!

!classDefinition: #XPathParser category: #'XPath-XPath'!
Object subclass: #XPathParser
	instanceVariableNames: 'stream path pathSource'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XPath-XPath'!
!classDefinition: 'XPathParser class' category: #'XPath-XPath'!
XPathParser class
	instanceVariableNames: ''!


!XPath commentStamp: '<historical>' prior: 0!
I represent an instance of an XPath, which can be applied to XML documents (or SAX event streams, and a few other things) to generate a list of nodes matching the query or path that I represent.

My instance creation methods accept a string (or stream) and compile the string representation of an XPath query into a sequence of selectors.  These selectors are messages for an XPathContext that when sent to an instance of an XPathContext will execute the path on the document specified by that context.

!

!XMLDOMParser methodsFor: '*xpath-content' stamp: 'PH 10/7/2003 07:34'!
characters: aString 
	| newElement |
	newElement := XMLStringNode string: aString.
	newElement parent: self top.
	self top addContent: newElement! !

!XMLDOMParser methodsFor: '*xpath-content' stamp: 'PH 10/7/2003 07:33'!
processingInstruction: piName data: dataString 
	| newElement |
	newElement := XMLPI target: piName data: dataString.
	newElement parent: self top.
	self top addElement: newElement! !

!XMLDOMParser methodsFor: '*xpath-content' stamp: 'PH 10/7/2003 07:34'!
startElement: elementName attributeList: attributeList 
	| newElement |
	newElement := XMLElement named: elementName attributes: attributeList.
	newElement parent: self top.
	self incremental
		ifFalse: [self stack isEmpty
				ifFalse: [self top addElement: newElement]].
	self push: newElement! !

!XMLNode methodsFor: '*xpath-accessing'!
parent
	^ parent! !

!XMLNode methodsFor: '*xpath-accessing'!
parent: node
	parent := node! !

!XPath methodsFor: 'accessing' stamp: 'PH 10/13/2003 06:41'!
block
	block
		ifNil: [block := self compile].
	^ block! !

!XPath methodsFor: 'accessing' stamp: 'PH 10/11/2003 14:03'!
block: aBlock
	block := aBlock! !

!XPath methodsFor: 'as yet unclassified' stamp: 'PH 10/12/2003 18:38'!
compile
	| methodNode |
	methodNode := Parser new
				parse: self source readStream
				class: XPathContext
				noPattern: true
				context: nil
				notifying: nil
				ifFail: [].
	^ methodNode generate: #(0 0 0 0 )! !

!XPath methodsFor: 'as yet unclassified' stamp: 'PH 10/12/2003 18:17'!
in: root 
	"Search the specified document / root for nodes matching this xpath"
	| context |
	root isTag
		ifTrue: [context := XPathContext for: self in: root]
		ifFalse: [context := XPathContext
						for: self
						in: (root elements at: 1)].
	^ self block valueWithReceiver: context arguments: #().
! !

!XPath methodsFor: 'initialization' stamp: 'PH 7/21/2002 11:58'!
initialize
	instructions _ OrderedCollection new.
	literals _ OrderedCollection new.
	^self! !

!XPath methodsFor: 'accessing' stamp: 'PH 10/12/2003 15:56'!
path
	^ path! !

!XPath methodsFor: 'accessing' stamp: 'PH 10/12/2003 18:21'!
path: string
	path := string.
	source := nil.
	block := nil.! !

!XPath methodsFor: 'accessing' stamp: 'PH 10/13/2003 06:42'!
source
	source
		ifNil: [ source := (XPathParser on: path readStream) parse].
	^ source! !

!XPath class methodsFor: 'instance creation' stamp: 'PH 10/12/2003 18:22'!
for: xpath 
	"Return a new XPath instance that has the xpath string representation. I parse the xpath 
	string to build an instance."
	| path |
	path := self new.
	path path: xpath.
	^ path
! !

!XPath class methodsFor: 'instance creation' stamp: 'PH 7/21/2002 11:58'!
new
	^super new initialize! !

!XPath class methodsFor: 'instance creation' stamp: 'PH 7/21/2002 10:41'!
on: aStream
	"Parse the XPath on aStream and return an instance of XPath containing the compiled result"
	^ (XPathParser on: aStream) 
		parse;
		path.
		! !

!XPathContext methodsFor: 'instructions' stamp: 'hfm 8/28/2009 01:44'!
andAttributes: namesCollection
	"Look up an attribute"
	| allAttributes |
	
	allAttributes := OrderedCollection new: stack size.
	self pop do:
		[:element | 
			| attributes |
			attributes := element attributes keys asSet.
			( attributes = namesCollection asSet )
				ifTrue: [ allAttributes add: element attributes ] ].
	self push: ( allAttributes collect: [: each | each values ] )! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/13/2003 06:42'!
at: nameIndex 
	| name values attribute |
	name := path literalAt: nameIndex.
	values := OrderedCollection new.
	name = '*'
		ifTrue: [self pop
				do: [:attributes | values addAll: attributes values]]
		ifFalse: [self pop
				do: [:attributes | 
					attributes at: name ifPresent: [:value | attribute := value. values add: attribute].
					]].
	self push: values! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/12/2003 18:31'!
attribute: name
	"Look up an attribute"
	| allAttributes values attribute |
	allAttributes := OrderedCollection new.
	self pop do:
		[:element | allAttributes add: element attributes].
	values := OrderedCollection new.
	name = '*'
		ifTrue: [allAttributes
				do: [:attributes | values addAll: attributes values]]
		ifFalse: [allAttributes
				do: [:attributes | 
					attributes at: name ifPresent: [:value | attribute := value. values add: attribute].
					]].
	self push: values! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 7/24/2002 21:34'!
attributes
	"Push the attributes"
	| allAttributes |
	"Processor activeProcess debugWithTitle: 'Trying out the attributes stuff'."
	allAttributes := OrderedCollection new.
	self pop do:
		[:element | allAttributes add: element attributes].
		"[:element | element attributes associationsDo: [:assoc | allAttributes add: assoc]]. "
	self push: allAttributes.! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/13/2003 06:42'!
both: arg1 and: arg2
	^ self! !

!XPathContext methodsFor: 'functions' stamp: 'PH 10/12/2003 20:30'!
count
	self
		push: (Array with: self pop size)! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/13/2003 06:42'!
descendants
	"Gather all of the descendant nodes"
	"Processor activeProcess debugWithTitle: 'Checking descendants code'."
	| parents children allDescendants |
	parents := OrderedCollection withAll: self pop.
	"allDescendants := OrderedCollection new."
	allDescendants := OrderedCollection withAll: parents.
	children := OrderedCollection new.
	[parents isEmpty]
		whileFalse: [parents
				do: [:parent | parent isTag
						ifTrue: [children addAll: parent elements]].
			allDescendants addAll: children.
			parents := children.
			children := OrderedCollection new].
	self push: allDescendants! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/8/2003 07:31'!
either: arg1 or: arg2! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/12/2003 18:30'!
element: name
	| children matches |
	children := OrderedCollection new.
	self pop
		do: [:element | "[:element | children addAll: element elements]."
			element isTag
				ifTrue: [children
						addAll: (element elements
								select: [:element2 | element2 isTag])]].
	matches := (children select:
		[:element | (element name = name) or: [name = '*']]).
	self push: matches.! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/13/2003 06:43'!
elements
	| children |
	children := OrderedCollection new.
	self pop
		do: [:element | "[:element | children addAll: element elements]."
			element isTag
				ifTrue: [children
						addAll: (element elements
								select: [:element2 | element2 isTag])]].
	self push: children! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 7/23/2002 07:34'!
finish
	done := true.
	results := self pop.
	! !

!XPathContext methodsFor: 'initialization' stamp: 'PH 7/24/2002 09:20'!
initialize
	locals := OrderedCollection new.
	done := false.
	stack := OrderedCollection new.
	pc := 1. "Start at the beginning"
	^self! !

!XPathContext methodsFor: 'functions' stamp: 'PH 10/12/2003 20:06'!
last
	self
		push: (self pop
				collect: [:element | element elements last])! !

!XPathContext methodsFor: 'as yet unclassified' stamp: 'PH 10/9/2003 19:33'!
mark
	stack push: pc.
	marker := stack size.
	! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/6/2003 21:16'!
name: nameIndex
	| name matches |
	"Processor activeProcess debugWithTitle: 'A problem with names'."
	name := path literalAt: nameIndex.
	matches := (self pop select:
		[:element | (element name = name) or: [name = '*']]).
		"[:element | (element tag type = name) or: [name = '*']])."
	self push: matches.! !

!XPathContext methodsFor: 'instructions' stamp: 'hfm 8/21/2009 01:15'!
orAttributes: namesCollection
	"Look up an attribute"
	| allAttributes |
	
	allAttributes := OrderedCollection new.
	self pop do:
		[:element | 
			| attributes |
			attributes := element attributes asSet.
			( attributes includesAnyOf: namesCollection asSet )
				ifTrue: [ allAttributes add: element attributes ] ].
	self push: allAttributes values! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 7/24/2002 06:48'!
parent
	self push: (self pop collect:
		[:element | element parent]).! !

!XPathContext methodsFor: 'accessing' stamp: 'PH 7/21/2002 10:26'!
path
	^path! !

!XPathContext methodsFor: 'accessing' stamp: 'PH 7/21/2002 10:25'!
path: aPath
	path := aPath! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 7/24/2002 06:20'!
pop
	^ stack removeLast.! !

!XPathContext methodsFor: 'functions' stamp: 'PH 7/27/2002 07:47'!
position
	| positions |
	positions := OrderedCollection new.
	self pop do:
		[:element | positions add: (element parent indexOf: element)].
	self push: positions.! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 7/22/2002 20:50'!
push: anObject
	stack addLast: anObject.! !

!XPathContext methodsFor: 'instructions' stamp: 'PH 10/11/2003 08:45'!
query
	| instruction |
	"Must start with an initial collection of nodes; size = 1"
	"Also, need to skip the document node and start with the root node"
	"self push: (OrderedCollection withAll: root elements)."
	self push: (OrderedCollection with: root).
	self results ifNil: 
		[
			[done] whileFalse:
				[
					| nextPC |
					nextPC := pc + 1.
					instruction := path instructionAt: pc.
					pc := nextPC.
					instruction sentTo: self.
				].
		].
	^self results! !

!XPathContext methodsFor: 'accessing' stamp: 'PH 7/21/2002 10:45'!
results
	^results! !

!XPathContext methodsFor: 'as yet unclassified' stamp: 'PH 10/9/2003 19:34'!
revert
	[stack size > marker]
		whileTrue: [stack pop].
	pc := stack pop.! !

!XPathContext methodsFor: 'accessing' stamp: 'PH 7/21/2002 11:31'!
root
	^root! !

!XPathContext methodsFor: 'accessing' stamp: 'PH 7/21/2002 11:32'!
root: rootNode
	root := rootNode! !

!XPathContext methodsFor: 'functions' stamp: 'PH 7/25/2002 21:01'!
text
	"Return element content"
	| allText |
	allText := OrderedCollection new.
	self pop collect: [:element | allText addAll: element contents].
	self push: allText.! !

!XPathContext class methodsFor: 'instance creation' stamp: 'PH 7/21/2002 11:31'!
for: aPath in: aDocument
	| context |
	context := self new.
	context path: aPath;
			root: aDocument.
	^context! !

!XPathContext class methodsFor: 'as yet unclassified' stamp: 'PH 7/21/2002 11:57'!
new
	^super new initialize.! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/11/2003 10:14'!
emit: string
	pathSource := pathSource , string! !

!XPathParser methodsFor: 'emitting' stamp: 'hfm 8/21/2009 00:54'!
emitAndBetween: operandCollection

	| code |
	
	code := String streamContents: [: s |
		s nextPutAll: 'self andAttributes: ';
			nextPut: ${;
			space.
		operandCollection do: [: each |
			s nextPutAll: ( self quoted: each ).
			  ( operandCollection indexOf: each ) = operandCollection size
				ifFalse: [ s nextPut: $. ] ].
		s nextPut: $};
			nextPut: $. ].
	self emit: code.! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/11/2003 06:45'!
emitArgList: args! !

!XPathParser methodsFor: 'emitting' stamp: 'hfm 8/18/2009 09:16'!
emitAttribute: name 
	
	| code |

	code := String streamContents: [: str |
		str nextPutAll: 'self attribute: ';
			nextPutAll: (self quoted: name);
			nextPut: $.. ].
	self emit: code! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/13/2003 06:43'!
emitDescendants
	self emit: 'self descendants.'! !

!XPathParser methodsFor: 'emitting' stamp: 'hfm 8/18/2009 09:17'!
emitElement: name 

	| code |

	code := String streamContents: [: str |
		str nextPutAll: 'self element: ';
			nextPutAll: (self quoted: name);
			nextPut: $.. ].
	self emit: code
! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/11/2003 10:16'!
emitEndFilter
	self emit: ']'! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/13/2003 06:43'!
emitFinish
	pathSource := pathSource , 'results := self pop. ^ self results'! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/13/2003 06:44'!
emitFunction: name 
	"No lookup of function name in a reserved word table; assume it's right and embed in  
	instructions"
	self emit: ('self ' , name , '.').! !

!XPathParser methodsFor: 'emitting' stamp: 'hfm 8/21/2009 01:10'!
emitOperation: aCollection
	
	| operandsCollection |

	operandsCollection := aCollection select: [: each | ( aCollection indexOf: each ) odd ].
	aCollection second = 'and'
		ifTrue: [ self emitAndBetween: operandsCollection ]
		ifFalse: [ aCollection second = '|'
					ifTrue: [ self emitOrBetween: operandsCollection ] ].
! !

!XPathParser methodsFor: 'emitting' stamp: 'hfm 8/21/2009 01:12'!
emitOrBetween: operandCollection

	| code |
	
	code := String streamContents: [: s |
		s nextPutAll: 'self orAttributes: ';
			nextPut: ${;
			space.
		operandCollection do: [: each |
			s nextPutAll: ( self quoted: each ).
		  ( operandCollection indexOf: each ) = operandCollection size
			ifFalse: [ s nextPut: $. ] ].
		s nextPut: $};
			nextPut: $. ].
	self emit: code.! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/13/2003 06:44'!
emitParent
	self emit: 'self parent.'! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/13/2003 06:44'!
emitStart
	pathSource := 'self push: (Array with: root).'! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/11/2003 10:15'!
emitStartFilter
	self emit: 'self filter: ['! !

!XPathParser methodsFor: 'emitting' stamp: 'PH 10/11/2003 06:14'!
emitThis! !

!XPathParser methodsFor: 'initialization' stamp: 'PH 10/12/2003 13:20'!
initialize
	path := XPath new.
	self emitStart.
	^self! !

!XPathParser methodsFor: 'streaming' stamp: 'PH 7/21/2002 12:01'!
next
	^stream next! !

!XPathParser methodsFor: 'streaming' stamp: 'PH 7/25/2002 06:12'!
nextName
	"Assume the stream is positioned at the beginning of a name. Read in that name and return it as a string." 
	| char name |
	name := String new.
	char := self peek. 
	char = $* ifTrue: [self next. ^'*'].
	[ 
		char := self peek.
		char notNil and:[(char isAlphaNumeric) or: [char = $-]]
		] 
		whileTrue:
			[char := self next. name := name , (char asString)].
	^name.
		
! !

!XPathParser methodsFor: 'streaming' stamp: 'PH 7/25/2002 06:33'!
nextNumber
	"Assume the stream is positioned at the beginning of a number. Read in that number and return it as a string." 
	| char number |
	number := String new.
	[ 
		char := self peek.
		char notNil and:[(char isDigit)]
		] 
		whileTrue:
			[char := self next. number := number , (char asString)].
	^number asInteger.
		
! !

!XPathParser methodsFor: 'streaming' stamp: 'hfm 8/20/2009 23:35'!
nextOperator
	"Assume the stream is positioned at the beginning of an operator. 
	Read in that name and return it as a string." 
	
	^ String streamContents: [: s |	
		| char |
		[ char := self peek. 
		char = Character space ]
			whileFalse: [ 
				char := self next.
				s nextPut: char ]
		].

		
! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 10/13/2003 06:45'!
parse
	"Parse the path found on the stream"
	self parseExpression.
	self emitFinish.
	^ pathSource! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 10/13/2003 06:45'!
parseArgList
	self next = $(
		ifTrue: [].
	[self peek = $)]
		whileFalse: [self parseExpression].
	self next = $)
		ifTrue: []! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 10/11/2003 06:16'!
parseAttribute
	"Parse the attribute found on the stream"
	| name |
	self next.	"Skip the @"
	name := self nextName.
	self emitAttribute: name.! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 10/11/2003 06:19'!
parseDot
	| char |
	char := self next; peek. "Skip the first $., then check to see if there is another behind it"
	char = $. 
		ifTrue: "Referencing parent node"
			[self next. self emitParent]
		ifFalse: [self emitThis]. "Do nothing--referencing current node"
		! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 10/13/2003 06:45'!
parseElementOrFunction
	"Parse the element found on the stream"
	| name |
	name := self nextName.
	"Do a quick check to see if this is really a function call"
	self peek = $(
		ifTrue: ["It is a function call"
			self parseArgList.
			self emitFunction: name.]
		ifFalse: ["It's an element name"
			self emitElement: name]! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 7/27/2002 15:57'!
parseEqualityExpression! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 8/26/2002 10:32'!
parseExpression
	self parseLocation.! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 10/11/2003 10:03'!
parseFilter
	self peek isDigit
		ifTrue: [self parsePosition]
		ifFalse: [ self emitStartFilter. self parseExpression. self emitEndFilter.].
	^ self! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 8/4/2002 19:38'!
parseFunctionCall! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 8/4/2002 19:37'!
parseLiteral! !

!XPathParser methodsFor: 'parsing' stamp: 'hfm 8/20/2009 13:42'!
parseLocation
	"Parse the location found on the stream"
	"path := XPath new."
	| char |
	[char := self peek.
	char notNil & ((char = $)) not)]
		whileTrue: [
			"Check to see if it's an element test"
			( char isLetter or: [char = $*] )
				ifTrue: [self parseElementOrFunction].
			char = $@
				ifTrue: [self parseAttribute].
			char = $.
				ifTrue: [self parseDot].
			char = $/
				ifTrue: [self parseSlash].
			char = $[
				ifTrue: [self parseFilter].
			char = $(
				ifTrue: [self parseOperator] ]! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 7/27/2002 15:57'!
parseNotExpression! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 8/4/2002 19:37'!
parseNumber! !

!XPathParser methodsFor: 'parsing' stamp: 'hfm 8/28/2009 01:46'!
parseOperator

	| nameLeft operator rem |
	
	self needsWork. " a lot really "
	rem := OrderedCollection new.
	" Skip the ( "
	self next.
	" Skip the @ "
	self next.
	nameLeft := self nextName.
	self peek = Character space
		ifTrue: [ 
			self next.
			operator := self nextOperator ].
	rem add: nameLeft;
		add: operator.
	[ self peek = Character space and: [ self peek ~= $) ] ]
		whileTrue: [ 
			" Skip space "
			self next.		
			" Skip the @ "
			self next.		
			rem add: self nextName.
			self peek = $)
				ifFalse: [ self next.
						rem add: self nextOperator ]
		].
				
				
	self emitOperation: rem.
! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 7/27/2002 15:56'!
parseOrExpression! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 8/4/2002 19:38'!
parseParentheticalExpression! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 10/13/2003 06:44'!
parsePosition
	^ self! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 8/3/2002 18:09'!
parsePrimaryExpression! !

!XPathParser methodsFor: 'parsing' stamp: 'PH 10/11/2003 06:20'!
parseSlash
	| char |
	char := self next; peek.
	char = $/ 
		ifTrue:
			[self next.  self emitDescendants]
		ifFalse: []. "Do nothing--parsing an element will take care of everything"
! !

!XPathParser methodsFor: 'parsing-unimplemented' stamp: 'PH 8/3/2002 18:09'!
parseVariableReference! !

!XPathParser methodsFor: 'accessing' stamp: 'PH 7/21/2002 10:39'!
path
	path ifNil: [self parse].
	^path! !

!XPathParser methodsFor: 'streaming' stamp: 'PH 7/21/2002 12:01'!
peek
	^stream peek! !

!XPathParser methodsFor: 'as yet unclassified' stamp: 'PH 10/12/2003 19:02'!
quoted: string
	^ ($' asString) , string , ($' asString)! !

!XPathParser methodsFor: 'accessing' stamp: 'PH 7/21/2002 10:36'!
stream
	^ stream! !

!XPathParser methodsFor: 'accessing' stamp: 'PH 7/21/2002 10:35'!
stream: aStream
	stream := aStream! !

!XPathParser class methodsFor: 'as yet unclassified' stamp: 'PH 7/21/2002 11:57'!
new
	^super new initialize! !

!XPathParser class methodsFor: 'as yet unclassified' stamp: 'PH 7/21/2002 11:54'!
on: aStream
	| parser |
	parser := self new.
	parser stream: aStream.
	^parser! !
