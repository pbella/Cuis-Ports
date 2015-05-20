'From Squeak3.10.2 of ''5 June 2008'' [latest update: #7179] on 26 April 2009 at 12:46:09 am'!

!OMeta2 class methodsFor: '*additions' stamp: 'rkrk 4/25/2009 01:20'!
firstPrecodeCommentFor: selector
	"For compatibility with squeak dev"

	^nil! !


'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:21 pm'!
OMeta2 subclass: #O2SqueakRecognizer
	instanceVariableNames: ''
	classVariableNames: 'TypeTable'
	poolDictionaries: ''
	category: 'OMeta2'!

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:43'!
arrayConstr =

	"{" expr ("." expr)* ("." | empty) "}"
|	"{" "}"! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:57'!
arrayLit =

	"#" "(" (literal | arrayLit | spaces tsArraySymbol)* ")"! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:57'!
binary =

	spaces tsBinary! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:58'!
binaryExpr =

	binaryExpr binaryMsg
|	unaryExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:58'!
binaryMsg =

	binary unaryExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:08'!
block =

	"["
		(	(":" identifier)+ "|"
		|	empty
		)

		(	"|" identifier* "|"
		|	empty
		)

		(	expr ("." expr)* ("." "^" expr | empty)
		|	"^" expr
		|	empty
		)

		(	"."
		|	empty
		)

	"]"! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:58'!
cascade =

	identifier
|	binaryMsg
|	keywordMsg! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:59'!
expr =

	identifier (":=" | "_") expr
|	msgExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:18'!
identifier =

	spaces tsIdentifier ~$:! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:19'!
keyword =

	spaces tsKeyword! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:19'!
keywordExpr =

	binaryExpr keywordMsg! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:19'!
keywordMsg =

	keywordMsg keywordMsgPart
|	keywordMsgPart! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:20'!
keywordMsgPart =

	keyword binaryExpr! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:20'!
literal =

	spaces	(	tsNumber
			|	tsCharacter
			|	tsString
			|	tsSymbol
			)
! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:21'!
msgExpr =

	(keywordExpr | binaryExpr) (";" cascade)*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:30'!
squeakExpr =

	<expr>! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:23'!
symbol =

	"#" spaces
		(	tsString
		|	tsKeyword (tsIdentifier | empty)
		)! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:23'!
tcBinaryChar =

	char:x ?[(TypeTable at: x asciiValue) == #xBinary]! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:25'!
tsArraySymbol =

	tsKeyword+ (tsIdentifier | empty)
|	tsIdentifier! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:25'!
tsBinary =

	($| | tcBinaryChar) tcBinaryChar*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:25'!
tsCharacter =

	$$ char! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:26'!
tsIdentifier =

	letter (letter | digit)*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:31'!
tsKeyword =

	tsIdentifier $:! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:26'!
tsNatural =

	digit+! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:27'!
tsNumber =

	($+ | $- | empty) tsNatural! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:28'!
tsString =

	$' ($' $' | ~$' char)* $'! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:28'!
tsSymbol =

	$# spaces (tsString | tsArraySymbol)! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:28'!
unaryExpr =

	unit identifier*! !

!O2SqueakRecognizer methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:29'!
unit =

	literal
|	identifier
|	arrayLit
|	arrayConstr
|	block
|	"(" expr ")"
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

O2SqueakRecognizer class
	instanceVariableNames: ''!

!O2SqueakRecognizer class methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 02:04'!
initialize

	TypeTable := #(#xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xDelimiter #xDelimiter #xBinary #xDelimiter #xDelimiter #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #doIt #xBinary #xDelimiter #xBinary #xDoubleQuote #xLitQuote #xDollar #xBinary #xBinary #xSingleQuote #leftParenthesis #rightParenthesis #xBinary #xBinary #xBinary #xBinary #period #xBinary #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xDigit #xColon #semicolon #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #leftBracket #xBinary #rightBracket #upArrow #leftArrow #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #leftBrace #verticalBar #rightBrace #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xLetter #xBinary #xBinary #xBinary #xBinary #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xLetter #xBinary)! !


O2SqueakRecognizer initialize!
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:17 pm'!
OMeta2Base subclass: #OMeta2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!OMeta2 commentStamp: '<historical>' prior: 0!
TODOS:

* implement OMeta -> Squeak translator
* implement Squeak parser
* implement OMeta/Squeak "compiler", make it OMeta2's compilerClass

* rewrite #char, #digit, #empty, #end, #exactly, #firstAndRest, #fromTo, #letter, #letterOrDigit, #listOf, #lower, #notLast, #number, #range, #space, #spaces, #string, #symbol, #token, and #upper in OMeta syntax
* consider implementing position-related functionality (see http://www.tinlizzie.org/ometa-js/ometa-base.js)
* consider the optimization suggestions in the comments of OMeta2Lib's methods!
]style[(6 41 26 95 56 16 27 17 257)bcblack;,cblack;,f0,cblack;,f0cblack;,f0,f0cblack;,f0,f0cblack;!


!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:01'!
char

	| x |
	x := self apply: #anything.
	self pred: (x isKindOf: Character).
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:05'!
digit

	| x |
	x := self apply: #char.
	self pred: x isDigit.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 16:14'!
empty

	^ true! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/16/2009 16:17'!
end

	" may want to inline the sends below, since this rule gets used a lot "
	^ self not: [self apply: #anything]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:32'!
exactly

	| wanted got |
	wanted := self apply: #anything.
	got := self apply: #anything.
	self pred: wanted = got.
	^ wanted! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 13:52'!
firstAndRest

	| first rest |
	first := self apply: #anything.
	rest := self apply: #anything.
	^ self genericMany: [self apply: rest] into: (OrderedCollection with: (self apply: first))! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 15:40'!
fromTo

	| x y |
	x := self apply: #string.
	y := self apply: #string.
	self apply: #seq withArgs: {x}.
	self many: [
		self not: [self apply: #seq withArgs: {y}].
		self apply: #char
	].
	self apply: #seq withArgs: {y}! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:03'!
letter

	| x |
	x := self apply: #char.
	self pred: x isLetter.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:05'!
letterOrDigit

	| x |
	x := self apply: #char.
	self pred: x isAlphaNumeric.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 13:50'!
listOf

	| rule delim |
	rule := self apply: #anything.
	delim := self apply: #anything.
	^ self ometaOr: {
		[
			| x |
			x := self apply: rule.
			self genericMany: [
				self apply: #token withArgs: {delim}.
				self apply: rule
			] into: (OrderedCollection with: x)
		].
		[
			#()
		]
	}! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:07'!
lower

	| x |
	x := self apply: #char.
	self pred: x isLowercase.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 13:54'!
notLast

	| rule ans |
	rule := self apply: #anything.
	ans := self apply: rule.
	self lookahead: [self apply: rule].
	^ ans! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:02'!
number

	| x |
	x := self apply: #anything.
	self pred: x isNumber.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 15:42'!
range

	| a b x |
	a := self apply: #anything.
	b := self apply: #anything.
	x := self apply: #anything.
	self pred: (a <= x & x <= b).
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 13:44'!
space

	| x |
	x := self apply: #char.
	self pred: x asciiValue <= 32.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 13:43'!
spaces

	^ self many: [self apply: #space]! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:02'!
string

	| x |
	x := self apply: #anything.
	self pred: x isString.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 11:33'!
symbol

	| x |
	x := self apply: #anything.
	self pred: x isSymbol.
	^ x! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 13:46'!
token

	| t |
	t := self apply: #anything.
	self apply: #spaces.
	^ self apply: #seq withArgs: {t}! !

!OMeta2 methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 14:07'!
upper

	| x |
	x := self apply: #char.
	self pred: x isUppercase.
	^ x! !
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:28 pm'!
OMeta2NullOpt subclass: #OMeta2AndOrOpt
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

!OMeta2AndOrOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:46'!
and =

	trans:x end setHelped -> [x]
|	transInside(#And):xs -> [xs addFirst: #And; yourself]! !

!OMeta2AndOrOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:46'!
or =

	trans:x end setHelped -> [x]
|	transInside(#Or):xs -> [xs addFirst: #Or; yourself]! !

!OMeta2AndOrOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:36'!
transInside :t =

	{exactly(t) transInside(t):xs} transInside(t):ys setHelped -> [xs, ys]
|	trans:x transInside(t):xs -> [xs addFirst: x; yourself]
|	empty -> [OrderedCollection new]! !
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:27 pm'!
OMeta2 subclass: #OMeta2Examples
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!
!OMeta2Examples commentStamp: '<historical>' prior: 0!
Here's how to run these examples:

	OMeta2Examples match: 5 with: #fact.
	OMeta2Examples matchAll: '1234' with: #number.
	OMeta2Examples matchAll: 'abc123' with: #identifier.
	OMeta2Examples matchAll: #($a $b $c 1 2 3 #(4 5)) with: #structure.
	OMeta2Examples matchAll: 'howdy' with: #greeting.!


!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 02:36'!
anotherSqueakMethod

	self inform: 'good-bye world'! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 02:35'!
aSqueakMethod

	self inform: 'hello world'! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:58'!
digit =

	// this example shows the new syntax for super rule applications
	// think of the ^ as an arrow that's pointing up to the parent grammar

	^digit:d -> [d digitValue]! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:58'!
fact 0 = [1],
fact _:n = fact(n - 1):m -> [n * m]

// this example shows that rules can have multiple definitions (they are tried in order)
! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 12:04'!
formTest =

	{[self inform: input hash printString] #foo #bar}
|	{[self inform: input hash printString] #bar}! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:36'!
greeting = <``howdy'' ``-ho''?>! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:45'!
identifier =

	// this is an example of the new <> operator

	spaces <letter (letter | digit)*>! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:48'!
identifierIdx =

	// this is an example of the new @<> operator

	spaces @<letter (letter | digit)*>! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:54'!
identifiers =

	([self inform: self pos printString] identifier)*! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:46'!
number =

	// this is an example of left recursion support

	number:n digit:d -> [n * 10 + d]
|	digit! !

!OMeta2Examples methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:46'!
structure =

	// this example shows how the {}s are used for matching sequenceable collections

	$a $b $c 1 2 3 {anything*}! !
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:28 pm'!

'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:24 pm'!
OMeta2 subclass: #OMeta2Flattener
	instanceVariableNames: 'ws'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

!OMeta2Flattener methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:06'!
flatten

	| tree |
	tree := self apply: #anything.
	self apply: #iFlatten withArgs: {tree}.
	^ ws contents! !

!OMeta2Flattener methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:32'!
iFlatten

	^ self ometaOr: {
		[
			| s |
			s := self apply: #string.
			ws nextPutAll: s
		].
		[
			self form: [
				self many: [self apply: #iFlatten]
			]
		]
	}! !

!OMeta2Flattener methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:05'!
initialize

	super initialize.
	ws := (String new: 64) writeStream! !
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:28 pm'!
OMeta2 subclass: #OMeta2NullOpt
	instanceVariableNames: 'didSomething'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
and = trans*:xs -> [xs addFirst: #And; yourself]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
consby = trans:x -> [{#ConsBy. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
form = trans:x -> [{#Form. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:01'!
helped = ?[didSomething]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
idxconsby = trans:x -> [{#IdxConsBy. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:30'!
initialize

	super initialize.
	didSomething := false! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
lookahead = trans:x -> [{#Lookahead. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
many = trans:x -> [{#Many. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:44'!
many1 = trans:x -> [{#Many1. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
not = trans:x -> [{#Not. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
opt = trans:x -> [{#Opt. x}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:02'!
optimize = trans:x helped -> [x]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
or = trans*:xs -> [xs addFirst: #Or; yourself]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
rule = _:name _:temps trans:body -> [{#Rule. name. temps. body}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:45'!
set = _:name trans:val -> [{#Set. name. val}]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:00'!
setHelped = [didSomething := true]! !

!OMeta2NullOpt methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:42'!
trans =

	{:t [t asLowercase asSymbol]:t ?[self class canUnderstand: t] apply(t):ans} -> [ans]
|	_! !
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:29 pm'!
OMeta2 subclass: #OMeta2Optimizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

!OMeta2Optimizer methodsFor: 'as yet unclassified' stamp: 'aw 5/20/2009 12:53'!
optimizeRule = _:ans (foreign(OMeta2AndOrOpt. #optimize. ans):ans)* -> [ans]! !
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:25 pm'!
OMeta2 subclass: #OMeta2RuleParser
	instanceVariableNames: 'temps'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 13:58'!
application

	| kindOfApply rule args |
	self ometaOr: {
		[
			self apply: #token withArgs: {'^'}.
			kindOfApply := #SuperApp
		].
		[
			kindOfApply := #App
		]
	}.
	rule := self apply: #name.
	args := self apply: #args.
	^ (OrderedCollection with: kindOfApply)
		add: rule;
		addAll: args;
		yourself! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 14:03'!
args

	| ans |
	^ self ometaOr: {
		[
			self apply: #exactly withArgs: {$(}.
			ans := self apply: #listOf withArgs: {#squeakExpression. '.'}.
			self apply: #token withArgs: {')'}.
			ans
		].
		[
			self not: [self apply: #exactly withArgs: {$(}].
			#()
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:53'!
characterLiteral

	| c |
	self apply: #spaces.
	self apply: #exactly withArgs: {$$}.
	c := self apply: #char.
	^ {#App. #exactly. c storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 13:17'!
characters

	| xs |
	self apply: #token withArgs: {'``'}.
	xs := self many: [
		self not: [
			self apply: #exactly withArgs: {$'}.
			self apply: #exactly withArgs: {$'}.
		].
		self apply: #char
	].
	self apply: #exactly withArgs: {$'}.
	self apply: #exactly withArgs: {$'}.
	^ {#App. #seq. (String withAll: xs) storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 13:33'!
expr

	| xs |
	xs := self apply: #listOf withArgs: {#expr4. '|'}.
	^ (OrderedCollection with: #Or)
		addAll: xs;
		yourself! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/23/2009 13:55'!
expr1

	| lit e |
	^ self ometaOr: {
		[
			lit := self ometaOr: {
				[self apply: #keyword withArgs: {'true'}].
				[self apply: #keyword withArgs: {'false'}].
				[self apply: #keyword withArgs: {'nil'}]
			}.
			{#App. #exactly. lit}
		].
		[self apply: #application].
		[self apply: #semanticAction].
		[self apply: #semanticPredicate].
		[self apply: #characters].
		[self apply: #tokenSugar].
		[self apply: #stringLiteral].
		[self apply: #symbolLiteral].
		[self apply: #numberLiteral].
		[self apply: #characterLiteral].
		[
			self apply: #token withArgs: {'{'}.
			e := self apply: #expr.
			self apply: #token withArgs: {'}'}.
			{#Form. e}
		].
		[
			self apply: #token withArgs: {'<'}.
			e := self apply: #expr.
			self apply: #token withArgs: {'>'}.
			{#ConsBy. e}
		].
		[
			self apply: #token withArgs: {'@<'}.
			e := self apply: #expr.
			self apply: #token withArgs: {'>'}.
			{#IdxConsBy. e}
		].
		[
			self apply: #token withArgs: {'('}.
			e := self apply: #expr.
			self apply: #token withArgs: {')'}.
			e
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 13:24'!
expr2

	| x |
	^ self ometaOr: {
		[
			self apply: #token withArgs: {'~'}.
			x := self apply: #expr2.
			{#Not. x}
		].
		[
			self apply: #token withArgs: {'&'}.
			x := self apply: #expr2.
			{#Lookahead. x}
		].
		[
			self apply: #expr1
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:27'!
expr3

	| x n |
	^ self ometaOr: {
		[
			x := self apply: #expr2.
			x := self apply: #optIter withArgs: {x}.
			self ometaOr: {
				[
					self apply: #exactly withArgs: {$:}.
					n := self apply: #nsName.
					temps add: n.
					{#Set. n. x}
				].
				[
					x
				]
			}
		].
		[
			self apply: #token withArgs: {':'}.
			n := self apply: #nsName.
			temps add: n.
			{#Set. n. {#App. #anything}}
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 13:56'!
expr4

	| xs |
	xs := self many: [self apply: #expr3].
	^ (OrderedCollection with: #And)
		addAll: xs;
		yourself! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:04'!
initialize

	super initialize.
	temps := IdentitySet new! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 12:46'!
keyword

	| xs |
	xs := self apply: #anything.
	self apply: #token withArgs: {xs}.
	self not: [self apply: #letterOrDigit].
	^ xs! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 16:09'!
name

	self apply: #spaces.
	^ self apply: #nsName! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 01:17'!
nameFirst

	^ self apply: #letter! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 16:08'!
nameRest

	^ self ometaOr: {
		[self apply: #nameFirst].
		[self apply: #digit]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/23/2009 10:50'!
nsName

	| xs |
	^ self ometaOr: {
		[
			xs := self apply: #firstAndRest withArgs: {#nameFirst. #nameRest}.
			(String withAll: xs) asSymbol
		].
		[
			self apply: #exactly withArgs: {$_}.
			^ #anything
		]
	}
! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:53'!
numberLiteral

	| sign ds |
	self apply: #spaces.
	sign := self ometaOr: {
		[
			self apply: #exactly withArgs: {$-}.
			self apply: #spaces.
			-1
		].
		[1]
	}.
	ds := self many1: [self apply: #digit].
	^ {#App. #exactly. (sign * (String withAll: ds) asNumber) storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:00'!
optIter

	| x |
	x := self apply: #anything.
	^ self ometaOr: {
		[
			self apply: #token withArgs: {'*'}.
			{#Many. x}
		].
		[
			self apply: #token withArgs: {'+'}.
			{#Many1. x}
		].
		[
			self apply: #token withArgs: {'?'}.
			self not: [self apply: #exactly withArgs: {$[}].
			{#Opt. x}
		].
		[
			x
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:47'!
rule

	| n x xs squeakCode |
	^ self ometaOr: {
		[
			self not: [
				self many: [self super: OMeta2 apply: #space withArgs: #()].
				self apply: #nsName.
				self apply: #expr4.
				self apply: #token withArgs: {'='}
			].
			squeakCode := self consumedBy: [self many: [self apply: #char]].
			{#Squeak. squeakCode}
		].
		[
			n := self lookahead: [
				self many: [self super: OMeta2 apply: #space withArgs: #()].
				self apply: #nsName
			].
			x := self apply: #rulePart withArgs: {n}.
			xs := self many: [
				self apply: #token withArgs: {','}.
				self apply: #rulePart withArgs: {n}
			].
			self apply: #spaces.
			self apply: #end.
			{#Rule. n. temps asArray. (OrderedCollection with: #Or with: x) addAll: xs; yourself}
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/17/2009 16:25'!
rulePart

	| ruleName b1 b2 |
	ruleName := self apply: #anything.
	self pred: (self apply: #name) = ruleName.
	b1 := self apply: #expr4.
	^ self ometaOr: {
		[
			self apply: #token withArgs: {'='}.
			b2 := self apply: #expr.
			{#And. b1. b2}
		].
		[
			b1
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 12:05'!
semanticAction

	| e |
	self ometaOr: {
		[self apply: #token withArgs: {'->'}].
		[]
	}.
	self apply: #token withArgs: {'['}.
	e := self apply: #squeakExpression.
	self apply: #exactly withArgs: {$]}.
	^ {#Act. e}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 10:58'!
semanticPredicate

	| e |
	self apply: #token withArgs: {'?['}.
	e := self apply: #squeakExpression.
	self apply: #exactly withArgs: {$]}.
	^ {#Pred. e}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/27/2009 15:49'!
space

	^ self ometaOr: {
		[self super: OMeta2 apply: #space withArgs: #()].
		[self apply: #fromTo withArgs: {'/*'. '*/'}].
		[self apply: #fromTo withArgs: {'//'. String cr}].
		[
			self apply: #seq withArgs: {'//'}.
			self many: [self super: OMeta2 apply: #space withArgs: #()]
		]
	}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/25/2009 11:08'!
squeakExpression =

	foreign(O2SqueakRecognizer. #squeakExpr):ans spaces -> [ans]! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:53'!
stringLiteral

	| xs |
	self apply: #token withArgs: {''''}.
	xs := self many: [
		self ometaOr: {
			[
				self apply: #exactly withArgs: {$'}.
				self apply: #exactly withArgs: {$'}.
				$'
			].
			[
				self not: [self apply: #exactly withArgs: {$'}].
				self apply: #char
			]
		}
	].
	self apply: #exactly withArgs: {$'}.
	^ {#App. #exactly. (String withAll: xs) storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:54'!
symbolLiteral

	" TODO: make this accept, e.g., #foo:bar: "
	| s |
	self apply: #token withArgs: {'#'}.
	s := self apply: #nsName.
	^ {#App. #exactly. s storeString}! !

!OMeta2RuleParser methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:33'!
tokenSugar

	| xs |
	self apply: #token withArgs: {'"'}.
	xs := self many: [
		self not: [self apply: #exactly withArgs: {$"}].
		self apply: #char
	].
	self apply: #exactly withArgs: {$"}.
	^ {#App. #token. (String withAll: xs) storeString}! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

OMeta2RuleParser class
	instanceVariableNames: ''!

!OMeta2RuleParser class methodsFor: 'as yet unclassified' stamp: 'aw 2/23/2009 10:07'!
isOMeta2Rule: aString

	^ [(self matchAll: aString with: #rule) first ~= #Squeak] on: OM2Fail do: [false]! !
'From Moshi of 3 March 2007 [latest update: #908] on 21 May 2009 at 2:14:26 pm'!
OMeta2 subclass: #OMeta2RuleTranslator
	instanceVariableNames: 'grammarClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OMeta2'!

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:22'!
act

	| x |
	x := self apply: #string.
	^ {'('. x. ')'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/26/2009 11:06'!
and

	| xs |
	xs := self many: [self apply: #trans.].
	^ {'(true ifTrue: ['. self delim: xs with: '. '. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:29'!
app

	| rule args |
	rule := self apply: #symbol.
	^ self ometaOr: {
		[
			args := self many1: [self apply: #anything].
			args := self delim: args with: '. '.
			{'(self apply: '. rule storeString. ' withArgs: {'. args. '})'}
		].
		[
			{'(self apply: '. rule storeString. ')'}
		]
	}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/19/2009 23:34'!
consby

	| x |
	x := self apply: #trans.
	^ {'(self consumedBy: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 14:43'!
delim: aSequenceableCollection  with: anObject

	| first ans |
	
	first := true.
	ans := OrderedCollection new.
	aSequenceableCollection do: [:x |
		first ifTrue: [first := false] ifFalse: [ans add: anObject].
		ans add: x
	].
	^ ans! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:06'!
form

	| x |
	x := self apply: #trans.
	^ {'(self form: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:56'!
idxconsby

	| x |
	x := self apply: #trans.
	^ {'(self indexConsumedBy: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:07'!
lookahead

	| x |
	x := self apply: #trans.
	^ {'(self lookahead: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:10'!
many

	| x |
	x := self apply: #trans.
	^ {'(self many: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:10'!
many1

	| x |
	x := self apply: #trans.
	^ {'(self many1: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:08'!
not

	| x |
	x := self apply: #trans.
	^ {'(self not: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 14:35'!
opt

	| x |
	x := self apply: #trans.
	^ {'(self opt: ['. x. '])'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:51'!
or

	| xs x |
	xs := self many: [
		x := self apply: #trans.
		{'['. x. ']'}
	].
	^ {'(self ometaOr: {'. self delim: xs with: '. '. '})'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:22'!
pred

	| x |
	x := self apply: #string.
	^ {'(self pred: ('. x. '))'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 19:20'!
rule

	| name temps body |
	name := self apply: #symbol.
	temps := self apply: #anything.
	temps := temps select: [:t | (grammarClass instVarNames includes: t) not].
	temps := temps collect: [:t | t asString].
	body := self apply: #trans.
	^ {name. ' |'. self delim: temps with: ' '. ' | ^ '. body}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 16:09'!
set

	| n v |
	n := self apply: #symbol.
	v := self apply: #trans.
	^ {'('. n asString. ' := '. v. ')'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/20/2009 00:07'!
squeak

	^ self apply: #string! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:46'!
superapp

	| rule args |
	rule := self apply: #symbol.
	args := self many: [self apply: #anything].
	args := self delim: args with: '. '.
	^ {'(self super: '. grammarClass superclass name. ' apply: '. rule storeString. ' withArgs: {'. args. '})'}! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:46'!
trans

	| type ans |
	self form: [
		type := self apply: #symbol.
		ans := self apply: type asLowercase asSymbol
	].
	^ ans! !

!OMeta2RuleTranslator methodsFor: 'as yet unclassified' stamp: 'aw 2/18/2009 15:35'!
translate

	grammarClass := self apply: #anything.
	^ self apply: #trans! !




"-- END OF STAGE 2 BOOTSTRAP --"!