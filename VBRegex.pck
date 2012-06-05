'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 4 June 2012 at 5:07:55 pm'!
'Description Based on http://www.squeaksource.com/Regex/VB-Regex-damienpollet.17.mcz

Tests have been moved to VBRegexTest'!
!classDefinition: #RegexError category: #'VBRegex-Exceptions'!
Error subclass: #RegexError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex-Exceptions'!
!classDefinition: 'RegexError class' category: #'VBRegex-Exceptions'!
RegexError class
	instanceVariableNames: ''!

!classDefinition: #RegexCompilationError category: #'VBRegex-Exceptions'!
RegexError subclass: #RegexCompilationError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex-Exceptions'!
!classDefinition: 'RegexCompilationError class' category: #'VBRegex-Exceptions'!
RegexCompilationError class
	instanceVariableNames: ''!

!classDefinition: #RegexMatchingError category: #'VBRegex-Exceptions'!
RegexError subclass: #RegexMatchingError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex-Exceptions'!
!classDefinition: 'RegexMatchingError class' category: #'VBRegex-Exceptions'!
RegexMatchingError class
	instanceVariableNames: ''!

!classDefinition: #RegexSyntaxError category: #'VBRegex-Exceptions'!
RegexError subclass: #RegexSyntaxError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex-Exceptions'!
!classDefinition: 'RegexSyntaxError class' category: #'VBRegex-Exceptions'!
RegexSyntaxError class
	instanceVariableNames: ''!

!classDefinition: #RxCharSetParser category: #VBRegex!
Object subclass: #RxCharSetParser
	instanceVariableNames: 'source lookahead elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxCharSetParser class' category: #VBRegex!
RxCharSetParser class
	instanceVariableNames: ''!

!classDefinition: #RxMatchOptimizer category: #VBRegex!
Object subclass: #RxMatchOptimizer
	instanceVariableNames: 'ignoreCase prefixes nonPrefixes conditions testBlock methodPredicates nonMethodPredicates predicates nonPredicates'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxMatchOptimizer class' category: #VBRegex!
RxMatchOptimizer class
	instanceVariableNames: ''!

!classDefinition: #RxMatcher category: #VBRegex!
Object subclass: #RxMatcher
	instanceVariableNames: 'matcher ignoreCase startOptimizer stream markerPositions markerCount lastResult lastChar'
	classVariableNames: 'Cr Lf'
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxMatcher class' category: #VBRegex!
RxMatcher class
	instanceVariableNames: ''!

!classDefinition: #RxParser category: #VBRegex!
Object subclass: #RxParser
	instanceVariableNames: 'input lookahead'
	classVariableNames: 'BackslashConstants BackslashSpecials ExceptionObjects'
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxParser class' category: #VBRegex!
RxParser class
	instanceVariableNames: ''!

!classDefinition: #RxmLink category: #VBRegex!
Object subclass: #RxmLink
	instanceVariableNames: 'next'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxmLink class' category: #VBRegex!
RxmLink class
	instanceVariableNames: ''!

!classDefinition: #RxmBranch category: #VBRegex!
RxmLink subclass: #RxmBranch
	instanceVariableNames: 'loopback alternative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxmBranch class' category: #VBRegex!
RxmBranch class
	instanceVariableNames: ''!

!classDefinition: #RxmMarker category: #VBRegex!
RxmLink subclass: #RxmMarker
	instanceVariableNames: 'index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxmMarker class' category: #VBRegex!
RxmMarker class
	instanceVariableNames: ''!

!classDefinition: #RxmPredicate category: #VBRegex!
RxmLink subclass: #RxmPredicate
	instanceVariableNames: 'predicate'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxmPredicate class' category: #VBRegex!
RxmPredicate class
	instanceVariableNames: ''!

!classDefinition: #RxmSpecial category: #VBRegex!
RxmLink subclass: #RxmSpecial
	instanceVariableNames: 'matchSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxmSpecial class' category: #VBRegex!
RxmSpecial class
	instanceVariableNames: ''!

!classDefinition: #RxmSubstring category: #VBRegex!
RxmLink subclass: #RxmSubstring
	instanceVariableNames: 'sample compare'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxmSubstring class' category: #VBRegex!
RxmSubstring class
	instanceVariableNames: ''!

!classDefinition: #RxmTerminator category: #VBRegex!
Object subclass: #RxmTerminator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxmTerminator class' category: #VBRegex!
RxmTerminator class
	instanceVariableNames: ''!

!classDefinition: #RxsNode category: #VBRegex!
Object subclass: #RxsNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsNode class' category: #VBRegex!
RxsNode class
	instanceVariableNames: ''!

!classDefinition: #RxsBranch category: #VBRegex!
RxsNode subclass: #RxsBranch
	instanceVariableNames: 'piece branch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsBranch class' category: #VBRegex!
RxsBranch class
	instanceVariableNames: ''!

!classDefinition: #RxsCharSet category: #VBRegex!
RxsNode subclass: #RxsCharSet
	instanceVariableNames: 'negated elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsCharSet class' category: #VBRegex!
RxsCharSet class
	instanceVariableNames: ''!

!classDefinition: #RxsCharacter category: #VBRegex!
RxsNode subclass: #RxsCharacter
	instanceVariableNames: 'character'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsCharacter class' category: #VBRegex!
RxsCharacter class
	instanceVariableNames: ''!

!classDefinition: #RxsContextCondition category: #VBRegex!
RxsNode subclass: #RxsContextCondition
	instanceVariableNames: 'kind'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsContextCondition class' category: #VBRegex!
RxsContextCondition class
	instanceVariableNames: ''!

!classDefinition: #RxsEpsilon category: #VBRegex!
RxsNode subclass: #RxsEpsilon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsEpsilon class' category: #VBRegex!
RxsEpsilon class
	instanceVariableNames: ''!

!classDefinition: #RxsMessagePredicate category: #VBRegex!
RxsNode subclass: #RxsMessagePredicate
	instanceVariableNames: 'selector negated'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsMessagePredicate class' category: #VBRegex!
RxsMessagePredicate class
	instanceVariableNames: ''!

!classDefinition: #RxsPiece category: #VBRegex!
RxsNode subclass: #RxsPiece
	instanceVariableNames: 'atom min max'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsPiece class' category: #VBRegex!
RxsPiece class
	instanceVariableNames: ''!

!classDefinition: #RxsPredicate category: #VBRegex!
RxsNode subclass: #RxsPredicate
	instanceVariableNames: 'predicate negation'
	classVariableNames: 'EscapedLetterSelectors NamedClassSelectors'
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsPredicate class' category: #VBRegex!
RxsPredicate class
	instanceVariableNames: ''!

!classDefinition: #RxsRange category: #VBRegex!
RxsNode subclass: #RxsRange
	instanceVariableNames: 'first last'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsRange class' category: #VBRegex!
RxsRange class
	instanceVariableNames: ''!

!classDefinition: #RxsRegex category: #VBRegex!
RxsNode subclass: #RxsRegex
	instanceVariableNames: 'branch regex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VBRegex'!
!classDefinition: 'RxsRegex class' category: #VBRegex!
RxsRegex class
	instanceVariableNames: ''!


!RxCharSetParser commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
I am a parser created to parse the insides of a character set ([...]) construct. I create and answer a collection of "elements", each being an instance of one of: RxsCharacter, RxsRange, or RxsPredicate.
Instance Variables:
	source	<Stream>	open on whatever is inside the square brackets we have to parse.
	lookahead	<Character>	The current lookahead character
	elements	<Collection of: <RxsCharacter|RxsRange|RxsPredicate>> Parsing result!

!RxMatchOptimizer commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A match start optimizer, handy for searching a string. Takes a regex syntax tree and sets itself up so that prefix characters or matcher states that cannot start a match are later recognized with #canStartMatch:in: method.
Used by RxMatcher, but can be used by other matchers (if implemented) as well.!

!RxMatcher commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
This is a recursive regex matcher. Not strikingly efficient, but simple. Also, keeps track of matched subexpressions.  The life cycle goes as follows:
1. Initialization. Accepts a syntax tree (presumably produced by RxParser) and compiles it into a matcher built of other classes in this category.
2. Matching. Accepts a stream or a string and returns a boolean indicating whether the whole stream or its prefix -- depending on the message sent -- matches the regex.
3. Subexpression query. After a successful match, and before any other match, the matcher may be queried about the range of specific stream (string) positions that matched to certain parenthesized subexpressions of the original expression.
Any number of queries may follow a successful match, and any number or matches may follow a successful initialization.
Note that `matcher' is actually a sort of a misnomer. The actual matcher is a web of Rxm* instances built by RxMatcher during initialization. RxMatcher is just the interface facade of this network.  It is also a builder of it, and also provides a stream-like protocol to easily access the stream being matched.
Instance variables:
	matcher				<RxmLink> The entry point into the actual matcher.
	stream				<Stream> The stream currently being matched against.
	markerPositions		<Array of: Integer> Positions of markers' matches.
	markerCount		<Integer> Number of markers.
	lastResult 			<Boolean> Whether the latest match attempt succeeded or not.
	lastChar			<Character | nil> character last seen in the matcher stream!

!RxParser commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
The regular expression parser. Translates a regular expression read from a stream into a parse tree. ('accessing' protocol). The tree can later be passed to a matcher initialization method.  All other classes in this category implement the tree. Refer to their comments for any details.
Instance variables:
	input		<Stream> A stream with the regular expression being parsed.
	lookahead	<Character>!

!RxmBranch commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
This is a branch of a matching process. Either `next' chain should match, or `alternative', if not nil, should match. Since this is also used to build loopbacks to match repetitions, `loopback' variable indicates whether the instance is a loopback: it affects the matcher-building operations (which of the paths through the branch is to consider as the primary when we have to find the "tail" of a matcher construct).
Instance variables
	alternative		<RxmLink> to match if `next' fails to match.
	loopback		<Boolean>!

!RxmLink commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A matcher is built of a number of links interconnected into some intricate structure. Regardless of fancy stuff, any link (except for the terminator) has the next one. Any link can match against a stream of characters, recursively propagating the match to the next link. Any link supports a number of matcher-building messages. This superclass does all of the above. 
The class is not necessarily abstract. It may double as an empty string matcher: it recursively propagates the match to the next link, thus always matching nothing successfully.
Principal method:
	matchAgainst: aMatcher
		Any subclass will reimplement this to test the state of the matcher, most
		probably reading one or more characters from the matcher's stream, and
		either decide it has matched and answer true, leaving matcher stream
		positioned at the end of match, or answer false and restore the matcher
		stream position to whatever it was before the matching attempt.
Instance variables:
	next		<RxmLink | RxmTerminator> The next link in the structure.!

!RxmMarker commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A marker is used to remember positions of match of certain points of a regular expression. The marker receives an identifying key from the Matcher and uses that key to report positions of successful matches to the Matcher.
Instance variables:
	index	<Object> Something that makes sense for the Matcher. Received from the latter during initalization and later passed to it to identify the receiver.!

!RxmPredicate commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
Instance holds onto a one-argument block and matches exactly one character if the block evaluates to true when passed the character as the argument.
Instance variables:
	predicate		<BlockClosure>!

!RxmSpecial commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A special node that matches a specific matcher state rather than any input character.
The state is either at-beginning-of-line or at-end-of-line.!

!RxmSubstring commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
Instance holds onto a string and matches exactly this string, and exactly once.
Instance variables:
	string 	<String>!

!RxmTerminator commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
Instances of this class are used to terminate matcher's chains. When a match reaches this (an instance receives #matchAgainst: message), the match is considered to succeed. Instances also support building protocol of RxmLinks, with some restrictions.!

!RxsBranch commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A Branch is a Piece followed by a Branch or an empty string.
Instance variables:
	piece		<RxsPiece>
	branch		<RxsBranch|RxsEpsilon>!

!RxsCharSet commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A character set corresponds to a [...] construct in the regular expression.
Instance variables:
	elements	<OrderedCollection> An element can be one of: RxsCharacter, RxsRange, or RxsPredicate.
	negated		<Boolean>!

!RxsCharacter commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A character is a literal character that appears either in the expression itself or in a character set within an expression.
Instance variables:
	character		<Character>!

!RxsContextCondition commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
One of a few special nodes more often representing special state of the match rather than a predicate on a character.  The ugly exception is the #any condition which *is* a predicate on a character.
Instance variables:
	kind		<Selector>!

!RxsEpsilon commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
This is an empty string.  It terminates some of the recursive constructs.!

!RxsMessagePredicate commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A message predicate represents a condition on a character that is tested (at the match time) by sending a unary message to the character expecting a Boolean answer.
Instance variables:
	selector		<Symbol>!

!RxsNode commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A generic syntax tree node, provides some common responses to the standard tests, as well as tree structure printing -- handy for debugging.!

!RxsPiece commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
A piece is an atom, possibly optional or repeated a number of times.
Instance variables:
	atom	<RxsCharacter|RxsCharSet|RxsPredicate|RxsRegex|RxsSpecial>
	min		<Integer>
	max	<Integer|nil> nil means infinity!

!RxsPredicate commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
This represents a character that satisfies a certain predicate.
Instance Variables:
	predicate	<BlockClosure>	A one-argument block. If it evaluates to the value defined by <negated> when it is passed a character, the predicate is considered to match.
	negation	<BlockClosure>	A one-argument block that is a negation of <predicate>.!

!RxsRange commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
I represent a range of characters as appear in character classes such as
	[a-ZA-Z0-9].
I appear in a syntax tree only as an element of RxsCharSet.
Instance Variables:
	first	<Character>
	last	<Character>!

!RxsRegex commentStamp: '<historical>' prior: 0!
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
-- See `documentation' protocol of RxParser class for user's guide.
--
The body of a parenthesized thing, or a top-level expression, also an atom.  
Instance variables:
	branch		<RxsBranch>
	regex		<RxsRegex | RxsEpsilon>!

!Character methodsFor: '*vbregex' stamp: 'avi 11/30/2003 13:31'!
isAlphabetic
	^ self isLetter! !

!RxCharSetParser methodsFor: 'parsing'!
addChar: aChar
	elements add: (RxsCharacter with: aChar)! !

!RxCharSetParser methodsFor: 'parsing'!
addRangeFrom: firstChar to: lastChar
	firstChar asInteger > lastChar asInteger ifTrue:
		[RxParser signalSyntaxException: ' bad character range'].
	elements add: (RxsRange from: firstChar to: lastChar)! !

!RxCharSetParser methodsFor: 'initialize-release'!
initialize: aStream
	source := aStream.
	lookahead := aStream next.
	elements := OrderedCollection new! !

!RxCharSetParser methodsFor: 'parsing'!
match: aCharacter
	aCharacter = lookahead
		ifFalse: [RxParser signalSyntaxException: 'unexpected character: ', (String with: lookahead)].
	^source atEnd
		ifTrue: [lookahead := nil]
		ifFalse: [lookahead := source next]! !

!RxCharSetParser methodsFor: 'accessing'!
parse
	lookahead = $- ifTrue:
		[self addChar: $-.
		self match: $-].
	[lookahead isNil] whileFalse: [self parseStep].
	^elements! !

!RxCharSetParser methodsFor: 'parsing'!
parseCharOrRange
	| firstChar |
	firstChar := lookahead.
	self match: firstChar.
	lookahead = $- ifTrue:
		[self match: $-.
		lookahead isNil
			ifTrue: [^self addChar: firstChar; addChar: $-]
			ifFalse: 
				[self addRangeFrom: firstChar to: lookahead.
				^self match: lookahead]].
	self addChar: firstChar! !

!RxCharSetParser methodsFor: 'parsing'!
parseEscapeChar
	self match: $\.
	$- = lookahead
		ifTrue: [elements add: (RxsCharacter with: $-)]
		ifFalse: [elements add: (RxsPredicate forEscapedLetter: lookahead)].
	self match: lookahead! !

!RxCharSetParser methodsFor: 'parsing'!
parseNamedSet
	| name |
	self match: $[; match: $:.
	name := (String with: lookahead), (source upTo: $:).
	lookahead := source next.
	self match: $].
	elements add: (RxsPredicate forNamedClass: name)! !

!RxCharSetParser methodsFor: 'parsing'!
parseStep
	lookahead = $[ ifTrue:
		[source peek = $:
			ifTrue: [^self parseNamedSet]
			ifFalse: [^self parseCharOrRange]].
	lookahead = $\ ifTrue:
		[^self parseEscapeChar].
	lookahead = $- ifTrue:
		[RxParser signalSyntaxException: 'invalid range'].
	self parseCharOrRange! !

!RxCharSetParser class methodsFor: 'instance creation'!
on: aStream
	^self new initialize: aStream! !

!RxMatchOptimizer methodsFor: 'accessing'!
canStartMatch: aCharacter in: aMatcher 
	"Answer whether a match could commence at the given lookahead
	character, or in the current state of <aMatcher>. True answered
	by this method does not mean a match will definitly occur, while false
	answered by this method *does* guarantee a match will never occur."
	aCharacter isNil ifTrue: [^true].
	^testBlock == nil or: [testBlock value: aCharacter value: aMatcher]! !

!RxMatchOptimizer methodsFor: 'accessing' stamp: 'avi 11/30/2003 13:28'!
conditionTester
	"#any condition is filtered at the higher level;
	it cannot appear among the conditions here."
	| matchCondition |
	conditions isEmpty ifTrue: [^nil].
	conditions size = 1 ifTrue:
		[matchCondition := conditions detect: [:ignored | true].
		"Special case all of the possible conditions."
		#atBeginningOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfLine]].
		#atEndOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfLine]].
		#atBeginningOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfWord]].
		#atEndOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfWord]].
		#atWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher atWordBoundary]].
		#notAtWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher notAtWordBoundary]].
		RxParser signalCompilationException: 'invalid match condition'].
	"More than one condition. Capture them as an array in scope."
	matchCondition := conditions asArray.
	^[:c :matcher |
		matchCondition anySatisfy:
			[:conditionSelector |
			matcher perform: conditionSelector]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'avi 11/30/2003 13:27'!
determineTestMethod
	"Answer a block closure that will work as a can-match predicate.
	Answer nil if no viable optimization is possible (too many chars would
	be able to start a match)."
	| testers |
	(conditions includes: #any) ifTrue: [^nil].
	testers := OrderedCollection new: 5.
	#(#prefixTester #nonPrefixTester #conditionTester #methodPredicateTester #nonMethodPredicateTester #predicateTester #nonPredicateTester)
		do: 
			[:selector | 
			| tester |
			tester := self perform: selector.
			tester notNil ifTrue: [testers add: tester]].
	testers isEmpty ifTrue: [^nil].
	testers size = 1 ifTrue: [^testers first].
	testers := testers asArray.
	^[:char :matcher | testers anySatisfy: [:t | t value: char value: matcher]]! !

!RxMatchOptimizer methodsFor: 'initialize-release'!
initialize: aRegex ignoreCase: aBoolean 
	"Set `testMethod' variable to a can-match predicate block:
	two-argument block which accepts a lookahead character
	and a matcher (presumably built from aRegex) and answers 
	a boolean indicating whether a match could start at the given
	lookahead. "
	ignoreCase := aBoolean.
	prefixes := Set new: 10.
	nonPrefixes := Set new: 10.
	conditions := Set new: 3.
	methodPredicates := Set new: 3.
	nonMethodPredicates := Set new: 3.
	predicates := Set new: 3.
	nonPredicates := Set new: 3.
	aRegex dispatchTo: self.	"If the whole expression is nullable, 
		end-of-line is an implicit can-match condition!!"
	aRegex isNullable ifTrue: [conditions add: #atEndOfLine].
	testBlock := self determineTestMethod! !

!RxMatchOptimizer methodsFor: 'accessing'!
methodPredicateTester
	| p selector |
	methodPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: methodPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			["might be a pretty common case"
			selector := p first.
			[:char :matcher | 
			RxParser doHandlingMessageNotUnderstood:
				[char perform: selector]]]
		ifFalse: 
			[[:char :m | 
			RxParser doHandlingMessageNotUnderstood:
				[p contains: [:sel | char perform: sel]]]]! !

!RxMatchOptimizer methodsFor: 'accessing'!
nonMethodPredicateTester
	| p selector |
	nonMethodPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: nonMethodPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			[selector := p first.
			[:char :matcher | 
			RxParser doHandlingMessageNotUnderstood:
				[(char perform: selector) not]]]
		ifFalse: 
			[[:char :m | 
			RxParser doHandlingMessageNotUnderstood:
				[p contains: [:sel | (char perform: sel) not]]]]! !

!RxMatchOptimizer methodsFor: 'private'!
nonPredicateTester
	| p pred |
	nonPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: nonPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			[pred := p first.
			[:char :matcher | (pred value: char) not]]
		ifFalse: 
			[[:char :m | p contains: [:some | (some value: char) not]]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'stephane.ducasse 4/13/2009 20:32'!
nonPrefixTester
	| np nonPrefixChar |
	nonPrefixes isEmpty ifTrue: [^nil].
	np := self optimizeSet: nonPrefixes. "also allows copying closures"
	^np size = 1 "might be be pretty common case"
		ifTrue: 
			[nonPrefixChar := np first.
			[:char :matcher | char ~= nonPrefixChar]]
		ifFalse: [[:char :matcher | (np includes: char) not]]! !

!RxMatchOptimizer methodsFor: 'private'!
optimizeSet: aSet
	"If a set is small, convert it to array to speed up lookup
	(Array has no hashing overhead, beats Set on small number
	of elements)."
	^aSet size < 10 ifTrue: [aSet asArray] ifFalse: [aSet]! !

!RxMatchOptimizer methodsFor: 'private'!
predicateTester
	| p pred |
	predicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: predicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			[pred := p first.
			[:char :matcher | pred value: char]]
		ifFalse: 
			[[:char :m | p contains: [:some | some value: char]]]! !

!RxMatchOptimizer methodsFor: 'private' stamp: 'pb 6/3/2012 21:46'!
prefixTester
	| p prefixChar |
	prefixes isEmpty ifTrue: [ ^ nil ].
	p := self optimizeSet: prefixes.
	"also allows copying closures"
	ignoreCase ifTrue: [
		p := p collect: [ :each |
			each asUppercase ]].
	^ p size = 1
		ifTrue: [
			"might be a pretty common case"
			prefixChar := p first.
			ignoreCase
			"
				ifTrue: [
					[ :char :matcher |
					char sameAs: prefixChar ]]
			"
				ifTrue: [
					[ :char :matcher |
					char asLowercase = prefixChar asLowercase ]]
				ifFalse: [
					[ :char :matcher |
					char = prefixChar ]]]
		ifFalse: [
			ignoreCase
				ifTrue: [
					[ :char :matcher |
					p includes: char asUppercase ]]
				ifFalse: [
					[ :char :matcher |
					p includes: char ]]].! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxAny
	"Any special char is among the prefixes."
	conditions add: #any! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxBeginningOfLine
	"Beginning of line is among the prefixes."
	conditions add: #atBeginningOfLine! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxBeginningOfWord
	"Beginning of line is among the prefixes."
	conditions add: #atBeginningOfWord! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxBranch: branchNode
	"If the head piece of the branch is transparent (allows 0 matches),
	we must recurse down the branch. Otherwise, just the head atom
	is important."
	(branchNode piece isNullable and: [branchNode branch notNil])
		ifTrue: [branchNode branch dispatchTo: self].
	branchNode piece dispatchTo: self! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxCharSet: charSetNode 
	"All these (or none of these) characters is the prefix."
	charSetNode isNegated
		ifTrue: [nonPrefixes addAll: charSetNode enumerableSet]
		ifFalse: [prefixes addAll: charSetNode enumerableSet].
	charSetNode hasPredicates ifTrue: 
			[charSetNode isNegated
				ifTrue: [nonPredicates addAll: charSetNode predicates]
				ifFalse: [predicates addAll: charSetNode predicates]]! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxCharacter: charNode
	"This character is the prefix, of one of them."
	prefixes add: charNode character! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxEndOfLine
	"Beginning of line is among the prefixes."
	conditions add: #atEndOfLine! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxEndOfWord
	conditions add: #atEndOfWord! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxEpsilon
	"Empty string, terminate the recursion (do nothing)."! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxMessagePredicate: messagePredicateNode 
	messagePredicateNode negated
		ifTrue: [nonMethodPredicates add: messagePredicateNode selector]
		ifFalse: [methodPredicates add: messagePredicateNode selector]! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxNonWordBoundary
	conditions add: #notAtWordBoundary! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxPiece: pieceNode
	"Pass on to the atom."
	pieceNode atom dispatchTo: self! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxPredicate: predicateNode 
	predicates add: predicateNode predicate! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxRegex: regexNode
	"All prefixes of the regex's branches should be combined.
	Therefore, just recurse."
	regexNode branch dispatchTo: self.
	regexNode regex notNil
		ifTrue: [regexNode regex dispatchTo: self]! !

!RxMatchOptimizer methodsFor: 'double dispatch'!
syntaxWordBoundary
	conditions add: #atWordBoundary! !

!RxMatcher methodsFor: 'private'!
allocateMarker
	"Answer an integer to use as an index of the next marker."
	markerCount := markerCount + 1.
	^markerCount! !

!RxMatcher methodsFor: 'testing'!
atBeginningOfLine
	^self position = 0 or: [lastChar = Cr]! !

!RxMatcher methodsFor: 'testing'!
atBeginningOfWord
	^(self isWordChar: lastChar) not
		and: [self isWordChar: stream peek]! !

!RxMatcher methodsFor: 'streaming'!
atEnd
	^stream atEnd! !

!RxMatcher methodsFor: 'testing'!
atEndOfLine
	^self atEnd or: [stream peek = Cr]! !

!RxMatcher methodsFor: 'testing'!
atEndOfWord
	^(self isWordChar: lastChar)
		and: [(self isWordChar: stream peek) not]! !

!RxMatcher methodsFor: 'testing'!
atWordBoundary
	^(self isWordChar: lastChar)
		xor: (self isWordChar: stream peek)! !

!RxMatcher methodsFor: 'accessing'!
buildFrom: aSyntaxTreeRoot
	"Private - Entry point of matcher build process."
	markerCount := 0.  "must go before #dispatchTo: !!"
	matcher := aSyntaxTreeRoot dispatchTo: self.
	matcher terminateWith: RxmTerminator new! !

!RxMatcher methodsFor: 'match enumeration'!
copy: aString replacingMatchesWith: replacementString
	"Copy <aString>, except for the matches. Replace each match with <aString>."
	| answer |
	answer := (String new: 40) writeStream.
	self
		copyStream: aString readStream
		to: answer
		replacingMatchesWith: replacementString.
	^answer contents! !

!RxMatcher methodsFor: 'match enumeration'!
copy: aString translatingMatchesUsing: aBlock
	"Copy <aString>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and replace the match with the answer."
	| answer |
	answer := (String new: 40) writeStream.
	self copyStream: aString readStream to: answer translatingMatchesUsing: aBlock.
	^answer contents! !

!RxMatcher methodsFor: 'match enumeration'!
copyStream: aStream to: writeStream replacingMatchesWith: aString
	"Copy the contents of <aStream> on the <writeStream>, except for the matches. Replace each match with <aString>."
	| searchStart matchStart matchEnd |
	stream := aStream.
	lastChar := nil.
	[searchStart := aStream position.
	self proceedSearchingStream: aStream] whileTrue:
		[matchStart := self subBeginning: 1.
		matchEnd := self subEnd: 1.
		aStream position: searchStart.
		searchStart to: matchStart - 1 do:
			[:ignoredPos | writeStream nextPut: aStream next].
		writeStream nextPutAll: aString.
		aStream position: matchEnd].
	aStream position: searchStart.
	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]! !

!RxMatcher methodsFor: 'match enumeration'!
copyStream: aStream to: writeStream translatingMatchesUsing: aBlock
	"Copy the contents of <aStream> on the <writeStream>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and write the answer to <writeStream> in place of the match."
	| searchStart matchStart matchEnd match |
	stream := aStream.
	lastChar := nil.
	[searchStart := aStream position.
	self proceedSearchingStream: aStream] whileTrue:
		[matchStart := self subBeginning: 1.
		matchEnd := self subEnd: 1.
		aStream position: searchStart.
		searchStart to: matchStart - 1 do:
			[:ignoredPos | writeStream nextPut: aStream next].
		match := (String new: matchEnd - matchStart + 1) writeStream.
		matchStart to: matchEnd - 1 do:
			[:ignoredPos | match nextPut: aStream next].
		writeStream nextPutAll: (aBlock value: match contents)].
	aStream position: searchStart.
	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]! !

!RxMatcher methodsFor: 'privileged'!
currentState
	"Answer an opaque object that can later be used to restore the
	matcher's state (for backtracking)."
	| origPosition origLastChar |
	origPosition := stream position.
	origLastChar := lastChar.
	^	[stream position: origPosition.
		lastChar := origLastChar]! !

!RxMatcher methodsFor: 'private'!
hookBranchOf: regexNode onto: endMarker
	"Private - Recurse down the chain of regexes starting at
	regexNode, compiling their branches and hooking their tails 
	to the endMarker node."
	| rest |
	rest := regexNode regex isNil
		ifTrue: [nil]
		ifFalse: [self hookBranchOf: regexNode regex onto: endMarker].
	^RxmBranch new
		next: ((regexNode branch dispatchTo: self)
					pointTailTo: endMarker; 
					yourself);
		alternative: rest;
		yourself! !

!RxMatcher methodsFor: 'initialize-release'!
initialize: syntaxTreeRoot ignoreCase: aBoolean
	"Compile thyself for the regex with the specified syntax tree.
	See comment and `building' protocol in this class and 
	#dispatchTo: methods in syntax tree components for details 
	on double-dispatch building. 
	The argument is supposedly a RxsRegex."
	ignoreCase := aBoolean.
	self buildFrom: syntaxTreeRoot.
	startOptimizer := RxMatchOptimizer new initialize: syntaxTreeRoot ignoreCase: aBoolean! !

!RxMatcher methodsFor: 'private'!
isWordChar: aCharacterOrNil
	"Answer whether the argument is a word constituent character:
	alphanumeric or _."
	^aCharacterOrNil ~~ nil
		and: [aCharacterOrNil isAlphaNumeric]! !

!RxMatcher methodsFor: 'accessing'!
lastResult
	^lastResult! !

!RxMatcher methodsFor: 'private'!
makeOptional: aMatcher
	"Private - Wrap this matcher so that the result would match 0 or 1
	occurrences of the matcher."
	| dummy branch |
	dummy := RxmLink new.
	branch := (RxmBranch new beLoopback)
		next: aMatcher;
		alternative: dummy.
	aMatcher pointTailTo: dummy.
	^branch! !

!RxMatcher methodsFor: 'private'!
makePlus: aMatcher
	"Private - Wrap this matcher so that the result would match 1 and more
	occurrences of the matcher."
	| loopback |
	loopback := (RxmBranch new beLoopback)
		next: aMatcher.
	aMatcher pointTailTo: loopback.
	^aMatcher! !

!RxMatcher methodsFor: 'private'!
makeStar: aMatcher
	"Private - Wrap this matcher so that the result would match 0 and more
	occurrences of the matcher."
	| dummy detour loopback |
	dummy := RxmLink new.
	detour := RxmBranch new
		next: aMatcher;
		alternative: dummy.
	loopback := (RxmBranch new beLoopback)
		next: aMatcher;
		alternative: dummy.
	aMatcher pointTailTo: loopback.
	^detour! !

!RxMatcher methodsFor: 'privileged'!
markerPositionAt: anIndex
	^markerPositions at: anIndex! !

!RxMatcher methodsFor: 'privileged'!
markerPositionAt: anIndex maybePut: position
	"Set position of the given marker, if not already set."
	(markerPositions at: anIndex) == nil
		ifTrue:	[markerPositions at: anIndex put: position]! !

!RxMatcher methodsFor: 'accessing'!
matches: aString
	"Match against a string."
	^self matchesStream: aString readStream! !

!RxMatcher methodsFor: 'match enumeration'!
matchesIn: aString
	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection of all matches (substrings)."
	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aString readStream
		do: [:match | result add: match].
	^result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'damien.pollet 5/2/2009 23:59'!
matchesIn: aString collect: aBlock
	"Search aString repeatedly for the matches of the receiver.  Evaluate aBlock for each match passing the matched substring as the argument, collect evaluation results in an OrderedCollection, and return it. The following example shows how to use this message to split a string into words."
	"'\w+' asRegex matchesIn: 'Now is the Time' collect: [:each | each asLowercase]"
	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aString readStream
		do: [:match | result add: (aBlock value: match)].
	^result! !

!RxMatcher methodsFor: 'match enumeration'!
matchesIn: aString do: aBlock
	"Search aString repeatedly for the matches of the receiver.
	Evaluate aBlock for each match passing the matched substring
	as the argument."
	self
		matchesOnStream: aString readStream
		do: aBlock! !

!RxMatcher methodsFor: 'match enumeration'!
matchesOnStream: aStream
	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aStream
		do: [:match | result add: match].
	^result! !

!RxMatcher methodsFor: 'match enumeration'!
matchesOnStream: aStream collect: aBlock
	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aStream
		do: [:match | result add: (aBlock value: match)].
	^result! !

!RxMatcher methodsFor: 'match enumeration'!
matchesOnStream: aStream do: aBlock
	[self searchStream: aStream] whileTrue:
		[aBlock value: (self subexpression: 1)]! !

!RxMatcher methodsFor: 'accessing'!
matchesPrefix: aString
	"Match against a string."
	^self matchesStreamPrefix: aString readStream! !

!RxMatcher methodsFor: 'accessing'!
matchesStream: theStream
	"Match thyself against a positionable stream."
	^(self matchesStreamPrefix: theStream)
		and: [stream atEnd]! !

!RxMatcher methodsFor: 'accessing'!
matchesStreamPrefix: theStream
	"Match thyself against a positionable stream."
	stream := theStream.
	lastChar := nil.
	^self tryMatch! !

!RxMatcher methodsFor: 'streaming'!
next
	lastChar := stream next.
	^lastChar! !

!RxMatcher methodsFor: 'testing'!
notAtWordBoundary
	^self atWordBoundary not! !

!RxMatcher methodsFor: 'streaming'!
position
	^stream position! !

!RxMatcher methodsFor: 'private'!
proceedSearchingStream: aStream
	| position |
	position := aStream position.
	[aStream atEnd] whileFalse:
		[self tryMatch ifTrue: [^true].
		aStream position: position.
		lastChar := aStream next.
		position := aStream position].
	"Try match at the very stream end too!!"
	self tryMatch ifTrue: [^true]. 
	^false! !

!RxMatcher methodsFor: 'privileged'!
restoreState: aBlock
	aBlock value! !

!RxMatcher methodsFor: 'accessing'!
search: aString
	"Search the string for occurrence of something matching myself.
	Answer a Boolean indicating success."
	^self searchStream: aString readStream! !

!RxMatcher methodsFor: 'accessing'!
searchStream: aStream
	"Search the stream for occurrence of something matching myself.
	After the search has occurred, stop positioned after the end of the
	matched substring. Answer a Boolean indicating success."
	| position |
	stream := aStream.
	lastChar := nil.
	position := aStream position.
	[aStream atEnd] whileFalse:
		[self tryMatch ifTrue: [^true].
		aStream position: position.
		lastChar := aStream next.
		position := aStream position].
	"Try match at the very stream end too!!"
	self tryMatch ifTrue: [^true]. 
	^false! !

!RxMatcher methodsFor: 'accessing'!
subBeginning: subIndex
	^markerPositions at: subIndex * 2 - 1! !

!RxMatcher methodsFor: 'accessing'!
subEnd: subIndex
	^markerPositions at: subIndex * 2! !

!RxMatcher methodsFor: 'accessing'!
subexpression: subIndex
	| originalPosition start end reply |
	originalPosition := stream position.
	start := self subBeginning: subIndex.
	end := self subEnd: subIndex.
	(start isNil or: [end isNil]) ifTrue: [^String new].
	reply := (String new: end - start) writeStream.
	stream position: start.
	start to: end - 1 do: [:ignored | reply nextPut: stream next].
	stream position: originalPosition.
	^reply contents! !

!RxMatcher methodsFor: 'accessing'!
subexpressionCount
	^markerCount // 2! !

!RxMatcher methodsFor: 'accessing' stamp: 'damien.pollet 5/2/2009 23:51'!
subexpressions
	| result |
	result := Array new: self subexpressionCount.
	1 to: self subexpressionCount do: [:index |
		result
			at: index
			put: (self subexpression: index) ].
	^ result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'damien.pollet 5/3/2009 00:03'!
submatchesIn: aString
	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection with an array of subexpressions per match."
	| result |
	result := OrderedCollection new.
	self
		submatchesOnStream: aString readStream
		do: [:subexprs | result add: subexprs].
	^result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'damien.pollet 5/3/2009 00:04'!
submatchesIn: aString collect: aBlock
	"Search aString repeatedly for the matches of the receiver.  Evaluate aBlock for each match passing the collection of matched subexpressions as the argument, collecting evaluation results in an OrderedCollection."
	| result |
	result := OrderedCollection new.
	self
		submatchesOnStream: aString readStream
		do: [:subexprs | result add: (aBlock value: subexprs)].
	^result! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'damien.pollet 5/3/2009 00:04'!
submatchesIn: aString do: aBlock
	"Search aString repeatedly for the matches of the receiver.
	Evaluate aBlock for each match passing the collection of matched subexpressions
	as the argument."
	self
		submatchesOnStream: aString readStream
		do: aBlock! !

!RxMatcher methodsFor: 'match enumeration' stamp: 'damien.pollet 5/2/2009 23:55'!
submatchesOnStream: aStream do: aBlock
	[self searchStream: aStream] whileTrue:
		[aBlock value: self subexpressions]! !

!RxMatcher methodsFor: 'testing'!
supportsSubexpressions
	^true! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxAny
	"Double dispatch from the syntax tree. 
	Create a matcher for any non-whitespace character."
	^RxmPredicate new
		predicate: [:char | (Cr = char or: [Lf = char]) not]! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxBeginningOfLine
	"Double dispatch from the syntax tree. 
	Create a matcher for beginning-of-line condition."
	^RxmSpecial new beBeginningOfLine! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxBeginningOfWord
	"Double dispatch from the syntax tree. 
	Create a matcher for beginning-of-word condition."
	^RxmSpecial new beBeginningOfWord! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxBranch: branchNode
	"Double dispatch from the syntax tree. 
	Branch node is a link in a chain of concatenated pieces.
	First build the matcher for the rest of the chain, then make 
	it for the current piece and hook the rest to it."
	| result next rest |
	branchNode branch isNil
		ifTrue: [^branchNode piece dispatchTo: self].
	"Optimization: glue a sequence of individual characters into a single string to match."
	branchNode piece isAtomic ifTrue:
		[result := WriteStream on: (String new: 40).
		next := branchNode tryMergingInto: result.
		result := result contents.
		result size > 1 ifTrue: "worth merging"
			[rest := next notNil 
				ifTrue: [next dispatchTo: self]
				ifFalse: [nil].
			^(RxmSubstring new substring: result ignoreCase: ignoreCase)
				pointTailTo: rest;
				yourself]].
	"No optimization possible or worth it, just concatenate all. "
	^(branchNode piece dispatchTo: self)
		pointTailTo: (branchNode branch dispatchTo: self);
		yourself! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxCharSet: charSetNode
	"Double dispatch from the syntax tree. 
	A character set is a few characters, and we either match any of them,
	or match any that is not one of them."
	^RxmPredicate with: charSetNode predicate! !

!RxMatcher methodsFor: 'double dispatch' stamp: 'pb 6/3/2012 21:48'!
syntaxCharacter: charNode
	"Double dispatch from the syntax tree. 
	We get here when no merging characters into strings was possible."
	| wanted |
	wanted := charNode character.
	^ RxmPredicate new predicate:
		(ignoreCase
			ifTrue: [
				[ :char |
					"
				char sameAs: wanted
				"
				char asLowercase = wanted asLowercase ]]
			ifFalse: [
				[ :char |
				char = wanted ]]).! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxEndOfLine
	"Double dispatch from the syntax tree. 
	Create a matcher for end-of-line condition."
	^RxmSpecial new beEndOfLine! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxEndOfWord
	"Double dispatch from the syntax tree. 
	Create a matcher for end-of-word condition."
	^RxmSpecial new beEndOfWord! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxEpsilon
	"Double dispatch from the syntax tree. Match empty string. This is unlikely
	to happen in sane expressions, so we'll live without special epsilon-nodes."
	^RxmSubstring new
		substring: String new
		ignoreCase: ignoreCase! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxMessagePredicate: messagePredicateNode
	"Double dispatch from the syntax tree. 
	Special link can handle predicates."
	^messagePredicateNode negated
		ifTrue: [RxmPredicate new bePerformNot: messagePredicateNode selector]
		ifFalse: [RxmPredicate new bePerform: messagePredicateNode selector]! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxNonWordBoundary
	"Double dispatch from the syntax tree. 
	Create a matcher for the word boundary condition."
	^RxmSpecial new beNotWordBoundary! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxPiece: pieceNode
	"Double dispatch from the syntax tree. 
	Piece is an atom repeated a few times. Take care of a special
	case when the atom is repeated just once."
	| atom |
	atom := pieceNode atom dispatchTo: self.
	^pieceNode isSingular
		ifTrue: [atom]
		ifFalse: [pieceNode isStar
			ifTrue: [self makeStar: atom]
			ifFalse: [pieceNode isPlus
				ifTrue: [self makePlus: atom]
				ifFalse: [pieceNode isOptional
					ifTrue: [self makeOptional: atom]
					ifFalse: [RxParser signalCompilationException: 
						'repetitions are not supported by RxMatcher']]]]! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxPredicate: predicateNode
	"Double dispatch from the syntax tree. 
	A character set is a few characters, and we either match any of them,
	or match any that is not one of them."
	^RxmPredicate with: predicateNode predicate! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxRegex: regexNode
	"Double dispatch from the syntax tree. 
	Regex node is a chain of branches to be tried. Should compile this 
	into a bundle of parallel branches, between two marker nodes." 
	
	| startIndex endIndex endNode alternatives |
	startIndex := self allocateMarker.
	endIndex := self allocateMarker.
	endNode := RxmMarker new index: endIndex.
	alternatives := self hookBranchOf: regexNode onto: endNode.
	^(RxmMarker new index: startIndex)
		pointTailTo: alternatives;
		yourself! !

!RxMatcher methodsFor: 'double dispatch'!
syntaxWordBoundary
	"Double dispatch from the syntax tree. 
	Create a matcher for the word boundary condition."
	^RxmSpecial new beWordBoundary! !

!RxMatcher methodsFor: 'private'!
tryMatch
	"Match thyself against the current stream."
	markerPositions := Array new: markerCount.
	startOptimizer == nil
		ifTrue: [lastResult := matcher matchAgainst: self]
		ifFalse: [lastResult := (startOptimizer canStartMatch: stream peek in: self)
									and: [matcher matchAgainst: self]].
	^lastResult! !

!RxMatcher class methodsFor: 'instance creation'!
for: aRegex
	"Create and answer a matcher that will match a regular expression
	specified by the syntax tree of which `aRegex' is a root."
	^self for: aRegex ignoreCase: false! !

!RxMatcher class methodsFor: 'instance creation'!
for: aRegex ignoreCase: aBoolean
	"Create and answer a matcher that will match a regular expression
	specified by the syntax tree of which `aRegex' is a root."
	^self new
		initialize: aRegex
		ignoreCase: aBoolean! !

!RxMatcher class methodsFor: 'instance creation'!
forString: aString
	"Create and answer a matcher that will match the regular expression
	`aString'."
	^self for: (RxParser new parse: aString)! !

!RxMatcher class methodsFor: 'instance creation'!
forString: aString ignoreCase: aBoolean
	"Create and answer a matcher that will match the regular expression
	`aString'."
	^self for: (RxParser new parse: aString) ignoreCase: aBoolean! !

!RxMatcher class methodsFor: 'class initialization' stamp: 'avi 11/30/2003 13:30'!
initialize
	"RxMatcher initialize"
	Cr := Character cr.
	Lf := Character lf.! !

!RxParser methodsFor: 'recursive descent'!
atom
	"An atom is one of a lot of possibilities, see below."
	| atom |
	(lookahead = #epsilon or: 
			[lookahead = $| or: 
					[lookahead = $)
						or: [lookahead = $* or: [lookahead = $+ or: [lookahead = $?]]]]])
		ifTrue: [^RxsEpsilon new].
	lookahead = $( ifTrue: 
			["<atom> ::= '(' <regex> ')' "
			self match: $(.
			atom := self regex.
			self match: $).
			^atom].
	lookahead = $[ ifTrue: 
			["<atom> ::= '[' <characterSet> ']' "
			self match: $[.
			atom := self characterSet.
			self match: $].
			^atom].
	lookahead = $: ifTrue: 
			["<atom> ::= ':' <messagePredicate> ':' "
			self match: $:.
			atom := self messagePredicate.
			self match: $:.
			^atom].
	lookahead = $. ifTrue: 
			["any non-whitespace character"
			self next.
			^RxsContextCondition new beAny].
	lookahead = $^ ifTrue: 
			["beginning of line condition"
			self next.
			^RxsContextCondition new beBeginningOfLine].
	lookahead = $$ ifTrue: 
			["end of line condition"
			self next.
			^RxsContextCondition new beEndOfLine].
	lookahead = $\ ifTrue: 
			["<atom> ::= '\' <character>"
			self next.
			lookahead = #epsilon ifTrue: 
				[self signalParseError: 'bad quotation'].
			(BackslashConstants includesKey: lookahead) ifTrue:
				[atom := RxsCharacter with: (BackslashConstants at: lookahead).
				self next.
				^atom].
			self ifSpecial: lookahead
				then: [:node | self next. ^node]].
	"If passed through the above, the following is a regular character."
	atom := RxsCharacter with: lookahead.
	self next.
	^atom! !

!RxParser methodsFor: 'recursive descent'!
branch
	"<branch> ::= e | <piece> <branch>"
	| piece branch |
	piece := self piece.
	(lookahead = #epsilon or: [lookahead = $| or: [lookahead = $) ]])
		ifTrue: [branch := nil]
		ifFalse: [branch := self branch].
	^RxsBranch new 
		initializePiece: piece 
		branch: branch! !

!RxParser methodsFor: 'recursive descent'!
characterSet
	"Match a range of characters: something between `[' and `]'.
	Opening bracked has already been seen, and closing should
	not be consumed as well. Set spec is as usual for
	sets in regexes."
	| spec errorMessage |
	errorMessage := ' no terminating "]"'.
	spec := self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage.
	(spec isEmpty or: [spec = '^']) ifTrue: "This ']' was literal."
		[self next.
		spec := spec, ']', (self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage)].
	^self characterSetFrom: spec! !

!RxParser methodsFor: 'private'!
characterSetFrom: setSpec
	"<setSpec> is what goes between the brackets in a charset regex
	(a String). Make a string containing all characters the spec specifies.
	Spec is never empty."
	| negated spec |
	spec := ReadStream on: setSpec.
	spec peek = $^
		ifTrue: 	[negated := true.
				spec next]
		ifFalse:	[negated := false].
	^RxsCharSet new
		initializeElements: (RxCharSetParser on: spec) parse
		negated: negated! !

!RxParser methodsFor: 'private'!
ifSpecial: aCharacter then: aBlock
	"If the character is such that it defines a special node when follows a $\,
	then create that node and evaluate aBlock with the node as the parameter.
	Otherwise just return."
	| classAndSelector |
	classAndSelector := BackslashSpecials at: aCharacter ifAbsent: [^self].
	^aBlock value: (classAndSelector key new perform: classAndSelector value)! !

!RxParser methodsFor: 'private'!
inputUpTo: aCharacter errorMessage: aString
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."
	| accumulator |
	accumulator := WriteStream on: (String new: 20).
	[lookahead ~= aCharacter and: [lookahead ~= #epsilon]]
		whileTrue:
			[accumulator nextPut: lookahead.
			self next].
	lookahead = #epsilon ifTrue: [self signalParseError: aString].
	^accumulator contents! !

!RxParser methodsFor: 'private'!
inputUpTo: aCharacter nestedOn: anotherCharacter errorMessage: aString 
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."
	| accumulator nestLevel |
	accumulator := WriteStream on: (String new: 20).
	nestLevel := 0.
	[lookahead ~= aCharacter or: [nestLevel > 0]] whileTrue: 
			[#epsilon = lookahead ifTrue: [self signalParseError: aString].
			accumulator nextPut: lookahead.
			lookahead = anotherCharacter ifTrue: [nestLevel := nestLevel + 1].
			lookahead = aCharacter ifTrue: [nestLevel := nestLevel - 1].
			self next].
	^accumulator contents! !

!RxParser methodsFor: 'private'!
match: aCharacter
	"<aCharacter> MUST match the current lookeahead.
	If this is the case, advance the input. Otherwise, blow up."
	aCharacter ~= lookahead 
		ifTrue: [^self signalParseError].	"does not return"
	self next! !

!RxParser methodsFor: 'recursive descent'!
messagePredicate
	"Match a message predicate specification: a selector (presumably
	understood by a Character) enclosed in :'s ."
	| spec negated |
	spec := (self inputUpTo: $: errorMessage: ' no terminating ":"').
	negated := false.
	spec first = $^ ifTrue:
		[negated := true.
		spec := spec copyFrom: 2 to: spec size].
	^RxsMessagePredicate new 
		initializeSelector: spec asSymbol
		negated: negated! !

!RxParser methodsFor: 'private'!
next
	"Advance the input storing the just read character
	as the lookahead."
	input atEnd
		ifTrue: [lookahead := #epsilon]
		ifFalse: [lookahead := input next]! !

!RxParser methodsFor: 'accessing'!
parse: aString
	"Parse input from a string <aString>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."
	^self parseStream: (ReadStream on: aString)! !

!RxParser methodsFor: 'accessing'!
parseStream: aStream
	"Parse an input from a character stream <aStream>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."
	| tree |
	input := aStream.
	lookahead := nil.
	self match: nil.
	tree := self regex.
	self match: #epsilon.
	^tree! !

!RxParser methodsFor: 'recursive descent'!
piece
	"<piece> ::= <atom> | <atom>* | <atom>+ | <atom>?"
	| atom errorMessage |
	errorMessage := ' nullable closure'.
	atom := self atom.
	lookahead = $* ifTrue: 
		[self next.
		atom isNullable ifTrue: [self signalParseError: errorMessage].
		^RxsPiece new initializeStarAtom: atom].
	lookahead = $+ ifTrue: 
		[self next.
		atom isNullable ifTrue: [self signalParseError: errorMessage].
		^RxsPiece new initializePlusAtom: atom].
	lookahead = $? ifTrue: 
		[self next.
		atom isNullable ifTrue: [self signalParseError: errorMessage].
		^RxsPiece new initializeOptionalAtom: atom].
	^RxsPiece new initializeAtom: atom! !

!RxParser methodsFor: 'recursive descent'!
regex
	"<regex> ::= e | <branch> `|' <regex>"
	| branch regex |
	branch := self branch.
	(lookahead = #epsilon or: [lookahead = $)])
		ifTrue: [regex := nil]
		ifFalse: 
			[self match: $|.
			regex := self regex].
	^RxsRegex new initializeBranch: branch regex: regex! !

!RxParser methodsFor: 'private'!
signalParseError
	self class signalSyntaxException: 'Regex syntax error'! !

!RxParser methodsFor: 'private'!
signalParseError: aString
	self class signalSyntaxException: aString! !

!RxParser class methodsFor: 'DOCUMENTATION'!
a:x introduction:xx 
" 
A regular expression is a template specifying a class of strings. A
regular expression matcher is an tool that determines whether a string
belongs to a class specified by a regular expression.  This is a
common task of a user input validation code, and the use of regular
expressions can GREATLY simplify and speed up development of such
code.  As an example, here is how to verify that a string is a valid
hexadecimal number in Smalltalk notation, using this matcher package:
	aString matchesRegex: '16r[[:xdigit:]]+'
(Coding the same ``the hard way'' is an exercise to a curious reader).
This matcher is offered to the Smalltalk community in hope it will be
useful. It is free in terms of money, and to a large extent--in terms
of rights of use. Refer to `Boring Stuff' section for legalese.
The 'What's new in this release' section describes the functionality
introduced in 1.1 release.
The `Syntax' section explains the recognized syntax of regular
expressions.
The `Usage' section explains matcher capabilities that go beyond what
String>>matchesRegex: method offers.
The `Implementation notes' sections says a few words about what is
under the hood.
Happy hacking,
--Vassili Bykov 
<vassili@objectpeople.com> <vassili@magma.ca>
August 6, 1996
April 4, 1999
"
	self error: 'comment only'! !

!RxParser class methodsFor: 'DOCUMENTATION'!
b:x whatsNewInThisRelease: xx
"
VERSION 1.1	(October 1999)
Regular expression syntax corrections and enhancements:
1. Backslash escapes similar to those in Perl are allowed in patterns:
	\w	any word constituent character (equivalent to [a-zA-Z0-9_])
	\W	any character but a word constituent (equivalent to [^a-xA-Z0-9_]
	\d	a digit (same as [0-9])
	\D	anything but a digit
	\s 	a whitespace character
	\S	anything but a whitespace character
	\b	an empty string at a word boundary
	\B	an empty string not at a word boundary
	\<	an empty string at the beginning of a word
	\>	an empty string at the end of a word
For example, '\w+' is now a valid expression matching any word.
2. The following backslash escapes are also allowed in character sets
(between square brackets):
	\w, \W, \d, \D, \s, and \S.
3. The following grep(1)-compatible named character classes are
recognized in character sets as well:
	[:alnum:]
	[:alpha:]
	[:cntrl:]
	[:digit:]
	[:graph:]
	[:lower:]
	[:print:]
	[:punct:]
	[:space:]
	[:upper:]
	[:xdigit:]
For example, the following patterns are equivalent:
	'[[:alnum:]]+' '\w+'  '[\w]+' '[a-zA-Z0-9_]+'
4. Some non-printable characters can be represented in regular
expressions using a common backslash notation:
	\t	tab (Character tab)
	\n	newline (Character lf)
	\r	carriage return (Character cr)
	\f	form feed (Character newPage)
	\e	escape (Character esc)
5. A dot is corectly interpreted as 'any character but a newline'
instead of 'anything but whitespace'.
6. Case-insensitive matching.  The easiest access to it are new
messages CharacterArray understands: #asRegexIgnoringCase,
#matchesRegexIgnoringCase:, #prefixMatchesRegexIgnoringCase:.
7. The matcher (an instance of RxMatcher, the result of
String>>asRegex) now provides a collection-like interface to matches
in a particular string or on a particular stream, as well as
substitution protocol. The interface includes the following messages:
	matchesIn: aString
	matchesIn: aString collect: aBlock
	matchesIn: aString do: aBlock
	matchesOnStream: aStream
	matchesOnStream: aStream collect: aBlock
	matchesOnStream: aStream do: aBlock
	copy: aString translatingMatchesUsing: aBlock
	copy: aString replacingMatchesWith: replacementString
	copyStream: aStream to: writeStream translatingMatchesUsing: aBlock
	copyStream: aStream to: writeStream replacingMatchesWith: aString
Examples:
	'\w+' asRegex matchesIn: 'now is the time'
returns an OrderedCollection containing four strings: 'now', 'is',
'the', and 'time'.
	'\<t\w+' asRegexIgnoringCase
		copy: 'now is the Time'
		translatingMatchesUsing: [:match | match asUppercase]
returns 'now is THE TIME' (the regular expression matches words
beginning with either an uppercase or a lowercase T).
ACKNOWLEDGEMENTS
Since the first release of the matcher, thanks to the input from
several fellow Smalltalkers, I became convinced a native Smalltalk
regular expression matcher was worth the effort to keep it alive. For
the advice and encouragement that made this release possible, I want
to thank:
	Felix Hack
	Eliot Miranda
	Robb Shecter
	David N. Smith
	Francis Wolinski
and anyone whom I haven't yet met or heard from, but who agrees this
has not been a complete waste of time.
--Vassili Bykov
October 3, 1999
"
	self error: 'comment only'! !

!RxParser class methodsFor: 'DOCUMENTATION'!
c:x syntax:xx 
" 
[You can select and `print it' examples in this method. Just don't
forget to cancel the changes.]
The simplest regular expression is a single character.  It matches
exactly that character. A sequence of characters matches a string with
exactly the same sequence of characters:
	'a' matchesRegex: 'a'				-- true
	'foobar' matchesRegex: 'foobar'		-- true
	'blorple' matchesRegex: 'foobar'		-- false
The above paragraph introduced a primitive regular expression (a
character), and an operator (sequencing). Operators are applied to
regular expressions to produce more complex regular expressions.
Sequencing (placing expressions one after another) as an operator is,
in a certain sense, `invisible'--yet it is arguably the most common.
A more `visible' operator is Kleene closure, more often simply
referred to as `a star'.  A regular expression followed by an asterisk
matches any number (including 0) of matches of the original
expression. For example:
	'ab' matchesRegex: 'a*b'		 		-- true
	'aaaaab' matchesRegex: 'a*b'	 	-- true
	'b' matchesRegex: 'a*b'		 		-- true
	'aac' matchesRegex: 'a*b'	 		-- false: b does not match
A star's precedence is higher than that of sequencing. A star applies
to the shortest possible subexpression that precedes it. For example,
'ab*' means `a followed by zero or more occurrences of b', not `zero
or more occurrences of ab':
	'abbb' matchesRegex: 'ab*'	 		-- true
	'abab' matchesRegex: 'ab*'		 	-- false
To actually make a regex matching `zero or more occurrences of ab',
`ab' is enclosed in parentheses:
	'abab' matchesRegex: '(ab)*'		 	-- true
	'abcab' matchesRegex: '(ab)*'	 	-- false: c spoils the fun
Two other operators similar to `*' are `+' and `?'. `+' (positive
closure, or simply `plus') matches one or more occurrences of the
original expression. `?' (`optional') matches zero or one, but never
more, occurrences.
	'ac' matchesRegex: 'ab*c'	 		-- true
	'ac' matchesRegex: 'ab+c'	 		-- false: need at least one b
	'abbc' matchesRegex: 'ab+c'		 	-- true
	'abbc' matchesRegex: 'ab?c'		 	-- false: too many b's
As we have seen, characters `*', `+', `?', `(', and `)' have special
meaning in regular expressions. If one of them is to be used
literally, it should be quoted: preceded with a backslash. (Thus,
backslash is also special character, and needs to be quoted for a
literal match--as well as any other special character described
further).
	'ab*' matchesRegex: 'ab*'		 	-- false: star in the right string is special
	'ab*' matchesRegex: 'ab\*'	 		-- true
	'a\c' matchesRegex: 'a\\c'		 	-- true
The last operator is `|' meaning `or'. It is placed between two
regular expressions, and the resulting expression matches if one of
the expressions matches. It has the lowest possible precedence (lower
than sequencing). For example, `ab*|ba*' means `a followed by any
number of b's, or b followed by any number of a's':
	'abb' matchesRegex: 'ab*|ba*'	 	-- true
	'baa' matchesRegex: 'ab*|ba*'	 	-- true
	'baab' matchesRegex: 'ab*|ba*'	 	-- false
A bit more complex example is the following expression, matching the
name of any of the Lisp-style `car', `cdr', `caar', `cadr',
... functions:
	c(a|d)+r
It is possible to write an expression matching an empty string, for
example: `a|'.  However, it is an error to apply `*', `+', or `?' to
such expression: `(a|)*' is an invalid expression.
So far, we have used only characters as the 'smallest' components of
regular expressions. There are other, more `interesting', components.
A character set is a string of characters enclosed in square
brackets. It matches any single character if it appears between the
brackets. For example, `[01]' matches either `0' or `1':
	'0' matchesRegex: '[01]'		 		-- true
	'3' matchesRegex: '[01]'		 		-- false
	'11' matchesRegex: '[01]'		 		-- false: a set matches only one character
Using plus operator, we can build the following binary number
recognizer:
	'10010100' matchesRegex: '[01]+'	 	-- true
	'10001210' matchesRegex: '[01]+'	 	-- false
If the first character after the opening bracket is `^', the set is
inverted: it matches any single character *not* appearing between the
brackets:
	'0' matchesRegex: '[^01]'		  		-- false
	'3' matchesRegex: '[^01]'		 		-- true
For convenience, a set may include ranges: pairs of characters
separated with `-'. This is equivalent to listing all characters
between them: `[0-9]' is the same as `[0123456789]'.
Special characters within a set are `^', `-', and `]' that closes the
set. Below are the examples of how to literally use them in a set:
	[01^]		-- put the caret anywhere except the beginning
	[01-]		-- put the dash as the last character
	[]01]		-- put the closing bracket as the first character 
	[^]01]			(thus, empty and universal sets cannot be specified)
Regular expressions can also include the following backquote escapes
to refer to popular classes of characters:
	\w	any word constituent character (same as [a-zA-Z0-9_])
	\W	any character but a word constituent
	\d	a digit (same as [0-9])
	\D	anything but a digit
	\s 	a whitespace character
	\S	anything but a whitespace character
These escapes are also allowed in character classes: '[\w+-]' means
'any character that is either a word constituent, or a plus, or a
minus'.
Character classes can also include the following grep(1)-compatible
elements to refer to:
	[:alnum:]		any alphanumeric, i.e., a word constituent, character
	[:alpha:]		any alphabetic character
	[:cntrl:]		any control character. In this version, it means any character which code is < 32.
	[:digit:]		any decimal digit.
	[:graph:]		any graphical character. In this version, this mean any character with the code >= 32.
	[:lower:]		any lowercase character
	[:print:]		any printable character. In this version, this is the same as [:cntrl:]
	[:punct:]		any punctuation character.
	[:space:]		any whitespace character.
	[:upper:]		any uppercase character.
	[:xdigit:]		any hexadecimal character.
Note that these elements are components of the character classes,
i.e. they have to be enclosed in an extra set of square brackets to
form a valid regular expression.  For example, a non-empty string of
digits would be represented as '[[:digit:]]+'.
The above primitive expressions and operators are common to many
implementations of regular expressions. The next primitive expression
is unique to this Smalltalk implementation.
A sequence of characters between colons is treated as a unary selector
which is supposed to be understood by Characters. A character matches
such an expression if it answers true to a message with that
selector. This allows a more readable and efficient way of specifying
character classes. For example, `[0-9]' is equivalent to `:isDigit:',
but the latter is more efficient. Analogously to character sets,
character classes can be negated: `:^isDigit:' matches a Character
that answers false to #isDigit, and is therefore equivalent to
`[^0-9]'.
As an example, so far we have seen the following equivalent ways to
write a regular expression that matches a non-empty string of digits:
	'[0-9]+'
	'\d+'
	'[\d]+'
	'[[:digit::]+'
	:isDigit:+'
The last group of special primitive expressions includes: 
	.	matching any character except a newline; 
	^	matching an empty string at the beginning of a line; 
	$	matching an empty string at the end of a line.
	\b	an empty string at a word boundary
	\B	an empty string not at a word boundary
	\<	an empty string at the beginning of a word
	\>	an empty string at the end of a word
	'axyzb' matchesRegex: 'a.+b'		-- true
	'ax zb' matchesRegex: 'a.+b'			-- false (space is not matched by `.')
Again, all the above three characters are special and should be quoted
to be matched literally.
	EXAMPLES
As the introductions said, a great use for regular expressions is user
input validation. Following are a few examples of regular expressions
that might be handy in checking input entered by the user in an input
field. Try them out by entering something between the quotes and
print-iting. (Also, try to imagine Smalltalk code that each validation
would require if coded by hand).  Most example expressions could have
been written in alternative ways.
Checking if aString may represent a nonnegative integer number:
	'' matchesRegex: ':isDigit:+'
or
	'' matchesRegex: '[0-9]+'
or
	'' matchesRegex: '\d+'
Checking if aString may represent an integer number with an optional
sign in front:
	'' matchesRegex: '(\+|-)?\d+'
Checking if aString is a fixed-point number, with at least one digit
is required after a dot:
	'' matchesRegex: '(\+|-)?\d+(\.\d+)?'
The same, but allow notation like `123.':
	'' matchesRegex: '(\+|-)?\d+(\.\d*)?'
Recognizer for a string that might be a name: one word with first
capital letter, no blanks, no digits.  More traditional:
	'' matchesRegex: '[A-Z][A-Za-z]*'
more Smalltalkish:
	'' matchesRegex: ':isUppercase::isAlphabetic:*'
A date in format MMM DD, YYYY with any number of spaces in between, in
XX century:
	'' matchesRegex: '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(\d\d?)[ ]*,[ ]*19(\d\d)'
Note parentheses around some components of the expression above. As
`Usage' section shows, they will allow us to obtain the actual strings
that have matched them (i.e. month name, day number, and year number).
For dessert, coming back to numbers: here is a recognizer for a
general number format: anything like 999, or 999.999, or -999.999e+21.
	'' matchesRegex: '(\+|-)?\d+(\.\d*)?((e|E)(\+|-)?\d+)?'
"
	self error: 'comment only'! !

!RxParser class methodsFor: 'test suite'!
compileRegex: regexSource into: matcherClass
	"Compile the regex and answer the matcher, or answer nil if compilation fails."
	| syntaxTree |
	syntaxTree := self safelyParse: regexSource.
	syntaxTree == nil ifTrue: [^nil].
	^matcherClass for: syntaxTree! !

!RxParser class methodsFor: 'DOCUMENTATION' stamp: 'sd 1/14/2008 10:22'!
d:x usage:xx
" 
The preceding section covered the syntax of regular expressions. It
used the simplest possible interface to the matcher: sending
#matchesRegex: message to the sample string, with regular expression
string as the argument.  This section explains hairier ways of using
the matcher.
	PREFIX MATCHING AND CASE-INSENSITIVE MATCHING
A CharacterArray (an EsString in VA) also understands these messages:
	#prefixMatchesRegex: regexString
	#matchesRegexIgnoringCase: regexString
	#prefixMatchesRegexIgnoringCase: regexString
#prefixMatchesRegex: is just like #matchesRegex, except that the whole
receiver is not expected to match the regular expression passed as the
argument; matching just a prefix of it is enough.  For example:
	'abcde' matchesRegex: '(a|b)+'		-- false
	'abcde' prefixMatchesRegex: '(a|b)+'	-- true
The last two messages are case-insensitive versions of matching.
	ENUMERATION INTERFACE
An application can be interested in all matches of a certain regular
expression within a String.  The matches are accessible using a
protocol modelled after the familiar Collection-like enumeration
protocol:
	#regex: regexString matchesDo: aBlock
Evaluates a one-argument <aBlock> for every match of the regular
expression within the receiver string.
	#regex: regexString matchesCollect: aBlock
Evaluates a one-argument <aBlock> for every match of the regular
expression within the receiver string. Collects results of evaluations
and anwers them as a SequenceableCollection.
	#allRegexMatches: regexString
Returns a collection of all matches (substrings of the receiver
string) of the regular expression.  It is an equivalent of <aString
regex: regexString matchesCollect: [:each | each]>.
	REPLACEMENT AND TRANSLATION
It is possible to replace all matches of a regular expression with a
certain string using the message:
	#copyWithRegex: regexString matchesReplacedWith: aString
For example:
	'ab cd ab' copyWithRegex: '(a|b)+' matchesReplacedWith: 'foo'
A more general substitution is match translation:
	#copyWithRegex: regexString matchesTranslatedUsing: aBlock
This message evaluates a block passing it each match of the regular
expression in the receiver string and answers a copy of the receiver
with the block results spliced into it in place of the respective
matches.  For example:
	'ab cd ab' copyWithRegex: '(a|b)+' matchesTranslatedUsing: [:each | each asUppercase]
All messages of enumeration and replacement protocols perform a
case-sensitive match.  Case-insensitive versions are not provided as
part of a CharacterArray protocol.  Instead, they are accessible using
the lower-level matching interface.
	LOWER-LEVEL INTERFACE
Internally, #matchesRegex: works as follows:
1. A fresh instance of RxParser is created, and the regular expression
string is passed to it, yielding the expression's syntax tree.
2. The syntax tree is passed as an initialization parameter to an
instance of RxMatcher. The instance sets up some data structure that
will work as a recognizer for the regular expression described by the
tree.
3. The original string is passed to the matcher, and the matcher
checks for a match.
	THE MATCHER
If you repeatedly match a number of strings against the same regular
expression using one of the messages defined in CharacterArray, the
regular expression string is parsed and a matcher is created anew for
every match.  You can avoid this overhead by building a matcher for
the regular expression, and then reusing the matcher over and over
again. You can, for example, create a matcher at a class or instance
initialization stage, and store it in a variable for future use.
You can create a matcher using one of the following methods:
	- Sending #forString:ignoreCase: message to RxMatcher class, with
the regular expression string and a Boolean indicating whether case is
ignored as arguments.
	- Sending #forString: message.  It is equivalent to <... forString:
regexString ignoreCase: false>.
A more convenient way is using one of the two matcher-created messages
understood by CharacterArray.
	- <regexString asRegex> is equivalent to <RxMatcher forString:
regexString>.
	- <regexString asRegexIgnoringCase> is equivalent to <RxMatcher
forString: regexString ignoreCase: true>.
Here are four examples of creating a matcher:
	hexRecognizer := RxMatcher forString: '16r[0-9A-Fa-f]+'
	hexRecognizer := RxMatcher forString: '16r[0-9A-Fa-f]+' ignoreCase: false
	hexRecognizer := '16r[0-9A-Fa-f]+' asRegex
	hexRecognizer := '16r[0-9A-F]+' asRegexIgnoringCase
	MATCHING
The matcher understands these messages (all of them return true to
indicate successful match or search, and false otherwise):
matches: aString
	True if the whole target string (aString) matches.
matchesPrefix: aString
	True if some prefix of the string (not necessarily the whole
	string) matches.
search: aString
	Search the string for the first occurrence of a matching
	substring. (Note that the first two methods only try matching from
	the very beginning of the string). Using the above example with a
	matcher for `a+', this method would answer success given a string
	`baaa', while the previous two would fail.
matchesStream: aStream
matchesStreamPrefix: aStream
searchStream: aStream
	Respective analogs of the first three methods, taking input from a
	stream instead of a string. The stream must be positionable and
	peekable.
All these methods answer a boolean indicating success. The matcher
also stores the outcome of the last match attempt and can report it:
lastResult
	Answers a Boolean -- the outcome of the most recent match
	attempt. If no matches were attempted, the answer is unspecified.
	SUBEXPRESSION MATCHES
After a successful match attempt, you can query the specifics of which
part of the original string has matched which part of the whole
expression.
A subexpression is a parenthesized part of a regular expression, or
the whole expression. When a regular expression is compiled, its
subexpressions are assigned indices starting from 1, depth-first,
left-to-right. For example, `((ab)+(c|d))?ef' includes the following
subexpressions with these indices:
	1:	((ab)+(c|d))?ef
	2:	(ab)+(c|d)
	3:	ab
	4:	c|d
After a successful match, the matcher can report what part of the
original string matched what subexpression. It understandards these
messages:
subexpressionCount
	Answers the total number of subexpressions: the highest value that
	can be used as a subexpression index with this matcher. This value
	is available immediately after initialization and never changes.
subexpression: anIndex
	An index must be a valid subexpression index, and this message
	must be sent only after a successful match attempt. The method
	answers a substring of the original string the corresponding
	subexpression has matched to.
subBeginning: anIndex
subEnd: anIndex
	Answer positions within the original string or stream where the
	match of a subexpression with the given index has started and
	ended, respectively.
This facility provides a convenient way of extracting parts of input
strings of complex format. For example, the following piece of code
uses the 'MMM DD, YYYY' date format recognizer example from the
`Syntax' section to convert a date to a three-element array with year,
month, and day strings (you can select and evaluate it right here):
	| matcher |
	matcher := RxMatcher forString:  '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(:isDigit::isDigit:?)[ ]*,[ ]*19(:isDigit::isDigit:)'.
	(matcher matches: 'Aug 6, 1996')
		ifTrue: 
			[Array 
				with: (matcher subexpression: 4)
				with: (matcher subexpression: 2)
				with: (matcher subexpression: 3)]
		ifFalse: ['no match']
(should answer ` #('96' 'Aug' '6')').
	ENUMERATION AND REPLACEMENT
The enumeration and replacement protocols exposed in CharacterArray
are actually implemented by the mather.  The following messages are
understood:
	#matchesIn: aString
	#matchesIn: aString do: aBlock
	#matchesIn: aString collect: aBlock
	#copy: aString replacingMatchesWith: replacementString
	#copy: aString translatingMatchesUsing: aBlock
	#matchesOnStream: aStream
	#matchesOnStream: aStream do: aBlock
	#matchesOnStream: aStream collect: aBlock
	#copy: sourceStream to: targetStream replacingMatchesWith: replacementString
	#copy: sourceStream to: targetStream translatingMatchesWith: aBlock
	ERROR HANDLING
Exception signaling objects (Signals in VisualWorks, Exceptions in VisualAge) are
accessible through RxParser class protocol. To handle possible errors, use
the protocol described below to obtain the exception objects and use the
protocol of the native Smalltalk implementation to handle them.
If a syntax error is detected while parsing expression,
RxParser>>syntaxErrorSignal is raised/signaled.
If an error is detected while building a matcher,
RxParser>>compilationErrorSignal is raised/signaled.
If an error is detected while matching (for example, if a bad selector
was specified using `:<selector>:' syntax, or because of the matcher's
internal error), RxParser>>matchErrorSignal is raised
RxParser>>regexErrorSignal is the parent of all three.  Since any of
the three signals can be raised within a call to #matchesRegex:, it is
handy if you want to catch them all.  For example:
VisualWorks:
	RxParser regexErrorSignal
		handle: [:ex | ex returnWith: nil]
		do: ['abc' matchesRegex: '))garbage[']
VisualAge:
	['abc' matchesRegex: '))garbage[']
		when: RxParser regexErrorSignal
		do: [:signal | signal exitWith: nil]
"
	self error: 'comment only'! !

!RxParser class methodsFor: 'exception signaling' stamp: 'avi 11/30/2003 13:24'!
doHandlingMessageNotUnderstood: aBlock
	"MNU should be trapped and resignaled as a match error in a few places in the matcher.
	This method factors out this dialect-dependent code to make porting easier."
	^ aBlock
		on: MessageNotUnderstood
		do: [:ex | RxMatcher signalMatchException: 'invalid predicate selector']! !

!RxParser class methodsFor: 'DOCUMENTATION'!
e:x implementationNotes:xx
"	
	Version:		1.1
	Released:		October 1999
	Mail to:		Vassili Bykov <vassili@magma.ca>, <vassili@objectpeople.com>
	Flames to:		/dev/null
	WHAT IS ADDED
The matcher includes classes in two categories:
	VBRegex-Syntax
	VBRegex-Matcher
and a few CharacterArray methods in `vbregex' protocol.  No system
classes or methods are modified.
	WHAT TO LOOK AT FIRST
String>>matchesRegex: -- in 90% cases this method is all you need to
access the package.
RxParser -- accepts a string or a stream of characters with a regular
expression, and produces a syntax tree corresponding to the
expression. The tree is made of instances of Rxs<whatever> classes.
RxMatcher -- accepts a syntax tree of a regular expression built by
the parser and compiles it into a matcher: a structure made of
instances of Rxm<whatever> classes. The RxMatcher instance can test
whether a string or a positionable stream of characters matches the
original regular expression, or search a string or a stream for
substrings matching the expression. After a match is found, the
matcher can report a specific string that matched the whole
expression, or any parenthesized subexpression of it.
All other classes support the above functionality and are used by
RxParser, RxMatcher, or both.
	CAVEATS
The matcher is similar in spirit, but NOT in the design--let alone the
code--to the original Henry Spencer's regular expression
implementation in C.  The focus is on simplicity, not on efficiency.
I didn't optimize or profile anything.  I may in future--or I may not:
I do this in my spare time and I don't promise anything.
The matcher passes H. Spencer's test suite (see 'test suite'
protocol), with quite a few extra tests added, so chances are good
there are not too many bugs.  But watch out anyway.
	EXTENSIONS, FUTURE, ETC.
With the existing separation between the parser, the syntax tree, and
the matcher, it is easy to extend the system with other matchers based
on other algorithms. In fact, I have a DFA-based matcher right now,
but I don't feel it is good enough to include it here.  I might add
automata-based matchers later, but again I don't promise anything.
	HOW TO REACH ME
As of today (October 3, 1999), you can contact me at
<vassili@objectpeople.com>. If this doesn't work, look around
comp.lang.smalltalk and comp.lang.lisp.  
"
	self error: 'comment only'! !

!RxParser class methodsFor: 'DOCUMENTATION'!
f:x boringStuff: xx
"
The Regular Expression Matcher (``The Software'') 
is Copyright (C) 1996, 1999 Vassili Bykov.  
It is provided to the Smalltalk community in hope it will be useful.
1. This license applies to the package as a whole, as well as to any
   component of it. By performing any of the activities described
   below, you accept the terms of this agreement.
2. The software is provided free of charge, and ``as is'', in hope
   that it will be useful, with ABSOLUTELY NO WARRANTY. The entire
   risk and all responsibility for the use of the software is with
   you.  Under no circumstances the author may be held responsible for
   loss of data, loss of profit, or any other damage resulting
   directly or indirectly from the use of the software, even if the
   damage is caused by defects in the software.
3. You may use this software in any applications you build.
4. You may distribute this software provided that the software
   documentation and copyright notices are included and intact.
5. You may create and distribute modified versions of the software,
   such as ports to other Smalltalk dialects or derived work, provided
   that: 
   a. any modified version is expressly marked as such and is not
   misrepresented as the original software; 
   b. credit is given to the original software in the source code and
   documentation of the derived work; 
   c. the copyright notice at the top of this document accompanies
   copyright notices of any modified version.  "
	self error: 'comment only'! !

!RxParser class methodsFor: 'class initialization' stamp: 'avi 11/30/2003 13:26'!
initialize
	"self initialize"
	self
		initializeBackslashConstants;
		initializeBackslashSpecials! !

!RxParser class methodsFor: 'class initialization' stamp: 'avi 11/30/2003 13:27'!
initializeBackslashConstants
	"self initializeBackslashConstants"
	(BackslashConstants := Dictionary new)
		at: $e put: Character escape;
		at: $n put: Character lf;
		at: $r put: Character cr;
		at: $f put: Character newPage;
		at: $t put: Character tab! !

!RxParser class methodsFor: 'class initialization'!
initializeBackslashSpecials
	"Keys are characters that normally follow a \, the values are
	associations of classes and initialization selectors on the instance side
	of the classes."
	"self initializeBackslashSpecials"
	(BackslashSpecials := Dictionary new)
		at: $w put: (Association key: RxsPredicate value: #beWordConstituent);
		at: $W put: (Association key: RxsPredicate value: #beNotWordConstituent);
		at: $s put: (Association key: RxsPredicate value: #beSpace);
		at: $S put: (Association key: RxsPredicate value: #beNotSpace);
		at: $d put: (Association key: RxsPredicate value: #beDigit);
		at: $D put: (Association key: RxsPredicate value: #beNotDigit);
		at: $b put: (Association key: RxsContextCondition value: #beWordBoundary);
		at: $B put: (Association key: RxsContextCondition value: #beNonWordBoundary);
		at: $< put: (Association key: RxsContextCondition value: #beBeginningOfWord);
		at: $> put: (Association key: RxsContextCondition value: #beEndOfWord)! !

!RxParser class methodsFor: 'utilities'!
parse: aString
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, the corresponding exception is signaled."
	^self new parse: aString! !

!RxParser class methodsFor: 'preferences'!
preferredMatcherClass
	"The matcher to use. For now just one is available, but in
	principle this determines the matchers built implicitly,
	such as by String>>asRegex, or String>>matchesRegex:.
	This might seem a bit strange place for this preference, but
	Parser is still more or less `central' thing in the whole package."
	^RxMatcher! !

!RxParser class methodsFor: 'test suite'!
runProtocolTestsForMatcher: matcherClass
	| matcher |
	Transcript show: 'Testing matcher protocol...'.
	matcher := matcherClass forString: '\w+'.
	(matcher matchesIn: 'now is the time') asArray = #('now' 'is' 'the' 'time')
		ifFalse: [self error: 'matchesIn: test failed'].
	(matcher copy: 'now is  the   time    ' translatingMatchesUsing: [:s | s reverse])
		= 'won si  eht   emit    '
		ifFalse: [self error: 'copy:translatingMatchesWith: test failed'].
	"See that the match context is preserved while copying stuff between matches:"
	((matcherClass forString: '\<\d\D+') 
		copy: '9aaa1bbb 8ccc'
		replacingMatchesWith: 'foo') = 'foo1bbb foo'
			ifFalse: [self error: 'test failed'].
	Transcript show: 'OK'; cr! !

!RxParser class methodsFor: 'test suite'!
runRegexTestsForMatcher: matcherClass
	"Run the whole suite of tests for the given matcher class. May blow up
	if anything goes wrong with the matcher or parser. Since this is a 
	developer's tool, who cares?"
	"self runRegexTestsForMatcher: RxMatcher"
	| failures |
	failures := 0.
	Transcript cr.
	self testSuite do: [:clause |
		| rxSource matcher isOK |
		rxSource := clause first.
		Transcript show: 'Testing regex: '; show: rxSource printString; cr.
		matcher := self compileRegex: rxSource into: matcherClass.
		matcher == nil
			ifTrue:
				[(clause at: 2) isNil
					ifTrue: 
						[Transcript tab; show: 'Compilation error as expected (ok)'; cr]
					ifFalse: 
						[Transcript tab; 
							show: 'Compilation error, UNEXPECTED -- FAILED'; cr.
						failures := failures + 1]]
			ifFalse:
				[(clause at: 2) == nil
					ifTrue: 
						[Transcript tab;
							show: 'Compilation succeeded, should have failed -- FAILED!!';
							cr.
						failures := failures + 1]
					ifFalse:
						[2 to: clause size by: 3 do: 
							[:i |
							isOK := self
								test: matcher
								with: (clause at: i)
								expect: (clause at: i + 1)
								withSubexpressions: (clause at: i + 2).
							isOK ifFalse: [failures := failures + 1].
							Transcript 
								show: (isOK ifTrue: [' (ok).'] ifFalse: [' -- FAILED!!']);
								cr]]]].
	failures = 0
		ifTrue: [Transcript show: 'PASSED ALL TESTS.'; cr]
		ifFalse: [Transcript show: failures printString, ' TESTS FAILED!!'; cr]! !

!RxParser class methodsFor: 'test suite'!
runTestsForMatcher: matcherClass
	"Run the whole suite of tests for the given matcher class. May blow up
	if something goes wrong with the matcher or the parser. Since this is a 
	developer's tool, who cares?"
	"self runTestsForMatcher: RxMatcher"
	self
		runRegexTestsForMatcher: matcherClass;
		runProtocolTestsForMatcher: matcherClass! !

!RxParser class methodsFor: 'utilities' stamp: 'avi 11/30/2003 13:23'!
safelyParse: aString
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, return nil.
	Exception handling here is dialect-dependent."
	^ [self new parse: aString] on: RegexSyntaxError do: [:ex | nil]! !

!RxParser class methodsFor: 'exception signaling' stamp: 'avi 11/30/2003 13:25'!
signalCompilationException: errorString
	RegexCompilationError new signal: errorString! !

!RxParser class methodsFor: 'exception signaling' stamp: 'avi 11/30/2003 13:25'!
signalMatchException: errorString
	RegexMatchingError new signal: errorString! !

!RxParser class methodsFor: 'exception signaling' stamp: 'avi 11/30/2003 13:25'!
signalSyntaxException: errorString
	RegexSyntaxError new signal: errorString! !

!RxParser class methodsFor: 'test suite' stamp: 'stephane.ducasse 4/13/2009 20:32'!
test: aMatcher with: testString expect: expected withSubexpressions: subexpr
	| got |
	Transcript tab; 
		show: 'Matching: ';
		show: testString printString; 
		show: ' expected: '; 
		show: expected printString;
		show: ' got: '.
	got := aMatcher search: testString.
	Transcript show: got printString.
	got asString ~= expected asString
		ifTrue: [^false].
	(subexpr ~= nil and: [aMatcher supportsSubexpressions])
		ifFalse:
			[^true]
		ifTrue:
			[ | isOK |
			isOK := true.
			1 to: subexpr size by: 2 do: [:i |
				| sub subExpect subGot |
				sub := subexpr at: i.
				subExpect := subexpr at: i + 1.
				subGot := aMatcher subexpression: sub.
				Transcript cr; tab; tab;
					show: 'Subexpression: ', sub printString;
					show: ' expected: ';
					show: subExpect printString;
					show: ' got: ';
					show: subGot printString.
				subExpect ~= subGot
					ifTrue: 
					[Transcript show: ' -- MISMATCH'.
					isOK := false]].
			^isOK]! !

!RxParser class methodsFor: 'test suite'!
testSuite
	"Answer an array of test clauses. Each clause is an array with a regex source
	string followed by sequence of 3-tuples. Each three-element
	group is one test to try against the regex, and includes: 1) test string;
	2) expected result; 3) expected subexpression as an array of (index, substring), 
	or nil.
	The test suite is based on the one in Henry Spencer's regexp.c package."
	^#(
		('abc'
			'abc' true (1 'abc')
			'xbc' false nil
			'axc' false nil
			'abx' false nil
			'xabcy' true (1 'abc')
			'ababc' true (1 'abc'))
		('ab*c'
			'abc' true (1 'abc'))
		('ab*bc'
			'abc' true (1 'abc')
			'abbc' true (1 'abbc')
			'abbbbc' true (1 'abbbbc'))
		('ab+bc'	
			'abbc' true (1 'abbc')
			'abc' false nil
			'abq' false nil
			'abbbbc' true (1 'abbbbc'))
		('ab?bc'
			'abbc' true (1 'abbc')
			'abc' true (1 'abc')
			'abbbbc' false nil
			'abc' true (1 'abc'))
		('^abc$'
			'abc' true (1 'abc')
			'abcc' false nil
			'aabc' false nil)
		('^abc'
			'abcc' true (1 'abc'))
		('abc$'
			'aabc' true (1 'abc'))
		('^'
			'abc' true nil)
		('$'
			'abc' true nil)
		('a.c'
			'abc' true (1 'abc')
			'axc' true (1 'axc'))
		('a.*c'	
			'axyzc' true (1 'axyzc')
			'axy zc' true (1 'axy zc') "testing that a dot matches a space"
			'axy
						 zc' false nil "testing that a dot does not match a newline"
			'axyzd' false nil)
		('.a.*'
			'1234abc' true (1 '4abc')
			'abcd' false nil)
		('a\w+c'
			' abbbbc ' true (1 'abbbbc')
			'abb bc' false nil)
		('\w+'
			'  	foobar	quux' true (1 'foobar')
			' 	~!!@#$%^&*()-+=\|/?.>,<' false nil)
		('a\W+c'
			'a   c' true (1 'a   c')
			'a bc' false nil)
		('\W+'
			'foo!!@#$bar' true (1 '!!@#$')
			'foobar' false nil)
		('a\s*c'
			'a   c' true (1 'a   c')
			'a bc' false nil)
		('\s+'
			'abc3457 sd' true (1 ' ')
			'1234$^*^&asdfb' false nil)
		('a\S*c'
			'aqwertyc' true (1 'aqwertyc')
			'ab c' false nil)
		('\S+'
			'     	asdf		' true (1 'asdf')
			' 	
				' false nil)
		('a\d+c'
			'a0123456789c' true (1 'a0123456789c')
			'a12b34c' false nil)
		('\d+'
			'foo@#$%123ASD #$$%^&' true (1 '123')
			'foo!!@#$asdfl;' false nil)
		('a\D+c'
			'aqwertyc' true (1 'aqwertyc')
			'aqw6ertc' false nil)
		('\D+'
			'1234 abc 456' true (1 ' abc ')
			'1234567890' false nil)
		('(f|o)+\b'
			'foo' true (1 'foo')
			' foo ' true (1 'foo'))
		('\ba\w+' "a word beginning with an A"
			'land ancient' true (1 'ancient')
			'antique vase' true (1 'antique')
			'goofy foobar' false nil)
		('(f|o)+\B'
			'quuxfoobar' true (1 'foo')
			'quuxfoo ' true (1 'fo'))
		('\Ba\w+' "a word with an A in the middle, match at A and further"
			'land ancient' true (1 'and')
			'antique vase' true (1 'ase')
			'smalltalk shall overcome' true (1 'alltalk')
			'foonix is better' false nil)
		('fooa\>.*'
			'fooa ' true nil
			'fooa123' false nil
			'fooa bar' true nil
			'fooa' true nil
			'fooargh' false nil)
		('\>.+abc'
			' abcde fg' false nil
			'foo abcde' true (1 ' abc')
			'abcde' false nil)
		('\<foo.*'
			'foo' true nil
			'foobar' true nil
			'qfoobarq foonix' true (1 'foonix')
			' foo' true nil
			' 12foo' false nil
			'barfoo' false nil)
		('.+\<foo'
			'foo' false nil
			'ab foo' true (1 'ab foo')
			'abfoo' false nil)
		('a[bc]d'
			'abc' false nil
			'abd' true (1 'abd'))
		('a[b-d]e'
			'abd' false nil
			'ace' true (1 'ace'))
		('a[b-d]'
			'aac' true (1 'ac'))
		('a[-b]'
			'a-' true (1 'a-'))
		('a[b-]'
			'a-' true (1 'a-'))
		('a[a-b-c]' nil)
		('[k]'
			'ab' false nil)
		('a[b-a]' nil)
		('a[]b' nil)
		('a[' nil)
		('a]' 
			'a]' true (1 'a]'))
		('a[]]b'
			'a]b' true (1 'a]b'))
		('a[^bc]d'
			'aed' true (1 'aed')
			'abd' false nil)
		('a[^-b]c'
			'adc' true (1 'adc')
			'a-c' false nil)
		('a[^]b]c'
			'a]c' false nil
			'adc' true (1 'adc'))
		('[\de]+'
			'01234' true (1 '01234')
			'0123e456' true (1 '0123e456')
			'0123e45g78' true (1 '0123e45'))
		('[e\d]+' "reversal of the above, should be the same"
			'01234' true (1 '01234')
			'0123e456' true (1 '0123e456')
			'0123e45g78' true (1 '0123e45'))
		('[\D]+'
			'123abc45def78' true (1 'abc'))
		('[[:digit:]e]+'
			'01234' true (1 '01234')
			'0123e456' true (1 '0123e456')
			'0123e45g78' true (1 '0123e45'))
		('[\s]+'
			'2  spaces' true (1 '  '))
		('[\S]+'
			'  word12!!@#$  ' true (1 'word12!!@#$'))
		('[\w]+'
			' 	foo123bar	45' true (1 'foo123bar'))
		('[\W]+'
			'fii234!!@#$34f' true (1 '!!@#$'))
		('[^[:alnum:]]+'
			'fii234!!@#$34f' true (1 '!!@#$'))
		('[%&[:alnum:]]+'
			'foo%3' true (1 'foo%3')
			'foo34&rt4$57a' true (1 'foo34&rt4')
			'!!@#$' false nil)
		('[[:alpha:]]+'
			' 123foo3 ' true (1 'foo')
			'123foo' true (1 'foo')
			'foo1b' true (1 'foo'))
		('[[:cntrl:]]+'
			' a 1234asdf' false nil)
		('[[:lower:]]+'
			'UPPERlower1234' true (1 'lower')
			'lowerUPPER' true (1 'lower'))
		('[[:upper:]]+'
			'UPPERlower1234' true (1 'UPPER')
			'lowerUPPER ' true (1 'UPPER'))
		('[[:space:]]+'
			'2  spaces' true (1 '  '))
		('[^[:space:]]+'
			'  word12!!@#$  ' true (1 'word12!!@#$'))
		('[[:graph:]]+'
			'abc' true (1 'abc'))
		('[[:print:]]+'
			'abc' true (1 'abc'))
		('[^[:punct:]]+'
			'!!hello,world!!' true (1 'hello'))
		('[[:xdigit:]]+'
			'  x10FCD  ' true (1 '10FCD')
			' hgfedcba0123456789ABCDEFGH '
				true (1 'fedcba0123456789ABCDEF'))
		('ab|cd'
			'abc' true (1 'ab')
			'abcd' true (1 'ab'))
		('()ef'
			'def' true (1 'ef' 2 ''))
		('()*' nil)
		('*a' nil)
		('^*' nil)
		('$*' nil)
		('(*)b' nil)
		('$b'	'b' false nil)
		('a\' nil)
		('a\(b'
			'a(b' true (1 'a(b'))
		('a\(*b'
			'ab' true (1 'ab')
			'a((b' true (1 'a((b'))
		('a\\b'
			'a\b' true (1 'a\b'))
		('abc)' nil)
		('(abc' nil)
		('((a))'
			'abc' true (1 'a' 2 'a' 3 'a'))
		('(a)b(c)'
			'abc' true (1 'abc' 2 'a' 3 'c'))
		('a+b+c'
			'aabbabc' true (1 'abc'))
		('a**' nil)
		('a*?' nil)
		('(a*)*' nil)
		('(a*)+' nil)
		('(a|)*' nil)
		('(a*|b)*' nil)
		('(a+|b)*'
			'ab' true (1 'ab' 2 'b'))
		('(a+|b)+'
			'ab' true (1 'ab' 2 'b'))
		('(a+|b)?'
			'ab' true (1 'a' 2 'a'))
		('[^ab]*'
			'cde' true (1 'cde'))
		('(^)*' nil)
		('(ab|)*' nil)
		(')(' nil)
		('' 'abc' true (1 ''))
		('abc' '' false nil)
		('a*'
			'' true '')
		('abcd'
			'abcd' true (1 'abcd'))
		('a(bc)d'
			'abcd' true (1 'abcd' 2 'bc'))
		('([abc])*d'
			'abbbcd' true (1 'abbbcd' 2 'c'))
		('([abc])*bcd'
			'abcd' true (1 'abcd' 2 'a'))
		('a|b|c|d|e' 'e' true (1 'e'))
		('(a|b|c|d|e)f' 'ef' true (1 'ef' 2 'e'))
			"	((a*|b))*	-	c	-	-"
		('abcd*efg' 'abcdefg' true (1 'abcdefg'))
		('ab*' 
			'xabyabbbz' true (1 'ab')
			'xayabbbz' true (1 'a'))
		('(ab|cd)e' 'abcde' true (1 'cde' 2 'cd'))
		('[abhgefdc]ij' 'hij' true (1 'hij'))
		('^(ab|cd)e' 'abcde' false nil)
		('(abc|)def' 'abcdef' true nil)
		('(a|b)c*d' 'abcd' true (1 'bcd' 2 'b'))
		('(ab|ab*)bc' 'abc' true (1 'abc' 2 'a'))
		('a([bc]*)c*' 'abc' true (1 'abc' 2 'bc'))
		('a([bc]*)(c*d)' 'abcd' true (1 'abcd' 2 'bc' 3 'd'))
		('a([bc]+)(c*d)' 'abcd' true (1 'abcd' 2 'bc' 3 'd'))
		('a([bc]*)(c+d)' 'abcd' true (1 'abcd' 2 'b' 3 'cd'))
		('a[bcd]*dcdcde' 'adcdcde' true (1 'adcdcde'))
		('a[bcd]+dcdcde' 'adcdcde' false nil)
		('(ab|a)b*c' 'abc' true (1 'abc'))
		('((a)(b)c)(d)' 'abcd' true (1 'abcd' 3 'a' 4 'b' 5 'd'))
		('[ -~]*' 'abc' true (1 'abc'))
		('[ -~ -~]*' 'abc' true (1 'abc'))
		('[ -~ -~ -~]*' 'abc' true (1 'abc'))
		('[ -~ -~ -~ -~]*' 'abc' true (1 'abc'))
		('[ -~ -~ -~ -~ -~]*' 'abc' true (1 'abc'))
		('[ -~ -~ -~ -~ -~ -~]*' 'abc' true (1 'abc'))
		('[ -~ -~ -~ -~ -~ -~ -~]*' 'abc' true (1 'abc'))
		('[a-zA-Z_][a-zA-Z0-9_]*' 'alpha' true (1 'alpha'))
		('^a(bc+|b[eh])g|.h$' 'abh' true (1 'bh' 2 ''))
		('(bc+d$|ef*g.|h?i(j|k))' 
			'effgz' true (1 'effgz' 2 'effgz' 3 '')
			'ij' true (1 'ij' 2 'ij' 3 'j')
			'effg' false nil
			'bcdd' false nil
			'reffgz' true (1 'effgz' 2 'effgz' 3 ''))
		('(((((((((a)))))))))' 'a' true (1 'a'))
		('multiple words of text' 
			'uh-uh' false nil
			'multiple words of text, yeah' true (1 'multiple words of text'))
		('(.*)c(.*)' 'abcde' true (1 'abcde' 2 'ab' 3 'de'))
		('\((.*), (.*)\)' '(a, b)' true (2 'a' 3 'b')))! !

!RxmBranch methodsFor: 'initialize-release'!
alternative: aBranch
	"See class comment for instance variable description."
	alternative := aBranch! !

!RxmBranch methodsFor: 'initialize-release'!
beLoopback
	"See class comment for instance variable description."
	loopback := true! !

!RxmBranch methodsFor: 'initialize-release'!
initialize
	"See class comment for instance variable description."
	loopback := false! !

!RxmBranch methodsFor: 'matching'!
matchAgainst: aMatcher
	"Match either `next' or `alternative'. Fail if the alternative is nil."
	^(next matchAgainst: aMatcher)
		or: [alternative notNil
			and: [alternative matchAgainst: aMatcher]]! !

!RxmBranch methodsFor: 'building'!
pointTailTo: aNode
	"See superclass for explanations."
	loopback
		ifTrue: [alternative == nil
			ifTrue: [alternative := aNode]
			ifFalse: [alternative pointTailTo: aNode]]
		ifFalse: [super pointTailTo: aNode]! !

!RxmBranch methodsFor: 'building'!
terminateWith: aNode
	"See superclass for explanations."
	loopback
		ifTrue: [alternative == nil
			ifTrue: [alternative := aNode]
			ifFalse: [alternative terminateWith: aNode]]
		ifFalse: [super terminateWith: aNode]! !

!RxmLink methodsFor: 'initialize-release'!
initialize
	"Subclasses may want to initialize."! !

!RxmLink methodsFor: 'matching'!
matchAgainst: aMatcher
	"If a link does not match the contents of the matcher's stream,
	answer false. Otherwise, let the next matcher in the chain match."
	^next matchAgainst: aMatcher! !

!RxmLink methodsFor: 'matching'!
next
	
	^next! !

!RxmLink methodsFor: 'initialize-release'!
next: aLink
	"Set the next link, either an RxmLink or an RxmTerminator."
	next := aLink! !

!RxmLink methodsFor: 'building'!
pointTailTo: anRxmLink
	"Propagate this message along the chain of links.
	Point `next' reference of the last link to <anRxmLink>.
	If the chain is already terminated, blow up."
	next == nil
		ifTrue: [next := anRxmLink]
		ifFalse: [next pointTailTo: anRxmLink]! !

!RxmLink methodsFor: 'building'!
terminateWith: aTerminator
	"Propagate this message along the chain of links, and
	make aTerminator the `next' link of the last link in the chain.
	If the chain is already reminated with the same terminator, 
	do not blow up."
	next == nil
		ifTrue: [next := aTerminator]
		ifFalse: [next terminateWith: aTerminator]! !

!RxmLink class methodsFor: 'instance creation'!
new
	^super new initialize! !

!RxmMarker methodsFor: 'initialize-release'!
index: anIndex
	"An index is a key that makes sense for the matcher.
	This key can be passed to marker position getters and
	setters to access position for this marker in the current
	matching session."
	index := anIndex! !

!RxmMarker methodsFor: 'matching'!
matchAgainst: aMatcher
	"If the rest of the link chain matches successfully, report the
	position of the stream *before* the match started to the matcher."
	| startPosition |
	startPosition := aMatcher position.
	(next matchAgainst: aMatcher)
		ifTrue:
			[aMatcher markerPositionAt: index maybePut: startPosition.
			^true].
	^false! !

!RxmPredicate methodsFor: 'initialize-release'!
bePerform: aSelector
	"Match any single character that answers true  to this message."
	self predicate: 
		[:char | 
		RxParser doHandlingMessageNotUnderstood: [char perform: aSelector]]! !

!RxmPredicate methodsFor: 'initialize-release'!
bePerformNot: aSelector
	"Match any single character that answers false to this message."
	self predicate: 
		[:char | 
		RxParser doHandlingMessageNotUnderstood: [(char perform: aSelector) not]]! !

!RxmPredicate methodsFor: 'matching'!
matchAgainst: aMatcher
	"Match if the predicate block evaluates to true when given the
	current stream character as the argument."
	| original |
	original := aMatcher currentState.
	(aMatcher atEnd not 
		and: [(predicate value: aMatcher next)
			and: [next matchAgainst: aMatcher]])
		ifTrue: [^true]
		ifFalse:
			[aMatcher restoreState: original.
			^false]! !

!RxmPredicate methodsFor: 'initialize-release'!
predicate: aBlock
	"This link will match any single character for which <aBlock>
	evaluates to true."
	aBlock numArgs ~= 1 ifTrue: [self error: 'bad predicate block'].
	predicate := aBlock.
	^self! !

!RxmPredicate class methodsFor: 'instance creation'!
with: unaryBlock
	^self new predicate: unaryBlock! !

!RxmSpecial methodsFor: 'initialize-release'!
beBeginningOfLine
	matchSelector := #atBeginningOfLine! !

!RxmSpecial methodsFor: 'initialize-release'!
beBeginningOfWord
	matchSelector := #atBeginningOfWord! !

!RxmSpecial methodsFor: 'initialize-release'!
beEndOfLine
	matchSelector := #atEndOfLine! !

!RxmSpecial methodsFor: 'initialize-release'!
beEndOfWord
	matchSelector := #atEndOfWord! !

!RxmSpecial methodsFor: 'initialize-release'!
beNotWordBoundary
	matchSelector := #notAtWordBoundary! !

!RxmSpecial methodsFor: 'initialize-release'!
beWordBoundary
	matchSelector := #atWordBoundary! !

!RxmSpecial methodsFor: 'matching'!
matchAgainst: aMatcher
	"Match without consuming any input, if the matcher is
	in appropriate state."
	^(aMatcher perform: matchSelector)
		and: [next matchAgainst: aMatcher]! !

!RxmSubstring methodsFor: 'initialize-release'!
beCaseInsensitive
	compare := [:char1 :char2 | char1 sameAs: char2]! !

!RxmSubstring methodsFor: 'initialize-release'!
beCaseSensitive
	compare := [:char1 :char2 | char1 = char2]! !

!RxmSubstring methodsFor: 'initialize-release'!
character: aCharacter ignoreCase: aBoolean
	"Match exactly this character."
	sample := String with: aCharacter.
	aBoolean ifTrue: [self beCaseInsensitive]! !

!RxmSubstring methodsFor: 'initialize-release'!
initialize
	super initialize.
	self beCaseSensitive! !

!RxmSubstring methodsFor: 'matching'!
matchAgainst: aMatcher
	"Match if my sample stream is exactly the current prefix
	of the matcher stream's contents."
	| originalState sampleStream mismatch |
	originalState := aMatcher currentState.
	sampleStream := self sampleStream.
	mismatch := false.
	[sampleStream atEnd
		or: [aMatcher atEnd
		or: [mismatch := (compare value: sampleStream next value: aMatcher next) not]]] whileFalse.
	(mismatch not and: [sampleStream atEnd and: [next matchAgainst: aMatcher]])
		ifTrue: [^true]
		ifFalse: 
			[aMatcher restoreState: originalState.
			^false]! !

!RxmSubstring methodsFor: 'private'!
sampleStream
	^sample readStream! !

!RxmSubstring methodsFor: 'initialize-release'!
substring: aString ignoreCase: aBoolean
	"Match exactly this string."
	sample := aString.
	aBoolean ifTrue: [self beCaseInsensitive]! !

!RxmTerminator methodsFor: 'matching'!
matchAgainst: aStream
	"If got here, the match is successful."
	^true! !

!RxmTerminator methodsFor: 'building'!
pointTailTo: anRxmLink
	"Branch tails are never redirected by the build algorithm.
	Healthy terminators should never receive this."
	RxParser signalCompilationException:
		'internal matcher build error - redirecting terminator tail'! !

!RxmTerminator methodsFor: 'building'!
terminateWith: aTerminator
	"Branch terminators are never supposed to change.
	Make sure this is the case."
	aTerminator ~~ self
		ifTrue: [RxParser signalCompilationException:
				'internal matcher build error - wrong terminator']! !

!RxsBranch methodsFor: 'accessing'!
branch
	^branch! !

!RxsBranch methodsFor: 'accessing'!
dispatchTo: aMatcher
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aMatcher syntaxBranch: self! !

!RxsBranch methodsFor: 'initialize-release'!
initializePiece: aPiece branch: aBranch
	"See class comment for instance variables description."
	piece := aPiece.
	branch := aBranch! !

!RxsBranch methodsFor: 'testing'!
isNullable
	^piece isNullable and: [branch isNil or: [branch isNullable]]! !

!RxsBranch methodsFor: 'accessing'!
piece
	^piece! !

!RxsBranch methodsFor: 'optimization'!
tryMergingInto: aStream
	"Concatenation of a few simple characters can be optimized
	to be a plain substring match. Answer the node to resume
	syntax tree traversal at. Epsilon node used to terminate the branch
	will implement this to answer nil, thus indicating that the branch
	has ended."
	piece isAtomic ifFalse: [^self].
	aStream nextPut: piece character.
	^branch isNil
		ifTrue: [branch]
		ifFalse: [branch tryMergingInto: aStream]! !

!RxsCharSet methodsFor: 'accessing'!
dispatchTo: aMatcher
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aMatcher syntaxCharSet: self! !

!RxsCharSet methodsFor: 'privileged'!
enumerablePartPredicate
	| enumeration |
	enumeration := self optimalSet.
	^negated
		ifTrue: [[:char | (enumeration includes: char) not]]
		ifFalse: [[:char | enumeration includes: char]]! !

!RxsCharSet methodsFor: 'privileged'!
enumerableSet
	"Answer a collection of characters that make up the portion of me
	that can be enumerated."
	| set |
	set := Set new.
	elements do:
		[:each |
		each isEnumerable ifTrue: [each enumerateTo: set]].
	^set! !

!RxsCharSet methodsFor: 'accessing' stamp: 'avi 11/30/2003 13:28'!
hasPredicates
	^elements anySatisfy: [:some | some isEnumerable not]! !

!RxsCharSet methodsFor: 'initialize-release'!
initializeElements: aCollection negated: aBoolean
	"See class comment for instance variables description."
	elements := aCollection.
	negated := aBoolean! !

!RxsCharSet methodsFor: 'testing'!
isEnumerable
	elements detect: [:some | some isEnumerable not] ifNone: [^true].
	^false! !

!RxsCharSet methodsFor: 'testing'!
isNegated
	^negated! !

!RxsCharSet methodsFor: 'privileged' stamp: 'avi 11/30/2003 13:28'!
optimalSet
	"Assuming the client with search the `set' using #includes:,
	answer a collection with the contents of `set', of the class
	that will provide the fastest lookup. Strings are faster than
	Sets for short strings."
	| set |
	set := self enumerableSet.
	^set size < 10
		ifTrue: [String withAll: set asArray]
		ifFalse: [set]! !

!RxsCharSet methodsFor: 'accessing'!
predicate
	| predicate enumerable |
	enumerable := self enumerablePartPredicate.
	^self hasPredicates
		ifFalse: [enumerable]
		ifTrue:
			[predicate := self predicatePartPredicate.
			negated
				ifTrue: [[:char | (enumerable value: char) and: [predicate value: char]]]
				ifFalse: [[:char | (enumerable value: char) or: [predicate value: char]]]]! !

!RxsCharSet methodsFor: 'privileged'!
predicatePartPredicate
	"Answer a predicate that tests all of my elements that cannot be
	enumerated."
	| predicates |
	predicates := elements reject: [:some | some isEnumerable].
	predicates isEmpty
		ifTrue: [^[:char | negated]].
	predicates size = 1
		ifTrue: [^negated
			ifTrue: [predicates first predicateNegation]
			ifFalse: [predicates first predicate]].
	predicates := predicates collect: [:each | each predicate].
	^negated
		ifFalse:
			[[:char | predicates contains: [:some | some value: char]]]
		ifTrue:
			[[:char | (predicates contains: [:some | some value: char]) not]]! !

!RxsCharSet methodsFor: 'accessing'!
predicates
	^(elements reject: [:some | some isEnumerable])
		collect: [:each | each predicate]! !

!RxsCharacter methodsFor: 'accessing'!
character
	^character! !

!RxsCharacter methodsFor: 'accessing'!
dispatchTo: aMatcher
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aMatcher syntaxCharacter: self! !

!RxsCharacter methodsFor: 'accessing'!
enumerateTo: aCollection
	aCollection add: character! !

!RxsCharacter methodsFor: 'initialize-release'!
initializeCharacter: aCharacter
	"See class comment for instance variable description."
	character := aCharacter! !

!RxsCharacter methodsFor: 'testing'!
isAtomic
	"A character is always atomic."
	^true! !

!RxsCharacter methodsFor: 'testing'!
isEnumerable
	^true! !

!RxsCharacter methodsFor: 'testing'!
isNullable
	^false! !

!RxsCharacter class methodsFor: 'instance creation'!
with: aCharacter
	^self new initializeCharacter: aCharacter! !

!RxsContextCondition methodsFor: 'initialize-release'!
beAny
	"Matches anything but a newline."
	kind := #syntaxAny! !

!RxsContextCondition methodsFor: 'initialize-release'!
beBeginningOfLine
	"Matches empty string at the beginning of a line."
	kind := #syntaxBeginningOfLine! !

!RxsContextCondition methodsFor: 'initialize-release'!
beBeginningOfWord
	"Matches empty string at the beginning of a word."
	kind := #syntaxBeginningOfWord! !

!RxsContextCondition methodsFor: 'initialize-release'!
beEndOfLine
	"Matches empty string at the end of a line."
	kind := #syntaxEndOfLine! !

!RxsContextCondition methodsFor: 'initialize-release'!
beEndOfWord
	"Matches empty string at the end of a word."
	kind := #syntaxEndOfWord! !

!RxsContextCondition methodsFor: 'initialize-release'!
beNonWordBoundary
	"Analog of \B."
	kind := #syntaxNonWordBoundary! !

!RxsContextCondition methodsFor: 'initialize-release'!
beWordBoundary
	"Analog of \w (alphanumeric plus _)."
	kind := #syntaxWordBoundary! !

!RxsContextCondition methodsFor: 'accessing'!
dispatchTo: aBuilder
	^aBuilder perform: kind! !

!RxsContextCondition methodsFor: 'testing'!
isNullable
	^#syntaxAny ~~ kind! !

!RxsEpsilon methodsFor: 'building'!
dispatchTo: aBuilder
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aBuilder syntaxEpsilon! !

!RxsEpsilon methodsFor: 'testing'!
isNullable
	"See comment in the superclass."
	^true! !

!RxsMessagePredicate methodsFor: 'accessing'!
dispatchTo: aBuilder
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aBuilder syntaxMessagePredicate: self! !

!RxsMessagePredicate methodsFor: 'initialize-release'!
initializeSelector: aSelector
	"The selector must be a one-argument message understood by Character."
	selector := aSelector! !

!RxsMessagePredicate methodsFor: 'initialize-release'!
initializeSelector: aSelector negated: aBoolean
	"The selector must be a one-argument message understood by Character."
	selector := aSelector.
	negated := aBoolean! !

!RxsMessagePredicate methodsFor: 'accessing'!
negated
	^negated! !

!RxsMessagePredicate methodsFor: 'accessing'!
selector
	^selector! !

!RxsNode methodsFor: 'constants'!
indentCharacter
	"Normally, #printOn:withIndent: method in subclasses
	print several characters returned by this method to indicate
	the tree structure."
	^$+! !

!RxsNode methodsFor: 'testing'!
isAtomic
	"Answer whether the node is atomic, i.e. matches exactly one 
	constant predefined normal character.  A matcher may decide to 
	optimize matching of a sequence of atomic nodes by glueing them 
	together in a string."
	^false "tentatively"! !

!RxsNode methodsFor: 'testing'!
isNullable
	"True if the node can match an empty sequence of characters."
	^false "for most nodes"! !

!RxsPiece methodsFor: 'accessing'!
atom
	^atom! !

!RxsPiece methodsFor: 'accessing'!
character
	"If this node is atomic, answer the character it
	represents. It is the caller's responsibility to make sure this
	node is indeed atomic before using this."
	^atom character! !

!RxsPiece methodsFor: 'accessing'!
dispatchTo: aMatcher
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aMatcher syntaxPiece: self! !

!RxsPiece methodsFor: 'initialize-release'!
initializeAtom: anAtom
	"This piece is exactly one occurrence of the specified RxsAtom."
	self initializeAtom: anAtom min: 1 max: 1! !

!RxsPiece methodsFor: 'initialize-release'!
initializeAtom: anAtom min: minOccurrences max: maxOccurrences
	"This piece is from <minOccurrences> to <maxOccurrences> 
	occurrences of the specified RxsAtom."
	atom := anAtom.
	min := minOccurrences.
	max := maxOccurrences! !

!RxsPiece methodsFor: 'initialize-release'!
initializeOptionalAtom: anAtom
	"This piece is 0 or 1 occurrences of the specified RxsAtom."
	self initializeAtom: anAtom min: 0 max: 1! !

!RxsPiece methodsFor: 'initialize-release'!
initializePlusAtom: anAtom
	"This piece is one or more occurrences of the specified RxsAtom."
	self initializeAtom: anAtom min: 1 max: nil! !

!RxsPiece methodsFor: 'initialize-release'!
initializeStarAtom: anAtom
	"This piece is any number of occurrences of the atom."
	self initializeAtom: anAtom min: 0 max: nil! !

!RxsPiece methodsFor: 'testing'!
isAtomic
	"A piece is atomic if only it contains exactly one atom
	which is atomic (sic)."
	^self isSingular and: [atom isAtomic]! !

!RxsPiece methodsFor: 'testing'!
isNullable
	"A piece is nullable if it allows 0 matches. 
	This is often handy to know for optimization."
	^min = 0 or: [atom isNullable]! !

!RxsPiece methodsFor: 'testing'!
isOptional
	^min = 0 and: [max = 1]! !

!RxsPiece methodsFor: 'testing'!
isPlus
	^min = 1 and: [max == nil]! !

!RxsPiece methodsFor: 'testing'!
isSingular
	"A piece with a range is 1 to 1 needs can be compiled
	as a simple match."
	^min = 1 and: [max = 1]! !

!RxsPiece methodsFor: 'testing'!
isStar
	^min = 0 and: [max == nil]! !

!RxsPiece methodsFor: 'accessing'!
max
	"The value answered may be nil, indicating infinity."
	^max! !

!RxsPiece methodsFor: 'accessing'!
min
	^min! !

!RxsPredicate methodsFor: 'initialize-release'!
beAlphaNumeric
	predicate := [:char | char isAlphaNumeric].
	negation := [:char | char isAlphaNumeric not]! !

!RxsPredicate methodsFor: 'initialize-release'!
beAlphabetic
	predicate := [:char | char isAlphabetic].
	negation := [:char | char isAlphabetic not]! !

!RxsPredicate methodsFor: 'initialize-release'!
beAny
	| cr lf |
	cr := Character cr.
	lf := Character lf.
	predicate := [:char | char ~= lf and: [char ~= cr]].
	negation := [:char | char = lf or: [char = cr]]! !

!RxsPredicate methodsFor: 'initialize-release'!
beControl
	predicate := [:char | char asInteger < 32].
	negation := [:char | char asInteger >= 32]! !

!RxsPredicate methodsFor: 'initialize-release'!
beDigit
	predicate := [:char | char isDigit].
	negation := [:char | char isDigit not]! !

!RxsPredicate methodsFor: 'initialize-release'!
beGraphics
	self
		beControl;
		negate! !

!RxsPredicate methodsFor: 'initialize-release'!
beHexDigit
	| hexLetters |
	hexLetters := 'abcdefABCDEF'.
	predicate := [:char | char isDigit or: [hexLetters includes: char]].
	negation := [:char | char isDigit not and: [(hexLetters includes: char) not]]! !

!RxsPredicate methodsFor: 'initialize-release'!
beLowercase
	predicate := [:char | char isLowercase].
	negation := [:char | char isLowercase not]! !

!RxsPredicate methodsFor: 'initialize-release'!
beNotDigit
	self
		beDigit;
		negate! !

!RxsPredicate methodsFor: 'initialize-release'!
beNotSpace
	self
		beSpace;
		negate! !

!RxsPredicate methodsFor: 'initialize-release'!
beNotWordConstituent
	self
		beWordConstituent;
		negate! !

!RxsPredicate methodsFor: 'initialize-release'!
bePrintable
	self
		beControl;
		negate! !

!RxsPredicate methodsFor: 'initialize-release'!
bePunctuation
	| punctuationChars |
	punctuationChars := #($. $, $!! $; $: $" $' $- $( $) $`).
	predicate := [:char | punctuationChars includes: char].
	negation := [:char | (punctuationChars includes: char) not]! !

!RxsPredicate methodsFor: 'initialize-release'!
beSpace
	predicate := [:char | char isSeparator].
	negation := [:char | char isSeparator not]! !

!RxsPredicate methodsFor: 'initialize-release'!
beUppercase
	predicate := [:char | char isUppercase].
	negation := [:char | char isUppercase not]! !

!RxsPredicate methodsFor: 'initialize-release'!
beWordConstituent
	predicate := [:char | char isAlphaNumeric].
	negation := [:char | char isAlphaNumeric not]! !

!RxsPredicate methodsFor: 'accessing'!
dispatchTo: anObject
	^anObject syntaxPredicate: self! !

!RxsPredicate methodsFor: 'testing'!
isAtomic
	"A predicate is a single character but the character is not known in advance."
	^false! !

!RxsPredicate methodsFor: 'testing'!
isEnumerable
	^false! !

!RxsPredicate methodsFor: 'private'!
negate
	| tmp |
	tmp := predicate.
	predicate := negation.
	negation := tmp! !

!RxsPredicate methodsFor: 'accessing'!
negated
	^self copy negate! !

!RxsPredicate methodsFor: 'accessing'!
predicate
	^predicate! !

!RxsPredicate methodsFor: 'accessing'!
predicateNegation
	^negation! !

!RxsPredicate methodsFor: 'accessing'!
value: aCharacter
	^predicate value: aCharacter! !

!RxsPredicate class methodsFor: 'instance creation'!
forEscapedLetter: aCharacter
	^self new perform:
		(EscapedLetterSelectors
			at: aCharacter
			ifAbsent: [RxParser signalSyntaxException: 'bad backslash escape'])! !

!RxsPredicate class methodsFor: 'instance creation'!
forNamedClass: aString
	^self new perform:
		(NamedClassSelectors
			at: aString
			ifAbsent: [RxParser signalSyntaxException: 'bad character class name'])! !

!RxsPredicate class methodsFor: 'class initialization'!
initialize
	"self initialize"
	self
		initializeNamedClassSelectors;
		initializeEscapedLetterSelectors! !

!RxsPredicate class methodsFor: 'class initialization'!
initializeEscapedLetterSelectors
	"self initializeEscapedLetterSelectors"
	(EscapedLetterSelectors := Dictionary new)
		at: $w put: #beWordConstituent;
		at: $W put: #beNotWordConstituent;
		at: $d put: #beDigit;
		at: $D put: #beNotDigit;
		at: $s put: #beSpace;
		at: $S put: #beNotSpace! !

!RxsPredicate class methodsFor: 'class initialization'!
initializeNamedClassSelectors
	"self initializeNamedClassSelectors"
	(NamedClassSelectors := Dictionary new)
		at: 'alnum' put: #beAlphaNumeric;
		at: 'alpha' put: #beAlphabetic;
		at: 'cntrl' put: #beControl;
		at: 'digit' put: #beDigit;
		at: 'graph' put: #beGraphics;
		at: 'lower' put: #beLowercase;
		at: 'print' put: #bePrintable;
		at: 'punct' put: #bePunctuation;
		at: 'space' put: #beSpace;
		at: 'upper' put: #beUppercase;
		at: 'xdigit' put: #beHexDigit! !

!RxsRange methodsFor: 'accessing'!
enumerateTo: aCollection
	"Add all of the elements I represent to the collection."
	first asInteger to: last asInteger do:
		[:charCode |
		aCollection add: charCode asCharacter]! !

!RxsRange methodsFor: 'initialize-release'!
initializeFirst: aCharacter last: anotherCharacter
	first := aCharacter.
	last := anotherCharacter! !

!RxsRange methodsFor: 'testing'!
isEnumerable
	^true! !

!RxsRange class methodsFor: 'instance creation'!
from: aCharacter to: anotherCharacter
	^self new initializeFirst: aCharacter last: anotherCharacter! !

!RxsRegex methodsFor: 'accessing'!
branch
	^branch! !

!RxsRegex methodsFor: 'accessing'!
dispatchTo: aMatcher
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aMatcher syntaxRegex: self! !

!RxsRegex methodsFor: 'initialize-release'!
initializeBranch: aBranch regex: aRegex
	"See class comment for instance variable description."
	branch := aBranch.
	regex := aRegex! !

!RxsRegex methodsFor: 'testing'!
isNullable
	^branch isNullable or: [regex notNil and: [regex isNullable]]! !

!RxsRegex methodsFor: 'accessing'!
regex
	^regex! !

!String methodsFor: '*vbregex'!
allRegexMatches: rxString
	^rxString asRegex matchesIn: self! !

!String methodsFor: '*vbregex'!
asRegex
	"Compile the receiver as a regex matcher. May raise RxParser>>syntaxErrorSignal
	or RxParser>>compilationErrorSignal.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."
	^RxParser preferredMatcherClass for: (RxParser new parse: self)! !

!String methodsFor: '*vbregex'!
asRegexIgnoringCase
	"Compile the receiver as a regex matcher. May raise RxParser>>syntaxErrorSignal
	or RxParser>>compilationErrorSignal.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."
	^RxParser preferredMatcherClass
		for: (RxParser new parse: self)
		ignoreCase: true! !

!String methodsFor: '*vbregex'!
copyWithRegex: rxString matchesReplacedWith: aString
	^rxString asRegex
		copy: self replacingMatchesWith: aString! !

!String methodsFor: '*vbregex'!
copyWithRegex: rxString matchesTranslatedUsing: aBlock
	^rxString asRegex
		copy: self translatingMatchesUsing: aBlock! !

!String methodsFor: '*vbregex'!
matchesRegex: regexString
	"Test if the receiver matches a regex.  May raise RxParser>>regexErrorSignal or
	child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."
	^regexString asRegex matches: self! !

!String methodsFor: '*vbregex'!
matchesRegexIgnoringCase: regexString
	"Test if the receiver matches a regex.  May raise RxParser>>regexErrorSignal or
	child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."
	^regexString asRegexIgnoringCase matches: self! !

!String methodsFor: '*vbregex'!
prefixMatchesRegex: regexString
	"Test if the receiver's prefix matches a regex.	
	May raise RxParser class>>regexErrorSignal or child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."
	^regexString asRegex matchesPrefix: self! !

!String methodsFor: '*vbregex'!
prefixMatchesRegexIgnoringCase: regexString
	"Test if the receiver's prefix matches a regex.	
	May raise RxParser class>>regexErrorSignal or child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."
	^regexString asRegexIgnoringCase matchesPrefix: self! !

!String methodsFor: '*vbregex'!
regex: rxString matchesCollect: aBlock
	^rxString asRegex matchesIn: self collect: aBlock! !

!String methodsFor: '*vbregex'!
regex: rxString matchesDo: aBlock
	^rxString asRegex matchesIn: self do: aBlock! !
RxMatcher initialize!
RxParser initialize!
RxsPredicate initialize!
