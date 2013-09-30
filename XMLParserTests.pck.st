'From Cuis 4.1 of 12 December 2012 [latest update: #1578] on 28 January 2013 at 8:11:35 am'!
'Description Split out from XMLParser'!
!classDefinition: #OPOpaxHandlerTestClass category: #'XMLParserTests-Opax'!
OPOpaxHandler subclass: #OPOpaxHandlerTestClass
	instanceVariableNames: 'cStackGet cStackSet cStartDocument cEndDocument cEndElement cStartElement cChar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XMLParserTests-Opax'!
!classDefinition: 'OPOpaxHandlerTestClass class' category: #'XMLParserTests-Opax'!
OPOpaxHandlerTestClass class
	instanceVariableNames: ''!

!classDefinition: #OPOpaxTest category: #'XMLParserTests-Opax'!
TestCase subclass: #OPOpaxTest
	instanceVariableNames: 'packages'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XMLParserTests-Opax'!
!classDefinition: 'OPOpaxTest class' category: #'XMLParserTests-Opax'!
OPOpaxTest class
	instanceVariableNames: ''!

!classDefinition: #OPTest1Element category: #'XMLParserTests-Opax'!
OPGenericElement subclass: #OPTest1Element
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XMLParserTests-Opax'!
!classDefinition: 'OPTest1Element class' category: #'XMLParserTests-Opax'!
OPTest1Element class
	instanceVariableNames: ''!

!classDefinition: #OPTest2Element category: #'XMLParserTests-Opax'!
OPGenericElement subclass: #OPTest2Element
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XMLParserTests-Opax'!
!classDefinition: 'OPTest2Element class' category: #'XMLParserTests-Opax'!
OPTest2Element class
	instanceVariableNames: ''!

!classDefinition: #XMLNodeTest category: #'XMLParserTests-Parser'!
TestCase subclass: #XMLNodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XMLParserTests-Parser'!
!classDefinition: 'XMLNodeTest class' category: #'XMLParserTests-Parser'!
XMLNodeTest class
	instanceVariableNames: ''!

!classDefinition: #XMLParserTest category: #'XMLParserTests-Parser'!
TestCase subclass: #XMLParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XMLParserTests-Parser'!
!classDefinition: 'XMLParserTest class' category: #'XMLParserTests-Parser'!
XMLParserTest class
	instanceVariableNames: ''!


!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/28/2009 13:30'!
cChar
	^cChar	! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/28/2009 13:30'!
cEndDocument
	^cEndDocument ! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/28/2009 13:30'!
cEndElement
	^cEndElement ! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/28/2009 13:29'!
cStackGet 
	^cStackGet! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/28/2009 13:29'!
cStackSet 
	^cStackSet ! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/28/2009 13:29'!
cStartDocument 
	^cStartDocument ! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/28/2009 13:30'!
cStartElement
	^cStartElement ! !

!OPOpaxHandlerTestClass methodsFor: 'public interface' stamp: 'fabrizio.perin 7/27/2009 16:41'!
characters: aString
	self stack last characters: aString.
	cChar := cChar + 1.! !

!OPOpaxHandlerTestClass methodsFor: 'public interface' stamp: 'FabrizioPerin 3/26/2010 15:37'!
endDocument
	cEndDocument := cEndDocument + 1.
	^super endDocument.! !

!OPOpaxHandlerTestClass methodsFor: 'public interface' stamp: 'FabrizioPerin 3/26/2010 15:32'!
endElement: qualifiedName prefix: prefix uri: namespaceURI localName: localName 
	"indicates the end of an element. See startElement"
	((stack last class xmlTags includes: localName) or: [stack last isMemberOf: OPGenericElement]) ifTrue: [stack removeLast: 1].
	cEndElement := cEndElement + 1.! !

!OPOpaxHandlerTestClass methodsFor: 'initialize' stamp: 'fabrizio.perin 7/27/2009 16:51'!
initialize
	super initialize.
	cStackGet := 0.
	cStackSet := 0.
	cStartDocument := 0.
	cEndDocument := 0.
	cEndElement := 0.
	cStartElement := 0.
	cChar:= 0.! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/27/2009 16:45'!
stack
	cStackGet := cStackGet + 1.
	^stack
	
	! !

!OPOpaxHandlerTestClass methodsFor: 'accessing' stamp: 'fabrizio.perin 7/27/2009 16:44'!
stack: anObject
	stack := anObject.
	cStackSet := cStackSet + 1.
	! !

!OPOpaxHandlerTestClass methodsFor: 'public interface' stamp: 'fabrizio.perin 7/27/2009 16:45'!
startDocument
	stack := OrderedCollection with: (OPRootElement new).
	cStartDocument := cStartDocument + 1.! !

!OPOpaxHandlerTestClass methodsFor: 'public interface' stamp: 'FabrizioPerin 3/9/2010 13:00'!
startElement: localName prefix: prefix uri: namespaceUri attributes: attributes
	| currentElement |
	currentElement := OPGenericElement newElementWithTag: localName.
	currentElement attributes: attributes.
	stack last addChildLast: currentElement.
	stack addLast: currentElement.
	cStartElement := cStartElement + 1.! !

!OPOpaxHandlerTestClass methodsFor: 'public interface' stamp: 'FabrizioPerin 3/26/2010 15:36'!
startElement: aQualifiedName prefix: prefix uri: namespaceUri localName: localName attributes: attributes
	| currentElement |
	currentElement := OPGenericElement newElementWithTag: localName.
	currentElement attributes: attributes.
	stack last addChildLast: currentElement.
	stack addLast: currentElement.
	cStartElement := cStartElement + 1.! !

!OPOpaxTest methodsFor: 'testing' stamp: 'FP 7/22/2009 15:25'!
lessSimpleXmlSample
	^'
<Test1>
	<Test1>
	</Test1>
	<Test2>
		<Test1>
		</Test1>
	</Test2>
</Test1>
'! !

!OPOpaxTest methodsFor: 'readme' stamp: 'Alexandre Bergel 5/21/2010 11:44'!
readme
"
Opax is a small addition to SAXParser that enables dealing with tags polymorphically.
"! !

!OPOpaxTest methodsFor: 'testing' stamp: 'FP 7/22/2009 15:25'!
simpleXmlSample
	^'
<Test1>
</Test1>
'! !

!OPOpaxTest methodsFor: 'testing' stamp: 'FP 7/22/2009 15:26'!
testCreation
	self assert: ((OPGenericElement newElementWithTag: 'test1') isMemberOf: OPTest1Element).
	self assert: ((OPGenericElement newElementWithTag: 'anotherTest1') isMemberOf: OPTest1Element).
	self assert: ((OPGenericElement newElementWithTag: 'some random string') isMemberOf: OPGenericElement)! !

!OPOpaxTest methodsFor: 'testing' stamp: 'fabrizio_perin 8/13/2009 11:21'!
testLessSimpleXml
	| parser root test1RootElement |
	parser := OPOpaxHandler on: self lessSimpleXmlSample readStream.
	parser startDocument.
	parser parseDocument.
	
	root := parser stack first.
	
	self assert: parser stack size = 1.
	self assert: (root isMemberOf: OPRootElement).
	self assert: root children size = 1.
	
	test1RootElement := root children first.
	
	self assert: (test1RootElement isMemberOf: OPTest1Element).
	self assert: test1RootElement tag = 'Test1'.
	self assert: test1RootElement children size = 2.
	self assert: (test1RootElement children first isMemberOf: OPTest1Element).
	self assert: (test1RootElement children last isMemberOf: OPTest2Element).
	self assert: test1RootElement children first tag = 'Test1'.
	self assert: test1RootElement children last tag = 'Test2'.
	self assert: root allChildren size = 4! !

!OPOpaxTest methodsFor: 'testing' stamp: 'FabrizioPerin 3/5/2010 14:14'!
testOPOpaxDriverTestClass

	| parser root |
	
	parser := OPOpaxHandlerTestClass on: self simpleXmlSample readStream.
	parser startDocument.
	parser parseDocument.


	self assert: (parser cStackGet = 0).
	self assert: (parser cStackSet = 0).
	self assert: (parser cStartDocument > 0).
	self assert: (parser cEndDocument > 0).
	self assert: (parser cEndElement > 0).
	self assert: (parser cStartElement > 0).
	self assert: (parser cChar = 0).


	root := parser stack first.
	
	self assert: (parser stack size = 1).
	self assert: (root isMemberOf: OPRootElement).
	self assert: (root children size = 1).
	self assert: (root children first isMemberOf: OPTest1Element).
	! !

!OPOpaxTest methodsFor: 'testing' stamp: 'fabrizio_perin 8/12/2009 16:37'!
testSimpleXml
	| parser root |
	parser := OPOpaxHandler on: self simpleXmlSample readStream.
	parser startDocument.
	parser parseDocument.
	root := parser stack first.
	self assert: parser stack size = 1.
	self assert: (root isMemberOf: OPRootElement).
	self assert: root children size = 1.
	self assert: (root children first isMemberOf: OPTest1Element)! !

!OPOpaxTest methodsFor: 'testing' stamp: 'fabrizio_perin 8/12/2009 16:37'!
testXmlWithAttributes
	| parser root test1RootElement |
	parser := OPOpaxHandler on: self xmlWithAttributesSample readStream.
	parser startDocument.
	parser parseDocument.
	root := parser stack first.
	self assert: parser stack size = 1.
	self assert: (root isMemberOf: OPRootElement).
	self assert: root children size = 1.
	test1RootElement := root children first.
	self assert: test1RootElement attributes size = 1.
	self assert: (test1RootElement attributeNamed: 'name') = 'test1'.
	self assert: (test1RootElement isMemberOf: OPTest1Element).
	self assert: test1RootElement children size = 2.
	self assert: (test1RootElement children first isMemberOf: OPTest1Element).
	self assert: (test1RootElement children last isMemberOf: OPTest2Element)! !

!OPOpaxTest methodsFor: 'testing' stamp: 'FabrizioPerin 10/22/2009 19:50'!
testXmlWithAttributesAndHeader
	| parser root test1RootElement |
	parser := OPOpaxHandler on: self xmlWithAttributesAndHeaderSample readStream.
	parser startDocument.
	parser parseDocument.
	root := parser stack first.
	
	self assert: parser stack size = 1.
	self assert: (root isMemberOf: OPRootElement).
	self assert: root children size = 1.
	test1RootElement := root children first.
	self assert: test1RootElement attributes size = 1.
	self assert: (test1RootElement attributeNamed: 'name') = 'test1'.
	self assert: (test1RootElement isMemberOf: OPTest1Element).
	self assert: test1RootElement children size = 2.
	self assert: (test1RootElement children first isMemberOf: OPTest1Element).
	self assert: (test1RootElement children last isMemberOf: OPTest2Element)! !

!OPOpaxTest methodsFor: 'testing' stamp: 'FabrizioPerin 10/22/2009 19:39'!
xmlWithAttributesAndHeaderSample
	^'
<?xml version="1.0" encoding="UTF-8"?>
<Test1 name="test1">
	<Test1 name="test1-test1">
	</Test1>
	<Test2 name="test1-test2">
		<Test1 name="test1-test2-test1">
		</Test1>
	</Test2>
</Test1>
'! !

!OPOpaxTest methodsFor: 'testing' stamp: 'FP 7/22/2009 15:38'!
xmlWithAttributesSample
	^'
<Test1 name="test1">
	<Test1 name="test1-test1">
	</Test1>
	<Test2 name="test1-test2">
		<Test1 name="test1-test2-test1">
		</Test1>
	</Test2>
</Test1>
'! !

!OPTest1Element class methodsFor: 'instance creation' stamp: 'FP 7/21/2009 16:53'!
xmlTags
	^OrderedCollection with: 'test1' with:'anotherTest1' with: 'Test1'! !

!OPTest2Element class methodsFor: 'instance creation' stamp: 'FP 7/21/2009 16:53'!
xmlTags
	^OrderedCollection with: 'test2' with:'anotherTest2' with: 'Test2'! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 2/25/2010 14:39'!
assertDocument: aDocument dtd: aDtd version: aVersion encoding: anEncoding required: aRequired
	self
		assert: aDocument dtd = aDtd;
		assert: aDocument version = aVersion;
		assert: aDocument encoding = anEncoding;
		assert: aDocument requiredMarkup = aRequired! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 3/8/2010 04:54'!
assertElement: anElement attribute: aString is: aValue
	self
		assert: (anElement includesAttribute: aString);
		assert: (anElement attributeAt: aString) = aValue;
		assert: (anElement at: aString) = aValue;
		assert: (anElement @ aString) = aValue! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 3/7/2010 21:35'!
assertElement: anElement attributes: aDictionary
	self assert: anElement attributes = aDictionary! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 2/26/2010 22:27'!
assertElement: anElement contentString: aContentString contents: aContentArray
	| contentStrings |

	self
		assert: anElement contentString = aContentString;
		assert: anElement characterData = aContentString.
	
	contentStrings := (anElement contents collect: [:each | each string]).
	self assert: contentStrings asArray = aContentArray.! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 3/1/2010 16:50'!
assertElement: anElement name: aName attributes: aDictionary contentString: aString
	self
		assert: (anElement isNamed: aName);
		assertElement: anElement attributes: aDictionary;
		assert: anElement contentString = aString! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 3/8/2010 07:11'!
assertElement: anElement name: aName localName: aLocalName
	self
		assert: anElement name = aName;
		assert: anElement tag = aName;
		assert: anElement qualifiedName = aName;
		assert: anElement localName = aLocalName;
		assert: (anElement isNamed: aName);
		assert: (anElement isNamed: aLocalName)! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 3/23/2010 16:32'!
assertNode: aNode prefix: aPrefix uri: aUri
	self
		assert: aNode prefix = aPrefix;
		assert: aNode namespaceURI = aUri! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 3/8/2010 07:20'!
assertPI: aNode target: aTarget data: aData
	self
		assert: aNode target = aTarget;
		assert: aNode data = aData;
		assert: aNode printString = ('<?', aTarget, ' ', aData, ' ?>')! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 3/8/2010 07:20'!
assertString: aNode is: aString printed: aPrintString
	self
		assert: aNode string = aString;
		assert: aNode characterData = aString;
		assert: aNode printString = aPrintString! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 07:21'!
createNodes: aNumber with: aBlock
	^ ((1 to: aNumber)
		collect: [:i | aBlock valueWithPossibleArgument: i])
			asOrderedCollection! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 3/1/2010 01:11'!
escapedString
	^ 'test&lt;&gt;&amp;&quot;'! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 11:02'!
intersperse: aCollection with: aPaddingCollection
	| mixedCollection nodeStream paddingStream |

	mixedCollection := XMLOrderedList new.
	nodeStream := aCollection readStream.
	paddingStream := aPaddingCollection readStream.
	[nodeStream atEnd and: [paddingStream atEnd]]
		whileFalse: [
			nodeStream atEnd
				ifFalse: [mixedCollection add: nodeStream next].
			paddingStream atEnd
				ifFalse: [mixedCollection add: paddingStream next]].
	^ mixedCollection.! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 2/28/2010 23:52'!
interspersedWithNodes: aNodeCollection
	^ self
		intersperse: aNodeCollection
		with: (self newNodes: aNodeCollection size)! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 3/1/2010 01:10'!
markupString
	^ 'test<>&"'! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 2/28/2010 23:40'!
newElements: aNumber
	^ self
		createNodes: aNumber
		with: [:i | XMLElement named: 'test', i asString]! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 3/27/2010 22:28'!
newElementsNamed: aNameArray
	| names qualifiedName element |

	names := aNameArray asOrderedCollection.
	^ XMLOrderedList newFrom:
		(self
			createNodes: aNameArray size
			with: [
				((qualifiedName := names removeFirst) includes: $:)
					ifTrue: [
						qualifiedName splitQualifiedNameInto: [:prefix :localPart |
							element := XMLElement
								named: qualifiedName
								namespaceURI: 'http://', prefix]]
					ifFalse: [element := XMLElement named: qualifiedName].
				element]).! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:39'!
newNodes: aNumber
	^ self createNodes: aNumber with: [XMLNode new]! !

!XMLNodeTest methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 01:51'!
newStrings: aNumber
	^ self
		createNodes: aNumber
		with: [:i | XMLString string: 'test', i asString]! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 2/25/2010 14:39'!
should: aBlock addNode: aNode to: aParent
	self should: aBlock addNodes: (Array with: aNode) to: aParent! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 2/25/2010 14:39'!
should: aBlock addNodes: aNodeCollection to: aParent
	| added |

	self deny: (aNodeCollection anySatisfy: [:each | aParent includesNode: each]).
	(added := aBlock value) isCollection
		ifFalse: [added := Array with: added].
	self
		assert: added asArray = aNodeCollection asArray;
		assert: (added allSatisfy: [:each |
			(aParent includesNode: each) and: [each parent = aParent]])! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 2/25/2010 14:39'!
should: aBlock enumerate: aCollection
	| enumerated |

	enumerated := OrderedCollection new.
	aBlock value: [:each | enumerated add: each].
	self assert: enumerated = aCollection asOrderedCollection.! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 2/25/2010 14:39'!
should: aBlock removeNode: aNode from: aParent
	self should: aBlock removeNodes: (Array with: aNode) from: aParent! !

!XMLNodeTest methodsFor: 'assertions' stamp: 'JAAyer 2/25/2010 14:39'!
should: aBlock removeNodes: aNodeCollection from: aParent
	| removed |

	self assert: (aNodeCollection allSatisfy: [:each | aParent includesNode: each]).
	(removed := aBlock value) isCollection
		ifFalse: [removed := Array with: removed].

	self
		assert: removed asArray = aNodeCollection asArray;
		deny: (removed anySatisfy: [:each | aParent includesNode: each])! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/12/2010 10:55'!
test010node
	| node parent testingMessages accessors |

	node := XMLNode new.
	testingMessages := (XMLNode methodsInCategory: 'testing') copyWithout: 'isEmpty'.
	accessors := #(nextNode previousNode parent).
	self
		assert: node isEmpty;
		assert: (testingMessages noneSatisfy: [:each | node perform: each]);
		assert: (accessors allSatisfy: [:each | (node perform: each) isNil]).

	parent := XMLNode new.
	node parent: parent.
	self
		assert: node hasParent;
		assert: node parent = parent.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 2/27/2010 21:23'!
test020processingInstruction
	| pi t d |

	pi := XMLPI new.
	t := 'target'.
	d := 'data'.
	self
		assert: pi isPI & pi isProcessingInstruction;
		assertPI: pi target: '' data: '';
		assertPI: (pi target: t) target: t data: '';
		assertPI: (pi data: d) target: t data: d;
		assertPI: (XMLPI target: t data: d) target: t data: d.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/23/2010 16:23'!
test030stringNode
	| node raw escaped |

	node := XMLString new.
	raw := self markupString.
	escaped := self escapedString.
	self
		assert: node isText & node isTagOrText;
		assertString: node is: '' printed: '';
		assertString: (node string: raw) is: raw printed: escaped;
		assertString: (node addContent: raw) is: raw, raw printed: escaped, escaped.
	self
		assertString: (node addContent: (XMLString string: raw))
		is: raw, raw, raw
		printed: escaped, escaped, escaped.

	self assertString: (XMLString string: raw) is: raw printed: escaped.
	self
		assertString: (XMLString string: (XMLString string: raw))
		is: raw
		printed: escaped.

	self assertString: raw asXMLNode is: raw printed: escaped.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'hjh 1/28/2013 08:06'!
test040emptyNodeWithElements
	| node |

	node := XMLNodeWithElements new.
	self
		assert: node isEmpty;
		deny: node hasElements;
		assert: node nodes isEmpty;
		assert: node elements isEmpty.
		"
		assert: node topNode isNil;
		assert: node topElement isNil.
		"
		[ node topNode isNil ]
			on: Error
			do: [ :error |
				self assert: error messageText = 'this collection is empty' ].
		[ node topElement isNil ]
			on: Error
			do: [ :error |
				self assert: error messageText = 'this collection is empty' ].! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/23/2010 16:31'!
test041namespaces
	| node ns uri |

	node := XMLElement new.
	ns := 'foo'.
	uri := 'http://foo'.
	self
		assertNode: node prefix: nil uri: nil;
		assertNode: (node prefix: ns uri: uri) prefix: ns uri: uri.
	self should: [node prefix: 'badprefix'] raise: XMLNamespaceException.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/22/2010 16:22'!
test042addingNodes
	| node children |

	node := XMLNodeWithElements new.
	children := self newNodes: 6.
	self
		should: [node addNode: children second] addNode: children second to: node;
		should: [node addNodeFirst: children first] addNode: children first to: node.
	self
		should: [node addNodes: (children copyFrom: 3 to: 4)]
		addNodes: (children copyFrom: 3 to: 4)
		to: node.
	self
		should: [node addNode: children sixth after: children fourth]
		addNode: children sixth
		to: node.
	self
		should: [node addNode: children fifth before: children sixth]
		addNode: children fifth
		to: node.

	self
		should: [node addNode: (XMLNode new parent: XMLNode new)] raise: Error;
		should: [node addNode: children first] raise: Error;
		assert: node nodes asOrderedCollection = children;
		assert: node topNode = children first.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/10/2010 08:32'!
test043removingNodes
	| node children |

	node := XMLNodeWithElements new.
	children := self newNodes: 5.
	node addNodes: children.
	self
		should: [node removeNode: children first]
		removeNode: children first
		from: node.
	self
		should: [node removeNodes: (children copyFrom: 2 to: 3)]
		removeNodes: (children copyFrom: 2 to: 3)
		from: node.
	self
		assert: node removeNodes = node;
		assert: node isEmpty.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'pb 6/10/2012 18:55'!
test044accessingBeforeAfterNodes
	| node children |

	node := XMLNodeWithElements new.
	children := self newNodes: 3.
	node addNodes: children.
	children do: [:each | | next previous |
		next := children after: each ifNone: [nil].
		previous := children before: each ifNone: [nil].
		
		self
			assert: each nextNode = next;
			assert: each previousNode = previous;
			assert: (node nodeAfter: each) = next;
			assert: (node nodeBefore: each) = previous].! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 2/27/2010 15:17'!
test050emptyElement
	| element |

	element := XMLElement new.
	self
		assert: element isTag & element isTagOrText & element isElement;
		deny: element hasAttributes;
		deny: element hasContents;
		assert: element elementsAndContents isEmpty;
		assert: element contents isEmpty.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/23/2010 16:31'!
test051elementNaming
	| element |

	element := XMLElement new.
	self
		assertElement: element name: '' localName: '';
		assertElement: (element name: 'foo') name: 'foo' localName: 'foo';
		assertNode: element prefix: nil uri: nil.

	element prefix: 'bar' uri: 'http://bar'.
	self
		assertElement: element name: 'bar:foo' localName: 'foo';
		assertNode: element prefix: 'bar' uri: 'http://bar'.

	element name: 'foo:baz' namespaceURI: 'http://foo'.
	self
		assertElement: element name: 'foo:baz' localName: 'baz';
		assertNode: element prefix: 'foo' uri: 'http://foo'.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/12/2010 01:51'!
test052elementContentString
	| element str |

	str := self markupString.
	(element := XMLElement new) contentString: str.
	self
		deny: element isEmpty;
		assert: element hasContents;
		assert: element contentString = str.

	"Intersperse between text"
	element addNode: XMLNode new.
	self
		assertElement: (element addContent: str)
		contentString: str
		contents: (Array with: str with: str).

	"addContent: should combine neighboring string nodes"
	self
		assertElement: (element addContent: (XMLString string: str))
		contentString: str
		contents: (Array with: str with: str, str).

	element addElement: ((XMLElement named: 'foo') addContent: 'test').
	self assert: (element contentStringAt: 'foo') = 'test'.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/8/2010 07:32'!
test053elementAttributes
	| element attrs |

	element := XMLElement new.
	self
		assert: (element at: 'foo') isEmpty;
		assert: (element attributeAt: 'foo') isNil;
		assert: (element at: 'foo' ifAbsent: ['absent']) = 'absent';
		assert: (element attributeAt: 'foo' ifAbsent: ['absent']) = 'absent';
		deny: (element includesAttribute: 'foo').

	self
		assert: (element attributeAt: 'foo' put: 'test') = 'test';
		assert: element hasAttributes;
		assertElement: element attribute: 'foo' is: 'test';
		assert: (element attributeAt: 'bar' put: 'test2') = 'test2';
		assertElement: element attribute: 'bar' is: 'test2';
		assert: (element at: 'baz' put: 'test3') = 'test3';
		assertElement: element attribute: 'baz' is: 'test3';
		assert: element attributeNames asArray =  #('foo' 'bar' 'baz').

	self
		assert: element attributeAssociations asArray =
			(Array with: 'foo'->'test' with: 'bar'->'test2' with: 'baz'->'test3').
	self
		assertElement: element
		attributes:
			(XMLOrderPreservingDictionary
				with: 'foo'->'test'
				with: 'bar'->'test2'
				with: 'baz'->'test3').

	self
		assert: (element removeAttribute: 'foo') = 'test';
		assert: (element removeAttribute: 'bar') = 'test2';
		assert: (element removeAttribute: 'baz') = 'test3';
		deny: element hasAttributes.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'NorbertHartl 8/5/2010 16:37'!
test054elementConstructors
	| noAttributes attributes str element |

	noAttributes := XMLOrderPreservingDictionary new.
	self
		assertElement: (XMLElement named: 'foo')
		name: 'foo'
		attributes: noAttributes
		contentString: ''.

	attributes := XMLOrderPreservingDictionary with: 'foo'->'test1' with: 'bar'->'test2'.
	self
		assertElement: (XMLElement named: 'foo' attributes: attributes)
		name: 'foo'
		attributes: attributes
		contentString: ''.

	str := self markupString.
	self
		assertElement: (XMLElement named: 'foo')
		name: 'foo'
		attributes: noAttributes
		contentString: ''.
	self
		assertElement: (XMLElement named: 'foo' attributes: attributes)
		name: 'foo'
		attributes: attributes
		contentString: ''.

	element := XMLElement
		named: 'prefix:foo'
		namespaceURI: 'http://foo'
		attributes: attributes.
	self
		assertElement: element name: 'prefix:foo' attributes: attributes contentString: '';
		assertNode: element prefix: 'prefix' uri: 'http://foo'.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 8/9/2010 17:29'!
test055elementCDATASection
	| element str |

	str := self markupString.
	(element := XMLElement new) addNode: (XMLCData string: str).
	self
		deny: element isEmpty;
		assert: element hasContents;
		assert: element contentString = str;
		assert: (element firstNode isKindOf: XMLCData) ! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/8/2010 07:22'!
test060addingElements
	| node elements children |

	node := XMLNodeWithElements new.
	elements := self newElementsNamed: #('foo' 'bar' 'baz').
	node addNodes: (self interspersedWithNodes: elements).
	self
		assert: node hasElements;
		assert: node topElement = elements first;
		assert: node elements = elements;
		assert: (elements allSatisfy: [:each | node includesElement: each name]).! !

!XMLNodeTest methodsFor: 'tests' stamp: 'pb 6/10/2012 18:56'!
test061accessingBeforeAfterElements
	| node elements |

	node := XMLNodeWithElements new.
	elements := self newElements: 3.
	node addNodes: (self interspersedWithNodes: elements).
	elements do: [:each | | next previous |
		next := elements after: each ifNone: [nil].
		previous := elements before: each ifNone: [nil].
		
		self
			assert: each nextElement = next;
			assert: each previousElement = previous;
			assert: (node elementAfter: each) = next;
			assert: (node elementBefore: each) = previous].! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/12/2010 01:44'!
test062elementPrinting
	| element attributes |

	element := (XMLElement named: 'foo').
	attributes := XMLOrderPreservingDictionary with: 'b'->'two' with: 'a'->'one'.
	self
		assert: element printString = '<foo></foo>';
		assert: (element contentString: 'test') printString = '<foo>test</foo>';
		assert: (element setAttributes: attributes) printString = '<foo b="two" a="one">test</foo>'! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 2/26/2010 21:01'!
test070nodeAndElementEnumeration
	| node nodes elements |

	node := XMLNodeWithElements new.
	self
		should: [:block | node nodesDo: block] enumerate: #();
		should: [:block | node elementsDo: block] enumerate: #().

	elements := self newElements: 3.
	nodes := self interspersedWithNodes: elements.
	node addNodes: nodes.
	self
		should: [:block | node nodesDo: block] enumerate: nodes;
		should: [:block | node elementsDo: block] enumerate: elements.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 2/28/2010 23:54'!
test071elementsAndContentsEnumeration
	| node nodes elements contents elementsAndContents |

	node := XMLElement new.
	elements := self newElements: 3.
	contents := self newStrings: 3.
	elementsAndContents := self intersperse: elements with: contents.
	node addNodes: (self interspersedWithNodes: elementsAndContents).

	self
		assert: node elementsAndContents = elementsAndContents;
		should: [:block | node elementsAndContentsDo: block] enumerate: elementsAndContents;
		should: [:block | node contentsDo: block] enumerate: contents.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/27/2010 22:27'!
test080elementAccessing
	| node nodes elements |

	node := XMLNodeWithElements new.
	elements := self newElementsNamed: #('foo' 'bar' 'prefix:foo' 'baz').
	node addNodes: (self interspersedWithNodes: elements).
	self
		assert: (node elementAt: 'absent') isNil;
		assert: (node elementAt: 'absent' ifAbsent: ['absent']) = 'absent';
		assert: (node elementsAt: 'absent') isEmpty;
		assert: (node elementsAt: 'absent' ifAbsent: ['absent']) = 'absent';
		assert: (node elementAtAny: #('absent')) isNil;
		assert: (node elementAtAny: #('absent') ifAbsent: ['absent']) = 'absent';
		assert: (node elementAt: 'foo') = elements first;
		assert: (node elementAt: 'bar') = elements second;
		assert: (node elementsAt: 'foo') = ((elements first: 3) copyWithout: elements second);
		assert: (node elementAt: 'prefix:foo') = elements third;
		assert: (node elementsAt: 'prefix:foo') = (XMLOrderedList with: elements third);
		assert: (node elementAtAny: #('bar' 'prefix:foo')) = elements second;
		assert: (node elementAtAny: #('absent' 'baz')) = elements last.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/8/2010 07:30'!
test081firstTagSearching
	| node elements |

	node := XMLNodeWithElements new.
	elements := self newElementsNamed: #('foo' 'bar' 'prefix:foo' 'prefix:bar' 'bar' 'baz').
	node addNodes: (elements first: 2).
	node nodes first addNodes: (elements copyFrom: 3 to: 4).
	node nodes second addNodes: (elements copyFrom: 5 to: 6).
	self
		assert: (node firstTagNamed: 'absent') isNil;
		assert: (node firstTagNamed: 'foo' with: [false]) isNil;
		assert: (node firstTagNamedAny: #('absent')) isNil;
		assert: (node firstTagNamedAny: #('foo') with: [false]) isNil.

	self
		assert: (node firstTagNamed: 'foo') = elements first;
		assert: (node firstTagNamed: 'prefix:bar') = elements fourth;
		assert: (node firstTagNamedAny: #('prefix:foo' 'prefix:bar' 'absent')) = elements third;
		assert: (node firstTagNamedAny: #('absent:bar' 'baz')) = elements last;
		assert: (elements first firstTagNamed: 'foo') = elements first;
		assert: (elements second firstTagNamedAny: #('foo' 'bar')) = elements second.
	self assert: (node
		firstTagNamed: 'bar'
		with: [:each | each = elements fifth]) = elements fifth.
	self assert: (node
		firstTagNamedAny: #('bar' 'baz')
		with: [:each | each = elements last]) = elements last.
	self assert: (elements first
		firstTagNamed: 'foo'
		with: [:each | each = elements third]) = elements third.
	self assert: (elements second
		firstTagNamedAny: #('bar')
		with: [:each | each = elements fifth]) = elements fifth.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/8/2010 07:28'!
test082tagsNamedSearching
	| node elements |

	node := XMLNodeWithElements new.
	elements := self newElementsNamed: #('foo' 'bar' 'prefix:foo' 'bar' 'prefix:bar' 'baz').
	node addNodes: (elements first: 2).
	node nodes first addNodes: (elements copyFrom: 3 to: 4).
	node nodes second addNodes: (elements copyFrom: 5 to: 6).
	self
		should: [:block | node tagsNamed: 'absent' do: block] enumerate: #();
		should: [:block | node elementsAt: 'absent' do: block] enumerate: #();
		should: [:block | node tagsNamed: 'absent' childrenDoAndRecurse: block] enumerate: #();
		should: [:block | node tagsNamed: 'absent' contentsDo: block] enumerate: #();
		should: [:block | node tagsNamed: 'foo' contentsDo: block] enumerate: #().

	self
		should: [:block | node tagsNamed: 'foo' do: block]
		enumerate: (Array with: elements first with: elements third).
	self
		should: [:block | node tagsNamed: 'prefix:foo' do: block]
		enumerate: (Array with: elements third).
	self
		should: [:block | node topElement elementsAt: 'foo' do: block]
		enumerate: (Array with: elements third).
	self
		should: [:block | node tagsNamed: 'bar' childrenDoAndRecurse: block]
		enumerate: (Array with: elements fourth with: elements second with: elements fifth).

	elements first
		addContent: 'testing 1';
		addNode: XMLNode new; "Intersperse between string nodes"
		addContent: 'testing 2'.
	elements third addContent: 'testing'.
	self
		should: [:block | node tagsNamed: 'foo' contentsDo: block]
		enumerate: elements first contents, elements third contents.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'hjh 1/28/2013 08:06'!
test083elementRemoving
	| node elements |

	node := XMLNodeWithElements new.
	elements := self newElementsNamed: #('foo' 'prefix:bar' 'bar' 'baz').
	node addNodes: (self interspersedWithNodes: elements).
	self
		assert: (node removeNode: elements first) = elements first;
		assert: node elements = elements allButFirst;
		assert: (node elementAt: 'foo') isNil;
		assert: (node elementsAt: 'foo') isEmpty;
		assert: (node removeNode: elements second) = elements second;
		assert: node elements = (elements last: 2);
		assert: (node elementAt: 'bar') = elements third;
		assert: (node elementsAt: 'bar') = (XMLOrderedList with: elements third);
		assert: (node removeNode: elements third) = elements third;
		assert: node elements = (elements last: 1);
		assert: (node elementAt: 'bar') isNil;
		assert: (node elementsAt: 'bar') isEmpty;
		assert: node topElement = elements last;
		assert: (node removeNode: elements last) = elements last;
		deny: node hasElements.
		"
		assert: node topElement isNil.
		"
		[ node topElement isNil ]
			on: Error
			do: [ :error |
				self assert: error messageText = 'this collection is empty' ].! !

!XMLNodeTest methodsFor: 'tests' stamp: 'JAAyer 3/8/2010 07:34'!
test084elementRenaming
	| node nodes elements |

	node := XMLNodeWithElements new.
	elements := self newElementsNamed: #('foo' 'prefix:bar' 'bar' 'baz').
	node addNodes: (self interspersedWithNodes: elements).

	elements first name: 'notfoo'.
	self
		deny: (node includesElement: 'foo');
		assert: (node elementAt: 'foo') isNil;
		assert: (node elementsAt: 'foo') isEmpty;
		assert: (node elementAt: 'notfoo') = elements first.

	elements third prefix: 'prefix' uri: ''.
	self assert: (node elementsAt: 'prefix:bar') = (elements copyFrom: 2 to: 3).

	elements fourth name: 'bar'.
	self assert: (node elementsAt: 'bar') = elements allButFirst.

	elements fourth name: 'foo'.
	self assert: (node elementAt: 'foo') = elements last.! !

!XMLNodeTest methodsFor: 'tests' stamp: 'pb 6/10/2012 23:33'!
test090documentTest
	| doc root |
	doc := XMLDocument new.
	root := XMLElement new.
	self
		 assert: doc isDocument;
		 assert: (doc addElement: root) root = root;
		
		assertDocument: doc
		dtd: nil
		version: nil
		encoding: 'UTF-8'
		required: nil.
	"
		assert: doc root isNil;
		"
	[ doc root isNil ]
		on: Error
		do: [ :error |
			self assert: error = 'this collection is empty' ].
	self
		assertDocument: (doc dtd: '<!!DOCTYPE root>')
		dtd: '<!!DOCTYPE root>'
		version: nil
		encoding: 'UTF-8'
		required: nil.
	self
		assertDocument: (doc version: '1.0')
		dtd: '<!!DOCTYPE root>'
		version: '1.0'
		encoding: 'UTF-8'
		required: nil.
	self
		assertDocument: (doc encoding: 'ISO Latin-1')
		dtd: '<!!DOCTYPE root>'
		version: '1.0'
		encoding: 'ISO Latin-1'
		required: nil.
	self
		assertDocument: (doc requiredMarkup: 'root')
		dtd: '<!!DOCTYPE root>'
		version: '1.0'
		encoding: 'ISO Latin-1'
		required: 'root'.! !

!XMLParserTest methodsFor: 'source' stamp: 'AlexandreBergel 1/29/2009 11:49'!
addressBookXML
	^'<addressbook>
  <person employee-number="A0000" family-name="Gates" first-name="Bob">
    <contact-info><!!--Confidential--></contact-info>
    <address city="Los Angeles" number="1239" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Manager"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7000" family-name="Brown"
    first-name="Robert" middle-initial="L.">
    <contact-info>
      <email address="robb@iro.ibm.com"/>
      <home-phone number="03-3987873"/>
    </contact-info>
    <address city="New York" number="344" state="NY" street="118 St."/>
    <job-info employee-type="Full-Time" is-manager="yes" job-description="Group Leader"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7890" family-name="DePaiva"
    first-name="Kassie" middle-initial="W.">
    <contact-info><!!-- Kassie''s agent phone: 03-987654 --></contact-info>
    <address city="Los Angeles" number="1234" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Actor"/>
    <manager employee-number="A0000"/>
    <misc-info>One of the most talented actresses on Daytime. Kassie
      plays the devious and beautiful Blair Cramer on ABC&apos;s
      &quot;One Life To Live.&quot;</misc-info>
  </person>
  <person employee-number="A7987" family-name="Smith" first-name="Joe">
    <contact-info>
      <email address="joes@iro.ibm.com"/>
      <mobile-phone number="888-7657765"/>
      <home-phone number="03-8767898"/>
      <home-phone number="03-8767871"/>
    </contact-info>
    <address city="New York" number="12789" state="NY" street="W. 15th Ave."/>
    <job-info employee-type="Part-Time" is-manager="no" job-description="Hacker"/>
    <manager employee-number="A7000"/>
  </person>
</addressbook>
'! !

!XMLParserTest methodsFor: 'source' stamp: 'AlexandreBergel 1/29/2009 11:49'!
addressBookXMLWithDTD
	^'<?xml version="1.0" encoding="UTF-8"?>
<!!DOCTYPE addressbook SYSTEM "addressbook.dtd">
<?xml-stylesheet type="text/xsl" href="demo.xsl"?>
<addressbook>
  <person employee-number="A0000" family-name="Gates" first-name="Bob">
    <contact-info><!!--Confidential--></contact-info>
    <address city="Los Angeles" number="1239" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Manager"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7000" family-name="Brown"
    first-name="Robert" middle-initial="L.">
    <contact-info>
      <email address="robb@iro.ibm.com"/>
      <home-phone number="03-3987873"/>
    </contact-info>
    <address city="New York" number="344" state="NY" street="118 St."/>
    <job-info employee-type="Full-Time" is-manager="yes" job-description="Group Leader"/>
    <manager employee-number="A0000"/>
  </person>
  <person employee-number="A7890" family-name="DePaiva"
    first-name="Kassie" middle-initial="W.">
    <contact-info><!!-- Kassie''s agent phone: 03-987654 --></contact-info>
    <address city="Los Angeles" number="1234" state="CA" street="Pine Rd."/>
    <job-info employee-type="Full-Time" is-manager="no" job-description="Actor"/>
    <manager employee-number="A0000"/>
    <misc-info>One of the most talented actresses on Daytime. Kassie
      plays the devious and beautiful Blair Cramer on ABC&apos;s
      &quot;One Life To Live.&quot;</misc-info>
  </person>
  <person employee-number="A7987" family-name="Smith" first-name="Joe">
    <contact-info>
      <email address="joes@iro.ibm.com"/>
      <mobile-phone number="888-7657765"/>
      <home-phone number="03-8767898"/>
      <home-phone number="03-8767871"/>
    </contact-info>
    <address city="New York" number="12789" state="NY" street="W. 15th Ave."/>
    <job-info employee-type="Part-Time" is-manager="no" job-description="Hacker"/>
    <manager employee-number="A7000"/>
  </person>
</addressbook>
'! !

!XMLParserTest methodsFor: 'benchmark' stamp: 'Alexandre Bergel 3/29/2010 09:10'!
benchmark1
	"
	XMLParserTest new benchmark1 
	=> 2131 (Alexandre Bergel 3/29/2010 09:09)
	"
	Smalltalk garbageCollect.
	^ [600 timesRepeat: [SAXHandler parse: self addressBookXML]] timeToRun! !

!XMLParserTest methodsFor: 'benchmark' stamp: 'JAAyer 3/22/2010 15:12'!
benchmark2
	"
	XMLParserTest new benchmark2
	"
	Smalltalk garbageCollect.
	^ [600 timesRepeat: [XMLDOMParser parse: self addressBookXML]] timeToRun! !

!XMLParserTest methodsFor: 'benchmark' stamp: 'JAAyer 3/22/2010 15:21'!
benchmark3
	"
	XMLParserTest new benchmark3
	"
	Smalltalk garbageCollect.
	^ [300 timesRepeat: [self testParsing]] timeToRun! !

!XMLParserTest methodsFor: 'benchmark' stamp: 'JAAyer 3/22/2010 15:48'!
benchmark4
	| doc |
	"
	XMLParserTest new benchmark4
	"
	doc := XMLDOMParser
		parseDocumentFrom: self addressBookXML readStream
		useNamespaces: false.
	^ [100000 timesRepeat: [
		doc root topElement
			elementAt: 'contact-info';
			elementAt: 'address';
			elementAt: 'job-info';
			elementAt: 'manager']] timeToRun.! !

!XMLParserTest methodsFor: 'source' stamp: 'jvds 8/9/2010 11:03'!
exampleCDATASectionXML
	^'<?xml version="1.0" encoding="UTF-8"?>
<test-data><!![CDATA[this is CDATA test content 1 < 2 & 5 > 3]]></test-data>'! !

!XMLParserTest methodsFor: 'source' stamp: 'DamienPollet 5/16/2010 19:25'!
exampleEncodedXML
	^'<?xml version="1.0" encoding="UTF-8"?>
<test-data>&#8230;</test-data>
'! !

!XMLParserTest methodsFor: 'tests-utf8' stamp: 'pb 6/10/2012 23:38'!
testCDATASection
	| xmlDocument element output |

	xmlDocument :=
		(XMLDOMParser on: (self exampleCDATASectionXML readStream))
			preservesCDataSections: true;
			parseDocument.
	element := xmlDocument firstTagNamed: #'test-data'.

	self assert: element contentString = 'this is CDATA test content 1 < 2 & 5 > 3'.
	
	output := element printString.
	self assert: '<test-data><!![CDATA[this is CDATA test content 1 < 2 & 5 > 3]]></test-data>' = output
	
! !

!XMLParserTest methodsFor: 'tests-utf8' stamp: 'pb 6/10/2012 23:37'!
testDecodingCharacters
	| xmlDocument element |

	xmlDocument := XMLDOMParser parseDocumentFrom: (self exampleEncodedXML readStream).
	element := xmlDocument firstTagNamed: #'test-data'.

	self assert: ((element contentString first) asInteger bitAnd: 16r3FFFFF) = 8230.	

! !

!XMLParserTest methodsFor: 'tests' stamp: 'JAAyer 3/11/2010 09:06'!
testExampleAddressBook
	| tokenizer |
	"self debug: #testExampleAddressBook"

	tokenizer := XMLTokenizer on: self addressBookXML readStream.

	"We enumerate the first characters of the addressbook example.
	The file being parsed begins with <addressbook"
	self assert: tokenizer streamReader next = $<.
	self assert: tokenizer streamReader next = $a.	
	self assert: tokenizer streamReader next = $d.
	self assert: tokenizer streamReader next = $d.
	self assert: tokenizer streamReader next = $r.

	self
		shouldnt: [tokenizer streamReader next notNil] whileTrue
		raise: Error. ! !

!XMLParserTest methodsFor: 'tests' stamp: 'JAAyer 3/11/2010 09:04'!
testExampleAddressBookWithDTD
	| tokenizer |
	"XMLTokenizer exampleAddressBookWithDTD"

	tokenizer := XMLTokenizer on: self addressBookXMLWithDTD readStream.
	self
		shouldnt: [tokenizer streamReader next notNil] whileTrue
		raise: Error! !

!XMLParserTest methodsFor: 'tests' stamp: 'pb 6/10/2012 18:59'!
testLineEndingsDoNotMatter
   |text cr crlf|
   text :=
 '<foo>
 bar
 baz
 </foo>'.
	cr := XMLDOMParser parseDocumentFrom: text readStream.
	crlf := XMLDOMParser parseDocumentFrom:
		(text copyReplaceAll: Character crCharacter asString with: Character lfCharacter asString) readStream.
	self assert: (cr root contentString) = (crlf root contentString).
! !

!XMLParserTest methodsFor: 'tests' stamp: 'JAAyer 3/11/2010 03:40'!
testParsing
	| xmlDocument root firstPerson numberOfPersons |
	"self debug: #testParsing"

	xmlDocument := XMLDOMParser parseDocumentFrom: self addressBookXML readStream.
	self assert: (xmlDocument isKindOf: XMLDocument).
	root := xmlDocument root.
	self assert: (root class == XMLElement).

	self assert: (root firstTagNamed: 'person') notNil.
	self assert: (root firstTagNamed: 'addressbook') notNil.

	self assert: (root firstTagNamed: 'addressbook') == root.

	numberOfPersons := 0.
	root tagsNamed: 'person' do: [:p | numberOfPersons := numberOfPersons + 1].
	self assert: numberOfPersons = 4.

	firstPerson := root firstTagNamed: 'person'.
	self assert: (firstPerson attributeAt: 'employee-number') = 'A0000'.
	self assert: (firstPerson attributeAt: 'family-name') = 'Gates'.
	self assert: (firstPerson attributeAt: 'first-name') = 'Bob'.
	self assert: firstPerson attributeNames asArray =
		#('employee-number' 'family-name' 'first-name').
	self assert: firstPerson attributeAssociations asArray =
		{('employee-number'->'A0000'). ('family-name'->'Gates'). ('first-name'->'Bob').}.

	self assert: (firstPerson @ 'employee-number') = (firstPerson attributeAt: 'employee-number').
	self assert: (firstPerson @ 'employee-number') = (firstPerson @ 'employee-number').! !

!XMLParserTest methodsFor: 'tests' stamp: 'JAAyer 3/14/2010 12:54'!
testParsingCharacters
	| parser |
	"This test is actually not that useful. This is not the proper way of using the parser. This test is here just for specification purpose"
	"self debug: #testParsingCharacters"

	parser := SAXDriver on: self addressBookXML readStream.

	self assert: parser streamReader next = $<.
	self assert: parser streamReader next = $a.
	self assert: parser streamReader next = $d.
	self assert: parser streamReader next = $d.
	self assert: parser streamReader next = $r.! !

!XMLParserTest methodsFor: 'tests-xmltokenizer' stamp: 'JAAyer 3/11/2010 09:06'!
testTokenizerAddressBook
	| tokenizer |
	tokenizer := XMLTokenizer on: self addressBookXML readStream.
	[tokenizer streamReader next notNil] whileTrue! !

!XMLParserTest methodsFor: 'tests-xmltokenizer' stamp: 'JAAyer 3/11/2010 09:06'!
testTokenizerAddressBookWithDTD
	| tokenizer |
	tokenizer := XMLTokenizer on: self addressBookXMLWithDTD readStream.
	[tokenizer streamReader next notNil] whileTrue! !
