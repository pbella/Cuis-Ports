'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 10 June 2012 at 11:09:12 pm'!
'Description Based on http://squeaksource.cdn.st/XMLSupport/XML-Parser-NorbertHartl.141.mcz

Tests have been split out into XMLParserTests'!
!classDefinition: #DTDEntityDeclaration category: #'XML-Parser-DTD'!
Object subclass: #DTDEntityDeclaration
	instanceVariableNames: 'name value ndata'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-DTD'!
!classDefinition: 'DTDEntityDeclaration class' category: #'XML-Parser-DTD'!
DTDEntityDeclaration class
	instanceVariableNames: 'contextBehavior'!

!classDefinition: #DTDExternalEntityDeclaration category: #'XML-Parser-DTD'!
DTDEntityDeclaration subclass: #DTDExternalEntityDeclaration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-DTD'!
!classDefinition: 'DTDExternalEntityDeclaration class' category: #'XML-Parser-DTD'!
DTDExternalEntityDeclaration class
	instanceVariableNames: ''!

!classDefinition: #DTDParameterEntityDeclaration category: #'XML-Parser-DTD'!
DTDEntityDeclaration subclass: #DTDParameterEntityDeclaration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-DTD'!
!classDefinition: 'DTDParameterEntityDeclaration class' category: #'XML-Parser-DTD'!
DTDParameterEntityDeclaration class
	instanceVariableNames: ''!

!classDefinition: #OPGenericElement category: #'XML-Parser-Opax'!
Object subclass: #OPGenericElement
	instanceVariableNames: 'tag attributes children characters'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Opax'!
!classDefinition: 'OPGenericElement class' category: #'XML-Parser-Opax'!
OPGenericElement class
	instanceVariableNames: ''!

!classDefinition: #OPRootElement category: #'XML-Parser-Opax'!
OPGenericElement subclass: #OPRootElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Opax'!
!classDefinition: 'OPRootElement class' category: #'XML-Parser-Opax'!
OPRootElement class
	instanceVariableNames: ''!

!classDefinition: #SAXException category: #'XML-Parser-Exception'!
Error subclass: #SAXException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Exception'!
!classDefinition: 'SAXException class' category: #'XML-Parser-Exception'!
SAXException class
	instanceVariableNames: ''!

!classDefinition: #SAXHandler category: #'XML-Parser-Parser'!
Object subclass: #SAXHandler
	instanceVariableNames: 'driver eod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'SAXHandler class' category: #'XML-Parser-Parser'!
SAXHandler class
	instanceVariableNames: ''!

!classDefinition: #OPOpaxHandler category: #'XML-Parser-Opax'!
SAXHandler subclass: #OPOpaxHandler
	instanceVariableNames: 'stack'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Opax'!
!classDefinition: 'OPOpaxHandler class' category: #'XML-Parser-Opax'!
OPOpaxHandler class
	instanceVariableNames: ''!

!classDefinition: #SAXNotWellFormedException category: #'XML-Parser-Exception'!
SAXException subclass: #SAXNotWellFormedException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Exception'!
!classDefinition: 'SAXNotWellFormedException class' category: #'XML-Parser-Exception'!
SAXNotWellFormedException class
	instanceVariableNames: ''!

!classDefinition: #SAXParseException category: #'XML-Parser-Exception'!
SAXException subclass: #SAXParseException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Exception'!
!classDefinition: 'SAXParseException class' category: #'XML-Parser-Exception'!
SAXParseException class
	instanceVariableNames: ''!

!classDefinition: #SAXWarning category: #'XML-Parser-Exception'!
Warning subclass: #SAXWarning
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Exception'!
!classDefinition: 'SAXWarning class' category: #'XML-Parser-Exception'!
SAXWarning class
	instanceVariableNames: ''!

!classDefinition: #XMLDOMParser category: #'XML-Parser-Parser'!
SAXHandler subclass: #XMLDOMParser
	instanceVariableNames: 'document incremental nodeStack preservesCDataSections isInCDataSection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'XMLDOMParser class' category: #'XML-Parser-Parser'!
XMLDOMParser class
	instanceVariableNames: ''!

!classDefinition: #XMLElementCache category: #'XML-Parser-Nodes'!
Object subclass: #XMLElementCache
	instanceVariableNames: 'elements elementsByName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLElementCache class' category: #'XML-Parser-Nodes'!
XMLElementCache class
	instanceVariableNames: ''!

!classDefinition: #XMLException category: #'XML-Parser-Exception'!
Error subclass: #XMLException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Exception'!
!classDefinition: 'XMLException class' category: #'XML-Parser-Exception'!
XMLException class
	instanceVariableNames: ''!

!classDefinition: #XMLDOMException category: #'XML-Parser-Exception'!
XMLException subclass: #XMLDOMException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Exception'!
!classDefinition: 'XMLDOMException class' category: #'XML-Parser-Exception'!
XMLDOMException class
	instanceVariableNames: ''!

!classDefinition: #XMLNamespaceException category: #'XML-Parser-Exception'!
XMLException subclass: #XMLNamespaceException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Exception'!
!classDefinition: 'XMLNamespaceException class' category: #'XML-Parser-Exception'!
XMLNamespaceException class
	instanceVariableNames: ''!

!classDefinition: #XMLNamespaceScope category: #'XML-Parser'!
Object subclass: #XMLNamespaceScope
	instanceVariableNames: 'defaultNamespace prefixMappings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLNamespaceScope class' category: #'XML-Parser'!
XMLNamespaceScope class
	instanceVariableNames: ''!

!classDefinition: #XMLNestedNamespaceScopes category: #'XML-Parser'!
Object subclass: #XMLNestedNamespaceScopes
	instanceVariableNames: 'scopes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLNestedNamespaceScopes class' category: #'XML-Parser'!
XMLNestedNamespaceScopes class
	instanceVariableNames: ''!

!classDefinition: #XMLNode category: #'XML-Parser-Nodes'!
Object subclass: #XMLNode
	instanceVariableNames: 'parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLNode class' category: #'XML-Parser-Nodes'!
XMLNode class
	instanceVariableNames: ''!

!classDefinition: #XMLAttribute category: #'XML-Parser-Nodes'!
XMLNode subclass: #XMLAttribute
	instanceVariableNames: 'name value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLAttribute class' category: #'XML-Parser-Nodes'!
XMLAttribute class
	instanceVariableNames: ''!

!classDefinition: #XMLNodeName category: #'XML-Parser-Nodes'!
Object subclass: #XMLNodeName
	instanceVariableNames: 'qualifiedName prefix localName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLNodeName class' category: #'XML-Parser-Nodes'!
XMLNodeName class
	instanceVariableNames: ''!

!classDefinition: #XMLNodeWithChildren category: #'XML-Parser-Nodes'!
XMLNode subclass: #XMLNodeWithChildren
	instanceVariableNames: 'nodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLNodeWithChildren class' category: #'XML-Parser-Nodes'!
XMLNodeWithChildren class
	instanceVariableNames: ''!

!classDefinition: #XMLNodeWithElements category: #'XML-Parser-Nodes'!
XMLNodeWithChildren subclass: #XMLNodeWithElements
	instanceVariableNames: 'elementCache'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLNodeWithElements class' category: #'XML-Parser-Nodes'!
XMLNodeWithElements class
	instanceVariableNames: ''!

!classDefinition: #XMLDocument category: #'XML-Parser-Nodes'!
XMLNodeWithElements subclass: #XMLDocument
	instanceVariableNames: 'dtd version encoding requiredMarkup'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLDocument class' category: #'XML-Parser-Nodes'!
XMLDocument class
	instanceVariableNames: ''!

!classDefinition: #XMLElement category: #'XML-Parser-Nodes'!
XMLNodeWithElements subclass: #XMLElement
	instanceVariableNames: 'name attributes scope usesNamespaces'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLElement class' category: #'XML-Parser-Nodes'!
XMLElement class
	instanceVariableNames: ''!

!classDefinition: #XMLOpenTags category: #'XML-Parser-Parser'!
Object subclass: #XMLOpenTags
	instanceVariableNames: 'tags'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'XMLOpenTags class' category: #'XML-Parser-Parser'!
XMLOpenTags class
	instanceVariableNames: ''!

!classDefinition: #XMLOrderPreservingDictionary category: #'XML-Parser'!
Dictionary subclass: #XMLOrderPreservingDictionary
	instanceVariableNames: 'orderedKeys'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLOrderPreservingDictionary class' category: #'XML-Parser'!
XMLOrderPreservingDictionary class
	instanceVariableNames: ''!

!classDefinition: #XMLOrderedList category: #'XML-Parser-Nodes'!
OrderedCollection subclass: #XMLOrderedList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLOrderedList class' category: #'XML-Parser-Nodes'!
XMLOrderedList class
	instanceVariableNames: ''!

!classDefinition: #XMLNodeList category: #'XML-Parser-Nodes'!
XMLOrderedList subclass: #XMLNodeList
	instanceVariableNames: 'parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLNodeList class' category: #'XML-Parser-Nodes'!
XMLNodeList class
	instanceVariableNames: ''!

!classDefinition: #XMLAttributeList category: #'XML-Parser-Nodes'!
XMLNodeList subclass: #XMLAttributeList
	instanceVariableNames: 'nodesByName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLAttributeList class' category: #'XML-Parser-Nodes'!
XMLAttributeList class
	instanceVariableNames: ''!

!classDefinition: #XMLPI category: #'XML-Parser-Nodes'!
XMLNode subclass: #XMLPI
	instanceVariableNames: 'target data'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLPI class' category: #'XML-Parser-Nodes'!
XMLPI class
	instanceVariableNames: ''!

!classDefinition: #XMLStreamReader category: #'XML-Parser-Parser'!
Object subclass: #XMLStreamReader
	instanceVariableNames: 'stream nestedStreams peekChar buffer'
	classVariableNames: 'CarriageReturn DigitTable LineFeed SeparatorTable'
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'XMLStreamReader class' category: #'XML-Parser-Parser'!
XMLStreamReader class
	instanceVariableNames: ''!

!classDefinition: #XMLStreamWriter category: #'XML-Parser-Parser'!
Object subclass: #XMLStreamWriter
	instanceVariableNames: 'streams nextStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'XMLStreamWriter class' category: #'XML-Parser-Parser'!
XMLStreamWriter class
	instanceVariableNames: ''!

!classDefinition: #XMLString category: #'XML-Parser-Nodes'!
XMLNode subclass: #XMLString
	instanceVariableNames: 'string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLString class' category: #'XML-Parser-Nodes'!
XMLString class
	instanceVariableNames: ''!

!classDefinition: #XMLCData category: #'XML-Parser-Nodes'!
XMLString subclass: #XMLCData
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Nodes'!
!classDefinition: 'XMLCData class' category: #'XML-Parser-Nodes'!
XMLCData class
	instanceVariableNames: ''!

!classDefinition: #XMLTokenizer category: #'XML-Parser-Parser'!
Object subclass: #XMLTokenizer
	instanceVariableNames: 'streamReader streamWriter entities externalEntities parameterEntities isValidating parsingMarkup'
	classVariableNames: 'CharEscapes LiteralChars NameDelimiters'
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'XMLTokenizer class' category: #'XML-Parser-Parser'!
XMLTokenizer class
	instanceVariableNames: ''!

!classDefinition: #SAXDriver category: #'XML-Parser-Parser'!
XMLTokenizer subclass: #SAXDriver
	instanceVariableNames: 'saxHandler openTags nestedScopes useNamespaces validateAttributes languageEnvironment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'SAXDriver class' category: #'XML-Parser-Parser'!
SAXDriver class
	instanceVariableNames: ''!

!classDefinition: #XMLWriter category: #'XML-Parser-Parser'!
Object subclass: #XMLWriter
	instanceVariableNames: 'stream openTags nestedScopes canonical currentIndent indentString newLinesAllowed'
	classVariableNames: 'XMLTranslation XMLTranslationMap'
	poolDictionaries: ''
	category: 'XML-Parser-Parser'!
!classDefinition: 'XMLWriter class' category: #'XML-Parser-Parser'!
XMLWriter class
	instanceVariableNames: ''!


!SAXDriver commentStamp: 'JAAyer 3/23/2010 16:10' prior: 0!
SAXDriver together with SAXHandler provides a SAX-like API for XML parsing. A SAXDriver processes XML tokens and signals SAX events to a SAXHandler. To use it, create a subclass of SAXHandler and override messages that handle SAX events of interest. See "instance creation" on the class-side of SAXHandler for ways to instantiate such a parser.!

!SAXHandler commentStamp: 'JAAyer 3/9/2010 12:11' prior: 0!
A SAXHandler is an XML parser that uses event handling. The acronym "SAX" refers to Java's "Simple API for XML," on which SAXHandler's API is based. To use this class, create a subclass and override handlers in "content" and "lexical" as needed. To instantiate it you can send #on: or any of the #parse- messages. The former creates a new parser on the given input stream and returns it; the instance can then be sent #parseDocument to parse the input. The #parse- messages take some input source and parse it, returning the result of #parseDocument.

By default, namespace support is enabled.!

!XMLAttribute commentStamp: 'JAAyer 3/23/2010 14:24' prior: 0!
The class represents an attribute node, to be stored in an XMLAttributeList. You should seldom need to interact with these objects directly; XMLElement's various #attribute- messages will usually suffice.

The name of an XMLAttribute can be namespaced using prefixes; however, the prefix must be mapped to a namespace in the parent XMLElement.!

!XMLDOMParser commentStamp: 'JAAyer 3/23/2010 14:08' prior: 0!
XMLDOMParser parses XML into a tree of XML nodes representing the document structure. To see what kinds of nodes it produces and what can be done with them, browse XML-Parser-Nodes. !

!XMLElement commentStamp: 'JAAyer 3/23/2010 15:43' prior: 0!
The class represents an XML element node. It can have a name, attributes, associated namespace information and any number child nodes/elements.

The #attribute- messages provide a Dictionary-like prototcol for manipulating attributes. The underlying XMLAttributeList can be accessed using #attributeNodes, and an order-preserving dictionary of attribute names/values can be obtained using #attributes.

Namespaces can be declared using #declareNamespace:uri: or simultaniously with changing the element's name or prefix using #name:namespaceURI: or #prefix:uri:. The default namespace can be accessed through #defaultNamespace and #defaultNamespace:.

Character data can be obtained using #contentString, which returns content of the first XMLString node, or through #contents, which returns all child XMLString nodes.!

!XMLNamespaceScope commentStamp: 'JAAyer 3/11/2010 00:01' prior: 0!
An XMLNamespaceScope is a set of mappings from namespace prefixes to URIs, and also a default URI. Scopes can be created from other scopes by sending #enclosingScope: to XMLNamespaceScope with the other XMLNamespaceScope as the argument.!

!XMLNestedNamespaceScopes commentStamp: 'JAAyer 3/11/2010 00:00' prior: 0!
An XMLNestedNamespaceScopes represents a stack of XMLNamespaceScopes. Sending #enterScope to an instance will cause it to push a new scope onto the stack that inherits all mappings from the previous, enclosing scope. Sending #leaveScop causes that scope to be removed.!

!XMLNode commentStamp: 'JAAyer 3/5/2010 03:47' prior: 0!
This is a base class for XML nodes. It contains testing messages and methods to set and access the parent node.!

!XMLNodeName commentStamp: 'JAAyer 3/23/2010 14:10' prior: 0!
The class represents a name for an element or attribute.!

!XMLNodeWithChildren commentStamp: 'JAAyer 3/23/2010 14:13' prior: 0!
This class represents a node that has child nodes. It provides a basic protocol to add/remove nodes and iterate over them. You will likely use its API through either XMLElement or XMLDocument.!

!XMLNodeWithElements commentStamp: 'JAAyer 3/23/2010 14:30' prior: 0!
This class represents a node with elements. You will likely use its API through XMLDocument or XMLElement.

To retrieve child elements by name, you can send any of the #element- messages in "accessing." The #firstTag- messages search the receiver and its descendants using depth-first traversal for specific elements, and the #tagsNamed- messages iterate over descendent elements also using depth-first traversal.!

!XMLOpenTags commentStamp: 'JAAyer 3/23/2010 14:07' prior: 0!
The class represents a list of currently open tags. It ensures tags are opened and closed in the right order.!

!XMLOrderPreservingDictionary commentStamp: 'JAAyer 3/10/2010 23:59' prior: 0!
The class is a Dictionary that preserves the order in which associations are added to it.!

!XMLPI commentStamp: 'JAAyer 3/5/2010 03:43' prior: 0!
This class represents processing instructions.
!

!XMLStreamReader commentStamp: 'JAAyer 3/24/2010 12:27' prior: 0!
This class represents a stream reader that implements pushBack: using nested streams. This enables subsitution/replacement to be performed without modifying the underlying collections streamed-over or copying them. It also performs line-ending normalization, returning CR and CRLF sequences read from the input stream as a single LF character.

(The code in this class looks the way it does for performance reasons.)!

!XMLStreamWriter commentStamp: 'JAAyer 3/24/2010 12:33' prior: 0!
This class handles the allocation and reuse of multiple WriteStreams. It uses a single execute-around method, #writeWith:, which takes a block that it evaluates with a WriteStream.
!

!XMLString commentStamp: 'JAAyer 3/23/2010 14:11' prior: 0!
XMLString nodes can be created directly using the class-side #string: message or by sending a string #asXMLNode. When printing, they XML-escape their content.!

!XMLTokenizer commentStamp: 'JAAyer 3/23/2010 16:01' prior: 0!
This class reads XML tokens from a stream using an XMLStreamReader. To create a parser, subclass XMLTokenizer and override the #handle- messages to handle tokens as they are read. To cause a token to be read, send #nextToken to an instance.!

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:29'!
bypass
	"Return my reference as is."
	^self reference! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:29'!
forbidden
	self error: 'Forbidden reference usage'! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 18:01'!
include
	"Return my expanded value."
	^value ifNil: [SAXWarning signal: 'XML undefined entity ' , name printString]! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 18:06'!
includedInLiteral
	"Return my expanded value."
	^self include! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 1/4/2002 19:40'!
name
	^name! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:52'!
name: aString
	name := aString! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:22'!
ndata
	^ndata! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:22'!
ndata: aString
	ndata := aString! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'JAAyer 3/11/2010 09:50'!
reference
	"Return my reference as is."
	^self class leadIn, self name, ';'! !

!DTDEntityDeclaration methodsFor: 'invocation' stamp: 'mir 11/16/2000 21:23'!
registerIn: aParser
	aParser entity: self name put: self! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 11/16/2000 10:54'!
value
	^value! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 11/16/2000 10:55'!
value: aString
	value := aString! !

!DTDEntityDeclaration methodsFor: 'invocation' stamp: 'mir 1/15/2002 15:08'!
valueForContext: aContext
	^self perform: (self class behaviorForContext: aContext)! !

!DTDEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:14'!
behaviorForContext: aContext
	^self contextBehavior at: aContext! !

!DTDEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:15'!
contextBehavior
	^contextBehavior! !

!DTDEntityDeclaration class methodsFor: 'class initialization' stamp: 'mir 1/15/2002 18:02'!
initialize
	"DTDEntityDeclaration initialize"

	contextBehavior := Dictionary new.
	contextBehavior
		at: #content put: #include ;
		at: #attributeValueContent put: #includedInLiteral ;
		at: #attributeValue put: #forbidden ;
		at: #entityValue put: #bypass ;
		at: #dtd put: #forbidden ! !

!DTDEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:27'!
leadIn
	^'&'! !

!DTDEntityDeclaration class methodsFor: 'instance creation' stamp: 'mir 11/16/2000 20:13'!
name: aString value: aValueString
	^self new
		name: aString;
		value: aValueString! !

!DTDExternalEntityDeclaration class methodsFor: 'class initialization' stamp: 'mir 1/14/2002 18:15'!
initialize
	"DTDExternalEntityDeclaration initialize"

	contextBehavior := Dictionary new.
	contextBehavior
		at: #content put: #include ;
		at: #attributeValueContent put: #includedInLiteral ;
		at: #attributeValue put: #forbidden ;
		at: #entityValue put: #bypass ;
		at: #dtd put: #forbidden ! !

!DTDParameterEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:30'!
includePE
	"Return my expanded value."
	^self include! !

!DTDParameterEntityDeclaration methodsFor: 'behaviors' stamp: 'JAAyer 3/19/2010 19:11'!
notRecognized
	SAXNotWellFormedException signal: 'Malformed entity.'! !

!DTDParameterEntityDeclaration methodsFor: 'invocation' stamp: 'mir 11/28/2000 17:26'!
registerIn: aParser
	aParser parameterEntity: self name put: self! !

!DTDParameterEntityDeclaration class methodsFor: 'class initialization' stamp: 'mir 1/14/2002 18:15'!
initialize
	"DTDParameterEntityDeclaration initialize"

	contextBehavior := Dictionary new.
	contextBehavior
		at: #content put: #notRecognized: ;
		at: #attributeValueContent put: #notRecognized: ;
		at: #attributeValue put: #notRecognized: ;
		at: #entityValue put: #include: ;
		at: #dtd put: #includePE:! !

!DTDParameterEntityDeclaration class methodsFor: 'accessing' stamp: 'mir 11/16/2000 20:27'!
leadIn
	^'%'! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:36'!
addChildLast: anElement
	self children addLast: anElement! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:36'!
allChildren

	| all |
	all := OrderedCollection new.
	self allChildrenDo: [:each | all addLast: each].
	^all! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:36'!
allChildrenDo: aBlock

	self children do: [:each |
		aBlock value: each.
		each allChildrenDo: aBlock
	]! !

!OPGenericElement methodsFor: 'accessing' stamp: 'tg 1/29/2010 00:54'!
attributeAt: aString
	| result |
	result := self attributes at: aString.
	^ result isEmpty ifFalse: [result] ifTrue: [nil].! !

!OPGenericElement methodsFor: 'accessing' stamp: 'tg 1/23/2010 23:09'!
attributeAt: aString ifPresent: aBlock
	^ self attributes at: aString ifPresent: aBlock! !

!OPGenericElement methodsFor: 'accessing' stamp: 'tg 1/29/2010 00:55'!
attributeNamed: aString
	^ self attributeAt: aString! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:36'!
attributes
	^attributes! !

!OPGenericElement methodsFor: 'accessing' stamp: 'tg 1/24/2010 22:59'!
attributes: anObject
	attributes := anObject.
	attributes associationsDo: [:assoc |
		attributes at: assoc key asLowercase put: assoc value ]! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:37'!
characters
	^characters ifNil: ['']! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:37'!
characters: aString
	characters := aString! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:37'!
children
	^children isNil 
		ifTrue: [children := OrderedCollection new]
		ifFalse: [children].! !

!OPGenericElement methodsFor: 'accessing' stamp: 'tg 1/29/2010 01:07'!
childrenNamed: aTag do: aBlock
	self allChildrenDo: [:each |
		each tag asLowercase = aTag asLowercase ifTrue: [ 
			aBlock value: each ] ]! !

!OPGenericElement methodsFor: 'accessing' stamp: 'tg 1/23/2010 23:29'!
deepChildNamed: aTag do: aBlock

	self allChildrenDo: [:each |
		each tag = aTag ifTrue: [
			aBlock value: each.
			^ self ]
	]! !

!OPGenericElement methodsFor: 'testing' stamp: 'tg 1/29/2010 01:42'!
hasTag: aTag
	^ self tag asLowercase = aTag asLowercase! !

!OPGenericElement methodsFor: 'printing' stamp: 'FabrizioPerin 10/22/2009 19:48'!
printAttributesOn: aStream
	self attributes ifNotNil: [
		self attributes keys do: [ :key | aStream nextPutAll: ' ', key asString, '="', (attributes at: key) asString, '"' ]
	].! !

!OPGenericElement methodsFor: 'printing' stamp: 'fabrizio_perin 8/12/2009 17:05'!
printChildrenOn: aStream
	| childStream |

	aStream nextPut: Character cr.
	childStream := ReadWriteStream on: String new.

	self children do: [ :each | each printOn: childStream ].

	childStream reset.
	[ childStream atEnd ] whileFalse: [
		aStream nextPutAll: '    '.
		aStream nextPutAll: (childStream upTo: Character cr).
		aStream nextPut: Character cr.
	
	]! !

!OPGenericElement methodsFor: 'printing' stamp: 'FP 7/21/2009 16:38'!
printLongTagOn: aStream
	aStream nextPutAll: '<', self tag.
	self printAttributesOn: aStream.
	aStream nextPutAll: '>'.

	self children isEmpty ifTrue: [
		aStream nextPutAll: self characters.
	] ifFalse: [
		self printChildrenOn: aStream.
	].

	aStream nextPutAll: '</', self tag, '>','
'! !

!OPGenericElement methodsFor: 'printing' stamp: 'fabrizio_perin 8/12/2009 16:35'!
printOn: aStream
"	(self children isEmpty & self characters trimSeparators isEmpty)
		ifTrue: [ self printShortTagOn: aStream ]
		ifFalse: [ "
			
			self printLongTagOn: aStream.
			 "]."! !

!OPGenericElement methodsFor: 'printing' stamp: 'FP 7/21/2009 16:40'!
printShortTagOn: aStream
	aStream nextPutAll: '<', self tag.
	self printAttributesOn: aStream.
	aStream nextPutAll: '/>'.! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:37'!
tag
	^tag! !

!OPGenericElement methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:37'!
tag: anObject
	tag := anObject! !

!OPGenericElement class methodsFor: 'instance creation' stamp: 'FP 7/21/2009 16:53'!
newElementWithTag: aString
	"returns an instance of a generic element or of a subclass"
	| wantedClass |
	wantedClass := self allSubclasses
		detect: [:each | each xmlTags includes: aString] ifNone: [ self ].
	^ wantedClass new tag: aString! !

!OPGenericElement class methodsFor: 'instance creation' stamp: 'FP 7/21/2009 16:52'!
xmlTags
	"returns the xml tags that are to be handled by the element"
	^OrderedCollection new! !

!OPOpaxHandler methodsFor: 'public interface' stamp: 'FP 7/21/2009 16:45'!
characters: aString
	self stack last characters: aString.! !

!OPOpaxHandler methodsFor: 'public interface' stamp: 'FabrizioPerin 3/26/2010 15:38'!
endDocument
	^super endDocument! !

!OPOpaxHandler methodsFor: 'public interface' stamp: 'FabrizioPerin 3/26/2010 15:32'!
endElement: qualifiedName prefix: prefix uri: namespaceURI localName: localName 
	"indicates the end of an element. See startElement"
	((stack last class xmlTags includes: localName) or: [stack last isMemberOf: OPGenericElement]) ifTrue: [stack removeLast: 1].! !

!OPOpaxHandler methodsFor: 'initialize' stamp: 'fabrizio.perin 7/27/2009 16:50'!
initialize
	super initialize.! !

!OPOpaxHandler methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:45'!
stack
	^stack! !

!OPOpaxHandler methodsFor: 'accessing' stamp: 'FP 7/21/2009 16:45'!
stack: anObject
	stack := anObject! !

!OPOpaxHandler methodsFor: 'public interface' stamp: 'fabrizio.perin 7/27/2009 16:14'!
startDocument
	stack := OrderedCollection with: (OPRootElement new).
! !

!OPOpaxHandler methodsFor: 'public interface' stamp: 'FabrizioPerin 3/9/2010 12:54'!
startElement: localName prefix: prefix uri: namespaceUri attributes: attributes
"	localName = 'Class' ifTrue: [self halt.]."
"	(ElementManager uniqueInstance elementNamesWithoutPrefix includes: localName) ifTrue: [ |currentElement|
		pName := (attributes select: [ :each | each key type = 'name']) first.
		id := (attributes select: [ :each | each key type = 'xmi.id']) first value.
		currentElement := (ElementManager uniqueInstance elementNamedWithoutPrefix: localName) named: pName in: stack last withId: id."
	| currentElement |
	currentElement := OPGenericElement newElementWithTag: localName.
	currentElement attributes: attributes.
	stack last addChildLast: currentElement.
	stack addLast: currentElement.
"	]."! !

!OPOpaxHandler methodsFor: 'public interface' stamp: 'FabrizioPerin 3/26/2010 15:36'!
startElement: aQualifiedName prefix: prefix uri: namespaceUri localName: localName attributes: attributes
"	localName = 'Class' ifTrue: [self halt.]."
"	(ElementManager uniqueInstance elementNamesWithoutPrefix includes: localName) ifTrue: [ |currentElement|
		pName := (attributes select: [ :each | each key type = 'name']) first.
		id := (attributes select: [ :each | each key type = 'xmi.id']) first value.
		currentElement := (ElementManager uniqueInstance elementNamedWithoutPrefix: localName) named: pName in: stack last withId: id."
	| currentElement |
	currentElement := OPGenericElement newElementWithTag: localName.
	currentElement attributes: attributes.
	stack last addChildLast: currentElement.
	stack addLast: currentElement.
"	]."! !

!OPOpaxHandler class methodsFor: 'instance creation' stamp: 'FabrizioPerin 3/15/2010 13:04'!
on: aStream
	| driver parser |
	driver := SAXDriver on: aStream.
	"driver validating: false."
	parser := self new driver: driver.
	^parser! !

!OPRootElement methodsFor: 'printing' stamp: 'FP 7/22/2009 14:00'!
printOn: aStream
	aStream
		nextPutAll: '<?xml version="1.0" encoding="utf-8"?>';
		nextPut: Character cr.
	self children do: [ :each | each printOn: aStream ].! !

!SAXDriver methodsFor: 'content' stamp: 'JAAyer 3/5/2010 10:38'!
activeSaxHandler
	saxHandler eod
		ifTrue: [self errorExpected: 'No more data expected.'].
	^ saxHandler.! !

!SAXDriver methodsFor: 'accessing' stamp: 'JAAyer 3/19/2010 19:12'!
closeTag: anElementName
	^ [self openTags closeTag: anElementName]
		on: Error
		do: [:error | self notWellFormedError: error messageText]! !

!SAXDriver methodsFor: 'accessing' stamp: 'JAAyer 3/13/2010 13:47'!
currentScope
	^ self usesNamespaces
		ifTrue: [self nestedScopes currentScope]
		ifFalse: [nil]! !

!SAXDriver methodsFor: 'namespaces' stamp: 'JAAyer 3/14/2010 11:49'!
declareNamespaces: aDictionary
	aDictionary keysAndValuesDo: [:prefix :uri |
		self nestedScopes declareNamespace: prefix uri: uri.
		self activeSaxHandler startPrefixMapping: prefix uri: uri]! !

!SAXDriver methodsFor: 'errors' stamp: 'JAAyer 3/22/2010 21:25'!
errorBadPrefix: aPrefix inStartTag: aStartTag
	XMLNamespaceException signal:
		'Start tag <', aStartTag, '> refers to unmapped prefix ', aPrefix! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/5/2010 10:33'!
handleCData: aString
	self activeSaxHandler
		startCData;
		characters: aString;
		endCData! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/5/2010 10:33'!
handleComment: aString
	self activeSaxHandler 
		comment: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/19/2010 19:12'!
handleEndDocument
	self openTags hasOpenTags
		ifTrue: [self notWellFormedError: 'Unclosed tags: ', self openTags printString].
	self saxHandler endDocument! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/24/2010 17:26'!
handleEndTag: anElementName
	| prefix localName namespaceURI |

	self closeTag: anElementName.

	anElementName splitQualifiedNameInto: [:prefixPart :localPart |
		prefix := prefixPart.
		localName := localPart].
	self usesNamespaces
		ifTrue: [namespaceURI := self nestedScopes resolvePrefix: prefix].

	self activeSaxHandler 
		endElement: anElementName
		prefix: prefix
		uri: namespaceURI
		localName: localName.

	self usesNamespaces
		ifTrue: [
			self undeclareNamespaces.
			self nestedScopes leaveScope].! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/5/2010 10:33'!
handlePCData: aString
	self languageEnvironment
		ifNotNil: [aString applyLanguageInfomation: self languageEnvironment].
	self activeSaxHandler 
		characters: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/5/2010 10:33'!
handlePI: piTarget data: piData
	self activeSaxHandler 
		processingInstruction: piTarget data: piData! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 18:29'!
handleStartDocument
	self saxHandler startDocument! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'pb 6/10/2012 18:07'!
handleStartTag: anElementName attributes: anAttributeDictionary namespaces: aNamespaceDictionary
	| prefix localName namespaceURI |
	self flag: #pbfix.
	"no language/locale support in cuis"
	self openTag: anElementName.
	"
	(anAttributeDictionary includesKey: 'xml:lang')
		ifTrue: [self languageEnvironment: (anAttributeDictionary at: 'xml:lang')].
		"
	anElementName splitQualifiedNameInto: [:prefixPart :localPart |
		prefix := prefixPart.
		localName := localPart].

	self usesNamespaces ifTrue: [
		self nestedScopes enterScope.
		self declareNamespaces: aNamespaceDictionary.
		namespaceURI := self nestedScopes resolvePrefix: prefix.
		self
			validateStartTag: anElementName
			prefix: prefix
			attributes: anAttributeDictionary].

	self activeSaxHandler 
		startElement: anElementName
		prefix: prefix
		uri: namespaceURI
		localName: localName
		attributes: anAttributeDictionary! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/5/2010 10:33'!
handleWhitespace: aString
	self activeSaxHandler 
		ignorableWhitespace: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'JAAyer 3/5/2010 10:33'!
handleXMLDecl: attributes
	self activeSaxHandler 
		documentAttributes: attributes! !

!SAXDriver methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 07:21'!
languageEnvironment
	^ languageEnvironment! !

!SAXDriver methodsFor: 'accessing' stamp: 'pb 6/10/2012 18:01'!
languageEnvironment: anIsoString
	self flag: #pbfix.
	"Cuis doesn't support"
	"
	languageEnvironment := LanguageEnvironment localeID: (LocaleID isoString: anIsoString)
	"! !

!SAXDriver methodsFor: 'namespaces' stamp: 'JAAyer 3/11/2010 07:21'!
nestedScopes
	^ nestedScopes ifNil: [nestedScopes := XMLNestedNamespaceScopes new]! !

!SAXDriver methodsFor: 'errors' stamp: 'JAAyer 3/19/2010 19:12'!
notWellFormedError: anErrorMessage
	SAXNotWellFormedException signal: anErrorMessage! !

!SAXDriver methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 09:49'!
openTag: anElementName
	^ self openTags openTag: anElementName! !

!SAXDriver methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 09:24'!
openTags
	^ openTags ifNil: [openTags := XMLOpenTags new]! !

!SAXDriver methodsFor: 'accessing' stamp: 'JAAyer 3/14/2010 10:52'!
saxHandler
	^ saxHandler! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 8/11/2000 17:52'!
saxHandler: aHandler
	saxHandler := aHandler! !

!SAXDriver methodsFor: 'namespaces' stamp: 'JAAyer 3/19/2010 18:57'!
undeclareNamespaces
	| currentScope enclosingScope |

	currentScope := self nestedScopes currentScope.
	enclosingScope := self nestedScopes enclosingScope.

	currentScope prefixMappingsDo: [:prefix :uri |
		(enclosingScope isPrefix: prefix mappedTo: uri)
			ifFalse: [self activeSaxHandler endPrefixMapping: prefix]].

	(currentScope defaultNamespace notNil
		and: [currentScope defaultNamespace ~= enclosingScope defaultNamespace])
		ifTrue: [self activeSaxHandler endPrefixMapping: 'xmlns']! !

!SAXDriver methodsFor: 'accessing' stamp: 'JAAyer 3/14/2010 03:14'!
useNamespaces: aBoolean
	useNamespaces := aBoolean! !

!SAXDriver methodsFor: 'testing' stamp: 'JAAyer 3/16/2010 09:25'!
usesNamespaces
	^ useNamespaces ifNil: [useNamespaces := false]! !

!SAXDriver methodsFor: 'namespaces' stamp: 'JAAyer 3/14/2010 11:44'!
validateStartTag: aStartTag prefix: aPrefix attributes: attributeList
	(aPrefix isNil or: [self nestedScopes isMappedPrefix: aPrefix])
		ifFalse: [self errorBadPrefix: aPrefix inStartTag: aStartTag].

	self validatesAttributes
		ifTrue: [self nestedScopes validateAttributes: attributeList].! !

!SAXDriver methodsFor: 'testing' stamp: 'JAAyer 3/19/2010 18:22'!
validatesAttributes
	^ validateAttributes ifNil: [validateAttributes := true]! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:27'!
characters: aString
	"This call corresponds to the Java SAX call
	characters(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:52'!
comment: commentString
	"This call corresponds to the Java SAX ext call
	comment(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 22:56'!
currentScope
	^ self driver currentScope! !

!SAXHandler methodsFor: 'content' stamp: 'JAAyer 3/16/2010 09:26'!
documentAttributes: aDictionary! !

!SAXHandler methodsFor: 'accessing' stamp: 'JAAyer 3/23/2010 15:55'!
driver
	^ driver! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:34'!
driver: aDriver
	driver := aDriver.
	driver saxHandler: self! !

!SAXHandler methodsFor: 'lexical' stamp: 'JAAyer 2/21/2010 15:13'!
endCData
	"This call corresponds to the Java SAX ext call
	endCData()."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:26'!
endDocument
	"This call corresponds to the Java SAX call
	endDocument()."
	eod := true! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/14/2000 18:07'!
endElement: elementName
! !

!SAXHandler methodsFor: 'content' stamp: 'JAAyer 3/24/2010 16:50'!
endElement: aQualifiedName prefix: aPrefix uri: aUri
	"This call corresponds to the Java SAX call
	endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName).
	By default this call is mapped to the following more convenient call:"

	self endElement: aQualifiedName! !

!SAXHandler methodsFor: 'content' stamp: 'JAAyer 3/24/2010 17:06'!
endElement: aQualifiedName prefix: aPrefix uri: aUri localName: aLocalName
	"This call corresponds to the Java SAX call
	endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName).
	By default this call is mapped to the following more convenient call:"

	self
		invokeDeprecated: #endElement:namespace:namespaceURI:qualifiedName:
		withArguments: (Array with: aLocalName with: aPrefix with: aUri with: aQualifiedName)
		orForwardTo: #endElement:prefix:uri:
		withArguments: (Array with: aQualifiedName with: aPrefix with: aUri)! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:53'!
endEntity: entityName
	"This call corresponds to the Java SAX ext call
	endEntity(java.lang.String name)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:25'!
endPrefixMapping: prefix
	"This call corresonds to the Java SAX call
	endPrefixMapping(java.lang.String prefix)."! !

!SAXHandler methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 09:27'!
eod
	^ eod ifNil: [eod := false]! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:25'!
ignorableWhitespace: aString
	"This call corresonds to the Java SAX call
	ignorableWhitespace(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'error handling' stamp: 'JAAyer 3/24/2010 17:06'!
invokeDeprecated: aDeprecatedSelector withArguments: anOldArgumentArray orForwardTo: aNewSelector withArguments: aNewArgumentArray
	| parentContext |

	parentContext := thisContext sender.
	(self class includesSelector: aDeprecatedSelector)
		ifFalse: [
			aNewSelector ifNil: [^ self].
			^ self
				perform: aNewSelector
				withArguments: aNewArgumentArray].

	(Deprecation
		method: (self class lookupSelector: aDeprecatedSelector)
		explanation: 'use ', parentContext selector, ' instead'
		on: nil
		in: nil) signal.
	self
		perform: aDeprecatedSelector
		withArguments: anOldArgumentArray.! !

!SAXHandler methodsFor: 'testing' stamp: 'JAAyer 3/11/2010 06:43'!
isValidating
	self driver isValidating! !

!SAXHandler methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 06:45'!
isValidating: aBoolean
	self driver isValidating: aBoolean! !

!SAXHandler methodsFor: 'parsing' stamp: 'JAAyer 3/9/2010 01:32'!
parseDocument
	self startDocument.
	[self driver nextToken isNil or: [self eod]] whileFalse.

	"Result of parsing"
	^ self.! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:26'!
processingInstruction: piName data: dataString
	"This call corresonds to the Java SAX call
	processingInstruction(java.lang.String target, java.lang.String data)."! !

!SAXHandler methodsFor: 'entity' stamp: 'mir 8/11/2000 17:33'!
resolveEntity: publicID systemID: systemID
	"This call corresonds to the Java SAX call
	resolveEntity(java.lang.String publicId, java.lang.String systemId)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:45'!
skippedEntity: aString
	"This call corresonds to the Java SAX call
	skippedEntity(java.lang.String name)."! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:53'!
startCData
	"This call corresponds to the Java SAX ext call
	startCData()."! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:54'!
startDTD: declName publicID: publicID systemID: systemID
	"This call corresponds to the Java SAX ext call
	startDTD(java.lang.String name, java.lang.String publicId, java.lang.String systemId)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:45'!
startDocument
	"This call corresonds to the Java SAX call
	startDocument()."! !

!SAXHandler methodsFor: 'content' stamp: 'JAAyer 3/24/2010 17:06'!
startElement: aQualifiedName attributes: aDictionary
	self
		invokeDeprecated: #startElement:attributeList:
		withArguments: (Array with: aQualifiedName with: aDictionary)
		orForwardTo: nil
		withArguments: nil.! !

!SAXHandler methodsFor: 'content' stamp: 'JAAyer 3/24/2010 16:47'!
startElement: aQualifiedName prefix: aPrefix uri: aUri attributes: aDictionary
	self startElement: aQualifiedName attributes: aDictionary! !

!SAXHandler methodsFor: 'content' stamp: 'JAAyer 8/9/2010 17:20'!
startElement: aQualifiedName prefix: aPrefix uri: aUri localName: aLocalName attributes: aDictionary
	"This call corresonds to the Java SAX call
	startElement(java.lang.String namespaceURI, java.lang.String localName,
		java.lang.String qName, Attributes atts)."

	self
		invokeDeprecated: #startElement:namespaceURI:namespace:attributeList:
		withArguments: (Array with: aLocalName with: aUri with: aPrefix with: aDictionary)
		orForwardTo: #startElement:prefix:uri:attributes:
		withArguments: (Array with: aQualifiedName with: aPrefix with: aUri with: aDictionary).! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:54'!
startEntity: entityName
	"This call corresponds to the Java SAX ext call
	startEntity(java.lang.String name)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:47'!
startPrefixMapping: prefix uri: uri
	"This call corresonds to the Java SAX call
	startPrefixMapping(java.lang.String prefix, java.lang.String uri)."! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 6/5/2003 19:28'!
useNamespaces: aBoolean
	self driver useNamespaces: aBoolean! !

!SAXHandler methodsFor: 'testing' stamp: 'JAAyer 3/8/2010 10:38'!
usesNamespaces
	^ self driver usesNamespaces! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'pb 6/10/2012 18:57'!
on: aStream
	| driver |
	driver := SAXDriver on:
		((aStream is: #Stream)
			ifTrue: [aStream]
			ifFalse: [aStream readStream]).
	^ self new
		driver: driver;
		isValidating: true.! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'JAAyer 3/8/2010 10:14'!
parse: aStream
	^ self parseDocumentFrom: aStream! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 02:03'!
parseDTDFrom: aStream
	| parser |

	parser := self on: aStream.
	parser driver startParsingMarkup.
	^ parser parseDocument.! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'JAAyer 3/9/2010 01:29'!
parseDocumentFrom: aStream
	^ self parseDocumentFrom: aStream useNamespaces: true! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'JAAyer 3/9/2010 00:07'!
parseDocumentFrom: aStream useNamespaces: aBoolean
	|  parser |
	parser := self on: aStream.
	parser useNamespaces: aBoolean.
	^ parser parseDocument.! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 1/8/2002 15:55'!
parseDocumentFromFileNamed: fileName
	^self parseDocumentFromFileNamed: fileName readIntoMemory: false! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'JAAyer 3/24/2010 18:56'!
parseDocumentFromFileNamed: fileName readIntoMemory: readIntoMemory
	| stream |
	stream := FileDirectory default readOnlyFileNamed: fileName.
	readIntoMemory
		ifTrue: [stream := stream contentsOfEntireFile readStream].
	^ [self parseDocumentFrom: stream] ensure: [stream close].! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 6/5/2003 19:15'!
parserOnFileNamed: fileName
	^self parserOnFileNamed: fileName readIntoMemory: false! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 6/5/2003 19:14'!
parserOnFileNamed: fileName readIntoMemory: readIntoMemory
	| stream  |
	stream := FileDirectory default readOnlyFileNamed: fileName.
	readIntoMemory
		ifTrue: [stream := stream contentsOfEntireFile readStream].
	^self on: stream! !

!String methodsFor: '*xml-parser' stamp: 'mir 7/14/2006 11:54'!
applyLanguageInfomation: languageEnvironment
	
	| leadingChar |
	leadingChar := languageEnvironment leadingChar.
	self withIndexDo: [:each :idx |
		each asciiValue > 255
			ifTrue: [self at: idx put: (Character leadingChar: leadingChar code: each asUnicode)]]! !

!String methodsFor: '*xml-parser' stamp: 'JAAyer 3/19/2010 19:16'!
asXMLNode
	^ XMLString string: self! !

!String methodsFor: '*xml-parser' stamp: 'JAAyer 3/19/2010 18:18'!
splitQualifiedNameInto: aTwoArgumentBlock
	"Split the name into namespace prefix and local name (the block arguments)."

	| i |
	i := self size.
	[i > 1]
		whileTrue: [
			((self at: i) == $:)
				ifTrue: [
					^ aTwoArgumentBlock
						value: (self copyFrom: 1 to: i - 1)
						value: (self copyFrom: i + 1 to: self size)].
			i := i - 1].
	^ aTwoArgumentBlock
		value: nil
		value: self! !

!WriteStream methodsFor: '*xml-parser' stamp: 'JAAyer 6/24/2010 13:07'!
stringContents
	| newSize |

	newSize := position.
	^ (collection class new: newSize)	
		replaceFrom: 1
		to: newSize
		with: collection
		startingAt: 1! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 00:32'!
expandedName
	^ self nodeName expandedWith: self namespaceURI! !

!XMLAttribute methodsFor: 'testing' stamp: 'JAAyer 3/17/2010 03:48'!
hasPrefix
	^ self nodeName hasPrefix! !

!XMLAttribute methodsFor: 'testing' stamp: 'JAAyer 3/7/2010 04:38'!
isAttribute
	^ true! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:48'!
localName
	^ self nodeName localName! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:48'!
name
	^ self nodeName asString! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 20:50'!
name: aName
	self notifyParentAfterRenamingWith: [self nodeName name: aName]! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/13/2010 12:05'!
namespaceURI
	^ self hasParent ifTrue: [self parent resolvePrefix: self prefix]! !

!XMLAttribute methodsFor: 'private' stamp: 'JAAyer 3/17/2010 03:48'!
nodeName
	^ name ifNil: [name := XMLNodeName new]! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:48'!
prefix
	^ self nodeName prefix! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 20:50'!
prefix: aPrefix
	self notifyParentAfterRenamingWith: [self nodeName prefix: aPrefix]! !

!XMLAttribute methodsFor: 'printing' stamp: 'JAAyer 3/8/2010 10:19'!
printXMLOn: aWriter
	aWriter attribute: self name value: self value
	! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:48'!
qualifiedName
	^ self nodeName qualifiedName! !

!XMLAttribute methodsFor: 'initialization' stamp: 'JAAyer 3/23/2010 13:40'!
setName: aName value: aValue
	self nodeName name: aName.
	value := aValue.! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 08:17'!
value
	^ value! !

!XMLAttribute methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 08:18'!
value: aString
	value := aString! !

!XMLAttribute class methodsFor: 'instance creation' stamp: 'JAAyer 3/23/2010 13:37'!
name: aName value: aValue
	^ self new setName: aName value: aValue! !

!XMLAttributeList methodsFor: 'adding' stamp: 'JAAyer 3/13/2010 12:48'!
addName: aName value: aValue
	^ self addLast: (XMLAttribute name: aName value: aValue)! !

!XMLAttributeList methodsFor: 'private' stamp: 'JAAyer 3/22/2010 22:44'!
errorNonAttributeNode
	^ self domError: 'Cannot contain non-attribute nodes'.! !

!XMLAttributeList methodsFor: 'testing' stamp: 'JAAyer 3/12/2010 01:04'!
includesName: aString
	^ self nodesByName includesKey: aString! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 07:19'!
names
	^ self collect: [:each | each name]! !

!XMLAttributeList methodsFor: 'enumerating' stamp: 'JAAyer 3/10/2010 07:20'!
namesAndValuesDo: aBlock
	self do: [:each | aBlock value: each name value: each value]! !

!XMLAttributeList methodsFor: 'enumerating' stamp: 'JAAyer 3/10/2010 07:20'!
namesDo: aBlock
	self do: [:each | aBlock value: each name]! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:58'!
nodeNamed: aString
	^ self nodeNamed: aString ifAbsent: [self defaultValue]! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 01:04'!
nodeNamed: aString ifAbsent: aBlock
	^ self nodesByName at: aString ifAbsent: aBlock! !

!XMLAttributeList methodsFor: 'private' stamp: 'JAAyer 3/22/2010 22:45'!
nodeRenamed: aNode from: anOldName to: aNewName
	self nodesByName
		removeKey: anOldName;
		at: aNewName put: aNode! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 01:05'!
nodesByName
	^ nodesByName ifNil: [nodesByName := Dictionary new: 10]! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:58'!
removeNodeNamed: aString
	^ self removeNodeNamed: aString ifAbsent: [self defaultValue]! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 22:35'!
removeNodeNamed: aString ifAbsent: aBlock
	^ (self remove: (self nodeNamed: aString ifAbsent: [^ aBlock value])) value! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:58'!
valueAt: aName
	^ self valueAt: aName ifAbsent: [self defaultValue]! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 01:04'!
valueAt: aName ifAbsent: aBlock
	^ (self nodesByName
		at: aName
		ifAbsent: [^ aBlock value]) value! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/7/2010 07:26'!
valueAt: aName ifAbsentPut: aValue
	^ self
		valueAt: aName
		ifAbsent: [self valueAt: aName put: aValue]! !

!XMLAttributeList methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 13:11'!
valueAt: aName put: aValue
	(self includesName: aName)
		ifTrue: [(self nodeNamed: aName) value: aValue]
		ifFalse: [self addName: aName value: aValue].
	^ aValue.! !

!XMLAttributeList methodsFor: 'private' stamp: 'JAAyer 3/22/2010 22:46'!
with: aBlock add: aNode
	aNode isAttribute ifFalse: [^ self errorNonAttributeNode].

	(super with: aBlock add: aNode) ifNil: [^ nil].

	self nodesByName
		at: aNode name
		ifPresent: [self removeNodeNamed: aNode name].
	self nodesByName at: aNode name put: aNode.
	^ aNode.! !

!XMLAttributeList methodsFor: 'private' stamp: 'JAAyer 3/12/2010 01:04'!
with: aBlock remove: aNode
	(super with: aBlock remove: aNode) ifNil: [^ nil].
	self nodesByName removeKey: aNode name.
	^ aNode.! !

!XMLCData methodsFor: 'printing' stamp: 'jvds 8/9/2010 10:47'!
printXMLOn: writer
	writer cdata: self string! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'JAAyer 3/9/2010 01:38'!
document
	^ document! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 20:31'!
document: aDocument
	document := aDocument! !

!XMLDOMParser methodsFor: 'content' stamp: 'JAAyer 3/16/2010 09:32'!
documentAttributes: attributeList
	self document
		version: (attributeList at: 'version' ifAbsent: [nil]);
		encoding: (attributeList at: 'encoding' ifAbsent: [nil]);
		requiredMarkup: (attributeList at: 'requiredMarkup' ifAbsent: [nil])
! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'JAAyer 3/8/2010 20:29'!
domDocument
	[self parseDocument] ensure: [self driver stream close].

	^ document.! !

!XMLDOMParser methodsFor: 'lexical' stamp: 'JAAyer 8/9/2010 17:27'!
endCData
	isInCDataSection := false! !

!XMLDOMParser methodsFor: 'content' stamp: 'JAAyer 3/16/2010 09:31'!
endDocument
	self popNode.
	super endDocument! !

!XMLDOMParser methodsFor: 'content' stamp: 'JAAyer 3/24/2010 16:55'!
endElement: aQualifiedName prefix: aPrefix uri: aUri localName: aLocalName
	self popNode! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 09:26'!
incremental
	^ incremental ifNil: [incremental := false]! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'mir 1/8/2001 12:05'!
incremental: aBoolean
	incremental := aBoolean! !

!XMLDOMParser methodsFor: 'testing' stamp: 'JAAyer 8/9/2010 17:27'!
isInCDataSection
	^ isInCDataSection ifNil: [isInCDataSection := false]! !

!XMLDOMParser methodsFor: 'private' stamp: 'JAAyer 3/22/2010 15:28'!
nodeStack
	^ nodeStack ifNil: [nodeStack := OrderedCollection new: 30]! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'JAAyer 3/9/2010 01:30'!
parseDocument
	super parseDocument.
	^ self document.! !

!XMLDOMParser methodsFor: 'private' stamp: 'JAAyer 3/16/2010 09:31'!
popNode
	^ self nodeStack removeLast! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'JAAyer 8/9/2010 17:25'!
preservesCDataSections
	^ preservesCDataSections ifNil: [preservesCDataSections := false]! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'JAAyer 8/9/2010 17:25'!
preservesCDataSections: aBoolean
	"Specify whether the receiver shoud preserve CDATA sections.
	If true XMLCDATASection nodes will be created. Otherwise plain
	XMLString nodes will be used."

	preservesCDataSections := aBoolean! !

!XMLDOMParser methodsFor: 'private' stamp: 'JAAyer 3/16/2010 09:30'!
pushElement: anElement

	(self incremental not and: [self nodeStack notEmpty])
			ifTrue: [self topNode addElement: anElement].

	self pushNode: anElement.! !

!XMLDOMParser methodsFor: 'private' stamp: 'JAAyer 3/16/2010 09:31'!
pushNode: aNode
	^ self nodeStack addLast: aNode
! !

!XMLDOMParser methodsFor: 'lexical' stamp: 'JAAyer 8/9/2010 17:27'!
startCData
	isInCDataSection := true! !

!XMLDOMParser methodsFor: 'content' stamp: 'JAAyer 3/16/2010 09:30'!
startDocument
	self document: XMLDocument new.
	self pushNode: self document ! !

!XMLDOMParser methodsFor: 'content' stamp: 'JAAyer 3/24/2010 17:07'!
startElement: aQualifiedName prefix: aPrefix uri: aUri localName: aLocalName attributes: aDictionary
	self pushElement:
		((XMLElement
			qualifiedName: aQualifiedName
			prefix: aPrefix
			localName: aLocalName
			attributes: aDictionary)
				setScope: self currentScope)! !

!XMLDOMParser methodsFor: 'private' stamp: 'JAAyer 3/16/2010 09:29'!
topNode
	^ self nodeStack isEmpty
		ifTrue: [nil]
		ifFalse: [self nodeStack last]! !

!XMLDocument methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 01:48'!
dtd
	^ dtd! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 11/30/2000 17:48'!
dtd: aDTD
	dtd := aDTD! !

!XMLDocument methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 01:48'!
encoding	
	^ encoding ifNil: ['UTF-8']! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
encoding: aString	
	encoding := aString! !

!XMLDocument methodsFor: 'testing' stamp: 'JAAyer 2/19/2010 23:51'!
isDocument
	^ true! !

!XMLDocument methodsFor: 'printing' stamp: 'JAAyer 2/25/2010 14:37'!
printCanonicalOn: aStream
	| writer |
	writer := XMLWriter on: aStream.
	writer canonical: true.
	self printXMLOn: writer! !

!XMLDocument methodsFor: 'printing' stamp: 'mir 5/16/2003 14:08'!
printXMLOn: writer
	version ifNotNil: [writer xmlDeclaration: self version encoding: self encoding].
	super printXMLOn: writer! !

!XMLDocument methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 01:48'!
requiredMarkup	
	^ requiredMarkup! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
requiredMarkup: aString	
	requiredMarkup := aString! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 11/29/2007 14:42'!
root
	"return my root element"
	^ self topElement ! !

!XMLDocument methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 01:48'!
version	
	^ version! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
version: aString	
	version := aString! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 11:50'!
addContent: aString
	"Adds a string or string node. If the last child node is already a string node,
	the characters being added are concatenated to it."

	(self hasContents and: [self lastNode isText])
		ifTrue: [self lastNode addContent: aString]
		ifFalse: [self addNode: (XMLString string: aString)]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
at: aString
	^ self at: aString ifAbsent: ['']
! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
at: aString ifAbsent: aBlock
	^ self attributeAt: aString ifAbsent: aBlock! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
at: aString put: aValue
	^ self attributeAt: aString put: aValue! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 23:19'!
attributeAssociations
	^ self attributeNodes collect: [:each | each name -> each value]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 05:09'!
attributeAt: aString
	^ self attributeAt: aString ifAbsent: [nil]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 23:19'!
attributeAt: aString ifAbsent: aBlock
	^ self attributeNodes valueAt: aString ifAbsent: aBlock! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 23:19'!
attributeAt: aString ifAbsentPut: aBlock
	^ self attributeNodes valueAt: aString ifAbsentPut: aBlock! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 23:19'!
attributeAt: aString put: aValue
	^ self attributeNodes valueAt: aString put: aValue! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 23:19'!
attributeNames
	^ self attributeNodes names! !

!XMLElement methodsFor: 'enumerating' stamp: 'JAAyer 3/11/2010 23:19'!
attributeNamesAndValuesDo: aTwoArgumentBlock
	self attributeNodes namesAndValuesDo: aTwoArgumentBlock! !

!XMLElement methodsFor: 'enumerating' stamp: 'JAAyer 3/11/2010 23:19'!
attributeNamesDo: aBlock
	self attributeNodes namesDo: aBlock! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 23:19'!
attributeNodeAt: aName
	^ self attributeNodes nodeNamed: aName! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 11:30'!
attributeNodes
	^ attributes ifNil: [self setAttributeNodes: XMLAttributeList new. attributes]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/7/2010 22:03'!
attributes
	^ XMLOrderPreservingDictionary withAll: self attributeAssociations! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 01:48'!
characterData
	^ self contentString! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 11:49'!
contentString
	| firstContentNode |

	firstContentNode := self elementsAndContents first.
	^ (firstContentNode notNil and:[firstContentNode isText])
		ifTrue: [firstContentNode string]
		ifFalse: ['']! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:37'!
contentString: aString
	self
		removeNodes;
		addContent: aString! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
contentStringAt: aString
	^ (self elementAt: aString ifAbsent: [^ '']) contentString! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:37'!
contents
	^ self nodes select: [:each | each isText]! !

!XMLElement methodsFor: 'enumerating' stamp: 'JAAyer 2/12/2010 19:09'!
contentsDo: aBlock
	self nodesDo: [:each | each isText ifTrue: [aBlock value: each]]! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/7/2010 03:12'!
declareNamespace: aPrefix uri: aUri
	(self scope isPrefix: aPrefix mappedTo: aUri)
		ifTrue: [^ self].

	self mapPrefix: aPrefix to: aUri.! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/12/2010 11:22'!
declareNamespaces: aDictionaryOrArray
	self
		from: aDictionaryOrArray 
		keysAndValuesDo: [:namespaceName :uri |
			namespaceName splitQualifiedNameInto: [:xmlns :prefix |
				(prefix = 'xmlns')
					ifTrue: [self defaultNamespace: uri]
					ifFalse: [self declareNamespace: prefix uri: uri]]]! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/12/2010 01:32'!
declaredNamespaces
	| namespaces |
	namespaces := self scope prefixMappings copy.
	self defaultNamespace notNil
		ifTrue: [namespaces at: 'xmlns' put: self defaultNamespace].
	^ namespaces.! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/7/2010 03:12'!
defaultNamespace
	^ self scope defaultNamespace! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/12/2010 11:25'!
defaultNamespace: aNamespaceUri
	| oldDefaultNamespace |

	oldDefaultNamespace := self defaultNamespace.
	self
		updateEnclosedScopesWith: [:each | each defaultNamespace: aNamespaceUri]
		where: [:each | each defaultNamespace = oldDefaultNamespace].! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 2/15/2010 12:22'!
elementsAndContents
	^ self nodes select: [:each | each isTagOrText]! !

!XMLElement methodsFor: 'enumerating' stamp: 'JAAyer 2/15/2010 12:23'!
elementsAndContentsDo: aBlock
	self nodesDo: [:each | each isTagOrText ifTrue: [aBlock value: each]]! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/22/2010 00:32'!
expandedName
	^ self nodeName expandedWith: self namespaceURI! !

!XMLElement methodsFor: 'private' stamp: 'pb 6/10/2012 19:15'!
from: aDictionaryOrArray keysAndValuesDo: aBlock
	aDictionaryOrArray isEmptyOrNil ifTrue: [^ self].

	self flag: #pbfix.
	"isDictionary has been removed and is: not defined"
	"
	(aDictionaryOrArray isDictionary
	"
	(aDictionaryOrArray class = Dictionary
		ifTrue: [aDictionaryOrArray associations]
		ifFalse: [aDictionaryOrArray])
			associationsDo: [:association |
				aBlock
					value: association key
					value: association value]! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 3/11/2010 23:19'!
hasAttributes
	^ self attributeNodes notEmpty! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 14:37'!
hasContents
	^ self contents notEmpty! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 3/17/2010 03:48'!
hasPrefix
	^ self nodeName hasPrefix! !

!XMLElement methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
ifNamed: aString do: aOneArgumentBlock
	"If the receiver tag equals aString, evaluate aOneArgumentBlock with the receiver"

	(self isNamed: aString)
		ifTrue: [aOneArgumentBlock valueWithPossibleArgument: self]
! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 3/11/2010 23:19'!
includesAttribute: aString
	^ self attributeNodes includesName: aString! !

!XMLElement methodsFor: 'private' stamp: 'JAAyer 3/5/2010 09:50'!
inheritMappingsFrom: aScope
	self recurse: [:each | each scope inheritMappingsFrom: aScope]! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 3/8/2010 04:58'!
isNamed: aString
	^ (self localName = aString)
		or: [self qualifiedName = aString]! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 3/8/2010 04:54'!
isNamedAny: aStringArray
	^ aStringArray anySatisfy: [:each | self isNamed: each]! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 01:48'!
isTag
	^ true! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:48'!
localName
	^ self nodeName localName! !

!XMLElement methodsFor: 'private' stamp: 'JAAyer 3/12/2010 11:55'!
mapPrefix: aPrefix to: aUri
	| oldMapping |

	oldMapping := self scope resolvePrefix: aPrefix ifUnmapped: [nil].
	self
		updateEnclosedScopesWith: [:each | each mapPrefix: aPrefix to: aUri]
		where: [:each |
			(each isPrefix: aPrefix mappedTo: oldMapping)
				or: [(each isMappedPrefix: aPrefix) not]]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:48'!
name
	^ self nodeName asString! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 21:32'!
name: aName
	self notifyParentAfterRenamingWith: [self nodeName name: aName].
	self validateName.! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/22/2010 20:50'!
name: aQualifiedName namespaceURI: aUri
	aQualifiedName splitQualifiedNameInto: [:prefixPart :localPart |
		self declareNamespace: prefixPart uri: aUri].
	self name: aQualifiedName.! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/14/2010 12:52'!
namespaceURI
	^ self resolvePrefix: self prefix! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:37'!
nextElement
	^ self hasParent ifTrue: [self parent elementAfter: self]! !

!XMLElement methodsFor: 'announcements' stamp: 'JAAyer 3/22/2010 20:08'!
nodeAdded: aNode
	super nodeAdded: aNode.
	
	(aNode isTag and: [self usesNamespaces])
		ifTrue: [aNode inheritMappingsFrom: self scope].
	aNode isAttribute
		ifTrue: [self validateAttribute: aNode name]
		! !

!XMLElement methodsFor: 'private' stamp: 'JAAyer 3/22/2010 20:11'!
nodeName
	^ name ifNil: [name := XMLNodeName new]! !

!XMLElement methodsFor: 'announcements' stamp: 'JAAyer 3/22/2010 22:45'!
nodeRenamed: aNode from: anOldName to: aNewName
	super nodeRenamed: aNode from: aNode to: aNewName.
	aNode isAttribute
		ifTrue: [
			self validateAttribute: aNewName.
			self attributeNodes nodeRenamed: aNode from: anOldName to: aNewName]! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/17/2010 03:48'!
prefix
	^ self nodeName prefix! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/22/2010 21:32'!
prefix: aPrefix
	self notifyParentAfterRenamingWith: [self nodeName prefix: aPrefix].
	self validateName.! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/7/2010 08:18'!
prefix: aPrefix uri: aUri
	self declareNamespace: aPrefix uri: aUri.
	self prefix: aPrefix.! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:37'!
previousElement
	^ self hasParent ifTrue: [self parent elementBefore: self]! !

!XMLElement methodsFor: 'printing' stamp: 'JAAyer 3/12/2010 01:36'!
printXMLOn: writer
	writer
		startElement: self name
		attributeList: self attributeNodes
		namespaces: self declaredNamespaces.
	(writer canonical not
		and: [self isEmpty and: [self attributeNodes notEmpty]])
		ifTrue: [writer endEmptyTag: self name]
		ifFalse: [
			writer endTag.
			super printXMLOn: writer.
			writer endTag: self name]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:48'!
qualifiedName
	^ self nodeName qualifiedName! !

!XMLElement methodsFor: 'searching' stamp: 'JAAyer 3/7/2010 04:00'!
recurse: aBlock
	"See superclass version."

	aBlock value: self.

	super recurse: aBlock.! !

!XMLElement methodsFor: 'searching' stamp: 'JAAyer 3/7/2010 04:01'!
recurseUntilTrue: aBlock
	"See superclass version."

	(aBlock value: self)
		ifTrue: [^ self].

	^ super recurseUntilTrue: aBlock.! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 23:19'!
removeAttribute: aString
	^ self attributeNodes removeNodeNamed: aString! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/13/2010 12:26'!
resolvePrefix: aPrefix
	^ self scope resolvePrefix: aPrefix! !

!XMLElement methodsFor: 'private' stamp: 'JAAyer 3/7/2010 21:17'!
scope
	^ scope ifNil: [self setScope: XMLNamespaceScope new. scope]! !

!XMLElement methodsFor: 'initialization' stamp: 'JAAyer 3/22/2010 20:13'!
setAttributeNodes: anAttributeList
	(attributes := anAttributeList) parent: self.
	attributes do: [:each | each parent: self]! !

!XMLElement methodsFor: 'initialization' stamp: 'JAAyer 3/12/2010 11:23'!
setAttributes: aDictionaryOrArray
	self attributeNodes removeAll.
	self
		from: aDictionaryOrArray
		keysAndValuesDo: [:attributeName :attributevalue |
			self attributeNodes
				addName: attributeName
				value: attributevalue].! !

!XMLElement methodsFor: 'initialization' stamp: 'JAAyer 3/19/2010 18:38'!
setScope: aNamespaceScope
	scope := aNamespaceScope.
	self useNamespaces: aNamespaceScope notNil.! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 23:57'!
tag
	^ self qualifiedName! !

!XMLElement methodsFor: 'private' stamp: 'JAAyer 3/5/2010 08:06'!
updateEnclosedScopesWith: aBlock where: aConditional
	"Updates the receiver's scope and the scopes of all descendants with aBlock
	where aConditional is true. It stops descending down a path of a subtree
	as soon as aConditional is false, but continues down other paths."

	(aConditional value: self scope)
		ifTrue: [aBlock value: self scope]
		ifFalse: [^ self].

	self elementsDo: [:each |
		each updateEnclosedScopesWith: aBlock where: aConditional]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/13/2010 13:50'!
useNamespaces: aBoolean
	usesNamespaces := aBoolean! !

!XMLElement methodsFor: 'testing' stamp: 'JAAyer 3/13/2010 13:40'!
usesNamespaces
	^ usesNamespaces ifNil: [usesNamespaces := true]! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/22/2010 11:06'!
validateAttribute: anAttributeName
	self usesNamespaces ifFalse: [^ self].

	self scope
		validateAttribute: anAttributeName
		in: self attributes.! !

!XMLElement methodsFor: 'namespaces' stamp: 'JAAyer 3/22/2010 21:31'!
validateName
	(self usesNamespaces and: [self hasPrefix])
		ifTrue: [self scope validatePrefix: self prefix]! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
valueFor: aString
	self deprecated: 'use #at: as a shortcut instead'.

	^ self at: aString.! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
valueFor: aString ifAbsent: aBlock 
	self deprecated: 'use #at:ifAbsent: as a shortcut instead'.

	^ self at: aString ifAbsent: aBlock! !

!XMLElement methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 05:09'!
@ aString
	"shorthand form"
	^ self at: aString! !

!XMLElement class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 23:10'!
named: aLocalName
	^ self new name: aLocalName! !

!XMLElement class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 23:09'!
named: aLocalName attributes: aDictionaryOrArray
	^ (self named: aLocalName)
		setAttributes: aDictionaryOrArray! !

!XMLElement class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 23:12'!
named: aQualifiedName namespaceURI: aUri
	^ self new name: aQualifiedName namespaceURI: aUri! !

!XMLElement class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 10:07'!
named: aQualifiedName namespaceURI: aUri attributes: aDictionaryOrArray
	^ (self named: aQualifiedName namespaceURI: aUri)
		setAttributes: aDictionaryOrArray! !

!XMLElement class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 23:08'!
named: aQualifiedName namespaces: aNamespaceDictionaryOrArray
	^ self new
		declareNamespaces: aNamespaceDictionaryOrArray;
		named: aQualifiedName! !

!XMLElement class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 10:22'!
named: aQualifiedName namespaces: aNamespaceDictionaryOrArray attributes: aDictionaryOrArray
	^ (self
		named: aQualifiedName
		namespaces: aNamespaceDictionaryOrArray)
			setAttributes: aDictionaryOrArray! !

!XMLElement class methodsFor: 'private' stamp: 'JAAyer 3/24/2010 17:02'!
qualifiedName: aQualifiedName prefix: aPrefix localName: aLocalName attributes: anAttributeDictionary
	| element attributeNodes |

	element := self new.
	element nodeName
		setQualifiedName: aQualifiedName
		prefix: aPrefix
		localName: aLocalName.
	attributeNodes := XMLAttributeList new.
	anAttributeDictionary keysAndValuesDo: [:key :value |
		attributeNodes addLast: (XMLAttribute name: key value: value)].
	element setAttributeNodes: attributeNodes.
	^ element.! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 16:19'!
cacheElement: anElement
	self elements add: anElement.
	self
		cacheElement: anElement
		underName: anElement localName.
	anElement hasPrefix
		ifTrue: [
			self
				cacheElement: anElement
				underName: anElement qualifiedName]! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 16:18'!
cacheElement: anElement underName: aName
	(self elementsByName
		at: aName
		ifAbsentPut: [self newElementList]) add: anElement! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 16:18'!
elements
	^ elements ifNil: [elements := self newElementList]! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/13/2010 12:58'!
elementsByName 
	^ elementsByName ifNil: [elementsByName := Dictionary new].! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 16:18'!
elementsNamed: aName
	^ self elementsByName at: aName ifAbsent: [self newElementList]! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 10:49'!
firstElementNamed: aName
	^ (self elementsByName at: aName ifAbsent: [^ nil]) first! !

!XMLElementCache methodsFor: 'private' stamp: 'JAAyer 3/27/2010 22:28'!
newElementList
	^ XMLOrderedList new! !

!XMLElementCache methodsFor: 'accessing' stamp: 'pb 6/10/2012 19:07'!
rebuildCacheFrom: aList
	self elements removeAll.
	elementsByName := nil.

	aList do: [:each | each isTag ifTrue: [self cacheElement: each]].! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 10:42'!
uncacheElement: anElement
	self elements remove: anElement.

	self
		uncacheElement: anElement
		underName: anElement localName.
	anElement hasPrefix
		ifTrue: [
			self
				uncacheElement: anElement
				underName: anElement qualifiedName].! !

!XMLElementCache methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 10:41'!
uncacheElement: anElement underName: aName
	(self elementsByName at: aName) remove: anElement.

	(self elementsByName at: aName)
		ifEmpty: [self elementsByName removeKey: aName].! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/2/2010 20:54'!
defaultNamespace
	^ defaultNamespace! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/2/2010 20:45'!
defaultNamespace: aNamespaceUri
	defaultNamespace := aNamespaceUri! !

!XMLNamespaceScope methodsFor: 'errors' stamp: 'JAAyer 3/19/2010 18:31'!
errorAttribute: anAttribute aliases: anAliasedAttribute with: aUri
	self namespaceError:
		('Attribute "{1}" aliases attribute "{2}"; both prefixes map to {3}'
			format: (Array with: anAttribute with: anAliasedAttribute with: aUri))! !

!XMLNamespaceScope methodsFor: 'errors' stamp: 'JAAyer 3/11/2010 05:01'!
errorUnmappedPrefix: aPrefix
	self namespaceError: 'Unmapped namespace prefix "', aPrefix, '"'! !

!XMLNamespaceScope methodsFor: 'initializing' stamp: 'NorbertHartl 8/5/2010 11:09'!
inheritMappingsFrom: anEnclosingScope
	self defaultNamespace
		ifNil: [self defaultNamespace: anEnclosingScope defaultNamespace].

	anEnclosingScope prefixMappingsDo: [:prefix :uri |
		(self isMappedPrefix: prefix)
			ifFalse: [self mapPrefix: prefix to: uri]].! !

!XMLNamespaceScope methodsFor: 'testing' stamp: 'JAAyer 3/11/2010 00:01'!
isMappedPrefix: aPrefix
	^ self prefixMappings includesKey: aPrefix! !

!XMLNamespaceScope methodsFor: 'testing' stamp: 'JAAyer 3/5/2010 09:50'!
isPrefix: aPrefix mappedTo: aUri
	^ ((self isMappedPrefix: aPrefix)
		and: [(self resolvePrefix: aPrefix) = aUri])! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 00:25'!
mapPrefix: aPrefix to: aUri
	(aPrefix isEmptyOrNil or: [aPrefix = 'xmlns'])
		ifTrue: [self defaultNamespace: aUri]
		ifFalse: [self prefixMappings at: aPrefix put: (aUri ifNil: [''])]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/6/2010 02:54'!
mappedPrefixes
	^ self prefixMappings keys! !

!XMLNamespaceScope methodsFor: 'errors' stamp: 'JAAyer 3/11/2010 05:00'!
namespaceError: anErrorMessage
	^ XMLNamespaceException signal: anErrorMessage! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/7/2010 01:16'!
prefixMappings
	^ prefixMappings ifNil: [prefixMappings := XMLOrderPreservingDictionary new]! !

!XMLNamespaceScope methodsFor: 'enumerating' stamp: 'JAAyer 3/9/2010 01:17'!
prefixMappingsDo: aTwoArgumentBlock
	self prefixMappings keysAndValuesDo: [:prefix :uri |
		aTwoArgumentBlock value: prefix value: uri]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 01:45'!
prefixesAliasing: aPrefix
	"Locate all prefixes that map to the same URI the given prefix does."
	| uri |

	uri := self
		resolvePrefix: aPrefix
		ifUnmapped: [self errorUnmappedPrefix: aPrefix].

	^ self mappedPrefixes select: [:each |
		(each ~= aPrefix)
			and: [self isPrefix: each mappedTo: uri]].! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 09:50'!
resolvePrefix: aPrefix
	"Retrieve the URI of the given namespace prefix, if it is defined. A nil namespace
	returns the global namespace"

	^ self resolvePrefix: aPrefix ifUnmapped: [nil]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 09:50'!
resolvePrefix: aPrefix ifUnmapped: aBlock
	"Retrieve the URI of the given namespace prefix, if it is defined. 
	A nil namespace returns the default namespace. 
	If no namespace can be found the value of the block is returned"

	^ (aPrefix isEmptyOrNil or: [aPrefix = 'xmlns'])
		ifTrue: [self defaultNamespace]
		ifFalse: [self prefixMappings at: aPrefix ifAbsent: aBlock]! !

!XMLNamespaceScope methodsFor: 'validation' stamp: 'JAAyer 3/19/2010 19:03'!
validateAttribute: anAttributeName in: aDictionary
	| namespaceUri aliasingAttributeName |
	
	anAttributeName splitQualifiedNameInto: [:prefix :localName |
		prefix ifNil: [^ self].
		self validatePrefix: prefix.
		namespaceUri := self resolvePrefix: prefix.

		(self prefixesAliasing: prefix) do: [:aliasingPrefix |
			aliasingAttributeName := aliasingPrefix, ':', localName.
			(aDictionary includesKey: aliasingAttributeName)
				ifTrue: [
					self
						errorAttribute: aliasingAttributeName
						aliases: anAttributeName
						with: namespaceUri]]].! !

!XMLNamespaceScope methodsFor: 'validation' stamp: 'JAAyer 3/13/2010 13:43'!
validateAttributes: attributeList
	"check all attribute namespaces are defined and not duplicated by aliasing"
	| prefix localName qualifiedAlias |

	attributeList keysDo: [:attrName |
		self
			validateAttribute: attrName
			in: attributeList]! !

!XMLNamespaceScope methodsFor: 'validation' stamp: 'JAAyer 3/13/2010 13:42'!
validatePrefix: aPrefix
	(self isMappedPrefix: aPrefix)
		ifFalse: [self errorUnmappedPrefix: aPrefix].! !

!XMLNamespaceScope class methodsFor: 'instance creation' stamp: 'JAAyer 3/5/2010 09:50'!
enclosingScope: aScope
	^ self new inheritMappingsFrom: aScope! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/3/2010 22:33'!
currentLevel
	^ self scopes size! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/3/2010 22:34'!
currentScope
	^ self scopes
		at: self currentLevel
		ifAbsent: [self emptyScope]! !

!XMLNestedNamespaceScopes methodsFor: 'scope' stamp: 'JAAyer 3/5/2010 09:50'!
declareNamespace: aPrefix uri: aUri
	"Declare the given name space prefix with the given URL"

	self currentScope mapPrefix: aPrefix to: aUri! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/3/2010 22:37'!
defaultNamespace
	^ self currentScope defaultNamespace! !

!XMLNestedNamespaceScopes methodsFor: 'private' stamp: 'JAAyer 3/13/2010 13:44'!
emptyScope
	^ XMLNamespaceScope new! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/2/2010 22:14'!
enclosingScope
	^ self scopes
		at: self scopes size - 1
		ifAbsent: [self emptyScope]! !

!XMLNestedNamespaceScopes methodsFor: 'scope' stamp: 'JAAyer 3/13/2010 13:42'!
enterScope
	self pushScope: (XMLNamespaceScope enclosingScope: self currentScope)! !

!XMLNestedNamespaceScopes methodsFor: 'testing' stamp: 'JAAyer 3/5/2010 09:50'!
isMappedPrefix: aPrefix
	^ self currentScope isMappedPrefix: aPrefix! !

!XMLNestedNamespaceScopes methodsFor: 'testing' stamp: 'JAAyer 3/5/2010 09:50'!
isPrefix: aPrefix mappedTo: aUri
	^ self currentScope isPrefix: aPrefix mappedTo: aUri! !

!XMLNestedNamespaceScopes methodsFor: 'scope' stamp: 'JAAyer 3/4/2010 00:12'!
leaveScope
	self popScope! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/4/2010 00:12'!
popScope
	self scopes removeLast! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/13/2010 13:43'!
pushScope: aScope
	self scopes add: aScope! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 06:00'!
resolvePrefix: aPrefix
	^ self currentScope resolvePrefix: aPrefix! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 09:50'!
resolvePrefix: aPrefix ifUnmapped: aBlock
	^ self currentScope resolvePrefix: aPrefix ifUnmapped: aBlock! !

!XMLNestedNamespaceScopes methodsFor: 'accessing' stamp: 'JAAyer 3/2/2010 21:35'!
scopes
	^ scopes ifNil: [scopes := OrderedCollection new: 20]! !

!XMLNestedNamespaceScopes methodsFor: 'validation' stamp: 'JAAyer 3/4/2010 00:31'!
validateAttributes: attributeList
	self currentScope validateAttributes: attributeList! !

!XMLNestedNamespaceScopes methodsFor: 'validation' stamp: 'JAAyer 3/11/2010 03:41'!
validatePrefix: aPrefix
	self currentScope validatePrefix: aPrefix! !

!XMLNode methodsFor: 'errors' stamp: 'JAAyer 3/11/2010 05:19'!
domError: aMessage
	^ XMLDOMException signal: aMessage! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 14:37'!
hasParent
	^ self parent notNil! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 3/7/2010 04:38'!
isAttribute
	^ false! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/19/2010 23:50'!
isDocument
	^ false! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/12/2010 20:23'!
isElement
	^ self isTag! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/21/2010 19:50'!
isEmpty
	^ true! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/19/2010 15:57'!
isPI
	^ self isProcessingInstruction! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 01:48'!
isProcessingInstruction
	^ false! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 01:48'!
isTag
	^ false! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/21/2010 19:50'!
isTagOrText
	^ self isTag or: [self isText]! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 01:48'!
isText
	^ false! !

!XMLNode methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:37'!
nextNode
	^ self hasParent ifTrue: [self parent nodeAfter: self]! !

!XMLNode methodsFor: 'testing' stamp: 'JAAyer 3/7/2010 20:16'!
notEmpty
	^ self isEmpty not! !

!XMLNode methodsFor: 'private' stamp: 'JAAyer 3/23/2010 12:38'!
notifyParentAfterRenamingWith: aBlock
	| oldName newName |

	oldName := self name.
	aBlock value.
	newName := self name.
	self hasParent
		ifTrue: [self parent nodeRenamed: self from: oldName to: newName]! !

!XMLNode methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:37'!
previousNode
	^ self hasParent ifTrue: [self parent nodeBefore: self]! !

!XMLNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:45'!
printOn: stream
	self printXMLOn: (XMLWriter on: stream)! !

!XMLNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:45'!
printXMLOn: writer
	self subclassResponsibility! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/10/2010 07:10'!
add: newNode after: oldNode
	^ self with: [super add: newNode after: oldNode] add: newNode! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/10/2010 07:10'!
add: newNode afterIndex: oldNode
	^ self with: [super add: newNode afterIndex: oldNode] add: newNode! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/10/2010 07:10'!
add: newNode before: oldNode
	^ self with: [super add: newNode before: oldNode] add: newNode! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/10/2010 07:10'!
add: newNode beforeIndex: oldNode
	^ self with: [super add: newNode beforeIndex: oldNode] add: newNode! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/10/2010 07:10'!
addFirst: aNode
	^ self with: [super addFirst: aNode] add: aNode! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/10/2010 07:11'!
addLast: aNode
	^ self with: [super addLast: aNode] add: aNode! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/10/2010 07:14'!
at: anIndex ifAbsentPut: aValue
	^ self
		at: anIndex
		ifAbsent: [self at: anIndex put: aValue]! !

!XMLNodeList methodsFor: 'adding' stamp: 'JAAyer 3/12/2010 06:49'!
at: anIndex put: aNode
	(self at: anIndex)
		ifNotNil: [self with: [super at: anIndex put: nil] remove: (self at: anIndex)].
	^ self with: [super at: anIndex put: aNode] add: aNode.! !

!XMLNodeList methodsFor: 'errors' stamp: 'JAAyer 3/22/2010 22:44'!
errorNodeWithParent
	^ self domError: 'Cannot add node that is already the child of another node'! !

!XMLNodeList methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 17:16'!
parent
	^ parent! !

!XMLNodeList methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 17:16'!
parent: aParent
	parent := aParent! !

!XMLNodeList methodsFor: 'removing' stamp: 'JAAyer 3/10/2010 07:25'!
remove: aNode ifAbsent: aBlock
	^ self with: [super remove: aNode ifAbsent: [^ aBlock value]] remove: aNode! !

!XMLNodeList methodsFor: 'removing' stamp: 'JAAyer 3/10/2010 08:53'!
removeAll
	self ifEmpty: [^ self].
	self copy do: [:each | self with: [self removeFirst] remove: each]! !

!XMLNodeList methodsFor: 'removing' stamp: 'JAAyer 3/10/2010 07:33'!
removeAt: anIndex
	^ self with: [super removeAt: anIndex] remove: (self at: anIndex)! !

!XMLNodeList methodsFor: 'removing' stamp: 'JAAyer 3/10/2010 07:33'!
removeFirst
	^ self removeAt: 1! !

!XMLNodeList methodsFor: 'removing' stamp: 'JAAyer 3/10/2010 07:33'!
removeLast
	^ self removeAt: self size! !

!XMLNodeList methodsFor: 'private' stamp: 'JAAyer 3/22/2010 21:27'!
with: aBlock add: aNode
	aNode hasParent ifTrue: [^ self errorNodeWithParent].
	self
		with: [aBlock value]
		onError: [^ nil].
	self parent ifNotNil: [self parent nodeAdded: aNode].

	^ aNode.! !

!XMLNodeList methodsFor: 'private' stamp: 'JAAyer 3/22/2010 12:57'!
with: aBlock onError: errorBlock
	^ aBlock
		on: Error
		do: [:error |
			errorBlock valueWithPossibleArgument: error.
			error signal]! !

!XMLNodeList methodsFor: 'private' stamp: 'JAAyer 3/22/2010 17:18'!
with: aBlock remove: aNode

	self
		with: [
			aBlock value.
			self parent ifNotNil: [self parent nodeRemoved: aNode]]
		onError: [^ nil].
	^ aNode.! !

!XMLNodeList class methodsFor: 'instance creation' stamp: 'JAAyer 3/22/2010 19:38'!
parent: aParentNode
	^ self new parent: aParentNode! !

!XMLNodeName methodsFor: 'converting' stamp: 'JAAyer 3/17/2010 03:46'!
asString
	^ self qualifiedName! !

!XMLNodeName methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 00:32'!
expandedWith: aUri
	^ aUri
		ifNil: [self localName]
		ifNotNil: ['{', aUri, '}', self localName]! !

!XMLNodeName methodsFor: 'testing' stamp: 'JAAyer 3/5/2010 07:31'!
hasPrefix
	^ prefix notNil! !

!XMLNodeName methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 05:10'!
localName
	^ localName ifNil: ['']! !

!XMLNodeName methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 20:09'!
name: aQualifiedName
	aQualifiedName
		splitQualifiedNameInto: [:prefixPart :localPart |
			self setPrefix: prefixPart localName: localPart]! !

!XMLNodeName methodsFor: 'accessing' stamp: 'JAAyer 3/5/2010 07:49'!
prefix
	^ prefix! !

!XMLNodeName methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 20:09'!
prefix: aPrefix
	self setPrefix: aPrefix localName: self localName! !

!XMLNodeName methodsFor: 'printing' stamp: 'JAAyer 3/13/2010 12:01'!
printOn: aStream
	aStream nextPutAll: self qualifiedName! !

!XMLNodeName methodsFor: 'accessing' stamp: 'JAAyer 3/17/2010 03:46'!
qualifiedName
	^ qualifiedName ifNil: ['']! !

!XMLNodeName methodsFor: 'private' stamp: 'JAAyer 3/24/2010 17:02'!
setPrefix: aPrefix localName: aLocalName
	aPrefix isEmptyOrNil
		ifTrue: [
			self
				setQualifiedName: aLocalName
				prefix: nil
				localName: aLocalName]
		ifFalse: [
			self
				setQualifiedName: aPrefix, ':', aLocalName
				prefix: aPrefix
				localName: aLocalName]! !

!XMLNodeName methodsFor: 'private' stamp: 'JAAyer 3/24/2010 17:02'!
setQualifiedName: aQualifiedName prefix: aPrefix localName: aLocalName
	qualifiedName := aQualifiedName.
	prefix := aPrefix.
	localName := aLocalName.! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 08:50'!
addNode: aNode
	^ self nodes addLast: aNode! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 07:37'!
addNode: aNode after: afterNode
	^ self nodes add: aNode after: afterNode! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 07:37'!
addNode: aNode before: beforeNode
	^ self nodes add: aNode before: beforeNode! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 07:38'!
addNodeFirst: aNode
	^ self nodes addFirst: aNode! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/6/2010 06:36'!
addNodes: aNodeCollection
	aNodeCollection do: [:each | self addNode: each].
	^ aNodeCollection.! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 10:34'!
firstNode
	^ self nodes first! !

!XMLNodeWithChildren methodsFor: 'testing' stamp: 'JAAyer 3/6/2010 06:37'!
includesNode: aNode
	^ self nodes includes: aNode! !

!XMLNodeWithChildren methodsFor: 'testing' stamp: 'JAAyer 3/6/2010 06:37'!
isEmpty
	^ self nodes isEmpty! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 10:35'!
lastNode
	^ self nodes last! !

!XMLNodeWithChildren methodsFor: 'announcements' stamp: 'JAAyer 3/22/2010 21:28'!
nodeAdded: aNode
	aNode parent: self! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 12:37'!
nodeAfter: aNode
	^ self nodes after: aNode! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 12:37'!
nodeBefore: aNode
	^ self nodes before: aNode! !

!XMLNodeWithChildren methodsFor: 'announcements' stamp: 'JAAyer 3/22/2010 20:02'!
nodeRemoved: aNode
	aNode parent: nil! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 19:53'!
nodes
	^ nodes ifNil: [nodes := XMLNodeList parent: self]! !

!XMLNodeWithChildren methodsFor: 'enumerating' stamp: 'JAAyer 3/6/2010 06:37'!
nodesDo: aBlock
	self nodes do: aBlock! !

!XMLNodeWithChildren methodsFor: 'printing' stamp: 'JAAyer 3/6/2010 06:37'!
printXMLOn: writer
	self nodesDo: [:each | each printXMLOn: writer].! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 08:01'!
removeNode: aNode
	^ self nodes remove: aNode! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/10/2010 08:01'!
removeNodes
	self nodes removeAll! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/6/2010 06:36'!
removeNodes: aNodeCollection
	aNodeCollection do: [:each | self removeNode: each].
	^ aNodeCollection.! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 12:35'!
replaceNode: aNode with: aReplacementNode
	^ self nodes
		at: [self nodes indexOf: aNode]
		put: aReplacementNode! !

!XMLNodeWithChildren methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 01:12'!
topNode
	^ self firstNode! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 14:37'!
addElement: anElement
	self addNode: anElement! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 11:45'!
elementAfter: anElement
	^ self elements after: anElement! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 10:42'!
elementAt: aString
	^ self elementCache firstElementNamed: aString! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 10:48'!
elementAt: aString ifAbsent: aBlock
	^ (self elementCache firstElementNamed:  aString) ifNil: aBlock! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
elementAtAny: aStringArray
	^ self elementAtAny: aStringArray ifAbsent: [nil]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
elementAtAny: aStringArray ifAbsent: aBlock
	| answer |

	aStringArray do: [:each | (answer := self elementAt: each) ifNotNil: [^ answer]].
	^ aBlock value.! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 11:45'!
elementBefore: anElement
	^ self elements before: anElement! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/13/2010 13:01'!
elementCache
	^ elementCache ifNil: [elementCache := XMLElementCache new]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
elementUnqualifiedAt: aString
	self deprecated: 'use #elementAt: with an unqualified name instead'.

	^ self elementAt: aString.! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
elementUnqualifiedAt: aString ifAbsent: aBlock
	self deprecated: 'use #elementAt:ifAbsent: with an unqualified name instead'.

	^ self elementAt: aString ifAbsent: aBlock.! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/13/2010 13:03'!
elements
	^ self elementCache elements! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 10:45'!
elementsAt: aString
	^ self elementCache elementsNamed: aString! !

!XMLNodeWithElements methodsFor: 'enumerating' stamp: 'JAAyer 3/8/2010 04:54'!
elementsAt: aString do: aBlock
	(self elementsAt: aString) do: aBlock! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/22/2010 10:45'!
elementsAt: aString ifAbsent: aBlock
	^ (self elementCache elementsNamed: aString) ifEmpty: aBlock.! !

!XMLNodeWithElements methodsFor: 'enumerating' stamp: 'JAAyer 2/12/2010 20:57'!
elementsDo: aBlock
	self elements do: aBlock! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 10:34'!
firstElement
	^ self elements first! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
firstTagNamed: aString 
	"Return the first encountered node with the specified tag. Pass the message on"

	^ self recurseUntilTrue: [:each | each isNamed: aString]! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
firstTagNamed: aString with: aBlock
	"Return the first encountered node with the specified tag that
	allows the block to evaluate to true. Pass the message on"

	^ self recurseUntilTrue: [:each |
		(each isNamed: aString) and: [aBlock valueWithPossibleArgument: each]]! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
firstTagNamedAny: aStringArray
	"Return the first encountered node with any of the specified tag names. Pass the message on"

	^ self recurseUntilTrue: [:each | each isNamedAny: aStringArray]! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
firstTagNamedAny: aStringArray with: aBlock
	"Return the first encountered node with any of the specified tag names that
	allows the block to evaluate to true. Pass the message on"

	^ self recurseUntilTrue: [:each |
		(each isNamedAny: aStringArray) and: [aBlock valueWithPossibleArgument: each]]! !

!XMLNodeWithElements methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 01:48'!
hasElements
	^ self elements notEmpty! !

!XMLNodeWithElements methodsFor: 'testing' stamp: 'JAAyer 3/8/2010 04:54'!
includesElement: aString
	^ (self elementAt: aString) notNil! !

!XMLNodeWithElements methodsFor: 'parsing' stamp: 'JAAyer 3/23/2010 15:49'!
innerXML: aString
	| parsedDocument newNodes |

	parsedDocument := XMLDOMParser
		parseDocumentFrom: aString
		useNamespaces: self usesNamespaces.
	newNodes := parsedDocument nodes copy.
	parsedDocument removeNodes.
	self
		removeNodes;
		addNodes: newNodes.! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 10:34'!
lastElement
	^ self elements last! !

!XMLNodeWithElements methodsFor: 'announcements' stamp: 'JAAyer 3/22/2010 20:04'!
nodeAdded: aNode
	super nodeAdded: aNode.

	aNode isTag
		ifTrue: [self elementCache cacheElement: aNode].! !

!XMLNodeWithElements methodsFor: 'announcements' stamp: 'JAAyer 3/22/2010 20:05'!
nodeRemoved: aNode
	super nodeRemoved: aNode.

	aNode isTag
		ifTrue: [self elementCache uncacheElement: aNode].! !

!XMLNodeWithElements methodsFor: 'announcements' stamp: 'JAAyer 3/22/2010 20:55'!
nodeRenamed: aNode from: anOldName to: aNewName
	aNode isTag
		ifTrue: [self elementCache rebuildCacheFrom: self nodes].! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 2/27/2010 19:28'!
recurse: aBlock
	"Descend depth-first visiting each element with aBlock."

	self elementsDo: [:each | each recurse: aBlock]! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 2/27/2010 19:29'!
recurseUntilTrue: aBlock
	"Descend depth-first visiting each element with aBlock until one such evaluation
	of aBlock with an element is true, then return that element. If no evaluation is
	true, then return nil."

	| result |

	self elementsDo: [:each | (result := each recurseUntilTrue: aBlock) ifNotNil: [^ result]].
	^ nil.! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
tagsNamed: aString childrenDo: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match"

	self deprecated: 'use #elementsAt:do: instead'.

	self elementsAt: aString do: aOneArgumentBlock.! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
tagsNamed: aString childrenDoAndRecurse: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match and recurse"

	self elementsDo: [:each | each tagsNamed: aString do: aOneArgumentBlock]! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
tagsNamed: aString contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver.
	The receiver has no tag, so pass the message on"

	self tagsNamed: aString do: [:each | each contentsDo: aBlock]! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
tagsNamed: aString do: aOneArgumentBlock
	"Search for nodes with tag aString. When encountered evaluate aOneArgumentBlock"

	self recurse: [:each | each ifNamed: aString do: aOneArgumentBlock]! !

!XMLNodeWithElements methodsFor: 'searching' stamp: 'JAAyer 3/8/2010 04:54'!
tagsNamed: aString ifReceiverDoAndRecurse: aOneArgumentBlock
	self deprecated: 'use #tagsNamed:do: instead'.

	self tagsNamed: aString do: aOneArgumentBlock.! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 01:12'!
topElement
	^ self firstElement! !

!XMLOpenTags methodsFor: 'accessing' stamp: 'JAAyer 3/23/2010 16:39'!
closeTag: aTagName
	(self enclosingTag = aTagName)
		ifTrue: [^ self tags removeLast].

	(self hasOpenTag: aTagName)
		ifTrue: [
			self error:
				('<{1}> tag closed before enclosed tags {2}'
					format: (Array with: aTagName with: (self enclosedTagsAsString: aTagName)))]
		ifFalse: [
			self error:
				('Closing </{1}> tag without corresponding opening <{1}> tag'
					format: (Array with: aTagName))]! !

!XMLOpenTags methodsFor: 'printing' stamp: 'NorbertHartl 8/5/2010 11:10'!
enclosedTagsAsString: aTagName
	| enclosedTags |

	enclosedTags := String new writeStream.
	self
		printOn: enclosedTags
		startingAt: (self tags lastIndexOf: aTagName) + 1.
	^ enclosedTags contents.! !

!XMLOpenTags methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 15:59'!
enclosingTag
	^ self tags
		at: self totalOpen
		ifAbsent: ['']! !

!XMLOpenTags methodsFor: 'testing' stamp: 'JAAyer 3/16/2010 08:24'!
hasOpenTag: aTagName
	^ self tags includes: aTagName! !

!XMLOpenTags methodsFor: 'testing' stamp: 'JAAyer 3/16/2010 08:24'!
hasOpenTags
	^ self tags notEmpty! !

!XMLOpenTags methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 16:16'!
maxTagsToPrint
	^ 5! !

!XMLOpenTags methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 08:23'!
openTag: aTagName
	^ self tags addLast: aTagName! !

!XMLOpenTags methodsFor: 'printing' stamp: 'JAAyer 3/16/2010 16:07'!
printOn: aStream
	self printOn: aStream startingAt: 1! !

!XMLOpenTags methodsFor: 'printing' stamp: 'JAAyer 3/16/2010 16:15'!
printOn: aStream startingAt: aPosition
	| endPosition |

	endPosition :=  self totalOpen min: (aPosition + self maxTagsToPrint - 1).
	(self tags copyFrom: aPosition to: endPosition)
		do: [:each |
			aStream
				nextPut: $<;
				nextPutAll: each;
				nextPut: $>]
		separatedBy: [aStream nextPutAll: ', '].
	(endPosition < self totalOpen)
		ifTrue: [aStream nextPutAll: '...'].! !

!XMLOpenTags methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 08:24'!
tags
	^ tags ifNil: [tags := OrderedCollection new: 30]! !

!XMLOpenTags methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 15:59'!
totalOpen
	^ self tags size
	! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation.! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
addAll: anAssociationCollection
	anAssociationCollection do: [:association | self add: association].
	^ anAssociationCollection.! !

!XMLOrderPreservingDictionary methodsFor: 'private' stamp: 'JAAyer 3/8/2010 04:54'!
addKeyToSequence: aString
	super at: aString ifAbsent: [orderedKeys addLast: aString]! !

!XMLOrderPreservingDictionary methodsFor: 'converting' stamp: 'JAAyer 3/6/2010 23:02'!
asOrderedCollection
	^ self associations asOrderedCollection! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
associationAt: aKey
	^ self associationAt: aKey ifAbsent: [self defaultValue]! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/6/2010 23:02'!
associations
	^ self keys collect: [:key | self associationAt: key]! !

!XMLOrderPreservingDictionary methodsFor: 'enumerating' stamp: 'JAAyer 3/8/2010 04:55'!
associationsDo: aBlock
	^ self associations do: [:each | aBlock value: each]! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 22:01'!
at: aKey
	^ self at: aKey ifAbsent: [self defaultValue]! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:58'!
at: aKey ifAbsentPut: aBlock
	self addKeyToSequence: aKey.
	^ super at: aKey ifAbsentPut: aBlock.! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:57'!
at: aKey put: aBlock
	self addKeyToSequence: aKey.
	^ super at: aKey put: aBlock.! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
defaultValue
	^ nil! !

!XMLOrderPreservingDictionary methodsFor: 'as yet unclassified' stamp: 'pb 6/10/2012 18:51'!
initialize
	"added method for tests"
	self flag: #pbfix.
	self initialize: 100! !

!XMLOrderPreservingDictionary methodsFor: 'initialization' stamp: 'pb 6/10/2012 18:53'!
initialize: aSize
	"
	super initialize: aSize.
	"
	super initialize.
	orderedKeys := OrderedCollection new: aSize.! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/6/2010 23:07'!
keys
	^ orderedKeys copy! !

!XMLOrderPreservingDictionary methodsFor: 'enumerating' stamp: 'JAAyer 3/6/2010 23:03'!
keysAndValuesDo: aBlock
	self keysDo: [:each | aBlock value: each value: (self at: each)]! !

!XMLOrderPreservingDictionary methodsFor: 'enumerating' stamp: 'JAAyer 3/6/2010 23:15'!
keysDo: aBlock
	orderedKeys do: aBlock! !

!XMLOrderPreservingDictionary methodsFor: 'printing' stamp: 'JAAyer 3/7/2010 21:39'!
printElementsOn: aStream
	self ifEmpty: [^ self].

	aStream nextPut: $(.
	self associations doWithIndex: [:associaiton :i |
		aStream
			print: associaiton key;
			nextPutAll: '->';
			print: associaiton value.
		(i < self size)
			ifTrue: [aStream space]].
	aStream nextPut: $).! !

!XMLOrderPreservingDictionary methodsFor: 'printing' stamp: 'JAAyer 3/12/2010 08:17'!
printNameOn: aStream
	aStream
		nextPutAll: 'an';
		space;
		nextPutAll: self class name! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/6/2010 23:07'!
removeAll
	orderedKeys removeAll.
	super removeAll.! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:54'!
removeKey: aKey
	^ self removeKey: aKey ifAbsent: [self defaultValue]! !

!XMLOrderPreservingDictionary methodsFor: 'accessing' stamp: 'JAAyer 3/8/2010 04:57'!
removeKey: aKey ifAbsent: aBlock
	self removeKeyFromSequence: aKey.
	^ super removeKey: aKey ifAbsent: aBlock.! !

!XMLOrderPreservingDictionary methodsFor: 'private' stamp: 'JAAyer 3/7/2010 20:37'!
removeKeyFromSequence: aKey
	(self includesKey: aKey)
		ifTrue: [orderedKeys remove: aKey]! !

!XMLOrderPreservingDictionary methodsFor: 'initialization' stamp: 'JAAyer 3/6/2010 23:17'!
setAssociationsFrom: aDictionary
	self addAll: aDictionary associations! !

!XMLOrderPreservingDictionary class methodsFor: 'instance creation' stamp: 'JAAyer 3/6/2010 23:17'!
newFrom: aDictionary
	^ self new setAssociationsFrom: aDictionary! !

!XMLOrderedList methodsFor: 'accessing' stamp: 'pb 6/10/2012 18:56'!
after: anObject
	^ self after: anObject ifNone: [self defaultValue]! !

!XMLOrderedList methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 07:20'!
at: anIndex
	^ self at: anIndex ifAbsent: [self defaultValue]! !

!XMLOrderedList methodsFor: 'accessing' stamp: 'JAAyer 3/12/2010 07:22'!
at: anIndex ifAbsent: aBlock
	^ (anIndex between: 1 and: self size)
		ifTrue: [super at: anIndex]
		ifFalse: [aBlock value]! !

!XMLOrderedList methodsFor: 'accessing' stamp: 'pb 6/10/2012 18:56'!
before: anObject
	^ self before: anObject ifNone: [self defaultValue]! !

!XMLOrderedList methodsFor: 'copying' stamp: 'JAAyer 3/12/2010 07:30'!
copy
	^ self species withAll: self! !

!XMLOrderedList methodsFor: 'copying' stamp: 'JAAyer 3/12/2010 07:30'!
copyEmpty
	^ self species new: self size! !

!XMLOrderedList methodsFor: 'copying' stamp: 'JAAyer 3/19/2010 21:29'!
copyFrom: start to: end
	^ super copyFrom: (start max: 1) to: (self size min: end)! !

!XMLOrderedList methodsFor: 'defaults' stamp: 'JAAyer 3/12/2010 06:48'!
defaultValue
	^ nil! !

!XMLOrderedList methodsFor: 'errors' stamp: 'JAAyer 3/12/2010 11:36'!
domError: anErrorMessage
	XMLDOMException signal: anErrorMessage.
	^ nil.
	! !

!XMLOrderedList methodsFor: 'printing' stamp: 'JAAyer 3/12/2010 08:18'!
printOn: aStream
	self printXMLOn: (XMLWriter on: aStream)! !

!XMLOrderedList methodsFor: 'printing' stamp: 'JAAyer 3/12/2010 08:18'!
printXMLOn: writer
	self do: [:each | each printXMLOn: writer].! !

!XMLOrderedList methodsFor: 'copying' stamp: 'JAAyer 3/12/2010 11:02'!
species
	^ XMLOrderedList! !

!XMLPI methodsFor: 'accessing' stamp: 'JAAyer 2/23/2010 22:40'!
data
	^ data ifNil: ['']! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
data: aString
	data := aString! !

!XMLPI methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 01:48'!
isProcessingInstruction
	^ true! !

!XMLPI methodsFor: 'printing' stamp: 'mir 1/17/2002 15:53'!
printXMLOn: writer
	writer pi: self target data: self data! !

!XMLPI methodsFor: 'accessing' stamp: 'JAAyer 2/23/2010 22:40'!
target
	^ target ifNil: ['']! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
target: aString
	target := aString! !

!XMLPI class methodsFor: 'instance creation' stamp: 'JAAyer 2/25/2010 01:48'!
target: targetName data: aString
	^ self new
		target: targetName;
		data: aString! !

!XMLStreamReader methodsFor: 'testing' stamp: 'JAAyer 3/16/2010 16:44'!
atEnd
	^ self peek isNil! !

!XMLStreamReader methodsFor: 'accessing' stamp: 'NorbertHartl 8/5/2010 16:39'!
basicNext
	"Returns next character in the stream after performing line-endings normalization.
	Normalization does not occur across nested streams."
	| nextChar |
	
	stream atEnd ifTrue: [ ^ nil ].
	((nextChar := stream next) == CarriageReturn)
		ifTrue: [
			nextChar := LineFeed.
			(stream peek == LineFeed)
				ifTrue: [stream next]].
	^ nextChar.! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/22/2010 22:00'!
checkNestedStream
	nestedStreams ifNotNil: [
		(peekChar isNil and: [stream atEnd])
			ifTrue: [
				self popNestingLevel.
				self checkNestedStream]]! !

!XMLStreamReader methodsFor: 'initialization' stamp: 'JAAyer 3/19/2010 13:35'!
initialize
	stream := nil.
	nestedStreams := nil.
	peekChar := nil.
	buffer := WriteStream on: (String new: 128).! !

!XMLStreamReader methodsFor: 'private' stamp: 'JAAyer 3/16/2010 08:29'!
nestedStreams
	^ nestedStreams ifNil: [nestedStreams := OrderedCollection new]! !

!XMLStreamReader methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 15:00'!
next
	"Return the next character from the current input stream. If the current
	stream is at end, pop to the next nesting level if there is one.

	Due to the potential nesting of original document, included documents
	and replacment texts the streams are held in a stack representing the
	nested streams. The current stream is the top one."
	| nextChar |

	peekChar
		ifNil: [
			nestedStreams ifNotNil: [self checkNestedStream].
			^ self basicNext]
		ifNotNil: [
			nextChar := peekChar.
			peekChar := nil.
			^ nextChar].! !

!XMLStreamReader methodsFor: 'testing' stamp: 'JAAyer 3/28/2010 13:20'!
nextMatchAll: aString
	| i oldStream oldPosition oldPeekChar nextChar pushBackString |

	(oldPeekChar := self peek) == (aString at: 1)
		ifFalse: [^ false].
	oldPosition := (oldStream := stream) position.
	i := 1.
	[(i <= aString size)
		and: [(aString at: i) == (nextChar := self next)]]
		whileTrue: [i := i + 1].
	(i > aString size)
		ifTrue: [^ true].

	stream == oldStream
		ifTrue: [
			peekChar := oldPeekChar.
			stream position: oldPosition]
		ifFalse:[
			pushBackString := aString copyFrom: 1 to: i - 1.
			self pushBack:
				(nextChar
					ifNotNil: [pushBackString copyWith: nextChar]
					ifNil: [pushBackString])].
	^ false.! !

!XMLStreamReader methodsFor: 'tokenizing' stamp: 'JAAyer 3/19/2010 14:27'!
nextWhitespace
	| nextChar |

	buffer reset.
	[(nextChar := self peek) notNil
		and: [SeparatorTable includes: nextChar]]
		whileTrue: [buffer nextPut: self next].
	^ buffer stringContents.! !

!XMLStreamReader methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 07:45'!
peek
	"Return the next character from the current input stream.

	Due to the potential nesting of original document, included
	documents and replacment texts the streams are held in a stack
	representing the nested streams. The current stream is the top one."

	^ peekChar
		ifNil: [
			nestedStreams ifNotNil: [self checkNestedStream].
			peekChar := self basicNext]! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/20/2010 15:18'!
popNestingLevel
	nestedStreams ifNotNil: [
		self stream close.
		self stream: self nestedStreams removeLast.
		self nestedStreams ifEmpty: [nestedStreams := nil]]! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/27/2010 22:35'!
pushBack: aString
	"Fixed to push the string before the peek char (if any)."
	| pushBackString |

	pushBackString := peekChar
		ifNil: [aString]
		ifNotNil: [aString copyWith: peekChar].
	peekChar := nil.
	self pushStream: (ReadStream on: pushBackString).! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/27/2010 22:37'!
pushStream: aStream
	"Continue parsing from the new nested stream."
	self unpeek.
	self nestedStreams addLast: self stream.
	self stream: aStream.! !

!XMLStreamReader methodsFor: 'private' stamp: 'JAAyer 3/16/2010 09:14'!
readNumberBase: base
	"Read a hex number from stream until encountering $; "

	| value digit |

	base = 10
		ifFalse: [ | numberString | 
			numberString := self upTo: $;.
			stream skip: -1.
			^ Integer readFrom: numberString asUppercase readStream base: base].

	value := 0.
	digit := DigitTable at: self peek asciiValue.
	digit < 0
		ifTrue: [^ nil].
	self next.
	value := digit.
	[digit := DigitTable at: self peek asciiValue.
	digit < 0
		ifTrue: [^value]
		ifFalse: [
			self next.
			value := value * base + digit]
		] repeat.
	^ value! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/16/2010 16:44'!
skipSeparators
	| nextChar |

	[(nextChar := self peek) notNil
		and: [SeparatorTable includes: nextChar]]
		whileTrue: [self next].! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/16/2010 08:57'!
skipUpTo: delimiter
	| nextChar |

	[self atEnd or: [(nextChar := self next) == delimiter]] whileFalse.
	^ nextChar == delimiter.! !

!XMLStreamReader methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 08:01'!
stream
	^ stream! !

!XMLStreamReader methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 09:34'!
stream: aNewStream
	"Continue parsing from the new nested stream."
	stream := aNewStream! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/11/2010 07:46'!
streamEncoding: encodingString

	| converterClass |
	Smalltalk at: #TextConverter ifPresent: [:tc | 
		(stream respondsTo: #converter:) ifTrue: [
			converterClass := tc defaultConverterClassForEncoding: encodingString asLowercase.
			converterClass ifNotNil: [stream converter: converterClass new]]].! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/16/2010 16:38'!
topStream
	^ nestedStreams
		ifNil: [self stream]
		ifNotNil: [self nestedStreams first]! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/11/2010 07:46'!
unpeek
	"Fixed to use nested stream since multi-byte streams
	do not properly override pushBack: to deal with multi-byte
	characters."
	
	peekChar ifNotNil: [self pushBack: '']! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/19/2010 14:27'!
upTo: aDelimiter
	| nextChar |

	buffer reset.
	[self atEnd or: [(nextChar := self next) == aDelimiter]]
		whileFalse: [buffer nextPut: nextChar].

	^ nextChar == aDelimiter
		ifTrue: [buffer stringContents]
		ifFalse: [nil].! !

!XMLStreamReader methodsFor: 'streaming' stamp: 'JAAyer 3/19/2010 14:27'!
upToAll: aDelimitingString
	"Answer a subcollection from the current access position to the occurrence
	(if any, but not inclusive) of delimitingString. If delimitingString is not
	in the stream, answer the entire rest of the stream."

	buffer reset.
	[self atEnd or: [self nextMatchAll: aDelimitingString]]
		whileFalse: [buffer nextPut: self next].
	^ buffer stringContents.! !

!XMLStreamReader class methodsFor: 'initialization' stamp: 'pb 6/10/2012 17:55'!
initialize
	"XMLStreamReader initialize"

	SeparatorTable  := CharacterSet new.
	#(9 10 12 13 32) do: [:each | SeparatorTable add: each asCharacter].

	DigitTable := Array new: 256.
	DigitTable atAllPut: -1.
	($0 to: $9) do: [:each | DigitTable at: each asciiValue put: each digitValue].
	($a to: $f) do: [:each | DigitTable at: each asciiValue put: each digitValue].
	($A to: $F) do: [:each | DigitTable at: each asciiValue put: each digitValue].

	CarriageReturn := Character crCharacter.
	LineFeed := Character lfCharacter.! !

!XMLStreamReader class methodsFor: 'instance creation' stamp: 'JAAyer 3/11/2010 09:34'!
on: anInitialStream
	^ self new stream: anInitialStream! !

!XMLStreamWriter methodsFor: 'growing' stamp: 'JAAyer 3/23/2010 17:57'!
grow
	streams := streams, self newStreams! !

!XMLStreamWriter methodsFor: 'initialization' stamp: 'JAAyer 3/23/2010 17:57'!
initialize
	streams := self newStreams.
	nextStream := 1.! !

!XMLStreamWriter methodsFor: 'growing' stamp: 'JAAyer 3/23/2010 18:12'!
newStreams
	^ (1 to: 10) collect: [:i | WriteStream on: (String new: 128)]! !

!XMLStreamWriter methodsFor: 'writing' stamp: 'JAAyer 3/23/2010 18:04'!
writeWith: aBlock
	| writeStream res|

	(nextStream = streams size)
		ifTrue: [self grow].
	(writeStream := streams at: nextStream) reset.
	nextStream := nextStream + 1.
	^ [aBlock value: writeStream] ensure: [nextStream := nextStream - 1].! !

!XMLString methodsFor: 'accessing' stamp: 'JAAyer 2/23/2010 02:55'!
addContent: aString
	self string: self string, (self toString: aString)! !

!XMLString methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 01:48'!
characterData
	^ self string! !

!XMLString methodsFor: 'testing' stamp: 'JAAyer 2/25/2010 01:48'!
isText
	^ true! !

!XMLString methodsFor: 'printing' stamp: 'mir 1/17/2002 15:53'!
printXMLOn: writer
	writer pcData: self string! !

!XMLString methodsFor: 'accessing' stamp: 'JAAyer 2/25/2010 01:48'!
string
	^ string ifNil: ['']! !

!XMLString methodsFor: 'accessing' stamp: 'JAAyer 2/23/2010 02:55'!
string: aString
	string := self toString: aString! !

!XMLString methodsFor: 'private' stamp: 'JAAyer 3/24/2010 18:54'!
toString: aString
	^ (aString isKindOf: String)
		ifTrue: [aString]
		ifFalse: [aString string]! !

!XMLString class methodsFor: 'instance creation' stamp: 'JAAyer 2/25/2010 01:48'!
string: aString
	^ self new string: aString! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/14/2010 12:03'!
checkAndExpandReference: parsingContext
	| referenceString nextChar |
	nextChar := streamReader peek.
	self isValidating
		ifFalse: [^ nil].

	nextChar == $&
		ifTrue: [
			streamReader next.
			streamReader peek == $#
				ifTrue: [^ streamReader pushStream: (ReadStream on: self nextCharReference asString)].
			referenceString := self nextLiteral.
			self expectNext: $;.
			self handleEntity: referenceString in: parsingContext ]
		ifFalse: [
			((nextChar == $%
				and: [self parsingMarkup])
				and: [parsingContext == #entityValue])
				ifTrue: [
					streamReader skipSeparators.
					referenceString := self nextLiteral.
					self handleEntity: referenceString in: parsingContext]].

	self expectMore.
	^ nextChar.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/11/2010 09:46'!
conditionalInclude: aKeyword
	aKeyword = 'INCLUDE'
		ifTrue: [^ true].
	aKeyword = 'IGNORE'
		ifTrue: [^ false].
	^ self conditionalInclude: (self parameterEntity: aKeyword) value! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
endParsingMarkup
	parsingMarkup := false! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/16/2010 16:39'!
entities
	^ entities ifNil: [entities := self initEntities]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/16/2010 12:45'!
entity: aReference
	self isValidating
		ifFalse: [^ DTDEntityDeclaration name: aReference value: ''].

	^ self entities
		at: aReference
		ifAbsentPut: [self parseError: 'Undefined entity ', aReference printString]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/11/2010 09:47'!
entity: aReference put: aValue
	"Only the first declaration of an entity is valid so if there is already
	one don't register the new value."
	self entities at: aReference ifAbsentPut: [aValue]! !

!XMLTokenizer methodsFor: 'errors' stamp: 'JAAyer 3/14/2010 12:02'!
errorExpected: expectedString
	self parseError: 'Expected ', expectedString! !

!XMLTokenizer methodsFor: 'errors' stamp: 'JAAyer 3/16/2010 12:49'!
errorExpected: anExpectedCharacterOrString butGot: aReceivedCharacterOrString
	| expectedString receivedString |

	expectedString := anExpectedCharacterOrString asString.	
	(receivedString := (aReceivedCharacterOrString ifNil: ['']) asString)
			ifEmpty: [receivedString := 'nothing'].

	self errorExpected: expectedString, ' but got ', receivedString.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 12:49'!
expectLiteral: anExpectedLiteral
	| nextLiteral |

	((nextLiteral := self nextLiteral) = anExpectedLiteral)
		ifFalse: [self errorExpected: anExpectedLiteral butGot: nextLiteral].
	^ nextLiteral.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 16:40'!
expectMore
	streamReader peek ifNil: [self errorExpected: 'more characters']! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 12:49'!
expectNext: aCharacter
	| nextChar |

	(nextChar := streamReader next) == aCharacter
		ifFalse: [
			self
				errorExpected: aCharacter
				butGot: nextChar].
	^ nextChar.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 12:46'!
expectQuote
	| nextChar expected |

	(((nextChar := streamReader next) == $")
		or: [nextChar == $'])
			ifFalse: [
				self
					errorExpected: 'quote character delimiter'
					butGot: nextChar].
	^ nextChar.! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/19/2010 14:19'!
externalEntities
	^ externalEntities ifNil: [externalEntities := Dictionary new]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/11/2010 09:48'!
externalEntity: aReference
	^ self entities at: aReference ifAbsentPut: ['']! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 11/13/2000 16:04'!
handleCData: aString
	self log: 'CData: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:37'!
handleComment: aString
	self log: 'Comment: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 18:27'!
handleEndDocument
	self log: 'End Doc '! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:38'!
handleEndTag: aString
	self log: 'End tag: ' , aString! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/11/2010 08:28'!
handleEntity: referenceString in: parsingContext 

	| entity entityValue |
	entity := self entity: referenceString.
	entityValue := entity valueForContext: parsingContext.
	(self class isCharEscape: entityValue)
		ifTrue: [entityValue := entity reference].
	streamReader pushStream: (ReadStream on: entityValue asString)! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:38'!
handlePCData: aString
	self log: 'PCData: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 12/11/2000 16:10'!
handlePI: piTarget data: piData
	self log: 'PI: ' , piTarget , ' data ' , piData! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 18:26'!
handleStartDocument
	self log: 'Start Doc'! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 11:39'!
handleStartTag: tagName attributes: attributes
	self log: 'Start tag: ' , tagName.
	attributes keysAndValuesDo: [:key :value |
		self log: key , '->' , value]! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'JAAyer 3/5/2010 04:11'!
handleStartTag: tagName attributes: attributes namespaces: namespaces
	self handleStartTag: tagName attributes: attributes! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'cwp 6/17/2003 21:08'!
handleWhitespace: aString
	self log: 'Whitespace: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'JAAyer 3/2/2010 22:54'!
handleXMLDecl: attributes
	attributes keysAndValuesDo: [:key :value |
		self log: key , '->' , value]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/23/2007 15:45'!
initEntities
	| ents |
	ents := Dictionary new.
	ents
		at: 'amp' put: (DTDEntityDeclaration name: 'amp' value: '&');
		at: 'quot' put: (DTDEntityDeclaration name: 'quot' value: '"');
		at: 'apos' put: (DTDEntityDeclaration name: 'apos' value: '''');
		at: 'gt' put: (DTDEntityDeclaration name: 'gt' value: '>');
		at: 'lt' put: (DTDEntityDeclaration name: 'lt' value: '<').
	^ents! !

!XMLTokenizer methodsFor: 'initialize' stamp: 'JAAyer 3/23/2010 18:11'!
initialize
	streamReader := XMLStreamReader new.
	streamWriter := XMLStreamWriter new.
	parsingMarkup := false.
	isValidating := false.! !

!XMLTokenizer methodsFor: 'testing' stamp: 'JAAyer 3/11/2010 06:44'!
isValidating
	^ isValidating! !

!XMLTokenizer methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 06:45'!
isValidating: aBoolean
	isValidating := aBoolean! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 12/7/2000 16:46'!
log: aString
	"Transcript show: aString; cr"! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/12/2010 14:20'!
nextAttributeInto: attributes namespaces: namespaces

	| attrName attrValue |
	attrName := self nextName.
	streamReader skipSeparators.
	self expectNext: $=.
	streamReader skipSeparators.
	attrValue := self nextAttributeValue.

	(self usesNamespaces
		and: [attrName beginsWith: 'xmlns'])
		ifTrue: [attrName size > 6
			ifTrue: [namespaces at: (attrName copyFrom: 7 to: attrName size) put: attrValue]
			ifFalse: [namespaces at: attrName put: attrValue]]
		ifFalse: [attributes at: attrName put: attrValue].! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/23/2010 18:09'!
nextAttributeValue
	| delimiterChar |

	delimiterChar := self expectQuote.
	^ streamWriter writeWith: [:writeStream |
		self nextPCDataDelimitedBy: delimiterChar putOn: writeStream.
		self expectNext: delimiterChar.
		writeStream stringContents]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 09:12'!
nextCDataContent
	| cdata |

	cdata := streamReader upToAll: ']]>'.
	self handleCData: cdata
! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/15/2010 04:36'!
nextCDataOrConditional

	| nextChar conditionalKeyword |
	"Skip ["
	streamReader next.
	streamReader skipSeparators.
	nextChar := streamReader peek.
	nextChar == $%
		ifTrue: [
			self checkAndExpandReference: (self parsingMarkup ifTrue: [#dtd] ifFalse: [#content]).
			conditionalKeyword := self nextLiteral.
			streamReader skipSeparators.
			self expectNext: $[.
			streamReader skipSeparators.
			^ self nextIncludeSection: (self conditionalInclude: conditionalKeyword)].

	self
		expectLiteral: 'CDATA';
		expectNext: $[;
		nextCDataContent.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'pb 6/10/2012 18:04'!
nextCharReference
	| base charValue |
	streamReader next == $#
		ifFalse: [self errorExpected: 'character reference'].
	streamReader peek == $x
		ifTrue: [
			streamReader next.
			base := 16]
		ifFalse: [base := 10].

	(charValue := streamReader readNumberBase: base)
		ifNil: [self errorExpected: 'number'].
	self expectNext: $;.
	self flag: #pbfix.
	"Cuis does not support"
	"
	^ Unicode value: charValue.
	"
	charValue < 255
		ifTrue: [ ^ Character value: charValue ]
		ifFalse: [ ^ $@ ].! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 14:37'!
nextComment
	"Skip first -"
	streamReader next.
	self expectNext: $-.
	self handleComment: (streamReader upToAll: '-->')! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'JAAyer 3/15/2010 04:32'!
nextDocType
	self expectLiteral: 'DOCTYPE'.
	self startParsingMarkup.
	^ self nextDocTypeDecl.! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'JAAyer 3/19/2010 18:33'!
nextDocTypeDecl
	| nextChar |
	streamReader skipSeparators.
	self nextLiteral.
	streamReader skipSeparators.
	streamReader peek == $[
		ifFalse: [[nextChar := streamReader peek.
				nextChar == $> or: [nextChar == $[ ]] whileFalse: [streamReader next]].
	streamReader peek == $[
		ifTrue: [
			streamReader next.
			[streamReader skipSeparators.
			streamReader peek == $]] whileFalse: [
				self checkAndExpandReference: #dtd.
				self nextMarkupToken].
			self expectNext: $]].
	streamReader skipSeparators.
	self expectNext: $>.

	self endParsingMarkup! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 09:08'!
nextEndTag
	| tagName |
	"Skip /"
	streamReader next.
	tagName := self nextName.
	streamReader skipSeparators.
	self expectNext: $>.
	self handleEndTag: tagName! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'JAAyer 3/11/2010 08:25'!
nextEntityDeclaration
	| entityName entityDef referenceClass reference |
	streamReader skipSeparators.
	referenceClass := streamReader peek == $%
		ifTrue: [
			streamReader next.
			streamReader skipSeparators.
			DTDParameterEntityDeclaration]
		ifFalse: [DTDEntityDeclaration].
	entityName := self nextLiteral.
	streamReader skipSeparators.
	entityDef := (streamReader peek == $" or: [streamReader peek == $'])
		ifTrue: [self nextEntityValue]
		ifFalse: [self nextExternalId].
	streamReader skipUpTo: $>.
	reference := referenceClass name: entityName value: entityDef.
	reference registerIn: self.
	^reference! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/19/2010 14:26'!
nextEntityValue
	| delimiterChar entityValueStream nextChar nextPeek referenceString entity entityValue |
	delimiterChar := self expectQuote.
	entityValueStream := WriteStream on: (String new: 32).
	[
	self expectMore.
	nextPeek := nextChar := streamReader peek.
	nextChar == $&
		ifTrue: [
			streamReader next.
			streamReader peek == $#
				ifTrue: [
					nextPeek := nil.
					nextChar := self nextCharReference]
				ifFalse: [
					referenceString := self nextLiteral.
					self expectNext: $;.
					entity := self entity: referenceString.
					entityValue := entity valueForContext: #entityValue.
					streamReader pushStream: (ReadStream on: entityValue asString).
					nextPeek := nextChar := streamReader next]]
		ifFalse: [
			nextChar == $%
				ifTrue: [
					streamReader skipSeparators.
					referenceString := self nextLiteral.
					nextChar := self handleEntity: referenceString in: #entityValue.
					nextPeek := nextChar := streamReader next]
				ifFalse: [streamReader next]].
	nextPeek == delimiterChar]
		whileFalse: [
			nextChar ifNotNil: [entityValueStream nextPut: nextChar]].
	^ entityValueStream stringContents.! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'JAAyer 3/19/2010 14:26'!
nextExternalId
	| extDefType systemId dir |
	extDefType := self nextLiteral.
	extDefType = 'PUBLIC'
		ifTrue: [
			streamReader skipSeparators.
			self nextPubidLiteral.
			streamReader skipSeparators.
			streamReader peek == $>
				ifFalse: [
					systemId := self nextSystemLiteral]].

	extDefType = 'SYSTEM'
		ifTrue: [
			streamReader skipSeparators.
			systemId := self nextSystemLiteral].

	systemId
		ifNil: [^nil].

	"The rest of this method only applies if we're reading aFileStream"
	(streamReader topStream isKindOf: FileStream)
		ifFalse: [^''].
	dir := streamReader topStream directory.
	^(dir fileExists: systemId)
		ifTrue: [(dir readOnlyFileNamed: systemId) stringContentsOfEntireFile]
		ifFalse: ['']! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 09:11'!
nextIncludeSection: parseSection
	| section |
	"Read the file up to the next include section delimiter and parse it if parseSection is true"

	section := streamReader upToAll: ']]>'.
	parseSection
		ifTrue: [streamReader pushStream: (ReadStream on: section)]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/24/2010 16:24'!
nextLiteral
	| nextChar |

	((nextChar := streamReader peek) isLetter
		or: [nextChar == $_])
		ifFalse: [self errorExpected: 'name literal.'].

	^ streamWriter writeWith: [:writeStream |
		[LiteralChars includes: nextChar]
			whileTrue: [
				nextChar == $&
					ifTrue: [
						nextChar := streamReader next.
						writeStream nextPut: (streamReader peek == $#
							ifTrue: [self nextCharReference]
							ifFalse: [^ writeStream stringContents])]
					ifFalse: [writeStream nextPut: streamReader next].
				nextChar := streamReader peek].
		writeStream position > 0
			ifFalse: [self errorExpected: 'name literal'].
		writeStream stringContents]! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'JAAyer 3/11/2010 09:57'!
nextMarkupDeclaration
	| declType |
	declType := self nextLiteral.
	self isValidating
		ifFalse: [^ self skipMarkupDeclaration].
	declType = 'ENTITY'
		ifTrue: [self nextEntityDeclaration]
		ifFalse: [self skipMarkupDeclaration]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/19/2010 18:34'!
nextMarkupToken
	| nextChar |
	"Skip < "
	streamReader next.
	nextChar := streamReader peek.
	nextChar == $!! ifTrue: [
		"Skip !!"
		streamReader next.
		nextChar := streamReader peek.
		nextChar == $- ifTrue: [^ self nextComment].
		nextChar == $[ ifTrue: [^ self nextCDataOrConditional].
		^ self parsingMarkup
			ifTrue: [self nextMarkupDeclaration]
			ifFalse: [self nextDocType]].
	nextChar == $? ifTrue: [^ self nextPI].
	self nextTag.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/24/2010 16:25'!
nextName
	| nextChar |

	^ streamWriter writeWith: [:writeStream |
		[(nextChar := streamReader peek) isNil
			or: [NameDelimiters includes: nextChar]]
				whileFalse: [writeStream nextPut: streamReader next].
		writeStream position > 0
			ifFalse: [self errorExpected: 'name'].
		writeStream stringContents]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/23/2010 18:04'!
nextPCData

	streamWriter writeWith: [:writeStream |
		self isValidating
			ifTrue: [self nextPCDataDelimitedBy: $< putOn: writeStream]
			ifFalse: [
				[streamReader peek == $<]
					whileFalse: [writeStream nextPut: streamReader next]].

		self handlePCData: writeStream stringContents]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 16:45'!
nextPCDataDelimitedBy: aDelimiter putOn: aStream
	| nextChar referenceString entity entityValue |

	[(nextChar := streamReader peek) isNil or: [nextChar == aDelimiter]]
		whileFalse: [
			nextChar == $&
				ifTrue: [
					streamReader next.
					streamReader peek == $#
						ifTrue: [aStream nextPut: self nextCharReference]
						ifFalse: [
							referenceString := self nextLiteral.
							self expectNext: $;.
							entity := self entity: referenceString.
							entityValue := entity valueForContext: #content.
							(self class isCharEscape: entityValue)
								ifTrue: [aStream nextPut: entityValue first]
								ifFalse: [
									entityValue := entityValue asString.
									entityValue isEmpty
										ifFalse: [
											streamReader pushStream:
												(ReadStream on: entityValue asString)]]]]
				ifFalse: [aStream nextPut: streamReader next]].! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/16/2010 09:11'!
nextPI
	| piTarget piData |
	"Skip ?"
	streamReader next.
	piTarget := self nextLiteral.
	piTarget asUppercase = 'XML'
		ifTrue: [^ self nextXMLDecl].
	streamReader skipSeparators.
	piData := streamReader upToAll: '?>'.
	self handlePI: piTarget data: piData.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/11/2010 09:56'!
nextPubidLiteral
	^ self nextAttributeValue! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/11/2010 09:56'!
nextSystemLiteral
	^ self nextAttributeValue! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/14/2010 12:37'!
nextTag
	| tagName attributes nextChar namespaces |
	(streamReader peek = $/)
		ifTrue: [^ self nextEndTag].
	tagName := self nextName.
	streamReader skipSeparators.
	attributes := XMLOrderPreservingDictionary new: 10.
	namespaces := XMLOrderPreservingDictionary new.
	[(nextChar := streamReader peek) == $> or: [nextChar == $/]] whileFalse: [
		self checkAndExpandReference: #content.
		self nextAttributeInto: attributes namespaces: namespaces.
		streamReader skipSeparators.].
	self handleStartTag: tagName attributes: attributes namespaces: namespaces.
	streamReader next == $/
		ifTrue: [
			self handleEndTag: tagName.
			self expectNext: $>].! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/19/2010 18:33'!
nextToken
	"return the next XMLnode, or nil if there are no more.
	Fixed to retain leading whitespace when PCDATA is detected."

	| whitespace |
	"branch, depending on what the first character is"
	whitespace := self nextWhitespace.
	streamReader atEnd
		ifTrue: [^ self handleEndDocument].

	self checkAndExpandReference: (self parsingMarkup ifTrue: [#dtd] ifFalse: [#content]).
	^ (streamReader peek = $<)
		ifTrue: [self nextMarkupToken]
		ifFalse: [
			whitespace ifNotEmpty: [streamReader pushBack: whitespace].
			self nextPCData].! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/11/2010 09:01'!
nextWhitespace
	| whitespace |

	(whitespace := streamReader nextWhitespace)
		ifNotEmpty: [self handleWhitespace: whitespace].
	^ whitespace.! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'JAAyer 3/14/2010 14:55'!
nextXMLDecl
	| attributes namespaces |
	streamReader skipSeparators.
	attributes := Dictionary new.
	namespaces := Dictionary new.
	[streamReader peek == $?]
		whileFalse: [
			self nextAttributeInto: attributes namespaces: namespaces.
			streamReader skipSeparators].
	streamReader next.
	self expectNext: $>.
	(attributes includesKey: 'encoding')
		ifTrue: [streamReader streamEncoding: (attributes at: 'encoding')].
	self handleXMLDecl: attributes.! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/14/2010 12:36'!
parameterEntities
	^ parameterEntities ifNil: [parameterEntities := Dictionary new]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/19/2010 14:17'!
parameterEntity: refName
	^ self parameterEntities
		at: refName
		ifAbsent: [self parseError: 'undefined parameter entity ', refName printString]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'JAAyer 3/11/2010 09:55'!
parameterEntity: refName put: aReference
	"Only the first declaration of an entity is valid so if there is already
	one don't register the new value."
	self parameterEntities at: refName ifAbsentPut: [aReference]! !

!XMLTokenizer methodsFor: 'errors' stamp: 'mir 1/8/2002 15:37'!
parseError: errorString
	SAXParseException signal: errorString! !

!XMLTokenizer methodsFor: 'private' stamp: 'JAAyer 3/11/2010 09:54'!
parsingMarkup
	^ parsingMarkup! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'JAAyer 3/11/2010 08:30'!
skipMarkupDeclaration
	streamReader skipUpTo: $>! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
startParsingMarkup
	parsingMarkup := true! !

!XMLTokenizer methodsFor: 'private' stamp: 'JAAyer 3/11/2010 08:04'!
stream
	^ streamReader stream! !

!XMLTokenizer methodsFor: 'private' stamp: 'JAAyer 3/11/2010 08:03'!
stream: aStream
	streamReader stream: aStream! !

!XMLTokenizer methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 09:03'!
streamReader
	^ streamReader! !

!XMLTokenizer methodsFor: 'testing' stamp: 'JAAyer 3/11/2010 09:54'!
usesNamespaces
	^ false! !

!XMLTokenizer class methodsFor: 'class initialization' stamp: 'JAAyer 3/11/2010 08:38'!
initialize
	"XMLTokenizer initialize"

	CharEscapes := CharacterSet newFrom: #( $& $" $' $> $< ).

	LiteralChars := CharacterSet newFrom: #( $: $- $: $= $.).
	0 to: 255 do: [:i | 
		| char |
		char := i asCharacter.
		(char isDigit or: [char isLetter])
		ifTrue: [LiteralChars add: char]].

	NameDelimiters := CharacterSet new.
	#(9 10 12 13 32 61 "$= asInteger 61" 62 "$> asInteger" 47 "$/ asInteger")
		do: [:each | NameDelimiters add: each asCharacter].! !

!XMLTokenizer class methodsFor: 'accessing' stamp: 'JAAyer 3/11/2010 07:53'!
isCharEscape: entityValue
	^entityValue size = 1
		and: [CharEscapes includes: entityValue first]! !

!XMLTokenizer class methodsFor: 'instance creation' stamp: 'JAAyer 3/8/2010 10:42'!
on: aStream
	^self new stream: aStream! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 11:04'!
attribute: attributeName value: attributeValue
	self stream
		space;
		nextPutAll: attributeName.
	self
		eq;
		putAsXMLString: attributeValue! !

!XMLWriter methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 16:21'!
canonical
	^ canonical ifNil: [canonical := false]! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:54'!
canonical: aBoolean
	canonical := aBoolean! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/8/2000 17:55'!
cdata: aString
	self startCData.
	self stream nextPutAll: aString.
	self endCData! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/8/2000 17:56'!
comment: aString
	self startComment.
	self stream nextPutAll: aString.
	self endComment! !

!XMLWriter methodsFor: 'private' stamp: 'GeorgeHerolyants 1/10/2010 18:53'!
cr
	self newLinesAllowed ifTrue: [self stream cr].! !

!XMLWriter methodsFor: 'private tags' stamp: 'GeorgeHerolyants 1/8/2010 19:50'!
endCData
	self stream nextPutAll: ']]>'.
	self cr.! !

!XMLWriter methodsFor: 'private tags' stamp: 'GeorgeHerolyants 1/8/2010 19:50'!
endComment
	self stream nextPutAll: ' -->'.
	self cr.! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 8/8/2000 18:13'!
endDecl: type
	self endTag! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 12/8/2000 18:02'!
endDeclaration
	self stream
		cr;
		nextPut: $].
	self endTag! !

!XMLWriter methodsFor: 'writing xml' stamp: 'JAAyer 3/4/2010 05:18'!
endElement: anElementName
	self nestedScopes leaveScope.
	self endTag: anElementName.! !

!XMLWriter methodsFor: 'writing xml' stamp: 'JAAyer 3/16/2010 16:18'!
endEmptyTag: aTagName
	self openTags closeTag: aTagName.
	self stream nextPutAll: '/>'.
	self canonical
		ifFalse: [self stream space].
	self cr.! !

!XMLWriter methodsFor: 'private tags' stamp: 'GeorgeHerolyants 1/8/2010 19:50'!
endPI
	self stream nextPutAll: ' ?>'.
	self cr.! !

!XMLWriter methodsFor: 'writing xml' stamp: 'GeorgeHerolyants 1/8/2010 19:48'!
endTag
	self stream nextPutAll: '>'.
	self cr.
	self indent.
	"self canonical
		ifFalse: [self stream space]"! !

!XMLWriter methodsFor: 'writing xml' stamp: 'JAAyer 3/16/2010 16:19'!
endTag: aTagName
	self outdent.
	self openTags closeTag: aTagName.
	self stream
		nextPutAll: '</';
		nextPutAll: aTagName.
	self endTag.
! !

!XMLWriter methodsFor: 'private' stamp: 'mir 12/8/2000 18:00'!
eq
	self stream nextPut: $=! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 11:13'!
flush
	self stream flush! !

!XMLWriter methodsFor: 'private' stamp: 'mir 7/12/2006 16:52'!
indent
	currentIndent
		ifNotNil: [currentIndent := currentIndent +1]! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 7/12/2006 16:08'!
indentString: aString
	currentIndent := 0.
	indentString := aString! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 7/12/2006 16:08'!
indentTab
	self indentString: (String with: Character tab)! !

!XMLWriter methodsFor: 'private' stamp: 'JAAyer 3/16/2010 16:20'!
nestedScopes
	^ nestedScopes ifNil: [nestedScopes := XMLNestedNamespaceScopes new]! !

!XMLWriter methodsFor: 'accessing' stamp: 'JAAyer 3/16/2010 16:21'!
newLinesAllowed
	^ newLinesAllowed ifNil: [newLinesAllowed := false]! !

!XMLWriter methodsFor: 'accessing' stamp: 'GeorgeHerolyants 1/10/2010 18:51'!
newLinesAllowed: aBoolean
	newLinesAllowed := aBoolean! !

!XMLWriter methodsFor: 'private' stamp: 'JAAyer 3/16/2010 16:20'!
openTags
	^ openTags ifNil: [openTags := XMLOpenTags new]! !

!XMLWriter methodsFor: 'private' stamp: 'GeorgeHerolyants 1/10/2010 18:53'!
outdent
	currentIndent
		ifNotNil: [
			currentIndent := currentIndent-1.
			self writeIndent.
			currentIndent := currentIndent-1.]! !

!XMLWriter methodsFor: 'writing xml' stamp: 'GeorgeHerolyants 1/8/2010 20:08'!
pcData: aString
	self writeIndent.
	self writeXmlEncoded: aString.
	self cr.! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/11/2000 16:12'!
pi: piTarget data: piData
	self startPI: piTarget.
	self stream nextPutAll: piData.
	self endPI! !

!XMLWriter methodsFor: 'accessing' stamp: 'GeorgeHerolyants 1/10/2010 19:20'!
prettyPrint
	self indentString: '    '.
	self newLinesAllowed: true.! !

!XMLWriter methodsFor: 'private' stamp: 'pb 6/10/2012 18:11'!
putAsXMLString: aValue
	self stream nextPut: $".
	self flag: #pbfix.
	"handling nil"
	"self writeXmlEncoded: aValue."
	aValue ifNil: [self writeXmlEncoded: '']
		ifNotNil: [self writeXmlEncoded: aValue].
	self stream nextPut: $"! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
startCData
	self stream nextPutAll: '<!![CDATA['! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
startComment
	self stream nextPutAll: '<-- '! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 12/8/2000 18:02'!
startDecl: type
	self stream
		nextPutAll: '<!!';
		nextPutAll: type asUppercase;
		space! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 12/8/2000 18:02'!
startDecl: type named: aString
	self stream
		nextPutAll: '<!!';
		nextPutAll: type asUppercase;
		space;
		nextPutAll: aString;
		space! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 12/8/2000 18:02'!
startDeclaration: dtdName
	self startDecl: 'DOCTYPE' named: dtdName.
	self stream
		nextPut: $[;
		cr! !

!XMLWriter methodsFor: 'writing xml' stamp: 'JAAyer 3/8/2010 10:22'!
startElement: elementName attributeList: attributeList
	"self canonical
		ifFalse: [self stream cr]."
	self startTag: elementName.
	attributeList printXMLOn: self.! !

!XMLWriter methodsFor: 'writing xml' stamp: 'JAAyer 3/16/2010 16:23'!
startElement: anElementName attributeList: anAttributeList namespaces: aNamespaceDictionary
	self startTag: anElementName.
	self nestedScopes enterScope.

	aNamespaceDictionary keysAndValuesDo: [:name :uri |
		name splitQualifiedNameInto: [:xmlns :prefix |
			(prefix = 'xmlns')
				ifTrue: [
					(self nestedScopes defaultNamespace = uri)
						ifFalse: [self xmlns: 'xmlns' uri: uri]]
				ifFalse: [
					(self nestedScopes isPrefix: prefix mappedTo: uri)
						ifFalse: [self xmlns: prefix uri: uri]]]].

	anAttributeList printXMLOn: self.! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
startPI: identifier
	self stream
		nextPutAll: '<?';
		nextPutAll: identifier;
		space! !

!XMLWriter methodsFor: 'writing xml' stamp: 'JAAyer 3/16/2010 16:18'!
startTag: aTagName
	self writeIndent.
	self stream
		nextPut: $<;
		nextPutAll: aTagName.
	"self canonical
		ifFalse: [self stream space]."
	self openTags openTag: aTagName! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:54'!
stream
	^stream! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:54'!
stream: aStream
	stream := aStream! !

!XMLWriter methodsFor: 'private' stamp: 'mir 7/12/2006 16:58'!
writeIndent
	currentIndent ifNotNil: [
		currentIndent timesRepeat: [self stream nextPutAll: indentString]]! !

!XMLWriter methodsFor: 'private' stamp: 'GeorgeHerolyants 1/8/2010 20:07'!
writeXmlEncoded: aString
	| lastIndex nextIndex |
	lastIndex := 1.
	"Unroll the first search to avoid copying"
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0 ifTrue:[self stream nextPutAll: aString. ^ self].
	[self stream nextPutAll: (aString copyFrom: lastIndex to: nextIndex-1).
	self stream nextPutAll: (XMLTranslation at: (aString at: nextIndex)).
	lastIndex := nextIndex + 1.
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0] whileFalse.
	self stream nextPutAll: (aString copyFrom: lastIndex to: aString size).! !

!XMLWriter methodsFor: 'writing xml' stamp: 'thf 9/2/2005 14:08'!
xmlDeclaration: versionString encoding: encodingString
	self canonical
		ifFalse: [
			self
				startPI: 'xml';
				attribute: 'version' value: versionString;
				attribute: 'encoding' value: encodingString;
				endPI.
			self stream flush]! !

!XMLWriter methodsFor: 'writing xml' stamp: 'JAAyer 3/16/2010 16:24'!
xmlns: aNamespace uri: aUri
	self nestedScopes declareNamespace: aNamespace uri: aUri.

	self
		attribute:
			(aNamespace = 'xmlns'
				ifTrue: ['xmlns']
				ifFalse: ['xmlns:', aNamespace])
		value: aUri! !

!XMLWriter class methodsFor: 'class initialization' stamp: 'pb 6/10/2012 17:56'!
initialize
	"XMLWriter initialize"

	XMLTranslation := Dictionary new.
	XMLTranslation
		at: Character crCharacter put: '&#13;';
		at: Character lfCharacter put: '&#10;';
		at: Character tab put: '&#9;';
		at: $& put: '&amp;';
		at: $< put: '&lt;';
		at: $> put: '&gt;';
"		at: $' put: '&apos;'; "
		at: $" put: '&quot;'.
	XMLTranslationMap := ByteArray new: 256.
	XMLTranslation keysDo:[:ch| XMLTranslationMap at: ch asciiValue+1 put: 1].
! !

!XMLWriter class methodsFor: 'instance creation' stamp: 'mir 12/8/2000 17:54'!
on: aStream
	^self basicNew initialize stream: aStream! !
DTDEntityDeclaration initialize!
DTDExternalEntityDeclaration initialize!
DTDParameterEntityDeclaration initialize!
XMLStreamReader initialize!
XMLTokenizer initialize!
XMLWriter initialize!
