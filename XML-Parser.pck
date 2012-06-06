'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 5 June 2012 at 8:22:08 pm'!
'Description Based on XML-Parser-AlexandreBergel.15.mcz (pbfix - need to confirm)

Work in progress'!
!classDefinition: #DTDEntityDeclaration category: #'XML-Parser'!
Object subclass: #DTDEntityDeclaration
	instanceVariableNames: 'name value ndata'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'DTDEntityDeclaration class' category: #'XML-Parser'!
DTDEntityDeclaration class
	instanceVariableNames: 'contextBehavior'!

!classDefinition: #DTDExternalEntityDeclaration category: #'XML-Parser'!
DTDEntityDeclaration subclass: #DTDExternalEntityDeclaration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'DTDExternalEntityDeclaration class' category: #'XML-Parser'!
DTDExternalEntityDeclaration class
	instanceVariableNames: ''!

!classDefinition: #DTDParameterEntityDeclaration category: #'XML-Parser'!
DTDEntityDeclaration subclass: #DTDParameterEntityDeclaration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'DTDParameterEntityDeclaration class' category: #'XML-Parser'!
DTDParameterEntityDeclaration class
	instanceVariableNames: ''!

!classDefinition: #SAXException category: #'XML-Parser'!
Error subclass: #SAXException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'SAXException class' category: #'XML-Parser'!
SAXException class
	instanceVariableNames: ''!

!classDefinition: #SAXHandler category: #'XML-Parser'!
Object subclass: #SAXHandler
	instanceVariableNames: 'document driver eod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'SAXHandler class' category: #'XML-Parser'!
SAXHandler class
	instanceVariableNames: ''!

!classDefinition: #SAXMalformedException category: #'XML-Parser'!
SAXException subclass: #SAXMalformedException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'SAXMalformedException class' category: #'XML-Parser'!
SAXMalformedException class
	instanceVariableNames: ''!

!classDefinition: #SAXParseException category: #'XML-Parser'!
SAXException subclass: #SAXParseException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'SAXParseException class' category: #'XML-Parser'!
SAXParseException class
	instanceVariableNames: ''!

!classDefinition: #SAXWarning category: #'XML-Parser'!
Warning subclass: #SAXWarning
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'SAXWarning class' category: #'XML-Parser'!
SAXWarning class
	instanceVariableNames: ''!

!classDefinition: #XMLDOMParser category: #'XML-Parser'!
SAXHandler subclass: #XMLDOMParser
	instanceVariableNames: 'entity stack incremental'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLDOMParser class' category: #'XML-Parser'!
XMLDOMParser class
	instanceVariableNames: ''!

!classDefinition: #XMLException category: #'XML-Parser'!
Error subclass: #XMLException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLException class' category: #'XML-Parser'!
XMLException class
	instanceVariableNames: ''!

!classDefinition: #XMLInvalidException category: #'XML-Parser'!
XMLException subclass: #XMLInvalidException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLInvalidException class' category: #'XML-Parser'!
XMLInvalidException class
	instanceVariableNames: ''!

!classDefinition: #XMLMalformedException category: #'XML-Parser'!
XMLException subclass: #XMLMalformedException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLMalformedException class' category: #'XML-Parser'!
XMLMalformedException class
	instanceVariableNames: ''!

!classDefinition: #XMLNamespaceScope category: #'XML-Parser'!
Object subclass: #XMLNamespaceScope
	instanceVariableNames: 'scope currentBindings useNamespaces validateAttributes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLNamespaceScope class' category: #'XML-Parser'!
XMLNamespaceScope class
	instanceVariableNames: ''!

!classDefinition: #XMLNode category: #'XML-Parser'!
Object subclass: #XMLNode
	instanceVariableNames: 'parent'
	classVariableNames: 'CanonicalTable'
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLNode class' category: #'XML-Parser'!
XMLNode class
	instanceVariableNames: ''!

!classDefinition: #XMLNodeWithElements category: #'XML-Parser'!
XMLNode subclass: #XMLNodeWithElements
	instanceVariableNames: 'elementsAndContents uri namespace'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLNodeWithElements class' category: #'XML-Parser'!
XMLNodeWithElements class
	instanceVariableNames: ''!

!classDefinition: #XMLDocument category: #'XML-Parser'!
XMLNodeWithElements subclass: #XMLDocument
	instanceVariableNames: 'dtd version encoding requiredMarkup'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLDocument class' category: #'XML-Parser'!
XMLDocument class
	instanceVariableNames: ''!

!classDefinition: #XMLElement category: #'XML-Parser'!
XMLNodeWithElements subclass: #XMLElement
	instanceVariableNames: 'name attributes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLElement class' category: #'XML-Parser'!
XMLElement class
	instanceVariableNames: ''!

!classDefinition: #XMLPI category: #'XML-Parser'!
XMLNode subclass: #XMLPI
	instanceVariableNames: 'target data'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLPI class' category: #'XML-Parser'!
XMLPI class
	instanceVariableNames: ''!

!classDefinition: #XMLStringNode category: #'XML-Parser'!
XMLNode subclass: #XMLStringNode
	instanceVariableNames: 'string'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLStringNode class' category: #'XML-Parser'!
XMLStringNode class
	instanceVariableNames: ''!

!classDefinition: #XMLTokenizer category: #'XML-Parser'!
Object subclass: #XMLTokenizer
	instanceVariableNames: 'stream nestedStreams entities externalEntities parameterEntities parsingMarkup markedPosition peekChar validating nameBuffer attributeBuffer'
	classVariableNames: 'CharEscapes DigitTable LiteralChars NameDelimiters SeparatorTable'
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLTokenizer class' category: #'XML-Parser'!
XMLTokenizer class
	instanceVariableNames: ''!

!classDefinition: #SAXDriver category: #'XML-Parser'!
XMLTokenizer subclass: #SAXDriver
	instanceVariableNames: 'saxHandler scope useNamespaces validateAttributes languageEnvironment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'SAXDriver class' category: #'XML-Parser'!
SAXDriver class
	instanceVariableNames: ''!

!classDefinition: #XMLParser category: #'XML-Parser'!
XMLTokenizer subclass: #XMLParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLParser class' category: #'XML-Parser'!
XMLParser class
	instanceVariableNames: ''!

!classDefinition: #XMLWarningException category: #'XML-Parser'!
XMLException subclass: #XMLWarningException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLWarningException class' category: #'XML-Parser'!
XMLWarningException class
	instanceVariableNames: ''!

!classDefinition: #XMLWriter category: #'XML-Parser'!
Object subclass: #XMLWriter
	instanceVariableNames: 'stream stack scope scanner canonical currentIndent indentString'
	classVariableNames: 'XMLTranslation XMLTranslationMap'
	poolDictionaries: ''
	category: 'XML-Parser'!
!classDefinition: 'XMLWriter class' category: #'XML-Parser'!
XMLWriter class
	instanceVariableNames: ''!


!XMLTokenizer commentStamp: '<historical>' prior: 0!
XMLTokenizer

bolot@cc.gatech.edu

breaks the stream of characters into a stream of XMLnodes (aka token stream)
token stream is used by XMLparser to generate XMLdocument tree!

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

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 1/17/2002 15:25'!
name: aString
	name := aString asSymbol! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:22'!
ndata
	^ndata! !

!DTDEntityDeclaration methodsFor: 'accessing' stamp: 'mir 12/8/2000 17:22'!
ndata: aString
	ndata := aString! !

!DTDEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 11:30'!
reference
	"Return my reference as is."
	^self class leadIn , self name , ';'! !

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

!DTDParameterEntityDeclaration methodsFor: 'behaviors' stamp: 'mir 1/15/2002 23:21'!
notRecognized
	SAXMalformedException signal: 'Malformed entity.'! !

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

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 1/16/2002 00:33'!
handleCData: aString
	self saxHandler
		checkEOD; 
		characters: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'cwp 6/17/2003 18:26'!
handleComment: aString
	self saxHandler
		checkEOD; 
		comment: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 1/8/2002 18:38'!
handleEndDocument
	self saxHandler endDocument! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 6/24/2003 13:36'!
handleEndTag: elementName
	| namespace localName namespaceURI qualifiedName |

	self usesNamespaces
		ifTrue: [
			self splitName: elementName into: [:ns :ln |
				namespace := ns.
				localName := ln].

			"ensure our namespace is defined"
			namespace
				ifNil: [
					namespace := self scope defaultNamespace.
					qualifiedName := namespace , ':' , elementName]
				ifNotNil: [
					namespaceURI := self scope namespaceURIOf: namespace.
					namespaceURI
						ifNil: [self parseError: 'Start tag ' , elementName , ' refers to undefined namespace ' , namespace asString].
					qualifiedName := elementName].

			"call the handler"
			self saxHandler
				checkEOD; 
				endElement: localName namespace: namespace namespaceURI: namespaceURI qualifiedName: qualifiedName.
			self scope leaveScope]
		ifFalse: [
			"call the handler"
			self saxHandler
				checkEOD; 
				endElement: elementName namespace: nil namespaceURI: nil qualifiedName: elementName]! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 7/14/2006 11:55'!
handlePCData: aString
	self languageEnvironment
		ifNotNil: [aString applyLanguageInfomation: self languageEnvironment].
	self saxHandler
		checkEOD; 
		characters: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 1/8/2002 18:24'!
handlePI: piTarget data: piData
	self saxHandler
		checkEOD; 
		processingInstruction: piTarget data: piData! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 8/14/2000 18:29'!
handleStartDocument
	self saxHandler startDocument! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'pb 6/4/2012 16:32'!
handleStartTag: elementName attributes: attributeList namespaces: namespaces
	| localName namespace namespaceURI |
	self flag: #pbfix.
	"no language/locale support in cuis"
	"(attributeList includesKey: 'xml:lang')
		ifTrue: [languageEnvironment := LanguageEnvironment localeID: (LocaleID isoString: (attributeList at: 'xml:lang'))]."
	self usesNamespaces
		ifTrue: [
			self scope enterScope.
			"declare any namespaces"
			namespaces keysAndValuesDo: [ :ns :uri |
				self scope
					declareNamespace: ns
					uri: uri ].
			self
				splitName: elementName
				into: [ :ns :ln |
					namespace := ns.
					localName := ln ].
			"ensure our namespace is defined"
			namespace
				ifNil: [ namespace := self scope defaultNamespace ]
				ifNotNil: [
					namespaceURI := self scope namespaceURIOf: namespace.
					namespaceURI ifNil: [ self parseError: 'Start tag ' , elementName , ' refers to undefined namespace ' , namespace asString ]].
			self validatesAttributes ifTrue: [ self scope validateAttributes: attributeList ].
			"call the handler"
			self saxHandler
				 checkEOD;
				
				startElement: localName
				namespaceURI: namespaceURI
				namespace: namespace
				attributeList: attributeList ]
		ifFalse: [
			"call the handler"
			self saxHandler
				 checkEOD;
				
				startElement: elementName
				namespaceURI: nil
				namespace: nil
				attributeList: attributeList ].! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'cwp 6/18/2003 01:00'!
handleWhitespace: aString
	self saxHandler
		checkEOD; 
		ignorableWhitespace: aString! !

!SAXDriver methodsFor: 'handling tokens' stamp: 'mir 6/24/2003 13:39'!
handleXMLDecl: attributes namespaces: namespaces
	self saxHandler
		checkEOD; 
		documentAttributes: attributes.
	self usesNamespaces
		ifTrue: [
			namespaces keysAndValuesDo: [:ns :uri |
				self scope declareNamespace: ns uri: uri]]! !

!SAXDriver methodsFor: 'initialization' stamp: 'mir 6/5/2003 16:29'!
initialize
	super initialize.
	useNamespaces := false.
	validateAttributes := false! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 7/14/2006 11:48'!
languageEnvironment
	^languageEnvironment! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 8/11/2000 17:51'!
saxHandler
	^saxHandler! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 8/11/2000 17:52'!
saxHandler: aHandler
	saxHandler := aHandler! !

!SAXDriver methodsFor: 'namespaces' stamp: 'mir 6/24/2003 13:40'!
scope
	scope ifNil: [scope := XMLNamespaceScope new].
	^scope! !

!SAXDriver methodsFor: 'namespaces' stamp: 'mir 6/16/2003 13:09'!
splitName: aName into: twoArgsBlock
	"Split the name into namespace and local name (the block arguments).
	Handle both qualified and unqualified names using the default name space"

	| i ns ln |
	i := aName lastIndexOf: $:.
	i = 0
		ifTrue: [
			ns := nil.
			ln := aName]
		ifFalse: [
			ns := aName copyFrom: 1 to: (i - 1).
			ln := aName copyFrom: i+1 to: aName size].
	twoArgsBlock value: ns value: ln! !

!SAXDriver methodsFor: 'accessing' stamp: 'mir 6/24/2003 14:51'!
useNamespaces: aBoolean
	useNamespaces := aBoolean! !

!SAXDriver methodsFor: 'testing' stamp: 'mir 6/5/2003 16:30'!
usesNamespaces
	^useNamespaces! !

!SAXDriver methodsFor: 'testing' stamp: 'mir 6/5/2003 17:06'!
validatesAttributes
	^validateAttributes! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:27'!
characters: aString
	"This call corresponds to the Java SAX call
	characters(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:24'!
checkEOD
	"Check if the document shouldn't be ended already"
	self eod
		ifTrue: [self driver errorExpected: 'No more data expected,']! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:52'!
comment: commentString
	"This call corresponds to the Java SAX ext call
	comment(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 11/30/2000 18:12'!
document
	^document! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 11/30/2000 18:12'!
document: aDocument
	document := aDocument! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/17/2002 13:12'!
documentAttributes: attributeList! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:34'!
driver
	^driver! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:34'!
driver: aDriver
	driver := aDriver.
	driver saxHandler: self! !

!SAXHandler methodsFor: 'content' stamp: 'mir 1/8/2002 18:26'!
endDocument
	"This call corresponds to the Java SAX call
	endDocument()."
	eod := true! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/14/2000 18:07'!
endElement: elementName
! !

!SAXHandler methodsFor: 'content' stamp: 'mir 6/5/2003 19:05'!
endElement: elementName namespace: namespace namespaceURI: namespaceURI qualifiedName: qualifiedName
	"This call corresponds to the Java SAX call
	endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName).
	By default this call is mapped to the following more convenient call:"

	self endElement: elementName! !

!SAXHandler methodsFor: 'lexical' stamp: 'mir 8/11/2000 18:53'!
endEntity: entityName
	"This call corresponds to the Java SAX ext call
	endEntity(java.lang.String name)."! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:25'!
endPrefixMapping: prefix
	"This call corresonds to the Java SAX call
	endPrefixMapping(java.lang.String prefix)."! !

!SAXHandler methodsFor: 'accessing' stamp: 'mir 1/8/2002 18:18'!
eod
	^eod! !

!SAXHandler methodsFor: 'content' stamp: 'mir 8/11/2000 16:25'!
ignorableWhitespace: aString
	"This call corresonds to the Java SAX call
	ignorableWhitespace(char[] ch, int start, int length)."! !

!SAXHandler methodsFor: 'initialize' stamp: 'mir 1/8/2002 18:18'!
initialize
	eod := false! !

!SAXHandler methodsFor: 'parsing' stamp: 'mir 1/8/2002 18:18'!
parseDocument
	[self driver nextEntity isNil or: [self eod]] whileFalse! !

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

!SAXHandler methodsFor: 'content' stamp: 'mir 8/14/2000 18:07'!
startElement: elementName attributeList: attributeList
! !

!SAXHandler methodsFor: 'content' stamp: 'mir 6/5/2003 16:50'!
startElement: localName namespaceURI: namespaceUri namespace: namespace attributeList: attributeList
	"This call corresonds to the Java SAX call
	startElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName, Attributes atts).
	By default this call is mapped to the following more convenient call:"

	self startElement: localName attributeList: attributeList! !

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

!SAXHandler class methodsFor: 'instance creation' stamp: 'sd 5/23/2003 15:19'!
on: aStream
	| driver parser |
	driver := SAXDriver on: aStream.
	driver validating: true.
	parser := self new driver: driver.
	^parser! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'cwp 5/28/2003 02:08'!
parseDTDFrom: aStream
	| driver parser |
	driver := SAXDriver on: aStream.
	driver validating: true.
	driver startParsingMarkup.
	parser := self new driver: driver.
	parser startDocument.
	parser parseDocument.
	^parser! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 7/12/2006 16:24'!
parseDocumentFrom: aStream
	^self parseDocumentFrom: aStream useNamespaces: false! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 7/12/2006 16:26'!
parseDocumentFrom: aStream useNamespaces: aBoolean
	|  parser |
	parser := self on: aStream.
	parser useNamespaces: aBoolean.
	parser startDocument.
	parser parseDocument.
	^parser! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 1/8/2002 15:55'!
parseDocumentFromFileNamed: fileName
	^self parseDocumentFromFileNamed: fileName readIntoMemory: false! !

!SAXHandler class methodsFor: 'instance creation' stamp: 'mir 1/8/2002 15:55'!
parseDocumentFromFileNamed: fileName readIntoMemory: readIntoMemory
	| stream xmlDoc |
	stream := FileDirectory default readOnlyFileNamed: fileName.
	readIntoMemory
		ifTrue: [stream := stream contentsOfEntireFile readStream].
	[xmlDoc := self parseDocumentFrom: stream]
		ensure: [stream close].
	^xmlDoc! !

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

!XMLDOMParser methodsFor: 'private' stamp: 'mir 6/16/2003 17:20'!
defaultNamespace
	^self top
		ifNotNil: [self top namespace]! !

!XMLDOMParser methodsFor: 'content' stamp: 'thf 9/2/2005 14:09'!
documentAttributes: attributeList
	self document version: (attributeList at: 'version' ifAbsent: [nil]).
	self document encoding: (attributeList at: 'encoding' ifAbsent: [nil]).
	self document requiredMarkup: (attributeList at: 'requiredMarkup' ifAbsent: [nil]).
! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'mir 6/5/2003 19:29'!
domDocument
	[self startDocument; parseDocument]
		ensure: [self driver stream close].
	^document! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 1/8/2002 18:19'!
endDocument
	self pop.
	super endDocument! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 1/8/2002 18:11'!
endElement: elementName
	| currentElement |
	currentElement := self pop.
	currentElement name = elementName
		ifFalse: [self driver errorExpected: 'End tag "', elementName , '" doesn''t match "' , currentElement name , '".']! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 6/24/2003 14:53'!
endElement: localName namespace: namespace namespaceURI: uri qualifiedName: qualifiedName
	| currentElement |
	currentElement := self pop.
	(currentElement namespace isNil
		or: [currentElement namespace = self defaultNamespace])
		ifTrue: [
			currentElement localName = localName
				ifFalse: [self driver errorExpected: 'End tag "', localName , '" doesn''t match "' , currentElement localName  , '".']]
		ifFalse: [
			currentElement qualifiedName = qualifiedName
				ifFalse: [self driver errorExpected: 'End tag "', qualifiedName , '" doesn''t match "' , currentElement qualifiedName  , '".']]! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'mir 1/8/2001 12:05'!
incremental
	^incremental! !

!XMLDOMParser methodsFor: 'accessing' stamp: 'mir 1/8/2001 12:05'!
incremental: aBoolean
	incremental := aBoolean! !

!XMLDOMParser methodsFor: 'initialize' stamp: 'mir 1/8/2001 12:05'!
initialize
	super initialize.
	stack := OrderedCollection new.
	incremental := false! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'mir 6/28/2001 18:45'!
nextEntity
	| currentTop |
	currentTop := self top.
	[self driver nextEntity isNil
		or: [self top ~~ currentTop]] whileTrue.
	^entity! !

!XMLDOMParser methodsFor: 'parsing' stamp: 'mir 12/21/2000 14:02'!
nextEntityStart
	[self driver nextEntity.
	self stack isEmpty] whileTrue.
	^entity! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 1/8/2001 12:04'!
pop
	| oldTop |
	oldTop := self stack removeLast.
	entity := oldTop.
	^oldTop! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 1/8/2001 12:02'!
push: anObject
	self stack add: anObject.
	entity := anObject
! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 8/14/2000 18:28'!
stack
	^stack! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 11/30/2000 18:14'!
startDocument
	self document: XMLDocument new.
	self push: self document ! !

!XMLDOMParser methodsFor: 'content' stamp: 'mir 6/24/2003 18:52'!
startElement: localName namespaceURI: namespaceUri namespace: namespace attributeList: attributeList
	| newElement |
	"newElement := namespace = self defaultNamespace
		ifTrue: [XMLElement named: localName namespace: nil uri: nil attributes: attributeList]
		ifFalse: [XMLElement named: localName namespace: namespace uri: namespaceUri attributes: attributeList]."
	newElement := XMLElement named: localName namespace: namespace uri: namespaceUri attributes: attributeList.
	self incremental
		ifFalse: [self stack isEmpty
			ifFalse: [self top addElement: newElement]].
	self push: newElement! !

!XMLDOMParser methodsFor: 'private' stamp: 'mir 1/8/2001 11:46'!
top
	^self stack isEmpty
		ifTrue: [nil]
		ifFalse: [self stack last]! !

!XMLDOMParser class methodsFor: 'examples' stamp: 'mir 7/12/2006 16:26'!
addressBookXMLWithDTD
	"XMLDOMParser addressBookXMLWithDTD"
	^self parseDocumentFrom: XMLTokenizer addressBookXMLWithDTD readStream useNamespaces: true! !

!XMLDOMParser class methodsFor: 'instance creation' stamp: 'mir 7/12/2006 16:25'!
parseDocumentFrom: aStream
	^self parseDocumentFrom: aStream useNamespaces: false! !

!XMLDOMParser class methodsFor: 'instance creation' stamp: 'mir 7/12/2006 16:25'!
parseDocumentFrom: aStream useNamespaces: aBoolean
	^(super parseDocumentFrom: aStream useNamespaces: aBoolean) document! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 11/30/2000 17:48'!
dtd
	^dtd! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 11/30/2000 17:48'!
dtd: aDTD
	dtd := aDTD! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 5/16/2003 14:09'!
encoding	
	^encoding ifNil: ['UTF-8']! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
encoding: aString	
	encoding := aString! !

!XMLDocument methodsFor: 'printing' stamp: 'mir 1/17/2002 16:44'!
printCanonicalOn: aStream

	| writer |
	writer := XMLWriter on: aStream.
	writer canonical: true.
	self printXMLOn: writer! !

!XMLDocument methodsFor: 'printing' stamp: 'mir 5/16/2003 14:08'!
printXMLOn: writer
	version ifNotNil: [writer xmlDeclaration: self version encoding: self encoding].
	super printXMLOn: writer! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
requiredMarkup	
	^requiredMarkup! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
requiredMarkup: aString	
	requiredMarkup := aString! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 11/29/2007 14:42'!
root
	"return my root element"
	^ self topElement ! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
version	
	^version! !

!XMLDocument methodsFor: 'accessing' stamp: 'mir 1/17/2002 12:57'!
version: aString	
	version := aString! !

!XMLElement methodsFor: 'initialize' stamp: 'mir 10/31/2007 17:55'!
addContent: contentString
	self addElement: contentString! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/18/2001 16:55'!
attributeAt: attributeName
	^self attributeAt: attributeName ifAbsent: [nil]! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/18/2001 16:55'!
attributeAt: attributeName ifAbsent: aBlock
	^self attributes at: attributeName ifAbsent: [^aBlock value]! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/17/2002 15:24'!
attributeAt: attributeName put: attributeValue
	self attributes at: attributeName asSymbol put: attributeValue! !

!XMLElement methodsFor: 'accessing' stamp: 'mas 10/15/2003 09:18'!
attributes
	^attributes ifNil: [attributes := Dictionary new]! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 3/7/2000 16:24'!
characterData
	^self contentString! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 10/31/2007 18:25'!
contentString
	| contentElements |
	contentElements := self elementsAndContents.
	^(contentElements size == 1
		and: [contentElements first isText])
		ifTrue: [contentElements first string]
		ifFalse: ['']! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 6/18/2003 13:47'!
contentStringAt: entityName
	^(self elementAt: entityName ifAbsent: [^'']) contentString! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 10/31/2007 18:24'!
contents
	^self elementsAndContents select: [:each | each isText]! !

!XMLElement methodsFor: 'enumerating' stamp: 'mir 10/31/2007 18:25'!
contentsDo: aBlock
	self elementsAndContentsDo: [:each | each isText ifTrue: [aBlock value: each]]! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 10/31/2007 18:25'!
elements
	^self elementsAndContents select: [:each | each isText not]! !

!XMLElement methodsFor: 'enumerating' stamp: 'mir 1/3/2008 14:18'!
elementsAndContentsDo: aBlock
	self elementsAndContents do: aBlock! !

!XMLElement methodsFor: 'enumerating' stamp: 'mir 10/31/2007 18:25'!
elementsDo: aBlock
	self elementsAndContentsDo: [:each | each isText ifFalse: [aBlock value: each]]! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:34'!
firstTagNamed: aSymbol 
	"Return the first encountered node with the specified tag.
	If it is not the receiver, pass the message on"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [^self].
	^super firstTagNamed: aSymbol ! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:34'!
firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that allows
	the block to evaluate to true. Pass the message on"

	((self localName == aSymbol
		or: [self tag == aSymbol])
 		and: [aBlock value: self])
		ifTrue: [^self].
	^super firstTagNamed: aSymbol with: aBlock.! !

!XMLElement methodsFor: 'testing' stamp: 'mir 10/31/2007 17:55'!
isEmpty
	^self elements isEmpty! !

!XMLElement methodsFor: 'testing' stamp: 'mir 1/17/2002 15:26'!
isTag
	^true! !

!XMLElement methodsFor: 'name space' stamp: 'sd 5/25/2003 18:24'!
localName
	^ name! !

!XMLElement methodsFor: 'accessing' stamp: 'sd 5/25/2003 18:25'!
name
	^ self qualifiedName! !

!XMLElement methodsFor: 'initialize' stamp: 'mir 1/17/2002 15:24'!
name: aString
	name := aString asSymbol! !

!XMLElement methodsFor: 'accessing' stamp: 'gc 6/28/2007 21:32'!
parent
	^ parent! !

!XMLElement methodsFor: 'accessing' stamp: 'gc 6/28/2007 21:32'!
parent: anXMLElement
	parent := anXMLElement ! !

!XMLElement methodsFor: 'printing' stamp: 'mir 10/31/2007 18:19'!
printXMLOn: writer
	writer startElement: self name attributeList: self attributes.
	(writer canonical not
		and: [self isEmpty and: [self attributes isEmpty not]])
		ifTrue: [writer endEmptyTag: self name]
		ifFalse: [
			writer endTag.
			self elementsAndContentsDo: [:content | content printXMLOn: writer].
			writer endTag: self name]! !

!XMLElement methodsFor: 'name space' stamp: 'mir 6/5/2003 15:20'!
qualifiedName
	^self namespace
		ifNil: [self localName]
		ifNotNil: [self namespace , ':' , self localName]! !

!XMLElement methodsFor: 'initialize' stamp: 'mir 3/7/2000 16:43'!
setAttributes: newAttributes
	attributes := newAttributes! !

!XMLElement methodsFor: 'accessing' stamp: 'sd 5/25/2003 18:25'!
tag
	^ self name asSymbol! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:33'!
tagsNamed: aSymbol contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver
	if the receiver tag equals aSymbol. Pass the message on"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [self contentsDo: aBlock].
	super tagsNamed: aSymbol contentsDo: aBlock! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol do: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock
	with the receiver. Continue the search"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol do: aOneArgumentBlock! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self]
! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock
	with the receiver. Then recurse through all the children"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock! !

!XMLElement methodsFor: 'searching' stamp: 'mir 6/25/2003 13:35'!
tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"If the receiver tag equals aSymbol, evaluate aOneArgumentBlock with the receiver.
	For each of the receivers children do the same. Do not go beyond direct children"

	(self localName == aSymbol
		or: [self tag == aSymbol])
		ifTrue: [aOneArgumentBlock value: self].
	super tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 6/5/2003 12:02'!
valueFor: aSymbol 
	^self valueFor: aSymbol ifAbsent: ['']! !

!XMLElement methodsFor: 'accessing' stamp: 'mir 1/17/2002 15:28'!
valueFor: aSymbol ifAbsent: aBlock 
	^self attributes at: aSymbol ifAbsent: aBlock! !

!XMLElement class methodsFor: 'instance creation' stamp: 'mir 3/7/2000 16:33'!
named: aString
	^self new name: aString! !

!XMLElement class methodsFor: 'instance creation' stamp: 'mir 8/14/2000 18:01'!
named: aString attributes: attributeList
	^self new
		name: aString;
		setAttributes: attributeList! !

!XMLElement class methodsFor: 'instance creation' stamp: 'mir 6/5/2003 15:21'!
named: aString namespace: ns uri: uri attributes: attributeList
	^self new
		name: aString;
		namespace: ns uri: uri;
		setAttributes: attributeList! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/4/2003 15:51'!
currentScope
	^self scope last! !

!XMLNamespaceScope methodsFor: 'scope' stamp: 'mir 6/16/2003 16:37'!
declareNamespace: ns uri: uri
	"Declare the given name space prefix with the given URL"

	ns = 'xmlns'
		ifTrue: [^self defaultNamespace: uri].
	self establishLocalBindings.
	currentBindings removeKey: ns ifAbsent: [].
	currentBindings at: ns put: uri! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/24/2003 14:22'!
defaultNamespace
	^self currentScope first! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/24/2003 14:23'!
defaultNamespace: ns
	"Declare the default namespace."
	self currentScope at: 1 put: ns! !

!XMLNamespaceScope methodsFor: 'scope' stamp: 'mir 6/5/2003 19:28'!
enterScope
	self scope addLast: { self defaultNamespace. nil. currentBindings. }! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/4/2003 16:08'!
establishLocalBindings
	(self currentScope at: 2)
		ifNil: [
			currentBindings := currentBindings copy.
			self currentScope at: 2 put: currentBindings]! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/24/2003 14:25'!
initScope
	scope := OrderedCollection new: 20.
	currentBindings := Dictionary new.
	scope addLast: {'http://www.w3.org/TR/REC-xml-names'. currentBindings. nil. }.
! !

!XMLNamespaceScope methodsFor: 'scope' stamp: 'mir 6/4/2003 16:05'!
leaveScope
	| leftScope |
	leftScope := self scope removeLast.
	currentBindings := (self currentScope at: 2) ifNil: [leftScope at: 3]! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/5/2003 19:32'!
namespaceAliases: namespace
	"Locate all namespaces that are aliases of the given URI."

	| aliases uri |
	aliases := Set new.
	uri := self namespaceURIOf: namespace ifAbsent: [self parseError: 'Attribute refers to undefined namespace ' , namespace asString ].
	currentBindings keysAndValuesDo: [:ns :u |
		(u = uri
			and: [ns ~= namespace])
			ifTrue: [aliases add: ns]].
	^ aliases! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'sd 5/28/2003 09:33'!
namespaceURIOf: ns
	"Retrieve the URI of the given namespace prefix, if it is defined. A nil namespace
	returns the global namespace"

	^ self namespaceURIOf: ns ifAbsent: [ nil ]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/24/2003 13:33'!
namespaceURIOf: ns ifAbsent: aBlock
	"Retrieve the URI of the given namespace prefix, if it is defined. 
	A nil namespace returns the default namespace. 
	If no namespace can be found the value of the block is returned"

	^ns
		ifNil: [self defaultNamespace]
		ifNotNil: [currentBindings at: ns ifAbsent: aBlock]! !

!XMLNamespaceScope methodsFor: 'accessing' stamp: 'mir 6/5/2003 19:32'!
namespaces
	^currentBindings! !

!XMLNamespaceScope methodsFor: 'private' stamp: 'mir 6/24/2003 14:26'!
scope
	scope ifNil: [self initScope].
	^scope! !

!XMLNamespaceScope methodsFor: 'validation' stamp: 'mir 6/5/2003 17:16'!
validateAttributes: attributeList
	"check all attribute namespaces are defined and not duplicated by aliasing"
	| namespace localName |
	attributeList keysDo: [:attrName |
		self splitName: attrName into: [:ns :ln |
			namespace := ns.
			localName := ln].
		namespace ifNotNil: [
			(self namespaceAliases: namespace) do: [:alias |
				(attributeList includesKey: alias , ':' , localName)
					ifTrue: [self parseError: 'Attributes ' , attrName , ' and ' , alias , ':' , localName , ' are aliased to namespace ' , (self namespaceURIOf: namespace) ]]]]! !

!XMLNode methodsFor: 'accessing' stamp: 'mir 1/8/2002 18:44'!
addContent: contentString
	SAXParseException signal: 'Illegal string data.'! !

!XMLNode methodsFor: 'enumerating' stamp: 'mir 1/17/2002 14:49'!
contentsDo: aBlock! !

!XMLNode methodsFor: 'enumerating' stamp: 'mir 10/31/2007 18:02'!
elementsAndContentsDo: aBlock
	self elementsDo: aBlock! !

!XMLNode methodsFor: 'enumerating' stamp: 'mir 3/6/2002 10:56'!
elementsDo: aBlock! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:52'!
firstTagNamed: aSymbol 
	"Return the first encountered node with the specified tag. Pass the message on"

	| answer |

	self elementsDo: [:node | (answer := node firstTagNamed: aSymbol) ifNotNil: [^answer]].
	^nil! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
firstTagNamed: aSymbol with: aBlock
	"Return the first encountered node with the specified tag that
	allows the block to evaluate to true. Pass the message on"

	| answer |

	self elementsDo: [:node |
		(answer := node firstTagNamed: aSymbol with: aBlock) ifNotNil: [^answer]].
	^nil! !

!XMLNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:28'!
isProcessingInstruction
	^false! !

!XMLNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:26'!
isTag
	^false! !

!XMLNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:26'!
isText
	^false! !

!XMLNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:45'!
printOn: stream
	self printXMLOn: (XMLWriter on: stream)! !

!XMLNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:45'!
printXMLOn: writer
	self subclassResponsibility! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol childrenDo: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match"

	self elementsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol childrenDoAndRecurse: aOneArgumentBlock
	"Evaluate aOneArgumentBlock for all children who match and recurse"

	self elementsDo: [:each | 
		each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol contentsDo: aBlock
	"Evaluate aBlock for all of the contents of the receiver.
	The receiver has no tag, so pass the message on"

	self elementsDo: [:each | each tagsNamed: aSymbol contentsDo: aBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol do: aOneArgumentBlock
	"Search for nodes with tag aSymbol. When encountered evaluate aOneArgumentBlock"

	self elementsDo: [:each | each tagsNamed: aSymbol do: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'SqR 7/2/2000 15:58'!
tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock
	"Handled only by XMLTagNode subclass"

! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock
	"Recurse all children"

	self elementsDo: [:each | each tagsNamed: aSymbol ifReceiverDoAndRecurse: aOneArgumentBlock]! !

!XMLNode methodsFor: 'searching' stamp: 'mir 3/6/2002 10:53'!
tagsNamed: aSymbol ifReceiverOrChildDo: aOneArgumentBlock
	"Recurse all children"

	self elementsDo: [:each | each tagsNamed: aSymbol ifReceiverDo: aOneArgumentBlock]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 11/14/2007 17:25'!
addElement: element
	self elementsAndContents add: element! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 10/25/2000 11:22'!
addEntity: entityName value: entityValue
	self entities add: entityName->entityValue! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 3/6/2002 10:46'!
elementAt: entityName
	^self elementAt: entityName ifAbsent: [nil]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 11/14/2007 17:23'!
elementAt: entityName ifAbsent: aBlock
	elementsAndContents
		ifNil: [^aBlock value].
	^self elements detect: [:each | each isProcessingInstruction not and: [each name = entityName or: [each localName = entityName]]] ifNone: [^aBlock value]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 6/16/2003 17:36'!
elementUnqualifiedAt: entityName
	^self elementUnqualifiedAt: entityName ifAbsent: [nil]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 11/14/2007 17:23'!
elementUnqualifiedAt: entityName ifAbsent: aBlock
	elementsAndContents
		ifNil: [^aBlock value].
	^self elements detect: [:each | each localName = entityName] ifNone: [^aBlock value]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 11/14/2007 17:13'!
elements
	^self elementsAndContents! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 11/14/2007 17:13'!
elementsAndContents
	elementsAndContents ifNil: [elementsAndContents := OrderedCollection new].
	^elementsAndContents! !

!XMLNodeWithElements methodsFor: 'enumerating' stamp: 'mir 11/14/2007 17:21'!
elementsDo: aBlock
	self elements do: [:each | aBlock value: each]! !

!XMLNodeWithElements methodsFor: 'name space' stamp: 'mir 6/5/2003 15:20'!
namespace
	^ namespace! !

!XMLNodeWithElements methodsFor: 'name space' stamp: 'mir 6/16/2003 16:21'!
namespace: ns uri: u
	namespace := ns.
	uri := u! !

!XMLNodeWithElements methodsFor: 'name space' stamp: 'mir 6/5/2003 15:20'!
namespaceURI
	^ uri! !

!XMLNodeWithElements methodsFor: 'printing' stamp: 'mir 3/6/2002 10:49'!
printXMLOn: writer
	self elementsDo: [:element | element printXMLOn: writer]! !

!XMLNodeWithElements methodsFor: 'accessing' stamp: 'mir 3/6/2002 10:50'!
topElement
	^self elements first! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
data
	^data! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
data: aString
	data := aString! !

!XMLPI methodsFor: 'testing' stamp: 'mir 1/17/2002 15:28'!
isProcessingInstruction
	^true! !

!XMLPI methodsFor: 'printing' stamp: 'mir 1/17/2002 15:53'!
printXMLOn: writer
	writer pi: self target data: self data! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
target
	^target! !

!XMLPI methodsFor: 'accessing' stamp: 'mir 1/17/2002 13:02'!
target: aString
	target := aString! !

!XMLPI class methodsFor: 'instance creation' stamp: 'mir 1/17/2002 13:03'!
target: targetName data: aString
	^self new
		target: targetName;
		data: aString! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:51'!
attribute: aSymbol value: aString
	"This method is called for each attribute/value pair in a start tag"

	^self subclassResponsibility! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
beginStartTag: aSymbol asPI: aBoolean
	"This method is called for at the beginning of a start tag.
	The asPI parameter defines whether or not the tag is a 'processing
	instruction' rather than a 'normal' tag."

	^self subclassResponsibility! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
endStartTag: aSymbol
	"This method is called at the end of the start tag after all of the
	attributes have been processed"

	^self subclassResponsibility! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
endTag: aSymbol
	"This method is called when the parser encounters either an
	end tag or the end of a unary tag"

	^self subclassResponsibility! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:27'!
handleCData: aString
	self text: aString! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:26'!
handleEndTag: aString
	self endTag: aString! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:27'!
handlePCData: aString
	self text: aString! !

!XMLParser methodsFor: 'handling tokens' stamp: 'mir 1/17/2002 09:26'!
handleStartTag: tagName attributes: attributes
	self beginStartTag: tagName asPI: false.
	attributes keysAndValuesDo: [:key :value |
		self attribute: key value: value].
	self endStartTag: tagName! !

!XMLParser methodsFor: 'callbacks' stamp: 'SqR 7/2/2000 16:52'!
text: aString
	"This method is called for the blocks of text between tags.
	It preserves whitespace, but has all of the enclosed entities expanded"

	^self subclassResponsibility! !

!XMLStringNode methodsFor: 'as yet unclassified' stamp: 'pb 1/27/2010 13:06'!
asString
	^self string! !

!XMLStringNode methodsFor: 'accessing'!
characterData
	^self string! !

!XMLStringNode methodsFor: 'testing' stamp: 'mir 1/17/2002 15:27'!
isText
	^true! !

!XMLStringNode methodsFor: 'printing' stamp: 'mir 1/17/2002 15:53'!
printXMLOn: writer
	writer pcData: self string! !

!XMLStringNode methodsFor: 'accessing' stamp: 'mir 10/25/2000 11:28'!
string
	^string ifNil: ['']! !

!XMLStringNode methodsFor: 'accessing' stamp: 'mir 10/25/2000 11:28'!
string: aString
	string := aString! !

!XMLStringNode class methodsFor: 'instance creation' stamp: 'mir 10/25/2000 11:30'!
string: aString
	^self new string: aString! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 22:58'!
atEnd
	nestedStreams == nil
		ifTrue: [^peekChar == nil and: [stream atEnd]].
	^stream atEnd
		ifTrue: [
			self popNestingLevel.
			self atEnd]
		ifFalse: [false]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 18:12'!
checkAndExpandReference: parsingContext
	| referenceString nextChar |
	nextChar := self peek.
	self validating
		ifFalse: [^nil].
	nextChar == $&
		ifTrue: [
			self next.
			self peek == $#
				ifTrue: [^self pushStream: (ReadStream on: self nextCharReference asString)].
			referenceString := self nextLiteral.
			self next == $;
				ifFalse: [self errorExpected: ';'].
			self handleEntity: referenceString in: parsingContext ]
		ifFalse: [
			((nextChar == $%
				and: [self parsingMarkup])
				and: [parsingContext == #entityValue])
				ifTrue: [
					self skipSeparators.
					referenceString := self nextLiteral.
					self handleEntity: referenceString in: parsingContext]].

	self atEnd ifTrue: [self errorExpected: 'Character expected.'].
	^nextChar! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 22:59'!
checkNestedStream
	nestedStreams == nil
		ifFalse: [(peekChar == nil and: [self stream atEnd])
			ifTrue: [
				self popNestingLevel.
				self checkNestedStream]]
! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/16/2000 21:41'!
conditionalInclude: conditionalKeyword
	conditionalKeyword = 'INCLUDE'
		ifTrue: [^true].
	conditionalKeyword = 'IGNORE'
		ifTrue: [^false].
	^self conditionalInclude: (self parameterEntity: conditionalKeyword) value! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 6/29/2001 00:08'!
endDocTypeDecl
	"Skip ]>"
	self next; next.
	^nil! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
endParsingMarkup
	parsingMarkup := false! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/14/2002 15:06'!
entities
	entities ifNil: [entities := self initEntities].
	^entities! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/17/2002 13:53'!
entity: refName
	^self validating
		ifTrue: [self entities
			at: refName
			ifAbsentPut: [self parseError: 'XML undefined entity ' , refName printString]]
		ifFalse: [DTDEntityDeclaration name: refName value: '']
! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:43'!
entity: refName put: aReference
	"Only the first declaration of an entity is valid so if there is already one don't register the new value."
	self entities at: refName ifAbsentPut: [aReference]! !

!XMLTokenizer methodsFor: 'errors' stamp: 'mir 5/14/2003 18:27'!
errorExpected: expectedString
	| actualString |
	actualString := ''.
	self atEnd
		ifFalse: [
			[actualString := self next: 20]
				on: Error
				do: [:ex | ]].
	self parseError: 'XML expected ' , expectedString printString , ': ' , actualString! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/14/2002 17:59'!
externalEntities
	externalEntities ifNil: [externalEntities := Dictionary new].
	^externalEntities! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/14/2002 17:59'!
externalEntity: refName
	^self entities
		at: refName
		ifAbsentPut: ['']! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 3/14/2003 23:09'!
fastStreamStringContents: writeStream
	| newSize |
	newSize := writeStream position.
	^(String new: newSize)
		replaceFrom: 1
		to: newSize
		with: writeStream originalContents
		startingAt: 1! !

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

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 1/17/2002 18:12'!
handleEntity: referenceString in: parsingContext 

	| entity entityValue |
	entity := self entity: referenceString.
	entityValue := entity valueForContext: parsingContext.
	(self class isCharEscape: entityValue)
		ifTrue: [entityValue := entity reference].
	self pushStream: (ReadStream on: entityValue asString)! !

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

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'cwp 6/17/2003 21:08'!
handleWhitespace: aString
	self log: 'Whitespace: ' , aString! !

!XMLTokenizer methodsFor: 'handling tokens' stamp: 'mir 6/5/2003 14:53'!
handleXMLDecl: attributes namespaces: namespaces
	attributes keysAndValuesDo: [:key :value |
		self log: key , '->' , value]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 6/28/2001 16:45'!
hasNestedStreams
	^nestedStreams notNil! !

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

!XMLTokenizer methodsFor: 'initialize' stamp: 'mir 3/14/2003 19:27'!
initialize
	parsingMarkup := false.
	validating := false.
	attributeBuffer := WriteStream on: (String new: 128).
	nameBuffer := WriteStream on: (String new: 128)! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 12/7/2000 16:46'!
log: aString
	"Transcript show: aString; cr"! !

!XMLTokenizer methodsFor: 'errors' stamp: 'mir 1/9/2002 15:26'!
malformedError: errorString
	SAXMalformedException signal: errorString! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 11/23/2007 18:16'!
match: subCollection into: resultStream
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

	| pattern startMatch |
	pattern _ ReadStream on: subCollection.
	startMatch _ nil.
	[pattern atEnd] whileFalse: 
		[self atEnd ifTrue: [^ false].
		(self next) = (pattern next) 
			ifTrue: [pattern position = 1 ifTrue: [startMatch _ self position]]
			ifFalse: [pattern position: 0.
					startMatch ifNotNil: [
						self position: startMatch.
						startMatch _ nil]]].
	^ true

! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 6/28/2001 16:54'!
nestedStreams
	nestedStreams ifNil: [nestedStreams := OrderedCollection new].
	^nestedStreams! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 23:04'!
next
	"Return the next character from the current input stream. If the current stream is at end pop to next nesting level if there is one.
	Due to the potential nesting of original document, included documents and replacment texts the streams are held in a stack representing the nested streams. The current stream is the top one."
	| nextChar |
	peekChar
		ifNil: [
			nestedStreams ifNotNil: [self checkNestedStream].
			^nextChar := stream next]
		ifNotNil: [
			nextChar := peekChar.
			peekChar := nil.
			^nextChar].
	! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 6/5/2003 16:32'!
nextAttributeInto: attributes namespaces: namespaces

	| attrName attrValue |
	attrName := self nextName.
	self skipSeparators.
	self next == $=
		ifFalse: [self errorExpected: '='].
	self skipSeparators.
	attrValue := self nextAttributeValue.

	(self usesNamespaces
		and: [(attrName findString: 'xmlns') = 1])
		ifTrue: [attrName size > 6
			ifTrue: [namespaces at: (attrName copyFrom: 7 to: attrName size) put: attrValue]
			ifFalse: [namespaces at: attrName put: attrValue]]
		ifFalse: [attributes at: attrName put: attrValue]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/23/2007 17:51'!
nextAttributeValue
	| delimiterChar attributeValueStream nextChar nextPeek referenceString entity entityValue |
	delimiterChar := self next.
	(delimiterChar == $"
		or: [delimiterChar == $'])
		ifFalse: [self errorExpected: 'Attribute value delimiter expected.'].
	attributeValueStream := attributeBuffer reset.
	[
	nextPeek := nextChar := self next.
	nextChar ifNil: [self errorExpected: 'Character expected.'].
	nextChar == $&
		ifTrue: [
			self peek == $#
				ifTrue: [
					nextPeek := nil.
					nextChar := self nextCharReference]
				ifFalse: [
					referenceString := self nextLiteral.
					self next == $;
						ifFalse: [self errorExpected: ';'].
					entity := self entity: referenceString.
					entityValue := entity valueForContext: #content.
					(self class isCharEscape: entityValue)
						ifTrue: [
							nextPeek := nil.
							nextChar := entityValue first]
						ifFalse: [
							entityValue := entityValue asString.
							entityValue isEmpty
								ifTrue: [nextPeek := nextChar := nil]
								ifFalse: [
									self pushStream: (ReadStream on: entityValue asString).
									nextPeek := nextChar := self next]]]].
	nextPeek == delimiterChar]
		whileFalse: [
			nextChar ifNotNil: [attributeValueStream nextPut: nextChar]].
	^self fastStreamStringContents: attributeValueStream
"	^attributeValueStream contents"! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 17:00'!
nextCDataContent
	| cdata |
	"Skip $[ "
	self next.
	cdata := self nextUpToAll: ']]>'.
	self handleCData: cdata
! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 12/6/2000 14:29'!
nextCDataOrConditional

	| nextChar conditionalKeyword |
	"Skip ["
	self next.
	self skipSeparators.
	nextChar := self peek.
	nextChar == $%
		ifTrue: [
			self checkAndExpandReference: (self parsingMarkup ifTrue: [#dtd] ifFalse: [#content]).
			conditionalKeyword := self nextLiteral.
			self skipSeparators.
			^self next == $[
				ifTrue: [
						self skipSeparators.
						self nextIncludeSection: (self conditionalInclude: conditionalKeyword)]
				ifFalse: [self errorExpected: '[' ]].

	nextChar == $C
		ifTrue: [
			^self nextLiteral = 'CDATA'
				ifTrue: [self peek == $[
							ifTrue: [self nextCDataContent]
							ifFalse: [self errorExpected: '[' ]]
				ifFalse: [self errorExpected: 'CData']].
	self errorExpected: 'CData or declaration'
! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'pb 6/4/2012 16:31'!
nextCharReference
	| base charValue |
	self next == $# ifFalse: [ self errorExpected: 'character reference' ].
	base := self peek == $x
		ifTrue: [
			self next.
			16 ]
		ifFalse: [ 10 ].
	charValue := [ self readNumberBase: base ]
		on: Error
		do: [ :ex |
			self errorExpected: 'Number.' ].
	self next == $; ifFalse: [ self errorExpected: '";"' ].
	"^Unicode value: charValue"
	self flag: #pbfix.
	charValue < 255
		ifTrue: [ ^ Character value: charValue ]
		ifFalse: [ ^ $@ ].! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/28/2000 17:54'!
nextComment
	| string |
	"Skip first -"
	self next.
	self next == $-
		ifFalse: [self errorExpected: 'second comment $-'].
	string := self nextUpToAll: '-->'.
	self handleComment: string! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/8/2002 13:54'!
nextDocType
	| declType |
	declType := self nextLiteral.
	declType = 'DOCTYPE'
		ifTrue: [
			self startParsingMarkup.
			^self nextDocTypeDecl].
	self errorExpected: 'markup declaration, not ' , declType printString! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/17/2002 17:29'!
nextDocTypeDecl
	| nextChar |
	self skipSeparators.
	self nextLiteral.
	self skipSeparators.
	self peek == $[
		ifFalse: [[nextChar := self peek.
				nextChar == $> or: [nextChar == $[ ]] whileFalse: [self next]].
	self peek == $[
		ifTrue: [
			self next.
			[self skipSeparators.
			self peek == $]] whileFalse: [
				self checkAndExpandReference: #dtd.
				self nextNode].
			self next == $] 
				ifFalse: [self errorExpected: ']' ]].
	self skipSeparators.
	self next == $>
		ifFalse: [self errorExpected: '>' ].

	self endParsingMarkup! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/23/2007 17:48'!
nextEndTag
	| tagName |
	"Skip /"
	self next.
	tagName := self nextName.
	self skipSeparators.
	(self nextTrimmedBlanksUpTo: $>)
		ifNotEmpty: [self parseError: 'XML invalid end tag ' , tagName].
	self handleEndTag: tagName! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'gvc 6/6/2007 13:17'!
nextEntity
	"return the next XMLnode, or nil if there are no more.
	Fixed to retain leading whitespace when PCDATA is detected."

	|whitespace|
	"branch, depending on what the first character is"
	whitespace := self nextWhitespace.
	self atEnd ifTrue: [self handleEndDocument. ^ nil].
	self checkAndExpandReference: (self parsingMarkup ifTrue: [#dtd] ifFalse: [#content]).
	^self peek = $<
		ifTrue: [self nextNode]
		ifFalse: [whitespace isEmpty
					ifFalse: [self pushBack: whitespace].
				self nextPCData]! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/17/2002 14:24'!
nextEntityDeclaration
	| entityName entityDef referenceClass reference |
	self skipSeparators.
	referenceClass := self peek == $%
		ifTrue: [
			self next.
			self skipSeparators.
			DTDParameterEntityDeclaration]
		ifFalse: [DTDEntityDeclaration].
	entityName := self nextLiteral.
	self skipSeparators.
	entityDef := (self peek == $" or: [self peek == $'])
		ifTrue: [self nextEntityValue]
		ifFalse: [self nextExternalId].
	self skipUpTo: $>.
	reference := referenceClass name: entityName value: entityDef.
	reference registerIn: self.
	^reference! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 18:14'!
nextEntityValue
	| delimiterChar entityValueStream nextChar nextPeek referenceString entity entityValue |
	delimiterChar := self next.
	(delimiterChar == $"
		or: [delimiterChar == $'])
		ifFalse: [self errorExpected: 'Entity value delimiter expected.'].

	entityValueStream := WriteStream on: (String new).
	[
	nextPeek := nextChar := self peek.
	nextChar ifNil: [self errorExpected: 'Character expected.'].
	nextChar == $&
		ifTrue: [
			self next.
			self peek == $#
				ifTrue: [
					nextPeek := nil.
					nextChar := self nextCharReference]
				ifFalse: [
					referenceString := self nextLiteral.
					self next == $;
						ifFalse: [self errorExpected: ';'].
					entity := self entity: referenceString.
					entityValue := entity valueForContext: #entityValue.
					self pushStream: (ReadStream on: entityValue asString).
					nextPeek := nextChar := self next]]
		ifFalse: [
			nextChar == $%
				ifTrue: [
					self skipSeparators.
					referenceString := self nextLiteral.
					nextChar := self handleEntity: referenceString in: #entityValue.
					nextPeek := nextChar := self next]
				ifFalse: [self next]].
	nextPeek == delimiterChar]
		whileFalse: [
			nextChar ifNotNil: [entityValueStream nextPut: nextChar]].
	^entityValueStream contents! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'rkris 7/28/2004 12:35'!
nextExternalId
	| extDefType systemId dir |
	extDefType := self nextLiteral.
	extDefType = 'PUBLIC'
		ifTrue: [
			self skipSeparators.
			self nextPubidLiteral.
			self skipSeparators.
			self peek == $>
				ifFalse: [
					systemId := self nextSystemLiteral]].

	extDefType = 'SYSTEM'
		ifTrue: [
			self skipSeparators.
			systemId := self nextSystemLiteral].

	systemId
		ifNil: [^nil].

	"The rest of this method only applies if we're reading aFileStream"
	(self topStream isKindOf: FileStream)
		ifFalse: [^''].
	dir := self topStream directory.
	^(dir fileExists: systemId)
		ifTrue: [(dir readOnlyFileNamed: systemId) contentsOfEntireFile]
		ifFalse: ['']! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 6/28/2001 16:38'!
nextIncludeSection: parseSection
	| section |
	"Read the file up to the next include section delimiter and parse it if parseSection is true"

	
	section := self nextUpToAll: ']]>'.
	parseSection
		ifTrue: [
			self pushStream: (ReadStream on: section)]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 10/31/2007 16:19'!
nextLiteral
	| resultStream nextChar resultString |
	resultStream := (String new: 10) writeStream.
	((nextChar := self peek) isLetter
		or: [nextChar == $_])
		ifFalse: [self errorExpected: 'Name literal.'].
	[nextChar := self peek.
	(LiteralChars includes: nextChar)
		ifTrue: [
			nextChar == $&
				ifTrue: [
					nextChar := self next.
					resultStream nextPut: (self peek == $#
						ifTrue: [self nextCharReference]
						ifFalse: [^resultStream contents])]
				ifFalse: [
					resultStream nextPut: self next]]
		ifFalse: [resultString := resultStream contents.
			resultString isEmpty
				ifTrue: [self errorExpected: 'Name literal']
				ifFalse: [^resultString]]] repeat! !

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/17/2002 13:49'!
nextMarkupDeclaration
	| declType |
	declType := self nextLiteral.
	self validating
		ifFalse: [^self skipMarkupDeclaration].
	declType = 'ENTITY'
		ifTrue: [self nextEntityDeclaration]
		ifFalse: [self skipMarkupDeclaration]! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 10/31/2007 16:20'!
nextName
	| nextChar |
	nameBuffer reset.
	self peek == $.
		ifTrue: [self malformedError: 'Character expected.'].
	[(nextChar := self peek)
		ifNil: [self errorExpected: 'Character expected.'].
	NameDelimiters includes: nextChar] whileFalse: [
			nameBuffer nextPut: self next].
	^self fastStreamStringContents: nameBuffer! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/28/2000 17:52'!
nextNode
	| nextChar |
	"Skip < "
	self next.
	nextChar := self peek.
	nextChar == $!! ifTrue: [
		"Skip !!"
		self next.
		nextChar := self peek.
		nextChar == $- ifTrue: [^self nextComment].
		nextChar == $[ ifTrue: [^self nextCDataOrConditional].
		^self parsingMarkup
			ifTrue: [self nextMarkupDeclaration]
			ifFalse: [self nextDocType]].
	nextChar == $? ifTrue: [^self nextPI].
	^self nextTag! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 11/23/2007 17:52'!
nextPCData
	| resultStream nextChar referenceString entity entityValue nextPeek |
	resultStream := (String new: 10) writeStream.
	self validating
		ifFalse: [
			[self peek == $<]
				whileFalse: [resultStream nextPut: self next].
			^self handlePCData: resultStream contents].

	[
	nextPeek := nextChar := self peek.
	nextChar ifNil: [self errorExpected: 'Character expected.'].
	nextChar == $&
		ifTrue: [
			self next.
			self peek == $#
				ifTrue: [
					nextPeek := nil.
					nextChar := self nextCharReference]
				ifFalse: [
					referenceString := self nextLiteral.
					self next == $;
						ifFalse: [self errorExpected: ';'].
					entity := self entity: referenceString.
					entityValue := entity valueForContext: #content.
					(self class isCharEscape: entityValue)
						ifTrue: [
							nextPeek := nil.
							nextChar := entityValue first]
						ifFalse: [
							entityValue := entityValue asString.
							entityValue isEmpty
								ifTrue: [nextPeek := nextChar := nil]
								ifFalse: [
									self pushStream: (ReadStream on: entityValue asString).
									nextPeek := nextChar := self peek]]]]
		ifFalse: [nextPeek == $< ifFalse: [self next]].
	nextPeek == $<]
		whileFalse: [
			nextChar ifNotNil: [resultStream nextPut: nextChar]].
	self handlePCData: resultStream contents! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 13:00'!
nextPI
	| piTarget piData |
	"Skip ?"
	self next.
	piTarget := self nextLiteral.
	piTarget asUppercase = 'XML'
		ifTrue: [^self nextXMLDecl].
	self skipSeparators.
	piData := self nextUpToAll: '?>'.
	self handlePI: piTarget data: piData! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 14:25'!
nextPubidLiteral
	^self nextAttributeValue! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 1/17/2002 14:25'!
nextSystemLiteral
	^self nextAttributeValue! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 6/5/2003 22:37'!
nextTag
	| tagName attributes nextChar namespaces |
	(self peek = $/)
		ifTrue: [^self nextEndTag].
	tagName := self nextName.
	self skipSeparators.
	attributes := Dictionary new: 33.
	namespaces := Dictionary new: 5.
	[(nextChar := self peek) == $> or: [nextChar == $/]] whileFalse: [
		self checkAndExpandReference: #content.
		self nextAttributeInto: attributes namespaces: namespaces.
		self skipSeparators.].
	self handleStartTag: tagName attributes: attributes namespaces: namespaces.
	self next == $/
		ifTrue: [
			self handleEndTag: tagName.
			self next].
	! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'bf 4/27/2006 17:55'!
nextTrimmedBlanksUpTo: delimiter
	| resultStream nextChar |
	resultStream := WriteStream on: (String new: 10).
	nextChar := nil.
	[(nextChar := self next) == delimiter]
		whileFalse: [
			nextChar == $  ifFalse: [
				resultStream nextPut: nextChar]].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found'].
	^resultStream contents
! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 5/14/2003 18:44'!
nextUpTo: delimiter
	| resultStream nextChar |
	resultStream := WriteStream on: (String new: 10).
	[self atEnd or: [(nextChar := self next) == delimiter]]
		whileFalse: [resultStream nextPut: nextChar].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found'].
	^resultStream contents
! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 11/23/2007 19:42'!
nextUpToAll: delimitingString
	| string |
	self unpeek.
	string := self upToAll: delimitingString.
	string
		ifNil: [self parseError: 'XML no delimiting ' , delimitingString printString , ' found'].
	^string! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'mir 10/31/2007 17:49'!
nextWhitespace
	| nextChar resultStream resultString|
	resultStream := (String new: 10) writeStream.
	[((nextChar := self peek) ~~ nil)
		and: [SeparatorTable includes: nextChar]]
		whileTrue: [resultStream nextPut: nextChar. self next].
	(nestedStreams == nil or: [self atEnd not])
		ifFalse: [self checkNestedStream.
				self nextWhitespace].
	resultString := resultStream contents.
	resultString isEmpty ifFalse: [self handleWhitespace: resultString].
	^resultString! !

!XMLTokenizer methodsFor: 'tokenizing' stamp: 'thf 9/2/2005 15:04'!
nextXMLDecl
	| attributes nextChar namespaces |
	self skipSeparators.
	attributes := Dictionary new.
	namespaces := Dictionary new.
	[(nextChar := self peek) == $?] whileFalse: [
		self nextAttributeInto: attributes namespaces: namespaces.
		self skipSeparators.].
	self next.
	self next == $>
		ifFalse: [self errorExpected: '> expected.'].
	(attributes includesKey: 'encoding') ifTrue: [self streamEncoding: (attributes at: 'encoding')].
	self handleXMLDecl: attributes namespaces: namespaces
	! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:20'!
parameterEntities
	parameterEntities ifNil: [parameterEntities := Dictionary new].
	^parameterEntities! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:40'!
parameterEntity: refName
	^self parameterEntities
		at: refName
		ifAbsent: [self parseError: 'XML undefined parameter entity ' , refName printString]! !

!XMLTokenizer methodsFor: 'entities' stamp: 'mir 11/16/2000 21:42'!
parameterEntity: refName put: aReference
	"Only the first declaration of an entity is valid so if there is already one don't register the new value."
	self parameterEntities at: refName ifAbsentPut: [aReference]! !

!XMLTokenizer methodsFor: 'errors' stamp: 'mir 1/8/2002 15:37'!
parseError: errorString
	SAXParseException signal: errorString! !

!XMLTokenizer methodsFor: 'accessing' stamp: 'mir 6/28/2001 16:51'!
parseStream: aStream
	self stream: aStream! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
parsingMarkup
	^parsingMarkup! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 3/14/2003 23:05'!
peek
	"Return the next character from the current input stream. If the current stream poop to next nesting level if there is one.
	Due to the potential nesting of original document, included documents and replacment texts the streams are held in a stack representing the nested streams. The current stream is the top one."
	peekChar
		ifNil: [
			nestedStreams ifNotNil: [self checkNestedStream].
			^peekChar := stream next]
		ifNotNil: [^peekChar]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 6/29/2001 00:36'!
popNestingLevel
	self hasNestedStreams
		ifTrue: [
			self stream close.
			self stream: self nestedStreams removeLast.
			self nestedStreams size > 0
				ifFalse: [nestedStreams := nil]]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'gvc 6/6/2007 13:13'!
pushBack: aString
	"Fixed to push the string before the peek char (if any)."
	
	| pushBackString |
	pushBackString := peekChar
		ifNil: [aString]
		ifNotNil: [aString, peekChar asString].
	peekChar := nil.
	self pushStream: (ReadStream on: pushBackString)! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 1/16/2002 10:54'!
pushStream: newStream
	"Continue parsing from the new nested stream."
	self unpeek.
	self nestedStreams addLast: self stream.
	self stream: newStream! !

!XMLTokenizer methodsFor: 'private' stamp: 'mu 11/14/2005 14:12'!
readNumberBase: base
	"Read a hex number from stream until encountering $; "

	| value digit |

	base = 10 ifFalse: [	| numberString | 
		numberString := self nextUpTo: $;.
		self stream skip: -1.
		^Integer readFrom: numberString asUppercase readStream base: base. 
	].

	value := 0.
	digit := DigitTable at: self peek asciiValue.
	digit < 0
		ifTrue: [self error: 'At least one digit expected here'].
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

!XMLTokenizer methodsFor: 'tokenizing dtd' stamp: 'mir 1/4/2002 11:05'!
skipMarkupDeclaration
	self skipUpTo: $>! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 10/31/2007 16:33'!
skipSeparators
	| nextChar |
	[((nextChar := self peek) ~~ nil)
		and: [SeparatorTable includes: nextChar]]
		whileTrue: [self next].
	(nestedStreams == nil or: [self atEnd not])
		ifFalse: [
			self checkNestedStream.
			self skipSeparators]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 1/16/2002 10:42'!
skipUpTo: delimiter
	| nextChar |
	self unpeek.
	[self atEnd or: [(nextChar := self next) == delimiter]]
		whileFalse: [].
	nextChar == delimiter
		ifFalse: [self parseError: 'XML no delimiting ' , delimiter printString , ' found']
! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 11/13/2000 18:19'!
startParsingMarkup
	parsingMarkup := true! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 6/28/2001 16:50'!
stream
	^stream! !

!XMLTokenizer methodsFor: 'private' stamp: 'mir 6/28/2001 16:50'!
stream: newStream
	"Continue parsing from the new nested stream."
	stream := newStream! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 11/23/2007 19:40'!
stream: aStream upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (not inclusive) of aCollection. If aCollection is not in the stream, answer nil."

	| startPos endMatch result |
	startPos := aStream position.
	(aStream  match: aCollection) 
		ifTrue: [endMatch := aStream position.
			aStream position: startPos.
			result := aStream next: endMatch - startPos - aCollection size.
			aStream position: endMatch.
			^ result]
		ifFalse: [
			aStream position: startPos.
			^nil]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 7/12/2006 17:46'!
streamEncoding: encodingString

	| converterClass |
	Smalltalk at: #TextConverter ifPresent: [:tc | 
		(stream respondsTo: #converter:) ifTrue: [
			converterClass := tc defaultConverterClassForEncoding: encodingString asLowercase.
			converterClass ifNotNil: [stream converter: converterClass new]]]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 1/17/2002 14:31'!
topStream
	^self hasNestedStreams
		ifTrue: [self nestedStreams first]
		ifFalse: [self stream]! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'gvc 6/6/2007 13:19'!
unpeek
	"Fixed to use nested stream since multi-byte streams
	do not properly override pushBack: to deal with multi-byte
	characters."
	
	peekChar ifNotNil: [self pushBack: '']! !

!XMLTokenizer methodsFor: 'streaming' stamp: 'mir 11/23/2007 20:00'!
upToAll: delimitingString
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of delimitingString. If delimitingString is not in the stream, answer the entire rest of the stream."

	| result |

	self hasNestedStreams
		ifFalse: [
			result := self stream: self stream upToAll: delimitingString.
			result
				ifNil: [self parseError: 'XML no delimiting ' , delimitingString printString , ' found'].
			^result].

	result := self stream: self stream upToAll: delimitingString.
	result
		ifNotNil: [^result].
	result := String streamContents: [:resultStream |
		resultStream nextPutAll: self stream upToEnd.
		self atEnd
			ifTrue: [self parseError: 'XML no delimiting ' , delimitingString printString , ' found'].
		self stream position timesRepeat: [
			self atEnd
				ifFalse: [
					resultStream nextPut: self next]]].
	self pushBack: result.
	^self upToAll: delimitingString! !

!XMLTokenizer methodsFor: 'testing' stamp: 'mir 6/5/2003 16:33'!
usesNamespaces
	^false! !

!XMLTokenizer methodsFor: 'testing' stamp: 'mir 1/14/2002 17:51'!
validating
	^validating! !

!XMLTokenizer methodsFor: 'accessing' stamp: 'mir 1/14/2002 17:51'!
validating: aBoolean
	validating := aBoolean! !

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 8/14/2000 11:41'!
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

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 7/12/2006 16:49'!
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

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 8/14/2000 11:41'!
exampleAddressBook
	| tokenizer |
	"XMLTokenizer exampleAddressBook"

	tokenizer := XMLTokenizer on: self addressBookXML readStream.
	[tokenizer next notNil]
		whileTrue: []! !

!XMLTokenizer class methodsFor: 'examples' stamp: 'mir 8/14/2000 16:23'!
exampleAddressBookWithDTD
	| tokenizer |
	"XMLTokenizer exampleAddressBookWithDTD"

	tokenizer := XMLTokenizer on: self addressBookXMLWithDTD readStream.
	[tokenizer next notNil]
		whileTrue: []! !

!XMLTokenizer class methodsFor: 'class initialization' stamp: 'mir 10/31/2007 17:22'!
initialize
	"XMLTokenizer initialize"

	CharEscapes := CharacterSet newFrom: #( $& $" $' $> $< ).

	SeparatorTable  := CharacterSet new.
	#(9 10 12 13 32) do: [:each | SeparatorTable add: each asCharacter].

	LiteralChars := CharacterSet newFrom: #( $: $- $: $= $.).
	0 to: 255 do: [:i | 
		| char |
		char := i asCharacter.
		(char isDigit or: [char isLetter])
		ifTrue: [LiteralChars add: char]].

	NameDelimiters := CharacterSet new.
	#(9 10 12 13 32 61 "$= asInteger 61" 62 "$> asInteger" 47 "$/ asInteger")
		do: [:each | NameDelimiters add: each asCharacter].

	DigitTable := Array new: 256.
	DigitTable atAllPut: -1.
	($0 to: $9) do: [:each | DigitTable at: each asciiValue put: each digitValue].
	($a to: $f) do: [:each | DigitTable at: each asciiValue put: each digitValue].
	($A to: $F) do: [:each | DigitTable at: each asciiValue put: each digitValue].
! !

!XMLTokenizer class methodsFor: 'accessing' stamp: 'mir 11/23/2007 15:50'!
isCharEscape: entityValue
	^entityValue size = 1
		and: [CharEscapes includes: entityValue first]! !

!XMLTokenizer class methodsFor: 'instance creation' stamp: 'mir 11/16/2000 07:58'!
on: aStream
	^self new parseStream: aStream! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 5/20/2003 11:04'!
attribute: attributeName value: attributeValue
	self stream
		space;
		nextPutAll: attributeName.
	self
		eq;
		putAsXMLString: attributeValue! !

!XMLWriter methodsFor: 'accessing' stamp: 'mir 12/7/2000 15:54'!
canonical
	^canonical! !

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

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 15:09'!
declareNamespace: ns uri: uri
	self scope declareNamespace: ns uri: uri! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 14:23'!
defaultNamespace
	^self scope defaultNamespace! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 14:23'!
defaultNamespace: ns
	"Declare the default namespace."
	self scope defaultNamespace: ns! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
endCData
	self stream nextPutAll: ']]>'! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
endComment
	self stream nextPutAll: ' -->'! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 8/8/2000 18:13'!
endDecl: type
	self endTag! !

!XMLWriter methodsFor: 'writing dtd' stamp: 'mir 12/8/2000 18:02'!
endDeclaration
	self stream
		cr;
		nextPut: $].
	self endTag! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/8/2000 17:56'!
endEmptyTag: tagName
	self popTag: tagName.
	self stream nextPutAll: '/>'.
	self canonical
		ifFalse: [self stream space]! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 5/20/2003 12:13'!
endPI
	self stream nextPutAll: ' ?>'! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 7/12/2006 16:07'!
endTag
	self stream nextPutAll: '>'.
	self indent.
	"self canonical
		ifFalse: [self stream space]"! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 7/12/2006 17:02'!
endTag: tagName
	self outdent.
	self endTag: tagName xmlns: nil! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 6/24/2003 14:46'!
endTag: tagName xmlns: xmlns
	self popTag: tagName.
	self stream
		nextPutAll: '</'.
	(xmlns notNil
		and: [xmlns ~= self defaultNamespace])
		ifTrue: [self stream
			nextPutAll: xmlns;
			nextPut: $:].
	stream nextPutAll: tagName.
	self endTag.
! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 15:02'!
enterScope
	self scope enterScope! !

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

!XMLWriter methodsFor: 'initialize' stamp: 'mir 7/12/2006 15:54'!
initialize
	stack := OrderedCollection new.
	canonical := false.
	scope := XMLNamespaceScope new! !

!XMLWriter methodsFor: 'namespaces' stamp: 'mir 6/24/2003 14:47'!
leaveScope
	self scope leaveScope! !

!XMLWriter methodsFor: 'private' stamp: 'mir 7/12/2006 17:05'!
outdent
	currentIndent
		ifNotNil: [
			stream cr.
			currentIndent := currentIndent-1.
			self writeIndent.
			currentIndent := currentIndent-1.]! !

!XMLWriter methodsFor: 'writing xml' stamp: 'ar 12/15/2002 15:56'!
pcData: aString
	| lastIndex nextIndex |
	lastIndex := 1.
	"Unroll the first search to avoid copying"
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0 ifTrue:[^self stream nextPutAll: aString].
	[self stream nextPutAll: (aString copyFrom: lastIndex to: nextIndex-1).
	self stream nextPutAll: (XMLTranslation at: (aString at: nextIndex)).
	lastIndex := nextIndex + 1.
	nextIndex := String findFirstInString: aString inSet: XMLTranslationMap startingAt: lastIndex.
	nextIndex = 0] whileFalse.
	self stream nextPutAll: (aString copyFrom: lastIndex to: aString size).! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 12/11/2000 16:12'!
pi: piTarget data: piData
	self startPI: piTarget.
	self stream nextPutAll: piData.
	self endPI! !

!XMLWriter methodsFor: 'private' stamp: 'mir 8/7/2000 16:23'!
popTag: tagName
	| stackTop |
	stackTop := self stack isEmpty
		ifTrue: ['<empty>']
		ifFalse: [self stack last].
	^stackTop = tagName
		ifTrue: [self stack removeLast]
		ifFalse: [self error: 'Closing tag "' , tagName , '" does not match "' , stackTop]! !

!XMLWriter methodsFor: 'private' stamp: 'mir 8/7/2000 16:18'!
pushTag: tagName
	self stack add: tagName! !

!XMLWriter methodsFor: 'private' stamp: 'pb 6/4/2012 16:33'!
putAsXMLString: aValue
	| useValue |
	self flag: #pbfix.
	"handling nil"
	self stream nextPut: $".
	"self pcData: aValue."
	useValue := aValue
		ifNil: [ '' ]
		ifNotNil: [ aValue ].
	self pcData: useValue.
	self stream nextPut: $".! !

!XMLWriter methodsFor: 'private' stamp: 'mir 6/24/2003 14:22'!
scope
	^scope! !

!XMLWriter methodsFor: 'private' stamp: 'mir 8/8/2000 17:02'!
stack
	^stack! !

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

!XMLWriter methodsFor: 'writing xml' stamp: 'pb 6/4/2012 16:34'!
startElement: elementName attributeList: attributeList
	self canonical ifFalse: [ self stream newLine ].
	self startTag: elementName.
	attributeList keys asSortedCollection do: [ :key |
		self
			attribute: key
			value: (attributeList at: key) ].! !

!XMLWriter methodsFor: 'private tags' stamp: 'mir 12/8/2000 18:01'!
startPI: identifier
	self stream
		nextPutAll: '<?';
		nextPutAll: identifier;
		space! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 7/12/2006 16:08'!
startTag: tagName
	self writeIndent.
	self startTag: tagName xmlns: nil! !

!XMLWriter methodsFor: 'writing xml' stamp: 'mir 6/24/2003 14:10'!
startTag: tagName xmlns: xmlns
	self stream
		nextPut: $<.
	(xmlns notNil
		and: [xmlns ~= self scope defaultNamespace])
		ifTrue: [self stream
			nextPutAll: xmlns;
			nextPut: $:].
	self stream
		nextPutAll: tagName.
	"self canonical
		ifFalse: [self stream space]."
	self pushTag: tagName! !

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

!XMLWriter class methodsFor: 'class initialization' stamp: 'pb 6/5/2012 20:20'!
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
XMLTokenizer initialize!
XMLWriter initialize!
