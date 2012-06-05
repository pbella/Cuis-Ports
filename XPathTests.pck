'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 4 June 2012 at 7:46:45 pm'!
'Description Split out from XML-Parser'!
!classDefinition: #XPathTest category: #XPathTests!
TestCase subclass: #XPathTest
	instanceVariableNames: 'xml document'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XPathTests'!
!classDefinition: 'XPathTest class' category: #XPathTests!
XPathTest class
	instanceVariableNames: ''!


!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/6/2003 20:47'!
setUp
	"Need this for VW parser"
	| parser |
	xml := XMLTokenizer addressBookXML.
	"This is for the basic XML parser (what's it called again?)"
	document := XMLDOMParser parseDocumentFrom: xml readStream.
	"This is for the ported VW parser
	parser := VWXMLXMLParser on: XMLTokenizer addressBookXML readStream.
	parser validate: false.
	parser scanDocument.
	document := parser documentNode root"! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:14'!
testAttribute
	| path results |
	path := XPath for: 'person/address/@city'.
	results := path in: document.
	self assert: results size = 4 & (results size > 0).
	self assert: (results at: 1)
			= 'Los Angeles'! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/12/2003 21:53'!
testCount
	| path results |
	path := XPath for: 'count(person)'.
	results := path in: document.
	self assert: results size = 1.
	self assert: ((results at: 1) = 4).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:15'!
testDescendant
	| path results |
	path := XPath for: '//address/@city'.
	results := path in: document.
	self assert: results size = 4 & (results size > 0).
	self assert: (results at: 1)
			= 'Los Angeles'.
! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:15'!
testDot
	| path results |
	path := XPath for: 'person/address/./@city'.
	results := path in: document.
	self assert: results size = 4 & (results size > 0).
	self assert: (results at: 1)
			= 'Los Angeles'.
! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:15'!
testDotDot
	| path results |
	path := XPath for: 'person/address/../address/@city'.
	results := path in: document.
	self assert: results size = 4 & (results size > 0).
	self assert: (results at: 1)
			= 'Los Angeles'.
! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:15'!
testDotDotTooHigh
	| path results |
	path := XPath for: 'person/address/../../../address/@city'.
	results := path in: document.
	self assert: (results size = 0).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 8/20/2002 07:33'!
testLast
	| path results |
	path := XPath for: 'person/last()'.
	results := path in: document.
	self assert: (results size = 4).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:16'!
testOneLevel
	| path results |
	path := XPath for: 'person'.
	results := path in: document.
	self assert: (results size = 4).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:16'!
testThreeLevel
	| path results |
	path := XPath for: 'person/contact-info/email'.
	results := path in: document.
	self assert: (results size = 2).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:16'!
testTwoLevel
	| path results |
	path := XPath for: 'person/address'.
	results := path in: document.
	self assert: (results size = 4).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 8/19/2002 07:53'!
testWildcard
	| path results |
	path := XPath for: '/*'.
	results := path in: document.
	self assert: (results size = 4).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 8/19/2002 07:52'!
testWildcardDescendant
	| path results |
	path := XPath for: '//*'.
	results := path in: document.
	self assert: (results size = 27).! !

!XPathTest methodsFor: 'as yet unclassified' stamp: 'PH 10/7/2003 07:16'!
testWildcardDescendantAttribute
	| path results |
	path := XPath for: '//*/@city'.
	results := path in: document.
	self assert: results size = 4 & (results size > 0).
	self assert: (results at: 1)
			= 'Los Angeles'.
! !
