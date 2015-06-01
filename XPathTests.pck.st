'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 19 February 2015 at 9:24:24.089253 pm'!
'Description Based on XML-Parser-AlexandreBergel.15.mcz XPath tests.  Split out from XML-Parser.

XPath has been removed from XML-Parser... sticking with this version until a better solution is found.'!
!provides: 'XPathTests' 1 1!
!requires: 'XPath' 1 1 nil!
!classDefinition: #XPathTest category: #XPathTests!
TestCase subclass: #XPathTest
	instanceVariableNames: 'xml document'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'XPathTests'!
!classDefinition: 'XPathTest class' category: #XPathTests!
XPathTest class
	instanceVariableNames: ''!


!XPathTest methodsFor: 'as yet unclassified' stamp: 'pb 6/11/2012 00:31'!
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

!XPathTest methodsFor: 'as yet unclassified' stamp: 'pb 6/11/2012 00:32'!
setUp
	"Need this for VW parser"
	| parser |
	"
	xml := XMLTokenizer addressBookXML.
	"
	xml := self addressBookXML.
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
