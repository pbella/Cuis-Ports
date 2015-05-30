Cuis-Ports
==========

This repository contains various Smalltalk packages that have been ported to Cuis.  Where possible, the original source that the package is based on has been noted in this document as well as in the package description field.

### Text Processing

#### XML

XML-Parser - Based on http://squeaksource.cdn.st/XMLSupport/XML-Parser-NorbertHartl.141.mcz. This is the version which 
is loaded into Pharo 2.0beta as of January 2013.

XPath - Based on XML-Parser-AlexandreBergel.15.mcz XPath.  Split out from XML-Parser

Installation

     | slash |

    slash :=  FileDirectory slash.

    {
         '..', slash, 'Cuis-Ports', slash, 'XML-Parser.pck.st' .
         '..', slash, 'Cuis-Ports', slash, 'XMLParserTests.pck.st' .

         '..', slash, 'Cuis-Ports', slash, 'XPath.pck.st'.
         '..', slash, 'Cuis-Ports', slash, 'XPathTests.pck.st' .
    }
    do:
    [ :fileName | CodePackageFile installPackageStream:
                 (FileStream concreteStream readOnlyFileNamed: fileName)
    ]    

Error messages while installing

     SAXHandler>>invokeDeprecated:withArguments:orForwardTo:withArguments: 
    (Deprecation is Undeclared) 

     Package XML-Parser successfully installed

     Undeclared: a Dictionary(#Deprecation->nil #Unicode->nil )

	 
#### HTML

HTML - a tolerant HTML parser.  Does a decent job of consuming the various malformed/invalid HTML out on the web.  Based on HTML-sd.2.mcz found at http://squeaksource.com/@HDNjkoaXwriIV8js/Q0l6qq8Y


#### Regular Expressions

VBRegex - A native regex implementation - no plugin required.
Based on http://www.squeaksource.com/Regex/VB-Regex-damienpollet.17.mcz


#### Math


3DTransform - Based on 3DTransform-pbm.19.mcz


#### OpenGL

OpenGL - Originally based on OpenGL-Core-jrd.6.mcz and have merged in some of the changes from OpenGL-Core-bf.17.mcz.  

Depends on 3DTransform and FFI.  If you are running on a Unix or Linux system, see the comment in OGLUnix openGLLibraryName. This was pushed out prematurely and has at least a couple of issues yet to be resolved: font handling is broken, and there are at least one or two hacks that were forgotten about that need to be fixed. 

FFI - See https://github.com/hhzl/Cuis-FFI

Note: after loading FFI projects, you must call 
    
	Smalltalk recreateSpecialObjectsArray.
	

#### OMeta 2

##### Installation

A. Download Cuis4.2-2337 or later

B. Pull down the OMeta*.st files from https://github.com/pbella/Cuis-Ports

C. File in in the  following sequence

	1. OMeta2-stage1.st
	2. OMeta2.pck.st
	3. OMeta2Examples.pck.st (optional)
	4. OMeta2Tests.pck.st (optional)

D. Check examples in the OMeta2Examples class (for more examples, see class comments in OMeta2Examples category)

	- OMeta2Examples match: 5 with: #fact.
	- OMeta2Examples matchAll: '1234' with: #number.
	- OMeta2Examples matchAll: 'abc123' with: #identifier.
	- OMeta2Examples matchAll: #($a $b $c 1 2 3 #(4 5)) with: #structure.
	- OMeta2Examples matchAll: 'howdy' with: #greeting.

The general idea is that the examples progress in complexity: OMeta2Examples (trivial) -> OMeta2TreeExample (simple but actually does something useful) -> OMeta2LamdaCalculusParserExample (parses a simple language but doesn't do anything with it) -> OMeta2LispExample (parses a minimal subset of a real language and executes it.)  Also, for more samples of using the examples, see the tests which are currently all using the example parsers.

##### Notes
- OMeta2.pck.st overrides methods in OMeta2-stage1.st that are needed to load the package.  This is why stage1 has not been moved into a package (i.e. to not give the illusion that its contents can be changed and saved out once the full OMeta2 package has been loaded)
- Debugging support is weak (a known issue with OMeta in general... let's work to improve it)
- More test cases and examples are need.

### Misc notes: 


The *Tests.pck files contain the SUnit tests for their base package name and can be ignored unless you have problems or plan to make changes to the package.  (i.e. if you just want to use the package and it's working for you, don't worry about the test projects)

It is intended that this repository will *not* contain any non-permissive licenses (i.e. requiring more than attribution credit) such as GPL'd code to avoid potential issues.  Any ports that are distributed under less permissive licenses will be distributed via separate repositories as needed.


### Other packages

Also see this more comprehensive list of ports from the larger Cuis community
https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/OptionalPackagesForCuis.md
