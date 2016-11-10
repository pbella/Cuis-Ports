Cuis-Ports
==========

This repository contains various Smalltalk packages that have been ported to Cuis.  Where possible, the original source that the package is based on has been noted in this document as well as in the package description field.

### Text Processing

#### XML

XML-Parser - Obsolete and has been removed... replaced by optional YAXO package in main Cuis repository.

XPath - Based on XML-Parser-AlexandreBergel.15.mcz XPath.  Split out from XML-Parser

Installation

     | slash |

    slash :=  FileDirectory slash.

    {
         '..', slash, 'Cuis-Ports', slash, 'XPath.pck.st'.
         '..', slash, 'Cuis-Ports', slash, 'XPathTests.pck.st' .
    }
    do:
    [ :fileName | CodePackageFile installPackageStream:
                 (FileStream concreteStream readOnlyFileNamed: fileName)
    ]    

	 
#### HTML

HTML - a tolerant HTML parser.  Does a decent job of consuming the various malformed/invalid HTML out on the web.  Based on HTML-sd.2.mcz found at http://squeaksource.com/@HDNjkoaXwriIV8js/Q0l6qq8Y


#### Regular Expressions

VBRegex - A native regex implementation - no plugin required.
Based on http://www.squeaksource.com/Regex/VB-Regex-damienpollet.17.mcz


#### Other

BTree - Jonathan Kelly's BTree implementation

#### Math

Moved to https://github.com/pbella/Cuis-OpenGL

#### OpenGL

Moved to https://github.com/pbella/Cuis-OpenGL

#### OMeta 2

Moved to https://github.com/pbella/OMeta-Cuis

### Misc notes: 


The *Tests.pck.st files contain the SUnit tests for their base package name and can be ignored unless you have problems or plan to make changes to the package.  (i.e. if you just want to use the package and it's working for you, don't worry about the test projects)

It is intended that this repository will *not* contain any non-permissive licenses (i.e. requiring more than attribution credit) such as GPL'd code to avoid potential issues.  Any ports that are distributed under less permissive licenses will be distributed via separate repositories as needed.


### Other packages

Also see this more comprehensive list of ports from the larger Cuis community
https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/OptionalPackagesForCuis.md
