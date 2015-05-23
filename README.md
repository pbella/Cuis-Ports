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


### Math


3DTransform - Based on 3DTransform-pbm.19.mcz


### OpenGL

OpenGL - Originally based on OpenGL-Core-jrd.6.mcz and have merged in some of the changes from OpenGL-Core-bf.17.mcz.  

Depends on 3DTransform and FFI.  If you are running on a Unix or Linux system, see the comment in OGLUnix openGLLibraryName. This was pushed out prematurely and has at least a couple of issues yet to be resolved: font handling is broken, and there are at least one or two hacks that were forgotten about that need to be fixed. 

FFI - See https://github.com/hhzl/Cuis-FFI

Note: after loading FFI projects, you must call 
    
	Smalltalk recreateSpecialObjectsArray.
	

### OMeta 2

Currently needs to be filed in/installed, in sequence:

1. OMeta2-stage1.st
2. OMeta2.pck.st
3. OMeta2Tests.pck.st (optional)

Notes:
- Only very basic functionality has been verified.  Please report problems/bugs.
- Requires Cuis4.2-2330 with the 2331 and 2332 changesets provided by Juan applied.
- stage2b overwrites methods in stage1 that are needed to load stage2b
- stage2a (a single method) is likely to experience breakage in future releases as it overrides an existing Cuis method and will need future changes merged.
- Suggestions as to how to best package this are appreciated.  I'm currently thinking of merging stage1 and 2a into a file named OMeta2Preload.st and creating a package for stage2b called OMeta2.pck.st.  The issue in creating a package for the preload is that it gives the mistaken impression that it could be modified and saved which it can't be once stage2 is loaded.
- Debugging support is weak (a known issue with OMeta in general... let's work to improve it)
- Test cases need to be created.
- OMeta2Examples has a few simple ones but more/better are needed.  Right now you can look at the <a href="http://tinlizzie.org/ometa-js/#Sample_Project">Javascript implementation</a> for some inspiration.

### Misc notes: 


The *Tests.pck files contain the SUnit tests for their base package name and can be ignored unless you have problems or plan to make changes to the package.  (i.e. if you just want to use the package and it's working for you, don't worry about the test projects)

It is intended that this repository will *not* contain any non-permissive licenses (i.e. requiring more than attribution credit) such as GPL'd code to avoid potential issues.  Any ports that are distributed under less permissive licenses will be distributed via separate repositories as needed.


### Other packages

See also this more comprehensive list of ports from the larger Cuis community
https://github.com/jvuletich/Cuis/blob/master/ListOfCuisPackages.md
