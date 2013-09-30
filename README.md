Cuis-Ports
==========

These are various Smalltalk packages that have been ported to Cuis.  Where possible, the original source that the package is based on has been noted in this document as well as in the package description field.

Text Processing
===============

HTML - a tolerant HTML parser.  Does a decent job of consuming the various malformed/invalid HTML out on the web.  Based on HTML-sd.2.mcz found at http://squeaksource.com/@HDNjkoaXwriIV8js/Q0l6qq8Y

XML-Parser - Based on http://squeaksource.cdn.st/XMLSupport/XML-Parser-NorbertHartl.141.mcz

XPath - Based on XML-Parser-AlexandreBergel.15.mcz XPath.  Split out from XML-Parser

VBRegex - A native regex implementation - no plugin required.  Based on http://www.squeaksource.com/Regex/VB-Regex-damienpollet.17.mcz


Math
====

3DTransform - Based on 3DTransform-pbm.19.mcz

FFI
===

FFI - Using FFI from https://github.com/bpieber/Cuis-StyledTextEditor 

OpenGL - Originally based on OpenGL-Core-jrd.6.mcz and have merged in some of the changes from OpenGL-Core-bf.17.mcz.  Depends on 3DTransform and FFI.  If you are running on a Unix or Linux system, see the comment in OGLUnix openGLLibraryName. This was pushed out prematurely and has at least a couple of issues yet to be resolved: font handling is broken, and there are at least one or two hacks that were forgotten about that need to be fixed. 

Note: after loading FFI projects, you must call 'Smalltalk recreateSpecialObjectsArray.'

Misc notes:
===========

The *Tests.pck files contain the SUnit tests for their base package name and can be ignored unless you have problems or plan to make changes to the package.  (i.e. if you just want to use the package and it's working for you, don't worry about the test projects)

It is intended that this repository will *not* contain any non-permissive licenses (i.e. requiring more than attribution credit) such as GPL'd code to avoid potential issues.  Any ports that are distributed under less permissive licenses will be distributed via separate repositories as needed.
