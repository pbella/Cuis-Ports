Cuis-Ports
==========

These are various Smalltalk packages that have been ported to Cuis.  Where possible, the original source that the package is based on has been noted in this document as well as in the package description field.

Text Processing
===============

HTML - a tolerant HTML parser.  Does a decent job of consuming the various malformed/invalid HTML out on the web.  Based on HTML-sd.2.mcz found at http://squeaksource.com/@HDNjkoaXwriIV8js/Q0l6qq8Y

XML-Parser - I *believe* this is based on XML-Parser-AlexandreBergel.15.mcz but need to confirm.

XPath - Split out from, and depends on XMLParser.

VBRegex - A native regex implementation - no plugin required.  Based on http://www.squeaksource.com/Regex/VB-Regex-damienpollet.17.mcz

The *Tests.pck files contain the SUnit tests for their base package name and can be ignored unless you have problems or plan to make changes to the package.  (i.e. if you just want to use the package and it's working for you, don't worry about the test projects)

Math
====

3DTransform - Based on 3DTransform-pbm.19.mcz

FFI
===

FFI - Based on FFI-Kernel-ar.9.mcz

FFITests - Based on FFI-Tests-ar.2.mcz

OpenGL - Based on OpenGL-Core-jrd.6.mcz.  Depends on 3DTransform and FFI.

Note: after loading FFI projects, you must call 'Smalltalk recreateSpecialObjectsArray.'

Misc notes:
===========

It is intended that this repository will *not* contain any non-permissive licenses (i.e. requiring more than attribution credit) such as GPL'd code to avoid potential issues.  Any ports that are distributed under less permissive licenses will be distributed via separate repositories as needed.
