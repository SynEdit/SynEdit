Unicode SynEdit CodeFolding branch
==================================

In this branch I will attempt to merge the improvements made to SynEdit by the Letterpress project (https://sourceforge.net/projects/letterpress/) including eventually the Code folding stuff.

Letterpress is a major departure from the existing code base:
a) has removed SYN_CLX IFDEFs and support for earlier version of DELPHI using generics in a couple of places.
b) used the SynUnihighlighter as the basis for syntax highlighting and has dropped support for existing highlighters

The plan is to merge features one by one starting with the improved WordWrap plugin and printing later moving to Code Folding.

Unicode SynEdit
===============

This is the Unicode version of SynEdit, a syntax highlighting editor that had
originally been designed for ANSI character sets.

For the available information check out the "Unicode SynEdit" Web-site at:

  http://mh-nexus.de/en/unisynedit.php

The ANSI version of SynEdit is located at:

  http://synedit.sourceforge.net

The SynEdit project has also a page at SourceForge

  http://sourceforge.net/projects/synedit

where you may find additional information.

Installation note: The Unicode and ANSI version of SynEdit may not be used
together on the same system. If you used ANSI SynEdit before delete every DCU
and BPL it created. Also make sure your library/source path in the environment
options is set correctly.

To get started check out the different examples. If you have questions, please
subscribe to the SynEdit user list and ask there, if you want to hack SynEdit
you should subscribe to the SynEdit developer list. General feedback and
suggestions or fixes are welcome.

There is no documentation yet, help in this area is especially welcome!


Historical Note
---------------

SynEdit has been started as an attempt to continue the no longer maintained
sources of the mwEdit project.The last public version 0.92a of mwEdit can be
found at the SynEdit website in the download section.
The mwEdit project was started in 1998 by Martin Waldenburg, aim was to produce
a syntax highlighting editor component for an Open Source IDE. In the 15 months
that mwEdit was developped in public Martin was increasingly dissatisfied with
the direction the development of mwEdit took, so he finally requested that his
name and initials had to be removed from the project.
Therefore this is now called SynEdit, and it is an Open Source project under the
MPL (Mozilla Public License, but see the individual source files for the terms).
There would however be no such project if Martin had not started to develop
mwEdit, so we would like to thank Martin Waldenburg for his efforts, and of
course all the other developers of the mwEdit project.

$Id: Readme.txt,v 1.3.2.2 2008/11/08 15:48:34 maelh Exp $
