
            Welcome to the Delphi Zip v1.60p Beta Release!
                   This is the Delphi Edition
                         Aug 23, 2001


This is the "revision p" beta release of Delphi Zip version 1.60.
This release is comprehensive - it includes all the files you need.


The Borland C++ Builder release is now distributed separately.


This release contains:

DZ-DEL160p.EXE  - overall Freeware Delphi Zip BETA v1.60p package.

The main setup directory defaults to C:\ZipBeta.
The rest of this document assumes you selected this directory for
the install.

Files in Main directory:

  \ZipBeta\README.TXT   - this file

  \ZipBeta\INSTALL.TXT - info. about how to install the VCLs and
                         run the demos
 
Subdirectories:

  \ZipBeta\DLL         - the ZIPDLL and UNZDLL DLL's (require Win32)
                         Put these into C:\WINDOWS\SYSTEM

  \ZipBeta\VCL         - Essential support files.  Includes the very
                         useful TZipMaster VCL, as well as the DLL import
                         specifications and callback files.  Put these
                         into a directory in your VCL search path.

  \ZipBeta\SFX         - Contains ZIPSFX.BIN, used to make self-extracting 
                         archives

  \ZipBeta\SFXsource   - Contains the source code for ZIPSFX.BIN, used to
                         make self-extracting executables.

  \ZipBeta\Demo1       - Delphi demo program #1.


                            IMPORTANT!!! 
                            ************

Also, each user project must specify which language to use for error
messages. For English users, include this in your .DPR files:
    {$R ZipMsgUS.res}


                     Installation Instructions

This ZIP should go into a base directory of \ZipBeta.

If you have installed an older version into a directory called
\ZipBeta, then you can install this one right over the top of it.

See this file for more installation instructions:
\ZipBeta\Install.txt


                      Distribution Policy
              Guidelines for Legal Re-distribution

  1) This applies to both end-users and developers.  The meat of
     this package is the .DLL files, and they are needed by all
     of your end-users if you use this ZIP package in your 
     program.
    
  2) You must not charge money for any part of the ZIP package.
     Warning:  The primary concern here is if you will market a
     new package that is only slightly more than a verbatim copy 
     of this package, or one of it's demos.  ANY release you sell
     is OK as long as you charge ONLY for the changes you make.

  3) You MUST either distribute the source code for this ZIP
     package, or give a WWW site where it can be obtained
     freely. This can be on a Help...About screen, in printed
     documentation, or in text files distributed with your 
     package.  Yes, it does seem odd to have this requirement
     for end-users who aren't programmers, but there is no
     mistake.  After all, it isn't hard to give them a URL
     and a few words explaining what it is for.

  4) You must document the Info-Zip's WWW home page URL, but 
     don't expect any help from the Info-Zip group, since my 
     release is only a derrivitive of their work.  They are 
     very busy doing support for their "official" releases.
     Since much of the DLL source code comes from them, they
     deserve to be mentioned.
        Info-Zip:   http://www.cdrom.com/pub/infozip/

  5) You must handle product support with your own end-users.
     This is needed because I don't have enough time to do
     support for end-users.

  6) I will handle support issues with programmers using this 
     package on a time-available basis. Since this is being
     distributed as freeware, you can't expect the kind of 
     support you'd get from a commercial vendor.  Please limit
     your questions to those that directly pertain to this
     ZIP package.

  7) You may NOT distribute executable versions of my demo
     programs to end-users without my approval.  These are 
     only example applications to teach you how to use
     this package. 


                    DLL Source Code in C
       
   The DLL source code is distributed separately due to it's
size, and the fact that most Delphi users of this package
probably don't want the C source for the DLL's.  The DLL 
source is also freeware, and will remain that way. 
The filename of the DLL source code is DZDLLSR6.ZIP, and 
it needs Borland C++ Builder v3 or v4.


                Problem Reports or Suggestions

We DO want to hear your ideas!  If you find a problem with
any part of this project, or if you just have an idea for
us to consider, send us e-mail!

But, please make sure that your bug has not already been
reported.  Check the "official" web site often:

http://www.geocities.com/SiliconValley/Network/2114/zipbeta.html
  
Eric Engler
englere@abraxis.com