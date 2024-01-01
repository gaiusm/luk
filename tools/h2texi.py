#!/usr/bin/python

# Copyright (C) 2011
#               Free Software Foundation, Inc.
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Modula-2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Modula-2; see the file COPYING.  If not, write to the
# Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
#

import sys
import os
import glob
import string
import getopt

libraryClassifications = [['ch', 'LUK modules']]

def initState ():
    global inFunc, inType, inDefine
    inFunc, inType, inDefine = False, False, False


#
#  displayLibraryClass - displays a node for a library directory and invokes
#                        a routine to summarize each module
#

def displayLibraryClass():
    global sourceDir, up
    previous = ""

    next=""
    # next=libraryClassifications[1][1]
    i = 0
    l = libraryClassifications[i]

    while True:
        print("@node " + l[1] + ", " + next + ", " + previous + ", " + up)
        print("@section " + l[1])
        print("")
        displayModules(l[0], os.path.join(sourceDir, l[0]))
        print("")
        print("@c ---------------------------------------------------------------------")
        previous = l[1]
        i += 1
        if i == len(libraryClassifications):
            break
        l = libraryClassifications[i]
        if i+1 == len(libraryClassifications):
            next = ""
        else:
            next = libraryClassifications[i+1][1]

#
#  displayMenu - displays the top level menu for library documentation
#

def displayMenu():
    print("@menu")
    for l in libraryClassifications:
        print("* " + l[0] + "::" + l[1])
    print("@end menu")

    print("\n")
    print("@c =====================================================================")
    print("\n")


#
#  removeInitialComments - removes any (* *) at the top of the definition module
#

def removeInitialComments (file, line):
    while (string.find(line, "*/") == -1):
        line = file.readline()

#
#  removeFields - removes Author/Date/Last edit/SYSTEM/Revision fields from a comment within the start
#                 of a definition module
#

def removeFields (file, line):
    while (string.find(line, "*/") == -1):
        if (string.find(line, "Author") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Last edit") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "LastEdit") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Last update") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Date") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Title") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "Revision") != -1) and (string.find(line, ":") != -1):
            line = file.readline()
        elif (string.find(line, "System") != -1) and (string.find(line, ":") != -1) and (string.find(line, "Description:") == -1):
            line = file.readline()
        elif (string.find(line, "SYSTEM") != -1) and (string.find(line, ":") != -1) and (string.find(line, "Description:") == -1):
            line = file.readline()
        else:
            print(line.rstrip ().replace ("{", "@{").replace ("}", "@}"))
            line = file.readline()
    print(string.rstrip(line))


#
#  checkIndex
#

def checkIndex (line):
    global inVar, inType, inDefine

    words = line.split ()
    function = ""
    if (len(words)>1) and (words[0] == "extern"):
        inFunc = False
        inType = False
        inDefine = False
        function = line[len("extern"):]

    if (len(line)>1) and (line[0:2] == '/*'):
        inFunc = False
        inType = False
        inDefine = False
    elif line == "#define":
        inFunc = False
        inDefine = True
        inType = False
        return
    elif line == "typedef":
        inFunc = False
        inType = True
        inDefine = False
        return

    if inType:
        words = string.lstrip(line)
        if string.find(words, '=') != -1:
            word = words.split ("=")
            if (len(word[0])>0) and (word[0][0] != '_'):
                print("@findex " + string.rstrip(word[0]) + " (type)")
        else:
            word = words.split()
            if (len(word)>1) and (word[1] == ';'):
                # hidden type
                if (len(word[0])>0) and (word[0][0] != '_'):
                    print("@findex " + string.rstrip(word[0]) + " (type)")

    if inDefine:
        words = line.split ()
        if len(words)>1:
            print("@findex " + words[1] + " (define)")

    if function != "":
        name = function.split ("(")
        if name[0] != "":
            proc = name[0].split()[-1]
            proc = proc.split('*')[-1]
            if proc != "":
                print("@findex " + proc)


#
#  parseHeader
#

def parseHeader (dir, file, needPage):
    print("")
    if os.path.exists(os.path.join(dir, file)):
        f = open(os.path.join(dir, file), 'r')
    initState()
    line = f.readline()
    while (line.find ("/*") != -1):
        removeInitialComments(f, line)
        line = f.readline()

    print("@example")
    print(line.rstrip ())
    line = f.readline()
    if len(line.rstrip()) == 0:
        print(line.rstrip ().replace ("{", "@{").replace ("}", "@}"))
        line = f.readline()
        if (line.find ("/*") != -1):
            removeFields (f, line)
        else:
            print (line.rstrip ())
    else:
        print (line.rstrip ())

    line = f.readline()
    while line:
        line = line.rstrip ()
        checkIndex(line)
        print (line.rstrip ().replace ("{", "@{").replace ("}", "@}"))
        line = f.readline()
    print("@end example")
    if needPage:
        print("@page")
    f.close()

def parseModules (up, dir, listOfModules):
    previous = ""
    i = 0
    if len(listOfModules)>1:
        next = dir + "/" + listOfModules[1][:-4]
    else:
        next = ""

    while i<len(listOfModules):
       print("@node " + listOfModules[i] + ", " + next + ", " + previous + ", " + up)
       print("@subsection " + listOfModules[i])
       parseHeader(dir, listOfModules[i], True)
       print("\n")
       previous = listOfModules[i]
       i = i + 1
       if i+1<len(listOfModules):
           next = listOfModules[i+1]
       else:
           next = ""


#
#  doCat - displays the contents of dir/file to stdout
#

def doCat (dir, file):
    file = open(os.path.join(dir, file), 'r')
    while True:
        line = file.readline()
        if not line: break
        print(string.rstrip(line))
    file.close()


#
#  moduleMenu - generates a simple menu for all definition modules
#               in dir
#

def moduleMenu (dir):
    print("@menu")
    listOfFiles = os.listdir(dir)
    listOfFiles.sort()
    for file in listOfFiles:
        if os.path.isfile(os.path.join(dir, file)):
            if (len(file)>4) and (file[-2:] == '.h'):
                print("* " + dir + "/" + file[:-2] + "::" + file)
    print("@end menu")
    print("\n")

#
#  displayModules - walks though the files in dir and parses
#                   definition modules and includes README.texi
#

def displayModules(up, dir):
    if os.path.exists(os.path.join(dir, "README.texi")):
        doCat(dir, "README.texi")

    moduleMenu(dir)
    listOfModules = []
    listOfFiles = os.listdir(dir)
    listOfFiles.sort()
    listOfFiles = list(dict.fromkeys(listOfFiles).keys())
    for file in listOfFiles:
        if os.path.isfile(os.path.join(dir, file)):
            if (len(file)>2) and (file[-2:] == '.h'):
                listOfModules = listOfModules + [file]
    parseModules(up, dir, listOfModules)

def displayCopyright ():
    print("@c Copyright (C) 2011")
    print("@c Free Software Foundation, Inc.")
    print("""
@c Permission is granted to copy, distribute and/or modify this document
@c under the terms of the GNU Free Documentation License, Version 1.2 or
@c any later version published by the Free Software Foundation.
""")

def Usage():
    print("h2texi.py [-h][-ssourcedir][-uupnode][-ffilename]")

def collectArgs():
    sourceDir="."
    filename=""
    up=""
    try:
        optlist, list = getopt.getopt(sys.argv[1:],':hs:f:u:')
    except getopt.GetoptError:
        Usage()
        os.sys.exit(1)
    for opt in optlist:
        if opt[0] == '-h':
            Usage()
        if opt[0] == '-f':
            filename = opt[1]
        if opt[0] == '-s':
            sourceDir = opt[1]
        if opt[0] == '-u':
            up = opt[1]
    return sourceDir, filename, up


sourceDir, filename, up = collectArgs()

if filename == "":
    displayCopyright()
    displayMenu()
    displayLibraryClass()
else:
    parseHeader(sourceDir, sourceDir, filename, False)
