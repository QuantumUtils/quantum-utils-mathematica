#!/usr/bin/python
# -*- coding: utf-8 -*-
##
# This package is part of QuantumUtils for Mathematica.
## 
# Copyright (c) 2015 and later,
# Christopher J. Wood, Christopher E. Granade, Ian N. Hincks.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 3. Neither the name of quantum-utils-mathematica nor the names of its
#    contributors may be used to endorse or promote products derived from  this
#    software without specific prior written permission.
##
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THEIMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE AREDISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLEFOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIALDAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS ORSERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVERCAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##

## DOCSTRING #########################################################

"""
This module defines a grammar for a restricted subset of the Mathematica
language, suitable for interprocess communication (IPC) between Python and
Mathematica.

Known Issues:
- Due to a bug in the most recent versions of PyPEG2, this module
  only works with the HEAD of the repo (as of 7 April 2015).
- Functions with no arguments get truncated to just the lone symbol,
  e. g.: List[] parses as List.
- Atomic data types such as floating point numbers are
  not yet supported.
- The String grammar element is buried one level too deep; e. g.:
  parse('"a"', MExpression) is embedded in an MExpression with
  head and body both None, as opposed to the behavior of Integer.
- Forms such as f[a][b] are not yet supported.
- Shorthand forms such as ; are not yet supported.
- PyPEG2's compose() function doesn't yet play well with this
  grammar.
"""

## FEATURES ##########################################################

from __future__ import division

## IMPORTS ###########################################################

from pypeg2 import Symbol, Literal, parse, optional, csl, attr
import re

## CLASSES ###########################################################

class MathematicaObject(object):
    head = ""
    body = []

    def __repr__(self):
        if self.body is not None:
            return "{}[{}]".format(self.head, ", ".join(map(repr, self.body)))
        else:
            return "{}".format(self.head)
    def __str__(self):
        return repr(self)

## GRAMMAR ELEMENTS ##################################################

class MSymbol(MathematicaObject, Symbol):
    Symbol.regex = re.compile(r'[\w\`]+')

    head = "Symbol"

    @property
    def body(self):
        return ['"{}"'.format(self)]

class Integer(MathematicaObject, int):
    grammar = re.compile(r'[+-]?[0-9]+')
    head = "Integer"

    @property
    def body(self):
        return [int(self)]

class String(MathematicaObject, str):
    grammar = Literal('"'), re.compile(r'([^\"]*)'), Literal('"')
    head = "String"

    @property
    def body(self):
        return [str(self[:])]

    def __repr__(self):
        return '"{}"'.format(str(self[:]))
    

atom = [Integer, String]

class MExpression(MathematicaObject, list):
    head = None
    body = None

class List(MExpression):
    head = MSymbol("List")

    grammar = (
        Literal("{"), optional(attr("body", csl(MExpression))), Literal("}")
    )


# Since MExpression is recursive, we need to define the class,
# then the grammar. Moreover, since it depends on List and other
# such things, we need to put it last.
MExpression.grammar = [
    (
        attr("head", MSymbol), Literal("["), optional(attr("body", csl(MExpression))), Literal("]")
    ),
    attr("head", MSymbol),
    List,
    atom
]


## TESTING ###########################################################

if __name__ == "__main__":
    print parse("ab`c", MExpression)
    print parse('12', MExpression)
    print parse('"a"', MExpression)
    print parse("List", MExpression)
    print parse("List[]", MExpression)
    print parse("List[foo]", MExpression)
    print parse("List[foo[bar, a]]", MExpression)
    print parse('{a, 12, "foo"}', MExpression)
    print parse('{a, 12, 12}', MExpression)

