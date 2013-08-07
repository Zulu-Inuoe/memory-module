# memory-module

## Overview

memory-module is Windows shared library (or Dynamic Link Library DLL) [Portable Executable](http://en.wikipedia.org/wiki/Portable_Executable) format file loader.
It serves the same role as the operating-system provided LoadLibrary set of API's.
memory-module is a straightforward port of [fancycode/MemoryModule](https://github.com/fancycode/MemoryModule) and thus not very lispy.

Goals of the project include integration with [CFFI](https://github.com/cffi/cffi), util libraries for dumping images with embedded DLLs, and support for ELF-based shared-libraries.

This code is still experimental and nowhere near mature. Use at your own risk.
