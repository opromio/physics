# Compressor algorithms
During the Information Theory course that I took in the winter semester of 2017,
we studied the redundancy on language and how this was abused by compressor algorithms
to minimize the information to the least amount  **without losing any information**. This way, you could 
always recover the original file exactly as it was. 

As voluntary projects we were challenged with building two compressor using different techniques; Huffman and
Lempez-Ziv-Welch.

## Huffman cofing

### Introduction
From [wikipedia](https://en.wikipedia.org/wiki/Huffman_coding):

In computer science and information theory, a Huffman code is a particular type of optimal prefix code that is commonly
 used for lossless data compression. The process of finding or using such a code proceeds by means of Huffman coding, 
 an algorithm developed by David A. Huffman while he was a Sc.D. student at MIT, and published in the 1952 paper 
 "A Method for the Construction of Minimum-Redundancy Codes".

The technique works by creating a binary tree of nodes. These can be stored in a regular array, the size of which 
depends on the number of symbols, *N*. A node can be either a leaf node or an internal node. Initially, all nodes are 
leaf nodes, which contain the symbol itself, the weight (frequency of appearance) of the symbol and optionally, a link
 to a parent node which makes it easy to read the code (in reverse) starting from a leaf node. Internal nodes contain 
 a weight, links to two child nodes and an optional link to a parent node. As a common convention, bit '0' represents 
 following the left child and bit '1' represents following the right child. A finished tree has up to *n* leaf nodes
  and *n-1* internal nodes. A Huffman tree that omits unused symbols produces the most optimal code lengths.
 ### Installation requirements
 Python version: 2.7.15
 
 There are no external libraries or additional requirements in order to run this code.
 
For running the code you would only need to open a terminal, move to the Huffman folder and execute:
 ```
python HuffmanCode.py
```
This will generate both the compressed file (compressed_quijote.bin) and the decompressed file (decompressed_quijote.txt).

If you want to make sure that the decompressed file is indeed identical to the original file run (from a Unix terminal):
```
diff ../quijote.txt quijote_decompressed.txt
```
The output should be nothing.

## Lempel-Ziv-Welch


From [wikipedia](https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch):

### Introduction

Lempel–Ziv–Welch (LZW) is a universal lossless data compression algorithm created by Abraham Lempel, Jacob Ziv, and Terry Welch. It was published by Welch in 1984 as an improved implementation of the LZ78 algorithm published by Lempel and Ziv in 1978. The algorithm is simple to implement and has the potential for very high throughput in hardware implementations. It is the algorithm of the widely used Unix file compression utility compress and is used in the GIF image format.

### *Pseudo-code*

A high level view of the encoding algorithm is shown here:
 1. Initialize the dictionary to contain all strings of length one.
 2. Find the longest string W in the dictionary that matches the current input.
 3. Emit the dictionary index for W to output and remove W from the input.
 4. Add W followed by the next symbol in the input to the dictionary.
 5. Go to Step 2.
 
 ### Installation requirements
 Python version: 2.7.15
 
 There are no external libraries or additional requirements in order to run this code.
 
For running the code you would only need to open a terminal, move to the LZW folder and execute:
 ```
python lzw_compressor.py
```
This will generate a compressed_quijote.bin file with the compressed version. For recovering the original file:
```
python lzw_decompressor.py
```
* Note The decompressor takes around 2 minutes to decompress. This can be improved by a lot and needs reviewing.

If you want to make sure that the decompressed file is indeed identical to the original file run (from a Unix terminal):
```
diff ../quijote.txt quijote_decompressed.txt
```
The output should be nothing.
