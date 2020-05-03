# Lempel-Ziv-Welch


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
