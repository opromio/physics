"""
Date. Winter Semester 2017
Python version: v2.7.15
"""

import timeit

"""
-------------------------------------------------------------------------------------
----------------------------  LZW DECOMPRESSOR  -------------------------------------
-------------------------------------------------------------------------------------
"""
with open("compressed_quijote.bin", "rb") as f, open("quijote_decompressed.txt", "w+") as output:
    start = timeit.default_timer()
    bit_string = ""
    byte = f.read(1)
    while len(byte) >= 1:  # Fem una string de la cadena de bits que hi ha al fitxer
        byte = ord(byte)
        bits = bin(byte)[2:].rjust(8, '0')
        bit_string += bits
        byte = f.read(1)

    padded_info = bit_string[:8]
    extra_padding = int(padded_info, 2)

    bit_string = bit_string[8:]
    encoded = bit_string[:-1 * extra_padding]
    # Funcionament analeg al compressor, llegim i anem afegint al diccionari on the fly
    c = 1
    dictionary = {"0": ''}
    dictionary.update({str(int(bin(c)[2:].zfill(1), 2)): chr(int(encoded[:8], 2))})

    text = chr(int(encoded[:8], 2))
    k = 0
    pbb = encoded[8:]
    finished = False
    while not finished:
        actual_k = 2 ** k
        for i in range(0, actual_k):
            pb = pbb[:k + 9]
            pbb = pbb[k + 9:]
            # Quan ja no queda mes que llegir, escrivim el text.
            if len(pb[:k + 1]) < 1 or len(pb[k + 1:]) < 1:
                output.write(text)
                print(timeit.default_timer() - start)
                finished = True
                # aixo es el caracter que tenim al diccionari
            word = str(dictionary[str(int(pb[:k + 1], 2))]) + chr(int(pb[k + 1:], 2))
            c += 1
            # guardem el codi binari associat a c, i el caracter que representen els 8
            dictionary.update({str(int(bin(c)[2:].zfill(k + 1), 2)): word})
            text += word
        k += 1

    stop = timeit.default_timer()
    print('Decompression time:', stop - start, '\n', 'Total time:', stop - start)
