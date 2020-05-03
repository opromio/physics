"""
Date. Winter Semester 2017
Python version: v2.7.15
"""

import timeit

"""
-------------------------------------------------------------------------------------
------------------------------  LZW COMPRESSOR  -------------------------------------
-------------------------------------------------------------------------------------
"""


def bit_converter(padded_encoded_text):
    b = bytearray()
    for i in range(0, len(padded_encoded_text), 8):
        byte = padded_encoded_text[i:i + 8]
        b.append(int(byte, 2))
    return b


with open('../quijote.txt', 'r') as f, open('compressed_quijote.bin', 'wb') as output:
    start = timeit.default_timer()
    c = 1
    first = f.read(1)  # Llegim el primer caracter
    dictionary = {first: bin(c)[2:]}  # L afegim al dictionary
    encoded = str('{0:08b}'.format(ord(first)))  # L 'escrivim' al output
    k = 0
    g = True
    while g:
        actual_k = 2 ** k
        for i in range(0, actual_k):
            a = f.read(1)
            if not a: g = False
            while a in dictionary:
                char = f.read(1)
                if not char: break
                a += char

            #   quan a ja no esta al diccionari
            if (len(a) == 1):  # si es un sol caracter
                c += 1
                dictionary.update({a: bin(c)[2:].zfill(
                    k + 1)})  # afegim al dict el binari del cont. amb la llarg. corresp. ie el seu codi.
                encoded += bin(0)[2:].zfill(k + 1)
                encoded += '{0:08b}'.format(ord(a))
            #                out.write(bin(0)[2:].zfill(k+1))
            #                out.write('{0:08b}'.format(ord(a)).zfill(k+1))

            elif (len(a) > 1):  # si es una cadena
                c += 1
                dictionary.update(
                    {a: bin(c)[2:].zfill(k + 1)})  # afegim la cadena i el codi binari associat al contador.
                encoded += str(dictionary[str(a[:len(a) - 1])]).zfill(k + 1)
                encoded += '{0:08b}'.format(ord(str(a[len(a) - 1])))
        #                out.write(str(dictionary[str(a[:len(a)-1])]).zfill(k+1))       #escrivim el codi associat a la cadena que coneix i guard. la nova.
        #                out.write('{0:08b}'.format(ord(str(a[len(a)-1]))))  #escrivim el separador en codi ASCII.
        k += 1
    # Afegim els bits necessaris per a que el # bits total sigui multiple de 8 i escrivim el numero de bits afegits a l'inici del document
    extra_padding = 8 - len(encoded) % 8
    for i in range(extra_padding):
        encoded += "0"
    padded_info = "{0:08b}".format(extra_padding)
    encoded = padded_info + encoded
    # Escrivim fitxer sortida
    output.write(bytes(bit_converter(encoded)))

f.close()
output.close()
stop1 = timeit.default_timer()
print('Compression time:', stop1 - start)

