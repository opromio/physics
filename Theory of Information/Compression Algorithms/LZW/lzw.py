import timeit


#-------------------------------------------------------------------------------------
#-------------------------------COMPRESSOR--------------------------------------------
#-------------------------------------------------------------------------------------

def Bit_converter(padded_encoded_text):
    b = bytearray()
    for i in range(0, len(padded_encoded_text), 8):
        byte = padded_encoded_text[i:i+8]
        b.append(int(byte, 2))
    return b
    
with open('quijote.txt','r') as f, open('binout.bin','wb') as output:
    start=timeit.default_timer()
    c=1
    first=f.read(1) #Llegim el primer caracter
    dictionary={first:bin(c)[2:]} #L afegim al dictionary
    encoded=str('{0:08b}'.format(ord(first))) #L 'escrivim' al output
    k=0
    g=True
    while g==True:
        actual_k=2**k 
        for i in range(0,actual_k):
            a=f.read(1)
            if not a: g=False
            while a in dictionary:
                char=f.read(1)
                if not char: break
                a+=char

    #   quan a ja no esta al diccionari
            if(len(a)==1): #si es un sol caracter
                c+=1
                dictionary.update({a:bin(c)[2:].zfill(k+1)}) #afegim al dict el binari del cont. amb la llarg. corresp. ie el seu codi.
                encoded+=bin(0)[2:].zfill(k+1)
                encoded+='{0:08b}'.format(ord(a))
#                out.write(bin(0)[2:].zfill(k+1))
#                out.write('{0:08b}'.format(ord(a)).zfill(k+1))

            elif(len(a)>1):#si es una cadena
                c+=1
                dictionary.update({a:bin(c)[2:].zfill(k+1)}) #afegim la cadena i el codi binari associat al contador.  
                encoded+=str(dictionary[str(a[:len(a)-1])]).zfill(k+1)
                encoded+='{0:08b}'.format(ord(str(a[len(a)-1])))
#                out.write(str(dictionary[str(a[:len(a)-1])]).zfill(k+1))       #escrivim el codi associat a la cadena que coneix i guard. la nova.
#                out.write('{0:08b}'.format(ord(str(a[len(a)-1]))))  #escrivim el separador en codi ASCII.
        k+=1
#Afegim els bits necessaris per a que el # bits total sigui multiple de 8 i escrivim el numero de bits afegits a l'inici del document
    extra_padding=8-len(encoded)%8
    for i in range(extra_padding):
        encoded += "0"
    padded_info = "{0:08b}".format(extra_padding)
    encoded= padded_info + encoded
    #Escrivim fitxer sortida
    output.write(bytes(Bit_converter(encoded)))

f.close()
output.close()
stop1=timeit.default_timer()
print('Compression time:', stop1-start)
#-------------------------------------------------------------------------------------
#----------------------------DESCOMPRESSOR--------------------------------------------
#-------------------------------------------------------------------------------------
with open("binout.bin","rb") as file, open("decomp.txt","w",encoding="utf8") as output:
    start = timeit.default_timer()
    bit_string = ""
    byte = file.read(1) 
    while(len(byte)>=1):#Fem una string de la cadena de bits que hi ha al fitxer
        byte = ord(byte)
        bits = bin(byte)[2:].rjust(8, '0')
        bit_string += bits
        byte = file.read(1)

    padded_info = bit_string[:8]
    extra_padding = int(padded_info, 2)

    bit_string= bit_string[8:] 
    encoded =  bit_string[:-1*extra_padding]
#Funcionament analeg al compressor, llegim i anem afegint al diccionari on the fly
    c=1   
    dictionary={"0":''}
    dictionary.update({str(int(bin(c)[2:].zfill(1),2)):chr(int(encoded[:8],2))})
    
    text=chr(int(encoded[:8],2))
    k=0
    pbb=encoded[8:]
    while True:
        actual_k=2**k
        for i in range(0,actual_k):
            pb=pbb[:k+9]
            pbb=pbb[k+9:]

            if(len(pb[:k+1])<1 or len(pb[k+1:])<1):#Quan ja no queda mes que llegir, escrivim el text.
                output.write(text)
                print(timeit.default_timer()-start)
                exit()
            
            word=str(dictionary[str(int(pb[:k+1],2))])+chr(int(pb[k+1:],2)) #aixo es el caracter que tenim al diccionari  
            c+=1
            dictionary.update({str(int(bin(c)[2:].zfill(k+1),2)) : word}) #guardem el codi binari associat a c, i el caracter que representen els 8 
            text+=word
#            output.write(word)
        k+=1
        

stop=timeit.default_timer()
print('Decompression time:', stop-stop1,'\n', 'Total time:', stop-start)
