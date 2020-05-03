import heapq
from collections import defaultdict
import timeit

class HuffmanCode:
    
    def __init__(self, text):
        self.text=text
        self.heap=[]
        self.code={}
        self.reverse_code={}
  
    def make_freq_dict(self, text):
        frequency={}
        for character in text:
            try:
                frequency[character] += 1
            except KeyError:
                frequency[character] = 1
        return frequency
        
#            if not character in frequency:
#                frequency[character] = 0
#            frequency[character] += 1
#        return frequency

   
    def make_heap(self,frequency): #Fem la pila
        for key in frequency:
            heapq.heappush(self.heap, (frequency[key], key))

    def encoder(self):
    
        self.code=defaultdict(list)
        
        while(len(self.heap)>1):
            a=heapq.heappop(self.heap)
            
            for char in a[1]: #Assignem 1
                self.code[char]="1" + "".join(map(str,self.code[char])) 
    
            b=heapq.heappop(self.heap)
            
            for char in b[1]:#Assignem 0
                self.code[char]="0" + "".join(map(str,self.code[char]))

            heapq.heappush(self.heap,(a[0]+b[0],a[1]+b[1]))
            
        for key in self.code:#Construim el diccionari invers que ens servira per descomprimir el fitxer
            self.reverse_code[self.code[key]]=key
            
    def get_encoded_text(self,text): #Escrivim el text ja codificat
        encoded_text=""
        for character in text:
            encoded_text+="".join(map(str,self.code[character]))
        #El fitxer ha de contenir un nombre total de bits multiple de 8
        extra_padding=8-len(encoded_text)%8
        #Afegim 0's per a que ho sigui
        for i in range(extra_padding):
            encoded_text += "0"
        #I escrivim al principi del document el nombre de 0's que hem afegit
        padded_info = "{0:08b}".format(extra_padding)
        encoded_text = padded_info + encoded_text
        return encoded_text
        #Per a escriure binari a l'output fem un bytearray
    def Bit_converter(self, padded_encoded_text):
        b = bytearray()
        for i in range(0, len(padded_encoded_text), 8):
            byte = padded_encoded_text[i:i+8]
            b.append(int(byte, 2))
        return b
#   Eliminem els bits extra que hem afegit
    def remove_extra_bits(self, padded_encoded_text):
        padded_info = padded_encoded_text[:8]
        extra_padding = int(padded_info, 2)

        padded_encoded_text = padded_encoded_text[8:] 
        encoded_text = padded_encoded_text[:-1*extra_padding]

        return encoded_text


    def decoder(self, encoded_text):
        encoded=""
        text="" 
        for bit in encoded_text:
            encoded+=bit
            if(encoded in self.reverse_code):
                char=self.reverse_code[encoded]
                text+= char
                encoded=""
        return text

    
#__PROGRAMA PRINCIPAL_______________________________________________________
with open("quijote.txt","r+") as file, open("compquijote.bin","wb") as output:
    start = timeit.default_timer()
    text=file.read()
    text=text.rstrip()
    
    h = HuffmanCode(text)
    freq_dict= h.make_freq_dict(text) #fem el diccionari de freq.
    h.make_heap(freq_dict) #fem la pila
    h.encoder() #assignem codis a cada element en funcio de la seva freq.
    encoded=h.get_encoded_text(text)#codifiquem el text
    output.write(bytes(h.Bit_converter(encoded)))#escrivim el fitxer de sortida
    file.close()
    stop = timeit.default_timer()
    print('Compression time:', stop-start)    


#--------------------------DESCOMPRESSOR--------------------------------   
with open("compquijote.bin","rb") as file, open("decompquijote.txt","w") as output:
    bit_string = ""
    byte = file.read(1) #Fem una string que contingui tots els bits del fitxer
    while(len(byte)>=1):
        byte = ord(byte)
        bits = bin(byte)[2:].rjust(8, '0')
        bit_string += bits
        byte = file.read(1)

    encoded_text = h.remove_extra_bits(bit_string) #Eliminem els bits extra
    decompressed_text = h.decoder(encoded_text)#Descodifiquem

    output.write(decompressed_text)
    file.close()
    
#-----------TIMER-------------------------------------------------------
stop2 = timeit.default_timer()
print('Decompression time:',stop2-stop,'\n','Total time:', stop2-start)    
exit()
#-----------------------------------------------------------------------
    













































