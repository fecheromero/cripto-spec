#include <iostream>
#include <climits>
#include <cmath>
#include <cstring>


typedef struct {
    int wordSize;
    int rounds;
    int keyWordsAmount;
    int beta;
    int alfa;
} speckConfig;

speckConfig  CONFIG;
void obtainConfig(){
    speckConfig aConfig;
    aConfig.wordSize=16;
    aConfig.keyWordsAmount=4;
    aConfig.alfa=7;
    aConfig.beta=2;
    aConfig.rounds=22;
    CONFIG=aConfig;
}

typedef struct word {
    uint16_t value;

    word operator+(word otherword) const {
        word wordRdo;
        int value1=value;
        int value2=otherword.value;
        wordRdo.value =  (value1 + value2) % (int) pow(2,CONFIG.wordSize);
        return wordRdo;
    }
    word operator-(word otherword) const {
        word wordRdo;
        int value1=value;
        int value2=otherword.value;
        if(value1-value2<0){
            wordRdo.value= pow(2,CONFIG.wordSize)+(value1-value2);
        }
        else{
        wordRdo.value =  (value1 - value2);
        }
        return wordRdo;
    }
    word operator<<(int shift) const {
        word wordRdo;
        wordRdo.value=(value << shift) | (value >> (sizeof(value) * CHAR_BIT - shift));
        return wordRdo;
    }

    word operator>>(int shift) const {
       word wordRdo;
        wordRdo.value=(value >> shift) | (value << (sizeof(value) * CHAR_BIT - shift));

        return wordRdo;
    }

    word operator|(word anotherWord) const {
        word wordRdo;
        wordRdo.value=value ^ anotherWord.value;

        return wordRdo;
    }
    word operator |(int aValue) const{
        uint16_t v1=aValue;
        word newWord;
        newWord.value=value^v1;
        return newWord;
    }

};
typedef struct {
    word word1;
    word word2;
} block;


block roundFunction(block aBlock, word key) {
    block newBlock;
    newBlock.word1=((aBlock.word1 >> CONFIG.alfa) + aBlock.word2) | key;
    newBlock.word2=(aBlock.word2 << CONFIG.beta) | newBlock.word1;
    return newBlock;
};

block decriptRoundFunction(block aBlock, word key) {
    block newBlock;
    newBlock.word2=(aBlock.word1 | aBlock.word2) >> CONFIG.beta;
    newBlock.word1=((aBlock.word1 | key) - newBlock.word2 ) << CONFIG.alfa  ;
    return newBlock;
};


block speckEncript(block aBlock,word* keys){
    for(int i=0;i<CONFIG.rounds;i++){
        aBlock=roundFunction(aBlock,keys[i]);
    }
    return aBlock;
}

block speckDecript(block aBlock,word* keys){
    for(int i=CONFIG.rounds-1;i>=0;i--){
        aBlock=decriptRoundFunction(aBlock,keys[i]);
    }
    return aBlock;
}

void keyComplete(word* keyWords,word* keys){
    keys[0]=keyWords[CONFIG.keyWordsAmount-1];
    word l[CONFIG.rounds-1];

    for(int k=0;k<CONFIG.keyWordsAmount-1;k++){
        l[k]=keyWords[CONFIG.keyWordsAmount-2-k];
    }
    for(int i=0;i<=CONFIG.rounds-2;i++){
        l[i+CONFIG.keyWordsAmount-1]=(keys[i] +(l[i] >> CONFIG.alfa) | i);
        keys[i+1]= (keys[i] << CONFIG.beta) | l[i+CONFIG.keyWordsAmount-1];
    }
}
int fileSize(FILE* bmpFile){
    fseek(bmpFile, 0, SEEK_END);
    int size = ftell(bmpFile);
    fseek(bmpFile,0,SEEK_SET);
    return size;
}

unsigned bmpDataSize(FILE* bmpFile){
    fflush(bmpFile);
    fseek(bmpFile, 0, SEEK_END);
    unsigned size = ftell(bmpFile);
    fseek(bmpFile,0,SEEK_SET);
    return size-54;
}
void loadBMPContain(uint16_t * header,uint16_t * data,FILE* bmpFile){
    unsigned size= fileSize(bmpFile);
    fread(header,1,54,bmpFile);
    fread(data,1,size,bmpFile);
}
void cipherBMPContain(uint16_t*  cipherText,uint16_t * contain,word* keys,int size){
    block auxBlock;
    for(int j=1;j<=(size/(CONFIG.wordSize/8));j=j+(CONFIG.wordSize/8)){
        auxBlock.word1.value=contain[j];
        auxBlock.word2.value=contain[j+1];
        block cipherBlock;
        cipherBlock=speckEncript(auxBlock,keys);
        cipherText[j]=cipherBlock.word1.value;
        cipherText[j+1]=cipherBlock.word2.value;
    }
}
void decipherBMPContain(uint16_t*  decipherText,uint16_t * contain,word* keys,int size){
    block auxBlock;
    for(int j=1;j<=(size/(CONFIG.wordSize/8));j=j+(CONFIG.wordSize/8)){
        auxBlock.word1.value=contain[j];
        auxBlock.word2.value=contain[j+1];
        block cipherBlock;
        cipherBlock=speckDecript(auxBlock,keys);
        decipherText[j]=cipherBlock.word1.value;
        decipherText[j+1]=cipherBlock.word2.value;
    }
}
int main() {
    obtainConfig();

    word* keys=(word*) calloc(CONFIG.rounds,CONFIG.wordSize/8);
    word* keysSelected=(word*) calloc(CONFIG.keyWordsAmount,CONFIG.wordSize/8);

    word word1,word2,word3,word4;

    char* rutaDeImg=(char *)malloc((sizeof(char)) * 255);
    char* rutaDeImgCifrada=(char *)malloc((sizeof(char)) * 255);
    char* rutaDeImgDecifrada=(char *)malloc((sizeof(char)) * 255);
    printf("ingrese la direccion de la imagen a encriptar \n ");
    scanf("%s",rutaDeImg);
    printf("ingrese la direccion donde guardar la imagen encriptada \n ");
    scanf("%s",rutaDeImgCifrada);
    printf("ingrese la direccion donde guardar la imagen desencriptada \n ");
    scanf("%s",rutaDeImgDecifrada);


    printf("ingrese la primera palabra de la clave \n ");
    scanf("%x",&word1);
    printf("ingrese la segunda palabra de la clave \n");
    scanf("%x",&word2);
    printf("ingrese la tercera palabra de la clave \n");
    scanf("%x",&word3);
    printf("ingrese la cuatro palabra de la clave \n");
    scanf("%x",&word4);
    keysSelected[0]=word1;
    keysSelected[1]=word2;
    keysSelected[2]=word3;
    keysSelected[4]=word4;
    keyComplete(keysSelected,keys);

    FILE* img;
    img=fopen(rutaDeImg,"r");

    uint16_t * header=(uint16_t *)calloc(54,1);
    int dataSize=fileSize(img);
    uint16_t * plainText=(uint16_t*) calloc(dataSize,1);
    loadBMPContain(header,plainText,img);

    uint16_t * cipherText=(uint16_t*) calloc(dataSize,1);

    memcpy(cipherText,header,54);
    cipherBMPContain(((uint16_t *)((char *)cipherText+54)),plainText,keys,bmpDataSize(img));

    uint16_t * decipherText=(uint16_t*) calloc(dataSize,1);

    printf("ingrese la primera palabra de la clave para decifrar \n ");
    scanf("%x",&word1);
    printf("ingrese la segunda palabra de la clave para decifrar\n");
    scanf("%x",&word2);
    printf("ingrese la tercera palabra de la clave para decifrar\n");
    scanf("%x",&word3);
    printf("ingrese la cuatro palabra de la clave  para decifrar\n");
    scanf("%x",&word4);
    keysSelected[0]=word1;
    keysSelected[1]=word2;
    keysSelected[2]=word3;
    keysSelected[4]=word4;
    keyComplete(keysSelected,keys);

    memcpy(decipherText,header,54);
    decipherBMPContain(((uint16_t *)((char *)decipherText+54)),((uint16_t *)((char *)cipherText+54)),keys,bmpDataSize(img));

    FILE* cipherImg=fopen(rutaDeImgCifrada,"w");
    FILE* deCipherImg=fopen(rutaDeImgDecifrada,"w");

    fwrite(cipherText,1,fileSize(img),cipherImg);
    fwrite(decipherText,1,fileSize(img),deCipherImg);

    free(keys);
    free(keysSelected);
    free(cipherText);
    free(plainText);
    free(decipherText);
    free(header);
    fclose(img);
    fclose(cipherImg);
    fclose(deCipherImg);
    free(rutaDeImg);
    free(rutaDeImgCifrada);
    free(rutaDeImgDecifrada);
    return 0;
}