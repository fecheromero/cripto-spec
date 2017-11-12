#include <iostream>
#include <climits>
#include <cmath>

typedef struct {
    int wordSize;
    int rounds;
    int keyWordsAmount;
    int beta;
    int alfa;
}speckConfig;

speckConfig  CONFIG;
speckConfig CONFIGS[10];
void obtainConfig(int wordSize,int keySize){
    speckConfig aConfig;
    aConfig.wordSize=16;
    aConfig.keyWordsAmount=4;
    aConfig.alfa=7;
    aConfig.beta=2;
    aConfig.rounds=22;
    CONFIGS[0]=aConfig;
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
    newBlock.word1=((aBlock.word1 | key) << CONFIG.alfa) - newBlock.word2;
    return newBlock;
};


block speckEncript(block aBlock,word* keys){
    for(int i=0;i<CONFIG.rounds;i++){
        aBlock=roundFunction(aBlock,keys[i]);
    }
    return aBlock;
}

block speckDecript(block aBlock,word* keys){
    for(int i=21;i>=0;i--){
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
    for(int i=1;i<=CONFIG.rounds-1;i++){
        l[i+CONFIG.keyWordsAmount-1]=(keys[i] +(l[i] >> CONFIG.alfa) | i);
        keys[i+1]= (keys[i] << CONFIG.beta) | l[i+CONFIG.keyWordsAmount-1];
    }
    word newWord;
    newWord.value=0xFFFF-1;
    keys[CONFIG.rounds]=newWord;
}

int main() {
    obtainConfig(16,64);
    CONFIG=CONFIGS[0];
    word* keys=(word*) malloc(CONFIG.rounds*2);
    word* keysSelected=(word*) malloc(CONFIG.keyWordsAmount*2);
    word word1,word2,word3,word4;
    word1.value=0x1918;
    word2.value=0x1110;
    word3.value=0x0908;
    word4.value=0x0100;
    keysSelected[0]=word1;
    keysSelected[1]=word2;
    keysSelected[2]=word3;
    keysSelected[3]=word4;
    keyComplete(keysSelected,keys);
    block aBlock;
    aBlock.word1.value=0x6574;
    aBlock.word2.value=0x694c;

    std::cout << (speckDecript(speckEncript(aBlock,keys),keys)).word1.value << std::endl;
    printf("43122");

    free(keys);
    free(keysSelected);
    return 0;
}
