// Struct Translation of an {char, long, float} tuple
#ifndef DS_AND_HELP
#define DS_AND_HELP

#define WARP (1<<lgWARP)
#define TILE 32

typedef struct {
    ftk_uchar c;
    ftk_ulong i;
    ftk_float f;
} MyRec;

inline void loadNeutral(MyRec* dst) {
    dst->c = 0; dst->i = 0; dst->f = 0.0;
}

// translation of the operator
inline void applyMyRecOP(MyRec* dst, const MyRec* src1, const MyRec* src2) {
    if(src2->c == 0) {
        dst->i = (src1->i >= src2->i) ? src1->i : src2->i;
        dst->f = src1->f + src2->f;
    } else {
        dst->i = src2->i; 
        dst->f = src2->f;
    }
    dst->c = src1->c | src2->c;
}

#endif //DS_AND_HELP
