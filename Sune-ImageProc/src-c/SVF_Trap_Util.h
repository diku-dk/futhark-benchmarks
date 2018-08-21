#ifndef SVF_TRAP_UTIL_H_
#define SVF_TRAP_UTIL_H_
#include <vector>
#include <stdio.h>
#include <math.h>

using namespace std;

template <typename T> void inv3x3(T* M1, T* inv){
            //compute the inverse using cramers rule
            T det =
                    1
                    / ((M1[0] * M1[4] * M1[8]) + (M1[1] * M1[5] * M1[6])
                    + (M1[2] * M1[3] * M1[7])
                    - (M1[0] * M1[5] * M1[7])
                    - (M1[1] * M1[3] * M1[8])
                    - (M1[2] * M1[4] * M1[6]));
            //the transpose inverse
// 			inv[0] = (M1[4] * M1[8] - M1[7] * M1[5]) * det;
// 			inv[1] = -(M1[3] * M1[8] - M1[5] * M1[6]) * det;
// 			inv[2] = (M1[3] * M1[7] - M1[4] * M1[6]) * det;
// 			inv[3] = -(M1[1] * M1[8] - M1[2] * M1[7]) * det;
// 			inv[4] = (M1[0] * M1[8] - M1[2] * M1[6]) * det;
// 			inv[5] = -(M1[0] * M1[7] - M1[1] * M1[6]) * det;
// 			inv[6] = (M1[1] * M1[5] - M1[2] * M1[4]) * det;
// 			inv[7] = -(M1[0] * M1[5] - M1[2] * M1[3]) * det;
// 			inv[8] = (M1[0] * M1[4] - M1[1] * M1[3]) * det;
            
            //the inverse
            inv[0]=(M1[4]*M1[8]-M1[7]*M1[5])*det;
            inv[3]=-(M1[3]*M1[8]-M1[5]*M1[6])*det;
            inv[6]=(M1[3]*M1[7]-M1[4]*M1[6])*det;
            inv[1]=-(M1[1]*M1[8]-M1[2]*M1[7])*det;
            inv[4]=(M1[0]*M1[8]-M1[2]*M1[6])*det;
            inv[7]=-(M1[0]*M1[7]-M1[1]*M1[6])*det;
            inv[2]=(M1[1]*M1[5]-M1[2]*M1[4])*det;
            inv[5]=-(M1[0]*M1[5]-M1[2]*M1[3])*det;
            inv[8]=(M1[0]*M1[4]-M1[1]*M1[3])*det;
}
template <typename T> void Mul3x3_3xN(T* M,T* X, int cnt)
{
    T tmp_mul1,tmp_mul2,tmp_mul3;
                for (int jj = 0; jj < cnt*3; jj++) {
                tmp_mul1 = M[0] * X[jj * 3] + M[1] * X[jj * 3 + 1]
                        + M[2] * X[jj * 3 + 2];
                tmp_mul2 = M[3] * X[jj * 3] + M[4] * X[jj * 3 + 1]
                        + M[5] * X[jj * 3 + 2];
                tmp_mul3 = M[6] * X[jj * 3] + M[7] * X[jj * 3 + 1]
                        + M[8] * X[jj * 3 + 2];
                X[jj * 3] = tmp_mul1;
                X[jj * 3 + 1] = tmp_mul2;
                X[jj * 3 + 2] = tmp_mul3;
                
            }
}
template <typename T> void Mul3x3(T* M,T* X)
{
    
    
    T tmp_mul1,tmp_mul2,tmp_mul3;
                for (int i = 0; i < 3; i++) {
                tmp_mul1 = M[0] * X[i] + M[1] * X[i + 3 ]
                        + M[2] * X[i + 3 * 2];
                tmp_mul2 = M[3] * X[i] + M[4] * X[i + 3 ]
                        + M[5] * X[i +6];
                tmp_mul3 = M[6] * X[i] + M[7] * X[i + 3 ]
                        + M[8] * X[i + 6];
                X[i ] = tmp_mul1;
                X[i + 3 ] = tmp_mul2;
                X[i + 6 ] = tmp_mul3;
                
            }
}
template <typename T> void Id_minus_half_3x3(T* in,T* out)
{
    for(int n=0;n<9;n++) out[n]=-0.5*in[n];
      out[0]+=1;
      out[4]+=1;
      out[8]+=1;
    
}
template <typename T> void Id_plus_half_3x3(T* in,T* out)
{   
    for(int n=0;n<9;n++) out[n]=0.5*in[n];
      out[0]+=1;
      out[4]+=1;
      out[8]+=1;
    
}
template <typename T> void add_3x3(T* in,T* out)
{   
    for(int n=0;n<9;n++) out[n]+=in[n];
}
template <typename T> void sub_3x3(T* in,T* out)
{   
    for(int n=0;n<9;n++) out[n]-=in[n];
}
template <typename T> void scal_mul_3x3(T* in,T scal)
{   
    for(int n=0;n<9;n++) in[n]=scal*in[n];
}
#endif