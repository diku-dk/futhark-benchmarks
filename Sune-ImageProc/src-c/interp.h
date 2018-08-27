#ifndef SVF_INTERP_H_
#define SVF_INTERP_H_

#include <math.h>
#include <matrix.h>
#include <mex.h>
#include <vector>
#include "SVF_Trap_Util.h"

using namespace std;

template<class T> class interp
{
private:
    //	mwSize* dims;
    T* dataI ;
    double* offset ;
    double* scale;
    int ndim ;
    int* dim_img;
    int* dims;
  
    
    
public:
    interp(T* vdataI,double* voffset,double* vscale,int vndim,int* vdim_img,int* vdims	) {
       
        dataI=vdataI;
        offset=voffset;
        scale=vscale;
        ndim=vndim;
        dim_img=vdim_img;
        dims=vdims;
        //		dfdx=vdfdx;
        
        
    }
      void run(double* pts,
            	double* val,int N)
    {
        
        double sumdfdp=0;
       
      
        //double* p=new double[s];
        int idx_idx=0;
        for(int i=0;i<N;i++){
            val[i]=0;
            double t=(pts[i]-offset[0])/scale[0]-floor((pts[i]-offset[0])/scale[0]);
            double t2=t*t;
            double t3=t2*t;
            
            /*double* x={-pow(t,3)+3*pow(t,2)-3*t+1, 3*pow(t,3)-6*pow(t,2)+4, -3*pow(t,3)+3*pow(t,2)+3*t+1,  pow(t,3)};
             * double* dx={-pow(t,3)+3*pow(t,2)-3*t+1, 3*pow(t,3)-6*pow(t,2)+4, -3*pow(t,3)+3*pow(t,2)+3*t+1,  pow(t,3)};
             */
            double x[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
            t=floor((pts[i]-offset[0])/scale[0]);
            int px[4]={(int)min<double>(max<double>(t-1,0),dim_img[0]-1),(int) max<double>(min<double>(t,dim_img[0]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[0]-1),(int) max<double>(min<double>(t+2,dim_img[0]-1),0)};
            
            t=(pts[i+N]-offset[1])/scale[1]-floor((pts[i+N]-offset[1])/scale[1]);
            t2=t*t;
            t3=t2*t;
            
            double y[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
             t=floor((pts[i+N]-offset[1])/scale[1]);
            int py[4]={(int)min<double>(max<double>(t-1,0),dim_img[1]-1),(int) max<double>(min<double>(t,dim_img[1]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[1]-1),(int) max<double>(min<double>(t+2,dim_img[1]-1),0)};
            
            t=(pts[i+2*N]-offset[2])/scale[2]-floor((pts[i+2*N]-offset[2])/scale[2]);
            t2=t*t;
            t3=t2*t;
            
            double z[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
             t=floor((pts[i+2*N]-offset[2])/scale[2]);
            int pz[4]={(int)min<double>(max<double>(t-1,0),dim_img[2]-1),(int) max<double>(min<double>(t,dim_img[2]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[2]-1),(int) max<double>(min<double>(t+2,dim_img[2]-1),0)};
            int index=0;
            int idx=0;
            double dfdp;
            for(int j=0;j<4;j++){
                for(int k=0;k<4;k++){
                    for(int l=0;l<4;l++){
                        for(int m=0;m<dims[1];m++){
                            //			mexPrintf("%f %d %d %d \n",data[px[l]+dim_img[0]*py[k]+dim_img[0]*dim_img[1]*pz[j]+dim_img[0]*dim_img[1]*dim_img[2]*m],px[l],py[k],pz[j]);
                            
                            if(m<1){
                                idx=px[l]+dim_img[0]*py[k]+dim_img[0]*dim_img[1]*pz[j];
                                dfdp=x[l]*y[k]*z[j]/216;
                            }
                            T dd=dataI[idx+dim_img[0]*dim_img[1]*dim_img[2]*m];
                            
                            
                            val[i+m*N]+=dfdp*dd;
                            
                            
                            
                        }
                    }
                }
            }
            
        }
    }

    void run(double* pts,double* diff1 ,double* diff2,	double* diff3,
            	double* val,double* dfdx,double* dfdp, int* idx,int N)
    {
        
        double sumdfdp=0;
        
        int s=64;
        //double* p=new double[s];
        int idx_idx=0;
        for(int i=0;i<N;i++){
            val[i]=0;
            diff1[i]=0;
            diff2[i]=0;
            diff3[i]=0;
            double t=(pts[i]-offset[0])/scale[0]-floor((pts[i]-offset[0])/scale[0]);
            double t2=t*t;
            double t3=t2*t;
            
            /*double* x={-pow(t,3)+3*pow(t,2)-3*t+1, 3*pow(t,3)-6*pow(t,2)+4, -3*pow(t,3)+3*pow(t,2)+3*t+1,  pow(t,3)};
             * double* dx={-pow(t,3)+3*pow(t,2)-3*t+1, 3*pow(t,3)-6*pow(t,2)+4, -3*pow(t,3)+3*pow(t,2)+3*t+1,  pow(t,3)};
             */
            double x[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
            double dx[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
            t=floor((pts[i]-offset[0])/scale[0]);
            int px[4]={(int)min<double>(max<double>(t-1,0),dim_img[0]-1),(int) max<double>(min<double>(t,dim_img[0]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[0]-1),(int) max<double>(min<double>(t+2,dim_img[0]-1),0)};
            
            t=(pts[i+N]-offset[1])/scale[1]-floor((pts[i+N]-offset[1])/scale[1]);
            t2=t*t;
            t3=t2*t;
            
            double y[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
            double dy[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
            t=floor((pts[i+N]-offset[1])/scale[1]);
            int py[4]={(int)min<double>(max<double>(t-1,0),dim_img[1]-1),(int) max<double>(min<double>(t,dim_img[1]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[1]-1),(int) max<double>(min<double>(t+2,dim_img[1]-1),0)};
            
            t=(pts[i+2*N]-offset[2])/scale[2]-floor((pts[i+2*N]-offset[2])/scale[2]);
            t2=t*t;
            t3=t2*t;
            
            double z[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
            double dz[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
            t=floor((pts[i+2*N]-offset[2])/scale[2]);
            int pz[4]={(int)min<double>(max<double>(t-1,0),dim_img[2]-1),(int) max<double>(min<double>(t,dim_img[2]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[2]-1),(int) max<double>(min<double>(t+2,dim_img[2]-1),0)};
            int index=0;
            for(int j=0;j<4;j++){
                for(int k=0;k<4;k++){
                    for(int l=0;l<4;l++){
                        for(int m=0;m<dims[1];m++){
                            //			mexPrintf("%f %d %d %d \n",data[px[l]+dim_img[0]*py[k]+dim_img[0]*dim_img[1]*pz[j]+dim_img[0]*dim_img[1]*dim_img[2]*m],px[l],py[k],pz[j]);
                            
                            if(m<1){
                                idx[idx_idx]=px[l]+dim_img[0]*py[k]+dim_img[0]*dim_img[1]*pz[j];
                                dfdp[idx_idx]=x[l]*y[k]*z[j]/216;
                                idx_idx++;
                            }
                            T dd=dataI[idx[idx_idx-1]+dim_img[0]*dim_img[1]*dim_img[2]*m];
                            
                            
                            val[i+m*N]+=dfdp[idx_idx-1]*dd;
                            
                            if(m<1){
                                index=i*(192)+l+4*k+16*j;
                                //mexPrintf(" %d \n",index);
                                dfdx[index]=dx[l]*y[k]*z[j]/216;
                                dfdx[index+64]=x[l]*dy[k]*z[j]/216;
                                dfdx[index+128]=x[l]*y[k]*dz[j]/216;
                                
                            }
                            
                            diff1[i+m*N]+=dfdx[index]*dd;
                            diff2[i+m*N]+=dfdx[index+64]*dd;
                            diff3[i+m*N]+=dfdx[index+128]*dd;
                        }
                    }
                }
            }
            
        }
    }
     void run(double* pts,double* diff1 ,double* diff2,	double* diff3,
            	double* val,int N)
    {
        
        double sumdfdp=0;
        
        int s=64;
        //double* p=new double[s];
        int idx_idx=0;
        for(int i=0;i<N;i++){
            val[i]=0;
            diff1[i]=0;
            diff2[i]=0;
            diff3[i]=0;
            double t=(pts[i]-offset[0])/scale[0]-floor((pts[i]-offset[0])/scale[0]);
            double t2=t*t;
            double t3=t2*t;
            
            /*double* x={-pow(t,3)+3*pow(t,2)-3*t+1, 3*pow(t,3)-6*pow(t,2)+4, -3*pow(t,3)+3*pow(t,2)+3*t+1,  pow(t,3)};
             * double* dx={-pow(t,3)+3*pow(t,2)-3*t+1, 3*pow(t,3)-6*pow(t,2)+4, -3*pow(t,3)+3*pow(t,2)+3*t+1,  pow(t,3)};
             */
            double x[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
            double dx[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
            t=floor((pts[i]-offset[0])/scale[0]);
            int px[4]={(int)min<double>(max<double>(t-1,0),dim_img[0]-1),(int) max<double>(min<double>(t,dim_img[0]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[0]-1),(int) max<double>(min<double>(t+2,dim_img[0]-1),0)};
            
            t=(pts[i+N]-offset[1])/scale[1]-floor((pts[i+N]-offset[1])/scale[1]);
            t2=t*t;
            t3=t2*t;
            
            double y[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
            double dy[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
            t=floor((pts[i+N]-offset[1])/scale[1]);
            int py[4]={(int)min<double>(max<double>(t-1,0),dim_img[1]-1),(int) max<double>(min<double>(t,dim_img[1]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[1]-1),(int) max<double>(min<double>(t+2,dim_img[1]-1),0)};
            
            t=(pts[i+2*N]-offset[2])/scale[2]-floor((pts[i+2*N]-offset[2])/scale[2]);
            t2=t*t;
            t3=t2*t;
            
            double z[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
            double dz[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
            t=floor((pts[i+2*N]-offset[2])/scale[2]);
            int pz[4]={(int)min<double>(max<double>(t-1,0),dim_img[2]-1),(int) max<double>(min<double>(t,dim_img[2]-1),0),(int)min<double>(max<double>(t+1,0),dim_img[2]-1),(int) max<double>(min<double>(t+2,dim_img[2]-1),0)};
            int index=0;
            int idx=0;
            double dfdp;
            for(int j=0;j<4;j++){
                for(int k=0;k<4;k++){
                    for(int l=0;l<4;l++){
                        for(int m=0;m<dims[1];m++){
                            //			mexPrintf("%f %d %d %d \n",data[px[l]+dim_img[0]*py[k]+dim_img[0]*dim_img[1]*pz[j]+dim_img[0]*dim_img[1]*dim_img[2]*m],px[l],py[k],pz[j]);
                            
                            if(m<1){
                                idx=px[l]+dim_img[0]*py[k]+dim_img[0]*dim_img[1]*pz[j];
                                dfdp=x[l]*y[k]*z[j]/216;
                              
                            }
                            T dd=dataI[idx+dim_img[0]*dim_img[1]*dim_img[2]*m];
                            
                            
                            val[i+m*N]+=dfdp*dd;
                            
                         
                            
                            diff1[i+m*N]+=(dx[l]*y[k]*z[j]/216)*dd;
                            diff2[i+m*N]+=(x[l]*dy[k]*z[j]/216)*dd;
                            diff3[i+m*N]+=(x[l]*y[k]*dz[j]/216)*dd;
                        }
                    }
                }
            }
            
        }
    }
};


#endif
