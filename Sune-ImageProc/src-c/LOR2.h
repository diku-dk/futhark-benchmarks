
#ifndef LOR_H_
#define LOR_H_

#include <vector>
#include <cstring>
#include "./../Interpolation/interp2d.h"
using namespace std;
class LOR
{
protected:
    int pwtype;
    double* dataR ;
    double* range;
    int ndim ;
    const int* dim_img;
    int* no_bins;
    double* det;
    double* val;
    double detSum;
    vector<double> pI;
    vector<double> pR;
    vector<double> pRI;
    vector<double> df_dpRI;
    
    
public:
    LOR(double* vdataR,int vndim,const int* vdim_img,double* vrange, int* vno_bins,double* vdet,double* vval) {
        pwtype=0;
        range=vrange;
        no_bins=vno_bins;
        dataR=vdataR;
        ndim=vndim;
        dim_img=vdim_img;
        det=vdet;
        val=vval;
        detSum=0;
        pI.assign((int)no_bins[0],0);
        pR.assign((int)no_bins[1],0);
        pRI.assign((int)(no_bins[0]*no_bins[1]),0);
        df_dpRI.assign((int)(no_bins[0]*no_bins[1]),0);
        
        //mexPrintf("%d \n",N);
    }
protected:
    
    inline void reset_density(){
        pI.assign((int)no_bins[0],0);
        pR.assign((int)no_bins[1],0);
        pRI.assign((int)(no_bins[0]*no_bins[1]),0);
        df_dpRI.assign((int)(no_bins[0]*no_bins[1]),0);
    }
    inline void estimate_density(double* pI,double* pR,double* pRI,int N){
        detSum=0;
        reset_density();
        if(pwtype==0){
            for(int i=0;i<N;i++){
                detSum+=det[i];
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                double t2=t*t;
                double t3=t2*t;
                
                double tr_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                for (int nn=0;nn<4;nn++){
                    pR[idxR+nn-1]+=det[i]*tr_val[nn]/6;
                }
                t=val[i]-(int)(val[i]);
                //mexPrintf("%f %f\n",vals[i],dataR[i]);
                t2=t*t;
                t3=t2*t;
                
                double t_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                int dd=(int)((val[i]));
                for(int m=0;m<4;m++)
                {
                    pI[dd+m-1]+=det[i]*t_val[m]/6;
                    for (int nn=0;nn<4;nn++){
                        pRI[idxR+nn-1+(int)no_bins[1]*(dd+m-1)]+=det[i]*t_val[m]*tr_val[nn]/36;
                    }
                }
            }
        }
        else if(pwtype==1){
            for(int i=0;i<N;i++){
                detSum+=det[i];
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                
                
                double tr_val[2]={1-t,t};
                for (int nn=0;nn<2;nn++){
                    pR[idxR+nn]+=det[i]*tr_val[nn];
                }
                t=val[i]-(int)(val[i]);
                //mexPrintf("%f %f\n",vals[i],dataR[i]);
                
                
                double t_val[2]={1-t,t};
                int dd=(int)((val[i]));
                for(int m=0;m<2;m++)
                {
                    pI[dd+m-1]+=det[i]*t_val[m];
                    for (int nn=0;nn<2;nn++){
                        pRI[idxR+nn+(int)no_bins[1]*(dd+m)]+=det[i]*t_val[m]*tr_val[nn];
                    }
                }
            }
        }
    }
    inline void estimate_density(double* pI,double* pR,double* pRI,int N, int* idx, int offset ){
        detSum=0;
        int i=0;
        reset_density();
        if(pwtype==0){
            for(int j=0;j<N;j++){
                i=idx[j]+offset;
                detSum+=det[i];
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                double t2=t*t;
                double t3=t2*t;
                
                double tr_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                for (int nn=0;nn<4;nn++){
                    pR[idxR+nn-1]+=det[i]*tr_val[nn]/6;
                }
                t=val[i]-(int)(val[i]);
                //mexPrintf("%f %f\n",vals[i],dataR[i]);
                t2=t*t;
                t3=t2*t;
                
                double t_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                int dd=(int)((val[i]));
                for(int m=0;m<4;m++)
                {
                    pI[dd+m-1]+=det[i]*t_val[m]/6;
                    for (int nn=0;nn<4;nn++){
                        pRI[idxR+nn-1+(int)no_bins[1]*(dd+m-1)]+=det[i]*t_val[m]*tr_val[nn]/36;
                    }
                }
            }
        }
        else if(pwtype==1){
            for(int j=0;j<N;j++){
                i=idx[j]+offset;
                detSum+=det[i];
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                
                
                double tr_val[2]={1-t,t};
                for (int nn=0;nn<2;nn++){
                    pR[idxR+nn]+=det[i]*tr_val[nn];
                }
                t=val[i]-(int)(val[i]);
                //mexPrintf("%f %f\n",vals[i],dataR[i]);
                
                
                double t_val[2]={1-t,t};
                int dd=(int)((val[i]));
                for(int m=0;m<2;m++)
                {
                    pI[dd+m-1]+=det[i]*t_val[m];
                    for (int nn=0;nn<2;nn++){
                        pRI[idxR+nn+(int)no_bins[1]*(dd+m)]+=det[i]*t_val[m]*tr_val[nn];
                    }
                }
            }
        }
        
    }
    inline void compute_derivative(double* diff, double* df_dpRI, int N){
        if(pwtype==0){
            for(int i=0;i!=N;i++)
            {
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                double t2=t*t;
                double t3=t2*t;
                double tr_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                t=val[i]-floor(val[i]);
                t2=t*t;
                t3=t2*t;
                int dd=(int)(floor(val[i])-range[0]);
                double t_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                double dt_val[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
                double dNMIdW=0;
                for(int m=0;m<4;m++)
                {
                    for (int nn=0;nn<4;nn++){
                        dNMIdW+=det[i]*dt_val[m]*tr_val[nn]/(36*detSum)*df_dpRI[idxR+nn-1+no_bins[1]*(dd+m-1)];
                    }
                }
                diff[i]=dNMIdW;
            }
        }
        else if(pwtype==1){
            for(int i=0;i!=N;i++)
            {
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                
                double tr_val[2]={1-t,t};
                t=val[i]-floor(val[i]);
                int dd=(int)(floor(val[i])-range[0]);
                double t_val[2]={1-t,t};
                double dt_val[2]={-1,1};
                double dNMIdW=0;
                for(int m=0;m<2;m++)
                {
                    for (int nn=0;nn<2;nn++){
                        dNMIdW+=det[i]*dt_val[m]*tr_val[nn]/(detSum)*df_dpRI[idxR+nn+no_bins[1]*(dd+m)];
                    }
                }
                diff[i]=dNMIdW;
            }
        }
    }
    inline void compute_derivative(double* diff, double* df_dpRI, int N, int* idx, int offset){
        int i=0;
        if(pwtype==0){
            for(int j=0;j!=N;j++)
            {
                i=idx[j]+offset;
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                double t2=t*t;
                double t3=t2*t;
                double tr_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                t=val[i]-floor(val[i]);
                t2=t*t;
                t3=t2*t;
                int dd=(int)(floor(val[i])-range[0]);
                double t_val[4]={-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3};
                double dt_val[4]={-3*t2+6*t-3, 9*t2-12*t, -9*t2+6*t+3,  3*t2};
                double dNMIdW=0;
                for(int m=0;m<4;m++)
                {
                    for (int nn=0;nn<4;nn++){
                        dNMIdW+=det[i]*dt_val[m]*tr_val[nn]/(36*detSum)*df_dpRI[idxR+nn-1+no_bins[1]*(dd+m-1)];
                    }
                }
                diff[i]+=dNMIdW;
            }
        }
        else if(pwtype==1){
            for(int j=0;j!=N;j++)
            {
                i=idx[j]+offset;
                int idxR=(int)floor((dataR[i]-range[2]));
                double t=(dataR[i])-floor(dataR[i]);
                
                double tr_val[2]={1-t,t};
                t=val[i]-floor(val[i]);
                int dd=(int)(floor(val[i])-range[0]);
                double t_val[2]={1-t,t};
                double dt_val[2]={-1,1};
                double dNMIdW=0;
                for(int m=0;m<2;m++)
                {
                    for (int nn=0;nn<2;nn++){
                        dNMIdW+=det[i]*dt_val[m]*tr_val[nn]/(detSum)*df_dpRI[idxR+nn+no_bins[1]*(dd+m)];
                    }
                }
                diff[i]=dNMIdW;
            }
        }
    }
    virtual void run(int N, double* value)=0;
public:
    void run(int N,double* diff,double* value,int* idx,int offset )
    {
        
        estimate_density(pI.data(),pR.data(),pRI.data(),N,idx, offset);
        run(N, value );
        compute_derivative(diff,df_dpRI.data(),N,idx,offset);
    }
    void run(int N,double* diff,double* value )
    {
        
        estimate_density(pI.data(),pR.data(),pRI.data(),N);
        run(N, value );
        compute_derivative(diff,df_dpRI.data(),N);
    }
    void run(int N,double* diff1 ,double* diff2,double* value )
    {
        
        vector<double> diff(N,0);
        estimate_density(pI.data(),pR.data(),pRI.data(),N);
        run(N, value );
        compute_derivative(diff.data(),df_dpRI.data(),N);
        for(int i=0;i<N;i++){
            diff1[i]=diff1[i]*diff[i];
            diff2[i]=diff2[i]*diff[i];
        }
    }
    void run(int N,double* diff1 ,double* diff2,double* diff3,double* value )
    {
        vector<double> diff(N,0);
        estimate_density(pI.data(),pR.data(),pRI.data(),N);
        run(N, value );
        compute_derivative(diff.data(),df_dpRI.data(),N);
        for(int i=0;i<N;i++){
            diff1[i]=diff1[i]*diff[i];
            diff2[i]=diff2[i]*diff[i];
            diff3[i]=diff3[i]*diff[i];
        }
    }
    void locally(int ksize,int* grid_dim, int stride, double* diff, double* val)
    {
        vector<int> idx(ksize*ksize*ksize,0);
        int end=ksize-1;
        int start=0;
        vector<double> t_val(1,0);
        int offset_stride=1;
        int nn=0;
        for(int ii=0;ii<ksize;ii++)
            for(int jj=0;jj<ksize;jj++)
                for(int kk=0;kk<ksize;kk++)
                    idx[nn++]=(ii+(jj)*grid_dim[0]+(kk)*grid_dim[0]*grid_dim[1]);
        
        
        for(int i=start;i<grid_dim[0]-end;i+=stride){
            for(int j=start;j<grid_dim[1]-end;j+=stride){
                for(int k=start;k<grid_dim[2]-end;k+=stride){
                    offset_stride=i+(j)*grid_dim[0]+(k)*grid_dim[0]*grid_dim[1];
                    run(idx.size(),diff,t_val.data(),idx.data(),offset_stride);
                    val[0]+=t_val[0];
                }
            }
        }
    }
     void locally(int ksize,int* grid_dim, int stride, double* diff, double* val, unsigned char* mask)
    {
        vector<int> idx(ksize*ksize*ksize,0);
        int end=ksize-1;
        int start=0;
        int center=(int) floor(ksize/2.0)*(grid_dim[0]+grid_dim[0]*grid_dim[1]);
        vector<double> t_val(1,0);
        int offset_stride=1;
        int nn=0;
        for(int ii=0;ii<ksize;ii++)
            for(int jj=0;jj<ksize;jj++)
                for(int kk=0;kk<ksize;kk++)
                    idx[nn++]=(ii+(jj)*grid_dim[0]+(kk)*grid_dim[0]*grid_dim[1]);
        
        
        for(int i=start;i<grid_dim[0]-end;i+=stride){
            for(int j=start;j<grid_dim[1]-end;j+=stride){
                for(int k=start;k<grid_dim[2]-end;k+=stride){
                    offset_stride=i+(j)*grid_dim[0]+(k)*grid_dim[0]*grid_dim[1];
                    if(mask[offset_stride+center]>0){
                    run(idx.size(),diff,t_val.data(),idx.data(),offset_stride);
                    val[0]+=t_val[0];
                    }
                }
            }
        }
    }
    
};

class NMI: public LOR
{
    
    
    
    
public:
    using LOR::LOR;
    NMI(double* vdataR,int vndim,const int* vdim_img,double* vrange, int* vno_bins,double* vdet,double* vval): LOR(vdataR,vndim,vdim_img,vrange,vno_bins,vdet,vval) {}
    void run(int N,double* value )
    {
       
        double HRI=0,HI=0,HR=0;
        vector<double> LogpI((int)no_bins[1],0);
        vector<double> LogpRI((int)(no_bins[0]*no_bins[1]),0);
        double sumPI=0;
        double sumPIR=0;
        double sumPR=0;
        
        
        //take the logarithm
        //mexPrintf("taking the log\n");
        for(int i=0;i<no_bins[0];i++){
            if(pI[i]>0){
                pI[i]=pI[i]/detSum;
                LogpI[i]=log(pI[i]);
                HI+=pI[i]*LogpI[i];
                sumPI+=pI[i];
            }
            for(int j=0;j<no_bins[1];j++){
                if(pRI[j + i*(int)no_bins[1]]>0){
                    pRI[j + i*(int)no_bins[1]]=pRI[j + i*(int)no_bins[1]]/detSum;
                    LogpRI[j + i*(int)no_bins[1]]=log(pRI[j + i*(int)no_bins[1]]);
                    HRI+=pRI[j + i*(int)no_bins[1]]*LogpRI[j + i*(int)no_bins[1]];
                    sumPIR+=pRI[j + i*(int)no_bins[1]];
                }
            }
        }
        for(int j=0;j<no_bins[1];j++){
            if(pR[j]>0){
                pR[j]=pR[j]/detSum;
                HR+=pR[j]*log(pR[j]);
                sumPR+=pR[j];
            }
        }
        for(int i=0;i<no_bins[0];i++){
            for(int j=0;j<no_bins[1];j++){
                df_dpRI[j+no_bins[1]*i]=((-(LogpI[i]+1)-(HR+HI)*(-(LogpRI[j+no_bins[1]*i]+1))/HRI)/(HRI));
            }
        }        
        value[0]=((-HR)+(-HI))/(-HRI);
    }
    
    
};
class MI: public LOR
{  
public:
    using LOR::LOR;
    MI(double* vdataR,int vndim,const int* vdim_img,double* vrange, int* vno_bins,double* vdet,double* vval): LOR(vdataR,vndim,vdim_img,vrange,vno_bins,vdet,vval) {}
    void run(int N,double* value )
    {
        double HRI=0,HI=0,HR=0;
        vector<double> LogpI((int)no_bins[1],0);
        vector<double> LogpRI((int)(no_bins[0]*no_bins[1]),0);
        double sumPI=0;
        double sumPIR=0;
        double sumPR=0;
        
        for(int i=0;i<no_bins[0];i++){
            if(pI[i]>0){
                pI[i]=pI[i]/detSum;
                LogpI[i]=log(pI[i]);
                HI+=pI[i]*LogpI[i];
                sumPI+=pI[i];
            }
            for(int j=0;j<no_bins[1];j++){
                if(pRI[j + i*(int)no_bins[1]]>0){
                    pRI[j + i*(int)no_bins[1]]=pRI[j + i*(int)no_bins[1]]/detSum;
                    LogpRI[j + i*(int)no_bins[1]]=log(pRI[j + i*(int)no_bins[1]]);
                    HRI+=pRI[j + i*(int)no_bins[1]]*LogpRI[j + i*(int)no_bins[1]];
                    sumPIR+=pRI[j + i*(int)no_bins[1]];
                }
            }
        }
        for(int j=0;j<no_bins[1];j++){
            if(pR[j]>0){
                pR[j]=pR[j]/detSum;
                HR+=pR[j]*log(pR[j]);
                sumPR+=pR[j];
            }
        }
        for(int i=0;i<no_bins[0];i++){
            for(int j=0;j<no_bins[1];j++){
                df_dpRI[j+no_bins[1]*i]=(-(LogpI[i]+1)-(-(LogpRI[j+no_bins[1]*i]+1)));
            }
        }
        value[0]=((-HR)+(-HI))-(-HRI);
    }
};
class NCC: public LOR
{  
public:
    using LOR::LOR;
    NCC(double* vdataR,int vndim,const int* vdim_img,double* vrange, int* vno_bins,double* vdet,double* vval): LOR(vdataR,vndim,vdim_img,vrange,vno_bins,vdet,vval) {}
    
    void run(int N,double* value )
    {
        
        vector<double> dmu_dpI(no_bins[0],0);
        vector<double> dsig_I_dpRI((no_bins[0]),0);
        double mu_I=0,mu_R=0,sig_I=0,sig_R=0;
        
        for(int i=0;i<no_bins[0];i++){
            pI[i]=pI[i]/detSum;
            mu_I+=pI[i]*i;
            dmu_dpI[i]=i;
            for(int j=0;j<no_bins[1];j++){
                pRI[j + i*no_bins[1]]=pRI[j + i*no_bins[1]]/detSum;
            }
        }
        for(int j=0;j<no_bins[1];j++){
            pR[j]=pR[j]/detSum;
            mu_R+=pR[j]*j;
            
        }
        for(int j=0;j<no_bins[1];j++)
            sig_R+=pR[j]*(j-mu_R)*(j-mu_R);
        
        for(int i=0;i<no_bins[0];i++)
            sig_I+=pI[i]*(i-mu_I)*(i-mu_I);
        value[0]=0;
        sig_R=sqrt(sig_R);
        sig_I=sqrt(sig_I);
        
        for(int i=0;i<no_bins[0];i++){
            dsig_I_dpRI[i]=(i-mu_I)*(i-mu_I);
            for(int ii=0;ii<no_bins[0];ii++){
                dsig_I_dpRI[i]-=2*pI[ii]*i*(ii-mu_I);
            }
            dsig_I_dpRI[i]=dsig_I_dpRI[i]*0.5/sig_I;
        }
        
        double sigIR=sig_I*sig_R;
        double sigIRIR=sig_I*sig_R*sig_I*sig_R;
        for(int i=0;i<no_bins[0];i++)
            for(int j=0;j<no_bins[1];j++)
                value[0]+=pRI[j + i*no_bins[1]]*(i-mu_I)*(j-mu_R)/(sig_I*sig_R);
        for(int i=0;i<no_bins[0];i++){
            for(int j=0;j<no_bins[1];j++){
                if(pRI[j + i*no_bins[1]]>0){
                    df_dpRI[j + i*no_bins[1]]=(i-mu_I)*(j-mu_R)/(sig_I*sig_R)-value[0]*dsig_I_dpRI[i]*sig_R/sigIR;
                    for(int jj=0;jj<no_bins[1];jj++){
                        df_dpRI[j + i*no_bins[1]]+=(-dmu_dpI[i])*pR[jj]*sigIR*(jj-mu_R)/(sigIRIR);//-pRI[jj + ii*no_bins[1]]*(ii-mu_I)*dsig_I_dpRI[i]*sig_R*(jj-mu_R)/(sigIRIR);
                    }
                }
            }
        }
    }    
};

//Linear LOR measures....inherently global
class PNORM: public LOR
{  
protected:
    double p;
public:
    using LOR::LOR;
    PNORM(double* vdataR,int vndim,const int* vdim_img,double* vrange, int* vno_bins,double* vdet,double* vval,double p): LOR(vdataR,vndim,vdim_img,vrange,vno_bins,vdet,vval) {p=p;}
    void run(int N,double* value )
    {
     
        for(int i=0;i<no_bins[0];i++){
            for(int j=0;j<no_bins[1];j++){
                df_dpRI[j+no_bins[1]*i]=pow(abs(i-j),p);
                value[0]+=pRI[j+no_bins[1]*i]*pow(abs(i-j),p);
            }
        }
    }
};
class HINGE: public LOR
{  
protected:
    double p;
    double alpha;
public:
    using LOR::LOR;
    HINGE(double* vdataR,int vndim,const int* vdim_img,double* vrange, int* vno_bins,double* vdet,double* vval,double p,double alpha): LOR(vdataR,vndim,vdim_img,vrange,vno_bins,vdet,vval) {p=p;}
    void run(int N,double* value )
    {
     
        for(int i=0;i<no_bins[0];i++){
            for(int j=0;j<no_bins[1];j++){
                if(abs(i-j)>alpha) {
                    df_dpRI[j+no_bins[1]*i]=pow(abs(i-j)-alpha,p);
                    value[0]+=pRI[j+no_bins[1]*i]*pow(abs(i-j)-alpha,p);
                }
            }
        }
    }
};
class HUBER: public LOR
{
protected:
    double p;
    double alpha;
public:
    using LOR::LOR;
    HUBER(double* vdataR,int vndim,const int* vdim_img,double* vrange, int* vno_bins,double* vdet,double* vval,double p,double alpha): LOR(vdataR,vndim,vdim_img,vrange,vno_bins,vdet,vval) {p=p;}
    void run(int N,double* value )
    {
        
        for(int i=0;i<no_bins[0];i++){
            for(int j=0;j<no_bins[1];j++){
                if(abs(i-j)>alpha) {
                    df_dpRI[j+no_bins[1]*i]=pow(abs(i-j),p-1);
                    value[0]+=pRI[j+no_bins[1]*i]*pow(abs(i-j),p-1);
                    
                }
                else{
                    df_dpRI[j+no_bins[1]*i]=pow(abs(i-j),p-1);
                    value[0]+=pRI[j+no_bins[1]*i]*p*pow(alpha*abs(i-j),p-1)-(p-1)*pow(alpha,p);
                    
                }
            }
        }
    }

};

#endif