// splineinterpolation2d.cpp : Defines the entry point for the console application.
//

#include <math.h>
#include <matrix.h>
#include <mex.h>
#include "LOR2.h"
#include <vector>
#include <cstring>
#include "./../Interpolation/interp.h"



void mexFunction(int nlhs, mxArray *plhs[], int nrhs,
        const mxArray *prhs[])
{
    
    //bool doDerivative = false;
    //if(nlhs>1)
    //    doDerivative = true;
    /*	if(nrhs==0)
     * mexPrintf("NMI3D takes 6 arguments,pts,ref_data, Image, range, nobins, offset, scale (optional) ");
     * if(nrhs<6)
     * mexErrMsgTxt("Number of arguments must be 6");
     */
    double* pts = static_cast<double*>(mxGetData(prhs[0]));
    double* dataR = static_cast<double*>(mxGetData(prhs[1]));
    void* dataI = mxGetData(prhs[2]);
    double* range = static_cast<double*>(mxGetData(prhs[3]));
    double* n_bins = static_cast<double*>(mxGetData(prhs[4]));
    double* offset = static_cast<double*>(mxGetData(prhs[5]));
    double* scale = static_cast<double*>(mxGetData(prhs[6]));
    double* det = static_cast<double*>(mxGetData(prhs[7]));
    unsigned char* mask=NULL;
    bool usingMask=false;
    vector<int> grid_dim(3,0);
    int type=1;
   int stride=3;
        int ksize=5;
    if(nrhs>9){
        double* ldim = static_cast<double*>(mxGetData(prhs[9]));
        grid_dim[0]=(int)ldim[0];
        grid_dim[1]=(int)ldim[1];
        grid_dim[2]=(int)ldim[2];
    }
        if(nrhs>10){
        double* lksize = static_cast<double*>(mxGetData(prhs[10]));
        ksize=(int)lksize[0];
    }
            if(nrhs>11){
        double* lstride = static_cast<double*>(mxGetData(prhs[11]));
        stride=(int)lstride[0];
    }
    if(nrhs>12&&mxGetClassID(prhs[12])==mxUINT8_CLASS){
        mask = static_cast<unsigned char*>(mxGetData(prhs[12]));
       usingMask=true;
    }
    if(nrhs>8){
        double* ltype = static_cast<double*>(mxGetData(prhs[8]));
        type=(int)ltype[0];
    }
    mexPrintf("type: %d ",type);
    
    //type=1;
    const mwSize* dim_img=mxGetDimensions(prhs[2]);
    const mwSize* dim_pts=mxGetDimensions(prhs[0]);
    const mwSize* dim_offset=mxGetDimensions(prhs[5]);
    const mwSize* dim_scale=mxGetDimensions(prhs[6]);
    //local variables
    int N=(int)dim_pts[0];
    
    if(2<mxGetNumberOfDimensions(prhs[0])) N=(int)(dim_pts[0]*dim_pts[1]*dim_pts[2]);
    
    int ndim =(int)mxGetNumberOfDimensions(prhs[2]);
    
    //copy data from matlab to local variables
    vector<double> lpts(pts,pts+(N*3));
    
    vector<double> ldataR(dataR,dataR+N);
    //memcpy(ldataR,dataR,sizeof(double)*N);
    
    //vector<double> ldataI(dataI,dataI+dim_img[0]*dim_img[1]*dim_img[2]);
    //memcpy(ldataI,dataI,sizeof(double)*dim_img[0]*dim_img[1]*dim_img[2]);
    
    vector<double> lrange(range,range+4);
    //memcpy(lrange,range,sizeof(double)*4);
    
    vector<int> ldim_img(3,0);
    ldim_img[0]=dim_img[0];
    ldim_img[1]=dim_img[1];
    ldim_img[2]=dim_img[2];
    
    vector<int> no_bins(2,0);
    no_bins[0]=n_bins[0];
    no_bins[1]=n_bins[1];
    
    vector<double> loffset(3);
    loffset[0]=offset[0];
    loffset[1]=offset[1];
    loffset[2]=offset[2];
    
    vector<double> lscale(3);
    lscale[0]=scale[0];
    lscale[1]=scale[1];
    lscale[2]=scale[2];
    
    vector<double> ldet(det,det+N);
    //end copy to local
    
    //allocate histogram
    
    //end Allocate histogram
    
    
    
    ////Allocate Tc
    mwSize dims[2];
    mwSize dims2[1];
    
    dims2[0] = 1;
    
    
    
    dims[0] = N; dims[1] = 1;
    vector<int> ldims(2);
    ldims[0] = N;
    ldims[1] = 1;
    vector<double> vals(N,0);
    vector<double> diff1(N,0);
    vector<double> diff2(N,0);
    vector<double> diff3(N,0);
    double HRI=0;
    double HI=0;
    double HR=0;
    
    double val = 0;
    
    
    
    //Initializing bins to 0
// 		for(int i=0;i<no_bins[0];i++){
// 			pI[i]=0;
// 			for(int j=0;j<no_bins[1];j++){
// 				pRI[j + i*(int)no_bins[1]]=0;
// 				pR[j]=0;
// 			}
// 		}
    if(mxGetClassID(prhs[2])==mxDOUBLE_CLASS){
        
        interp<double> spl((double*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    }
    if(mxGetClassID(prhs[2])==mxSINGLE_CLASS){
        
        interp<float> spl((float*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    }
    if(mxGetClassID(prhs[2])==mxINT32_CLASS){
        
        interp<int> spl((int*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    }
    if(mxGetClassID(prhs[2])==mxUINT32_CLASS){
        
        interp<unsigned int> spl((unsigned int*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    }
    if(mxGetClassID(prhs[2])==mxUINT16_CLASS){
        
        interp<unsigned short> spl((unsigned short*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    } 
    if(mxGetClassID(prhs[2])==mxINT16_CLASS){
        
        interp<short> spl((short*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    }
    if(mxGetClassID(prhs[2])==mxUINT8_CLASS){
        
        interp<unsigned char> spl((unsigned char*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    }
     if(mxGetClassID(prhs[2])==mxINT8_CLASS){
        
        interp<char> spl((char*)dataI,loffset.data(),lscale.data(),ndim,ldim_img.data(),ldims.data());
        spl.run(lpts.data(),diff1.data(),diff2.data(),diff3.data(),vals.data(),N);
        
    }
    // making the histogram
    
    
    
//          mexPrintf("sumPIR: %f \n",sumPIR);
//          mexPrintf("sumPR: %f \n",sumPR);
//         mexPrintf("detSum: %f \n",detSum);
    //compute the spatial derivatives of NMI
    
    //task_scheduler_init init2();
// 		mexPrintf("NMI calc. HR HI HRI.. %f %f %f\n",HR,HI,HRI);
    //parallel_for(blocked_range<int>(0,N,N),NMI2(lpts,ldataR,ldataI,loffset,lscale,ndim,ldim_img,dim_offset,dim_scale,N,diff1 ,diff2,diff3 ,dims,HI,HR,HRI,lrange, no_bins,LogpI,LogpRI,ldet,vals,&detSum),auto_partitioner());
    if(grid_dim[0]==0){
        mexPrintf("global \n");
        if(1==type){
            NCC ncc(ldataR.data(),ndim,ldim_img.data() ,lrange.data(), no_bins.data(),ldet.data(),vals.data());
            ncc.LOR::run(N,diff1.data() ,diff2.data(),diff3.data(),&val);
        }
        if(2==type){
            MI ncc(ldataR.data(),ndim,ldim_img.data() ,lrange.data(), no_bins.data(),ldet.data(),vals.data());
            ncc.LOR::run(N,diff1.data() ,diff2.data(),diff3.data(),&val);
        }
        if(3==type){
            NMI ncc(ldataR.data(),ndim,ldim_img.data() ,lrange.data(), no_bins.data(),ldet.data(),vals.data());
            ncc.LOR::run(N,diff1.data() ,diff2.data(),diff3.data(),&val);
        }
    }
     if(grid_dim[0]!=0){
      
//         vector<int> idx(ksize*ksize*ksize,0);
//         int end=ksize-1;
//         int start=0;
//         vector<double> t_val(1,0);
        vector<double> f_diff(N,0);
        //vector<double> tmp_diff(ksize*ksize*ksize,0);
        
          if(1==type){
            NCC ncc(ldataR.data(),ndim,ldim_img.data() ,lrange.data(), no_bins.data(),ldet.data(),vals.data());
            if(usingMask)
            ncc.LOR::locally(ksize,grid_dim.data(),stride,f_diff.data(),&val);
            
            else
                ncc.LOR::locally(ksize,grid_dim.data(),stride,f_diff.data(),&val,mask);
        }
        if(2==type){
            MI mi(ldataR.data(),ndim,ldim_img.data() ,lrange.data(), no_bins.data(),ldet.data(),vals.data());
            if(usingMask)
            mi.LOR::locally(ksize,grid_dim.data(),stride,f_diff.data(),&val,mask);
            else
                mi.LOR::locally(ksize,grid_dim.data(),stride,f_diff.data(),&val);
        }
        if(3==type){
            NMI nmi(ldataR.data(),ndim,ldim_img.data() ,lrange.data(), no_bins.data(),ldet.data(),vals.data());
            if(usingMask) 
            nmi.LOR::locally(ksize,grid_dim.data(),stride,f_diff.data(),&val,mask);
            else
                nmi.LOR::locally(ksize,grid_dim.data(),stride,f_diff.data(),&val);
        }
        for(int i=0;i<N;i++){
            diff1[i]=diff1[i]*f_diff[i];
            diff2[i]=diff2[i]*f_diff[i];
            diff3[i]=diff3[i]*f_diff[i];
        }
        
    }
    
    
    //val=((-HR)+(-HI))/(-HRI);
    
    
    /******allocate return variable and copy****/
    //allocate
    plhs[0] = mxCreateNumericArray(1,dims2,mxDOUBLE_CLASS, mxREAL);
    double* rval = static_cast<double*>(mxGetData(plhs[0]));
    plhs[1] = mxCreateNumericArray(2,dims,mxDOUBLE_CLASS, mxREAL);
    
    double* rdiff1 = static_cast<double*>(mxGetData(plhs[1]));
    plhs[2] = mxCreateNumericArray(2,dims,mxDOUBLE_CLASS, mxREAL);
    double* rdiff2 = static_cast<double*>(mxGetData(plhs[2]));
    
    plhs[3] = mxCreateNumericArray(2,dims,mxDOUBLE_CLASS, mxREAL);
    double* rdiff3 = static_cast<double*>(mxGetData(plhs[3]));
    //copy
    rval[0]=val;
    memcpy(rdiff1,diff1.data(),sizeof(double)*N);
    memcpy(rdiff2,diff2.data(),sizeof(double)*N);
    memcpy(rdiff3,diff3.data(),sizeof(double)*N);
    
    /*****end allocate return variable and copy****/
    
    
    
    //cleanup local variables
    
    return;
    
    
    
    
};
