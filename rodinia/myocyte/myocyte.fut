-- Rodinia's Myocyte benchmark translated to Futhark.
-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/myocyte/
--
-- ==
--
-- notravis input @ data/small.in
-- output @ data/small.out

----------------------
----- Ecc MODULE -----
----------------------

fun f32 pow(f32 x, f32 y) = x ** y
fun f32 log10(f32 x) = log32(x) / log32(10.0f32)

fun f32 fmod(f32 a, f32 b) = ( a - b * f32(int(a / b)) )

fun f32 fabs(f32 a) = if (a < 0.0f32) then -a else a

fun *[f32,EQUS] ecc( f32 timeinst, [f32,EQUS] initvalu, int initvalu_offset,
                    [f32,PARS] parameter, int parameter_offset, *[f32,EQUS] finavalu ) =
  -- variable references
  let offset_1  = initvalu_offset in
  let offset_2  = initvalu_offset+1 in
  let offset_3  = initvalu_offset+2 in
  let offset_4  = initvalu_offset+3 in
  let offset_5  = initvalu_offset+4 in
  let offset_6  = initvalu_offset+5 in
  let offset_7  = initvalu_offset+6 in
  let offset_8  = initvalu_offset+7 in
  let offset_9  = initvalu_offset+8 in
  let offset_10 = initvalu_offset+9 in
  let offset_11 = initvalu_offset+10 in
  let offset_12 = initvalu_offset+11 in
  let offset_13 = initvalu_offset+12 in
  let offset_14 = initvalu_offset+13 in
  let offset_15 = initvalu_offset+14 in
  let offset_16 = initvalu_offset+15 in
  let offset_17 = initvalu_offset+16 in
  let offset_18 = initvalu_offset+17 in
  let offset_19 = initvalu_offset+18 in
  let offset_20 = initvalu_offset+19 in
  let offset_21 = initvalu_offset+20 in
  let offset_22 = initvalu_offset+21 in
  let offset_23 = initvalu_offset+22 in
  let offset_24 = initvalu_offset+23 in
  let offset_25 = initvalu_offset+24 in
  let offset_26 = initvalu_offset+25 in
  let offset_27 = initvalu_offset+26 in
  let offset_28 = initvalu_offset+27 in
  let offset_29 = initvalu_offset+28 in
  let offset_30 = initvalu_offset+29 in
  let offset_31 = initvalu_offset+30 in
  let offset_32 = initvalu_offset+31 in
  let offset_33 = initvalu_offset+32 in
  let offset_34 = initvalu_offset+33 in
  let offset_35 = initvalu_offset+34 in
  let offset_36 = initvalu_offset+35 in
  let offset_37 = initvalu_offset+36 in
  let offset_38 = initvalu_offset+37 in
  let offset_39 = initvalu_offset+38 in
  let offset_40 = initvalu_offset+39 in
  let offset_41 = initvalu_offset+40 in
  let offset_42 = initvalu_offset+41 in
  let offset_43 = initvalu_offset+42 in
  let offset_44 = initvalu_offset+43 in
  let offset_45 = initvalu_offset+44 in
  let offset_46 = initvalu_offset+45 in
  
  -- variable references
  let parameter_offset_1  = parameter_offset in

  -- decoded input initial data
  let initvalu_1  = initvalu[offset_1 ] in
  let initvalu_2  = initvalu[offset_2 ] in
  let initvalu_3  = initvalu[offset_3 ] in
  let initvalu_4  = initvalu[offset_4 ] in
  let initvalu_5  = initvalu[offset_5 ] in
  let initvalu_6  = initvalu[offset_6 ] in
  let initvalu_7  = initvalu[offset_7 ] in
  let initvalu_8  = initvalu[offset_8 ] in
  let initvalu_9  = initvalu[offset_9 ] in
  let initvalu_10 = initvalu[offset_10] in
  let initvalu_11 = initvalu[offset_11] in
  let initvalu_12 = initvalu[offset_12] in
  let initvalu_13 = initvalu[offset_13] in
  let initvalu_14 = initvalu[offset_14] in
  let initvalu_15 = initvalu[offset_15] in
  let initvalu_16 = initvalu[offset_16] in
  let initvalu_17 = initvalu[offset_17] in
  let initvalu_18 = initvalu[offset_18] in
  let initvalu_19 = initvalu[offset_19] in
  let initvalu_20 = initvalu[offset_20] in
  let initvalu_21 = initvalu[offset_21] in
  let initvalu_22 = initvalu[offset_22] in
  let initvalu_23 = initvalu[offset_23] in
  let initvalu_24 = initvalu[offset_24] in
  let initvalu_25 = initvalu[offset_25] in
  let initvalu_26 = initvalu[offset_26] in
  let initvalu_27 = initvalu[offset_27] in
  let initvalu_28 = initvalu[offset_28] in
  let initvalu_29 = initvalu[offset_29] in
  let initvalu_30 = initvalu[offset_30] in
  let initvalu_31 = initvalu[offset_31] in
  let initvalu_32 = initvalu[offset_32] in
  let initvalu_33 = initvalu[offset_33] in
  let initvalu_34 = initvalu[offset_34] in
  let initvalu_35 = initvalu[offset_35] in
  let initvalu_36 = initvalu[offset_36] in
  let initvalu_37 = initvalu[offset_37] in
  let initvalu_38 = initvalu[offset_38] in
  let initvalu_39 = initvalu[offset_39] in
  let initvalu_40 = initvalu[offset_40] in
  let initvalu_41 = initvalu[offset_41] in
  let initvalu_42 = initvalu[offset_42] in
  let initvalu_43 = initvalu[offset_43] in
  let initvalu_44 = initvalu[offset_44] in
  let initvalu_45 = initvalu[offset_45] in
  let initvalu_46 = initvalu[offset_46] in

  -- decoded input parameters
  let parameter_1 = parameter[parameter_offset_1] in

  -- constants
  let pi   = 3.1416f32     in
  let R    = 8314.0f32     in -- [J/kmol*K]  
  let Frdy = 96485.0f32    in -- [C/mol]  
  let Temp = 310.0f32      in -- [K] 310
  let FoRT = Frdy/R/Temp   in
  let Cmem = 1.3810e-10f32 in -- [F] membrane capacitance
  let Qpow = (Temp-310.0f32)/10.0f32
  in
  -- Cell geometry
  let cellLength = 100.0f32   in  -- cell length [um]
  let cellRadius = 10.25f32   in  -- cell radius [um]
  let junctionLength = 160.0e-3f32 in  -- junc length [um]
  let junctionRadius = 15.0e-3f32  in  -- junc radius [um]
  let distSLcyto = 0.45f32    in  -- dist. SL to cytosol [um]
  let distJuncSL = 0.5f32     in  -- dist. junc to SL [um]
  let DcaJuncSL = 1.64e-6f32  in  -- Dca junc to SL [cm^2/sec]
  let DcaSLcyto = 1.22e-6f32  in  -- Dca SL to cyto [cm^2/sec]
  let DnaJuncSL = 1.09e-5f32  in  -- Dna junc to SL [cm^2/sec]
  let DnaSLcyto = 1.79e-5f32  in  -- Dna SL to cyto [cm^2/sec] 
  let Vcell = pi*pow(cellRadius,2.0f32)*cellLength*1.0e-15f32 in -- [L]
  let Vmyo = 0.65f32*Vcell    in
  let Vsr = 0.035f32*Vcell    in
  let Vsl = 0.02f32 *Vcell    in
  let Vjunc = 0.0539f32 * 0.01f32 * Vcell in 
  let SAjunc = 20150.0f32 * pi * 2.0f32 * junctionLength * junctionRadius in -- [um^2]
  let SAsl = pi * 2.0f32 * cellRadius * cellLength in -- [um^2]
  let J_ca_juncsl = 1.0f32/1.2134e12f32  in    -- [L/msec]
  let J_ca_slmyo  = 1.0f32/2.68510e11f32 in    -- [L/msec]
  let J_na_juncsl = 1.0f32/(1.6382e12f32 / 3.0f32 * 100.0f32) in -- [L/msec] 
  let J_na_slmyo  = 1.0f32/(1.8308e10f32 / 3.0f32 * 100.0f32) in -- [L/msec]
  
  -- Fractional currents in compartments
  let Fjunc = 0.11f32      in
  let Fsl = 1.0f32 - Fjunc in
  let Fjunc_CaL = 0.9f32   in
  let Fsl_CaL = 1.0f32 - Fjunc_CaL in

  -- Fixed ion concentrations
  let Cli = 15.0f32  in -- Intracellular Cl  [mM]
  let Clo = 150.0f32 in -- Extracellular Cl  [mM]
  let Ko = 5.4f32    in -- Extracellular K   [mM]
  let Nao = 140.0f32 in -- Extracellular Na  [mM]
  let Cao = 1.8f32   in -- Extracellular Ca  [mM]
  let Mgi = 1.0f32   in
  
  -- Nernst Potentials
  let ena_junc = (1.0f32/FoRT)*log32(Nao/initvalu_32)        in -- [mV]
  let ena_sl = (1.0f32/FoRT)*log32(Nao/initvalu_33)          in -- [mV]
  let ek = (1.0f32/FoRT)*log32(Ko/initvalu_35)               in -- [mV]
  let eca_junc = (1.0f32/FoRT/2.0f32)*log32(Cao/initvalu_36) in -- [mV]
  let eca_sl = (1.0f32/FoRT/2.0f32)*log32(Cao/initvalu_37)   in -- [mV]
  let ecl = (1.0f32/FoRT)*log32(Cli/Clo)                     in -- [mV]

  -- Na transport parameters
  let GNa =  16.0f32       in -- [mS/uF]
  let GNaB = 0.297e-3f32   in -- [mS/uF] 
  let IbarNaK = 1.90719f32 in -- [uA/uF]
  let KmNaip = 11.0f32     in -- [mM]
  let KmKo = 1.5f32        in -- [mM]
  let Q10NaK = 1.63f32     in
  let Q10KmNai = 1.39f32   
  in
  -- K current parameters
  let pNaK = 0.01833f32 in
  let GtoSlow = 0.06f32 in -- [mS/uF] 
  let GtoFast = 0.02f32 in -- [mS/uF] 
  let gkp = 0.001f32    
  in
  -- Cl current parameters
  let GClCa = 0.109625f32 in -- [mS/uF]
  let GClB = 9.0e-3f32      in -- [mS/uF]
  let KdClCa = 100.0e-3f32     -- [mM]
  in
  -- I_Ca parameters
  let pNa = 1.5e-8f32  in -- [cm/sec]
  let pCa = 5.4e-4f32  in -- [cm/sec]
  let pK = 2.7e-7f32   in -- [cm/sec]
  let KmCa = 0.6e-3f32 in -- [mM]
  let Q10CaL = 1.8f32 
  in
  -- Ca transport parameters
  let IbarNCX = 9.0f32   in -- [uA/uF]
  let KmCai = 3.59e-3f32 in -- [mM]
  let KmCao = 1.3f32     in -- [mM]
  let KmNai = 12.29f32   in -- [mM]
  let KmNao = 87.5f32    in -- [mM]
  let ksat = 0.27f32     in -- [none]  
  let nu = 0.35f32       in -- [none]
  let Kdact = 0.256e-3f32 in -- [mM] 
  let Q10NCX = 1.57f32   in -- [none]
  let IbarSLCaP = 0.0673f32 in -- [uA/uF]
  let KmPCa = 0.5e-3f32  in -- [mM] 
  let GCaB = 2.513e-4f32 in -- [uA/uF] 
  let Q10SLCaP = 2.35f32 in -- [none]

  -- SR flux parameters
  let Q10SRCaP = 2.6f32 in -- [none]
  let Vmax_SRCaP = 2.86e-4f32 in -- [mM/msec] (mmol/L cytosol/msec)
  let Kmf = 0.246e-3f32    in -- [mM]
  let Kmr = 1.7f32         in -- [mM]L cytosol
  let hillSRCaP = 1.787f32 in -- [mM]
  let ks   = 25.0f32 in -- [1/ms]      
  let koCa = 10.0f32 in -- [mM^-2 1/ms]      
  let kom  = 0.06f32 in -- [1/ms]     
  let kiCa = 0.5f32  in -- [1/mM/ms]
  let kim = 0.005f32 in -- [1/ms]
  let ec50SR = 0.45f32  in -- [mM]

  -- Buffering parameters
  let Bmax_Naj = 7.561f32 in -- [mM] 
  let Bmax_Nasl = 1.65f32 in -- [mM]
  let koff_na = 1.0e-3f32   in -- [1/ms]
  let kon_na = 0.1e-3f32  in -- [1/mM/ms]
  let Bmax_TnClow = 70e-3f32 in -- [mM], TnC low affinity
  let koff_tncl = 19.6e-3f32 in -- [1/ms] 
  let kon_tncl = 32.7f32     in -- [1/mM/ms]
  let Bmax_TnChigh = 140.0e-3f32  in -- [mM], TnC high affinity 
  let koff_tnchca = 0.032e-3f32 in -- [1/ms] 
  let kon_tnchca = 2.37f32      in -- [1/mM/ms]
  let koff_tnchmg = 3.33e-3f32  in -- [1/ms] 
  let kon_tnchmg = 3.0e-3f32      in -- [1/mM/ms]
  let Bmax_CaM = 24.0e-3f32       in -- [mM], CaM buffering
  let koff_cam = 238.0e-3f32      in -- [1/ms] 
  let kon_cam = 34.0f32           in -- [1/mM/ms]
  let Bmax_myosin = 140.0e-3f32   in -- [mM], Myosin buffering
  let koff_myoca = 0.46e-3f32     in -- [1/ms]
  let kon_myoca = 13.8f32         in -- [1/mM/ms]
  let koff_myomg = 0.057e-3f32    in -- [1/ms]
  let kon_myomg = 0.0157f32       in -- [1/mM/ms]
  let Bmax_SR = 19.0f32 * 0.9e-3f32 in -- [mM] 
  let koff_sr = 60.0e-3f32        in -- [1/ms]
  let kon_sr = 100.0f32           in -- [1/mM/ms]
  let Bmax_SLlowsl = 37.38e-3f32 * Vmyo / Vsl in -- [mM], SL buffering
  let Bmax_SLlowj = 4.62e-3f32   * Vmyo / Vjunc * 0.1f32 in -- [mM]    
  let koff_sll = 1300.0e-3f32     in -- [1/ms]
  let kon_sll = 100.0f32          in -- [1/mM/ms]
  let Bmax_SLhighsl = 13.35e-3f32 * Vmyo / Vsl in -- [mM] 
  let Bmax_SLhighj = 1.65e-3f32 * Vmyo / Vjunc * 0.1f32 in -- [mM] 
  let koff_slh = 30e-3f32 in -- [1/ms]
  let kon_slh = 100.0f32  in -- [1/mM/ms]
  let Bmax_Csqn = 2.7f32  in -- 140e-3*Vmyo/Vsr; [mM]
  let koff_csqn = 65.0f32 in -- [1/ms]
  let kon_csqn = 100.0f32 in -- [1/mM/ms]

  -- I_Na: Fast Na Current
  let am = 0.32f32 * (initvalu_39 + 47.13f32) / (1.0f32 - exp32(-0.1f32*(initvalu_39+47.13f32))) in
  let bm = 0.08f32 * exp32(-initvalu_39/11.0f32) in
  let { ah, aj, bh, bj } =  if (initvalu_39 >= -40.0f32)
                            then { 0.0f32, 
                                   0.0f32,
                                   1.0f32/(0.13f32*(1.0f32+exp32(-(initvalu_39+10.66f32)/11.1f32))),
                                   0.3f32*exp32(-2.535e-7f32*initvalu_39)/(1.0f32+exp32(-0.1f32*(initvalu_39+32.0f32))) 
                                 }
                            else { 0.135f32*exp32((80.0f32+initvalu_39)/-6.8f32),
                                   ( -127140.0f32*exp32(0.2444f32*initvalu_39) - 3.474e-5f32*exp32(-0.04391f32*initvalu_39) ) *
                                        (initvalu_39+37.78f32)/(1.0f32+exp32(0.311f32*(initvalu_39+79.23f32))),
                                   3.56f32*exp32(0.079f32*initvalu_39)+3.1e5f32*exp32(0.35f32*initvalu_39),
                                   0.1212f32 * exp32(-0.01052f32*initvalu_39) / (1.0f32 + exp32(-0.1378f32 * (initvalu_39+40.14f32)))
                                 }
  in
  let finavalu[offset_1] = am*(1.0f32-initvalu_1) - bm*initvalu_1 in
  let finavalu[offset_2] = ah*(1.0f32-initvalu_2) - bh*initvalu_2 in
  let finavalu[offset_3] = aj*(1.0f32-initvalu_3) - bj*initvalu_3 in
  let I_Na_junc = Fjunc*GNa*pow(initvalu_1,3.0f32)*initvalu_2*initvalu_3*(initvalu_39-ena_junc) in
  let I_Na_sl = Fsl*GNa*pow(initvalu_1,3.0f32)*initvalu_2*initvalu_3*(initvalu_39-ena_sl) in
  let I_Na = I_Na_junc + I_Na_sl 
  in
  -- I_nabk: Na Background Current
  let I_nabk_junc = Fjunc*GNaB*(initvalu_39-ena_junc) in
  let I_nabk_sl   = Fsl  *GNaB*(initvalu_39-ena_sl  ) in
  let I_nabk      = I_nabk_junc + I_nabk_sl           in

  -- I_nak: Na/K Pump Current
  let sigma = (exp32(Nao/67.3f32)-1.0f32)/7.0f32 in
  let fnak = 1.0f32/(1.0f32+0.1245f32*exp32(-0.1f32*initvalu_39*FoRT)+0.0365f32*sigma*exp32(-initvalu_39*FoRT)) in
  let I_nak_junc = Fjunc*IbarNaK*fnak*Ko / (1.0f32+pow((KmNaip/initvalu_32),4.0f32)) /(Ko+KmKo) in
  let I_nak_sl = Fsl*IbarNaK*fnak*Ko /(1.0f32+pow((KmNaip/initvalu_33),4.0f32)) /(Ko+KmKo) in
  let I_nak = I_nak_junc + I_nak_sl 
  in
  -- I_kr: Rapidly Activating K Current
  let gkr = 0.03f32 * sqrt32(Ko/5.4f32) in
  let xrss = 1.0f32 / (1.0f32 + exp32(-(initvalu_39+50.0f32)/7.5f32)) in
  let tauxr = 1.0f32/(0.00138f32*(initvalu_39+7.0f32) / (1.0f32-exp32(-0.123f32*(initvalu_39+7.0f32))) +
                6.1e-4f32*(initvalu_39+10.0f32)/(exp32(0.145f32*(initvalu_39+10.0f32))-1.0f32)) in
  let finavalu[offset_12] = (xrss-initvalu_12)/tauxr in
  let rkr = 1.0f32 / (1.0f32 + exp32((initvalu_39+33.0f32)/22.4f32)) in
  let I_kr = gkr*initvalu_12*rkr*(initvalu_39-ek)
  in
  -- I_ks: Slowly Activating K Current
  let pcaks_junc = -log10(initvalu_36)+3.0f32 in
  let pcaks_sl = -log10(initvalu_37)+3.0f32 in  
  let gks_junc = 0.07f32*(0.057f32 +0.19f32/(1.0f32+ exp32((-7.2f32+pcaks_junc)/0.6f32))) in
  let gks_sl = 0.07f32*(0.057f32 +0.19f32/(1.0f32+ exp32((-7.2f32+pcaks_sl)/0.6f32))) in 
  let eks = (1.0f32/FoRT)*log32((Ko+pNaK*Nao)/(initvalu_35+pNaK*initvalu_34)) in
  let xsss = 1.0f32/(1.0f32+exp32(-(initvalu_39-1.5f32)/16.7f32)) in
  let tauxs = 1.0f32/(7.19e-5f32*(initvalu_39+30.0f32)/(1.0f32-exp32(-0.148f32*(initvalu_39+30.0f32))) + 
                1.31e-4f32*(initvalu_39+30.0f32)/(exp32(0.0687f32*(initvalu_39+30.0f32))-1.0f32)) in
  let finavalu[offset_13] = (xsss-initvalu_13) / tauxs in
  let I_ks_junc = Fjunc*gks_junc*pow(initvalu_12,2.0f32)*(initvalu_39-eks) in
  let I_ks_sl = Fsl*gks_sl*pow(initvalu_13,2.0f32)*(initvalu_39-eks) in
  let I_ks = I_ks_junc+I_ks_sl
  in
  -- I_kp: Plateau K current
  let kp_kp = 1.0f32/(1.0f32+exp32(7.488f32-initvalu_39/5.98f32)) in
  let I_kp_junc = Fjunc*gkp*kp_kp*(initvalu_39-ek) in
  let I_kp_sl = Fsl*gkp*kp_kp*(initvalu_39-ek) in
  let I_kp = I_kp_junc+I_kp_sl in

  -- I_to: Transient Outward K Current (slow and fast components)
  let xtoss = 1.0f32/(1.0f32+exp32(-(initvalu_39+3.0f32)/15.0f32)) in
  let ytoss = 1.0f32/(1.0f32+exp32((initvalu_39+33.5f32)/10.0f32)) in
  let rtoss = 1.0f32/(1.0f32+exp32((initvalu_39+33.5f32)/10.0f32)) in
  let tauxtos = 9.0f32/(1.0f32+exp32((initvalu_39+3.0f32)/15.0f32))+0.5f32 in
  let tauytos = 3.0e3f32/(1.0f32+exp32((initvalu_39+60.0f32)/10.0f32))+30.0f32 in
  let taurtos = 2800.0f32/(1.0f32+exp32((initvalu_39+60.0f32)/10.0f32))+220.0f32 in
  let finavalu[offset_8] = (xtoss-initvalu_8)/tauxtos in
  let finavalu[offset_9] = (ytoss-initvalu_9)/tauytos in
  let finavalu[offset_40]= (rtoss-initvalu_40)/taurtos in
  let I_tos = GtoSlow*initvalu_8*(initvalu_9+0.5f32*initvalu_40)*(initvalu_39-ek) -- [uA/uF]
  in
  -- 
  let tauxtof = 3.5f32*exp32(-initvalu_39*initvalu_39/30.0f32/30.0f32)+1.5f32 in
  let tauytof = 20.0f32/(1.0f32+exp32((initvalu_39+33.5f32)/10.0f32))+20.0f32 in
  let finavalu[offset_10] = (xtoss-initvalu_10)/tauxtof in
  let finavalu[offset_11] = (ytoss-initvalu_11)/tauytof in
  let I_tof = GtoFast*initvalu_10*initvalu_11*(initvalu_39-ek) in
  let I_to = I_tos + I_tof 
  in
  -- I_ki: Time-Independent K Current
  let aki = 1.02f32/(1.0f32+exp32(0.2385f32*(initvalu_39-ek-59.215f32))) in
  let bki =(0.49124f32*exp32(0.08032f32*(initvalu_39+5.476f32-ek)) + exp32(0.06175f32*(initvalu_39-ek-594.31f32))) /
                (1.0f32 + exp32(-0.5143f32*(initvalu_39-ek+4.753f32))) in
  let kiss = aki/(aki+bki) in
  let I_ki = 0.9f32*sqrt32(Ko/5.4f32)*kiss*(initvalu_39-ek) 
  in
  -- I_ClCa: Ca-activated Cl Current, I_Clbk: background Cl Current
  let I_ClCa_junc = Fjunc*GClCa/(1.0f32+KdClCa/initvalu_36)*(initvalu_39-ecl) in
  let I_ClCa_sl = Fsl*GClCa/(1.0f32+KdClCa/initvalu_37)*(initvalu_39-ecl) in
  let I_ClCa = I_ClCa_junc+I_ClCa_sl in
  let I_Clbk = GClB*(initvalu_39-ecl) 
  in
  -- I_Ca: L-type Calcium Current
  let dss = 1.0f32/(1.0f32+exp32(-(initvalu_39+14.5f32)/6.0f32)) in
  let taud = dss*(1.0f32-exp32(-(initvalu_39+14.5f32)/6.0f32))/(0.035f32*(initvalu_39+14.5f32)) in
  let fss = 1.0f32/(1.0f32+exp32((initvalu_39+35.06f32)/3.6f32))+0.6f32/(1.0f32+exp32((50.0f32-initvalu_39)/20.0f32)) in
  let tauf = 1.0f32/(0.0197f32*exp32(-pow(0.0337f32*(initvalu_39+14.5f32),2.0f32))+0.02f32) in
  let finavalu[offset_4] = (dss-initvalu_4)/taud in
  let finavalu[offset_5] = (fss-initvalu_5)/tauf in
  let finavalu[offset_6] = 1.7f32*initvalu_36*(1.0f32-initvalu_6)-11.9e-3f32*initvalu_6 in -- fCa_junc  
  let finavalu[offset_7] = 1.7f32*initvalu_37*(1.0f32-initvalu_7)-11.9e-3f32*initvalu_7    -- fCa_sl
  in
  -- 
  let ibarca_j = pCa*4.0f32*(initvalu_39*Frdy*FoRT) * (0.341f32*initvalu_36*exp32(2.0f32*initvalu_39*FoRT)-0.341f32*Cao) /
                    (exp32(2.0f32*initvalu_39*FoRT)-1.0f32) in
  let ibarca_sl= pCa*4.0f32*(initvalu_39*Frdy*FoRT) * (0.341f32*initvalu_37*exp32(2.0f32*initvalu_39*FoRT)-0.341f32*Cao) /
                    (exp32(2.0f32*initvalu_39*FoRT)-1.0f32) in
  let ibark = pK*(initvalu_39*Frdy*FoRT)*(0.75f32*initvalu_35*exp32(initvalu_39*FoRT)-0.75f32*Ko) /
                    (exp32(initvalu_39*FoRT)-1.0f32) in
  let ibarna_j = pNa*(initvalu_39*Frdy*FoRT) *(0.75f32*initvalu_32*exp32(initvalu_39*FoRT)-0.75f32*Nao) /
                    (exp32(initvalu_39*FoRT)-1.0f32) in
  let ibarna_sl= pNa*(initvalu_39*Frdy*FoRT) *(0.75f32*initvalu_33*exp32(initvalu_39*FoRT)-0.75f32*Nao) /
                    (exp32(initvalu_39*FoRT)-1.0f32) in
  let I_Ca_junc = (Fjunc_CaL*ibarca_j*initvalu_4*initvalu_5*(1.0f32-initvalu_6)*pow(Q10CaL,Qpow))*0.45f32 in
  let I_Ca_sl   = (Fsl_CaL*ibarca_sl*initvalu_4*initvalu_5*(1.0f32-initvalu_7)*pow(Q10CaL,Qpow))*0.45f32  in
  let I_Ca = I_Ca_junc+I_Ca_sl in
  let finavalu[offset_43] = -I_Ca*Cmem/(Vmyo*2.0f32*Frdy)*1e3f32 in
  let I_CaK = (ibark*initvalu_4*initvalu_5*(Fjunc_CaL*(1.0f32-initvalu_6)+Fsl_CaL*(1.0f32-initvalu_7))*pow(Q10CaL,Qpow))*0.45f32 in
  let I_CaNa_junc = (Fjunc_CaL*ibarna_j*initvalu_4*initvalu_5*(1.0f32-initvalu_6)*pow(Q10CaL,Qpow))*0.45f32 in
  let I_CaNa_sl = (Fsl_CaL*ibarna_sl*initvalu_4*initvalu_5*(1.0f32-initvalu_7)*pow(Q10CaL,Qpow))*0.45f32 in
  let I_CaNa = I_CaNa_junc+I_CaNa_sl in
  let I_Catot = I_Ca+I_CaK+I_CaNa 
  in
  -- I_ncx: Na/Ca Exchanger flux
  let Ka_junc = 1.0f32/(1.0f32+pow((Kdact/initvalu_36),3.0f32)) in
  let Ka_sl = 1.0f32/(1.0f32+pow((Kdact/initvalu_37),3.0f32)) in
  let s1_junc = exp32(nu*initvalu_39*FoRT)*pow(initvalu_32,3.0f32)*Cao in
  let s1_sl = exp32(nu*initvalu_39*FoRT)*pow(initvalu_33,3.0f32)*Cao in
  let s2_junc = exp32((nu-1.0f32)*initvalu_39*FoRT)*pow(Nao,3.0f32)*initvalu_36 in
  let s3_junc = (KmCai*pow(Nao,3.0f32)*(1.0f32+pow((initvalu_32/KmNai),3.0f32))+pow(KmNao,3.0f32)*initvalu_36 +
                    pow(KmNai,3.0f32)*Cao*(1.0f32+initvalu_36/KmCai)+KmCao*pow(initvalu_32,3.0f32)+pow(initvalu_32,3.0f32)*Cao +
                    pow(Nao,3.0f32)*initvalu_36)*(1.0f32+ksat*exp32((nu-1.0f32)*initvalu_39*FoRT)) in
  let s2_sl = exp32((nu-1.0f32)*initvalu_39*FoRT)*pow(Nao,3.0f32)*initvalu_37 in
  let s3_sl = (KmCai*pow(Nao,3.0f32)*(1.0f32+pow((initvalu_33/KmNai),3.0f32)) +
                pow(KmNao,3.0f32)*initvalu_37+pow(KmNai,3.0f32)*Cao*(1.0f32+initvalu_37/KmCai) +
                KmCao*pow(initvalu_33,3.0f32)+pow(initvalu_33,3.0f32)*Cao+pow(Nao,3.0f32)*initvalu_37)*
                (1.0f32+ksat*exp32((nu-1.0f32)*initvalu_39*FoRT)) in
  let I_ncx_junc = Fjunc*IbarNCX*pow(Q10NCX,Qpow)*Ka_junc*(s1_junc-s2_junc)/s3_junc in
  let I_ncx_sl = Fsl*IbarNCX*pow(Q10NCX,Qpow)*Ka_sl*(s1_sl-s2_sl)/s3_sl in
  let I_ncx = I_ncx_junc+I_ncx_sl in
  let finavalu[offset_45] = 2.0f32*I_ncx*Cmem/(Vmyo*2.0f32*Frdy)*1e3f32
  in
  -- I_pca: Sarcolemmal Ca Pump Current
  let I_pca_junc =  Fjunc *
                    pow(Q10SLCaP,Qpow) * 
                    IbarSLCaP *
                    pow(initvalu_36,1.6f32) /
                    (pow(KmPCa,1.6f32) + pow(initvalu_36,1.6f32)) in
  let I_pca_sl = Fsl *
                pow(Q10SLCaP,Qpow) *
                IbarSLCaP *
                pow(initvalu_37,1.6f32) / 
                (pow(KmPCa,1.6f32) + pow(initvalu_37,1.6f32)) in 
  let I_pca = I_pca_junc + I_pca_sl in
  let finavalu[offset_44] = -I_pca*Cmem/(Vmyo*2.0f32*Frdy)*1e3f32 
  in
  -- I_cabk: Ca Background Current
  let I_cabk_junc = Fjunc*GCaB*(initvalu_39-eca_junc) in
  let I_cabk_sl = Fsl*GCaB*(initvalu_39-eca_sl) in
  let I_cabk = I_cabk_junc+I_cabk_sl in
  let finavalu[offset_46] = -I_cabk*Cmem/(Vmyo*2.0f32*Frdy)*1e3f32 
  in
  -- SR fluxes: Calcium Release, SR Ca pump, SR Ca leak
  let MaxSR = 15.0f32 in
  let MinSR = 1.0f32  in
  let kCaSR = MaxSR - (MaxSR-MinSR)/(1.0f32+pow(ec50SR/initvalu_31,2.5f32)) in
  let koSRCa = koCa/kCaSR in
  let kiSRCa = kiCa*kCaSR in
  let RI = 1.0f32 - initvalu_14 - initvalu_15 - initvalu_16 in
  let finavalu[offset_14] = (kim*RI-kiSRCa*initvalu_36*initvalu_14) - 
                            (koSRCa*pow(initvalu_36,2.0f32) * initvalu_14-kom*initvalu_15) in -- R
  let finavalu[offset_15] = (koSRCa*pow(initvalu_36,2.0f32) * initvalu_14-kom*initvalu_15) - 
                            (kiSRCa*initvalu_36*initvalu_15-kim*initvalu_16) in -- O
  let finavalu[offset_16] = (kiSRCa*initvalu_36*initvalu_15-kim*initvalu_16) - 
                            (kom*initvalu_16-koSRCa*pow(initvalu_36,2.0f32)*RI) in -- I
  let J_SRCarel = ks*initvalu_15*(initvalu_31-initvalu_36) in -- [mM/ms]
  let J_serca = pow(Q10SRCaP,Qpow)*Vmax_SRCaP*(pow((initvalu_38/Kmf),hillSRCaP)-pow((initvalu_31/Kmr),hillSRCaP))
                                        /(1.0f32+pow((initvalu_38/Kmf),hillSRCaP)+pow((initvalu_31/Kmr),hillSRCaP)) in
  let J_SRleak = 5.348e-6f32*(initvalu_31-initvalu_36) -- [mM/ms]
  in
  -- Sodium and Calcium Buffering
  let finavalu[offset_17] = kon_na*initvalu_32*(Bmax_Naj-initvalu_17)-koff_na*initvalu_17   in -- NaBj  [mM/ms]
  let finavalu[offset_18] = kon_na*initvalu_33*(Bmax_Nasl-initvalu_18)-koff_na*initvalu_18  in -- NaBsl [mM/ms]
  -- Cytosolic Ca Buffers
  let finavalu[offset_19] = kon_tncl*initvalu_38*(Bmax_TnClow-initvalu_19)-koff_tncl*initvalu_19 in -- TnCL [mM/ms]
  let finavalu[offset_20] = kon_tnchca*initvalu_38*(Bmax_TnChigh-initvalu_20-initvalu_21)-koff_tnchca*initvalu_20 in -- TnCHc [mM/ms]
  let finavalu[offset_21] = kon_tnchmg*Mgi*(Bmax_TnChigh-initvalu_20-initvalu_21)-koff_tnchmg*initvalu_21 in -- TnCHm [mM/ms]
  let finavalu[offset_22] = 0.0f32 in -- CaM [mM/ms]
  let finavalu[offset_23] = kon_myoca*initvalu_38*(Bmax_myosin-initvalu_23-initvalu_24)-koff_myoca*initvalu_23 in -- Myosin_ca [mM/ms]
  let finavalu[offset_24] = kon_myomg*Mgi*(Bmax_myosin-initvalu_23-initvalu_24)-koff_myomg*initvalu_24 in -- Myosin_mg [mM/ms]
  let finavalu[offset_25] = kon_sr*initvalu_38*(Bmax_SR-initvalu_25)-koff_sr*initvalu_25 in -- SRB [mM/ms]
  let J_CaB_cytosol =   finavalu[offset_19] + finavalu[offset_20] + finavalu[offset_21] + 
                        finavalu[offset_22] + finavalu[offset_23] + finavalu[offset_24] + finavalu[offset_25]
  in
  -- Junctional and SL Ca Buffers
  let finavalu[offset_26] = kon_sll*initvalu_36*(Bmax_SLlowj-initvalu_26)-koff_sll*initvalu_26  in -- SLLj  [mM/ms]
  let finavalu[offset_27] = kon_sll*initvalu_37*(Bmax_SLlowsl-initvalu_27)-koff_sll*initvalu_27 in -- SLLsl [mM/ms]
  let finavalu[offset_28] = kon_slh*initvalu_36*(Bmax_SLhighj-initvalu_28)-koff_slh*initvalu_28 in -- SLHj  [mM/ms]
  let finavalu[offset_29] = kon_slh*initvalu_37*(Bmax_SLhighsl-initvalu_29)-koff_slh*initvalu_29 in-- SLHsl [mM/ms]
  let J_CaB_junction = finavalu[offset_26]+finavalu[offset_28] in
  let J_CaB_sl = finavalu[offset_27]+finavalu[offset_29]
  in
  -- SR Ca Concentrations
  let finavalu[offset_30] = kon_csqn*initvalu_31*(Bmax_Csqn-initvalu_30)-koff_csqn*initvalu_30 in -- Csqn [mM/ms]
  let oneovervsr = 1.0f32 / Vsr in
  let finavalu[offset_31] = J_serca*Vmyo*oneovervsr-(J_SRleak*Vmyo*oneovervsr+J_SRCarel)-finavalu[offset_30] -- Ca_sr [mM/ms]
  in
  -- Sodium Concentrations
  let I_Na_tot_junc = I_Na_junc+I_nabk_junc+3.0f32*I_ncx_junc+3.0f32*I_nak_junc+I_CaNa_junc in -- [uA/uF]
  let I_Na_tot_sl = I_Na_sl+I_nabk_sl+3.0f32*I_ncx_sl+3.0f32*I_nak_sl+I_CaNa_sl in -- [uA/uF]
  let finavalu[offset_32] = -I_Na_tot_junc*Cmem/(Vjunc*Frdy)+J_na_juncsl/Vjunc*(initvalu_33-initvalu_32)-finavalu[offset_17] in
  let oneovervsl = 1.0f32 / Vsl in
  let finavalu[offset_33] = -I_Na_tot_sl*Cmem*oneovervsl/Frdy+J_na_juncsl*oneovervsl*(initvalu_32-initvalu_33) +
                            J_na_slmyo*oneovervsl*(initvalu_34-initvalu_33)-finavalu[offset_18] in
  let finavalu[offset_34] = J_na_slmyo/Vmyo*(initvalu_33-initvalu_34) -- [mM/msec] 
  in
  -- Potassium Concentration
  let I_K_tot = I_to+I_kr+I_ks+I_ki-2.0f32*I_nak+I_CaK+I_kp in -- [uA/uF]
  let finavalu[offset_35] = 0.0f32 -- [mM/msec]
  in
  -- Calcium Concentrations
  let I_Ca_tot_junc = I_Ca_junc+I_cabk_junc+I_pca_junc-2.0f32*I_ncx_junc in -- [uA/uF]
  let I_Ca_tot_sl = I_Ca_sl+I_cabk_sl+I_pca_sl-2.0f32*I_ncx_sl in -- [uA/uF]
  let finavalu[offset_36] = -I_Ca_tot_junc*Cmem/(Vjunc*2.0f32*Frdy)+
                            J_ca_juncsl/Vjunc*(initvalu_37-initvalu_36) - 
                            J_CaB_junction+(J_SRCarel)*Vsr/Vjunc+J_SRleak*Vmyo/Vjunc in -- Ca_j
  let finavalu[offset_37] = -I_Ca_tot_sl*Cmem/(Vsl*2.0f32*Frdy) + 
                            J_ca_juncsl/Vsl*(initvalu_36-initvalu_37) +
                            J_ca_slmyo/Vsl*(initvalu_38-initvalu_37)-J_CaB_sl in -- Ca_sl
  let finavalu[offset_38] = -J_serca-J_CaB_cytosol +J_ca_slmyo/Vmyo*(initvalu_37-initvalu_38) in
  let junc_sl=J_ca_juncsl/Vsl*(initvalu_36-initvalu_37) in
  let sl_junc=J_ca_juncsl/Vjunc*(initvalu_37-initvalu_36) in
  let sl_myo=J_ca_slmyo/Vsl*(initvalu_38-initvalu_37) in
  let myo_sl=J_ca_slmyo/Vmyo*(initvalu_37-initvalu_38) 
  in
  let I_app = if(fmod(timeinst,parameter_1) <= 5.0f32)
              then 9.5f32 else 0.0f32
  in
  -- Membrane Potential
  let I_Na_tot = I_Na_tot_junc + I_Na_tot_sl in -- [uA/uF]
  let I_Cl_tot = I_ClCa+I_Clbk in -- [uA/uF]
  let I_Ca_tot = I_Ca_tot_junc + I_Ca_tot_sl in
  let I_tot = I_Na_tot+I_Cl_tot+I_Ca_tot+I_K_tot in
  let finavalu[offset_39] = -(I_tot-I_app) 
  in
  -- Set unused output values to 0 (MATLAB does it by default)
  let finavalu[offset_41] = 0.0f32 in
  let finavalu[offset_42] = 0.0f32 in
  finavalu


----------------------
----- Cam MODULE -----
----------------------

fun {f32, *[f32,EQUS]} cam( f32 timeinst, [f32,EQUS] initvalu, 
                            int initvalu_offset,
                            [f32,PARS] parameter, int parameter_offset, 
                            *[f32,EQUS] finavalu, f32 Ca ) =
  -- input data and output data variable references
  let offset_1  = initvalu_offset in
  let offset_2  = initvalu_offset+1 in
  let offset_3  = initvalu_offset+2 in
  let offset_4  = initvalu_offset+3 in
  let offset_5  = initvalu_offset+4 in
  let offset_6  = initvalu_offset+5 in
  let offset_7  = initvalu_offset+6 in
  let offset_8  = initvalu_offset+7 in
  let offset_9  = initvalu_offset+8 in
  let offset_10 = initvalu_offset+9 in
  let offset_11 = initvalu_offset+10 in
  let offset_12 = initvalu_offset+11 in
  let offset_13 = initvalu_offset+12 in
  let offset_14 = initvalu_offset+13 in
  let offset_15 = initvalu_offset+14
  in
  -- input parameters variable references
  let parameter_offset_1  = parameter_offset in
  let parameter_offset_2  = parameter_offset+1 in
  let parameter_offset_3  = parameter_offset+2 in
  let parameter_offset_4  = parameter_offset+3 in
  let parameter_offset_5  = parameter_offset+4 
  in
  -- decoding input array
  let CaM   = initvalu[offset_1] in
  let Ca2CaM  = initvalu[offset_2] in
  let Ca4CaM  = initvalu[offset_3] in
  let CaMB    = initvalu[offset_4] in
  let Ca2CaMB = initvalu[offset_5] in
  let Ca4CaMB = initvalu[offset_6] in           
  let Pb2     = initvalu[offset_7] in
  let Pb      = initvalu[offset_8] in
  let Pt      = initvalu[offset_9] in
  let Pt2     = initvalu[offset_10] in
  let Pa      = initvalu[offset_11] in                            
  let Ca4CaN  = initvalu[offset_12] in
  let CaMCa4CaN = initvalu[offset_13] in
  let Ca2CaMCa4CaN = initvalu[offset_14] in
  let Ca4CaMCa4CaN = initvalu[offset_15] 
  in
  -- decoding input parameters
  let CaMtot    = parameter[parameter_offset_1] in
  let Btot      = parameter[parameter_offset_2] in
  let CaMKIItot = parameter[parameter_offset_3] in
  let CaNtot    = parameter[parameter_offset_4] in
  let PP1tot    = parameter[parameter_offset_5] 
  in
  -- values [CONSTANTS FOR ALL THREADS]
  let K = 135.0f32 in
  let Mg = 1.0f32  
  in
  -- Ca/CaM parameters
  let {Kd02, Kd24} = if (Mg <= 1.0f32)
                     then { 0.0025f32*(1.0f32+K/0.94f32-Mg/0.012f32)*(1.0f32+K/8.1f32+Mg/0.022f32),
                            0.128f32*(1.0f32+K/0.64f32+Mg/0.0014f32)*(1.0f32+K/13.0f32-Mg/0.153f32)
                          }
                     else { 0.0025f32*(1.0f32+K/0.94f32-1.0f32/0.012f32+(Mg-1.0f32)/0.060f32) *
                                (1.0f32+K/8.1f32+1.0f32/0.022f32+(Mg-1.0f32)/0.068f32),
                            0.128f32*(1.0f32+K/0.64f32+1.0f32/0.0014f32+(Mg-1.0f32)/0.005f32) *
                                (1.0f32+K/13.0f32-1.0f32/0.153f32+(Mg-1.0f32)/0.150f32)
                          }
  in
  let k20 = 10.0f32  in -- [s^-1]      
  let k02 = k20/Kd02 in -- [uM^-2 s^-1]
  let k42 = 500.0f32 in -- [s^-1]      
  let k24 = k42/Kd24    -- [uM^-2 s^-1]
  in
  -- CaM buffering (B) parameters
  let k0Boff = 0.0014f32 in -- [s^-1] 
  let k0Bon = k0Boff/0.2f32 in -- [uM^-1 s^-1] kon = koff/Kd
  let k2Boff = k0Boff/100.0f32 in -- [s^-1] 
  let k2Bon = k0Bon in -- [uM^-1 s^-1]
  let k4Boff = k2Boff in -- [s^-1]
  let k4Bon = k0Bon      -- [uM^-1 s^-1]
  in
  -- using thermodynamic constraints
  let k20B = k20/100.0f32 in -- [s^-1] thermo constraint on loop 1
  let k02B = k02 in -- [uM^-2 s^-1] 
  let k42B = k42 in -- [s^-1] thermo constraint on loop 2
  let k24B = k24    -- [uM^-2 s^-1]
  in
  -- Wi Wa Wt Wp
  let kbi = 2.2f32 in -- [s^-1] (Ca4CaM dissocation from Wb)
  let kib = kbi/33.5e-3f32 in -- [uM^-1 s^-1]
  let kpp1 = 1.72f32 in -- [s^-1] (PP1-dep dephosphorylation rates)
  let Kmpp1 = 11.5f32 in -- [uM]
  let kib2 = kib in
  let kb2i = kib2*5.0f32 in
  let kb24 = k24 in
  let kb42 = k42*33.5e-3f32/5.0f32 in
  let kta = kbi/1000.0f32 in -- [s^-1] (Ca4CaM dissociation from Wt)
  let kat = kib in -- [uM^-1 s^-1] (Ca4CaM reassociation with Wa)
  let kt42 = k42*33.5e-6f32/5.0f32 in
  let kt24 = k24 in
  let kat2 = kib in
  let kt2a = kib*5.0f32
  in
  -- CaN parameters
  let kcanCaoff = 1.0f32 in -- [s^-1] 
  let kcanCaon = kcanCaoff/0.5f32 in -- [uM^-1 s^-1] 
  let kcanCaM4on = 46.0f32 in -- [uM^-1 s^-1]
  let kcanCaM4off = 0.0013f32 in -- [s^-1]
  let kcanCaM2on = kcanCaM4on in
  let kcanCaM2off = 2508.0f32*kcanCaM4off in
  let kcanCaM0on = kcanCaM4on in
  let kcanCaM0off = 165.0f32*kcanCaM2off in
  let k02can = k02 in
  let k20can = k20/165.0f32 in
  let k24can = k24 in
  let k42can = k20/2508.0f32 
  in
  -- CaM Reaction fluxes
  let rcn02 = k02*pow(Ca,2.0f32)*CaM - k20*Ca2CaM in
  let rcn24 = k24*pow(Ca,2.0f32)*Ca2CaM - k42*Ca4CaM
  in
  -- CaM buffer fluxes
  let B = Btot - CaMB - Ca2CaMB - Ca4CaMB in
  let rcn02B = k02B*pow(Ca,2.0f32)*CaMB - k20B*Ca2CaMB in
  let rcn24B = k24B*pow(Ca,2.0f32)*Ca2CaMB - k42B*Ca4CaMB in
  let rcn0B = k0Bon*CaM*B - k0Boff*CaMB in
  let rcn2B = k2Bon*Ca2CaM*B - k2Boff*Ca2CaMB in
  let rcn4B = k4Bon*Ca4CaM*B - k4Boff*Ca4CaMB 
  in
  -- CaN reaction fluxes 
  let Ca2CaN = CaNtot - Ca4CaN - CaMCa4CaN - Ca2CaMCa4CaN - Ca4CaMCa4CaN in
  let rcnCa4CaN = kcanCaon*pow(Ca,2.0f32)*Ca2CaN - kcanCaoff*Ca4CaN in
  let rcn02CaN = k02can*pow(Ca,2.0f32)*CaMCa4CaN - k20can*Ca2CaMCa4CaN in
  let rcn24CaN = k24can*pow(Ca,2.0f32)*Ca2CaMCa4CaN - k42can*Ca4CaMCa4CaN in
  let rcn0CaN = kcanCaM0on*CaM*Ca4CaN - kcanCaM0off*CaMCa4CaN in
  let rcn2CaN = kcanCaM2on*Ca2CaM*Ca4CaN - kcanCaM2off*Ca2CaMCa4CaN in
  let rcn4CaN = kcanCaM4on*Ca4CaM*Ca4CaN - kcanCaM4off*Ca4CaMCa4CaN 
  in
  -- CaMKII reaction fluxes
  let Pix = 1.0f32 - Pb2 - Pb - Pt - Pt2 - Pa in
  let rcnCKib2 = kib2*Ca2CaM*Pix - kb2i*Pb2 in
  let rcnCKb2b = kb24*pow(Ca,2.0f32)*Pb2 - kb42*Pb in
  let rcnCKib = kib*Ca4CaM*Pix - kbi*Pb in
  let T = Pb + Pt + Pt2 + Pa in
  let kbt = 0.055f32 * T + 0.0074f32 * pow(T,2.0f32) + 0.015f32*pow(T,3.0f32) in
  let rcnCKbt = kbt*Pb - kpp1*PP1tot*Pt/(Kmpp1+CaMKIItot*Pt) in
  let rcnCKtt2 = kt42*Pt - kt24*pow(Ca,2.0f32)*Pt2 in
  let rcnCKta = kta*Pt - kat*Ca4CaM*Pa in
  let rcnCKt2a = kt2a*Pt2 - kat2*Ca2CaM*Pa in
  let rcnCKt2b2 = kpp1*PP1tot*Pt2/(Kmpp1+CaMKIItot*Pt2) in
  let rcnCKai = kpp1*PP1tot*Pa/(Kmpp1+CaMKIItot*Pa)
  in
  -- CaM equations
  let dCaM = 1e-3f32*(-rcn02 - rcn0B - rcn0CaN) in
  let dCa2CaM = 1e-3f32*(rcn02 - rcn24 - rcn2B - rcn2CaN + CaMKIItot*(-rcnCKib2 + rcnCKt2a) ) in
  let dCa4CaM = 1e-3f32*(rcn24 - rcn4B - rcn4CaN + CaMKIItot*(-rcnCKib+rcnCKta) ) in
  let dCaMB = 1e-3f32*(rcn0B-rcn02B) in
  let dCa2CaMB = 1e-3f32*(rcn02B + rcn2B - rcn24B) in
  let dCa4CaMB = 1e-3f32*(rcn24B + rcn4B) 
  in
  -- CaMKII equations
  let dPb2 = 1e-3f32*(rcnCKib2 - rcnCKb2b + rcnCKt2b2) in -- Pb2
  let dPb = 1e-3f32*(rcnCKib + rcnCKb2b - rcnCKbt) in -- Pb
  let dPt = 1e-3f32*(rcnCKbt-rcnCKta-rcnCKtt2) in -- Pt
  let dPt2 = 1e-3f32*(rcnCKtt2-rcnCKt2a-rcnCKt2b2) in -- Pt2
  let dPa = 1e-3f32*(rcnCKta+rcnCKt2a-rcnCKai)        -- Pa
  in
  -- CaN equations
  let dCa4CaN = 1e-3f32*(rcnCa4CaN - rcn0CaN - rcn2CaN - rcn4CaN) in -- Ca4CaN
  let dCaMCa4CaN = 1e-3f32*(rcn0CaN - rcn02CaN) in           -- CaMCa4CaN
  let dCa2CaMCa4CaN = 1e-3f32*(rcn2CaN+rcn02CaN-rcn24CaN) in -- Ca2CaMCa4CaN
  let dCa4CaMCa4CaN = 1e-3f32*(rcn4CaN+rcn24CaN)             -- Ca4CaMCa4CaN
  in
  -- encode output array
  let finavalu[offset_1] = dCaM in
  let finavalu[offset_2] = dCa2CaM in
  let finavalu[offset_3] = dCa4CaM in
  let finavalu[offset_4] = dCaMB in
  let finavalu[offset_5] = dCa2CaMB in
  let finavalu[offset_6] = dCa4CaMB in
  let finavalu[offset_7] = dPb2 in
  let finavalu[offset_8] = dPb in
  let finavalu[offset_9] = dPt in
  let finavalu[offset_10] = dPt2 in
  let finavalu[offset_11] = dPa in
  let finavalu[offset_12] = dCa4CaN in
  let finavalu[offset_13] = dCaMCa4CaN in
  let finavalu[offset_14] = dCa2CaMCa4CaN in
  let finavalu[offset_15] = dCa4CaMCa4CaN
  in
  -- write to global variables for adjusting Ca buffering in EC coupling model
  let JCa = 1e-3f32*(2.0f32*CaMKIItot*(rcnCKtt2-rcnCKb2b) -
            2.0f32*(rcn02+rcn24+rcn02B+rcn24B+rcnCa4CaN+rcn02CaN+rcn24CaN)) -- [uM/msec]
  in { JCa, finavalu}

----------------------
----- Fin MODULE -----
----------------------

fun *[f32,EQUS] fin(    [f32,EQUS] initvalu, int initvalu_offset_ecc,
                        int initvalu_offset_Dyad, int initvalu_offset_SL,
                        int initvalu_offset_Cyt, [f32,PARS] parameter,
                        *[f32,EQUS] finavalu, f32 JCaDyad, f32 JCaSL, f32 JCaCyt ) =
  let BtotDyad      = parameter[2] in
  let CaMKIItotDyad = parameter[3] 
  in
  -- set variables
  let Vmyo = 2.1454e-11f32 in
  let Vdyad = 1.7790e-14f32 in
  let VSL = 6.6013e-13f32 in
  let kDyadSL = 3.6363e-16f32 in
  let kSLmyo = 8.587e-15f32 in
  let k0Boff = 0.0014f32 in
  let k0Bon = k0Boff/0.2f32 in
  let k2Boff = k0Boff/100.0f32 in
  let k2Bon = k0Bon in
  let k4Boff = k2Boff in
  let k4Bon = k0Bon
  in
  -- ADJUST ECC incorporate Ca buffering from CaM, convert JCaCyt from uM/msec to mM/msec
  let finavalu[initvalu_offset_ecc+35] = finavalu[initvalu_offset_ecc+35] + 1e-3f32*JCaDyad in
  let finavalu[initvalu_offset_ecc+36] = finavalu[initvalu_offset_ecc+36] + 1e-3f32*JCaSL in
  let finavalu[initvalu_offset_ecc+37] = finavalu[initvalu_offset_ecc+37] + 1e-3f32*JCaCyt
  in
  -- incorporate CaM diffusion between compartments
  let CaMtotDyad =    initvalu[initvalu_offset_Dyad+0]
                    + initvalu[initvalu_offset_Dyad+1]
                    + initvalu[initvalu_offset_Dyad+2]
                    + initvalu[initvalu_offset_Dyad+3]
                    + initvalu[initvalu_offset_Dyad+4]
                    + initvalu[initvalu_offset_Dyad+5]
                    + CaMKIItotDyad * (  initvalu[initvalu_offset_Dyad+6]
                                    + initvalu[initvalu_offset_Dyad+7]
                                    + initvalu[initvalu_offset_Dyad+8]
                                    + initvalu[initvalu_offset_Dyad+9])
                    + initvalu[initvalu_offset_Dyad+12]
                    + initvalu[initvalu_offset_Dyad+13]
                    + initvalu[initvalu_offset_Dyad+14]
  in
  let Bdyad = BtotDyad - CaMtotDyad in -- [uM dyad]
  let J_cam_dyadSL = 1e-3f32 * (  k0Boff*initvalu[initvalu_offset_Dyad+0] - 
                        k0Bon*Bdyad*initvalu[initvalu_offset_SL+0]) in -- [uM/msec dyad]
  let J_ca2cam_dyadSL = 1e-3f32 * (  k2Boff*initvalu[initvalu_offset_Dyad+1] - 
                        k2Bon*Bdyad*initvalu[initvalu_offset_SL+1]) in -- [uM/msec dyad]
  let J_ca4cam_dyadSL = 1e-3f32 * (  k2Boff*initvalu[initvalu_offset_Dyad+2] - 
                        k4Bon*Bdyad*initvalu[initvalu_offset_SL+2])    -- [uM/msec dyad]
  in
  let J_cam_SLmyo    = kSLmyo * (  initvalu[initvalu_offset_SL+0] - initvalu[initvalu_offset_Cyt+0]) in -- [umol/msec]
  let J_ca2cam_SLmyo = kSLmyo * (  initvalu[initvalu_offset_SL+1] - initvalu[initvalu_offset_Cyt+1]) in -- [umol/msec]
  let J_ca4cam_SLmyo = kSLmyo * (  initvalu[initvalu_offset_SL+2] - initvalu[initvalu_offset_Cyt+2])    -- [umol/msec]
  in
  -- ADJUST CAM Dyad 
  let finavalu[initvalu_offset_Dyad+0] = finavalu[initvalu_offset_Dyad+0] - J_cam_dyadSL in
  let finavalu[initvalu_offset_Dyad+1] = finavalu[initvalu_offset_Dyad+1] - J_ca2cam_dyadSL in
  let finavalu[initvalu_offset_Dyad+2] = finavalu[initvalu_offset_Dyad+2] - J_ca4cam_dyadSL
  in
  -- ADJUST CAM Sl
  let finavalu[initvalu_offset_SL+0] = finavalu[initvalu_offset_SL+0] + J_cam_dyadSL*Vdyad/VSL - J_cam_SLmyo/VSL in
  let finavalu[initvalu_offset_SL+1] = finavalu[initvalu_offset_SL+1] + J_ca2cam_dyadSL*Vdyad/VSL - J_ca2cam_SLmyo/VSL in
  let finavalu[initvalu_offset_SL+2] = finavalu[initvalu_offset_SL+2] + J_ca4cam_dyadSL*Vdyad/VSL - J_ca4cam_SLmyo/VSL
  in
  -- ADJUST CAM Cyt 
  let finavalu[initvalu_offset_Cyt+0] = finavalu[initvalu_offset_Cyt+0] + J_cam_SLmyo/Vmyo in
  let finavalu[initvalu_offset_Cyt+1] = finavalu[initvalu_offset_Cyt+1] + J_ca2cam_SLmyo/Vmyo in
  let finavalu[initvalu_offset_Cyt+2] = finavalu[initvalu_offset_Cyt+2] + J_ca4cam_SLmyo/Vmyo
  in finavalu

-------------------------
----- Master MODULE -----
-------------------------

fun f32  inf() = 1.0f32 / 0.0f32
fun f32 minf() = 0.0f32 - inf()
fun f32  nan() = inf()  / inf()
fun f32 mnan() = 0.0f32 - nan()

fun bool isnan(f32 x) = pow(x,2.0f32) != (x*x) -- ( x == nan() || x == mnan() )
fun bool isinf(f32 x) = ( x == inf() || x == minf() )

fun *[f32,EQUS] master( f32 timeinst, [f32,EQUS] initvalu, [f32,PARS] parameter ) =
  let finavalu = replicate(EQUS, 0.0f32) in
  -- ecc function
  let initvalu_offset_ecc  = 0 in
  let parameter_offset_ecc = 0 in
  let finavalu = ecc( timeinst, initvalu, initvalu_offset_ecc, parameter, parameter_offset_ecc, finavalu)
  in
  let JCaDyad = 0.0f32 in
  let JCaSL   = 0.0f32 in
  let JCaCyt  = 0.0f32 in
  loop ({JCaDyad, JCaSL, JCaCyt, finavalu}) = for ii < 3 do
        let {initvalu_offset, parameter_offset, ind} = 
            if      (ii == 0) then -- cam function for Dyad
                { 46, 1, 35 } 
            else if (ii == 1) then -- cam function for SL
                { 61, 6, 36 } 
            else -- if (ii == 2) then -- cam function for Cyt
                { 76, 11, 37 }
        in
        let inp_val = initvalu[ind]*1e3f32 in
        let {res_val, finavalu} = cam(  timeinst, initvalu, initvalu_offset, parameter,
                                        parameter_offset, finavalu, inp_val )
        in
        let { JCaDyad, JCaSL, JCaCyt } = 
            if      (ii == 0) then { res_val, JCaSL,   JCaCyt }
            else if (ii == 1) then { JCaDyad, res_val, JCaCyt }
            else                   { JCaDyad, JCaSL,   res_val}
        in  { JCaDyad, JCaSL, JCaCyt, finavalu}
  in
  -- final adjustments
  let finavalu = fin(  initvalu, initvalu_offset_ecc, 46, 61, 76, 
                       parameter, finavalu, JCaDyad, JCaSL, JCaCyt ) 
  in
  map(fn f32 (f32 x) => 
        if ( isnan(x) || isinf(x) ) 
        then 0.0001f32
        else x 
     , finavalu)


----------------------------------------
----- embedded_fehlberg_7_8 MODULE -----
----------------------------------------

fun {*[f32,EQUS], *[f32,EQUS]} 
embedded_fehlberg_7_8(  f32 timeinst, f32 h,
                        [f32,EQUS]  initvalu,
                        [f32,PARS]  parameter ) =
  let c_1_11 = 41.0f32 / 840.0f32 in
  let c6 = 34.0f32 / 105.0f32 in
  let c_7_8= 9.0f32 / 35.0f32 in
  let c_9_10 = 9.0f32 / 280.0f32 in

  let a2 = 2.0f32 / 27.0f32 in
  let a3 = 1.0f32 / 9.0f32 in
  let a4 = 1.0f32 / 6.0f32 in
  let a5 = 5.0f32 / 12.0f32 in
  let a6 = 1.0f32 / 2.0f32 in
  let a7 = 5.0f32 / 6.0f32 in
  let a8 = 1.0f32 / 6.0f32 in
  let a9 = 2.0f32 / 3.0f32 in
  let a10 = 1.0f32 / 3.0f32 in

  let b31 = 1.0f32 / 36.0f32 in
  let b32 = 3.0f32 / 36.0f32 in
  let b41 = 1.0f32 / 24.0f32 in
  let b43 = 3.0f32 / 24.0f32 in
  let b51 = 20.0f32 / 48.0f32 in
  let b53 = -75.0f32 / 48.0f32 in
  let b54 = 75.0f32 / 48.0f32 in
  let b61 = 1.0f32 / 20.0f32 in
  let b64 = 5.0f32 / 20.0f32 in
  let b65 = 4.0f32 / 20.0f32 in
  let b71 = -25.0f32 / 108.0f32 in
  let b74 =  125.0f32 / 108.0f32 in
  let b75 = -260.0f32 / 108.0f32 in
  let b76 =  250.0f32 / 108.0f32 in
  let b81 = 31.0f32/300.0f32 in
  let b85 = 61.0f32/225.0f32 in
  let b86 = -2.0f32/9.0f32 in
  let b87 = 13.0f32/900.0f32 in
  let b91 = 2.0f32 in
  let b94 = -53.0f32/6.0f32 in
  let b95 = 704.0f32 / 45.0f32 in
  let b96 = -107.0f32 / 9.0f32 in
  let b97 = 67.0f32 / 90.0f32 in
  let b98 = 3.0f32 in
  let b10_1 = -91.0f32 / 108.0f32 in
  let b10_4 = 23.0f32 / 108.0f32 in
  let b10_5 = -976.0f32 / 135.0f32 in
  let b10_6 = 311.0f32 / 54.0f32 in
  let b10_7 = -19.0f32 / 60.0f32 in
  let b10_8 = 17.0f32 / 6.0f32 in
  let b10_9 = -1.0f32 / 12.0f32 in
  let b11_1 = 2383.0f32 / 4100.0f32 in
  let b11_4 = -341.0f32 / 164.0f32 in
  let b11_5 = 4496.0f32 / 1025.0f32 in
  let b11_6 = -301.0f32 / 82.0f32 in
  let b11_7 = 2133.0f32 / 4100.0f32 in
  let b11_8 = 45.0f32 / 82.0f32 in
  let b11_9 = 45.0f32 / 164.0f32 in
  let b11_10 = 18.0f32 / 41.0f32 in
  let b12_1 = 3.0f32 / 205.0f32 in
  let b12_6 = - 6.0f32 / 41.0f32 in
  let b12_7 = - 3.0f32 / 205.0f32 in
  let b12_8 = - 3.0f32 / 41.0f32 in
  let b12_9 = 3.0f32 / 41.0f32 in
  let b12_10 = 6.0f32 / 41.0f32 in
  let b13_1 = -1777.0f32 / 4100.0f32 in
  let b13_4 = -341.0f32 / 164.0f32 in
  let b13_5 = 4496.0f32 / 1025.0f32 in
  let b13_6 = -289.0f32 / 82.0f32 in
  let b13_7 = 2193.0f32 / 4100.0f32 in
  let b13_8 = 51.0f32 / 82.0f32 in
  let b13_9 = 33.0f32 / 164.0f32 in
  let b13_10 = 12.0f32 / 41.0f32 in

  let err_factor  = -41.0f32 / 840.0f32 in
  let h2_7 = a2 * h in
  
  -- initvalu_temp[EQUATIONS]
  -- finavalu_temp[13, EQUATIONS]
  let finavalu_temp = replicate(13, replicate(EQUS, 0.0f32)) in

  loop (finavalu_temp) = for ii < 13 do
    let { timeinst_temp, initvalu_temp } = 
        if      (ii == 0) then 
            { timeinst, initvalu }
        else if (ii == 1) then
            let timeinst_temp = timeinst + h2_7 in
            let initvalu_temp = map( fn f32 ({f32,f32} xy) => let {x,y} = xy in x + h2_7 * y
                                    , zip(initvalu,finavalu_temp[0]) ) in
             { timeinst_temp, initvalu_temp }
        else if (ii == 2) then
            let timeinst_temp = timeinst + a3*h in
            let initvalu_temp = map( fn f32 ({f32,f32,f32} xy) => 
                                        let {x,y1,y2} = xy in 
                                        x + h * ( b31*y1 + b32*y2 )
                                   , zip(initvalu,finavalu_temp[0],finavalu_temp[1]) ) in
            { timeinst_temp, initvalu_temp }
        else if (ii == 3) then
            let timeinst_temp = timeinst + a4*h in
            let initvalu_temp = map( fn f32 ({f32,f32,f32} xy) => 
                                        let {x,y1,y2} = xy in 
                                        x + h * ( b41*y1 + b43*y2 )
                                   , zip(initvalu,finavalu_temp[0],finavalu_temp[2]) ) in
            { timeinst_temp, initvalu_temp }
        else if (ii == 4) then
            let timeinst_temp = timeinst + a5*h in
            let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b51*finavalu_temp[0,i] + 
                                        b53*finavalu_temp[2,i] + b54*finavalu_temp[3,i])
                                   , iota(EQUS) ) in
            { timeinst_temp, initvalu_temp }
        else if (ii == 5) then
            let timeinst_temp = timeinst + a6*h in
            let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b61*finavalu_temp[0,i] + 
                                        b64*finavalu_temp[3,i] + b65*finavalu_temp[4,i] )
                                   , iota(EQUS) ) in
            { timeinst_temp, initvalu_temp }
        else if (ii == 6) then
              let timeinst_temp = timeinst + a7 * h in
              let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b71*finavalu_temp[0,i] + 
                                        b74*finavalu_temp[3,i] + b75*finavalu_temp[4,i] + 
                                        b76*finavalu_temp[5,i] )
                                     , iota(EQUS) ) in
              { timeinst_temp, initvalu_temp }
        else if (ii == 7) then
              let timeinst_temp = timeinst + a8 * h in
              let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b81*finavalu_temp[0,i] + 
                                        b85*finavalu_temp[4,i] + b86*finavalu_temp[5,i] + 
                                        b87*finavalu_temp[6,i] )
                                     , iota(EQUS) ) in
              { timeinst_temp, initvalu_temp }
        else if (ii == 8) then
            let timeinst_temp = timeinst + a9*h in
            let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b91*finavalu_temp[0,i] + 
                                        b94*finavalu_temp[3,i] + b95*finavalu_temp[4,i] + 
                                        b96*finavalu_temp[5,i] + b97*finavalu_temp[6,i] + 
                                        b98*finavalu_temp[7,i] )
                                   , iota(EQUS) ) in
            { timeinst_temp, initvalu_temp }
        else if (ii == 9) then
            let timeinst_temp = timeinst + a10*h in
            let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b10_1*finavalu_temp[0,i] + 
                                        b10_4*finavalu_temp[3,i] + b10_5*finavalu_temp[4,i] + 
                                        b10_6*finavalu_temp[5,i] + b10_7*finavalu_temp[6,i] + 
                                        b10_8*finavalu_temp[7,i] + b10_9*finavalu_temp[8,i] )
                                   , iota(EQUS) ) in
            { timeinst_temp, initvalu_temp }
        else if (ii == 10) then
            let timeinst_temp = timeinst + h in
            let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b11_1*finavalu_temp[0,i] + 
                                        b11_4*finavalu_temp[3,i] + b11_5*finavalu_temp[4,i] + 
                                        b11_6*finavalu_temp[5,i] + b11_7*finavalu_temp[6,i] + 
                                        b11_8*finavalu_temp[7,i] + b11_9*finavalu_temp[8,i] + 
                                        b11_10 * finavalu_temp[9,i])
                                   , iota(EQUS) ) in
            { timeinst_temp, initvalu_temp }
        else if (ii == 11) then
            let timeinst_temp = timeinst in
            let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b12_1*finavalu_temp[0,i] + 
                                        b12_6*finavalu_temp[5,i] + b12_7*finavalu_temp[6,i] + 
                                        b12_8*finavalu_temp[7,i] + b12_9*finavalu_temp[8,i] + 
                                        b12_10 * finavalu_temp[9,i] )
                                   , iota(EQUS) ) in
            { timeinst_temp, initvalu_temp }
        else -- if (ii == 12) then
            let timeinst_temp = timeinst + h in
            let initvalu_temp = map( fn f32 (int i) => 
                                        initvalu[i] + h * ( b13_1*finavalu_temp[0,i] + 
                                        b13_4*finavalu_temp[3,i] + b13_5*finavalu_temp[4,i] + 
                                        b13_6*finavalu_temp[5,i] + b13_7*finavalu_temp[6,i] + 
                                        b13_8*finavalu_temp[7,i] + b13_9*finavalu_temp[8,i] + 
                                        b13_10*finavalu_temp[9,i] + finavalu_temp[11,i] )
                                   , iota(EQUS) ) in
            { timeinst_temp, initvalu_temp }
    in
    let finavalu_temp[ii] = master( timeinst_temp, initvalu_temp, parameter ) in
    finavalu_temp
  --------------
  -- end loop --
  --------------
  in
--  { finavalu_temp[5], replicate(EQUS, 0.0f32) } 
  let finavalu = map( fn f32 (int i) =>
                        initvalu[i] +  h * (c_1_11 * (finavalu_temp[0,i] + finavalu_temp[10,i]) +
                            c6 * finavalu_temp[5,i] + c_7_8 * (finavalu_temp[6,i] + finavalu_temp[7,i]) +
                            c_9_10 * (finavalu_temp[8,i] + finavalu_temp[9,i]) )
                    , iota(EQUS) )
  in 
  let error = map( fn f32 (int i) =>
                        fabs(err_factor * (finavalu_temp[0,i] + finavalu_temp[10,i] - finavalu_temp[11,i] - finavalu_temp[12,i]))
                 , iota(EQUS) )
  in { finavalu, error }


-------------------------
----- SOLVER MODULE -----
-------------------------

fun f32 MIN_SCALE_FACTOR() = 0.125f32
fun f32 MAX_SCALE_FACTOR() = 4.0f32
fun int ATTEMPTS()         = 12

fun f32 max(f32 x, f32 y) = if ( x < y ) then y else x
fun f32 min(f32 x, f32 y) = if ( x < y ) then x else y

fun {bool,[f32,EQUS]} 
solver(int xmax, [f32,PARS] params, [f32,EQUS] y0) =
  let err_exponent  = 1.0f32 / 7.0f32 in
  let last_interval = 0.0f32 in
  let h_init = 1.0f32 in
  let h = h_init in
  let xmin = 0 in
  let tolerance = 10.0f32 / f32(xmax-xmin) in
  let y_km1  = y0 in -- copy(y0)

  if (xmax < xmin || h <= 0.0f32) then { False, y_km1 }
  else if ( xmax == xmin ) then { True, y_km1 }
  else 
  let {h, last_interval} = if (h > f32(xmax - xmin) )
                           then { f32(xmax - xmin), 1 }
                           else { h, last_interval }
  in
  -- initialize return and loop-variant params
  let failed = False in
  let km1    = 0     in
  loop({km1, failed, y_km1}) = while ( (!failed) && (km1 < xmax) ) do -- for km1 < xmax do
    -- reinitialize variables
    let h          = h_init in
    let scale_fina = 1.0f32 in
    let y_k = replicate(EQUS, 0.0f32) in
    
    -- make attempts to minimize error
    let breakLoop  = False  in
    let j = 0 in
    loop({j,h,y_k,breakLoop,scale_fina}) = while ( (!breakLoop) && (j < ATTEMPTS()) ) do
      -- REINITIALIZE ALL VARIABLES
      let error   = 0 in
      let outside = 0 in
      let scale_min = MAX_SCALE_FACTOR() in

      -- EVALUATE ALL EQUATIONS
      let {y_k, err} = reshape( (EQUS), embedded_fehlberg_7_8( f32(km1), h, y_km1, params) ) in
      
      -- IF THERE WAS NO ERROR FOR ANY OF EQUATIONS, SET SCALE AND LEAVE THE LOOP
      let errs = map( fn bool (f32 e) => if e > 0.0f32 then True else False, err ) in
      let error= reduce(||, False, errs) in

--      let {breakLoop, scale_fina} = if(!error)
--                                    then {True,  MAX_SCALE_FACTOR()}
--                                    else {False, scale_fina}

      if (!error)
      then {j+1, h, y_k, True, MAX_SCALE_FACTOR()}
      else 
      -- FIGURE OUT SCALE AS THE MINIMUM OF COMPONENT SCALES
      let yy = map( fn f32 (f32 x) =>
                        if (x == 0.0f32) then tolerance else fabs(x)
                  , y_km1 )
      in
      let scale = map ( fn f32 ({f32,f32} yy_erri) =>
                            let {yyi, erri} = yy_erri in
                            0.8f32 * pow( tolerance * yyi / erri , err_exponent )
                      , zip(yy,err) )
      in
      let scale_min = reduce(min, scale_min, scale) in
      let scale_fina = min( max(scale_min,MIN_SCALE_FACTOR()), MAX_SCALE_FACTOR())
      in
      -- IF WITHIN TOLERANCE, FINISH ATTEMPTS...
      let tmps =map ( fn bool ({f32,f32} err_yyi) =>
                        let {erri, yyi} = err_yyi in
                        erri <= ( tolerance * yyi )
                    , zip(err, yy) ) in
      let breakLoop = reduce (&&, True, tmps) in

      -- ...OTHERWISE, ADJUST STEP FOR NEXT ATTEMPT
      -- scale next step in a default way
      let h = h * scale_fina in

      -- limit step to 0.9, because when it gets close to 1, it no longer
      -- makes sense, as 1 is already the next time instance
      -- (added to original algorithm)
      let h = if (h >= 0.9f32) then 0.9f32 else h
      in
      -- if instance+step exceeds range limit, limit to that range
      let h = if ( f32(km1) + h > f32(xmax) ) then f32(xmax - km1)
              else if ( f32(km1) + h + 0.5f32 * h > f32(xmax) )
                   then 0.5f32 * h else h
      in {j+1, h, y_k, breakLoop, scale_fina}
    in { km1+1, !breakLoop, y_k }
  in {!failed,y_km1}

-----------------------
----- MAIN MODULE -----
-----------------------

fun int EQUATIONS () = 91
fun int PARAMETERS() = 16

fun {bool, [[f32,91],workload]}
main(int repeat, f32 eps, int workload, int xmax, [f32,91] y0, [f32,16] params) =
  let {oks, y_res} = unzip (
    map ( fn {bool,[f32,91]} (int i) =>
            let add_fact = f32(i % repeat)*eps in
            let y_row = map(+add_fact, y0) in
            solver(xmax, params, y_row)
        , iota(workload) ) )
  in
  { reduce(&&, True, oks), y_res }



fun {[f32,91], [f32,91]}
main_EMBEDDED(int xmax, [[f32,91],workload] y, [[f32,16],workload] params) =
    embedded_fehlberg_7_8( 0.0f32, 1.0f32, y[0], params[0])

fun {f32, [f32,91]}
main_ECC_CAM(int xmax, [[f32,91],workload] y, [[f32,16],workload] params) =
    let finavalu = replicate(91, 0.0f32) in
    let finavalu = ecc( 0.0f32, y[0], 0, params[0], 0, finavalu ) in
    let {res_val, finavalu} = cam(  0.0f32, y[0], 46, params[0],  
                                    1, finavalu, y[0, 35]*1e3f32 )
    in {res_val, finavalu}

fun [f32,91]
main_MASTER(int xmax, [[f32,91],workload] y, [[f32,16],workload] params) =
    master( 0.0f32, y[0], params[0] ) 
