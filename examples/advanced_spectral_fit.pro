; NASA Ames PAH IR Spectroscopic Database
;
; This is an example of fitting an astronomical spectrum built around
; the functionality provided by the Suite and should help confirm that
; the AmesPAHdbIDLSuite has been properly installed.
; 
; Additional information can be found at
; http://www.astrochem.org/pahdb, in Bauschlicher et al. 2010, The
; Astrophysical Journal Supplement Series, 189, 341 and in Boersma et
; al. 2014, The Astrophysical Journal Supplement Series, 211, 8.
;
; USAGE
;   advanced_spectral_fit
;
FUNCTION FUNC,p

  ; common block
  COMMON _func,pahdb,uids,nuids,ncarbon,observation,options,fit,nfit
  
  ; translate and check parameters
  i = 0

  IF options.model GT 0 THEN BEGIN

     IF options.input LE 0 THEN BEGIN

        input = p[i++]

        IF input LE 0 THEN RETURN,(MACHAR(/DOUBLE)).xmax

     ENDIF ELSE input = options.input
  
  ENDIF ELSE input = 0.0D

  IF options.width LE 0 THEN BEGIN

     width = p[i++]

     IF width LE 0 THEN RETURN,(MACHAR(/DOUBLE)).xmax

  ENDIF ELSE width = options.width

  IF options.redshift GT 0 THEN BEGIN

     redshift = p[i++]

     IF redshift GT 0 THEN RETURN,(MACHAR(/DOUBLE)).xmax

  ENDIF ELSE redshift = options.redshift

  IF options.balance LT 0 AND options.Tgas LT 0 THEN BEGIN

     balance = p[i++]

     IF balance LT 0 THEN RETURN,(MACHAR(/DOUBLE)).xmax

  ENDIF ELSE balance = options.balance

  IF options.Tgas LT 0 THEN BEGIN

     Tgas = p[i++]

     IF Tgas LT 0 THEN RETURN,(MACHAR(/DOUBLE)).xmax

  ENDIF ELSE Tgas = options.Tgas

  ; increment nfit
  nfit++

  ; retrieve the transitions from the database
  IF options.balance NE 0 AND options.Tgas NE 0 THEN transitions = pahdb->GetTransitionsByUID(REFORM(uids, nuids * 3)) $
  ELSE transitions = pahdb->GetTransitionsByUID(uids)

  ; turn into emission spectrum
  CASE options.model OF

     0 : BREAK

     1 : transitions->FixedTemperature,input

     2 : transitions->CalculatedTemperature,input*1.6021765D-12,Star=options.star

     3 : transitions->Cascade,input*1.6021765D-12,Star=options.star

  ENDCASE

  IF redshift LT 0 THEN transitions->Shift,redshift

  IF options.balance NE 0 AND options.Tgas NE 0 THEN BEGIN

    ; intersect
    transitions->Intersect,uids[0,*]
     
    ; retrieve data in struct
    t = transitions->Get()

    ; use a working copy
    c = t

    ; merge the charges and assign pseudo identifiers
    FOR i = 0, nuids - 1 DO BEGIN

       sel0 = WHERE(t.data.uid EQ uids[0, i], nsel0)

       sel1 = WHERE(t.data.uid EQ uids[1, i], nsel1)

       sel2 = WHERE(t.data.uid EQ uids[2, i], nsel2)

       c.data[[sel0, sel1, sel2]].uid = i + 1

       c.data[sel0].intensity *= 1.6D2 / SQRT(ncarbon[0, i]) / balance

       c.data[sel2].intensity *= 5.11D-6 * SQRT(ncarbon[2, i]) * SQRT(Tgas) * balance

    ENDFOR

    ; set data
    transitions->Set,Data=c.data,Uids=LINDGEN(nuids) + 1

  ENDIF

   ; create spectrum
  spectrum = transitions->Convolve(Grid=observation->getGrid(), Gaussian=options.profile, FWHM=width)

  ; fit the spectrum
  fit = spectrum->Fit(observation)

  ; clean up after ourselves
  OBJ_DESTROY,[spectrum, transitions]

  ; chi-squared
  chisq = fit->getChiSquared()

  ; norm
  norm = fit->getNorm()
  
  ; print parameters
  PRINT
  PRINT,FORMAT='(89("="),"'+STRING(10B)+'",A4,4X,A8,4X,A8,4X,A8,4X,A8,4X,A8,4X,A8,4X,A8)',"run","redshift","FWHM","gamma","Tgas","T/energy","norm","chi-sq"
  PRINT,FORMAT='(I4,4X,g8.3,4X,g8.3,4X,g8.3,4X,g8.3,4X,g8.3,4X,g8.3,4X,g8.3)',nfit,redshift,width,balance,Tgas,input,norm,chisq
  PRINT,FORMAT='(89("="))'
  PRINT
 

  ; return
  IF options.minimize EQ 0 THEN RETURN,norm

  RETURN,chisq
END

PRO ADVANCED_SPECTRAL_FIT,OPTS

  ; the Spitzer IRS/SL 10 - 15 micron spectrum of NGC7023 
  file = 'ngc7023.dat'

  ; create a common block and set options :
  ; size:
  ;   > 0 fixed
  ; model:
  ;   0 - none
  ;   1 - FixedTemperature
  ;   2 - CalculatedTemperature
  ;   3 - Cascade
  ; input:
  ;  >0 - fixed
  ; <=0 - optimize
  ; star:
  ;    0 - no 
  ;    1 - yes
  ; profile:
  ;   0 - Lorentzian
  ;   1 - Gaussian
  ; width:
  ; >=0 - fixed
  ;  <0 - optimize
  ; redshift:
  ; <=0 - fixed
  ;  >0 - optimize
  ; balance (see Carelli et al. 2012 on electron attachement rates) :
  ;   0 - disable
  ;  >0 - fixed
  ;  <0 - optimize
  ; Tgas:
  ;   0 - disable
  ;  >0 - fixed
  ;  <0 - optimize
  ; minimize:
  ;  0 - norm
  ;  1 - chi-squared
  ; query:
  ;  string - search query
  COMMON _func,pahdb,uids,nuids,ncarbon,observation,options,fit,nfit
  
  IF N_PARAMS() EQ 0 THEN options = {model:0, $
                                     input:0D, $
                                     star:0, $
                                     profile:1, $
                                     width:16D, $
                                     redshift:15D, $
                                     balance:0, $
                                     Tgas:0, $
                                     minimize:0, $
                                     query:"o=0 mg=0 fe=0 si=0 chx=0 ch2=0 c>=25"} $
  ELSE options = OPTS

  nfit = 0

  ; read observations into AmesPAHdbIDLSuite_Observation
  observation = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                        file, $
                        Units=AmesPAHdbIDLSuite_CREATE_OBSERVATION_UNITS_S(AUNIT=3, OUNIT=1))

  ; turn wavelength into frequency
  observation->AbscissaUnitsTo,1
  
  ; hide overflow/underflow messages expected with Planck's function
  ; and Gaussian emission profiles
  IF options.model GT 1 OR options.profile EQ 1 THEN BEGIN

     except = !EXCEPT

     !EXCEPT = 0

  ENDIF
  
  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use the
  ; keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  IF options.balance NE 0 AND options.Tgas NE 0 THEN BEGIN

     ; get complete set of anion, neutral and cation species
     uids = pahdb->getUIDsCompleteChargeSet(-1, nuids)

     ; get species and fill ncarbon 
     species = pahdb->getSpeciesByUID(REFORM(uids, nuids * 3))

     ncarbon = REFORM((species->get()).data.nc, 3, nuids)

     ; clean up species
     OBJ_DESTROY,species

  ENDIF ELSE uids = pahdb->Search(options.query, nuids)
  
  ; optimize or not?
  IF (options.model GT 0 AND options.input LE 0) OR $
     options.width LE 0 OR $
     options.redshift GT 0 OR $
     options.balance LT 0 OR $
     options.Tgas LT 0 THEN BEGIN

     ; initial guesses and scale
     p0 = 0D

     scale = 0D

     IF options.model EQ 1 THEN BEGIN

        p0 = [p0, 850D] 

        scale = [scale, 750D]
        
     ENDIF ELSE IF options.model GT 1 THEN BEGIN

        p0 = [p0, 5.5D]
     
        scale = [scale, 4.5D]

     ENDIF

     IF options.width LE 0 THEN BEGIN

        p0 = [p0, 16D]
     
        scale = [scale, 15D]

     ENDIF

     IF options.redshift GT 0 THEN BEGIN

        p0 = [p0, -15D]
     
        scale = [scale, -30D]

     ENDIF

     IF options.balance LT 0 THEN BEGIN

        p0 = [p0, 1.3D2]
        
        scale = [scale, 1.3D3]

     ENDIF
 
     IF options.Tgas LT 0 THEN BEGIN

        p0 = [p0, 600D]
        
        scale = [scale, 4D2]

     ENDIF

     p0 = p0[1:*]

     scale = scale[1:*]

     ; minimize parameters
     param = AMOEBA(1D-5, P0=p0, SCALE=scale)

     IF N_ELEMENTS(param) EQ 1 THEN BEGIN

        IF param EQ -1 THEN BEGIN
        
           MESSAGE,"FAILED TO CONVERGE IN "+STRTRIM(STRING(nfit),2)+" ITERATIONS",/INFORMATIONAL
           
           GOTO,FINISH

        ENDIF

     ENDIF

  ENDIF ELSE param = [options.input, $
                      options.width, $
                      options.redshift, $
                      options.balance, $
                      options.Tgas]

  ; a single run to fill the fit object using the fixed and/or
  ; optimized parameters
  void = FUNC(param)

  ; translate parameters
  i = 0

  IF options.model GT 0 THEN BEGIN

     IF options.input LE 0 THEN BEGIN

        input = param[i++]

     ENDIF ELSE input = options.input
  
  ENDIF ELSE input = 0.0D

  IF options.width LE 0 THEN BEGIN

     width = param[i++]

  ENDIF ELSE width = options.width

  IF options.redshift GT 0 THEN BEGIN

     redshift = param[i++]

  ENDIF ELSE redshift = options.redshift

  IF options.balance LT 0 THEN BEGIN

     balance = param[i++]

  ENDIF ELSE balance = options.balance

  IF options.Tgas LT 0 THEN BEGIN

     Tgas = param[i++]

  ENDIF ELSE Tgas = options.Tgas

  IF options.balance NE 0 AND options.Tgas NE 0 THEN BEGIN

     ; store norm of the fit
     norm = fit->getNorm()
     
     ; store the weights of the fitted combined spectra
     f_weights = fit->getWeights()

     ; store the identifiers of the fitted combined species
     f_uids = fit->getUIDS(nf_uids) - 1

     nn_uids = 3 * nf_uids
   
     ; store the identifiers of the individual species
     n_uids = REFORM(uids[*, f_uids], nn_uids)

    ; new weights
     n_weights = REPLICATE({AmesPAHdbIDLSuite_Weights_S, $
                            uid:0L, $
                            weight:0D}, nn_uids)

     ; get transitions of found species
     transitions = pahdb->getTransitionsByUID(n_uids)

     ; turn into emission spectrum
     CASE options.model OF

        0 : break

        1 : transitions->FixedTemperature,input

        2 : transitions->CalculatedTemperature,input*1.6021765D-12,Star=options.star

        3 : transitions->Cascade,input*1.6021765D-12,Star=options.star

     ENDCASE

     ; shift the transitions
     IF redshift LT 0 THEN transitions->Shift,redshift

     ; convolve into spectra
     spectrum = transitions->Convolve(FWHM=width, Gaussian=options.profile, Grid=observation->getGrid())

     ; get spectra in struct
     s = spectrum->get()

     ; clean up
     OBJ_DESTROY,[spectrum]
     
     ; use a working copy
     c = s

     ; assign correct weights using the charge balance
     FOR i = 0, nf_uids - 1 DO BEGIN
      
        n_weights[3*i].weight = f_weights[i].weight * 1.6D2 / SQRT(ncarbon[0, f_uids[i]]) / balance

        n_weights[3*i].uid = uids[0, f_uids[i]]
        
        n_weights[3*i+1].weight = f_weights[i].weight

        n_weights[3*i+1].uid = uids[1, f_uids[i]]
        
        n_weights[3*i+2].weight = f_weights[i].weight * 5.11D-6 * SQRT(ncarbon[2, f_uids[i]]) * SQRT(Tgas) * balance

        n_weights[3*i+2].uid = uids[2, f_uids[i]]

        
        sel0 = WHERE(s.data.uid EQ uids[0, f_uids[i]])

        sel1 = WHERE(s.data.uid EQ uids[1, f_uids[i]])

        sel2 = WHERE(s.data.uid EQ uids[2, f_uids[i]])

     
        c[sel0].data.intensity = s[sel0].data.intensity * n_weights[3*i].weight

        c[sel1].data.intensity = s[sel1].data.intensity * n_weights[3*i+1].weight
       
        c[sel2].data.intensity = s[sel2].data.intensity * n_weights[3*i+2].weight
         
     ENDFOR

     fit->Set,Data=c.data,Uids=n_uids,Weights=n_weights

  ENDIF

  ; restore underflow/overflow reporting
  IF options.model GT 1 OR options.profile EQ 1 THEN !EXCEPT = except

  ; plot the fit
  fit->Plot
  
  key = ''
  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Residual 

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Charge

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Size

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Composition

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; predict 2 - 20 um spectrum

  xrange = 1D4 / [20D, 3D]

  IF NOT OBJ_VALID(transitions) THEN transitions = pahdb->getTransitionsByUID(uids)
  
  spectrum = transitions->Convolve(FWHM=width, Gaussian=options.profile, XRange=xrange)

  coadded = spectrum->Coadd(Weights=n_weights)

  coadded->Plot,/Wavelength

  ; clean up the objects
  OBJ_DESTROY,[coadded, spectrum, transitions, fit, observation, pahdb]

  FINISH:

END
