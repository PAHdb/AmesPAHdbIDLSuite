;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Main interface to work with the NASA Ames PAH IR Spectroscopic
;    Database's XML file
;
; CATEGORY:
;    INTERFACE
;
; SUPERCLASSES:
;   NONE  
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;    AMESPAHDBIDLSUITE_XMLPARSER
;    AMESPAHDBIDLSUITE_SPECIES
;    AMESPAHDBIDLSUITE_TRANSITIONS
;    AMESPAHDBIDLSUITE_LABORATORY
;    AMESPAHDBIDLSUITE_GEOMETRY
;
; OPTIONAL:
;   NONE
;
; CREATION:
;    db = OBJ_NEW('AmesPAHdbIDLSuite')
;
; METHODS
;    PUBLIC:
;      SEARCH (FUNCTION)
;      CHECKVERSION (FUNCTION)
;      GETVERSION (FUNCTION)
;      GETSPECIESBYUID (FUNCTION)
;      GETTRANSITIONSBYUID (FUNCTION)
;      GETLABORATORYBYUID (FUNCTION)
;      GETGEOMETRYBYUID (FUNCTION)
;      GETUIDSCOMPLETECHARGESET (FUNCTION)
;      POINTER (FUNCTION)
;
;    PRIVATE:
;      FILE_MD5 (PROCEDURE)
;      READFILE (PROCEDURE)
;      GETTAGBYUID (FUNCTION)
;      TOKENIZEWORDS (FUNCTION)
;      PARSETOKENS (FUNCTION)
;      
; EXAMPLE
;    dbi = OBJ_NEW('AmesPAHdbIDLSuite', Filename='experimental.xml')
;    pahs = db->GetTransitionsByUID(100)
;    pahs->Plot
;    OBJ_DESTROY,[pahs, dbi]
;
; MODIFICATION HISTORY
;
;   08-26-2015
;   Added File_MD5 procedure and updated READFILE and SEARCH
;   procedures. Updated Init to read VERSION-file from current
;   distribution. Christiaan Boersma
;   04-21-2015
;   Moved stop condition to beginning of SEARCH-method. Removed
;   duplicate charge tokens from TOKENIZEWORDS. Christiaan Boersma
;   04-07-2015
;   Ensured UIDs in GetXXXXByUID are arrays, even when when called
;   with scalar. Christiaan Boersma.
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

;; FILE_MD5

PRO AmesPAHdbIDLSuite::File_MD5,File,Hash,Err

  COMPILE_OPT IDL2

  ON_ERROR,2

  md5 = FILE_WHICH(getenv('PATH'), 'md5')
     
  IF MD5 NE '' THEN BEGIN
  
     SPAWN,[md5,'-q', File],Hash,Err,/NOSHELL

     RETURN
  ENDIF

  md5sum = FILE_WHICH(getenv('PATH'), 'md5sum')

  IF md5sum NE '' THEN BEGIN
  
     SPAWN,[md5sum, File],Hash,Err,/NOSHELL

     Hash = (STRSPLIT(Hash, /EXTRACT))[0]
     
     RETURN
  ENDIF
  
  Err = "       UNABLE TO CALCULATE MD5: COMMAND NOT FOUND        "

  PRINT
  PRINT,"========================================================="
  PRINT,Err
  PRINT,"========================================================="
  PRINT
END

;; GETUIDSCOMPLETECHARGESET

FUNCTION AmesPAHdbIDLSuite::GetUIDsCompleteChargeSet,UIDs,Count,Two=Two

  COMPILE_OPT IDL2

  ON_ERROR,2

  pahs = self->GetTagByUID('species', UIDs)

  tags = STRLOWCASE(TAG_NAMES(pahs))

  tags = tags[WHERE(tags NE 'uid' AND tags NE 'charge' AND tags NE 'energy' AND tags NE 'scale' AND tags NE 'formula' AND tags NE 'symm' AND tags NE 'method' AND tags NE 'zeropoint' AND tags NE 'formula')]

  anions = WHERE(pahs.charge EQ -1, nanions)

  cations = WHERE(pahs.charge EQ 1, ncations)

  neutrals = WHERE(pahs.charge EQ 0)

  IF NOT KEYWORD_SET(Two) THEN BEGIN

     set = [0L, 0L, 0L]

     FOR i = 0, nanions - 1 DO BEGIN
        
        cmd = "sel1 = WHERE(" + STRJOIN("pahs[cations]." + tags + " EQ pahs[anions[i]]." + tags, " AND ") + ", ncation)"
        
        ret = EXECUTE(cmd)

        IF ncation GT 1 THEN BEGIN
           
           geometries = self->getGeometryByUID([pahs[anions[i]].uid, pahs[cations[sel1]].uid])
           
           geometries->Diagonalize
           
           geo = geometries->get()
           
           OBJ_DESTROY,geometries
           
           norms = DBLARR(ncation)
           
           sel3 = WHERE(geo.data.uid EQ pahs[anions[i]].uid)
           
           x = geo.data[sel3].x & y = geo.data[sel3].y & z = geo.data[sel3].z
           
           FOR j = 0, ncation - 1 DO BEGIN
              
              sel3 = WHERE(geo.data.uid EQ pahs[cations[sel1[j]]].uid)
              
              norms[j] = NORM((x - geo.data[sel3].x)^2 + (y - geo.data[sel3].y)^2 + (z - geo.data[sel3].z)^2)

           ENDFOR
           
           min = MIN(norms, imin)
           
           sel1 = sel1[imin] & ncation = 1
           
        ENDIF
        
        IF ncation EQ 1 THEN BEGIN
           
           ret = EXECUTE("sel2 = WHERE(" + STRJOIN("pahs[neutrals]." + tags + " EQ pahs[anions[i]]." + tags, " AND ") + ", nneutral)")
           
           IF nneutral GT 1 THEN BEGIN
              
              geometries = self->getGeometryByUID([pahs[anions[i]].uid, pahs[neutrals[sel2]].uid])
              
              geometries->Diagonalize
              
              geo = geometries->get()
              
              OBJ_DESTROY,geometries
              
              norms = DBLARR(nneutral)
              
              sel3 = WHERE(geo.data.uid EQ pahs[anions[i]].uid)
              
              x = geo.data[sel3].x & y = geo.data[sel3].y & z = geo.data[sel3].z
              
              FOR j = 0, nneutral - 1 DO BEGIN
                 
                 sel3 = WHERE(geo.data.uid EQ pahs[neutrals[sel2[j]]].uid)
                 
                 norms[j] = NORM((x - geo.data[sel3].x)^2 + (y - geo.data[sel3].y)^2 + (z - geo.data[sel3].z)^2)
                 
              ENDFOR
              
              min = MIN(norms, imin)
              
              sel2 = sel2[imin] & nneutral = 1
              
           ENDIF
           
           IF nneutral EQ 1 THEN BEGIN
              
              set = [[set], [pahs[anions[i]].uid, pahs[neutrals[sel2]].uid, pahs[cations[sel1]].uid]]
              
              keep = WHERE(cations NE cations[sel1[0]], nkeep)
              
              IF nkeep GT 0 THEN cations = cations[keep]
              
              keep = WHERE(neutrals NE neutrals[sel2[0]], nkeep)
              
              IF nkeep GT 0 THEN neutrals = neutrals[keep]
              
           ENDIF
           
        ENDIF
        
     ENDFOR
     
  ENDIF ELSE BEGIN
     
     set = [0L, 0L]

     FOR i = 0, ncations - 1 DO BEGIN
        
        cmd = "sel1 = WHERE(" + STRJOIN("pahs[neutrals]." + tags + " EQ pahs[cations[i]]." + tags, " AND ") + ", nneutral)"
        
        ret = EXECUTE(cmd)
        
        IF nneutral GT 1 THEN BEGIN
           
           geometries = self->getGeometryByUID([pahs[cations[i]].uid, pahs[neutrals[sel1]].uid])
           
           geometries->Diagonalize
           
           geo = geometries->get()
           
           OBJ_DESTROY,geometries
           
           norms = DBLARR(nneutral)
           
           sel2 = WHERE(geo.data.uid EQ pahs[cations[i]].uid)
           
           x = geo.data[sel2].x & y = geo.data[sel2].y & z = geo.data[sel2].z
           
           FOR j = 0, nneutral - 1 DO BEGIN
              
              sel3 = WHERE(geo.data.uid EQ pahs[neutrals[sel1[j]]].uid)
              
              norms[j] = NORM((x - geo.data[sel3].x)^2 + (y - geo.data[sel3].y)^2 + (z - geo.data[sel3].z)^2)
              
           ENDFOR
           
           min = MIN(norms, imin)
           
           sel1 = sel1[imin] & nneutral = 1
           
        ENDIF

        IF nneutral EQ 1 THEN BEGIN
              
           set = [[set], [pahs[neutrals[sel1]].uid, pahs[cations[i]].uid]]
              
           keep = WHERE(neutrals NE neutrals[sel1[0]], nkeep)

           IF nkeep GT 0 THEN neutrals = neutrals[keep]

        ENDIF

     ENDFOR
     
  ENDELSE

  set = set[*, 1:*]

  Count = N_ELEMENTS(set[0, *])

  RETURN,set
END

;; GET

FUNCTION AmesPAHdbIDLSuite::GetSpeciesByUID,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Species', $
                 Type=(*self.pahdb).type ,$
                 Version=(*self.pahdb).version, $
                 Data=self->GetTagByUID('species', UIDs, Count), $
                 PAHdb=self.pahdb, $
                 Uids=[UIDs], $
                 References=self->GetTagByUID('references', UIDs, Count), $
                 Comments=self->GetTagByUID('comments', UIDs, Count))
END

FUNCTION AmesPAHdbIDLSuite::GetTransitionsByUID,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Transitions', $
                 Type=(*self.pahdb).type ,$
                 Version=(*self.pahdb).version, $
                 Data=self->GetTagByUID('transitions', UIDs, Count), $
                 PAHdb=self.pahdb, $
                 Uids=[UIDs], $
                 Model={type:'AMESPAHDBIDLSUITE_MODEL_ZEROKELVIN_S',$
                        Temperature:0D, $
                        description:STRING(FORMAT='(A-12,":",X,A-0)', "model", "ZeroKelvin")}, $
                 Units={AmesPAHdb_Data_Units_S, $
                 abscissa:{AmesPAHdb_Unit_S, $
                           unit:1, $
                           str:'frequency [cm!U-1!N]'}, $
                 ordinate:{AmesPAHdb_Unit_S, $
                           unit:2, $
                           str:'integrated cross-section [km/mol]'}})
END

FUNCTION AmesPAHdbIDLSuite::GetLaboratoryByUID,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF (*self.pahdb).type NE 'experimental' THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"            EXPERIMENTAL DATABASE REQUIRED               "
     PRINT,"========================================================="
     PRINT
     RETURN, OBJ_NEW()
  ENDIF
  
  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Laboratory_Spectrum', $
                 Type=(*self.pahdb).type ,$
                 Version=(*self.pahdb).version, $
                 Data=self->GetTagByUID('laboratory', UIDs, Count), $
                 PAHdb=self.pahdb, $
                 Model={type:'AMESPAHDBIDLSUITE_RAW_LABORATORY_S',$
                        Temperature:0D, $
                        description:STRING(FORMAT='(A-12,":",X,A-0)', "model", "Laboratory")}, $
                 Uids=[UIDs], $
                 Units={AmesPAHdb_Data_Units_S, $
                        abscissa:{AmesPAHdb_Unit_S, $
                                  unit:1, $
                                  str:'frequency [cm!U-1!N]'}, $
                        ordinate:{AmesPAHdb_Unit_S, $
                                  unit:1, $
                                  str:'absorbance [-log(I/I!L0!N)]'}})
END

FUNCTION AmesPAHdbIDLSuite::GetGeometryByUID,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Geometry', $
                 Data=self->GetTagByUID('geometries', UIDs, Count), $
                 Uids=[UIDs])
END

FUNCTION AmesPAHdbIDLSuite::GetTagByUID,Tag,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = 0

  itag = WHERE(STRLOWCASE(TAG_NAMES((*self.pahdb).data)) EQ Tag, ntag)

  IF ntag NE 1 THEN RETURN,-1

  nuids = N_ELEMENTS(UIDs)

  IF SIZE(UIDs, /DIMENSIONS) EQ 0 AND UIDs EQ -1 THEN BEGIN

     Count = N_ELEMENTS((*self.pahdb).data.(itag))

     UIDs = (*self.pahdb).data.(itag)[UNIQ((*self.pahdb).data.(itag).uid, SORT((*self.pahdb).data.(itag).uid))].uid

     RETURN,(*self.pahdb).data.(itag)
  ENDIF

  nitag = N_ELEMENTS((*self.pahdb).data.(itag))

  idx = LINDGEN(nitag, nuids)

  select = WHERE((*self.pahdb).data.(itag)[idx MOD nitag].uid EQ UIDs[idx / nitag], Count) MOD nitag

  IF Count EQ 0 THEN RETURN,-1

  RETURN,(*self.pahdb).data.(itag)[select]
END

FUNCTION AmesPAHdbIDLSuite::Pointer

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.pahdb
END

;; SEARCH

FUNCTION AmesPAHdbIDLSuite::TokenizeWords,Word
  
  COMPILE_OPT IDL2

  ON_ERROR,2

  token = {word:Word, $
           translation:'', $
           type:'None', $
           valid:1}
    
  word = STRUPCASE(Word)
    
  charge = [{word:'anion', $
             translation:'(*self.joined).charge LT 0'}, $
            {word:'cation', $
             translation:'(*self.joined).charge GT 0'}, $
            {word:'neutral', $
             translation:'(*self.joined).charge EQ 0'}, $
            {word:'positive', $
             translation:'(*self.joined).charge GT 0'}, $
            {word:'negative', $
             translation:'(*self.joined).charge LT 0'}, $
            {word:'-', $
             translation:'(*self.joined).charge EQ -1'}, $
            {word:'+', $
             translation:'(*self.joined).charge EQ 1'}, $
            {word:'++', $
             translation:'(*self.joined).charge EQ 2'}, $
            {word:'+++', $
             translation:'(*self.joined).charge EQ 3'}, $
            {word:'---', $
             translation:'(*self.joined).charge EQ -3'}]

  icharge = WHERE(STRCMP(charge.word, Word, /FOLD_CASE), ncharge)

  identities = [{word:'uid', $
                 translation:'(*self.joined).uid'}, $
                {word:'identifier', $
                 translation:'(*self.joined).uid'}, $
                {word:'atoms', $
                 translation:'(*self.joined).natoms'}, $
                {word:'carbon', $
                 translation:'(*self.joined).nc'}, $
                {word:'hydrogen', $
                 translation:'(*self.joined).nh'}, $
                {word:'nitrogen', $
                 translation:'(*self.joined).nn'}, $
                {word:'oxygen', $
                 translation:'(*self.joined).no'}, $
                {word:'magnesium', $
                 translation:'(*self.joined).nmg'}, $
                {word:'silicium', $
                 translation:'(*self.joined).nsi'}, $
                {word:'iron', $
                 translation:'(*self.joined).nfe'}, $
                {word:'c', $
                 translation:'(*self.joined).nc'}, $
                {word:'h', $
                 translation:'(*self.joined).nh'}, $
                {word:'n', $
                 translation:'(*self.joined).nn'}, $
                {word:'o', $
                 translation:'(*self.joined).no'}, $
                {word:'mg', $
                 translation:'(*self.joined).nmg'}, $
                {word:'si', $
                 translation:'(*self.joined).nsi'}, $
                {word:'fe', $
                 translation:'(*self.joined).nfe'}, $
                {word:'wavenumber', $
                 translation:'(*self.joined).frequency'}, $
                {word:'absorbance', $
                 translation:'(*self.joined).intensity'}, $
                {word:'frequency', $
                 translation:'(*self.joined).frequency'}, $
                {word:'intensity', $
                 translation:'(*self.joined).intensity'}, $
                {word:'ch2', $
                 translation:'(*self.joined).nch2'}, $
                {word:'chx', $
                 translation:'(*self.joined).nchx'}, $
                {word:'solo', $
                 translation:'(*self.joined).nsolo'}, $
                {word:'duo', $
                 translation:'(*self.joined).nduo'}, $
                {word:'trio', $
                 translation:'(*self.joined).ntrio'}, $
                {word:'quartet', $
                 translation:'(*self.joined).nquartet'}, $
                {word:'quintet', $
                 translation:'(*self.joined).nquintet'}, $
                {word:'charge', $
                 translation:'(*self.joined).charge'}, $
                {word:'symmetry', $
                 translation:'(*self.joined).symmetry'}, $
                {word:'weight', $
                 translation:'(*self.joined).weight'}, $
                {word:'scale', $
                 translation:'(*self.joined).scale'}, $
                {word:'energy', $
                 translation:'(*self.joined).total_e'}, $
                {word:'zeropoint', $
                 translation:'(*self.joined).vib_e'}, $
                {word:'experiment', $
                 translation:'(*self.joined).exp'}]
  
  iidentity = WHERE(STRCMP(identities.word, Word, /FOLD_CASE), nidentity)

  logical = [{word:'and', $
              translation:'AND'}, $
             {word:'or', $
              translation:'OR'}, $
             {word:'|', $
              translation: 'OR'}, $
             {word:'&', $
              translation: 'AND'}]

  ilogical = WHERE(STRCMP(logical.word, Word, /FOLD_CASE), nlogical)
    
  comparison = [{word:'<', $
                 translation:'LT'}, $
                {word:'lt', $
                 translation:'LT'}, $
                {word:'>', $
                 translation:'GT'}, $
                {word:'gt', $
                 translation:'GT'}, $
                {word:'=', $
                 translation:'EQ'}, $
                {word:'eq', $
                 translation:'EQ'}, $
                {word:'<=', $
                 translation:'LE'}, $
                {word:'le', $
                 translation:'LE'}, $
                {word:'>=', $
                 translation:'GE'}, $
                {word:'ge', $
                 translation:'GE'}, $
                {word:'with', $
                 translation:'AND'}, $
                {word:'ne', $
                 translation:'ne'}, $
                {word:'!=', $
                 translation:'ne'}]

  icomparison = WHERE(STRCMP(comparison.word, Word, /FOLD_CASE), ncomparison)

  transfer = [{word:'(', $
               translation:'('}, $
              {word:')', $
               translation:')'}]

  itransfer = WHERE(STRCMP(transfer.word, Word, /FOLD_CASE), ntransfer)

  IF STREGEX(Word, '(^[+-]?[0-9]+)|([A-Z][0-9][A-Z])', /FOLD_CASE, /BOOLEAN) THEN BEGIN

     token.type = "NUMERIC"

     token.translation = Word
  ENDIF ELSE IF ncharge THEN BEGIN

     token.type = "CHARGE"

     token.translation = charge[icharge].translation 
  ENDIF ELSE IF nidentity THEN BEGIN

     token.type = "IDENTITY"

     token.translation = identities[iidentity].translation 
  ENDIF ELSE IF nlogical THEN BEGIN

     token.type = "LOGICAL"

     token.translation = logical[ilogical].translation 
  ENDIF ELSE IF ncomparison THEN BEGIN

     token.type = "COMPARISON"

     token.translation = comparison[icomparison].translation 
  ENDIF ELSE IF ntransfer THEN BEGIN

     token.type = "TRANSFER"

     token.translation = transfer[itransfer].translation 
  ENDIF ELSE IF STREGEX(Word,'(MG+|SI+|FE+|[CHNO]+)([0-9]*)(MG+|SI+|FE+|[CHNO]+)([0-9]*)(MG+|SI+|FE+|[CHNO]*)([0-9]*)', /FOLD_CASE, /BOOLEAN) THEN BEGIN

     token.type = "FORMULA"

     token.translation = Word
  ENDIF ELSE BEGIN

     token.type = "IGNORE"      ; SHOULD BE NAME

     token.translation = Word
  ENDELSE

  RETURN,token
END

FUNCTION AmesPAHdbIDLSuite::ParseTokens,Tokens

  COMPILE_OPT IDL2

  ON_ERROR,2

  ntokens = N_ELEMENTS(Tokens)

  prev = -1 & current = 0

  IF ntokens GT 1 THEN next = 1 ELSE next = -1
    
  parsed = ''
  WHILE current NE -1 DO BEGIN
     CASE Tokens[current].type OF

        'FORMULA': BEGIN
           IF prev NE -1 THEN BEGIN

              IF NOT (Tokens[prev].type NE 'LOGICAL' AND Tokens[prev].valid EQ 1) THEN parsed += ' OR '
           ENDIF

           parsed += ' STRMATCH(joined.formula, "' + Tokens[current].translation + '*", /FOLD_CASE)'
        END	
        'IDENTITY': BEGIN

           IF prev NE -1 THEN BEGIN

              IF NOT (Tokens[prev].type EQ 'LOGICAL' OR Tokens[prev].type EQ 'TRANSFER' OR Tokens[prev].type EQ 'TRANSFER' AND Tokens[prev].valid EQ 1) THEN parsed += ' AND '
           ENDIF

           IF next NE -1 THEN BEGIN

              IF Tokens[next].type EQ 'COMPARISON' THEN parsed += ' ' + Tokens[current].translation ELSE parsed += ' ' + Tokens[current].translation + ' GT 0'
           ENDIF
        END	

        'NUMERIC': BEGIN 

           IF prev NE -1 THEN BEGIN

              IF Tokens[prev].type EQ 'COMPARISON' AND Tokens[prev].valid EQ 1 THEN parsed += ' ' + Tokens[current].translation ELSE Tokens[current].valid = -1
           ENDIF
        END

        'LOGICAL': BEGIN 

           IF prev NE -1 THEN BEGIN

              IF (Tokens[prev].type EQ 'IDENTITY' OR Tokens[prev].type EQ 'NUMERIC' OR Tokens[prev].type EQ 'FORMULA' OR Tokens[prev].type EQ 'CHARGE' AND Tokens[prev].valid EQ 1) THEN BEGIN

                 IF next NE -1 THEN BEGIN

                    IF (Tokens[next].type EQ 'TRANSFER') THEN parsed += Tokens[current].translation $
                    ELSE IF (Tokens[next].type EQ 'IDENTITY' OR Tokens[next].type EQ 'NUMERIC' OR Tokens[next].type EQ 'FORMULA' OR Tokens[next].type EQ 'CHARGE') THEN parsed += ' ' + Tokens[current].translation ELSE Tokens[current].valid = -1
                 ENDIF
              ENDIF
           ENDIF
        END

        'COMPARISON': BEGIN

           IF prev NE -1 THEN BEGIN

              IF Tokens[prev].type EQ 'IDENTITY' AND Tokens[prev].valid EQ 1 THEN BEGIN

                 IF next NE -1 THEN BEGIN

                    IF Tokens[next].type EQ 'NUMERIC' THEN parsed +=  ' ' + Tokens[current].translation ELSE Tokens[current].valid = -1
                 ENDIF
              ENDIF
           ENDIF
        END

        'CHARGE': BEGIN              

           IF prev NE -1 THEN BEGIN

              IF NOT (Tokens[prev].type EQ 'LOGICAL' AND Tokens[prev].valid EQ 1) THEN parsed += ' AND '
           ENDIF

           parsed += ' ' + Tokens[current].translation
        END

        'TRANSFER': BEGIN

           parsed += Tokens[current].translation
        END

        'NAME': BEGIN	

           IF prev NE -1 THEN BEGIN

              IF NOT (Tokens[prev].type EQ 'LOGICAL' AND Tokens[prev].valid EQ 1) THEN parsed += ' AND '
           ENDIF

           ; THIS IS NOT CORRECTLY IMPLEMENTED
           ;parsed += ' STRMATCH(pahdb.data.comments.str, "' + Tokens[current].translation + '*", /FOLD_CASE)'
        END

        'IGNORE': BEGIN

           MESSAGE,Tokens[current].word+' NOT UNDERSTOOD',/INFORMATIONAL

           RETURN, ''
        END
     ENDCASE

     prev = current & current = next

     IF next++ GE ntokens - 1 THEN next = -1 
  ENDWHILE

  RETURN,parsed
END

FUNCTION AmesPAHdbIDLSuite::Search,Str,Count,Query=Query

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = 0

  IF STRLEN(Str) EQ 0 THEN RETURN,-1

  IF NOT PTR_VALID(self.joined) THEN BEGIN
  
     self->File_MD5,(*self.pahdb).filename,hash,err

     IF err NE '' THEN BEGIN

        self.state = 0
        RETURN, self.state
     ENDIF
     
     IF FILE_TEST(GETENV('IDL_TMPDIR')+hash[0]+'_joined.sav', /READ) THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"              RESTORING INDEX FROM CACHE                 "
        PRINT,"========================================================="
        PRINT
        RESTORE,FILENAME=GETENV('IDL_TMPDIR')+hash[0]+'_joined.sav'
     ENDIF ELSE BEGIN
        
        PRINT
        PRINT,"========================================================="
        PRINT,"       BUILDING INDEX: THIS MAY TAKE A FEW MINUTES       "
        PRINT,"========================================================="
        PRINT
        
        tags = TAG_NAMES((*self.pahdb).data.species)
        
        ntags = N_TAGS((*self.pahdb).data.species)
        
        command = 'template = CREATE_STRUCT((*self.pahdb).data.transitions[0]'
        
        FOR i = 0,  ntags - 1 DO BEGIN
        
           IF tags[i] EQ 'UID' THEN CONTINUE
        
           CASE SIZE((*self.pahdb).data.species.(i), /TYPE) OF
              2: command += ",'"+tags[i]+"'"+',0'
              3: command += ",'"+tags[i]+"'"+',0L'
              4: command += ",'"+tags[i]+"'"+',0E'
              5: command += ",'"+tags[i]+"'"+',0D'
              7: command += ",'"+tags[i]+"'"+',""'
              ELSE:
           ENDCASE
        ENDFOR
        
        command += ')'
        
        result = EXECUTE(command)
     
        ntransitions = N_ELEMENTS((*self.pahdb).data.transitions)
        
        joined = REPLICATE(template, ntransitions)
     
        STRUCT_ASSIGN,(*self.pahdb).data.transitions,joined,/NOZERO
     
        nids = N_ELEMENTS((*self.pahdb).data.species)
     
        jtags = TAG_NAMES(joined)
  
        FOR i = 0, nids - 1 DO BEGIN
        
           select = WHERE(joined.uid EQ (*self.pahdb).data.species[i].uid, nselect)
        
           FOR j = 0, ntags - 1 DO BEGIN
           
              k = WHERE(jtags EQ tags[j])
           
              IF tags[j] EQ 'UID' THEN CONTINUE
           
              joined[select].(k) = (*self.pahdb).data.species[i].(j)
           ENDFOR     
        ENDFOR

        SAVE,joined,FILENAME=GETENV('IDL_TMPDIR')+hash[0]+'_joined.sav'
     ENDELSE

     self.joined = PTR_NEW(joined)
  ENDIF

  words = ''

  len = STRLEN(Str)

  i = 0

  WHILE 1 DO BEGIN

     IF i EQ len THEN BREAK
     
     WHILE STRMID(Str, i, 1) EQ " " DO ++i

     token = STRMID(Str, i, 1)

     IF SIZE(WHERE(["=", "<", ">", "(", ")"] EQ token), /DIMENSIONS) NE 0 THEN BEGIN

        IF ++i LT len AND STRMID(Str, i, 1) EQ "=" THEN token += STRMID(Str, i++, 1)     
     ENDIF ELSE IF token EQ "&" THEN BEGIN
        
        IF ++i LT len AND STRMID(Str, i, 1) EQ "&" THEN token += STRMID(Str, i++, 1)
     ENDIF ELSE IF token EQ "|" THEN BEGIN
        
        IF ++i LT len AND STRMID(Str, i, 1) EQ "|" THEN token += STRMID(Str, i++, 1)
     ENDIF ELSE IF token EQ "!" THEN BEGIN
     
        IF ++i LT len AND STRMID(Str, i, 1) EQ "=" THEN token += STRMID(Str, i++, 1)
     ENDIF ELSE BEGIN

        ++i
        WHILE i LT len AND SIZE(WHERE([" ", "=", "<", ">", "&", "|", "(", ")", "!"] EQ STRMID(Str, i, 1)), /DIMENSIONS) EQ 0 DO token += STRMID(Str, i++, 1)
     ENDELSE

     words = [words, token]
  ENDWHILE

  words = words[1:*]

  nwords = N_ELEMENTS(words)

  tokens = {word:'', $
            translation:'', $
            type:'', $
            valid:0}

  FOR i = 0, nwords - 1 DO tokens = [tokens, self->TokenizeWords(words[i])]

  tokens = tokens[1:*]

  QUERY = self->ParseTokens(tokens)

  IF QUERY EQ '' THEN RETURN,-1

  res = EXECUTE('select = WHERE(' + QUERY + ', Count)')

  IF Count EQ 0 THEN RETURN,-1

  u = (*self.joined)[select[UNIQ((*self.joined)[select].uid, SORT((*self.joined)[select].uid))]].uid

  Count = N_ELEMENTS(u)

  RETURN,u
END

;; VERSIONING

FUNCTION AmesPAHdbIDLSuite::CheckVersion,Version

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,STRCMP((*self.pahdb).version, Version)
END

FUNCTION AmesPAHdbIDLSuite::GetVersion,String=String

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF KEYWORD_SET(String) THEN RETURN,STRING(FORMAT='("PAHdb,",X,A0,X,", version",X,A0,X,"(",A0,",",X,A0,")")', (*self.pahdb).type, (*self.pahdb).version, (*self.pahdb).date, (*self.pahdb).full ? "complete" : "partial")

  RETURN,{AmesPAHdbIDLSuite_Version_S, $
          type:(*self.pahdb).type, $
          number:(*self.pahdb).version, $
          date:(*self.pahdb).date, $
          full:(*self.pahdb).full}
END

;; LOAD

PRO AmesPAHdbIDLSuite::ReadFile,File,Check=Check,Cache=Cache

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->File_MD5,File,hash,err

  IF err NE '' THEN BEGIN

     self.state = 0
     RETURN
  ENDIF
  
  IF SIZE(Cache, /TYPE) EQ 0 THEN Cache = 1

  IF NOT Cache THEN FILE_DELETE,GETENV('IDL_TMPDIR')+hash[0]+'_joined.sav',/ALLOW_NONEXISTENT

  PRINT
  PRINT,"========================================================="
  PRINT,"         NASA Ames PAH IR SPECTROSCOPIC DATABASE         " 
  PRINT,"========================================================="
  PRINT
  
  IF Cache AND FILE_TEST(GETENV('IDL_TMPDIR')+hash[0]+'.sav', /READ) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"             RESTORING DATABASE FROM CACHE               "
     PRINT,"========================================================="
     PRINT
     timer = SYSTIME(/SECONDS)
     RESTORE,FILENAME=GETENV('IDL_TMPDIR')+hash[0]+'.sav'
     PRINT,"========================================================="
     PRINT,FORMAT='(A-30, ": ", A0)',"FILENAME",""+GETENV('IDL_TMPDIR')+hash[0]+'.sav'
     PRINT,FORMAT='(A-30, ": ", A0)',"ORIGINAL FILE",""+pahdb.filename
     PRINT,FORMAT='(A-30, ": ", A0)',"DATABASE",""+pahdb.type+" ("+(pahdb.full ? "complete" : "partial")+")"
     timer = SYSTIME(/SECONDS) - timer
     IF timer LT 1 THEN PRINT,FORMAT='(A-30, ": ", A0)',"RESTORE TIME",STRING(FORMAT='(I-3)', timer*1D3)+" MILLISECONDS" $
     ELSE IF timer LT 60 THEN PRINT,FORMAT='(A-30, ": ", A0)',"RESTORE TIME",STRING(FORMAT='(I02)',timer)+" SECONDS" $
     ELSE IF timer LT 3600 THEN PRINT,FORMAT='(A-30, ": ", A0)',"RESTORE TIME",STRING(FORMAT='(I02,":",I02)',timer/60,timer MOD 60)+" MINUTES" $
     ELSE IF timer LT 86400 THEN PRINT,FORMAT='(A-30, ": ", A0)',"RESTORE TIME",STRING(FORMAT='(I02,":",I02,":",I02)',timer/3600,(timer MOD 3600)/60,(timer MOD 3600) MOD 60)+" HOURS" $
     ELSE PRINT,FORMAT='(A-30, ": ", A0)',"RESTORE TIME",STRING(FORMAT='(I03)',timer/86400E)+" DAYS"
     PRINT,FORMAT='(A-30, ": ", A0)',"VERSION (DATE)",""+pahdb.version+" ("+pahdb.date+")"
     PRINT,FORMAT='(A-30, ": ", I-0)',"TOTAL NUMBER OF SPECIES",N_ELEMENTS(pahdb.data.species)
     PRINT,FORMAT='(A-30, ": ", I-0)',"TOTAL NUMBER OF TRANSITIONS",N_ELEMENTS(pahdb.data.transitions)
     PRINT,FORMAT='(A-30,": ", A0)',"COMMENT",pahdb.comment
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"      PARSING DATABASE: THIS MAY TAKE A FEW MINUTES      "
     PRINT,"========================================================="
     PRINT     

     IF SIZE(Check, /TYPE) EQ 0 THEN Check = 2

     XML = OBJ_NEW('AmesPAHdbIDLSuite_XMLParser', SCHEMA_CHECKING=Check)
     XML->ParseFile,File
     pahdb = XML->GetDatabase()
     OBJ_DESTROY,XML
     SAVE,pahdb,FILENAME=GETENV('IDL_TMPDIR')+hash[0]+'.sav'
  ENDELSE

  self.pahdb = PTR_NEW(pahdb)
END

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.pahdb) THEN PTR_FREE,self.pahdb

  IF PTR_VALID(self.joined) THEN PTR_FREE,self.joined
END

FUNCTION AmesPAHdbIDLSuite::Init,Filename=Filename,Check=Check,Cache=Cache

  COMPILE_OPT IDL2

  ON_ERROR,2

  PRINT
  PRINT,"========================================================="
  PRINT
  PRINT,"                    AmesPAHdbIDLSuite                    " 
  PRINT
  PRINT,"                           by                            "
  PRINT
  PRINT,"                  Dr. Christiaan Boersma                 "
  PRINT
  PRINT,"========================================================="
  PRINT

  version_file = FILE_DIRNAME(ROUTINE_FILEPATH()) + '/VERSION'
  
  IF FILE_TEST(version_file, /READ) THEN BEGIN

     OPENR,funit,version_file,/GET_LUN

     dated = ''

     READF,funit,dated

     PRINT
     PRINT,"========================================================="
     PRINT,"                SUITE DATED: "+STRUPCASE(dated)
     PRINT,"========================================================="
     PRINT
     
     CLOSE,funit

     FREE_LUN,funit
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"         WEBSITE: HTTP://WWW.ASTROCHEM.ORG/PAHDB/        "
  PRINT,"========================================================="
  PRINT
  
  PRINT
  PRINT,"========================================================="
  PRINT,"          CONTACT: CHRISTIAAN.BOERSMA@NASA.GOV           "
  PRINT,"========================================================="
  PRINT
  
  DEFSYSV,'!GDL',EXISTS=isgdl

  IF isgdl THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"    THE AMESPAHDBIDLSUITE IS NOT COMPATIBLE WITH GDL     "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN,0
  ENDIF

  IF NOT KEYWORD_SET(Filename) THEN BEGIN
     
     DEFSYSV,'!AMESPAHDEFAULTDB',EXISTS=defsys
     
     IF defsys EQ 1 THEN Filename = !AMESPAHDEFAULTDB $
     ELSE IF GETENV('AMESPAHDEFAULTDB') NE '' THEN Filename = GETENV('AMESPAHDEFAULTDB') $
     ELSE BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT," DATABASE NOT FOUND: SET IDL OR SYSTEM AMESPAHDEFAULTDB ENVIRONMENT VARIABLE"
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,self.state
     ENDELSE
  ENDIF

  IF NOT FILE_TEST(FILENAME, /READ) THEN BEGIN
     IF (alt = FILE_WHICH(FILE_BASENAME(FILENAME), /INCLUDE_CURRENT_DIR))  NE '' THEN Filename = alt $
     ELSE BEGIN
       PRINT
       PRINT,"========================================================="
       PRINT,"      UNABLE TO READ: "+FILENAME
       PRINT,"========================================================="
       PRINT
       RETURN,0
    ENDELSE 
  ENDIF

  self->ReadFile,FILENAME,Check=check,Cache=Cache

  RETURN,1
END

PRO AmesPAHdbIDLSuite__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite, $
          state:0, $
          pahdb:PTR_NEW(), $
          joined:PTR_NEW()}
END

; END OF AmesPAHdbIDLSuite__define.pro
