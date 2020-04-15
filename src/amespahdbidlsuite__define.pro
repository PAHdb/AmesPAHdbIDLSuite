; docformat = 'rst'

;+
;
; Main interface to work with the NASA Ames PAH IR Spectroscopic
; Database's XML file.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Obtaining and plotting the experimental spectrum of the PAH species
;   with UID = 100::
;
;     IDL> dbi = OBJ_NEW('AmesPAHdbIDLSuite', Filename='experimental.xml')
;     IDL> pahs = db->GetTransitionsByUID(100)
;     IDL> pahs->Plot
;     IDL> OBJ_DESTROY,[pahs, dbi]
;
; :Properties:
;   Filename: in, optional, type=string
;     Database XML-file
;   Check: in, optional, type=int
;     Check the consistency of the XML-file (defaults to 1)
;   Cache: in, optional, type=int
;     Use the cache (defaults to 1)
;   UpdateCheck: in, optional, type=int
;     Check for updates to the Suite (defaults to 1)
;
; :Uses:
;    AmesPAHdbIDLSuite_XMLParser,AmesPAHdbIDLSuite_Species,AmesPAHdbIDLSuite_Transitions,AmesPAHdbIDLSuite_Laboratory_Spectrum,AmesPAHdbIDLSuite_Geometry
;
; :Author:
;   Dr. Christiaan Boersma
;
; :Copyright:
;   BSD licensed
;
; :History:
;   Changes::
;
;     11-15-2019
;     Check the state of parsing an XML-file in PARSEFILE, which
;     avoids caching invalid data. Christiaan Boersma.
;     02-12-2019
;     Fixed hitting the maximum value that can be held in a LONG in
;     GETTAGBYUID by changing data type to ULONG. Ensuring returned
;     UIDs reflect those found in GETTAGBYUID. Christiaan Boersma
;     07-17-2018
;     Added EXTENDDATABASE
;     10-10-2017
;     Fixed GETUIDSCOMPLETECHARGESET to set list to !NULL when last
;     last anion/neutral has been matched. Christiaan Boersma.
;     10-06-2017
;     Added GETHASH to allow retrieval of the calculated md5-sum.
;     Christiaan Boersma.
;     08-11-2016
;     If md5 cannot be calculated, the basename of the database XML-file
;     is used instead. Christiaan Boersma.
;     07-06-2016
;     Using now TEMPORARY to move pahdb and joined to properties of
;     AmesPAHdbIDLSuite in READFILE and SEARCH, respectively. Christiaan
;     Boersma.
;     07-02-2016
;     Added file_md5 as a property, now calling method FILE_MD5 only
;     once in method READFILE, which sets file_md5. Affected routines
;     have been updated. File_md5 is added as a tag to the pahdb
;     structure. SEARCH and READFILE now print a message when saving to
;     cache and both routines have been refactored to store the location
;     of the cache file into a variable. READFILE now only prints the
;     database header when restoring from cache. Christiaan Boersma.
;     06-29-2016
;     Remove scale-property from species. Removed extra comma from
;     GETVERSION. Christiaan Boersma.
;     03-15-2016
;     Provide argument to ROUTINE_FILEPATH in INIT or compatibility with
;     older version of IDL. Christiaan Boersma.
;     09-09-2015
;     Added File_WGET procedure and updated INIT to allow for update
;     check (enabled with the UpdateCheck-keyword). Christiaan Boersma.
;     08-26-2015
;     Added File_MD5 procedure and updated READFILE and SEARCH
;     procedures. Updated Init to read VERSION-file from current
;     distribution. Christiaan Boersma.
;     04-21-2015
;     Moved stop condition to beginning of SEARCH-method. Removed
;     duplicate charge tokens from TOKENIZEWORDS. Christiaan Boersma
;     04-07-2015
;     Ensured UIDs in GetXXXXByUID are arrays, even when when called
;     with scalar. Christiaan Boersma.
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;-

;+
; Retrieves the contents of the file pointed to by Url.
;
; :Params:
;   Url: in, required, type=string
;     URL of file
;   File: out, optional, type=string
;     Contents of the file pointed to by Url
;
; :Categories:
;   INTERNET
;
; :Private:
;-
PRO AmesPAHdbIDLSuite::File_WGET,Url,File

  COMPILE_OPT IDL2

  ON_ERROR,2

  urlC = PARSE_URL(Url)

  netUrl = OBJ_NEW('IDLnetUrl')

  netUrl->SetProperty,URL_SCHEME=urlC.scheme

  netUrl->SetProperty,URL_HOSTNAME=urlC.host

  netUrl->SetProperty,URL_PORT=urlC.port

  netUrl->SetProperty,URL_PATH=urlC.path

  netUrl->SetProperty,URL_QUERY=urlC.query

  netUrl->SetProperty,HEADER='User-Agent: ' +  "IDL/" + !VERSION.RELEASE + ' (' + !VERSION.OS + '; ' + !VERSION.ARCH + ' like ' + !VERSION.OS_FAMILY + ')'

  File = STRING(netUrl->Get(/BUFFER))

  netUrl->CloseConnections

  OBJ_DESTROY,netUrl
END

;+
; Calculates the MD5-hash of File.
;
; :Params:
;   File: in, required, type=string
;     File to calculate MD5 of
;   Hash: out, optional, type=string
;     MD5-hash of File
;   Err: out, optional, type=string
;     Error string
;
; :Categories:
;   HASHING
;
; :Private:
;-
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

;+
; Returns the MD5-hash of the parsed database file.
;
; :Returns:
;   string
;
; :Categories:
;   HASHING
;-
FUNCTION AmesPAHdbIDLSuite::GetHash

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.file_md5
END

;+
; Returns the UIDs of complementary charge sets.
;
; :Returns:
;   long array (2D)
;
; :Params:
;   UIDs: in, required, type="long array (1D)"
;     UIDs to consider; -1 for all
;   Count: out, optional, type=long
;     Number of charge sets found
;
; :Keywords:
;   Two: in, optional, type=int
;      Consider only two charge states
;
; :Categories:
;   SEARCH
;
; :Bugs:
;  There are issues when isomers are present.
;-
FUNCTION AmesPAHdbIDLSuite::GetUIDsCompleteChargeSet,UIDs,Count,Two=Two

  COMPILE_OPT IDL2

  ON_ERROR,2

  pahs = self->GetTagByUID('species', UIDs)

  tags = STRLOWCASE(TAG_NAMES(pahs))

  tags = tags[WHERE(tags NE 'uid' AND tags NE 'charge' AND tags NE 'energy' AND tags NE 'formula' AND tags NE 'symm' AND tags NE 'method' AND tags NE 'zeropoint' AND tags NE 'formula')]

  anions = WHERE(pahs.charge EQ -1, nanions)

  cations = WHERE(pahs.charge EQ 1, ncations)

  neutrals = WHERE(pahs.charge EQ 0)

  IF NOT KEYWORD_SET(Two) THEN BEGIN

     set = [0L, 0L, 0L]

     FOR i = 0, nanions - 1 DO BEGIN

        cmd = "sel1 = WHERE(" + STRJOIN("pahs[cations]." + tags + " EQ pahs[anions[i]]." + tags, " AND ") + ", ncation)"

        ret = EXECUTE(cmd)

        IF ncation EQ 0 THEN CONTINUE

        geometries = self->getGeometryByUID([pahs[anions[i]].uid, pahs[cations[sel1]].uid])

        ;rings = geometries->rings()

        ;nrings = N_TAGS(rings[0])

        ;sel2 = WHERE(rings.uid EQ pahs[anions[i]].uid)

        ;same = BYTARR(ncation) + 1B

        ;FOR j = 0, ncation - 1 DO BEGIN

           ;sel3 = WHERE(rings.uid EQ pahs[cations[sel1[j]]].uid)

           ;FOR k = 1L, nrings - 1 DO BEGIN
           ;   IF rings[sel2].(k) NE rings[sel3].(k) THEN BEGIN

           ;      same[j] = 0B

           ;      CONTINUE
           ;   ENDIF
           ;ENDFOR
        ;ENDFOR

        ;keep = WHERE(same, ncation)

        ;sel1 = sel1[keep]

        ;IF ncation GT 1 THEN BEGIN

           geometries->Diagonalize,Full=0

           geo = geometries->get()

           norms = DBLARR(ncation)

           sel2 = WHERE(geo.data.uid EQ pahs[anions[i]].uid)

           x = geo.data[sel2].x & y = geo.data[sel2].y & z = geo.data[sel2].z

           FOR j = 0, ncation - 1 DO BEGIN

              sel2 = WHERE(geo.data.uid EQ pahs[cations[sel1[j]]].uid)

              norms[j] = NORM((x - geo.data[sel2].x)^2 + (y - geo.data[sel2].y)^2 + (z - geo.data[sel2].z)^2)
           ENDFOR

           min = MIN(norms, imin)

           IF min LT 1 THEN BEGIN

              sel1 = sel1[imin]

              ncation = 1L
           ENDIF ELSE ncation = 0L
        ;ENDIF

        OBJ_DESTROY,geometries

        IF ncation EQ 1 THEN BEGIN

           ret = EXECUTE("sel2 = WHERE(" + STRJOIN("pahs[neutrals]." + tags + " EQ pahs[anions[i]]." + tags, " AND ") + ", nneutral)")

           IF nneutral EQ 0 THEN CONTINUE

           geometries = self->getGeometryByUID([pahs[anions[i]].uid, pahs[neutrals[sel2]].uid])

           ;rings = geometries->rings()

           ;nrings = N_TAGS(rings[0])

           ;sel3 = WHERE(rings.uid EQ pahs[anions[i]].uid)

           ;same = BYTARR(nneutral) + 1B

           ;FOR j = 0, nneutral - 1 DO BEGIN

           ;   sel4 = WHERE(rings.uid EQ pahs[neutrals[sel2[j]]].uid)

           ;   FOR k = 1L, nrings - 1 DO BEGIN

           ;      IF rings[sel3].(k) NE rings[sel4].(k) THEN BEGIN

           ;         same[j] = 0B

           ;         CONTINUE
           ;      ENDIF
           ;   ENDFOR
           ;ENDFOR

           ;keep = WHERE(same, nneutral)

           ;sel2 = sel2[keep]

           ;IF nneutral GT 1 THEN BEGIN

              geometries->Diagonalize

              geo = geometries->get()

              norms = DBLARR(nneutral)

              sel3 = WHERE(geo.data.uid EQ pahs[anions[i]].uid)

              x = geo.data[sel3].x & y = geo.data[sel3].y & z = geo.data[sel3].z

              FOR j = 0, nneutral - 1 DO BEGIN

                 sel3 = WHERE(geo.data.uid EQ pahs[neutrals[sel2[j]]].uid)

                 norms[j] = SQRT(TOTAL((x - geo.data[sel3].x)^2 + (y - geo.data[sel3].y)^2 + (z - geo.data[sel3].z)^2))
              ENDFOR

              min = MIN(norms, imin)

              IF min LT 1 THEN BEGIN

                 sel2 = sel2[imin]

                 nneutral = 1
              ENDIF ELSE nneutral = 0
           ;ENDIF

           OBJ_DESTROY,geometries

           IF nneutral EQ 1 THEN BEGIN

              set = [[set], [pahs[anions[i]].uid, pahs[neutrals[sel2]].uid, pahs[cations[sel1]].uid]]

              keep = WHERE(cations NE cations[sel1[0]], nkeep)

              cations = nkeep GT 0 ? cations[keep] : !NULL

              keep = WHERE(neutrals NE neutrals[sel2[0]], nkeep)

              neutrals = nkeep GT 0 ? neutrals[keep] : !NULL
           ENDIF
        ENDIF
     ENDFOR
  ENDIF ELSE BEGIN

     set = [0L, 0L]

     FOR i = 0, ncations - 1 DO BEGIN

        cmd = "sel1 = WHERE(" + STRJOIN("pahs[neutrals]." + tags + " EQ pahs[cations[i]]." + tags, " AND ") + ", nneutral)"

        ret = EXECUTE(cmd)

        IF nneutral EQ 0 THEN CONTINUE

        geometries = self->getGeometryByUID([pahs[cations[i]].uid, pahs[neutrals[sel1]].uid])

        ;rings = geometries->rings()

        ;nrings = N_TAGS(rings[0])

        ;sel2 = WHERE(rings.uid EQ pahs[cations[i]].uid)

        ;same = BYTARR(nneutral) + 1B

        ;FOR j = 0, nneutral - 1 DO BEGIN

        ;   sel3 = WHERE(rings.uid EQ pahs[neutrals[sel1[j]]].uid)

        ;   FOR k = 1L, nrings - 1 DO BEGIN

        ;      IF rings[sel2].(k) NE rings[sel3].(k) THEN BEGIN

        ;         same[j] = 0B

        ;         CONTINUE
        ;      ENDIF
        ;   ENDFOR
        ;ENDFOR

        ;keep = WHERE(same, nneutral)

        ;sel1 = sel1[keep]

        ;IF nneutral GT 1 THEN BEGIN

           geometries->Diagonalize,Full=0

           geo = geometries->get()

           norms = DBLARR(nneutral)

           sel2 = WHERE(geo.data.uid EQ pahs[cations[i]].uid)

           x = geo.data[sel2].x & y = geo.data[sel2].y & z = geo.data[sel2].z

           FOR j = 0, nneutral - 1 DO BEGIN

              sel2 = WHERE(geo.data.uid EQ pahs[neutrals[sel1[j]]].uid)

              norms[j] = SQRT(TOTAL((x - geo.data[sel2].x)^2 + (y - geo.data[sel2].y)^2 + (z - geo.data[sel2].z)^2))
           ENDFOR

           min = MIN(norms, imin)
print,[pahs[neutrals[sel1[imin]]].uid, pahs[cations[i]].uid],min
           IF min LT 1 THEN BEGIN

              sel1 = sel1[imin]

              nneutral = 1
           ENDIF ELSE nneutral = 0
        ;ENDIF

        OBJ_DESTROY,geometries

        IF nneutral EQ 1 THEN BEGIN

           set = [[set], [pahs[neutrals[sel1]].uid, pahs[cations[i]].uid]]

           keep = WHERE(neutrals NE neutrals[sel1[0]], nkeep)

           neutrals = nkeep GT 0 ? neutrals[keep] : !NULL
        ENDIF
     ENDFOR
  ENDELSE

  set = set[*, 1:*]

  Count = N_ELEMENTS(set[0, *])

  RETURN,set
END

;+
; Returns AmesPAHdbIDLSuite_Species-instance containing the data on
; the species with UIDs.
;
; :Returns:
;   AmesPAHdbIDLSuite_Species-instance
;
; :Params:
;   UIDs: in, required, type="long array (1D)"
;     UIDs to consider; -1 for all
;   Count: out, optional, type=long
;     Number of species found
;
; :Categories:
;   RETRIEVAL
;
; :Bugs:
;   Not all species have references/comments and UIDs is updated to
;   only those that do
;
;-
FUNCTION AmesPAHdbIDLSuite::GetSpeciesByUID,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Species', $
                 Type=(*self.pahdb).type ,$
                 Version=(*self.pahdb).version, $
                 Data=self->GetTagByUID('species', UIDs, Count), $
                 PAHdb=self.pahdb, $
                 Uids=[UIDs]);;, $
                 ;;References=self->GetTagByUID('references', UIDs, Count), $
                 ;;Comments=self->GetTagByUID('comments', UIDs, Count))
END

;+
; Returns AmesPAHdbIDLSuite_Transitions-instance containing the data
; on the species with UIDs.
;
; :Returns:
;   AmesPAHdbIDLSuite_Transitions-instance
;
; :Params:
;   UIDs: in, required, type="long array (1D)"
;     UIDs to consider; -1 for all
;   Count: out, optional, type=long
;     Number of species found
;
; :Categories:
;   RETRIEVAL
;-
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

;+
; Returns AmesPAHdbIDLSuite_Laboratory_Spectrum-instance containing
; the data on the species with UIDs.
;
; :Returns:
;   AmesPAHdbIDLSuite_Laboratory_Spectrum-instance
;
; :Params:
;   UIDs: in, required, type="long array (1D)"
;     UIDs to consider; -1 for all
;   Count: out, optional, type=long
;     Number of species found
;
; :Categories:
;   RETRIEVAL
;-
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

;+
; Returns AmesPAHdbIDLSuite_Geometry-instance containing the data on
; the species with UIDs.
;
; :Returns:
;   AmesPAHdbIDLSuite_Geometry-instance
;
; :Params:
;   UIDs: in, required, type="long array (1D)"
;     UIDs to consider; -1 for all
;   Count: out, optional, type=long
;     Number of species found
;
; :Categories:
;   RETRIEVAL
;-
FUNCTION AmesPAHdbIDLSuite::GetGeometryByUID,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Geometry', $
                 Data=self->GetTagByUID('geometries', UIDs, Count), $
                 Uids=[UIDs])
END

;+
; Returns the data on associated with the given Tag
;
; :Returns:
;  struct (variable)
;
; :Params:
;   Tag: in, required, type=string
;    Name of the Tag
;   UIDs: in, required, type="long array (1D)"
;     UIDs to consider; -1 for all
;   Count: out, optional, type=long
;     Number of species found
;
; :Categories:
;   RETRIEVAL
;
; :Private:
;-
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

  idx = ULINDGEN(nitag, nuids)

  select = WHERE((*self.pahdb).data.(itag)[idx MOD nitag].uid EQ UIDs[idx / nitag], Count) MOD nitag

  IF Count EQ 0 THEN RETURN,-1

  UIDs = (*self.pahdb).data.(itag)[select[UNIQ((*self.pahdb).data.(itag)[select].uid, SORT((*self.pahdb).data.(itag)[select].uid))]].uid

  RETURN,(*self.pahdb).data.(itag)[select]
END

;+
; Returns a pointer to the parsed database.
;
; :Returns:
;   pointer
;
; :Categories:
;   RETRIEVAL
;-
FUNCTION AmesPAHdbIDLSuite::Pointer

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.pahdb
END

;+
; Returns the tokinazation of Word
;
; :Returns:
;   array of structures
;
; :Params:
;   Word: in, required, type=string
;      String to tokenize
;
; :Categories:
;   SEARCH
;
; :Private:
;-
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
                {word:'scale', $
                 translation:'(*self.joined).scale'}, $
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

;+
; Parses Tokens into a search string
;
; :Returns:
;   string
;
; :Params:
;   Tokens: in, required, type="array of tokens"
;      Tokens to parse
;
; :Categories:
;   SEARCH
;
; :Private:
;-
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

;+
; Returns the UIDs that match the given search Str.
;
; :Returns:
;   long array (1d)
;
; :Params:
;   Str: in, required, type=string
;     Search string
;   Count: out, optional, type=long
;     Number of results
;
; :Keywords:
;   Query: in, optional, type=string The generate query-string that can be used with the WHERE-function
;
; :Categories:
;   SEARCH
;-
FUNCTION AmesPAHdbIDLSuite::Search,Str,Count,Query=Query

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = 0

  IF STRLEN(Str) EQ 0 THEN RETURN,-1

  IF NOT PTR_VALID(self.joined) THEN BEGIN

     file_cache = GETENV('IDL_TMPDIR')+self.file_md5+'_joined.sav'

     IF FILE_TEST(file_cache, /READ) THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"              RESTORING INDEX FROM CACHE                 "
        PRINT,"========================================================="
        PRINT
        RESTORE,FILENAME=file_cache
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

        PRINT
        PRINT,"========================================================="
        PRINT,"STORING INDEX IN CACHE        : "+file_cache
        PRINT,"========================================================="
        PRINT

        SAVE,joined,FILENAME=file_cache
     ENDELSE

     self.joined = PTR_NEW(TEMPORARY(joined))
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

;+
; Checks whether the given Version matched the version of the parsed
; database.
;
; :Returns:
;   long
;
; :Params:
;   Version: in, required, type=string
;     Version string
;
; :Categories:
;   VERSIONING
;-
FUNCTION AmesPAHdbIDLSuite::CheckVersion,Version

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,STRCMP((*self.pahdb).version, Version)
END

;+
; Returns versioning information.
;
; :Returns:
;   structure or string
;
; :Keywords:
;   String: out, optional, type=string
;     Formatted version string
;
; :Categories:
;   VERSIONING
;-
FUNCTION AmesPAHdbIDLSuite::GetVersion,String=String

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF KEYWORD_SET(String) THEN RETURN,STRING(FORMAT='("PAHdb,",X,A0,", version",X,A0,X,"(",A0,",",X,A0,")")', (*self.pahdb).type, (*self.pahdb).version, (*self.pahdb).date, (*self.pahdb).full ? "complete" : "partial")

  RETURN,{AmesPAHdbIDLSuite_Version_S, $
          type:(*self.pahdb).type, $
          number:(*self.pahdb).version, $
          date:(*self.pahdb).date, $
          full:(*self.pahdb).full}
END

;+
; Extends the database with outside-data
;
; :Keywords:
;   Species: in, optional, type=AmesPAHdb_Property_S
;     Species data
;   Comments: in, optional, type=AmesPAHdb_Comment_S
;     Comment data
;   References: in, optional, type=AmesPAHdb_Reference_S
;     Reference data
;   Transitions: in, optional, type=AmesPAHdb_Transition_S
;     Transition data
;   Geometries: in, optional, type=AmesPAHdb_Geometry_S
;     Geometry data
;   Laboratory: in, optional, type=AmesPAHdb_Laboratory_S
;     Laboratory data
;
; :Categories:
;   EXTENDING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite::ExtendDatabase,Species=Species,Comments=Comments,References=References,Transitions=Transitions,Geometries=Geometries,Laboratory=Laboratory

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.pahdb) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                NO VALID DATABASE                        "
     PRINT,"========================================================="
     PRINT
     RETURN
  ENDIF

  s = (*self.pahdb).data.species
  IF KEYWORD_SET(Species) THEN  s = [s, Species]
  c = (*self.pahdb).data.comments
  IF KEYWORD_SET(Comments) THEN  c = [c, Comments]
  r = (*self.pahdb).data.references
  IF KEYWORD_SET(References) THEN  r = [r, References]
  t = (*self.pahdb).data.transitions
  IF KEYWORD_SET(Transitions) THEN  t = [t, Transitions]
  g = (*self.pahdb).data.geometries
  IF KEYWORD_SET(Geometries) THEN  g = [g, Geometries]
  l = (*self.pahdb).data.laboratory
  IF KEYWORD_SET(Laboratory) THEN  r = [r, Laboratory]

  (*self.pahdb) = {filename:(*self.pahdb).filename, $
                   type:(*self.pahdb).type, $
                   version:(*self.pahdb).version, $
                   date:(*self.pahdb).date,$
                   full:(*self.pahdb).full, $
                   comment:(*self.pahdb).comment, $
                   sizes:(*self.pahdb).sizes, $
                   data:{species:s, $
                         comments:c, $
                         references:r, $
                         transitions:t, $
                         geometries:g, $
                         laboratory:l}}

	(*self.pahdb).sizes = {nspecies:N_ELEMENTS(s), ncomments:N_ELEMENTS(c), nreferences:N_ELEMENTS(r), ntransitions:N_ELEMENTS(t), ngeometries:N_ELEMENTS(g), nlaboratory:N_ELEMENTS(l)}
END

;+
; Reads in a database XML-file
;
; :Params:
;   File: in, required, type=string
;     Database XML-file
;
; :Keywords:
;   Check: in, optional, type=int
;     Check the consistency of the XML-file (defaults to 1)
;   Cache: in, optional, type=int
;     Use the cache (defaults to 1)
;
; :Categories:
;   PARSING
;-
PRO AmesPAHdbIDLSuite::ReadFile,File,Check=Check,Cache=Cache

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->File_MD5,File,hash,err

  IF err NE '' THEN hash = FILE_BASENAME(File, '.xml')

  self.file_md5 = hash[0]

  file_cache = GETENV('IDL_TMPDIR')+self.file_md5+'.sav'

  IF SIZE(Cache, /TYPE) EQ 0 THEN Cache = 1

  IF NOT Cache THEN FILE_DELETE,file_cache,/ALLOW_NONEXISTENT

  IF Cache AND FILE_TEST(file_cache, /READ) THEN BEGIN

     PRINT
     PRINT,"========================================================="
     PRINT,"         NASA Ames PAH IR SPECTROSCOPIC DATABASE         "
     PRINT,"========================================================="
     PRINT

     PRINT
     PRINT,"========================================================="
     PRINT,"             RESTORING DATABASE FROM CACHE               "
     PRINT,"========================================================="
     PRINT

     timer = SYSTIME(/SECONDS)
     RESTORE,FILENAME=file_cache
     PRINT,"========================================================="
     PRINT,FORMAT='(A-30, ": ", A0)',"FILENAME",""+file_cache
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

     IF XML->Status() EQ 0 THEN BEGIN

        pahdb = CREATE_STRUCT('file_md5', self.file_md5, XML->GetDatabase())

        PRINT
        PRINT,"========================================================="
        PRINT,"STORING DATABASE IN CACHE     : "+file_cache
        PRINT,"========================================================="
        PRINT

        SAVE,pahdb,FILENAME=file_cache
     ENDIF
  ENDELSE

  self.pahdb = PTR_NEW(TEMPORARY(pahdb))
END

;+
; Clean-up an AmesPAHdbIDLSuite-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.pahdb) THEN PTR_FREE,self.pahdb

  IF PTR_VALID(self.joined) THEN PTR_FREE,self.joined
END

;+
; Create an AmesPAHdbIDLSuite-instance.
;
; :Returns:
;   AmesPAHdbIDLSuite-instance
;
; :Keywords:
;   Filename: in, optional, type=string
;     Database XML-file
;   Check: in, optional, type=int
;     Check the consistency of the XML-file (defaults to 1)
;   Cache: in, optional, type=int
;     Use the cache (defaults to 1)
;   UpdateCheck: in, optional, type=int
;     Check for updates to the Suite (defaults to 1)
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite::Init,Filename=Filename,Check=Check,Cache=Cache,UpdateCheck=UpdateCheck

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

  version_file = FILE_DIRNAME(ROUTINE_FILEPATH("amespahdbidlsuite__define", /EITHER)) + '/VERSION'

  IF FILE_TEST(version_file, /READ) THEN BEGIN

     OPENR,funit,version_file,/GET_LUN

     v = ''

     READF,funit,v

     self.version = v

     CLOSE,funit

     FREE_LUN,funit

     PRINT
     PRINT,"========================================================="
     PRINT,"               SUITE DATED: "+STRUPCASE(self.version)
     PRINT,"========================================================="
     PRINT

     IF KEYWORD_SET(UpdateCheck) THEN BEGIN

        self->File_WGET,'https://www.astrochemistry.org/pahdb/theoretical/2.00/ajax/amespahdbidlsuite/versioncheck',remote_version

        IF remote_version EQ '' THEN BEGIN

           PRINT
           PRINT,"========================================================="
           PRINT,"               CHECKING FOR UPDATES FAILED               "
           PRINT,"========================================================="
           PRINT
        ENDIF ELSE BEGIN

           IF self.version EQ remote_version THEN BEGIN

              PRINT
              PRINT,"========================================================="
              PRINT,"                   NO UPDATES AVAILABLE                  "
              PRINT,"========================================================="
              PRINT
           ENDIF ELSE BEGIN

              PRINT
              PRINT,"========================================================="
              PRINT,"           UPDATE AVAILABLE: "+STRUPCASE(remote_version)
              PRINT,"========================================================="
              PRINT
           ENDELSE
        ENDELSE
     ENDIF
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"      WEBSITE: HTTP://WWW.ASTROCHEMISTRY.ORG/PAHDB/      "
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

  IF NOT FILE_TEST(Filename, /READ) THEN BEGIN
     IF (alt = FILE_WHICH(FILE_BASENAME(Filename), /INCLUDE_CURRENT_DIR))  NE '' THEN Filename = alt $
     ELSE BEGIN
       PRINT
       PRINT,"========================================================="
       PRINT,"      UNABLE TO READ: "+Filename
       PRINT,"========================================================="
       PRINT
       RETURN,0
    ENDELSE
  ENDIF

  self->ReadFile,Filename,Check=Check,Cache=Cache

  RETURN,1
END

;+
; Defines the AmesPAHdbIDLSuite Class.
;
; :Fields:
;   state: type=int
;     Internal state
;   file_md5: type=string
;     MD5-hash of the parsed database file
;   version: type=sting
;     Versioning information
;   pahdb: type=pointer
;     Database pointer
;   joined: type=pointer
;     Pointer to the joined database tables
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite, $
          state:0, $
          file_md5:'', $
          version:'', $
          pahdb:PTR_NEW(), $
          joined:PTR_NEW()}
END

; END OF AmesPAHdbIDLSuite__define.pro
