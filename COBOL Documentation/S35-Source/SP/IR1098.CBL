       IDENTIFICATION DIVISION.
       PROGRAM-ID.       IR1098.
       DATE-WRITTEN.   12/19/86.
      ******************************************************************
      *
      *                      (SP)
      *       DESCRIPTION:  INTEREST PAID 1098 EXTRACTION
      *
      * REV:
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 120996 ADDED TEST FOR ZIP +4
      *  BAH 010598 CHANGED TO RUN GLOBALLY, AND PUT FILES IN GLOBAL SP
      *             ADVANCE MORTG. PR# 628
      *  BAH 012898 FM RECORDS WERE ONLY GETTING WRITTEN IN BRANCH YOU
      *             WERE RUNNING IN, CREATE EACH USING FM-ALL-PATH
      *  012799 BAH  IL RECORD EXPANDED FROM 420 TO 750, ALOT OF
      *              FIELDS CHANGED AS PER SPECS FROM IRS
      *  020399 BAH REMOVED REAL ESTATE CODES, IRS DID AWAY WITH
      *             THE IL-B-SPECIFIC-CD THAT TESTED FOR MTG'S
      *             AFTER 1987, ADDED MORE COLLATERAL CODES, CAVALIER
      *             NEEDS 26 CODES
      *  991214 BLV ADDED PARAMETER CORP-FM-FG, IF Y, DON'T USE FM-ALL-PATH
      *             & FORM REQUESTS WILL ALL GET WRITTEN TO CORPORATE BRANCH-
      *             USER MUST BE LOGGED INTO CORPORATE OR HOME OFFICE BRANCH;
      *             IF FLAG IS NO, USE FM-ALL-PATH & FORM REQUESTS WILL BE
      *             WRITTEN TO EACH BRANCH'S FORM FILE; REGWAN PR# 264;
      *             ALSO MOVE BR# TO FM-BRNO TO PREVENT DUPLICATE FM KEYS
      *  BLV 010110 FIX BUG IN WHICH FM-BRNO2 WASN'T GETTING MOVED IN BEFORE
      *             READ OF FM (CAUSED WRITE ERROR 22 ON FM), BUT ONLY MOVE
      *             INTO FM-BRNO2 IF CORP-FM-FG = "Y", BECAUSE CUTSHEET
      *             PRINTING DOES NOT WORK WITH FM-BRNO2 SET!  IF CUSTOMER
      *             DOING CORPORATE PRINT, THEY MUST BE CONTINUOUS!
      *  BAH 010131 SKIP ACCOUNTS WITH "BT" IN THE POCD (BULK TRANSFERRED),
      *             THE NEW OWNING BRANCH WILL REPORT IT !!, LENDMARK PL# 314
      *  BAH 030122 IF CO1-BUF = "**", SKIP TESTING COLLCD'S, CAVALIER
      *
      * KEC 2016.0222 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *  BAH 161122 ADDED SET OF IL-B-PAYAMT(6) FOR MORTGAGE PRINCIPLE BALANCE
      *             AS OF 01/01/TAX YEAR REPORTING. #5214
      *  20170926 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  20171221 ADDED SET OF IL-B-NO-MORTGAGED-PROP FOR REGENCY PR#5332
      *  BAH 181004 GLOBAL BWFILE
      *  BAH 181112 GLOBAL FMFILE
      *  BAH 181214 GLOBAL LNFILE
      *  BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *             REDEFINE IN THE FM-REC
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      *  20190716 CHANGED IL-B-PAYEE-NAME FROM BW-NAME TO A WORKING STORAGE
      *           FIELD SO IT CONTINUES TO MATCH A15. WE CHANGED NAME FIELDS
      *           FROM 16 TO 35, QA0051
      * BAH 2019.0930 REMOVED ACCESS-CALL LNFILE
      * BAH 2022.0111 IL-B-PAYAMT NOW OCCURS 18, NOT 16                 # 1537
      * BAH 2022.0803 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBSP/SPFSIL.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBSP/SPFDIL.CPY".

      ********************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/IR1098.CBL S35 03/26/24-12:23:27 >".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBSP/SPWSIL.CPY".
       01  IL-KEY               PIC 9(6)     VALUE 0.

       01  TEST-DATE            PIC 9(8).
       01  TEST-DWELLCD.
           03  TEST-DWELL-1     PIC X.
           03  TEST-DWELL-2     PIC X.
       01  NUMERIC-DWELLCD      PIC 99.

       01  PAYEE-NAME.
           03  PAYEE-LNAME      PIC X(16).
           03  PAYEE-FNAME      PIC X(16).

       01  CARRAGE-RET          PIC X VALUE X"0D".
       01  SUB                  PIC 9(6) COMP-3       VALUE 0.
       01  SUB2                 PIC 9(7) COMP-3       VALUE 0.
       01  MATCH                PIC X        VALUE "N".
       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.
       01  ZIP-WORK             PIC X(10).
       01  X REDEFINES ZIP-WORK.
           03  FILLER           PIC X(6).
           03  PART-2           PIC X(4).
       01  X1 REDEFINES ZIP-WORK.
           03  FILLER           PIC X(5).
           03  PART-2-A         PIC X(4).
           03  ZIP-JUNK         PIC X.
       01  X3 REDEFINES ZIP-WORK.
           03  PART-3-ZIP       PIC X(5).
           03  PART-3-DASH      PIC X(1).
           03  PART-3-EXT       PIC X(4).

       01  WORK-FIELD           PIC X(40).
       01  X2 REDEFINES WORK-FIELD.
           03  CHAR-4           PIC X(4).
           03  FILLER           PIC X(36).
       01  X3 REDEFINES WORK-FIELD.
           03  WORK-CHAR        PIC X OCCURS 40 TIMES.
       01  X5 REDEFINES WORK-FIELD.
           03  CHAR-1-2         PIC 99.
           03  CHAR-3           PIC X.
           03  CHAR-4-5         PIC 99.
           03  CHAR-6           PIC X.
           03  CHAR-7-12        PIC 9(6).
           03  CHAR-13          PIC X.
           03  CHAR-14-15       PIC 99.
           03  FILLER           PIC X(25).

       01  WORK-FIELD-2         PIC X(40).
       01  X4 REDEFINES WORK-FIELD-2.
           03  WORK-CHAR-2      PIC X OCCURS 40 TIMES.

       01  TRANS-DATE           PIC 9(8).
       01  VERSION-CONTROL      PIC X VALUE "A".
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR           PIC 9(4).
           03  END-BR           PIC 9(4).
           03  BEG-CL           PIC 9(3).
           03  END-CL           PIC 9(3).
           03  BEG-ACCT         PIC 9(8).
           03  END-ACCT         PIC 9(8).
           03  COCODES.
               05  COCODE       PIC XX OCCURS 30.
           03  COCODEX REDEFINES COCODES.
               05  CO1-BUF      PIC XX.
               05  CO2-BUF      PIC XX.
               05  CO3-BUF      PIC XX.
               05  CO4-BUF      PIC XX.
               05  CO5-BUF      PIC XX.
               05  CO6-BUF      PIC XX.
               05  CO7-BUF      PIC XX.
               05  CO8-BUF      PIC XX.
               05  CO9-BUF      PIC XX.
               05  CO10-BUF     PIC XX.
               05  CO11-BUF     PIC XX.
               05  CO12-BUF     PIC XX.
               05  CO13-BUF     PIC XX.
               05  CO14-BUF     PIC XX.
               05  CO15-BUF     PIC XX.
               05  CO16-BUF     PIC XX.
               05  CO17-BUF     PIC XX.
               05  CO18-BUF     PIC XX.
               05  CO19-BUF     PIC XX.
               05  CO20-BUF     PIC XX.
               05  CO21-BUF     PIC XX.
               05  CO22-BUF     PIC XX.
               05  CO23-BUF     PIC XX.
               05  CO24-BUF     PIC XX.
               05  CO25-BUF     PIC XX.
               05  CO26-BUF     PIC XX.
               05  CO27-BUF     PIC XX.
               05  CO28-BUF     PIC XX.
               05  CO29-BUF     PIC XX.
               05  CO30-BUF     PIC XX.
           03  BEG-INT          PIC 9(6).
           03  END-INT          PIC 9(6).
           03  WHICHYEAR        PIC X.
           03  PAY-YEAR-BUF     PIC 9999.
           03  FORMNO           PIC 999.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  CORP-FM-FG       PIC X.


       01  MESS-LIST            PIC X(5) VALUE "MESS.".

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/IR1098_DEF.CPY".
       COPY "LIBSP/IR1098_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/IR1098_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               IL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               IL-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *********************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *********************************************
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "IR1098" TO VDU-FORM-NAME.

           MOVE 0 TO IL-KEY.
           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.
           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              BX-PATH-OVERRIDE
                              LP-PATH-OVERRIDE.

      * IF NOT CORPORATE FORM PRINTING, SET EACH BRANCH FM PATH
           IF CORP-FM-FG = "N"
              MOVE "B"                     TO FM-PATH-OVERRIDE
           ELSE
      * IF CORPORATE PRINTING, USE CURRENT LOGIN BRANCH (EXTERNALS)
              MOVE EXT-FILPATH-BASE        TO FM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
              MOVE "Y"                     TO FM-PATH-OVERRIDE.


           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-BX1-FILE.

           PERFORM OPEN-LN3-FILE.
           MOVE BEG-CL        TO LN-CLASS.
           MOVE BEG-ACCT      TO LN-ACCTNO.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR.
           MOVE BEG-CL        TO LN-CLASS
                                 QLN3-WBEG-CLASS.
           MOVE END-CL        TO QLN3-WEND-CLASS.
           MOVE BEG-ACCT      TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO.
           MOVE END-ACCT      TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BR
              GO TO END-OF-GROUP.

      * VOIDED LOAN, RESCISSION
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CL OR LN-CLASS > END-CL
              GO TO NEXT-LN.

           IF LN-ACCTNO < BEG-ACCT OR LN-ACCTNO > END-ACCT
              GO TO NEXT-LN.

      * SKIP ACCOUNTS "BULK TRANSFERRED" TO ANOTHER BRANCH, THE NEW
      * OWNING BRANCH WILL REPORT IT !!
           IF LN-POCD = "BT"
              GO TO NEXT-LN.

           IF WHICHYEAR = "C"
              IF LN-YTDINTPD < BEG-INT OR LN-YTDINTPD > END-INT
                 GO TO NEXT-LN.
           IF WHICHYEAR = "L"
              IF LN-LYRINTPD < BEG-INT OR LN-LYRINTPD > END-INT
                  GO TO NEXT-LN.

           IF CO1-BUF = "**"
              MOVE "Y" TO MATCH
              GO TO MATCH-CODE.

           MOVE "N" TO MATCH.
           MOVE 0 TO SUB.
       CHECK-CODE.
           ADD 1 TO SUB.
           IF COCODE(SUB) = LN-COLLCD
              MOVE "Y" TO MATCH
              GO TO MATCH-CODE.
           IF SUB < 30
              GO TO CHECK-CODE.
       MATCH-CODE.
           IF MATCH NOT = "Y"
              GO TO NEXT-LN.

       CREATE-FM-REC.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-ZIP.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

      * FM-BRNO IN KEY WILL BE SET WITH FM-PATH-OWNBR WHICH WILL BE
      * CURRENT BRANCH LOGIN IF CORPORATE "Y", OR LOCAL BRANCH IF "N"

           MOVE LN-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE FORMNO             TO LPWSFM-EDIT-FM-FORMNO.
      * CUTSHEET PRINT WONT WORK IF BRNO2 IS FILLED IN !!! IF THEY USE
      * CUSTSHEET, DO NOT ANSWER "Y" TO CORPORATE PRINT !!
           IF CORP-FM-FG = "Y"
              MOVE LN-OWNBR        TO LPWSFM-EDIT-FM-BRNO2.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FORMNO                 TO FM-FMNO.

           MOVE FM1-KEY       TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 1                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           IF IO-FG = 0
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM CLOSE-FM1-FILE.

           PERFORM CREATE-IL-REC.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-FM1-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ********************************************
       INITIALIZATION SECTION.
      ********************************************
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE " INTEREST PAID EXTRACTION IN PROGRESS..." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-IL-FILE-OUTPUT.
           PERFORM CLOSE-IL-FILE.
           PERFORM OPEN-IL-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      **********************************************
       ENTER-PARAMETERS SECTION.
      **********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO CORP-FM.

       BR-01.
           MOVE "E RA000BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BEGBR." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BEGBR." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040ENDBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BEGBR." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBR." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020BEGCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.
       CL-02.
           MOVE "ENNN020ENDCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO ACCT1.
       ALL-CL.
           MOVE "SNNN020BEGCL." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.
       ACCT1.
           MOVE "ENNN060BEGACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLACC.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACCT1.
           MOVE VDUNUM0 TO BEG-ACCT.
       ACCT2.
           MOVE "ENNN060ENDACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ACCT1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACCT2.
           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ACCT1.
           GO TO CO01.
       ALLACC.
           MOVE "SNNN060BEGACCT." TO VDU.
           MOVE 0 TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDACCT." TO VDU.
           MOVE 999999 TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.
       CO01.
           MOVE "E  A020CO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ACCT2.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO01.
           MOVE VDUBUF TO CO1-BUF.
           IF CO1-BUF = SPACES
              GO TO BEGIN.
       CO02.
           MOVE "E  A020CO2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO02.
           MOVE VDUBUF TO CO2-BUF.
           IF CO2-BUF = SPACES
              GO TO BEGIN.
       CO03.
           MOVE "E  A020CO3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO03.
           MOVE VDUBUF TO CO3-BUF.
           IF CO3-BUF = SPACES
              GO TO BEGIN.
       CO04.
           MOVE "E  A020CO4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO03.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO04.
           MOVE VDUBUF TO CO4-BUF.
           IF CO4-BUF = SPACES
              GO TO BEGIN.
       CO05.
           MOVE "E  A020CO5." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO04.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO05.
           MOVE VDUBUF TO CO5-BUF.
           IF CO5-BUF = SPACES
              GO TO BEGIN.
       CO06.
           MOVE "E  A020CO6." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO05.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO06.
           MOVE VDUBUF TO CO6-BUF.
           IF CO6-BUF = SPACES
              GO TO BEGIN.
       CO07.
           MOVE "E  A020CO7." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO06.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO07.
           MOVE VDUBUF TO CO7-BUF.
           IF CO7-BUF = SPACES
              GO TO BEGIN.
       CO08.
           MOVE "E  A020CO8." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO07.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO08.
           MOVE VDUBUF TO CO8-BUF.
           IF CO8-BUF = SPACES
              GO TO BEGIN.
       CO09.
           MOVE "E  A020CO9." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO08.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO09.
           MOVE VDUBUF TO CO9-BUF.
           IF CO9-BUF = SPACES
              GO TO BEGIN.
       CO10.
           MOVE "E  A020CO10." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO09.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO10.
           MOVE VDUBUF TO CO10-BUF.
           IF CO10-BUF = SPACES
              GO TO BEGIN.
       CO11.
           MOVE "E  A020CO11." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO10.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO11.
           MOVE VDUBUF TO CO11-BUF.
           IF CO11-BUF = SPACES
              GO TO BEGIN.
       CO12.
           MOVE "E  A020CO12." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO11.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO12.
           MOVE VDUBUF TO CO12-BUF.
           IF CO12-BUF = SPACES
              GO TO BEGIN.
       CO13.
           MOVE "E  A020CO13." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO12.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO13.
           MOVE VDUBUF TO CO13-BUF.
           IF CO13-BUF = SPACES
              GO TO BEGIN.
       CO14.
           MOVE "E  A020CO14." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO13.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO14.
           MOVE VDUBUF TO CO14-BUF.
           IF CO14-BUF = SPACES
              GO TO BEGIN.
       CO15.
           MOVE "E  A020CO15." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO14.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO15.
           MOVE VDUBUF TO CO15-BUF.
           IF CO15-BUF = SPACES
              GO TO BEGIN.
       CO16.
           MOVE "E  A020CO16." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO15.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO16.
           MOVE VDUBUF TO CO16-BUF.
           IF CO16-BUF = SPACES
              GO TO BEGIN.
       CO17.
           MOVE "E  A020CO17." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO16.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO17.
           MOVE VDUBUF TO CO17-BUF.
           IF CO17-BUF = SPACES
              GO TO BEGIN.
       CO18.
           MOVE "E  A020CO18." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO17.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO18.
           MOVE VDUBUF TO CO18-BUF.
           IF CO18-BUF = SPACES
              GO TO BEGIN.
       CO19.
           MOVE "E  A020CO19." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO18.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO19.
           MOVE VDUBUF TO CO19-BUF.
           IF CO19-BUF = SPACES
              GO TO BEGIN.
       CO20.
           MOVE "E  A020CO20." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO19.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO20.
           MOVE VDUBUF TO CO20-BUF.
           IF CO20-BUF = SPACES
              GO TO BEGIN.
       CO21.
           MOVE "E  A020CO21." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO20.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO21.
           MOVE VDUBUF TO CO21-BUF.
           IF CO21-BUF = SPACES
              GO TO BEGIN.
       CO22.
           MOVE "E  A020CO22." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO21.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO22.
           MOVE VDUBUF TO CO22-BUF.
           IF CO22-BUF = SPACES
              GO TO BEGIN.
       CO23.
           MOVE "E  A020CO23." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO22.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO23.
           MOVE VDUBUF TO CO23-BUF.
           IF CO23-BUF = SPACES
              GO TO BEGIN.
       CO24.
           MOVE "E  A020CO24." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO23.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO24.
           MOVE VDUBUF TO CO24-BUF.
           IF CO24-BUF = SPACES
              GO TO BEGIN.
       CO25.
           MOVE "E  A020CO25." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO24.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO25.
           MOVE VDUBUF TO CO25-BUF.
           IF CO25-BUF = SPACES
              GO TO BEGIN.
       CO26.
           MOVE "E  A020CO26." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO25.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO26.
           MOVE VDUBUF TO CO26-BUF.
           IF CO26-BUF = SPACES
              GO TO BEGIN.
       CO27.
           MOVE "E  A020CO27." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO26.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO27.
           MOVE VDUBUF TO CO27-BUF.
           IF CO27-BUF = SPACES
              GO TO BEGIN.
       CO28.
           MOVE "E  A020CO28." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO27.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO28.
           MOVE VDUBUF TO CO28-BUF.
           IF CO28-BUF = SPACES
              GO TO BEGIN.
       CO29.
           MOVE "E  A020CO29." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO28.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO29.
           MOVE VDUBUF TO CO29-BUF.
           IF CO29-BUF = SPACES
              GO TO BEGIN.
       CO30.
           MOVE "E  A020CO30." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO29.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CO30.
           MOVE VDUBUF TO CO30-BUF.
           IF CO30-BUF = SPACES
              GO TO BEGIN.
       BEGIN.
           MOVE "ENNN060BEGINT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CO30.
           IF VDUSTATUS = "16" GO TO ALLINT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN.
           MOVE VDUNUM0 TO BEG-INT.
           GO TO ENDIN.
       ALLINT.
           MOVE "SNNN060BEGINT." TO VDU.
           MOVE 0 TO BEG-INT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDINT." TO VDU.
           MOVE 999999 TO END-INT VDUNUM0.
           PERFORM SCREENIO.
           GO TO WHYEAR.
       ENDIN.
           MOVE "ENNN060ENDINT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEGIN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDIN.
           MOVE VDUNUM0 TO END-INT.
           IF BEG-INT > END-INT
              MOVE "INVALID INTEREST PAID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN.
       WHYEAR.
           MOVE "E  A010YEAR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENDIN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO WHYEAR.
           MOVE VDUBUF TO WHICHYEAR.
           IF WHICHYEAR = "C" GO TO PYYR.
           IF WHICHYEAR NOT = "L" GO TO WHYEAR.
       PYYR.
           MOVE "ENNN040REP-YEAR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO WHYEAR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PYYR.
           MOVE VDUNUM0 TO PAY-YEAR-BUF.
           IF PAY-YEAR-BUF < 1900
              MOVE "INVALID YEAR" TO MESS
              PERFORM SEND-MESS
              GO TO PYYR.
       FMNO.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PYYR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FMNO.
           MOVE VDUNUM0 TO FORMNO.
           PERFORM OPEN-FD1-FILE.
           MOVE FORMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO FMNO.
           PERFORM CLOSE-FD1-FILE.

       CORP-FM.
           IF BEG-BR = END-BR
              MOVE "S  A010CORPFM." TO VDU
              MOVE "N" TO CORP-FM-FG VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-PARM.
           MOVE "E  A010CORPFM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO FMNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO WHYEAR.
           MOVE VDUBUF TO CORP-FM-FG.
           IF CORP-FM-FG NOT = "Y"
              IF CORP-FM-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO CORP-FM.

       EXIT-PARM.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEGBR." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEGBR." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040ENDBR." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020BEGCL." TO VDU.
           MOVE BEG-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE END-CL TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060BEGACCT." TO VDU.
           MOVE BEG-ACCT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDACCT." TO VDU.
           MOVE END-ACCT TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020CO1." TO VDU.
           MOVE CO1-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO2." TO VDU.
           MOVE CO2-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO3." TO VDU.
           MOVE CO3-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO4." TO VDU.
           MOVE CO4-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO5." TO VDU.
           MOVE CO5-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO6." TO VDU.
           MOVE CO6-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO7." TO VDU.
           MOVE CO7-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO8." TO VDU.
           MOVE CO8-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO9." TO VDU.
           MOVE CO9-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO10." TO VDU.
           MOVE CO10-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO11." TO VDU.
           MOVE CO11-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO12." TO VDU.
           MOVE CO12-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO13." TO VDU.
           MOVE CO13-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO14." TO VDU.
           MOVE CO14-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO15." TO VDU.
           MOVE CO15-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO16." TO VDU.
           MOVE CO16-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO17." TO VDU.
           MOVE CO17-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO18." TO VDU.
           MOVE CO18-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO19." TO VDU.
           MOVE CO19-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO20." TO VDU.
           MOVE CO20-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO21." TO VDU.
           MOVE CO21-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO22." TO VDU.
           MOVE CO22-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO23." TO VDU.
           MOVE CO23-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO24." TO VDU.
           MOVE CO24-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO25." TO VDU.
           MOVE CO25-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO26." TO VDU.
           MOVE CO26-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO27." TO VDU.
           MOVE CO27-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO28." TO VDU.
           MOVE CO28-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO29." TO VDU.
           MOVE CO29-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020CO30." TO VDU.
           MOVE CO30-BUF TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN060BEGINT." TO VDU.
           MOVE BEG-INT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDINT." TO VDU.
           MOVE END-INT TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010YEAR." TO VDU.
           MOVE WHICHYEAR TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN040REP-YEAR." TO VDU.
           MOVE PAY-YEAR-BUF TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020FORMNO." TO VDU.
           MOVE FORMNO TO VDUNUM0.
           PERFORM SCREENIO.

      **************************************
       CREATE-IL-REC SECTION.
      **************************************

           ADD 1 TO IL-KEY.

           PERFORM READ-IL-FILE.

           MOVE SPACES TO IL-REC.

           MOVE "B"          TO IL-TYPE.
           MOVE PAY-YEAR-BUF TO IL-B-PAY-YEAR.
           MOVE BW-LNAME     TO WORK-FIELD.
           MOVE CHAR-4       TO IL-B-NAME-CONTROL.
           MOVE "2"          TO IL-B-TIN-TYPE-CD.
           MOVE BW-SSNO      TO IL-B-TIN.
           MOVE LN-OWNBR     TO WORK-FIELD.
           MOVE LN-CLASS     TO CHAR-4-5.
           MOVE LN-ACCTNO    TO CHAR-7-12.
           MOVE 01           TO CHAR-14-15.
           MOVE "-"          TO CHAR-3 CHAR-6 CHAR-13.
           MOVE WORK-FIELD   TO IL-B-ACCTNO.
           MOVE LN-OWNBR     TO IL-B-OFFICE-CODE.
           IF WHICHYEAR = "C"
              MOVE LN-YTDINTPD TO IL-B-PAYAMT(1)
           ELSE
              MOVE LN-LYRINTPD TO IL-B-PAYAMT(1).
           PERFORM ZERO-OTHER-AMTS.

      * NEED PRINCIPLE BALANCE AS OF JAN 1, TAX REPORTING YEAR.,
      * GOES IN IL-B-PAYAMT(6) (NEW LAW 2016 PR#5214)
           PERFORM SETUP-MORTGAGE.

           IF BW-STATE > "ZZ"
              MOVE "1" TO IL-B-FOREIGN-CD.
      * CHANGED THIS FROM BW-NAME TO A WORKING STORAGE FIELD SO IT
      * CONTINUES TO MATCH A15. WE CHANGED NAME FIELDS FROM 16 TO 35
           MOVE BW-LNAME TO PAYEE-LNAME.
           MOVE BW-FNAME TO PAYEE-FNAME.
           MOVE PAYEE-NAME TO IL-B-PAYEE-NAME.

           MOVE BW-ADR1 TO WORK-FIELD.
           MOVE BW-ADR2 TO WORK-FIELD-2.
           MOVE 40 TO SUB.
           MOVE 1 TO SUB2.
           PERFORM MERGE-IT.
           MOVE WORK-FIELD TO IL-B-PAYEE-ADDR.
           MOVE BW-CITY TO IL-B-PAYEE-CITY.
           MOVE BW-STATE TO IL-B-PAYEE-STATE.
           MOVE BW-ZIP TO ZIP-WORK.
      *    -------------------------------------------------------------
      *    IF ZIP IS IN XXXXX-XXXX FORMAT THEN SHIFT EXT TO PUT
      *       INTO XXXXXXXXXX FORMAT
      *    -------------------------------------------------------------
           IF PART-3-DASH = "-"
              MOVE PART-2 TO PART-2-A
              MOVE SPACE TO ZIP-JUNK.

           MOVE ZIP-WORK TO IL-B-PAYEE-ZIP.

      *
      * IF MORE THAN ONE PROPERTY SECURING THE MORTGAGE, ENTER THE TOTAL
      * NUMBER OF PROPERTIES SECURED BY THIS MORTGAGE. IF LESS THAN 2, 
      * ENTER BLANKS. VALID VALUES ARE 0000 - 9999.
           MOVE 0 TO IL-B-NO-MORTGAGED-PROP.

           MOVE CARRAGE-RET TO IL-CARR-RTN.
           PERFORM WRITE-IL-FILE.

      ******************************************
       FIND-NO-MORTGAGED-PROPERTIES.
      ******************************************
      * MUST TEST FOR "4 ", "04", " 4"
           MOVE 0 TO NUMERIC-DWELLCD.
           MOVE BW-DWELLCD TO TEST-DWELLCD.
           IF TEST-DWELL-1 NUMERIC
              IF TEST-DWELL-2 NOT NUMERIC
                 MOVE TEST-DWELL-1 TO NUMERIC-DWELLCD
              ELSE
                 MOVE TEST-DWELLCD TO NUMERIC-DWELLCD
           ELSE
              IF TEST-DWELL-2 NUMERIC
                 MOVE TEST-DWELL-2 TO NUMERIC-DWELLCD.


      ******************************************
       MERGE-IT SECTION.
      ******************************************
       MERGE-LOOP.
           IF WORK-CHAR(SUB) = " " AND SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO MERGE-LOOP.
           IF SUB = 1
              MOVE WORK-FIELD-2 TO WORK-FIELD
              GO TO EXIT-MERGE-IT.
           ADD 2 TO SUB.
           IF SUB > 39
              GO TO EXIT-MERGE-IT.

       MERGE-THEM.
           MOVE WORK-CHAR-2(SUB2) TO WORK-CHAR(SUB).
           IF SUB < 40
              ADD 1 TO SUB SUB2
              GO TO MERGE-THEM.

       EXIT-MERGE-IT.
           EXIT.

      * MUST FIND PRINCIPLE BALANCE AS OF JAN. 1, TAX REPORTING YEAR
      * READ BACKWARDS UNTIL NO LONGER IN TAX REPORTING YEAR.
      *****************************************
       SETUP-MORTGAGE SECTION.
      *****************************************

      * ENTER THE DATE OF THE MORTGAGE ORIGINATION IN YYYYMMDD FORMAT
           MOVE LN-LOANDATE TO IL-B-LOANDATE-CCYYMMDD.

      * ENTER "1" IF PROPERTY SECURING MORTGAGE(ALT ADDRESS) IS THE SAME
      * AS BORROWER ADDRESS, OTHERWISE ENTER BLANK
           MOVE LN-SSNO(1) TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-FG = 0
              IF BX-ALT-ADR1 = SPACES AND
                  BX-ALT-ADR2 = SPACES AND
                   BX-ALT-CITY = SPACES
                 MOVE "1" TO IL-B-PROPERTY-SECURING
              ELSE
                 STRING BX-ALT-ADR1," ",BX-ALT-ADR2," ",BX-ALT-CITY,", ",
                   BX-ALT-STATE," ",BX-ALT-ZIP DELIMITED BY "  " INTO
                       IL-B-PROPERTY-ADDRESS
           ELSE
              MOVE "1" TO IL-B-PROPERTY-SECURING.
                

           MOVE 0 TO IL-B-PAYAMT(6).

      * SETUP DATE AS JAN 01, TAX YEAR REPORTING
      * PAY-YEAR-BUF IS A 4 DIGIT YEAR ENTERED ON THE SCREEN
           MOVE PAY-YEAR-BUF     TO DATE-YYYYMMDD-YYYY.

           MOVE 01               TO DATE-YYYYMMDD-MM
                                    DATE-YYYYMMDD-DD.
           MOVE DATE-YYYYMMDD    TO TEST-DATE.

      * FOR MORTGAGES MADE DURING THE TAX YEAR, SEND ZERO
           MOVE LN-LOANDATE TO NUM-DATE.
           IF NUM-YR = DATE-YYYYMMDD-YY
              GO TO EXIT-SETUP-MORTGAGE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       FIND-PRINCIPLE-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0
              GO TO EXIT-SETUP-MORTGAGE.

           IF LP-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-SETUP-MORTGAGE.

      * EXAMPLE: RUNNING FOR 2016 IN JANUARY OF 2017, THEY WOULD HAVE
      *          ENTERED 2016 IN PAY-YEAR-BUF, I NEED THE LAST PAYMENT PRIOR
      *          TO OR EQUAL TO 01/01/16 TO GET THE PRIN BALANCE
      *          TEST-DATE = 010116 IN THIS EXAMPLE

           IF LP-PAYDATE NOT > TEST-DATE
              MOVE LP-CURBAL TO IL-B-PAYAMT(6)
           ELSE
              GO TO FIND-PRINCIPLE-NEXT.

       EXIT-SETUP-MORTGAGE.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO IR1098-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IR1098-SYSDATE.
           DISPLAY IR1098-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IR1098-FORM-FIELDS TO WR-BUF.
           MOVE IR1098-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE IR1098-HELP--FILE TO HELP-LINK-FILE.
           MOVE IR1098-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/IR1098_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       ZERO-OTHER-AMTS.
           MOVE 0 TO IL-B-PAYAMT(2) IL-B-PAYAMT(3) IL-B-PAYAMT(4)
                     IL-B-PAYAMT(5) IL-B-PAYAMT(6) IL-B-PAYAMT(7)
                     IL-B-PAYAMT(8) IL-B-PAYAMT(9) IL-B-PAYAMT(10)
                     IL-B-PAYAMT(11) IL-B-PAYAMT(12) IL-B-PAYAMT(13)
                     IL-B-PAYAMT(14) IL-B-PAYAMT(15) IL-B-PAYAMT(16)
                     IL-B-PAYAMT(17) IL-B-PAYAMT(18).

      ****************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      ****************************************
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBSP/SPILI.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/IRMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IR1098-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
