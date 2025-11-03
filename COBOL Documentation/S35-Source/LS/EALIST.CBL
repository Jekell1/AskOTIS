       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EALIST.
       DATE-WRITTEN. 1/27/84
      ******************************************************************
      *
      *                      (LP)
      *  DESCRIPTION:  EXTRACTION/ACTION FILE LISTING
      *
      * REV:
      *  KEC 020296 ADDED EA-CQ-FG TO DETAIL-LINE.
      *  KEC 031596 ADDED EA-GROUP-ID TO DETAIL-LINE. ALSO ADDED RANGE
      *             FOR GROUP ID.
      *  MJD 051096 MODIFIED DETAIL LINE TO ADD DEALER NUMBER AND
      *             CONTRACTUAL DAYS.  ADDED TEST OF EA-CQ-FG TO SKIP
      *             RECORDS WITH EA-CQ-FG NOT = "N".
      *  MJD 082996 ADDED EA-PL, EA-POTS, EA-BANKRUPT, AND EA-REPO TO
      *             DETAIL-LINE. REMOVED "QUEUE : " FROM DETAIL-LINE.
      *  BLV 051297 ADD CD-FILE TO DECLARATIVES, CLOSE-FILES
      *  GLF 030298 ADDED EA-JUDGEMENT AND EA-DEBTAID
      *  BLV 120600 RENAMED EA-POTS TO EA-CPOTS, WORLD PR#244
      *  BLV 010102 INITIALIZED  CD-SCAN-GQ-FG TO SPACES, TFC PR# 60
      *   CS 010402 ADDED PRINT OF INVALID ITEMS, GROUP, FORM NUMBERS,
      *             COLLECTOR IDS, POOL IDS, AND INVALID POOL COLLECTOR ID'S
      *             AND ADDED OPTION TO EXTRACT ONLY INVALID EXTRACTIONS
      *   CS 010417 MOVED VERIFY OF EA-FIELDS. HEADINGS PRINTED FOR EACH
      *             BREAK ON CLASS OR BRANCH. WASTING PAPER IF PRINTING
      *             INVALID RECORDS ONLY
      *   CS 010507 ADDED EA-ALLOTMENTS
      *  BLV 011012 ADDED BEG, END DAYS SINCE LOAN DATE, DAYS UNTIL
      *             MATURITY, REGWAN PR# 1587
      *  BLV 021030 ADDED BEG, END DAY OF THE MONTH FOR DUE DATE,
      *             REGACC PR# 0009
      *  BAH 110922 REPLACED RRFORM WITH SCREEN
      *  BLM 130423 ADDED EA-BORR-STATE, SECURITY AUTO, PR# 4248
      *  MJD 131210 A30 CONVERSION COMPLETED
      *
      * KEC 2016.0301 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *         FIXING EA-ALPNAME1&2 BUT SKIPPED AT THIS TIME
      *         FIXING D-LNAME IN THE REDEFINE D-LINE. THE REASON
      *         IS THAT D-LINE IS 135 BYTES NOT 132 SO NOT SURE
      *         WHERE TO GET A BYTE TO EXPAND D-LNAME.  THIS
      *         NEEDS MORE TIME TO FIGURE OUT.
      *
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED CD-EA-FILLER FOR CHANGE TO CD-EA-REC.
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BAH 170427 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34; 
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LS/EALIST S35 03/04/24-13:13:02 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LP01EA_SQL.CPY".
       COPY "LIBLP/LPWSEA.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".


      ******************************************************************
      *
      *    L I N E   W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *    M I S C   W O R K E R S
      *
      ******************************************************************
       01  SUB                  PIC 99.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  FORM-1               PIC 9(4)     VALUE ZEROS.
       01  FORM-2               PIC 9(4)     VALUE ZEROS.
       01  FORM-3               PIC 9(4)     VALUE ZEROS.
       01  FORM-4               PIC 9(4)     VALUE ZEROS.
       01  GLOBAL-QUEUING-DONE  PIC X        VALUE SPACES.
       01  HOLD-BRANCH          PIC 9(4)     VALUE ZEROS.
       01  WS-GROUP-ID-TYPE     PIC X     VALUE SPACES.
       01  SUB-COL-ID-ON-LINE   PIC 9(2)    VALUE ZEROS.
      * ALPLNAME WAS X(3) IN A15, X(4) IN S35, CANT FIND ROOM ON DETAIL-LINE
       01  HOLD-LNAME           PIC X(4).
       01  HOLD-LNAMEX REDEFINES HOLD-LNAME.
           03  FILLER           PIC X.
           03  HOLD-LNAME-A15   PIC X(3). 

       01  VERIFY-EA-WORKERS.
           03  VERIFY-EA-GROUP-ID-STAT     PIC X.
               88 VERIFY-EA-GROUP-ID-BAD   VALUE "X".
           03  VERIFY-EA-GROUP-QUEUE-MATCH     PIC X.
               88 VERIFY-EA-GROUP-QUEUE-BAD   VALUE "X".

           03  VERIFY-EA-FORMS-STAT.
               05  VERIFY-EA-FORM1-STAT        PIC X.
                   88 VERIFY-EA-FORM1-BAD      VALUE "X".

               05  VERIFY-EA-FORM2-STAT        PIC X.
                   88 VERIFY-EA-FORM2-BAD      VALUE "X".

               05  VERIFY-EA-FORM3-STAT        PIC X.
                   88 VERIFY-EA-FORM3-BAD      VALUE "X".

               05  VERIFY-EA-FORM4-STAT        PIC X.
                   88 VERIFY-EA-FORM4-BAD      VALUE "X".

           03  VERIFY-EA-COLLECTOR-STAT       PIC X.
               88 VERIFY-EA-COLLECTOR-BAD     VALUE "X".

           03  VERIFY-EA-POOL-STAT            PIC X.
               88 VERIFY-EA-POOL-BAD          VALUE "X".

       01  VERIFY-EA-POOL-COLLECTORS-STAT  PIC X.
               88  VERIFY-EA-POOL-COLLECTORS-BAD VALUE "X".


      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       01  SCREEN-BUF           VALUE SPACES.
           03  ERRCD            PIC X.
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 99.
           03  END-CLASS        PIC 99.
           03  BEG-GROUP-ID     PIC X(6).
           03  END-GROUP-ID     PIC X(6).
           03  INVALID-ONLY-FG  PIC X.

       01  ACCEPT-BUF            PIC X     VALUE " ".

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE SPACES.
           03  FILLER           PIC XX       VALUE SPACES.
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(30)    VALUE SPACES.
           03  H-NAME           PIC X(40)    VALUE SPACES.
           03  FILLER           PIC X(39)    VALUE SPACES.
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZ9.

       
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(24)    VALUE SPACES.
           03  FILLER           PIC X(41)    VALUE
           "COLLECTION EXTRACTION/ACTION FILE LISTING".
           03  FILLER           PIC X(47)    VALUE SPACES.

       
       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH :".
           03  H-BRANCH         PIC Z(4)     VALUE SPACES.
           03  FILLER           PIC X        VALUE SPACES.
           03  H-BR-NAME        PIC X(20)    VALUE SPACES.

       
       01  HEAD4.
           03  FILLER           PIC X(10)    VALUE "CLASS  :".
           03  H-CLASS          PIC ZZ       VALUE SPACES.
           03  FILLER           PIC X        VALUE SPACES.
           03  H-LC-DESC        PIC X(20)    VALUE SPACES.

       
       01  HEAD5.
           03  FILLER           PIC X(6)     VALUE SPACES.
           03  FILLER           PIC X(39)    VALUE
               " **************************************".
           03  FILLER           PIC X(19)    VALUE
               " EXTRACTION CODES  ".
           03  FILLER           PIC X(42)    VALUE
               "******************************************".
           03  FILLER           PIC X(1)     VALUE SPACES.
           03  FILLER           PIC X(25)    VALUE
               "  **** ACTION CODES **** ".

       
       01  HEAD6.
           03  FILLER           PIC X(22)    VALUE
               "GROUP  PDUE    PDUE   ".
           03  FILLER           PIC X(26)    VALUE
               "ACCOUNT --LAST FORM-- PAST".
           03  FILLER           PIC X(37)    VALUE
               "  RECY LAST CON DEALER  LOAN MATU DUE".

           03  FILLER           PIC X(36)    VALUE
               " ACT POT MAT LGL PL  CNF  --SEND--  ".

           03  FILLER           PIC X(12)    VALUE
               " ACTN  ASSN".

       
       01  HEAD7.
           03  FILLER           PIC X(22)    VALUE
               "SEQ    DAYS    AMTS   ".
           03  FILLER           PIC X(26)    VALUE
               "BALANCE DATE       NO PROM".
           03  FILLER           PIC X(37)    VALUE
               "  DAYS NAME DAY         DAYS DAYS DAY".
           03  FILLER           PIC X(36)    VALUE
               " ORG RPT PRM DA  PLO 1ST FORMS      ".
           03  FILLER           PIC X(12)    VALUE
               " INC    COL".
       
       01  HEAD8.
           03  FILLER           PIC X(22)    VALUE SPACES.
           03  FILLER           PIC X(26)    VALUE SPACES.
           03  FILLER           PIC X(34)    VALUE SPACES.
           03  FILLER           PIC X(39)    VALUE
               " ST REC MIS REP BK  JD  ALL            ".
           03  FILLER           PIC X(15)    VALUE
               " PROM         ".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
       
           03  D-SEQNO-GROUPID                PIC X(6).
           03  D-SEQNO-GROUPID-RDF1           REDEFINES D-SEQNO-GROUPID.
               05  FILLER                     PIC X(1).
               05  D-SEQNO                    PIC ZZ.
               05  FILLER                     PIC X(3).
           03  D-SEQNO-GROUPID-RDF1           REDEFINES D-SEQNO-GROUPID.
               05  D-GROUP-ID                 PIC X(6).
       
           03  D-GROUP-ID-TYPE                PIC X(1).
           03  D-PASTDUE-DAY                  PIC ZZZ9.
           03  FILLER                         PIC X(1).
           03  D-PASTDUE-AMT                  PIC ZZZ9.99.
           03  FILLER                         PIC X.
           03  D-ACCT-BAL                     PIC ZZZZZ9.99.
           03  FILLER                         PIC X.
           03  D-LSTFM-DATE                   PIC 99/99/99.
           03  FILLER                         PIC X.
           03  D-LSTFM-NO                     PIC ZZZ9.
           03  FILLER                         PIC X(1).
           03  D-PASTPROM-DAY                 PIC ZZZ9-.
           03  FILLER                         PIC X.
           03  D-RECENCY-DAY                  PIC ZZZ9.
           03  FILLER                         PIC X.
           03  D-LNAME                        PIC XXX.
           03  FILLER                         PIC X.
           03  D-CONTR-DAY                    PIC ZZZ9.
           03  FILLER                         PIC X.
           03  D-DEALER                       PIC ZZZ9.99.
           03  FILLER                         PIC X.
           03  D-LNDATE-DAYS                  PIC ZZZ9.
           03  FILLER                         PIC X.
           03  D-MATURE-DAYS                  PIC ZZZ9.
           03  FILLER                         PIC X.
           03  D-DAY-OF-MO-DUE                PIC Z9.
           03  D-STATE REDEFINES D-DAY-OF-MO-DUE
                                              PIC XX.
           03  FILLER                         PIC X.
           03  D-ACTION-ORGN                  PIC X(4).
           03  D-ACTIONX REDEFINES D-ACTION-ORGN.
               05 FILLER                      PIC X(3).
               05 D-LNRECNY                   PIC X.
           03  FILLER                         PIC X(2).
           03  D-POTS-MPAY                    PIC X.
           03  FILLER                         PIC X(3).
           03  D-MATD-PROM-REPO               PIC X.
           03  FILLER                         PIC X(2).
           03  D-LEGAL-DA-BK                  PIC X.
           03  FILLER                         PIC X(3).
           03  D-PL-PLO-JD                    PIC X.
           03  FILLER                         PIC X(2).
           03  D-CNF-1STPY-ALLOT              PIC X.
           03  FILLER                         PIC X(1).

           03  D-SETFM-NO1                    PIC ZZZ9.
           03  D-FORM-TYPE1                   PIC X(3).
           03  FILLER                         PIC X(1).
           03  D-SETFM-NO2                    PIC ZZZ9.
           03  D-FORM-TYPE2                   PIC X(3).
           03  FILLER                         PIC X(1).
       
           03  D-CODES-QUEUE                  PIC X(12).
           03  D-CODES-QUEUE1       REDEFINES D-CODES-QUEUE.
               05  D-SETACT-CD                PIC X(4).
               05  D-SETACT-CDX REDEFINES D-SETACT-CD.
                   07 D-INC-PROMISED          PIC ZZZ9.
               05  D-INC-CNT REDEFINES D-SETACT-CD
                                              PIC X(4).
               05  FILLER                     PIC X.
               05  D-SET-COL                  PIC XXX.
       
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER                        PIC X(7).
           03  D-DESCRIPTION                 PIC X(6).
           03  D-DESC                        PIC X(52).
           03  FILLER                        PIC X(67).


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01 EALIST-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/EALIST_DEF.CPY".
       COPY "LIBLS/EALIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/EALIST_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EALIST" TO VDU-FORM-NAME.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS EALIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE FORM-PATHNAME TO FORM-PATH.
           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = " " )
              GO TO END-PROGRAM.

       NEXT-EA-RECORD.
           PERFORM READ-EA1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO END-ROUTINE.

           IF EA-CQ-FG NOT = "N"
              GO TO NEXT-EA-RECORD.

           IF ( EA-BRANCH > END-BRANCH )
              GO TO END-ROUTINE.

           IF ( EA-CLASS < BEG-CLASS )
              GO TO NEXT-EA-RECORD.

           IF ( EA-CLASS > END-CLASS )
              GO TO NEXT-EA-RECORD.

           IF ( EA-GROUP-ID < BEG-GROUP-ID )
              GO TO NEXT-EA-RECORD.

           IF ( EA-GROUP-ID > END-GROUP-ID )
              GO TO NEXT-EA-RECORD.

           IF EA-BRANCH NOT = HOLD-BRANCH
              MOVE EA-BRANCH TO HOLD-BRANCH BR-NO
              PERFORM GET-BRANCH
              IF IO-BAD GO TO IO-ERROR.

      D    STOP MESS. 
           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-EA-RECORD.

       END-ROUTINE.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO RECORDS-FOUND.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE " " TO ERRCD.

       SETRP.
           MOVE EALIST-QUEUE-POS TO Q-POS.
           MOVE EALIST-COPIES-POS TO C-POS.
           MOVE EALIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

       BEGBRN.
           MOVE "ENNN040BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALLBR.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGBRN.
           MOVE VDUNUM0 TO BEG-BRANCH.

       
       ENDBRN.
           MOVE "ENNN040ENDBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO BEGBRN.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDBRN.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGBRN.

           GO TO BEGCLS.

       
       ALLBR.
           MOVE "SNNN040BEGBR." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBR." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       
       BEGCLS.
           MOVE "ENNN020BEGCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO ENDBRN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGCLS.
           MOVE VDUNUM0 TO BEG-CLASS.

       
       ENDCLS.
           MOVE "ENNN020ENDCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO BEGCLS.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDCLS.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGCLS.

           GO TO BEGIN-GROUP-ID.

       
       ALLCLS.
           MOVE "SNNN020BEGCL." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       
       BEGIN-GROUP-ID.
      D    STOP MESS.
           MOVE "E  A060BEG-GROUP-ID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO ENDCLS.
           IF VDUSTATUS = "16" GO TO ALL-GROUPS.
           IF ( VDUSTATUS = "1<" )
              PERFORM CODE-SCAN
              IF ( IO-FG = 9 )
                 GO TO BEGIN-GROUP-ID
              ELSE
                 MOVE "S  A060BEG-GROUP-ID." TO VDU
                 MOVE CDSCAN-CODE TO BEG-GROUP-ID VDUBUF
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-GROUP-ID.
           MOVE VDUBUF TO BEG-GROUP-ID.
           IF ( BEG-GROUP-ID = SPACES ) GO TO BEGIN-GROUP-ID.

           MOVE "EA"         TO CD-TYPE.
           MOVE BEG-GROUP-ID TO CD-CODE.
           PERFORM GET-CODE-RECORD.
           IF ( IO-FG NOT = 0 )
              GO TO BEGIN-GROUP-ID.

       
       ENDING-GROUP-ID.
           MOVE "E  A060END-GROUP-ID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO BEGIN-GROUP-ID.
           IF ( VDUSTATUS = "1<" )
              PERFORM CODE-SCAN
              IF ( IO-FG = 9 )
                 GO TO ENDING-GROUP-ID
              ELSE
                 MOVE "S  A060END-GROUP-ID." TO VDU
                 MOVE CDSCAN-CODE TO END-GROUP-ID VDUBUF
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-GROUP-ID.
           MOVE VDUBUF TO END-GROUP-ID.

           IF ( END-GROUP-ID = SPACES ) GO TO ENDING-GROUP-ID.

           MOVE "EA"         TO CD-TYPE.
           MOVE END-GROUP-ID TO CD-CODE.
           PERFORM GET-CODE-RECORD.
           IF ( IO-FG NOT = 0 )
              GO TO ENDING-GROUP-ID.

           IF ( BEG-GROUP-ID > END-GROUP-ID )
              MOVE "INVALID GROUP ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-GROUP-ID.

           GO TO INVALID-ONLY.

       
       ALL-GROUPS.
           MOVE "S  A060BEG-GROUP-ID." TO VDU.
           MOVE SPACES TO BEG-GROUP-ID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A060END-GROUP-ID." TO VDU.
           MOVE "ZZZZZZ" TO END-GROUP-ID VDUBUF.
           PERFORM SCREENIO.

       
       INVALID-ONLY.
           MOVE "E  A010INVALIDONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO ENDING-GROUP-ID.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO INVALID-ONLY.
           MOVE VDUBUF TO INVALID-ONLY-FG.

           IF INVALID-ONLY-FG NOT = "Y"
              IF INVALID-ONLY-FG NOT = "N"
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO INVALID-ONLY.

       
       RRSEL.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRSEL.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO BEGBRN.
           IF ACCEPT-BUF NOT = "Y" GO TO BEGBRN.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST                                 TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-EA1-FILE.

           MOVE BEG-BRANCH   TO EA-BRANCH
                                QEA1-WBEG-BRANCH
                                QEA1-WEND-BRANCH.
           MOVE BEG-CLASS    TO EA-CLASS
                                QEA1-WBEG-CLASS.
           MOVE END-CLASS    TO QEA1-WEND-CLASS.
           MOVE BEG-GROUP-ID TO EA-GROUP-ID
                                QEA1-WBEG-GROUP-ID.
           MOVE END-GROUP-ID TO QEA1-WEND-GROUP-ID.
           MOVE 0            TO EA-SEQNO
                                QEA1-WBEG-SEQNO.
           MOVE ALL "9"      TO QEA1-WEND-SEQNO.

           PERFORM START-EA1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           IF FIRST-TIME = "Y"
              MOVE EA-BRANCH TO BR-NO
              MOVE EA-CLASS TO LC-CODE.

           PERFORM VERIFY-EA-FIELDS.

           IF VERIFY-EA-WORKERS =  SPACES
              IF VERIFY-EA-POOL-COLLECTORS-STAT =  SPACES
                 IF INVALID-ONLY-FG  = "Y"
                 GO TO EXIT-DETAIL-LN-PROCESS.

           IF EA-BRANCH NOT = BR-NO
              IF FIRST-TIME NOT = "Y"
                 MOVE EA-BRANCH TO BR-NO
                 MOVE EA-CLASS TO LC-CODE
                 PERFORM HEAD-PAGE.

           IF EA-CLASS NOT = LC-CODE
              IF FIRST-TIME NOT = "Y"
                 MOVE EA-CLASS TO LC-CODE
                 PERFORM HEAD-PAGE.

           MOVE SPACES              TO DETAIL-LINE.

           MOVE EA-GROUP-ID         TO D-GROUP-ID.
           MOVE WS-GROUP-ID-TYPE    TO D-GROUP-ID-TYPE.
           MOVE EA-PASTDUE-DAY1     TO D-PASTDUE-DAY.
           MOVE EA-PASTDUE-AMT1     TO D-PASTDUE-AMT.
           MOVE EA-ACCT-BAL1        TO D-ACCT-BAL.
           MOVE EA-LSTFM-DATE1      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-LSTFM-DATE.
           MOVE EA-LSTFM-NO1        TO D-LSTFM-NO.
           MOVE EA-PASTPROM-DAY1    TO D-PASTPROM-DAY.
           MOVE EA-RECENCY-DAY1     TO D-RECENCY-DAY.
      * EA-ALPNAME1 IS "   A", WE INCREASED TO 4, BUT NO ROOM ON DETAIL
      *    MOVE EA-ALPNAME1         TO D-LNAME.
           MOVE EA-ALPNAME1         TO HOLD-LNAME.
           MOVE HOLD-LNAME-A15      TO D-LNAME.
           MOVE EA-CONTRACTUAL-DAY1 TO D-CONTR-DAY.
           MOVE EA-DLNO1            TO D-DEALER.
           MOVE EA-LNDATE-DAYS1     TO D-LNDATE-DAYS.
           MOVE EA-MATURE-DAYS1 TO D-MATURE-DAYS.
           MOVE EA-DAY-OF-MO-DUE1 TO D-DAY-OF-MO-DUE.

           MOVE EA-ACTION          TO D-ACTION-ORGN.
           MOVE EA-CPOTS           TO D-POTS-MPAY.
           MOVE EA-MATURED         TO D-MATD-PROM-REPO.
           MOVE EA-LEGAL           TO D-LEGAL-DA-BK.
           MOVE EA-PL              TO D-PL-PLO-JD.
           MOVE EA-CONFIDENTIAL-FG TO D-CNF-1STPY-ALLOT.

           MOVE EA-SETFM-NO        TO D-SETFM-NO1.
           MOVE "(1)"              TO D-FORM-TYPE1.
           MOVE EA-SETFM(2)        TO D-SETFM-NO2.
           MOVE "(3)"              TO D-FORM-TYPE2.
           MOVE EA-SETACT-CD       TO D-SETACT-CD.
           MOVE EA-SET-LN-COL      TO D-SET-COL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           MOVE SPACES              TO DETAIL-LINE.
           MOVE EA-SEQNO            TO D-SEQNO.
           MOVE EA-PASTDUE-DAY2     TO D-PASTDUE-DAY.
           MOVE EA-PASTDUE-AMT2     TO D-PASTDUE-AMT.
           MOVE EA-ACCT-BAL2        TO D-ACCT-BAL.
           MOVE EA-LSTFM-DATE2      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-LSTFM-DATE.
           MOVE EA-LSTFM-NO2        TO D-LSTFM-NO.
           MOVE EA-PASTPROM-DAY2    TO D-PASTPROM-DAY.
           MOVE EA-RECENCY-DAY2     TO D-RECENCY-DAY.
           MOVE EA-CONTRACTUAL-DAY2 TO D-CONTR-DAY.
           MOVE EA-DLNO2            TO D-DEALER.
           MOVE EA-LNDATE-DAYS2     TO D-LNDATE-DAYS.
           MOVE EA-MATURE-DAYS2     TO D-MATURE-DAYS.
           MOVE EA-DAY-OF-MO-DUE2   TO D-DAY-OF-MO-DUE.
      * EA-ALPNAME2 WE INCREASED TO 4, BUT NO ROOM ON DETAIL
      *    MOVE EA-ALPNAME2         TO D-LNAME.
           MOVE EA-ALPNAME2         TO HOLD-LNAME.
           MOVE HOLD-LNAME-A15      TO D-LNAME.

           MOVE EA-ORIGIN           TO D-ACTION-ORGN.
           MOVE EA-RPOTS            TO D-POTS-MPAY.
           MOVE EA-PROMISED         TO D-MATD-PROM-REPO.
           MOVE EA-DEBTAID          TO D-LEGAL-DA-BK.
           MOVE EA-PL-IN-ACTIVE-BOTH TO D-PL-PLO-JD.
           MOVE EA-1ST-PMT-MISSES   TO D-CNF-1STPY-ALLOT.
           MOVE EA-SETFM(1)         TO D-SETFM-NO1.
           MOVE "(2)"               TO D-FORM-TYPE1.
           MOVE EA-SETFM(3)         TO D-SETFM-NO2.
           MOVE "(4)"               TO D-FORM-TYPE2.
           MOVE EA-INC-CNT          TO D-INC-CNT.

           IF EA-SET-LN-COL = "P"
              MOVE EA-COL-POOL      TO D-SET-COL
           ELSE
              MOVE EA-LN-COL-ID     TO D-SET-COL.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
       
           MOVE SPACES TO DETAIL-LINE.
           MOVE "DESC: " TO D-DESCRIPTION.
           MOVE EA-DESC TO D-DESC.

           MOVE EA-BORR-STATE  TO D-STATE.
           MOVE EA-RECENCYDATE TO D-LNRECNY.
           MOVE EA-MISPAYS     TO D-POTS-MPAY.
           MOVE EA-REPO        TO D-MATD-PROM-REPO.
           MOVE EA-BANKRUPT    TO D-LEGAL-DA-BK.
           MOVE EA-JUDGEMENT   TO D-PL-PLO-JD.
           MOVE EA-ALLOTMENTS  TO D-CNF-1STPY-ALLOT
           MOVE EA-INC-PROMISED TO D-INC-PROMISED.
           PERFORM WRITE-DETAIL-LINE.

           IF VERIFY-EA-WORKERS = SPACES
               IF VERIFY-EA-POOL-COLLECTORS-STAT = SPACES
                   GO TO END-DETAIL-LN-PROCESS.

       
       CONTINUE-INVALID-GROUP-ID.
           IF VERIFY-EA-WORKERS NOT = SPACES
              MOVE "[ INVALID ]" TO DETAIL-LINE
           ELSE
              GO TO CONTINUE-INVALID-POOL-COLL.

           IF VERIFY-EA-GROUP-ID-BAD
              STRING DETAIL-LINE, " ", "GROUP ID:", " ", EA-GROUP-ID,
                   DELIMITED BY "  " INTO DETAIL-LINE.

           IF VERIFY-EA-GROUP-QUEUE-BAD
              STRING DETAIL-LINE,
               " ", "GLOBAL QUEUE TYPE MISMATCH: " DELIMITED BY "  "
                 INTO DETAIL-LINE.

      * INVALID GROUP MISMATCH

           IF VERIFY-EA-GROUP-ID-BAD
               OR VERIFY-EA-GROUP-QUEUE-BAD
               STRING DETAIL-LINE,
                " [ NO FURTHER VERIFICATION DONE ]"
                 DELIMITED BY "  " INTO DETAIL-LINE
                 GO TO PRINT-INVALID-LINE1.

           IF VERIFY-EA-FORMS-STAT NOT = SPACES
              STRING DETAIL-LINE, " FORM: " DELIMITED BY "  "
                  INTO DETAIL-LINE
           ELSE
              GO TO CONTINUE-INVALID-COLLECTOR.

           IF VERIFY-EA-FORM1-BAD
               STRING DETAIL-LINE," ", "(1)", " ", FORM-1  DELIMITED BY
               "  " INTO DETAIL-LINE.

           IF VERIFY-EA-FORM2-BAD
               STRING DETAIL-LINE," ", "(2)", " ", FORM-2 DELIMITED BY
               "  " INTO DETAIL-LINE.

           IF VERIFY-EA-FORM3-BAD
               STRING DETAIL-LINE," ", "(3)"," ", FORM-3 DELIMITED BY
               "  " INTO DETAIL-LINE.

           IF VERIFY-EA-FORM4-BAD
              STRING DETAIL-LINE," ", "(4)"," " FORM-4 DELIMITED BY
               "  " INTO DETAIL-LINE.

       CONTINUE-INVALID-COLLECTOR.
           IF VERIFY-EA-COLLECTOR-BAD
               STRING DETAIL-LINE, " ", "COLLECTOR:", " ", EA-LN-COL-ID
                DELIMITED BY "  " INTO DETAIL-LINE.

           IF VERIFY-EA-POOL-BAD
                STRING DETAIL-LINE, " ", "POOL-ID:", " ", EA-COL-POOL
                 DELIMITED BY "  " INTO DETAIL-LINE.

       PRINT-INVALID-LINE1.

      * PRINT 1ST LINE OF INVALID EA-FIELDS

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE .


      * IF NO INVALID POOL COLLECTOR SKIP CHECKING POOL COLLECTORS

           IF VERIFY-EA-POOL-COLLECTORS-STAT = SPACES
              GO TO END-DETAIL-LN-PROCESS.

       CONTINUE-INVALID-POOL-COLL.

      * INVALID POOL COLLECTORS ARE ON SEPARATE NEW DETAIL LINE 

           MOVE ZEROS TO SUB-COL-ID-ON-LINE.

           MOVE "CN"         TO CDB-TYPE.
           IF GLOBAL-QUEUING-DONE  = "Y"
              MOVE 0 TO CDB-BRANCH
           ELSE
              MOVE EA-BRANCH TO CDB-BRANCH.

           MOVE "[ INVALID POOL COLLECTORS ]" TO DETAIL-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 80
              IF CP-COL-ID(SUB) = SPACES
                 MOVE 80 TO SUB
              ELSE
                 MOVE CP-COL-ID(SUB)  TO CDB-CODE
                 PERFORM GET-CDB-RECORD
                 IF IO-FG NOT = 0
                    INSPECT CP-COL-ID(SUB) REPLACING ALL " " BY "~"
                    STRING DETAIL-LINE, " ", CP-COL-ID(SUB)
                    DELIMITED BY "  " INTO DETAIL-LINE
                    ADD 1 TO SUB-COL-ID-ON-LINE
                    INSPECT CP-COL-ID(SUB) REPLACING ALL "~" BY " "
                 END-IF

                 IF SUB-COL-ID-ON-LINE GREATER THAN 25
                    INSPECT DETAIL-LINE REPLACING ALL "~" BY " "
                    PERFORM WRITE-DETAIL-LINE
                    MOVE ZEROS TO SUB-COL-ID-ON-LINE
                    MOVE "[ ******* M O R E ******* ]" TO DETAIL-LINE
                 END-IF
              END-IF
           END-PERFORM.

           INSPECT DETAIL-LINE REPLACING ALL "~" BY " ".
           PERFORM WRITE-DETAIL-LINE.

       
       END-DETAIL-LN-PROCESS.
           MOVE 2 TO LN-FEED.

       EXIT-DETAIL-LN-PROCESS.
           EXIT.


      *************************************************************
       VERIFY-EA-FIELDS SECTION.
      *************************************************************

      * VERIFY EA GROUP ID

           MOVE SPACES TO VERIFY-EA-WORKERS.
           MOVE SPACES TO VERIFY-EA-POOL-COLLECTORS-STAT.
           MOVE ZEROS  TO FORM-1 FORM-2 FORM-3 FORM-4.

           MOVE "EA"         TO CD-TYPE.
           MOVE  EA-GROUP-ID TO CD-CODE.
           PERFORM GET-CODE-RECORD.
           IF IO-FG NOT = 0
              MOVE "X" TO VERIFY-EA-GROUP-ID-STAT
              MOVE "*" TO WS-GROUP-ID-TYPE
              GO TO END-VERIFY-EA-FIELDS.

           MOVE CD-EA-GQ-FG TO GLOBAL-QUEUING-DONE.

           IF CD-EA-GQ-FG = "Y"
      * LOWER "G"
               MOVE X'67' TO WS-GROUP-ID-TYPE
           ELSE
               MOVE SPACES TO WS-GROUP-ID-TYPE.

           IF CD-EA-GQ-FG = "Y"
               IF BR-GLOBAL-QUEUE NOT = "Y"
                  MOVE "X" TO VERIFY-EA-GROUP-QUEUE-MATCH
                  GO TO END-VERIFY-EA-FIELDS.

           IF CD-EA-GQ-FG NOT = "Y"
               IF BR-GLOBAL-QUEUE = "Y"
                  MOVE "X" TO VERIFY-EA-GROUP-QUEUE-MATCH
                  GO TO END-VERIFY-EA-FIELDS.

      * VERIFY EA FORMS

           IF EA-SETFM-NO NOT = ZEROS
              MOVE EA-SETFM-NO  TO FD-FMNO
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                MOVE "X" TO VERIFY-EA-FORM1-STAT
                MOVE EA-SETFM-NO TO FORM-1.

           IF EA-SETFM(1) NOT = ZEROS
              MOVE EA-SETFM(1)  TO FD-FMNO
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                MOVE "X" TO VERIFY-EA-FORM2-STAT
                MOVE EA-SETFM(1) TO FORM-2.

           IF EA-SETFM(2) NOT = ZEROS
              MOVE EA-SETFM(2)  TO FD-FMNO
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                MOVE "X" TO VERIFY-EA-FORM3-STAT
                MOVE EA-SETFM(2) TO FORM-3.

           IF EA-SETFM(3) NOT = ZEROS
              MOVE EA-SETFM(3)  TO FD-FMNO
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                MOVE "X" TO VERIFY-EA-FORM4-STAT
                MOVE EA-SETFM(3) TO FORM-4.

      * VERIFY EA COLLECTOR

           IF EA-SET-LN-COL NOT = "Y"
              GO TO VERIFY-EA-POOL.

      * CD-EA-GQ-FG IS 'Y' IF GLOBAL-QUEING IS DONE

           MOVE "CN"         TO CDB-TYPE.
           IF GLOBAL-QUEUING-DONE  = "Y"
              MOVE 0 TO CDB-BRANCH
           ELSE
              MOVE EA-BRANCH TO CDB-BRANCH.
           MOVE EA-LN-COL-ID TO CDB-CODE.

           PERFORM GET-CDB-RECORD.

           IF IO-FG NOT = 0
               MOVE "X" TO VERIFY-EA-COLLECTOR-STAT.

       VERIFY-EA-POOL.
           IF EA-SET-LN-COL NOT = "P"
               GO TO END-VERIFY-EA-FIELDS.

           MOVE EA-COL-POOL  TO  CP-ID.

           IF GLOBAL-QUEUING-DONE = "Y"
              MOVE EXT-FILPATH-BASE TO CP-GLOBAL-BASE
              MOVE BR-MACHINE       TO CP-GLOBAL-MACHINE
              MOVE CP-GLOBAL-PATH   TO CP-PATH
              MOVE "G"              TO CP-PATH-OVERRIDE
           ELSE
              MOVE EXT-FILPATH-BASE TO CP-ALL-BASE
              MOVE BR-MACHINE       TO CP-ALL-MACHINE
              MOVE BR-DATA-PATH     TO CP-ALL-DATA
              MOVE CP-ALL-PATH      TO CP-PATH
              MOVE "Y"              TO CP-PATH-OVERRIDE.

           PERFORM GET-COLLECTOR-POOL.
           IF IO-FG NOT = 0
               MOVE "X" TO VERIFY-EA-POOL-STAT
               GO TO END-VERIFY-EA-FIELDS.

      * VERIFY COLLECTORS IN POOL

           MOVE "CN"         TO CDB-TYPE.
           IF GLOBAL-QUEUING-DONE  = "Y"
              MOVE 0         TO CDB-BRANCH
           ELSE
              MOVE EA-BRANCH TO CDB-BRANCH.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 80
                IF CP-COL-ID(SUB) = SPACES
                   MOVE 80 TO SUB
                ELSE
                   MOVE CP-COL-ID(SUB)  TO CDB-CODE
                   PERFORM GET-CDB-RECORD
                   IF IO-FG NOT = 0
                     MOVE "X"  TO VERIFY-EA-POOL-COLLECTORS-STAT
                     MOVE 80 TO SUB
                   END-IF
                END-IF
            END-PERFORM.

       
       END-VERIFY-EA-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           PERFORM GET-BRANCH.

           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO BR-NAME.

           PERFORM READ-LC1-FILE.

           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.

           MOVE BR-NO TO H-BRANCH.
           MOVE LC-CODE TO H-CLASS.
           MOVE BR-NAME TO H-BR-NAME.
           MOVE LC-DESC TO H-LC-DESC.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD5 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD6 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD7 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD8 TO PR-REC.
           MOVE 1      TO LN-FEED.
           PERFORM WRITE-PR-REC.


       
       END-HEAD-PAGE.
           MOVE 10 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************
           MOVE "EA"       TO CDSCAN-TYPE.
           MOVE SPACES     TO CDSCAN-CODE.
           MOVE "N"        TO CDSCAN-EA-CQ-FG.
           MOVE " "        TO CDSCAN-EA-GQ-FG.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO EALIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EALIST-SYSDATE.
           DISPLAY SPACES UPON EALIST-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EALIST-SCREEN UPON EALIST-WINDOW-HANDLE.
           MOVE EALIST-FORM-FIELDS TO WR-BUF.
           MOVE EALIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE EALIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE EALIST-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/EALIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

       GET-FD-RECORD.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.

       GET-CODE-RECORD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       GET-CDB-RECORD.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

       GET-COLLECTOR-POOL.
           PERFORM OPEN-CP1-FILE.
           PERFORM READ-CP1-FILE.
           PERFORM CLOSE-CP1-FILE.

       GET-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.


      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPEA1RN.CPY".
       COPY "LIBSP/SPCP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           MOVE "LP/CPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY EALIST-WINDOW-HANDLE
           EXIT PROGRAM.
