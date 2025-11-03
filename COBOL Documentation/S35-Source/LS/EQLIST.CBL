       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EQLIST.
       AUTHOR.             MJD.
       DATE-WRITTEN.  05/13/96.
      ******************************************************************
      *                      (LS)
      *
      *  DESCRIPTION:  COLLECTOR QUEUE LISTING
      *
      * REV:
      *  MJD 082996 ADDED EA-COL-POOL TO DETAIL-LINE.
      *  BLV 051297 ADD CD-FILE TO DECLARATIVES, CLOSE-FILES
      *  BLV 120600 RENAMED EA-POTS TO EA-CPOTS, WORLD PR# 244
      *  BLV 010102 INITIALIZED CDSCAN-EA-GQ-FG TO SPACES, TFC PR# 60
      *  CS  010406 ADDED VERIFY OF GROUP ID, POOL ID, AND MEMBERS OF
      *      POOL.  ADDED OPTION TO EXTRACT ONLY INVALID QUEUE LISTINGS
      *  CS  010417 MOVED VERIFY OF EA-FIELDS, HEADINGS WERE PRINTING AT
      *             CHANGE OF BRANCH/CLASS WASTING PAPER IF PRINTING INVALID
      *             ONLY
      *  CS  010507 ADDED EA-ALLOTMENTS
      *  BLV 011012 ADDED BEG, END DAYS SINCE LOAN DATE, DAYS UNTIL
      *             MATURITY, REGWAN PR# 1587
      *  BLV 021030 ADDED EA-DAY-OF-MO-DUE1 & 2, REGACC PR# 0009
      *  BAH 110922 REPLACED RRFORM WITH SCREEN
      * MJD 131210 A30 CONVERSION COMPLETED
      *
      * KEC 2016.0301 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED EA-ALPNAME1 & EA-ALPNAME2 FROM 3 TO 4 BYTES.
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
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34; 
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      ******************************************************************
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
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/EQLIST.CBL S35 03/25/22-12:33:20 >".

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
       01  HOLD-BRANCH          PIC 9(4)     VALUE ZEROS.
       01  GLOBAL-QUEUING-DONE  PIC X        VALUE SPACES.
       01  WS-GROUP-ID-TYPE     PIC X        VALUE SPACES.
       01  SUB-COL-ID-ON-LINE   PIC 99       VALUE ZEROS.

       01  VERIFY-EA-WORKERS.
           03  VERIFY-EA-GROUP-ID-STAT       PIC X.
               88  VERIFY-EA-GROUP-ID-BAD    VALUE "X".
           03  VERIFY-EA-GROUP-QUEUE-MATCH   PIC X.
               88  VERIFY-EA-GROUP-QUEUE-BAD VALUE "X".
           03  VERIFY-EA-POOL-STAT           PIC X.
               88 VERIFY-EA-POOL-BAD         VALUE "X".
       01  VERIFY-EA-POOL-COLLECTORS-STAT    PIC X.
           88  VERIFY-EA-POOL-COLLECTORS-BAD  VALUE "X".


      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       01  SCREEN-BUF           VALUE SPACES.
           03  ERRCD            PIC X.
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 9(3).
           03  END-CLASS        PIC 99.
           03  BEG-GROUP-ID     PIC X(6).
           03  END-GROUP-ID     PIC X(6).
           03  INVALID-ONLY-FG  PIC X.
      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(24)    VALUE SPACES.
           03  FILLER           PIC X(41)    VALUE
           "       COLLECTOR QUEUE FILE LISTING      ".
           03  FILLER           PIC X(47)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH :".
           03  H-BRANCH         PIC Z(4)     VALUE SPACES.
           03  FILLER           PIC X        VALUE SPACES.
           03  H-BR-NAME        PIC X(20)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10)    VALUE "CLASS  :".
           03  H-CLASS          PIC ZZ       VALUE SPACES.
           03  FILLER           PIC X        VALUE SPACES.
           03  H-LC-DESC        PIC X(20)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(22)    VALUE
               "GROUP  PDUE    PDUE   ".
           03  FILLER           PIC X(16)    VALUE
               "ACCOUNT  PAST  R".
           03  FILLER           PIC X(38)    VALUE
               "ECY LAST   CONT  DEALER LOAN MATU DUE ".
           03  FILLER           PIC X(34)    VALUE
               "ACTN  POTS  MATD  LGL  ALL PL-BEG ".
           03  FILLER           PIC X(22)    VALUE
               "  JD   PRIO BP    POOL".

       01  HEAD7.
           03  FILLER           PIC X(22)    VALUE
               "SEQ    DAYS    AMTS   ".
           03  FILLER           PIC X(16)    VALUE
               "BALANCE  PROM  D".
           03  FILLER           PIC X(38)    VALUE
               "AYS NAME   DAYS         DAYS DAYS DAY ".
           03  FILLER           PIC X(34)    VALUE
               "ORGN RPOTS  PROM  DA   PL  PL-END ".
           03  FILLER           PIC X(22)    VALUE
               "  CNF  BEG-PROM       ".

       01  HEAD8.
           03  FILLER           PIC X(76) VALUE SPACES.

           03  FILLER           PIC X(34)    VALUE
               "REC  MPAYS  REPO  BK   PLA        ".

           03  FILLER           PIC X(22)    VALUE
               "  1ST  END-PROM       ".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE REDEFINES DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-SEQNO-GROUPID                PIC X(6).
           03  D-SEQNO-GROUPID-RDF1           REDEFINES D-SEQNO-GROUPID.
               05  FILLER                     PIC X(1).
               05  D-SEQNO                    PIC ZZ.
               05  FILLER                     PIC X(3).
           03  D-SEQNO-GROUPID-RDF1           REDEFINES D-SEQNO-GROUPID.
               05  D-GROUP-ID                 PIC X(6).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-GROUP-ID-TYPE                PIC X(1).
           03  D-PASTDUE-DAY                  PIC ZZZ9.
           03  FILLER                         PIC X.
           03  D-PASTDUE-AMT                  PIC ZZZ9.99.
           03  FILLER                         PIC X.
           03  D-ACCT-BAL                     PIC ZZZZZ9.99.
           03  FILLER                         PIC XX.
           03  D-PAST-PROM-DAY                PIC ZZZ9-.
           03  FILLER                         PIC X.
           03  D-RECENCY-DAY                  PIC ZZZ9.
           03  FILLER                         PIC X(1).
           03  D-LNAME                        PIC X(4).
           03  FILLER                         PIC X(3).
           03  D-CONTR-DAY                    PIC ZZZ9.
           03  FILLER                         PIC X(1).
           03  D-DEALER                       PIC ZZZ9.99.

           03  FILLER                         PIC X.
           03  D-LNDATE-DAYS                  PIC ZZZ9.
           03  FILLER                         PIC X.
           03  D-MATURE-DAYS                  PIC ZZZ9.
           03  FILLER                         PIC X.
           03  D-DAY-OF-MO-DUE                PIC Z9.
           03  FILLER                         PIC XX.

           03  D-ACTN-ORGN-REC                PIC XXXX.
           03  D-REC REDEFINES D-ACTN-ORGN-REC.
               05  FILLER                     PIC X(3).
               05  D-LOAN-RECENCY             PIC X.

           03  FILLER                         PIC X(3).
           03  D-POTS-RPOTS-MPAY              PIC X(1).
           03  FILLER                         PIC X(5).
           03  D-MATD-PROM-REPO               PIC X.
           03  FILLER                         PIC X(4).
           03  D-LG-DA-BK                     PIC X(1).
           03  FILLER                         PIC X(4).
           03  D-ALLOT-PL-PLO                 PIC X.
           03  FILLER                         PIC X(3).
           03  D-PL-BEG-END-DATE              PIC 99/99/99.
           03  FILLER                         PIC X(2).
           03  D-JD-CNF-1ST                   PIC X.
           03  FILLER                         PIC X(4).
           03  D-BROKEN-PROM-DAYS             PIC ZZZ9.
           03  D-PROMISE-DAYS REDEFINES D-BROKEN-PROM-DAYS.
               05  FILLER                     PIC X(3).
               05  D-BROKEN-PROM-FG           PIC X.
           03  FILLER                         PIC X(7).
           03  D-POOL                         PIC X(3).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(7).
           03  D-DESCRIPTION                  PIC X(13).
           03  D-DESC                         PIC X(52).
           03  FILLER                         PIC X(60).



      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01 EQLIST-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/EQLIST_DEF.CPY".
       COPY "LIBLS/EQLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/EQLIST_SCN.CPY".

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
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "EQLIST" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS EQLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = " " )
              GO TO END-PROGRAM.

       NEXT-EA-RECORD.
           PERFORM READ-EA1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO END-ROUTINE.

           IF EA-CQ-FG NOT = "Y"
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
           MOVE EQLIST-QUEUE-POS TO Q-POS.
           MOVE EQLIST-COPIES-POS TO C-POS.
           MOVE EQLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

      *-----------------------------------------------------------------
       CONT.
           MOVE "ENNN040BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALLBR.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CONT.
           MOVE VDUNUM0 TO BEG-BRANCH.

      *-----------------------------------------------------------------
       ENDBRN.
           MOVE "ENNN040ENDBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO CONT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDBRN.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CONT.

           GO TO BEGCLS.

      *-----------------------------------------------------------------
       ALLBR.
           MOVE "SNNN040BEGBR." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBR." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGCLS.
           MOVE "ENNN020BEGCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO ENDBRN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGCLS.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       ALLCLS.
           MOVE "SNNN020BEGCL." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-GROUP-ID.
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
           PERFORM GET-CD1-RECORD.
           IF ( IO-FG NOT = 0 )
              GO TO BEGIN-GROUP-ID.

      *-----------------------------------------------------------------
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
           PERFORM GET-CD1-RECORD.
           IF ( IO-FG NOT = 0 )
              GO TO ENDING-GROUP-ID.

           IF ( BEG-GROUP-ID > END-GROUP-ID )
              MOVE "INVALID GROUP ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-GROUP-ID.

           GO TO INVALID-ONLY.

      *-----------------------------------------------------------------
       ALL-GROUPS.
           MOVE "S  A060BEG-GROUP-ID." TO VDU.
           MOVE SPACES TO BEG-GROUP-ID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A060END-GROUP-ID." TO VDU.
           MOVE "ZZZZZZ" TO END-GROUP-ID VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       RRSEL.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRSEL.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO CONT.
           IF ACCEPT-BUF NOT = "Y" GO TO CONT.

           MOVE "LISTING IN PROGRESS...................." TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-EA1-FILE.

           MOVE BEG-BRANCH TO EA-BRANCH
                              QEA1-WBEG-BRANCH.
           MOVE END-BRANCH TO QEA1-WEND-BRANCH.
           MOVE BEG-CLASS  TO EA-CLASS
                              QEA1-WBEG-CLASS.
           MOVE END-CLASS  TO QEA1-WEND-CLASS.
           MOVE SPACES     TO EA-GROUP-ID
                              QEA1-WBEG-GROUP-ID.
           MOVE ALL "Z"    TO QEA1-WEND-GROUP-ID.
           MOVE 0          TO EA-SEQNO
                              QEA1-WBEG-SEQNO.
           MOVE ALL "9"    TO QEA1-WEND-SEQNO.

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

           IF VERIFY-EA-WORKERS  = SPACES
              IF VERIFY-EA-POOL-COLLECTORS-STAT =  SPACES
                  IF INVALID-ONLY-FG = "Y"
                 GO TO EXIT-DETAIL-LN-PROCESS.

           IF EA-BRANCH NOT = BR-NO
              IF FIRST-TIME NOT = "Y"
                 MOVE EA-BRANCH TO BR-NO
                 MOVE EA-CLASS TO LC-CODE
                 PERFORM HEAD-PAGE.

           IF EA-CLASS NOT = LC-CODE
              IF FIRST-TIME NOT = "Y"
                 MOVE EA-CLASS TO LC-CODE

      * LINE 1 

           MOVE SPACES TO DETAIL-LINE.

           MOVE EA-GROUP-ID         TO D-GROUP-ID.
           MOVE WS-GROUP-ID-TYPE    TO D-GROUP-ID-TYPE.
           MOVE EA-PASTDUE-DAY1     TO D-PASTDUE-DAY.
           MOVE EA-PASTDUE-AMT1     TO D-PASTDUE-AMT.
           MOVE EA-ACCT-BAL1        TO D-ACCT-BAL.
           MOVE EA-PASTPROM-DAY1    TO D-PAST-PROM-DAY.
           MOVE EA-RECENCY-DAY1     TO D-RECENCY-DAY.
           MOVE EA-ALPNAME1         TO D-LNAME.
           MOVE EA-CONTRACTUAL-DAY1 TO D-CONTR-DAY.
           MOVE EA-DLNO1            TO D-DEALER.
           MOVE EA-LNDATE-DAYS1     TO D-LNDATE-DAYS.
           MOVE EA-MATURE-DAYS1     TO D-MATURE-DAYS.
           MOVE EA-DAY-OF-MO-DUE1   TO D-DAY-OF-MO-DUE.
           MOVE EA-ACTION           TO D-ACTN-ORGN-REC.
           MOVE EA-CPOTS            TO D-POTS-RPOTS-MPAY.
           MOVE EA-MATURED          TO D-MATD-PROM-REPO.
           MOVE EA-LEGAL            TO D-LG-DA-BK.
           MOVE EA-ALLOTMENTS       TO D-ALLOT-PL-PLO.
           MOVE EA-PL-BEG-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-PL-BEG-END-DATE.
           MOVE EA-JUDGEMENT        TO D-JD-CNF-1ST.
           MOVE EA-BROKEN-PROM-FG   TO D-BROKEN-PROM-FG.
           MOVE EA-COL-POOL         TO D-POOL.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      * LINE 2 

           MOVE SPACES TO DETAIL-LINE.

           MOVE EA-SEQNO            TO D-SEQNO.
           MOVE EA-PASTDUE-DAY2     TO D-PASTDUE-DAY.
           MOVE EA-PASTDUE-AMT2     TO D-PASTDUE-AMT.
           MOVE EA-ACCT-BAL2        TO D-ACCT-BAL.
           MOVE EA-PASTPROM-DAY2    TO D-PAST-PROM-DAY.
           MOVE EA-RECENCY-DAY2     TO D-RECENCY-DAY.
           MOVE EA-ALPNAME2         TO D-LNAME.
           MOVE EA-CONTRACTUAL-DAY2 TO D-CONTR-DAY.
           MOVE EA-DLNO2            TO D-DEALER.
           MOVE EA-LNDATE-DAYS2     TO D-LNDATE-DAYS.
           MOVE EA-MATURE-DAYS2     TO D-MATURE-DAYS.
           MOVE EA-DAY-OF-MO-DUE2   TO D-DAY-OF-MO-DUE.
           MOVE EA-ORIGIN           TO D-ACTN-ORGN-REC.
           MOVE EA-RPOTS            TO D-POTS-RPOTS-MPAY.
           MOVE EA-PROMISED         TO D-MATD-PROM-REPO.
           MOVE EA-DEBTAID          TO D-LG-DA-BK.
           MOVE EA-PL               TO D-ALLOT-PL-PLO.
           MOVE EA-PL-END-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-PL-BEG-END-DATE.
           MOVE EA-CONFIDENTIAL-FG  TO D-JD-CNF-1ST.
           MOVE EA-PROM-BREAK1      TO D-BROKEN-PROM-DAYS.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      * LINE 3

           MOVE SPACES TO DETAIL-LINE.

           MOVE "DESCRIPTION: " TO D-DESCRIPTION.
           MOVE EA-DESC         TO D-DESC.

           MOVE EA-RECENCYDATE   TO D-LOAN-RECENCY.
           MOVE EA-MISPAYS       TO D-POTS-RPOTS-MPAY.
           MOVE EA-REPO          TO D-MATD-PROM-REPO.
           MOVE EA-BANKRUPT      TO D-LG-DA-BK.
           MOVE EA-PL-IN-ACTIVE-BOTH TO D-ALLOT-PL-PLO.
           MOVE EA-1ST-PMT-MISSES TO D-JD-CNF-1ST.
           MOVE EA-PROM-BREAK2 TO D-BROKEN-PROM-DAYS.

           PERFORM WRITE-DETAIL-LINE.

           IF VERIFY-EA-WORKERS = SPACES
               IF VERIFY-EA-POOL-COLLECTORS-STAT = SPACES
                  MOVE 2 TO LN-FEED
                  GO TO END-DETAIL-LN-PROCESS.

      * CONTINUE INVALID GROUP ID

           IF VERIFY-EA-WORKERS NOT = SPACES
               MOVE "[ INVALID ]" TO DETAIL-LINE
           ELSE
               GO TO CONTINUE-CHECK-POOL-COLLECTORS.

           IF VERIFY-EA-GROUP-ID-BAD
               STRING DETAIL-LINE, " ", "GROUP ID:", " " EA-GROUP-ID,
                 DELIMITED BY "  " INTO DETAIL-LINE .

           IF VERIFY-EA-GROUP-QUEUE-BAD
               STRING DETAIL-LINE,
                " ", "GLOBAL QUEUE TYPE MISMATCH: " DELIMITED BY "  "
                INTO DETAIL-LINE.

      * CHECK INVALID GROUP MISMATCH

           IF VERIFY-EA-GROUP-ID-BAD OR VERIFY-EA-GROUP-QUEUE-BAD
               STRING DETAIL-LINE,
                " [ NO FURTHER VERIFICATION DONE ]"
                      DELIMITED BY "  " INTO DETAIL-LINE
                 GO TO PRINT-INVALID-LINE1.

      * CONTINUE INVALID POOL

           IF VERIFY-EA-POOL-BAD
               STRING DETAIL-LINE," ", "POOL ID:", " " EA-COL-POOL
                 DELIMITED BY "  " INTO DETAIL-LINE .

      * PRINT 1ST LINE OF INVALID EA-FIELDS
       PRINT-INVALID-LINE1.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * IF NO INVALID-POOL-COLLECTOR STATUS SKIP CHECKING POOL-COLLECTORS

           IF VERIFY-EA-POOL-COLLECTORS-STAT = SPACES
              GO TO END-DETAIL-LN-PROCESS.

       CONTINUE-CHECK-POOL-COLLECTORS.

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

           MOVE SPACES TO VERIFY-EA-WORKERS.
           MOVE SPACES TO VERIFY-EA-POOL-COLLECTORS-STAT.

           MOVE "EA"         TO CD-TYPE.
           MOVE EA-GROUP-ID  TO CD-CODE.
           PERFORM GET-CD1-RECORD.
           IF IO-BAD
              MOVE "X" TO VERIFY-EA-GROUP-ID-STAT
              MOVE "*" TO WS-GROUP-ID-TYPE
              GO TO END-VERIFY-EA-FIELDS.

           MOVE CD-EA-GQ-FG TO GLOBAL-QUEUING-DONE.

           IF CD-EA-GQ-FG = "Y"
      * LOWER "G'
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

      * VERIFY EA POOLS

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

           MOVE EA-COL-POOL TO  CP-ID.
           PERFORM GET-COLLECTOR-POOL.
           IF IO-BAD
               MOVE "X" TO VERIFY-EA-POOL-STAT
               GO TO END-VERIFY-EA-FIELDS.

      * VERIFY EA POOL COLLECTORS

           MOVE "CN" TO CDB-TYPE.
           IF GLOBAL-QUEUING-DONE = "Y"
              MOVE 0 TO CDB-BRANCH
           ELSE
              MOVE EA-BRANCH TO CDB-BRANCH.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 80
              IF CP-COL-ID(SUB) = SPACES
                  MOVE 80 TO SUB
              ELSE
                  MOVE CP-COL-ID(SUB) TO CDB-CODE
                  PERFORM GET-CDB-RECORD
                  IF IO-FG NOT = 0
                      MOVE "X" TO VERIFY-EA-POOL-COLLECTORS-STAT
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

           MOVE HEAD6 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD7 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD8 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 10 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************
           MOVE "EA"       TO CDSCAN-TYPE.
           MOVE SPACES     TO CDSCAN-CODE.
           MOVE "Y"        TO CDSCAN-EA-CQ-FG.
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
           MOVE EXT-CONAME-CONAME TO EQLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EQLIST-SYSDATE.
           DISPLAY SPACES UPON EQLIST-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EQLIST-SCREEN UPON EQLIST-WINDOW-HANDLE.
           MOVE EQLIST-FORM-FIELDS TO WR-BUF.
           MOVE EQLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE EQLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE EQLIST-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/EQLIST_EVA.CPY".
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

       GET-CD1-RECORD.
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
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
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
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EQLIST-SCREEN.
           DESTROY EQLIST-WINDOW-HANDLE
           EXIT PROGRAM.
